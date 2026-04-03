# =============================================================================
# Profil Accélération-Vitesse In Situ — Dashboard R Shiny
# Méthodologie : Morin et al. (identification points haute intensité)
#                + régression linéaire / quantile
# Format attendu : export OpenField (séparateur ; / décimale ,)
# =============================================================================
# Packages requis :
#   install.packages(c("shiny","shinydashboard","dplyr","ggplot2",
#                      "quantreg","dbscan","DT"))
# =============================================================================

# Limites des axes des graphiques (m/s et m/s²)
AXIS_MAX_SPEED <- 11
AXIS_MAX_ACC   <- 11

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(quantreg)
library(dbscan)
library(DT)

# =============================================================================
# CHARGEMENT DES DONNÉES
# =============================================================================

#' Lire un fichier export OpenField
#'
#' Saute les lignes de métadonnées (# …), convertit la vitesse km/h → m/s,
#' nettoie les noms de colonnes (corrige l'erreur "NA or '' names").
read_openfield_csv <- function(filepath) {
  lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)

  # Extraction du nom de l'athlète depuis les métadonnées
  athlete_line <- lines[grep("Athlete:", lines)]
  athlete <- if (length(athlete_line) > 0) {
    trimws(gsub('.*Athlete:\\s*"?|"\\s*$', "", athlete_line[1]))
  } else ""

  # Si nom vide, utiliser le DeviceId
  if (nchar(athlete) == 0) {
    device_line <- lines[grep("DeviceId:", lines)]
    athlete <- if (length(device_line) > 0) {
      trimws(gsub(".*DeviceId\\s*:\\s*", "", device_line[1]))
    } else "Inconnu"
  }

  # Nombre de lignes de métadonnées à ignorer (toutes commençant par #)
  skip_n <- sum(startsWith(lines, "#"))

  # Lecture CSV (format européen : séparateur ; / décimale ,)
  df <- read.csv(
    filepath,
    skip        = skip_n,
    sep         = ";",
    dec         = ",",
    header      = TRUE,
    stringsAsFactors = FALSE
  )

  # Correction des noms de colonnes invalides (évite l'erreur NA/"" names)
  names(df) <- make.names(names(df), unique = TRUE)

  # Renommage Velocity → Speed et conversion km/h → m/s
  if ("Velocity" %in% names(df)) {
    df <- rename(df, Speed = Velocity)
  }

  df <- df %>%
    mutate(
      Speed        = as.numeric(Speed) / 3.6,
      Acceleration = as.numeric(Acceleration),
      Player       = athlete,
      Date         = as.character(Timestamp)
    )

  df
}

# =============================================================================
# DÉTECTION DES OUTLIERS
# =============================================================================

#' Supprime les accélérations négatives
remove_negative_accelerations <- function(df) {
  filter(df, Acceleration >= 0)
}

#' Erreurs d'utilisation (points physiologiquement impossibles)
#' Seuil : Acceleration >= 10.93 - 10.93/10.5 * Speed, par session
identify_misuse_errors <- function(correct_points, nb_outlier = 10) {
  outliers <- correct_points %>%
    filter(Acceleration >= 10.93 - (10.93 / 10.5) * Speed) %>%
    group_by(Player, Date) %>%
    mutate(n_error = n()) %>%
    ungroup() %>%
    filter(n_error >= nb_outlier)

  outlier_dates <- unique(outliers$Date)

  list(
    correct_points = filter(correct_points, !Date %in% outlier_dates),
    misuse_error   = outliers
  )
}

#' Erreurs de mesure (DBSCAN) — points isolés physiquement incohérents
identify_measurement_errors <- function(correct_points,
                                        neighb_dbscan = 3,
                                        eps_dbscan    = 0.5) {
  # Identifiant de ligne stable
  correct_points$.row_id <- seq_len(nrow(correct_points))

  # Sous-ensemble pertinent pour le DBSCAN
  players_sample <- filter(correct_points, Acceleration >= 5 - Speed)

  if (nrow(players_sample) == 0) {
    correct_points$.row_id <- NULL
    return(list(correct_points = correct_points, measurement_error = data.frame()))
  }

  # DBSCAN par joueur (cluster 0 = bruit en R, -1 en Python/sklearn)
  labeled <- players_sample %>%
    group_by(Player) %>%
    group_modify(~ {
      mat    <- as.matrix(.x[, c("Speed", "Acceleration")])
      result <- dbscan::dbscan(mat, eps = eps_dbscan, minPts = neighb_dbscan)
      .x$label <- result$cluster
      .x
    }) %>%
    ungroup()

  outliers_dbscan <- filter(labeled, label == 0)
  outlier_ids     <- outliers_dbscan$.row_id

  correct_clean  <- correct_points %>%
    filter(!.row_id %in% outlier_ids) %>%
    select(-.row_id)

  measurement_clean <- outliers_dbscan %>% select(-.row_id, -label)

  list(
    correct_points   = correct_clean,
    measurement_error = measurement_clean
  )
}

# =============================================================================
# IDENTIFICATION DES POINTS HAUTE INTENSITÉ (méthode Morin)
# =============================================================================

#' Pour chaque intervalle de vitesse dv, conserve les n_max points
#' ayant les accélérations les plus élevées (par joueur).
#' Puis limite aux points dont la vitesse ≥ vitesse au point de Amax.
identify_high_intensity_points <- function(points, dv = 0.3, n_max = 2) {
  df <- points %>%
    mutate(dv_interval = floor(Speed / dv)) %>%
    group_by(Player, dv_interval) %>%
    mutate(rank_acc = rank(-Acceleration, ties.method = "min")) %>%
    ungroup() %>%
    filter(rank_acc <= n_max)

  # Point de puissance maximale : Amax par joueur
  df <- df %>%
    group_by(Player) %>%
    mutate(max_Acceleration = max(Acceleration)) %>%
    ungroup()

  max_speed_at_max_acc <- df %>%
    filter(Acceleration == max_Acceleration) %>%
    group_by(Player) %>%
    summarise(max_Speed_at_max_Acceleration = max(Speed), .groups = "drop")

  df %>%
    left_join(max_speed_at_max_acc, by = "Player") %>%
    filter(Speed >= max_Speed_at_max_Acceleration)
}

# =============================================================================
# RÉGRESSIONS
# =============================================================================

#' Régression linéaire Acceleration ~ Speed par joueur
#' Retourne a0 (intercept) et s0 = -a0/b (vitesse maximale théorique)
compute_linear_regression <- function(hi_points, r2_threshold = 0.5) {
  hi_points %>%
    group_by(Player) %>%
    group_modify(~ {
      model <- lm(Acceleration ~ Speed, data = .x)
      r2    <- summary(model)$r.squared
      if (r2 <= r2_threshold) {
        warning(paste("Régression linéaire de faible qualité pour", unique(.x$Player)))
      }
      a0 <- coef(model)[["(Intercept)"]]
      b  <- coef(model)[["Speed"]]
      tibble(
        `a0 : Regression lineaire` = a0,
        `s0 : Regression lineaire` = -a0 / b,
        R2_lineaire                = round(r2, 3)
      )
    }) %>%
    ungroup()
}

#' Régression quantile (quantiles 0.05 à 0.95 par pas de 0.01) par joueur
compute_quantile_regression <- function(hi_points) {
  quantiles <- seq(0.05, 0.95, by = 0.01)

  hi_points %>%
    group_by(Player) %>%
    group_modify(~ {
      data_p <- .x
      rows   <- lapply(quantiles, function(q) {
        tryCatch({
          model <- quantreg::rq(Acceleration ~ Speed, tau = q, data = data_p)
          a0    <- coef(model)[["(Intercept)"]]
          b     <- coef(model)[["Speed"]]
          data.frame(q = q, a0 = a0, s0 = -a0 / b)
        }, error = function(e) data.frame(q = q, a0 = NA_real_, s0 = NA_real_))
      })
      do.call(rbind, rows)
    }) %>%
    ungroup()
}

#' Résumé (moyenne ± écart-type) des a0/s0 sur l'ensemble des quantiles
compute_quantile_summary <- function(quantile_results) {
  quantile_results %>%
    group_by(Player) %>%
    summarise(
      `a0 : Regression quantile` = mean(a0, na.rm = TRUE),
      std_a0                     = sd(a0,   na.rm = TRUE),
      `s0 : Regression quantile` = mean(s0, na.rm = TRUE),
      std_s0                     = sd(s0,   na.rm = TRUE),
      .groups = "drop"
    )
}

# =============================================================================
# VISUALISATIONS (ggplot2)
# =============================================================================

plot_outliers <- function(correct_points, measurement_error = NULL,
                          misuse_error = NULL, player) {
  p_data <- filter(correct_points, Player == player)

  p <- ggplot(p_data, aes(x = Speed, y = Acceleration)) +
    geom_point(alpha = 0.4, size = 1, colour = "#1f77b4") +
    labs(
      title = paste("Outliers :", player),
      x = "Vitesse (m/s)", y = "Accélération (m/s²)"
    ) +
    coord_cartesian(xlim = c(0, AXIS_MAX_SPEED), ylim = c(0, AXIS_MAX_ACC)) +
    theme_minimal(base_size = 13)

  if (!is.null(measurement_error) && nrow(measurement_error) > 0) {
    me <- filter(measurement_error, Player == player)
    if (nrow(me) > 0)
      p <- p + geom_point(data = me, colour = "red", alpha = 0.9, size = 1.5,
                          aes(x = Speed, y = Acceleration))
  }

  if (!is.null(misuse_error) && nrow(misuse_error) > 0) {
    mue <- filter(misuse_error, Player == player)
    if (nrow(mue) > 0)
      p <- p + geom_point(data = mue, colour = "black", alpha = 0.9, size = 1.5,
                          aes(x = Speed, y = Acceleration))
  }

  p
}

plot_linear_regression <- function(points, hi_points, linear_results, player) {
  p_data  <- filter(points,         Player == player)
  hi_data <- filter(hi_points,      Player == player)
  reg     <- filter(linear_results, Player == player)

  a0 <- reg$`a0 : Regression lineaire`
  s0 <- reg$`s0 : Regression lineaire`

  ggplot(p_data, aes(x = Speed, y = Acceleration)) +
    geom_point(alpha = 0.4, size = 1, colour = "#1f77b4") +
    geom_point(data = hi_data, colour = "red", alpha = 0.9, size = 1.5,
               aes(x = Speed, y = Acceleration)) +
    geom_abline(intercept = a0, slope = -a0 / s0, colour = "red", linewidth = 0.8) +
    annotate("text", x = 2,  y = 9.5,
             label = sprintf("a0 = %.2f m/s²", a0), colour = "red", size = 4) +
    annotate("text", x = 8,  y = 5,
             label = sprintf("s0 = %.2f m/s",  s0), colour = "red", size = 4) +
    labs(title = paste("Régression linéaire :", player),
         x = "Vitesse (m/s)", y = "Accélération (m/s²)") +
    coord_cartesian(xlim = c(0, AXIS_MAX_SPEED), ylim = c(0, AXIS_MAX_ACC)) +
    theme_minimal(base_size = 13)
}

plot_quantile_regression <- function(points, hi_points, quantile_results,
                                     quantile_summary, player) {
  p_data  <- filter(points,            Player == player)
  hi_data <- filter(hi_points,         Player == player)
  q_data  <- filter(quantile_results,  Player == player)
  q_sum   <- filter(quantile_summary,  Player == player)

  a0     <- q_sum$`a0 : Regression quantile`
  s0     <- q_sum$`s0 : Regression quantile`
  std_a0 <- q_sum$std_a0
  std_s0 <- q_sum$std_s0

  p <- ggplot(p_data, aes(x = Speed, y = Acceleration)) +
    geom_point(alpha = 0.4, size = 1, colour = "#1f77b4") +
    geom_point(data = hi_data, colour = "red", alpha = 0.9, size = 1.5,
               aes(x = Speed, y = Acceleration))

  # Droites pour quantiles sélectionnés (de 0.05 à 0.95, pas 0.10)
  for (q_val in seq(0.05, 0.95, by = 0.10)) {
    q_row <- filter(q_data, abs(q - q_val) < 0.005)
    if (nrow(q_row) > 0 && !is.na(q_row$s0[1]) && q_row$s0[1] > 0) {
      p <- p + geom_abline(
        intercept = q_row$a0[1],
        slope     = -q_row$a0[1] / q_row$s0[1],
        colour = "grey60", linetype = "dotted", linewidth = 0.5
      )
    }
  }

  # Droite moyenne
  if (!is.na(s0) && s0 > 0) {
    p <- p +
      geom_abline(intercept = a0, slope = -a0 / s0, colour = "red", linewidth = 0.8) +
      annotate("text", x = 2, y = 9.5,
               label = sprintf("a0 = %.2f \u00b1 %.2f m/s²", a0, std_a0),
               colour = "red", size = 4) +
      annotate("text", x = 8, y = 5,
               label = sprintf("s0 = %.2f \u00b1 %.2f m/s",  s0, std_s0),
               colour = "red", size = 4)
  }

  p +
    labs(title = paste("Profil Accélération-Vitesse :", player),
         x = "Vitesse (m/s)", y = "Accélération (m/s²)") +
    coord_cartesian(xlim = c(0, AXIS_MAX_SPEED), ylim = c(0, AXIS_MAX_ACC)) +
    theme_minimal(base_size = 13)
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Profil A-V In Situ"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement",  tabName = "upload",     icon = icon("upload")),
      menuItem("Nettoyage",   tabName = "cleaning",   icon = icon("broom")),
      menuItem("Profil A-V",  tabName = "regression", icon = icon("chart-line")),
      menuItem("Résultats",   tabName = "results",    icon = icon("table"))
    ),
    hr(),
    tags$h5("Paramètres", style = "padding-left:15px; color:white; font-weight:bold;"),
    numericInput("dv",           "Intervalle vitesse (dv, m/s)", value = 0.3, min = 0.1, max = 1,   step = 0.1),
    numericInput("n_max",        "Points max / intervalle",      value = 2,   min = 1,   max = 5),
    numericInput("nb_outlier",   "Seuil erreur utilisation",     value = 10,  min = 1),
    numericInput("eps_dbscan",   "Eps DBSCAN",                   value = 0.5, min = 0.1, step = 0.1),
    numericInput("neighb_dbscan","Min points DBSCAN",            value = 3,   min = 1),
    numericInput("r2_threshold", "Seuil R² (régr. linéaire)",   value = 0.5, min = 0, max = 1, step = 0.05)
  ),

  dashboardBody(
    tabItems(

      # ---- Onglet 1 : Chargement -----------------------------------------------
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Chargement du fichier CSV (export OpenField)",
            width = 12, status = "primary", solidHeader = TRUE,
            fileInput("file_upload", "Choisir un fichier CSV",
                      accept = ".csv",
                      buttonLabel = "Parcourir",
                      placeholder = "Aucun fichier sélectionné"),
            helpText("Format attendu : export OpenField — séparateur « ; », décimale « , »")
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_points"),
          valueBoxOutput("vbox_players"),
          valueBoxOutput("vbox_sessions")
        ),
        fluidRow(
          box(title = "Aperçu des données (100 premières lignes)",
              width = 12, DTOutput("table_preview"))
        )
      ),

      # ---- Onglet 2 : Nettoyage ------------------------------------------------
      tabItem(tabName = "cleaning",
        fluidRow(
          box(
            title = "Lancer le nettoyage", width = 12, status = "warning", solidHeader = TRUE,
            actionButton("btn_clean", "Nettoyer les données",
                         icon = icon("play"), class = "btn-warning btn-lg"),
            br(), br(),
            verbatimTextOutput("clean_summary")
          )
        ),
        fluidRow(
          box(title = "Joueur", width = 3, uiOutput("select_player_clean")),
          box(title = "Nuage de points — Outliers", width = 9,
              plotOutput("plot_outliers_out", height = "420px"))
        ),
        fluidRow(
          box(title = "Erreurs de mesure (DBSCAN — points rouges)",  width = 6,
              DTOutput("table_measurement_error")),
          box(title = "Erreurs d'utilisation (points noirs)", width = 6,
              DTOutput("table_misuse_error"))
        )
      ),

      # ---- Onglet 3 : Profil A-V -----------------------------------------------
      tabItem(tabName = "regression",
        fluidRow(
          box(
            title = "Calcul du profil", width = 12, status = "info", solidHeader = TRUE,
            checkboxInput("use_linear",   "Régression linéaire", value = TRUE),
            checkboxInput("use_quantile", "Régression quantile", value = TRUE),
            actionButton("btn_regress", "Calculer le profil A-V",
                         icon = icon("play"), class = "btn-primary btn-lg")
          )
        ),
        fluidRow(
          box(title = "Joueur", width = 3, uiOutput("select_player_reg")),
          tabBox(
            title = "Visualisations", width = 9, id = "tab_plots",
            tabPanel("Régression linéaire",
                     plotOutput("plot_linear_reg",  height = "420px")),
            tabPanel("Régression quantile",
                     plotOutput("plot_quantile_reg", height = "420px"))
          )
        )
      ),

      # ---- Onglet 4 : Résultats ------------------------------------------------
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Tableau des résultats", width = 12, status = "success", solidHeader = TRUE,
            DTOutput("table_results"),
            br(),
            downloadButton("download_results", "Télécharger les résultats (.csv)",
                           class = "btn-success")
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- États réactifs ---------------------------------------------------------
  raw_data             <- reactiveVal(NULL)
  clean_data           <- reactiveVal(NULL)
  measurement_err_data <- reactiveVal(data.frame())
  misuse_err_data      <- reactiveVal(data.frame())
  hi_points_data       <- reactiveVal(NULL)
  linear_results_data  <- reactiveVal(NULL)
  quantile_results_data <- reactiveVal(NULL)
  quantile_summary_data <- reactiveVal(NULL)

  # --- Chargement du fichier --------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      df <- read_openfield_csv(input$file_upload$datapath)
      raw_data(df)
      clean_data(df)
      # Réinitialisation des résultats précédents
      measurement_err_data(data.frame())
      misuse_err_data(data.frame())
      hi_points_data(NULL)
      linear_results_data(NULL)
      quantile_results_data(NULL)
      quantile_summary_data(NULL)
      showNotification("\u2705 Fichier chargé avec succès !", type = "message")
    }, error = function(e) {
      showNotification(paste("\u274c Erreur :", e$message), type = "error", duration = 10)
    })
  })

  # --- Value boxes ------------------------------------------------------------
  output$vbox_points <- renderValueBox({
    req(raw_data())
    valueBox(format(nrow(raw_data()), big.mark = " "),
             "Points de données", icon = icon("database"), color = "blue")
  })
  output$vbox_players <- renderValueBox({
    req(raw_data())
    valueBox(length(unique(raw_data()$Player)),
             "Joueurs", icon = icon("user"), color = "green")
  })
  output$vbox_sessions <- renderValueBox({
    req(raw_data())
    valueBox(length(unique(raw_data()$Date)),
             "Horodatages uniques", icon = icon("calendar"), color = "orange")
  })

  # --- Aperçu -----------------------------------------------------------------
  output$table_preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 100),
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })

  # --- Sélecteurs de joueur ---------------------------------------------------
  output$select_player_clean <- renderUI({
    req(clean_data())
    selectInput("player_clean", "Sélectionner un joueur",
                choices = sort(unique(clean_data()$Player)))
  })
  output$select_player_reg <- renderUI({
    req(clean_data())
    selectInput("player_reg", "Sélectionner un joueur",
                choices = sort(unique(clean_data()$Player)))
  })

  # --- Nettoyage --------------------------------------------------------------
  observeEvent(input$btn_clean, {
    req(raw_data())
    tryCatch({
      df <- remove_negative_accelerations(raw_data())

      res_misuse  <- identify_misuse_errors(df, nb_outlier = input$nb_outlier)
      df          <- res_misuse$correct_points
      misuse_err_data(res_misuse$misuse_error)

      res_measure <- identify_measurement_errors(df,
                       neighb_dbscan = input$neighb_dbscan,
                       eps_dbscan    = input$eps_dbscan)
      df          <- res_measure$correct_points
      measurement_err_data(res_measure$measurement_error)

      clean_data(df)
      showNotification("\u2705 Nettoyage terminé !", type = "message")
    }, error = function(e) {
      showNotification(paste("\u274c Erreur :", e$message), type = "error", duration = 10)
    })
  })

  output$clean_summary <- renderText({
    req(raw_data(), clean_data())
    n_raw     <- nrow(raw_data())
    n_clean   <- nrow(clean_data())
    n_misuse  <- nrow(misuse_err_data())
    n_measure <- nrow(measurement_err_data())
    paste0(
      "Points initiaux         : ", n_raw,     "\n",
      "Erreurs d'utilisation   : ", n_misuse,  "\n",
      "Erreurs de mesure       : ", n_measure, "\n",
      "Points conservés        : ", n_clean,
      " (", round(n_clean / n_raw * 100, 1), " %)"
    )
  })

  output$plot_outliers_out <- renderPlot({
    req(clean_data(), input$player_clean)
    plot_outliers(
      correct_points    = clean_data(),
      measurement_error = measurement_err_data(),
      misuse_error      = misuse_err_data(),
      player            = input$player_clean
    )
  })

  output$table_measurement_error <- renderDT({
    req(measurement_err_data())
    datatable(measurement_err_data(),
              options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
  })
  output$table_misuse_error <- renderDT({
    req(misuse_err_data())
    datatable(misuse_err_data(),
              options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
  })

  # --- Régression -------------------------------------------------------------
  observeEvent(input$btn_regress, {
    req(clean_data())
    tryCatch({
      hi_pts <- identify_high_intensity_points(clean_data(),
                  dv = input$dv, n_max = input$n_max)
      hi_points_data(hi_pts)

      if (input$use_linear) {
        linear_results_data(compute_linear_regression(hi_pts, r2_threshold = input$r2_threshold))
      }

      if (input$use_quantile) {
        withProgress(message = "Calcul de la régression quantile…", value = 0.1, {
          q_res <- compute_quantile_regression(hi_pts)
          incProgress(0.7)
          quantile_results_data(q_res)
          quantile_summary_data(compute_quantile_summary(q_res))
        })
      }

      showNotification("\u2705 Profil A-V calculé !", type = "message")
    }, error = function(e) {
      showNotification(paste("\u274c Erreur :", e$message), type = "error", duration = 10)
    })
  })

  output$plot_linear_reg <- renderPlot({
    req(clean_data(), hi_points_data(), linear_results_data(), input$player_reg)
    plot_linear_regression(
      points         = clean_data(),
      hi_points      = hi_points_data(),
      linear_results = linear_results_data(),
      player         = input$player_reg
    )
  })

  output$plot_quantile_reg <- renderPlot({
    req(clean_data(), hi_points_data(),
        quantile_results_data(), quantile_summary_data(), input$player_reg)
    plot_quantile_regression(
      points           = clean_data(),
      hi_points        = hi_points_data(),
      quantile_results = quantile_results_data(),
      quantile_summary = quantile_summary_data(),
      player           = input$player_reg
    )
  })

  # --- Tableau & téléchargement des résultats ---------------------------------
  combined_results <- reactive({
    lin   <- linear_results_data()
    q_sum <- quantile_summary_data()
    if (is.null(lin) && is.null(q_sum)) return(NULL)
    if (!is.null(lin) && !is.null(q_sum)) return(left_join(lin, q_sum, by = "Player"))
    if (!is.null(lin))   return(lin)
    return(q_sum)
  })

  output$table_results <- renderDT({
    req(combined_results())
    res <- combined_results()
    numeric_cols <- names(res)[sapply(res, is.numeric)]
    datatable(res, options = list(scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = numeric_cols, digits = 3)
  })

  output$download_results <- downloadHandler(
    filename = function() paste0("ProfilAV_insitu_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(combined_results())
      write.csv(combined_results(), file, row.names = FALSE)
    }
  )
}

# =============================================================================
# LANCEMENT
# =============================================================================

shinyApp(ui, server)
