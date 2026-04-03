# ============================================================================
# R SHINY APP - ACCELERATION-SPEED PROFILING (OpenField GPS)
# Méthode JB Morin - Sans rank()
# ============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(dbscan)
library(quantreg)

# Opérateur null-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# FONCTIONS UTILITAIRES
# ============================================================================

# Lecture du fichier OpenField CSV (ignore les lignes de commentaires #)
read_openfield <- function(filepath, athlete_name = "Athlete") {
  lines  <- readLines(filepath, encoding = "UTF-8")
  n_skip <- which(!startsWith(trimws(lines), "#"))[1] - 1

  df <- read.table(
    filepath,
    skip             = n_skip,
    header           = TRUE,
    sep              = ";",
    dec              = ",",
    stringsAsFactors = FALSE,
    check.names      = FALSE,
    encoding         = "UTF-8"
  )

  required <- c("Timestamp", "Velocity", "Acceleration")
  missing  <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    stop(paste0(
      "Colonnes manquantes : ", paste(missing, collapse = ", "),
      "\nColonnes disponibles : ", paste(colnames(df), collapse = ", ")
    ))
  }

  df$Speed        <- as.numeric(df$Velocity) / 3.6   # km/h -> m/s
  df$Acceleration <- as.numeric(df$Acceleration)
  df$Timestamp    <- as.POSIXct(df$Timestamp, format = "%d/%m/%Y %H:%M:%OS")
  df$Date         <- as.Date(df$Timestamp)
  df$Player       <- athlete_name

  df <- df[!is.na(df$Speed) & !is.na(df$Acceleration), ]
  df[order(df$Timestamp), ]
}

# Méthode JB Morin : top-n accélérations par intervalle de vitesse (SANS rank())
identify_high_intensity <- function(df, dv = 0.3, n_max = 2) {
  df_pos <- df[df$Acceleration >= 0, ]
  df_pos$dv_interval <- floor(df_pos$Speed / dv)

  hi <- df_pos %>%
    group_by(Player, dv_interval) %>%
    slice_max(order_by = Acceleration, n = n_max, with_ties = FALSE) %>%
    ungroup()

  if (nrow(hi) == 0) return(hi)

  max_acc <- hi %>%
    group_by(Player) %>%
    summarise(max_Accel = max(Acceleration), .groups = "drop")

  hi <- hi %>% left_join(max_acc, by = "Player")

  max_speed <- hi %>%
    filter(Acceleration == max_Accel) %>%
    group_by(Player) %>%
    summarise(threshold_speed = max(Speed), .groups = "drop")

  hi %>%
    left_join(max_speed, by = "Player") %>%
    filter(Speed >= threshold_speed)
}

# Détection erreurs de mesure (DBSCAN)
detect_measurement_errors <- function(df, eps = 0.5, min_pts = 3) {
  df_sub <- df[df$Acceleration >= (5 - df$Speed), ]
  if (nrow(df_sub) == 0) return(integer(0))

  bad_idx <- c()
  for (pl in unique(df_sub$Player)) {
    pts <- df_sub[df_sub$Player == pl, c("Speed", "Acceleration")]
    if (nrow(pts) < min_pts) next
    res   <- dbscan::dbscan(as.matrix(pts), eps = eps, minPts = min_pts)
    noise <- which(res$cluster == 0)   # 0 = bruit dans le package R dbscan
    if (length(noise) > 0) {
      bad_idx <- c(bad_idx, as.integer(rownames(pts)[noise]))
    }
  }
  bad_idx
}

# Détection erreurs de mauvaise utilisation (formule JB Morin)
detect_misuse_errors <- function(df, nb_outlier = 10) {
  df_pos <- df[df$Acceleration >= 0, ]
  threshold <- 10.93 - (10.93 / 10.5) * df_pos$Speed
  df_pos$is_misuse <- df_pos$Acceleration >= threshold

  df_pos %>%
    filter(is_misuse) %>%
    group_by(Player, Date) %>%
    mutate(n_errors = n()) %>%
    ungroup() %>%
    filter(n_errors >= nb_outlier)
}

# Régression linéaire par joueur
compute_linear_regression <- function(hi) {
  hi %>%
    group_by(Player) %>%
    do({
      m <- lm(Acceleration ~ Speed, data = .)
      data.frame(
        a0_linear = coef(m)[[1]],
        s0_linear = -coef(m)[[1]] / coef(m)[[2]],
        r_squared = summary(m)$r.squared
      )
    }) %>%
    ungroup()
}

# Régression quantile par joueur
compute_quantile_regression <- function(hi, quantiles = seq(0.05, 0.95, 0.01)) {
  result_list <- list()
  for (pl in unique(hi$Player)) {
    pd <- hi[hi$Player == pl, ]
    if (nrow(pd) < 5) next
    rows <- lapply(quantiles, function(q) {
      tryCatch({
        m     <- rq(Acceleration ~ Speed, tau = q, data = pd)
        a0    <- coef(m)[[1]]
        slope <- coef(m)[[2]]
        data.frame(Player = pl, q = q, a0 = a0, slope = slope,
                   s0 = -a0 / slope)
      }, error = function(e) NULL)
    })
    result_list[[pl]] <- bind_rows(rows)
  }
  bind_rows(result_list)
}

# Résumé régression quantile (moyenne des a0 et s0)
summarise_quantile <- function(qreg) {
  qreg %>%
    group_by(Player) %>%
    summarise(
      a0_quantile = mean(a0, na.rm = TRUE),
      std_a0      = sd(a0,   na.rm = TRUE),
      s0_quantile = mean(s0, na.rm = TRUE),
      std_s0      = sd(s0,   na.rm = TRUE),
      .groups = "drop"
    )
}

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "OpenField GPS - Profil A-V"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Paramètres",  tabName = "upload",     icon = icon("upload")),
      menuItem("Aperçu des données",   tabName = "overview",   icon = icon("table")),
      menuItem("Détection Outliers",   tabName = "outliers",   icon = icon("exclamation-triangle")),
      menuItem("Régressions",          tabName = "regression", icon = icon("chart-line")),
      menuItem("Export",               tabName = "export",     icon = icon("download"))
    )
  ),

  dashboardBody(
    tabItems(

      # ------------------------------------------------------------------
      # TAB 1 : UPLOAD & PARAMÈTRES
      # ------------------------------------------------------------------
      tabItem(tabName = "upload",
        fluidRow(
          box(title = "Chargement du fichier OpenField", status = "primary",
              solidHeader = TRUE, width = 12,
              fileInput("file_upload", "Sélectionner le fichier CSV OpenField",
                        accept = ".csv"),
              textInput("athlete_name", "Nom de l'athlète :", value = "Athlète")
          )
        ),
        fluidRow(
          box(title = "Filtrage interactif", status = "info",
              solidHeader = TRUE, width = 12,
              sliderInput("speed_range", "Plage de vitesse (m/s) :",
                          min = 0, max = 11, value = c(0, 11), step = 0.1),
              sliderInput("accel_range", "Plage d'accélération (m/s²) :",
                          min = 0, max = 11, value = c(0, 11), step = 0.1)
          )
        ),
        fluidRow(
          box(title = "Paramètres d'analyse", status = "warning",
              solidHeader = TRUE, width = 6,
              numericInput("dv",      "Intervalle vitesse dv :",    value = 0.3, min = 0.1, max = 1,  step = 0.1),
              numericInput("n_max",   "Points par intervalle n_max :", value = 2, min = 1,   max = 10, step = 1),
              numericInput("nb_outlier", "Seuil mauvaise utilisation :", value = 10, min = 1, max = 50, step = 1)
          ),
          box(title = "Paramètres DBSCAN", status = "warning",
              solidHeader = TRUE, width = 6,
              numericInput("eps",     "DBSCAN eps :",          value = 0.5, min = 0.1, max = 2,  step = 0.1),
              numericInput("min_pts", "DBSCAN min points :",   value = 3,   min = 1,   max = 10, step = 1)
          )
        ),
        fluidRow(
          box(width = 12,
              actionButton("process_data", "Analyser les données",
                           class = "btn-success btn-lg", width = "100%")
          )
        ),
        fluidRow(
          box(title = "Statut", status = "info", solidHeader = TRUE, width = 12,
              verbatimTextOutput("status_message"))
        )
      ),

      # ------------------------------------------------------------------
      # TAB 2 : APERÇU
      # ------------------------------------------------------------------
      tabItem(tabName = "overview",
        fluidRow(
          box(title = "Résumé des données", status = "info",
              solidHeader = TRUE, width = 12,
              verbatimTextOutput("data_summary"))
        ),
        fluidRow(
          box(title = "Données filtrées (100 premières lignes)", status = "info",
              solidHeader = TRUE, width = 12,
              dataTableOutput("data_table"))
        )
      ),

      # ------------------------------------------------------------------
      # TAB 3 : OUTLIERS
      # ------------------------------------------------------------------
      tabItem(tabName = "outliers",
        fluidRow(
          box(title = "Résumé des outliers", status = "warning",
              solidHeader = TRUE, width = 12,
              verbatimTextOutput("outliers_summary"))
        ),
        fluidRow(
          box(title = "Nuage de points – outliers", status = "warning",
              solidHeader = TRUE, width = 12,
              plotlyOutput("plot_outliers", height = "500px"))
        )
      ),

      # ------------------------------------------------------------------
      # TAB 4 : RÉGRESSIONS
      # ------------------------------------------------------------------
      tabItem(tabName = "regression",
        fluidRow(
          box(title = "Régression Linéaire", status = "success",
              solidHeader = TRUE, width = 6,
              dataTableOutput("linear_table")),
          box(title = "Régression Quantile (résumé)", status = "success",
              solidHeader = TRUE, width = 6,
              dataTableOutput("quantile_table"))
        ),
        fluidRow(
          box(title = "Graphique des régressions", status = "success",
              solidHeader = TRUE, width = 12,
              plotlyOutput("plot_regression", height = "600px"))
        )
      ),

      # ------------------------------------------------------------------
      # TAB 5 : EXPORT
      # ------------------------------------------------------------------
      tabItem(tabName = "export",
        fluidRow(
          box(title = "Télécharger les résultats", status = "primary",
              solidHeader = TRUE, width = 12,
              fluidRow(
                column(6,
                  downloadButton("dl_filtered_csv",  "Données filtrées (CSV)"),  br(), br(),
                  downloadButton("dl_linear_csv",     "Régression linéaire (CSV)"), br(), br(),
                  downloadButton("dl_quantile_csv",   "Régression quantile (CSV)")
                ),
                column(6,
                  downloadButton("dl_plot_outliers",  "Graphique outliers (PNG)"),  br(), br(),
                  downloadButton("dl_plot_regression","Graphique régressions (PNG)")
                )
              )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVEUR
# ============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    raw           = NULL,
    filtered      = NULL,
    clean         = NULL,
    misuse        = NULL,
    measurement   = NULL,
    hi            = NULL,
    lin_reg       = NULL,
    quant_reg     = NULL,
    quant_summary = NULL,
    status        = "En attente d'un fichier CSV..."
  )

  # ---- Chargement automatique à l'upload --------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      rv$raw    <- read_openfield(input$file_upload$datapath, input$athlete_name)
      rv$status <- paste0("✅ Fichier chargé : ", nrow(rv$raw), " enregistrements. Cliquez sur 'Analyser' pour continuer.")
    }, error = function(e) {
      rv$status <- paste0("❌ Erreur de chargement : ", e$message)
    })
  })

  # ---- Analyse complète -------------------------------------------------
  observeEvent(input$process_data, {
    req(rv$raw)

    tryCatch({
      rv$status <- "🔄 Filtrage des données..."
      df <- rv$raw

      # Filtrage interactif par vitesse et accélération
      df <- df[df$Speed        >= input$speed_range[1] & df$Speed        <= input$speed_range[2], ]
      df <- df[df$Acceleration >= input$accel_range[1] & df$Acceleration <= input$accel_range[2], ]
      rv$filtered <- df

      rv$status <- "🔍 Détection des outliers..."

      # Mauvaise utilisation (JB Morin)
      misuse <- detect_misuse_errors(df, input$nb_outlier)
      rv$misuse <- misuse

      # Suppression des dates avec trop d'erreurs
      bad_dates <- unique(misuse$Date)
      df_clean  <- df[!df$Date %in% bad_dates, ]

      # Erreurs de mesure (DBSCAN)
      bad_idx <- detect_measurement_errors(df_clean, input$eps, input$min_pts)
      rv$measurement <- df_clean[rownames(df_clean) %in% as.character(bad_idx), ]
      if (length(bad_idx) > 0) {
        df_clean <- df_clean[!rownames(df_clean) %in% as.character(bad_idx), ]
      }
      rv$clean <- df_clean

      rv$status <- "📊 Identification des points haute intensité (JB Morin)..."

      hi <- identify_high_intensity(df_clean, input$dv, input$n_max)
      rv$hi <- hi

      if (nrow(hi) < 5) {
        rv$status <- "⚠️ Pas assez de points haute intensité. Ajustez les paramètres."
        return()
      }

      rv$status <- "📈 Calcul des régressions..."

      rv$lin_reg       <- compute_linear_regression(hi)
      qreg             <- compute_quantile_regression(hi)
      rv$quant_reg     <- qreg
      rv$quant_summary <- summarise_quantile(qreg)

      rv$status <- "✅ Analyse terminée avec succès !"

    }, error = function(e) {
      rv$status <- paste0("❌ Erreur : ", e$message)
    })
  })

  # ---- Statut -----------------------------------------------------------
  output$status_message <- renderText({ rv$status })

  # ---- Résumé données ---------------------------------------------------
  output$data_summary <- renderPrint({
    df <- rv$clean
    if (is.null(df)) { cat("Aucune donnée analysée.\n"); return() }
    cat("=== RÉSUMÉ DES DONNÉES NETTOYÉES ===\n")
    cat("Enregistrements :", nrow(df), "\n")
    cat("Athlète :", unique(df$Player), "\n")
    cat("Période :", format(min(df$Date)), "→", format(max(df$Date)), "\n\n")
    cat("Vitesse (m/s) :\n"); print(summary(df$Speed))
    cat("\nAccélération (m/s²) :\n"); print(summary(df$Acceleration))
  })

  # ---- Tableau données --------------------------------------------------
  output$data_table <- renderDataTable({
    df <- rv$clean
    if (is.null(df)) return(data.frame())
    head(df[, c("Timestamp", "Speed", "Acceleration")], 100)
  })

  # ---- Résumé outliers --------------------------------------------------
  output$outliers_summary <- renderPrint({
    cat("=== OUTLIERS DÉTECTÉS ===\n")
    cat("Erreurs mauvaise utilisation :", nrow(rv$misuse      %||% data.frame()), "\n")
    cat("Erreurs de mesure (DBSCAN)   :", nrow(rv$measurement %||% data.frame()), "\n")
  })

  # ---- Graphique outliers -----------------------------------------------
  output$plot_outliers <- renderPlotly({
    df <- rv$filtered
    if (is.null(df)) return(NULL)

    p <- ggplot(df, aes(x = Speed, y = Acceleration)) +
      geom_point(alpha = 0.4, size = 1.5, color = "steelblue") +
      coord_cartesian(xlim = c(0, 11), ylim = c(0, 11)) +
      theme_minimal() +
      labs(title = "Détection des Outliers", x = "Vitesse (m/s)", y = "Accélération (m/s²)")

    meas <- rv$measurement
    if (!is.null(meas) && nrow(meas) > 0) {
      p <- p + geom_point(data = meas, aes(x = Speed, y = Acceleration),
                          color = "red", size = 3, shape = 1, stroke = 1.2)
    }
    mis <- rv$misuse
    if (!is.null(mis) && nrow(mis) > 0) {
      p <- p + geom_point(data = mis, aes(x = Speed, y = Acceleration),
                          color = "black", size = 2.5, shape = 2)
    }
    ggplotly(p)
  })

  # ---- Tableau régression linéaire --------------------------------------
  output$linear_table <- renderDataTable({
    lr <- rv$lin_reg
    if (is.null(lr)) return(data.frame())
    lr %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  })

  # ---- Tableau régression quantile --------------------------------------
  output$quantile_table <- renderDataTable({
    qs <- rv$quant_summary
    if (is.null(qs)) return(data.frame())
    qs %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  })

  # ---- Graphique régressions --------------------------------------------
  output$plot_regression <- renderPlotly({
    df  <- rv$clean
    hi  <- rv$hi
    lr  <- rv$lin_reg
    qr  <- rv$quant_reg
    qs  <- rv$quant_summary

    if (is.null(df) || is.null(hi)) return(NULL)

    p <- ggplot(df, aes(x = Speed, y = Acceleration)) +
      geom_point(alpha = 0.3, size = 1.5, color = "steelblue") +
      geom_point(data = hi, aes(x = Speed, y = Acceleration),
                 color = "red", size = 2.5, alpha = 0.9) +
      coord_cartesian(xlim = c(0, 11), ylim = c(0, 11)) +
      theme_minimal() +
      labs(title = "Profil Accélération–Vitesse",
           x = "Vitesse (m/s)", y = "Accélération (m/s²)",
           caption = "Rouge = points haute intensité | Bleu = régression linéaire | Gris = droites quantiles")

    # Droites quantiles (toutes)
    if (!is.null(qr) && nrow(qr) > 0) {
      for (q_val in seq(0.1, 0.9, 0.1)) {
        row <- qr[abs(qr$q - q_val) < 0.005, ]
        if (nrow(row) == 0) next
        a0    <- row$a0[1]; slope <- row$slope[1]
        if (is.na(a0) || is.na(slope) || slope >= 0) next
        s0 <- -a0 / slope
        p  <- p + geom_segment(x = 0, xend = s0, y = a0, yend = 0,
                               color = "grey70", linewidth = 0.4, linetype = "dashed")
      }
    }

    # Droite régression quantile (moyenne)
    if (!is.null(qs) && nrow(qs) > 0) {
      a0_q <- qs$a0_quantile[1]; s0_q <- qs$s0_quantile[1]
      if (!is.na(a0_q) && !is.na(s0_q) && s0_q > 0) {
        p <- p + geom_segment(x = 0, xend = s0_q, y = a0_q, yend = 0,
                              color = "darkorange", linewidth = 1.2, linetype = "dashed")
      }
    }

    # Droite régression linéaire
    if (!is.null(lr) && nrow(lr) > 0) {
      a0_l <- lr$a0_linear[1]; s0_l <- lr$s0_linear[1]
      if (!is.na(a0_l) && !is.na(s0_l) && s0_l > 0) {
        p <- p + geom_segment(x = 0, xend = s0_l, y = a0_l, yend = 0,
                              color = "blue", linewidth = 1.2)
      }
    }

    ggplotly(p)
  })

  # ---- Téléchargements --------------------------------------------------
  output$dl_filtered_csv <- downloadHandler(
    filename = function() paste0("donnees_filtrees_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- rv$clean
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_linear_csv <- downloadHandler(
    filename = function() paste0("regression_lineaire_", Sys.Date(), ".csv"),
    content  = function(file) {
      lr <- rv$lin_reg
      if (!is.null(lr)) write.csv(lr, file, row.names = FALSE)
    }
  )

  output$dl_quantile_csv <- downloadHandler(
    filename = function() paste0("regression_quantile_", Sys.Date(), ".csv"),
    content  = function(file) {
      qs <- rv$quant_summary
      if (!is.null(qs)) write.csv(qs, file, row.names = FALSE)
    }
  )

  output$dl_plot_outliers <- downloadHandler(
    filename = function() paste0("outliers_", Sys.Date(), ".png"),
    content  = function(file) {
      df <- rv$filtered
      if (is.null(df)) return()
      p <- ggplot(df, aes(x = Speed, y = Acceleration)) +
        geom_point(alpha = 0.4, size = 1.5, color = "steelblue") +
        coord_cartesian(xlim = c(0, 11), ylim = c(0, 11)) +
        theme_minimal() +
        labs(title = "Détection des Outliers",
             x = "Vitesse (m/s)", y = "Accélération (m/s²)")
      meas <- rv$measurement
      if (!is.null(meas) && nrow(meas) > 0)
        p <- p + geom_point(data = meas, aes(x = Speed, y = Acceleration),
                            color = "red", size = 3, shape = 1)
      mis <- rv$misuse
      if (!is.null(mis) && nrow(mis) > 0)
        p <- p + geom_point(data = mis, aes(x = Speed, y = Acceleration),
                            color = "black", size = 2.5, shape = 2)
      ggsave(file, plot = p, width = 10, height = 7, dpi = 150)
    }
  )

  output$dl_plot_regression <- downloadHandler(
    filename = function() paste0("regression_", Sys.Date(), ".png"),
    content  = function(file) {
      df <- rv$clean; hi <- rv$hi; lr <- rv$lin_reg; qs <- rv$quant_summary
      if (is.null(df) || is.null(hi)) return()
      p <- ggplot(df, aes(x = Speed, y = Acceleration)) +
        geom_point(alpha = 0.3, size = 1.5, color = "steelblue") +
        geom_point(data = hi, aes(x = Speed, y = Acceleration),
                   color = "red", size = 2.5, alpha = 0.9) +
        coord_cartesian(xlim = c(0, 11), ylim = c(0, 11)) +
        theme_minimal() +
        labs(title = "Profil Accélération–Vitesse",
             x = "Vitesse (m/s)", y = "Accélération (m/s²)")
      if (!is.null(qs) && nrow(qs) > 0) {
        a0_q <- qs$a0_quantile[1]; s0_q <- qs$s0_quantile[1]
        if (!is.na(a0_q) && !is.na(s0_q) && s0_q > 0)
          p <- p + geom_segment(x = 0, xend = s0_q, y = a0_q, yend = 0,
                                color = "darkorange", linewidth = 1.2)
      }
      if (!is.null(lr) && nrow(lr) > 0) {
        a0_l <- lr$a0_linear[1]; s0_l <- lr$s0_linear[1]
        if (!is.na(a0_l) && !is.na(s0_l) && s0_l > 0)
          p <- p + geom_segment(x = 0, xend = s0_l, y = a0_l, yend = 0,
                                color = "blue", linewidth = 1.2)
      }
      ggsave(file, plot = p, width = 10, height = 7, dpi = 150)
    }
  )
}

shinyApp(ui, server)
