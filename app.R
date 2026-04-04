# app.R - Dashboard Shiny - Profilage Accélération-Vitesse
# Conversion du pipeline Python (N. Miguens) en R/Shiny

# Résoudre le répertoire contenant app.R.
# Fonctionne pour : Rscript app.R, RStudio "Run App", shiny::runApp(),
# source("app.R"), et double-clic sur app.R (Windows/macOS).
.app_dir <- local({

  .norm <- function(p) {
    tryCatch(
      normalizePath(p, winslash = "/", mustWork = FALSE),
      error = function(e) NULL
    )
  }
  .ok <- function(d) !is.null(d) && nzchar(d)

  # Priorité 1 : argument --file= (Rscript app.R depuis un autre répertoire)
  rarg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(rarg) > 0) {
    d <- .norm(dirname(sub("^--file=", "", rarg[1])))
    if (.ok(d)) return(d)
  }

  # Priorité 2 : rstudioapi — getSourceEditorContext() est le plus fiable
  # pour le bouton "Run App" (pas besoin de requireNamespace ; tryCatch suffit).
  d <- tryCatch({
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) .norm(dirname(p)) else NULL
  }, error = function(e) NULL)
  if (.ok(d)) return(d)

  d <- tryCatch({
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) .norm(dirname(p)) else NULL
  }, error = function(e) NULL)
  if (.ok(d)) return(d)

  # Priorité 3 : fichier en cours de source() dans la pile d'appels
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile) && nzchar(ofile)) {
      d <- .norm(dirname(ofile))
      if (.ok(d)) return(d)
    }
  }

  # Priorité 4 : répertoire de travail courant (dernier recours)
  .norm(getwd())
})

# Charger les variables globales et les packages
source(file.path(.app_dir, "global.R"))
source(file.path(.app_dir, "functions_outliers.R"))
source(file.path(.app_dir, "functions_regression.R"))

# ============================================================
# INTERFACE UTILISATEUR
# ============================================================

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Profilage Acc-Vit"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Données",     tabName = "tab_data",       icon = icon("table")),
      menuItem("Outliers",    tabName = "tab_outliers",   icon = icon("filter")),
      menuItem("Régression",  tabName = "tab_regression", icon = icon("chart-line")),
      menuItem("Résultats",   tabName = "tab_results",    icon = icon("clipboard-list"))
    ),

    hr(),

    # ----- Upload CSV -----
    fileInput(
      inputId  = "csv_file",
      label    = "Charger un fichier CSV",
      multiple = FALSE,
      accept   = c("text/csv", ".csv"),
      buttonLabel = "Parcourir...",
      placeholder = "Aucun fichier sélectionné"
    ),

    # ----- Nom du joueur (optionnel) -----
    textInput(
      inputId     = "player_name",
      label       = "Nom du joueur (optionnel)",
      placeholder = "Extrait automatiquement du fichier"
    ),

    hr(),

    # ----- Paramètres outliers -----
    h5("Paramètres Outliers", style = "padding-left: 10px; color: #aaa;"),
    numericInput("nb_outlier",    "Seuil erreur utilisation",  value = 10, min = 1, step = 1),
    numericInput("eps_dbscan",    "Epsilon DBSCAN",            value = 0.5, min = 0.01, step = 0.05),
    numericInput("neighb_dbscan", "Min voisins DBSCAN",        value = 3,   min = 1,    step = 1),

    hr(),

    # ----- Filtres qualité GPS -----
    h5("Filtres qualité GPS", style = "padding-left: 10px; color: #aaa;"),
    numericInput("max_hdop",    "HDOP maximum (précision GPS)",       value = 5,  min = 0.1, step = 0.1),
    numericInput("min_quality", "Qualité positionnelle min (%)",      value = 0,  min = 0,   max = 100, step = 5),

    hr(),

    # ----- Paramètres régression -----
    h5("Paramètres Régression", style = "padding-left: 10px; color: #aaa;"),
    numericInput("dv",    "Intervalle vitesse dv (m/s)", value = 0.3, min = 0.05, step = 0.05),
    numericInput("n_max", "Points max / intervalle",     value = 2,   min = 1,    step = 1),

    hr(),

    actionButton(
      inputId = "btn_run",
      label   = "Lancer l'analyse",
      icon    = icon("play"),
      class   = "btn-primary",
      style   = "width: 90%; margin: 5px auto; display: block;"
    )
  ),

  dashboardBody(

    # ---- CSS splash ----
    tags$head(tags$style(HTML("
      .splash-title {
        font-size: 2.4em;
        font-weight: bold;
        text-align: center;
        margin-top: 40px;
        margin-bottom: 10px;
        color: #2c3e50;
      }
      .splash-sub {
        text-align: center;
        color: #7f8c8d;
        font-size: 1.1em;
        margin-bottom: 40px;
      }
    "))),

    tabItems(

      # ---- Onglet Données ----
      tabItem(
        tabName = "tab_data",
        fluidRow(
          box(
            title = "Informations sur le fichier chargé",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("file_info")
          )
        ),
        fluidRow(
          box(
            title = "Aperçu des données brutes",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("raw_data_table")
          )
        ),
        fluidRow(
          box(
            title = "Scatter plot - Données brutes (Vitesse vs Accélération)",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("raw_scatter_plot", height = "450px")
          )
        )
      ),

      # ---- Onglet Outliers ----
      tabItem(
        tabName = "tab_outliers",
        fluidRow(
          valueBoxOutput("vbox_total",       width = 3),
          valueBoxOutput("vbox_negative",    width = 3),
          valueBoxOutput("vbox_clean",       width = 3),
          valueBoxOutput("vbox_outliers",    width = 3)
        ),
        fluidRow(
          box(
            title = "Résumé du nettoyage",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("cleaning_summary")
          )
        ),
        fluidRow(
          box(
            title = "Filtres d'affichage",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            uiOutput("outlier_filters_ui")
          )
        ),
        fluidRow(
          box(
            title = "Scatter plot - Points propres et outliers",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("outlier_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Erreurs d'utilisation — Sessions exclues",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            DT::dataTableOutput("misuse_table"),
            hr(),
            downloadButton("download_misuse", "Télécharger erreurs utilisation", class = "btn-sm btn-danger")
          ),
          box(
            title = "Erreurs de mesure (DBSCAN) — Points exclus",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            DT::dataTableOutput("measurement_table"),
            hr(),
            downloadButton("download_measurement", "Télécharger erreurs mesure", class = "btn-sm btn-warning")
          )
        ),
        fluidRow(
          box(
            title = "Statistiques par joueur",
            width = 9,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("player_stats_table")
          ),
          box(
            title = "Données propres",
            width = 3,
            status = "success",
            solidHeader = TRUE,
            p("Télécharger les données après nettoyage (utilisées pour la régression)."),
            downloadButton("download_clean", "Télécharger données propres", class = "btn-sm btn-success")
          )
        )
      ),

      # ---- Onglet Régression ----
      tabItem(
        tabName = "tab_regression",
        fluidRow(
          box(
            title = "Sélectionner un joueur",
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            selectInput(
              inputId  = "player_select",
              label    = "Joueur affiché :",
              choices  = c(),
              selected = NULL,
              width    = "300px"
            )
          )
        ),
        fluidRow(
          box(
            title = "Régression linéaire",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("linear_plot", height = "650px")
          )
        ),
        fluidRow(
          box(
            title = "Régression quantile",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("quantile_plot", height = "650px")
          )
        )
      ),

      # ---- Onglet Résultats ----
      tabItem(
        tabName = "tab_results",
        fluidRow(
          box(
            title = "Tableau des résultats (a0 et s0)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("results_table"),
            hr(),
            downloadButton("btn_download", "Télécharger les résultats (CSV)")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVEUR
# ============================================================

server <- function(input, output, session) {

  # ---- Splash screen ----
  session$onFlushed(function() {
    showModal(modalDialog(
      title = NULL,
      div(class = "splash-title", "\U0001F3C3 Bienvenue au test de la patience !"),
      div(class = "splash-sub",  "Appuyez sur Entrée ou cliquez sur le bouton pour commencer"),
      footer = actionButton(
        "btn_splash_ok", "Commencer \u2192",
        class = "btn-primary btn-lg",
        style = "display:block; margin: 0 auto;"
      ),
      size       = "m",
      easyClose  = FALSE,
      tags$script(HTML("
        $(document).on('keypress', function(e) {
          if (e.which === 13) { $('#btn_splash_ok').click(); }
        });
      "))
    ))
  }, once = TRUE)

  observeEvent(input$btn_splash_ok, { removeModal() })

  # ---- Réactifs ----

  # Données brutes lues depuis le CSV
  raw_data <- reactive({
    req(input$csv_file)

    withProgress(message = "Lecture du fichier CSV...", value = 0.1, {
      tryCatch({
        player_name <- trimws(input$player_name)
        if (nchar(player_name) == 0) player_name <- NULL

        df <- read_openfield_csv(input$csv_file$datapath, player_name,
                                 min_quality = input$min_quality,
                                 max_hdop    = input$max_hdop)
        setProgress(1, message = "Fichier chargé avec succès.")
        df
      }, error = function(e) {
        showNotification(
          paste("Erreur lors de la lecture du CSV :", conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        NULL
      })
    })
  })

  # Résultats du pipeline outliers (déclenchés par le bouton)
  outlier_results <- eventReactive(input$btn_run, {
    req(raw_data())

    withProgress(message = "Détection des outliers...", value = 0, {
      setProgress(0.2)
      tryCatch({
        res <- run_outlier_detection(
          raw_data(),
          nb_outlier    = input$nb_outlier,
          eps_DBSCAN    = input$eps_dbscan,
          neighb_DBSCAN = input$neighb_dbscan
        )
        setProgress(1, message = "Détection terminée.")
        res
      }, error = function(e) {
        showNotification(
          paste("Erreur détection outliers :", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      })
    })
  })

  # Résultats du pipeline régression
  regression_results <- eventReactive(input$btn_run, {
    req(outlier_results())

    withProgress(message = "Calcul des régressions...", value = 0, {
      setProgress(0.2)
      tryCatch({
        res <- run_regression(
          outlier_results()$correct_points,
          dv    = input$dv,
          n_max = input$n_max
        )
        setProgress(1, message = "Régressions calculées.")
        res
      }, error = function(e) {
        showNotification(
          paste("Erreur régression :", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      })
    })
  })

  # Mettre à jour la liste des joueurs pour la sélection
  observeEvent(regression_results(), {
    req(regression_results())
    players <- unique(regression_results()$linear_results$Player)
    updateSelectInput(session, "player_select", choices = players, selected = players[1])
  })

  # ---- Onglet Données ----

  output$file_info <- renderPrint({
    req(raw_data())
    df <- raw_data()
    cat("Fichier              :", input$csv_file$name, "\n")
    cat("Nombre de lignes     :", nrow(df), "\n")
    cat("Colonnes disponibles :", paste(colnames(df), collapse = ", "), "\n")
    cat("Joueur(s) détecté(s) :", paste(unique(df$Player), collapse = ", "), "\n")
    cat("Plage de vitesse     :", round(min(df$Speed, na.rm = TRUE), 2),
        "à", round(max(df$Speed, na.rm = TRUE), 2), "m/s\n")
    cat("Plage d'accélération :", round(min(df$Acceleration, na.rm = TRUE), 2),
        "à", round(max(df$Acceleration, na.rm = TRUE), 2), "m/s²\n")
  })

  output$raw_data_table <- DT::renderDataTable({
    req(raw_data())
    DT::datatable(
      head(raw_data(), 500),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Aperçu des 500 premières lignes"
    )
  })

  output$raw_scatter_plot <- renderPlotly({
    req(raw_data())
    df <- raw_data()

    p <- ggplot(df, aes(x = Speed, y = Acceleration, color = Player)) +
      geom_point(alpha = 0.3, size = 1) +
      labs(
        x     = "Vitesse (m/s)",
        y     = "Accélération (m/s²)",
        title = "Données brutes : Vitesse vs Accélération"
      ) +
      xlim(0, NA) + ylim(0, NA) +
      theme_minimal()

    ggplotly(p)
  })

  # ---- Onglet Outliers ----

  # Reactive combinant tous les points étiquetés (propres + outliers)
  all_outlier_pts <- reactive({
    req(outlier_results())
    res <- outlier_results()

    clean         <- res$correct_points
    clean$type    <- "Points propres"

    misuse        <- res$misuse_error
    if (nrow(misuse) > 0)  misuse$type  <- "Erreur utilisation"

    measure       <- res$measurement_error
    if (nrow(measure) > 0) measure$type <- "Erreur mesure (DBSCAN)"

    needed <- c("Speed", "Acceleration", "Player", "Date", "type")
    dplyr::bind_rows(
      clean[,  intersect(needed, colnames(clean)),  drop = FALSE],
      if (nrow(misuse)  > 0) misuse[,  intersect(needed, colnames(misuse)),  drop = FALSE] else NULL,
      if (nrow(measure) > 0) measure[, intersect(needed, colnames(measure)), drop = FALSE] else NULL
    )
  })

  # Reactive : points après application de tous les filtres actifs
  filtered_outlier_pts <- reactive({
    req(all_outlier_pts())
    df <- all_outlier_pts()

    if (!is.null(input$speed_range))
      df <- df[df$Speed >= input$speed_range[1] & df$Speed <= input$speed_range[2], , drop = FALSE]

    if (!is.null(input$accel_range))
      df <- df[df$Acceleration >= input$accel_range[1] & df$Acceleration <= input$accel_range[2], , drop = FALSE]

    if (!is.null(input$type_filter) && length(input$type_filter) > 0)
      df <- df[df$type %in% input$type_filter, , drop = FALSE]

    if (!is.null(input$player_filter) && nzchar(input$player_filter))
      df <- df[df$Player == input$player_filter, , drop = FALSE]

    if (!is.null(input$date_filter) && length(input$date_filter) > 0)
      df <- df[format(df$Date, "%d/%m/%Y") %in% input$date_filter, , drop = FALSE]

    df
  })

  # Filtres dynamiques (speed, accélération, type, joueur, dates)
  output$outlier_filters_ui <- renderUI({
    req(all_outlier_pts())
    df <- all_outlier_pts()

    speed_max   <- ceiling(max(df$Speed,        na.rm = TRUE) * 2) / 2
    accel_max   <- ceiling(max(df$Acceleration, na.rm = TRUE) * 2) / 2
    players     <- sort(unique(df$Player))
    all_dates   <- sort(unique(df$Date))
    date_labels <- format(all_dates, "%d/%m/%Y")

    tagList(
      fluidRow(
        column(4,
          sliderInput("speed_range", "Plage de vitesse (m/s) :",
                      min = 0, max = speed_max, value = c(0, speed_max), step = 0.5)
        ),
        column(4,
          sliderInput("accel_range", "Plage d'accélération (m/s²) :",
                      min = 0, max = accel_max, value = c(0, accel_max), step = 0.5)
        ),
        column(4,
          checkboxGroupInput(
            "type_filter",
            "Types de points affichés :",
            choices  = c("Points propres", "Erreur utilisation", "Erreur mesure (DBSCAN)"),
            selected = c("Points propres", "Erreur utilisation", "Erreur mesure (DBSCAN)"),
            inline   = TRUE
          )
        )
      ),
      fluidRow(
        if (length(players) > 1) {
          column(3,
            selectInput("player_filter", "Joueur :",
                        choices  = c("Tous" = "", players),
                        selected = "")
          )
        } else NULL,
        column(if (length(players) > 1) 9 else 12,
          checkboxGroupInput(
            "date_filter",
            "Sessions à afficher :",
            choices  = setNames(date_labels, date_labels),
            selected = date_labels,
            inline   = TRUE
          )
        )
      )
    )
  })

  output$vbox_total <- renderValueBox({
    req(raw_data())
    valueBox(
      value    = nrow(raw_data()),
      subtitle = "Points bruts",
      icon     = icon("database"),
      color    = "blue"
    )
  })

  output$vbox_negative <- renderValueBox({
    res <- outlier_results()
    n <- if (!is.null(res)) res$n_negative else "—"
    valueBox(
      value    = n,
      subtitle = "Acc. négatives",
      icon     = icon("minus-circle"),
      color    = "purple"
    )
  })

  output$vbox_clean <- renderValueBox({
    res <- outlier_results()
    n <- if (!is.null(res)) nrow(res$correct_points) else "—"
    valueBox(
      value    = n,
      subtitle = "Points propres",
      icon     = icon("check-circle"),
      color    = "green"
    )
  })

  output$vbox_outliers <- renderValueBox({
    res       <- outlier_results()
    n_misuse  <- if (!is.null(res)) nrow(res$misuse_error)      else 0
    n_measure <- if (!is.null(res)) nrow(res$measurement_error) else 0
    valueBox(
      value    = n_misuse + n_measure,
      subtitle = paste0("Outliers  (util.: ", n_misuse, "  /  DBSCAN: ", n_measure, ")"),
      icon     = icon("exclamation-triangle"),
      color    = "red"
    )
  })

  # Résumé textuel du nettoyage étape par étape
  output$cleaning_summary <- renderPrint({
    req(outlier_results())
    res     <- outlier_results()
    n_raw   <- nrow(raw_data())
    n_neg   <- res$n_negative
    n_mis   <- nrow(res$misuse_error)
    n_dbs   <- nrow(res$measurement_error)
    n_clean <- nrow(res$correct_points)

    cat("══════════════════════════════════════════════════════\n")
    cat("  RÉSUMÉ DU NETTOYAGE DES DONNÉES\n")
    cat("══════════════════════════════════════════════════════\n")
    cat(sprintf("  Points bruts (CSV)                  : %6d\n", n_raw))
    cat(sprintf("  — Accélérations négatives supprimées: %6d\n", n_neg))

    cat(sprintf("\n  — Erreurs d'utilisation             : %6d points détectés\n", n_mis))
    if (n_mis > 0) {
      sess <- unique(res$misuse_error[, c("Player", "Date"), drop = FALSE])
      sess <- sess[order(sess$Player, sess$Date), ]
      cat(sprintf("       Sessions entières exclues : %d\n", nrow(sess)))
      for (i in seq_len(nrow(sess))) {
        n_pts <- sum(res$misuse_error$Player == sess$Player[i] &
                       res$misuse_error$Date == sess$Date[i])
        cat(sprintf("         • %-25s  %s  (%d points au-dessus du seuil)\n",
                    sess$Player[i],
                    format(sess$Date[i], "%d/%m/%Y"),
                    n_pts))
      }
    }

    cat(sprintf("\n  — Erreurs de mesure (DBSCAN)        : %6d points exclus\n", n_dbs))
    if (n_dbs > 0) {
      by_player <- tapply(seq_len(n_dbs), res$measurement_error$Player, length)
      for (p in names(by_player))
        cat(sprintf("         • %-25s  %d points\n", p, by_player[[p]]))
    }

    pct <- if (n_raw > 0) sprintf("%.1f %%", 100 * n_clean / n_raw) else "—"
    cat(sprintf("\n  Points propres conservés            : %6d  (%s)\n", n_clean, pct))
    cat("══════════════════════════════════════════════════════\n")
  })

  output$outlier_plot <- renderPlotly({
    req(filtered_outlier_pts())
    pts <- filtered_outlier_pts()

    if (nrow(pts) == 0) {
      return(plotly_empty() %>% layout(title = "Aucun point à afficher avec les filtres actuels"))
    }

    color_map <- c(
      "Points propres"         = "#2196F3",
      "Erreur utilisation"     = "#000000",
      "Erreur mesure (DBSCAN)" = "#F44336"
    )

    p <- ggplot(pts, aes(x = Speed, y = Acceleration, color = type, text = Player)) +
      geom_point(alpha = 0.5, size = 1.2) +
      scale_color_manual(values = color_map, name = "Type de point") +
      labs(
        x     = "Vitesse (m/s)",
        y     = "Accélération (m/s²)",
        title = "Détection des outliers"
      ) +
      xlim(0, MAX_PLOT_SPEED) + ylim(0, MAX_PLOT_ACCELERATION) +
      theme_minimal()

    ggplotly(p)
  })

  output$misuse_table <- DT::renderDataTable({
    req(outlier_results())
    df <- outlier_results()$misuse_error

    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "Aucune erreur d'utilisation détectée"),
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    # Formater les colonnes clés pour l'affichage
    display_df <- df
    if ("Speed"        %in% colnames(display_df)) display_df$Speed        <- round(display_df$Speed,        3)
    if ("Acceleration" %in% colnames(display_df)) display_df$Acceleration <- round(display_df$Acceleration, 3)
    if ("Date"         %in% colnames(display_df)) display_df$Date         <- format(display_df$Date, "%d/%m/%Y")

    DT::datatable(
      display_df,
      caption  = "Points exclus — erreurs d'utilisation (sessions entières exclues)",
      filter   = "top",
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$measurement_table <- DT::renderDataTable({
    req(outlier_results())
    df <- outlier_results()$measurement_error

    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "Aucune erreur de mesure détectée"),
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    # Formater les colonnes clés pour l'affichage
    display_df <- df
    if ("Speed"        %in% colnames(display_df)) display_df$Speed        <- round(display_df$Speed,        3)
    if ("Acceleration" %in% colnames(display_df)) display_df$Acceleration <- round(display_df$Acceleration, 3)
    if ("Date"         %in% colnames(display_df)) display_df$Date         <- format(display_df$Date, "%d/%m/%Y")

    DT::datatable(
      display_df,
      caption  = paste0("Points exclus par DBSCAN (eps=", input$eps_dbscan,
                        ", minPts=", input$neighb_dbscan, ")"),
      filter   = "top",
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ---- Statistiques par joueur après nettoyage ----

  output$player_stats_table <- DT::renderDataTable({
    req(outlier_results(), raw_data())
    res   <- outlier_results()
    n_raw <- nrow(raw_data())

    players <- sort(unique(raw_data()$Player))

    stats_rows <- lapply(players, function(p) {
      n_p_raw   <- sum(raw_data()$Player == p)

      mis  <- res$misuse_error
      meas <- res$measurement_error
      cln  <- res$correct_points

      n_mis  <- if (nrow(mis)  > 0) sum(mis$Player  == p) else 0L
      n_meas <- if (nrow(meas) > 0) sum(meas$Player == p) else 0L
      n_cln  <- sum(cln$Player == p)

      sess_excl <- if (n_mis > 0) length(unique(mis$Date[mis$Player == p])) else 0L
      pct_kept  <- if (n_p_raw > 0) round(100 * n_cln / n_p_raw, 1) else NA_real_

      data.frame(
        Joueur                      = p,
        `Points bruts`              = n_p_raw,
        `Erreurs utilisation`       = n_mis,
        `Sessions exclues`          = sess_excl,
        `Erreurs mesure (DBSCAN)`   = n_meas,
        `Points propres conservés`  = n_cln,
        `% conservé`                = pct_kept,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    stats_df <- do.call(rbind, stats_rows)

    DT::datatable(
      stats_df,
      caption  = "Résumé du nettoyage par joueur",
      options  = list(pageLength = 20, scrollX = TRUE, dom = "tip"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "% conservé",
        background         = DT::styleColorBar(c(0, 100), "#b3d9ff"),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ---- Téléchargements ----

  output$download_clean <- downloadHandler(
    filename = function() paste0("donnees_propres_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(outlier_results())
      df <- outlier_results()$correct_points
      if ("Date" %in% colnames(df)) df$Date <- format(df$Date, "%d/%m/%Y")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_misuse <- downloadHandler(
    filename = function() paste0("erreurs_utilisation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(outlier_results())
      df <- outlier_results()$misuse_error
      if ("Date" %in% colnames(df)) df$Date <- format(df$Date, "%d/%m/%Y")
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_measurement <- downloadHandler(
    filename = function() paste0("erreurs_mesure_DBSCAN_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(outlier_results())
      df <- outlier_results()$measurement_error
      if ("Date" %in% colnames(df)) df$Date <- format(df$Date, "%d/%m/%Y")
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ---- Onglet Régression ----

  # Données filtrées pour le joueur sélectionné
  player_data <- reactive({
    req(regression_results(), input$player_select)
    list(
      correct = outlier_results()$correct_points[
        outlier_results()$correct_points$Player == input$player_select, ],
      hi_pts  = regression_results()$high_intensity_points[
        regression_results()$high_intensity_points$Player == input$player_select, ],
      lin     = regression_results()$linear_results[
        regression_results()$linear_results$Player == input$player_select, ],
      quant_detail = regression_results()$quantile_detail[
        regression_results()$quantile_detail$Player == input$player_select, ],
      quant_summary = regression_results()$quantile_summary[
        regression_results()$quantile_summary$Player == input$player_select, ]
    )
  })

  output$linear_plot <- renderPlotly({
    req(player_data())
    pd <- player_data()

    lin <- pd$lin
    if (nrow(lin) == 0 || is.na(lin$a0_linear)) {
      return(plotly_empty() %>% layout(title = "Données insuffisantes"))
    }

    a0 <- lin$a0_linear
    s0 <- lin$s0_linear
    r2 <- lin$r_squared_linear

    # Ligne de régression (de 0 à s0)
    if (!is.na(s0) && s0 > 0) {
      line_df <- data.frame(Speed = c(0, s0), Acceleration = c(a0, 0))
    } else {
      line_df <- data.frame(Speed = numeric(0), Acceleration = numeric(0))
    }

    p <- ggplot() +
      geom_point(
        data  = pd$correct,
        aes(x = Speed, y = Acceleration),
        alpha = 0.3, size = 1, color = "#2196F3"
      ) +
      geom_point(
        data  = pd$hi_pts,
        aes(x = Speed, y = Acceleration),
        alpha = 0.9, size = 2, color = "#F44336"
      ) +
      {if (nrow(line_df) > 0)
        geom_line(data = line_df, aes(x = Speed, y = Acceleration),
                  color = "#F44336", linewidth = 1)
      } +
      annotate("text", x = 1,   y = 9.5,
               label = sprintf("a0 = %.2f m/s²", a0), color = "#F44336", size = 4, hjust = 0) +
      annotate("text", x = 6,   y = 1,
               label = sprintf("s0 = %.2f m/s", s0),  color = "#F44336", size = 4, hjust = 0) +
      annotate("text", x = 1,   y = 8.5,
               label = sprintf("R² = %.3f", r2),       color = "grey40",  size = 3.5, hjust = 0) +
      labs(
        x     = "Vitesse (m/s)",
        y     = "Accélération (m/s²)",
        title = paste("Régression linéaire :", input$player_select)
      ) +
      coord_cartesian(xlim = c(0, MAX_PLOT_SPEED), ylim = c(0, MAX_PLOT_ACCELERATION)) +
      theme_minimal()

    ggplotly(p)
  })

  output$quantile_plot <- renderPlotly({
    req(player_data())
    pd <- player_data()

    qs <- pd$quant_summary
    qd <- pd$quant_detail

    if (nrow(qs) == 0 || is.na(qs$a0_quantile_mean)) {
      return(plotly_empty() %>% layout(title = "Données insuffisantes"))
    }

    a0_mean <- qs$a0_quantile_mean
    s0_mean <- qs$s0_quantile_mean
    a0_sd   <- qs$a0_quantile_sd
    s0_sd   <- qs$s0_quantile_sd

    p <- ggplot() +
      geom_point(
        data  = pd$correct,
        aes(x = Speed, y = Acceleration),
        alpha = 0.3, size = 1, color = "#2196F3"
      ) +
      geom_point(
        data  = pd$hi_pts,
        aes(x = Speed, y = Acceleration),
        alpha = 0.9, size = 2, color = "#F44336"
      )

    # Tracer les droites quantiles en gris clair (approche vectorisée)
    sub_q       <- qd[seq(1, nrow(qd), by = 10), ]
    sub_q_valid <- sub_q[!is.na(sub_q$a0) & !is.na(sub_q$s0) & sub_q$s0 > 0, ]
    if (nrow(sub_q_valid) > 0) {
      sub_q_valid$x_start <- 0
      sub_q_valid$y_end   <- 0
      p <- p + geom_segment(
        data        = sub_q_valid,
        aes(x = x_start, y = a0, xend = s0, yend = y_end),
        color       = "grey70",
        linetype    = "dotted",
        linewidth   = 0.4,
        inherit.aes = FALSE
      )
    }

    # Droite moyenne quantile
    if (!is.na(a0_mean) && !is.na(s0_mean) && s0_mean > 0) {
      mean_seg <- data.frame(x = 0, y = a0_mean, xend = s0_mean, yend = 0)
      p <- p + geom_segment(
        data        = mean_seg,
        aes(x = x, y = y, xend = xend, yend = yend),
        color       = "#F44336",
        linewidth   = 1,
        inherit.aes = FALSE
      )
    }

    p <- p +
      annotate("text", x = 1, y = 9.5,
               label = sprintf("a0 = %.2f ± %.2f m/s²", a0_mean, a0_sd),
               color = "#F44336", size = 4, hjust = 0) +
      annotate("text", x = 6, y = 1,
               label = sprintf("s0 = %.2f ± %.2f m/s", s0_mean, s0_sd),
               color = "#F44336", size = 4, hjust = 0) +
      labs(
        x     = "Vitesse (m/s)",
        y     = "Accélération (m/s²)",
        title = paste("Régression quantile :", input$player_select)
      ) +
      coord_cartesian(xlim = c(0, MAX_PLOT_SPEED), ylim = c(0, MAX_PLOT_ACCELERATION)) +
      theme_minimal()

    ggplotly(p)
  })

  # ---- Onglet Résultats ----

  results_df <- reactive({
    req(regression_results())
    build_results_table(
      regression_results()$linear_results,
      regression_results()$quantile_summary
    )
  })

  output$results_table <- DT::renderDataTable({
    req(results_df())
    DT::datatable(
      results_df(),
      options  = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE,
      caption  = "Paramètres a0 (accélération max) et s0 (vitesse max) par joueur"
    ) %>%
      DT::formatStyle(
        columns    = "Player",
        fontWeight = "bold"
      )
  })

  output$btn_download <- downloadHandler(
    filename = function() {
      paste0("ProfilAV_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(results_df(), file, row.names = FALSE)
    }
  )
}

# ============================================================
# LANCEMENT DE L'APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
