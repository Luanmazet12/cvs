# app.R - Dashboard Shiny - Profilage Accélération-Vitesse
# Conversion du pipeline Python (N. Miguens) en R/Shiny

# Charger les variables globales et les packages
source("global.R")
source("functions_outliers.R")
source("functions_regression.R")

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
          valueBoxOutput("vbox_total",   width = 4),
          valueBoxOutput("vbox_clean",   width = 4),
          valueBoxOutput("vbox_outliers", width = 4)
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
            title = "Erreurs de mauvaise utilisation",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            DT::dataTableOutput("misuse_table")
          ),
          box(
            title = "Erreurs de mesure (DBSCAN)",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            DT::dataTableOutput("measurement_table")
          )
        )
      ),

      # ---- Onglet Régression ----
      tabItem(
        tabName = "tab_regression",
        fluidRow(
          box(
            title = "Régression linéaire",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("linear_plot", height = "450px")
          ),
          box(
            title = "Régression quantile",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("quantile_plot", height = "450px")
          )
        ),
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

  # ---- Réactifs ----

  # Données brutes lues depuis le CSV
  raw_data <- reactive({
    req(input$csv_file)

    withProgress(message = "Lecture du fichier CSV...", value = 0.1, {
      tryCatch({
        player_name <- trimws(input$player_name)
        if (nchar(player_name) == 0) player_name <- NULL

        df <- read_openfield_csv(input$csv_file$datapath, player_name)
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

  output$vbox_total <- renderValueBox({
    req(raw_data())
    valueBox(
      value    = nrow(raw_data()),
      subtitle = "Points bruts",
      icon     = icon("database"),
      color    = "blue"
    )
  })

  output$vbox_clean <- renderValueBox({
    n <- if (!is.null(outlier_results())) nrow(outlier_results()$correct_points) else "—"
    valueBox(
      value    = n,
      subtitle = "Points propres",
      icon     = icon("check-circle"),
      color    = "green"
    )
  })

  output$vbox_outliers <- renderValueBox({
    n_misuse  <- if (!is.null(outlier_results())) nrow(outlier_results()$misuse_error) else 0
    n_measure <- if (!is.null(outlier_results())) nrow(outlier_results()$measurement_error) else 0
    valueBox(
      value    = n_misuse + n_measure,
      subtitle = "Outliers détectés",
      icon     = icon("exclamation-triangle"),
      color    = "red"
    )
  })

  output$outlier_plot <- renderPlotly({
    req(outlier_results())
    res <- outlier_results()

    # Construire un data.frame annoté
    clean <- res$correct_points
    clean$type <- "Points propres"

    misuse <- res$misuse_error
    if (nrow(misuse) > 0) misuse$type <- "Erreur utilisation"

    measure <- res$measurement_error
    if (nrow(measure) > 0) measure$type <- "Erreur mesure (DBSCAN)"

    all_pts <- dplyr::bind_rows(
      clean[, c("Speed", "Acceleration", "Player", "type")],
      if (nrow(misuse)  > 0) misuse[,  c("Speed", "Acceleration", "Player", "type")] else NULL,
      if (nrow(measure) > 0) measure[, c("Speed", "Acceleration", "Player", "type")] else NULL
    )

    color_map <- c(
      "Points propres"          = "#2196F3",
      "Erreur utilisation"      = "#000000",
      "Erreur mesure (DBSCAN)"  = "#F44336"
    )

    p <- ggplot(all_pts, aes(x = Speed, y = Acceleration, color = type, text = Player)) +
      geom_point(alpha = 0.5, size = 1.2) +
      scale_color_manual(values = color_map, name = "Type de point") +
      labs(
        x     = "Vitesse (m/s)",
        y     = "Accélération (m/s²)",
        title = "Détection des outliers"
      ) +
      xlim(0, 11) + ylim(0, 11) +
      theme_minimal()

    ggplotly(p)
  })

  output$misuse_table <- DT::renderDataTable({
    req(outlier_results())
    df <- outlier_results()$misuse_error
    DT::datatable(
      df[, intersect(c("Player", "Date", "Speed", "Acceleration"), colnames(df))],
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$measurement_table <- DT::renderDataTable({
    req(outlier_results())
    df <- outlier_results()$measurement_error
    DT::datatable(
      df[, intersect(c("Player", "Date", "Speed", "Acceleration"), colnames(df))],
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

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

    a0    <- lin$a0_linear
    s0    <- lin$s0_linear
    r2    <- lin$r_squared_linear
    slope <- if (!is.na(s0) && abs(s0) > 1e-10) -a0 / s0 else NA

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
      xlim(0, 11) + ylim(0, 11) +
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

    # Tracer les droites quantiles en gris clair
    sub_q <- qd[seq(1, nrow(qd), by = 10), ]  # sous-ensemble pour lisibilité
    for (i in seq_len(nrow(sub_q))) {
      ai <- sub_q$a0[i]
      si <- sub_q$s0[i]
      if (!is.na(ai) && !is.na(si) && si > 0) {
        p <- p + geom_segment(
          aes(x = 0, y = ai, xend = si, yend = 0),
          color    = "grey70",
          linetype = "dotted",
          linewidth = 0.4,
          data     = NULL,
          inherit.aes = FALSE
        )
      }
    }

    # Droite moyenne quantile
    if (!is.na(a0_mean) && !is.na(s0_mean) && s0_mean > 0) {
      p <- p + geom_segment(
        aes(x = 0, y = a0_mean, xend = s0_mean, yend = 0),
        color     = "#F44336",
        linewidth = 1,
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
      xlim(0, 11) + ylim(0, 11) +
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
