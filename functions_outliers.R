# functions_outliers.R
# Fonctions de détection des outliers pour le profilage Accélération-Vitesse
# Conversion R du fichier outliers.py (N. Miguens)

#' Lecture et nettoyage du fichier CSV OpenField
#'
#' @param file_path  Chemin vers le fichier CSV
#' @param player_name Nom du joueur (extrait du header ou fourni)
#' @return data.frame avec les colonnes Player, Date, Speed (m/s), Acceleration
read_openfield_csv <- function(file_path, player_name = NULL) {

  # --- 1. Lire les lignes brutes pour extraire les métadonnées du header ---
  raw_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")

  # Ligne de début des données (première ligne qui ne commence pas par '#')
  data_start <- which(!startsWith(raw_lines, "#"))[1]

  # Extraire le nom du joueur depuis le header si non fourni
  if (is.null(player_name) || nchar(trimws(player_name)) == 0) {
    athlete_line <- raw_lines[startsWith(raw_lines, "# Athlete:")]
    if (length(athlete_line) > 0) {
      # Format: # Athlete: "Nom Prénom"
      player_name <- gsub('.*"(.*)".*', "\\1", athlete_line[1])
    }
    # Si toujours vide, extraire l'identifiant depuis le nom de fichier
    if (is.null(player_name) || nchar(trimws(player_name)) == 0) {
      device_line <- raw_lines[startsWith(raw_lines, "# DeviceId:")]
      if (length(device_line) > 0) {
        device_id <- trimws(gsub("# DeviceId:", "", device_line[1]))
        player_name <- paste0("Device_", device_id)
      } else {
        player_name <- sub("\\.[^.]*$", "", basename(file_path))
      }
    }
  }

  # --- 2. Lire le CSV (skip des lignes de commentaires) ---
  # On passe les lignes brutes déjà lues pour éviter un double encodage
  header_line  <- raw_lines[data_start]
  data_lines   <- raw_lines[(data_start + 1):length(raw_lines)]

  # Remplacer la virgule décimale par un point dans les colonnes numériques
  # (sauf dans les timestamps qui peuvent contenir des /)
  data_lines_clean <- gsub(",([0-9])", ".\\1", data_lines)

  # Construire un bloc texte et lire avec read.csv
  csv_text <- paste(c(header_line, data_lines_clean), collapse = "\n")
  df <- read.csv(
    text      = csv_text,
    sep       = ";",
    header    = TRUE,
    stringsAsFactors = FALSE,
    check.names      = FALSE,
    encoding         = "UTF-8"
  )

  # --- 3. Renommer et sélectionner les colonnes utiles ---
  # Colonnes obligatoires (noms tels qu'ils apparaissent dans l'export OpenField)
  needed <- c("Timestamp", "Velocity", "Acceleration")
  missing_cols <- setdiff(needed, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Colonnes manquantes dans le CSV :", paste(missing_cols, collapse = ", ")))
  }

  df <- df[, needed, drop = FALSE]
  colnames(df)[colnames(df) == "Timestamp"] <- "Date"
  colnames(df)[colnames(df) == "Velocity"]  <- "Speed"

  # --- 4. Convertir les types ---
  df$Speed        <- as.numeric(df$Speed)
  df$Acceleration <- as.numeric(df$Acceleration)

  # Vitesse : km/h → m/s
  df$Speed <- df$Speed / 3.6

  # Ajouter le nom du joueur
  df$Player <- player_name

  # Supprimer les lignes avec NA dans les colonnes clés
  df <- df[!is.na(df$Speed) & !is.na(df$Acceleration), ]

  return(df)
}

# ---- Fonctions de détection des outliers ----

#' Supprimer les valeurs d'accélération négatives
#'
#' @param points data.frame avec la colonne Acceleration
#' @return data.frame filtré
remove_negative_acceleration <- function(points) {
  points[points$Acceleration >= 0, , drop = FALSE]
}

#' Identifier les erreurs de mauvaise utilisation
#'
#' Points au-dessus de la droite théorique max :
#'   Acceleration >= 10.93 - (10.93/10.5) * Speed
#' Si une session contient >= nb_outlier de tels points, elle est marquée entière.
#'
#' @param correct_points data.frame avec Speed, Acceleration, Player, Date
#' @param nb_outlier     Seuil minimum de points outliers par session
#' @return list(correct_points, misuse_error)
identify_misuse_errors <- function(correct_points, nb_outlier = NB_OUTLIER) {

  # Seuil physique (cf. papier)
  threshold <- 10.93 - (10.93 / 10.5) * correct_points$Speed

  outliers <- correct_points[
    correct_points$Acceleration >= 0 &
      correct_points$Acceleration >= threshold,
    , drop = FALSE
  ]

  if (nrow(outliers) == 0) {
    return(list(correct_points = correct_points, misuse_error = data.frame()))
  }

  # Compter le nombre d'outliers par session (Player + Date)
  outliers$n_error <- ave(
    outliers$Speed, outliers$Player, outliers$Date,
    FUN = length
  )

  outliers <- outliers[outliers$n_error >= nb_outlier, , drop = FALSE]

  if (nrow(outliers) == 0) {
    return(list(correct_points = correct_points, misuse_error = data.frame()))
  }

  # Supprimer toutes les sessions (Date) contenant des outliers
  flagged_dates    <- unique(outliers$Date)
  correct_points_new <- correct_points[
    !correct_points$Date %in% flagged_dates, , drop = FALSE
  ]

  list(correct_points = correct_points_new, misuse_error = outliers)
}

#' Clustering DBSCAN pour un seul joueur
#'
#' @param df          data.frame avec les colonnes Speed et Acceleration
#' @param eps_DBSCAN  epsilon (rayon du voisinage)
#' @param neighb_DBSCAN min_samples
#' @return Vecteur d'étiquettes (-1 = bruit, >= 1 = cluster)
dbscan_clustering <- function(df, eps_DBSCAN = EPS_DBSCAN, neighb_DBSCAN = NEIGHB_DBSCAN) {
  if (nrow(df) < neighb_DBSCAN) {
    return(rep(-1L, nrow(df)))
  }
  mat <- as.matrix(df[, c("Speed", "Acceleration")])
  res <- dbscan::dbscan(mat, eps = eps_DBSCAN, minPts = neighb_DBSCAN)
  # Dans le package dbscan R, cluster == 0 signifie bruit (équivalent -1 sklearn)
  ifelse(res$cluster == 0L, -1L, res$cluster)
}

#' Identifier les erreurs de mesure via DBSCAN
#'
#' Sélectionne uniquement les points proches du "front de Pareto" (haute intensité)
#' pour réduire le temps de calcul, puis applique DBSCAN par joueur.
#'
#' @param correct_points data.frame avec Speed, Acceleration, Player
#' @param eps_DBSCAN     epsilon pour DBSCAN
#' @param neighb_DBSCAN  min_samples pour DBSCAN
#' @return list(correct_points, measurement_error)
identify_measurement_errors <- function(correct_points,
                                        eps_DBSCAN    = EPS_DBSCAN,
                                        neighb_DBSCAN = NEIGHB_DBSCAN) {

  # Sous-échantillon : points d'intérêt (proches du front)
  sample_idx <- which(correct_points$Acceleration >= 5 - correct_points$Speed)
  sample_pts <- correct_points[sample_idx, , drop = FALSE]

  if (nrow(sample_pts) == 0) {
    return(list(correct_points = correct_points, measurement_error = data.frame()))
  }

  # Ajouter un index de ligne pour relier les résultats
  sample_pts$.row_id <- seq_len(nrow(sample_pts))

  # DBSCAN par joueur
  players <- unique(sample_pts$Player)
  labels  <- rep(NA_integer_, nrow(sample_pts))

  for (p in players) {
    idx_p  <- which(sample_pts$Player == p)
    df_p   <- sample_pts[idx_p, , drop = FALSE]
    lbl    <- dbscan_clustering(df_p, eps_DBSCAN, neighb_DBSCAN)
    labels[idx_p] <- lbl
  }

  sample_pts$label <- labels
  outliers_dbscan  <- sample_pts[!is.na(sample_pts$label) & sample_pts$label == -1L, , drop = FALSE]

  # Supprimer les outliers des points corrects (par position dans correct_points)
  out_idx <- sample_idx[!is.na(sample_pts$label) & sample_pts$label == -1L]

  if (length(out_idx) == 0) {
    correct_points_new <- correct_points
  } else {
    correct_points_new <- correct_points[-out_idx, , drop = FALSE]
  }

  # Nettoyer les colonnes internes
  outliers_dbscan$.row_id <- NULL
  outliers_dbscan$label   <- NULL

  list(correct_points = correct_points_new, measurement_error = outliers_dbscan)
}

#' Pipeline complet de détection des outliers
#'
#' @param points        data.frame brut (après lecture CSV)
#' @param nb_outlier    Seuil misuse
#' @param eps_DBSCAN    Epsilon DBSCAN
#' @param neighb_DBSCAN Min_samples DBSCAN
#' @return list avec correct_points, misuse_error, measurement_error
run_outlier_detection <- function(points,
                                  nb_outlier    = NB_OUTLIER,
                                  eps_DBSCAN    = EPS_DBSCAN,
                                  neighb_DBSCAN = NEIGHB_DBSCAN) {

  # Étape 1 : supprimer les accélérations négatives
  correct <- remove_negative_acceleration(points)

  # Étape 2 : détecter les erreurs de mauvaise utilisation
  res_misuse <- identify_misuse_errors(correct, nb_outlier)
  correct    <- res_misuse$correct_points

  # Étape 3 : détecter les erreurs de mesure
  res_measure <- identify_measurement_errors(correct, eps_DBSCAN, neighb_DBSCAN)
  correct     <- res_measure$correct_points

  list(
    correct_points   = correct,
    misuse_error     = res_misuse$misuse_error,
    measurement_error = res_measure$measurement_error
  )
}
