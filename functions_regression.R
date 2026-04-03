# functions_regression.R
# Fonctions de régression pour le profilage Accélération-Vitesse
# Conversion R du fichier regression.py (N. Miguens)

#' Identifier les points à haute intensité (méthode JB Morin)
#'
#' Pour chaque intervalle de vitesse de largeur dv, on sélectionne les n_max
#' points avec la plus grande accélération. La régression est ensuite effectuée
#' uniquement sur les points situés à une vitesse supérieure à celle du point
#' d'accélération maximale.
#'
#' @param points data.frame avec Player, Speed, Acceleration
#' @param dv     Largeur de l'intervalle de vitesse (défaut 0.3 m/s)
#' @param n_max  Nombre de points à garder par intervalle (défaut 2)
#' @return data.frame des points à haute intensité avec colonnes supplémentaires
identify_high_intensity_points <- function(points, dv = DV, n_max = N_MAX) {

  # Identifiant de l'intervalle de vitesse
  points$dv_bin <- floor(points$Speed / dv)

  # Rang de l'accélération dans chaque intervalle (par joueur)
  points <- points %>%
    dplyr::group_by(Player, dv_bin) %>%
    dplyr::mutate(
      rank_acc = dplyr::dense_rank(dplyr::desc(Acceleration))
    ) %>%
    dplyr::ungroup()

  # Conserver uniquement les n_max meilleurs points par intervalle
  hi_pts <- points[points$rank_acc <= n_max, , drop = FALSE]

  # Vitesse au point d'accélération maximale (par joueur)
  hi_pts <- hi_pts %>%
    dplyr::group_by(Player) %>%
    dplyr::mutate(
      max_Acceleration = max(Acceleration, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Pour chaque joueur : vitesse maximale parmi les points d'accel max
  max_speed_tbl <- hi_pts %>%
    dplyr::filter(Acceleration == max_Acceleration) %>%
    dplyr::group_by(Player) %>%
    dplyr::summarise(
      max_Speed_at_max_Acceleration = max(Speed, na.rm = TRUE),
      .groups = "drop"
    )

  # Joindre et filtrer : garder uniquement les points au-delà de ce seuil
  hi_pts <- dplyr::left_join(hi_pts, max_speed_tbl, by = "Player")
  hi_pts <- hi_pts[hi_pts$Speed >= hi_pts$max_Speed_at_max_Acceleration, , drop = FALSE]

  return(hi_pts)
}

#' Régression linéaire sur un groupe de points (un joueur)
#'
#' @param df data.frame avec Speed et Acceleration (un seul joueur)
#' @return list(a0, s0, r_squared) ou NULL si régression de mauvaise qualité
linear_regression_player <- function(df) {
  df_clean <- df[!is.na(df$Speed) & !is.na(df$Acceleration), ]
  if (nrow(df_clean) < 2) return(NULL)

  fit <- lm(Acceleration ~ Speed, data = df_clean)
  r2  <- summary(fit)$r.squared

  if (r2 <= 0.5) {
    warning(paste(
      "Régression linéaire de mauvaise qualité pour le joueur",
      unique(df$Player), "- R² =", round(r2, 3)
    ))
  }

  intercept <- coef(fit)[["(Intercept)"]]  # a0
  slope     <- coef(fit)[["Speed"]]        # b

  # s0 = -a0 / b  (intersection avec l'axe des vitesses)
  s0 <- if (abs(slope) > 1e-10) -intercept / slope else NA_real_

  list(
    a0        = intercept,
    s0        = s0,
    slope     = slope,
    r_squared = r2
  )
}

#' Régression quantile sur un groupe de points (un joueur)
#'
#' Calcule la régression pour plusieurs quantiles et retourne un data.frame
#' avec q, a0, s0.
#'
#' @param df        data.frame avec Speed et Acceleration (un seul joueur)
#' @param quantiles Vecteur de quantiles à calculer
#' @return data.frame avec colonnes q, a0, s0
quantile_regression_player <- function(df, quantiles = QUANTILES) {
  df_clean <- df[!is.na(df$Speed) & !is.na(df$Acceleration), ]
  df_clean$Speed        <- as.numeric(df_clean$Speed)
  df_clean$Acceleration <- as.numeric(df_clean$Acceleration)

  if (nrow(df_clean) < 3) return(data.frame(q = numeric(0), a0 = numeric(0), s0 = numeric(0)))

  results <- lapply(quantiles, function(q) {
    tryCatch({
      fit       <- quantreg::rq(Acceleration ~ Speed, data = df_clean, tau = q)
      intercept <- coef(fit)[["(Intercept)"]]
      slope     <- coef(fit)[["Speed"]]
      s0        <- if (abs(slope) > 1e-10) -intercept / slope else NA_real_
      data.frame(q = q, a0 = intercept, s0 = s0)
    }, error = function(e) {
      data.frame(q = q, a0 = NA_real_, s0 = NA_real_)
    })
  })

  do.call(rbind, results)
}

#' Calculer les résultats de la régression linéaire pour tous les joueurs
#'
#' @param high_intensity_points data.frame retourné par identify_high_intensity_points()
#' @return data.frame avec colonnes Player, a0_linear, s0_linear, r_squared_linear
compute_linear_regression <- function(high_intensity_points) {
  players <- unique(high_intensity_points$Player)

  rows <- lapply(players, function(p) {
    df_p <- high_intensity_points[high_intensity_points$Player == p, , drop = FALSE]
    res  <- linear_regression_player(df_p)
    if (is.null(res)) {
      data.frame(
        Player           = p,
        a0_linear        = NA_real_,
        s0_linear        = NA_real_,
        r_squared_linear = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Player           = p,
        a0_linear        = res$a0,
        s0_linear        = res$s0,
        r_squared_linear = res$r_squared,
        stringsAsFactors = FALSE
      )
    }
  })

  do.call(rbind, rows)
}

#' Calculer les résultats de la régression quantile pour tous les joueurs
#'
#' @param high_intensity_points data.frame retourné par identify_high_intensity_points()
#' @param quantiles             Vecteur de quantiles
#' @return data.frame détaillé (q, a0, s0) par joueur + résumé statistique
compute_quantile_regression <- function(high_intensity_points, quantiles = QUANTILES) {
  players <- unique(high_intensity_points$Player)

  detail_list  <- list()
  summary_list <- list()

  for (p in players) {
    df_p   <- high_intensity_points[high_intensity_points$Player == p, , drop = FALSE]
    res    <- quantile_regression_player(df_p, quantiles)
    res$Player <- p

    detail_list[[p]] <- res

    # Résumé : moyenne ± écart-type de a0 et s0
    summary_list[[p]] <- data.frame(
      Player            = p,
      a0_quantile_mean  = mean(res$a0, na.rm = TRUE),
      a0_quantile_sd    = sd(res$a0,   na.rm = TRUE),
      s0_quantile_mean  = mean(res$s0, na.rm = TRUE),
      s0_quantile_sd    = sd(res$s0,   na.rm = TRUE),
      stringsAsFactors  = FALSE
    )
  }

  list(
    detail  = do.call(rbind, detail_list),
    summary = do.call(rbind, summary_list)
  )
}

#' Pipeline de régression complet
#'
#' @param correct_points data.frame nettoyé (sortie de run_outlier_detection)
#' @param dv             Intervalle de vitesse
#' @param n_max          Points max par intervalle
#' @param quantiles      Quantiles pour la régression quantile
#' @return list avec high_intensity_points, linear_results, quantile_results (detail + summary)
run_regression <- function(correct_points,
                           dv        = DV,
                           n_max     = N_MAX,
                           quantiles = QUANTILES) {

  # 1. Sélection des points à haute intensité
  hi_pts <- identify_high_intensity_points(correct_points, dv, n_max)

  if (nrow(hi_pts) == 0) {
    stop("Aucun point à haute intensité identifié. Vérifiez les données.")
  }

  # 2. Régression linéaire
  lin_res <- compute_linear_regression(hi_pts)

  # 3. Régression quantile
  quant_res <- compute_quantile_regression(hi_pts, quantiles)

  list(
    high_intensity_points = hi_pts,
    linear_results        = lin_res,
    quantile_detail       = quant_res$detail,
    quantile_summary      = quant_res$summary
  )
}

#' Construire le tableau de résultats final (linéaire + quantile)
#'
#' @param linear_results  data.frame de compute_linear_regression()
#' @param quantile_summary data.frame de compute_quantile_regression()$summary
#' @return data.frame combiné
build_results_table <- function(linear_results, quantile_summary) {
  merged <- merge(linear_results, quantile_summary, by = "Player", all = TRUE)

  # Formater les valeurs numériques à 2 décimales pour l'affichage
  num_cols <- setdiff(colnames(merged), "Player")
  merged[num_cols] <- lapply(merged[num_cols], function(x) round(as.numeric(x), 2))

  # Renommer pour l'affichage
  colnames(merged)[colnames(merged) == "a0_linear"]        <- "a0 Régr. Linéaire (m/s²)"
  colnames(merged)[colnames(merged) == "s0_linear"]        <- "s0 Régr. Linéaire (m/s)"
  colnames(merged)[colnames(merged) == "r_squared_linear"] <- "R² Linéaire"
  colnames(merged)[colnames(merged) == "a0_quantile_mean"] <- "a0 Régr. Quantile moy. (m/s²)"
  colnames(merged)[colnames(merged) == "a0_quantile_sd"]   <- "a0 Régr. Quantile SD"
  colnames(merged)[colnames(merged) == "s0_quantile_mean"] <- "s0 Régr. Quantile moy. (m/s)"
  colnames(merged)[colnames(merged) == "s0_quantile_sd"]   <- "s0 Régr. Quantile SD"

  merged
}
