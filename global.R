# global.R - Variables globales et chargements de packages
# Application Dashboard Profilage Accélération-Vitesse

# Taille maximale d'upload : 500 MB (les exports OpenField peuvent être volumineux)
options(shiny.maxRequestSize = 500 * 1024^2)

# Liste des packages requis
required_packages <- c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "plotly",
  "DT",
  "dplyr",
  "stringr",
  "dbscan",
  "quantreg"
)

# Installation et chargement des packages manquants
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.rstudio.com/")
  }
  library(pkg, character.only = TRUE)
}

# ---- Constantes globales (méthode JB Morin / N. Miguens) ----

# Intervalle de vitesse pour la sélection des points à haute intensité
DV <- 0.3

# Nombre de points maximum par intervalle de vitesse
N_MAX <- 2

# Seuil minimum d'outliers d'utilisation par session pour déclencher le nettoyage
NB_OUTLIER <- 10

# Paramètres DBSCAN pour la détection des erreurs de mesure
NEIGHB_DBSCAN <- 3   # min_samples
EPS_DBSCAN    <- 0.5 # epsilon (rayon du voisinage)

# Quantiles calculés pour la régression quantile
QUANTILES <- seq(0.05, 0.95, by = 0.01)

# Seuil de qualité R² minimal pour la régression linéaire
MIN_R_SQUARED <- 0.5

# Tolérance numérique pour éviter la division par zéro
SLOPE_TOLERANCE <- 1e-10

# Limites des axes pour les graphiques (m/s et m/s²)
MAX_PLOT_SPEED        <- 11
MAX_PLOT_ACCELERATION <- 11
