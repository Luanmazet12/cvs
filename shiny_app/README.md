# Profil Accélération-Vitesse In Situ — R Shiny Dashboard

Dashboard R Shiny reproduisant la méthodologie CRISP (Python) pour le calcul
des profils accélération-vitesse in situ à partir d'exports OpenField.

---

## Installation des packages R

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "quantreg",
  "dbscan",
  "DT"
))
```

---

## Lancement de l'application

```r
shiny::runApp("shiny_app/app.R")
```

ou directement depuis RStudio : ouvrir `app.R` et cliquer **Run App**.

---

## Format du fichier CSV attendu

Export **OpenField** (Catapult) :

| Caractéristique | Valeur |
|---|---|
| Séparateur | `;` |
| Décimale | `,` |
| Vitesse | km/h (convertie automatiquement en m/s) |
| En-tête | 8 lignes de métadonnées (`# …`) ignorées automatiquement |

L'athlète est extrait de la ligne `# Athlete: "…"`.  
Si le nom est vide, le **DeviceId** est utilisé à la place.

---

## Fonctionnalités

### 1. Chargement
- Importation d'un CSV OpenField
- Affichage du nombre de points, joueurs, sessions

### 2. Nettoyage (Outliers)
Reproduit `outliers.py` :
- Suppression des accélérations négatives
- **Erreurs d'utilisation** : points physiologiquement impossibles  
  (`Acceleration ≥ 10.93 − 10.93/10.5 × Speed`, ≥ seuil par session)
- **Erreurs de mesure** : DBSCAN par joueur sur les points `Acceleration ≥ 5 − Speed`  
  (bruit = cluster 0 en R, équivalent de -1 en scikit-learn)

### 3. Profil A-V
Reproduit `regression.py` (méthode Morin) :
1. **Points haute intensité** : top `n_max` accélérations par intervalle `dv`,  
   limités aux vitesses ≥ vitesse au point d'accélération maximale
2. **Régression linéaire** : `Acceleration ~ Speed` → a0, s0, R²
3. **Régression quantile** : quantiles 0.05 → 0.95 (pas 0.01) → moyenne ± σ de a0 et s0

### 4. Résultats
- Tableau récapitulatif par joueur
- Export CSV

---

## Paramètres ajustables (panneau latéral)

| Paramètre | Défaut | Description |
|---|---|---|
| `dv` | 0.3 m/s | Largeur des intervalles de vitesse |
| `n_max` | 2 | Points max retenus par intervalle |
| Seuil utilisation | 10 | Nb min de points pour déclarer une session erronée |
| Eps DBSCAN | 0.5 | Rayon de voisinage |
| Min points DBSCAN | 3 | `minPts` |

---

## Correspondance Python → R

| Python (`outliers.py` / `regression.py`) | R (`app.R`) |
|---|---|
| `Outliers.__init__` | `remove_negative_accelerations()` |
| `misuse_error_identification()` | `identify_misuse_errors()` |
| `measurement_error_identification()` + DBSCAN sklearn | `identify_measurement_errors()` + `dbscan::dbscan()` |
| `intensity_max_identification()` | `identify_high_intensity_points()` |
| `regression_lineaire()` | `compute_linear_regression()` |
| `regression_quantile()` + `compute_quantile_a0_s0()` | `compute_quantile_regression()` + `compute_quantile_summary()` |
| `plot_linear()` / `plot_quantile()` | `plot_linear_regression()` / `plot_quantile_regression()` |

---

## Note sur l'erreur "Can't transform a data frame with `NA` or `\"\"` names"

Cette erreur R/dplyr survient quand un `data.frame` possède des colonnes dont le
nom est `NA` ou `""`. Elle est corrigée dans `read_openfield_csv()` par :

```r
names(df) <- make.names(names(df), unique = TRUE)
```

qui garantit que tous les noms de colonnes sont des identifiants R valides.
