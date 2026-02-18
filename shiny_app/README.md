# Application Shiny - Visualisation des aires protégées

Cette application interactive permet d'explorer les données consolidées des aires protégées de Madagascar.

## Prérequis

### Données nécessaires

Placer les fichiers suivants dans le dossier `data/` :

- `all_PAs_conso.rds` : Données spatiales consolidées (généré par `04_combination.qmd`)
- `legal_texts.rds` : Textes juridiques CNLEGIS (généré par `03_legal_docs.qmd`)
- `MDG_WDPA_Consolidated.parquet` : Historique WDPA (généré par `02_wdpa_data.qmd`)

### Packages R

```r
install.packages(c("shiny", "dplyr", "sf", "ggplot2", "tmap", 
                   "arrow", "geoarrow", "DT", "htmltools", 
                   "stringr", "lubridate"))
```

## Déploiement sur shinyapps.io

### Configuration initiale (une seule fois)

1. Créer un compte sur https://www.shinyapps.io/
2. Obtenir votre token et secret depuis le dashboard
3. Créer un fichier `secret.R` (non versionné) :

```r
# secret.R
rsconnect::setAccountInfo(
  name = "votre-nom",
  token = "VOTRE_TOKEN",
  secret = "VOTRE_SECRET"
)
```

### Déploiement

```r
# Exécuter deploy.R
source("deploy.R")
```

Ou manuellement :

```r
library(rsconnect)
source("secret.R")  # Charge les credentials

rsconnect::deployApp(
  appDir = ".",
  appName = "conservation-madagascar-dashboard",
  appTitle = "Aires protégées Madagascar - Visualisation",
  forceUpdate = TRUE
)
```

## Lancer localement

```r
shiny::runApp()
```

## Notes

- Les fichiers de données (`data/`) ne sont **pas versionnés** (trop volumineux)
- Le fichier `secret.R` ne doit **jamais être commité** (contient credentials)
- Pour reproduire l'app, exécuter d'abord les chapitres 02, 03 et 04 du livre Quarto pour générer les données
