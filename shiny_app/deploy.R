# deploy.R - Script de déploiement sur shinyapps.io

library(rsconnect)

# Charger les credentials (depuis secret.R non versionné)
if (!file.exists("secret.R")) {
  stop(
    "Fichier secret.R manquant. Créer ce fichier avec vos credentials shinyapps.io"
  )
}
source("secret.R")

# Déployer l'application
rsconnect::deployApp(
  appDir = ".",
  appName = "conservation-madagascar-dashboard",
  appTitle = "Aires protégées Madagascar - Visualisation interactive",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\n✓ Déploiement terminé!\n")
cat(
  "URL: https://VOTRE-COMPTE.shinyapps.io/conservation-madagascar-dashboard/\n"
)
