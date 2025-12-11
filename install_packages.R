options(repos = c(CRAN = "https://cloud.r-project.org"))

# Prevent installing raster/terra on Render
Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")
Sys.setenv("PKG_CONFIG_PATH" = "")

install.packages(c(
  "shiny","RPostgres","DBI","dplyr","ggplot2","plotly",
  "leaflet","DT","bslib","shinyWidgets","thematic",
  "shinyjs","lubridate","shinycssloaders","tools"
))











