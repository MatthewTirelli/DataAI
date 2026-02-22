# Run the Pediatric Suicide + Ollama Shiny app.
# From project root: Rscript app/run.R
# From app folder: Rscript run.R

pkgs <- c("shiny", "bslib", "httr2", "jsonlite", "dplyr", "ggplot2", "officer")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

if (file.exists("app.R")) {
  # Already in app_v1 folder
} else if (file.exists("app_v1/app.R")) {
  setwd("app_v1")
} else if (file.exists("app/app.R")) {
  setwd("app")
} else {
  stop("Run this script from the project root or from the app_v1 folder.")
}

shiny::runApp(".", launch.browser = TRUE)
