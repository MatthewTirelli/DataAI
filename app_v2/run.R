# Run the Pediatric Suicide + Ollama Shiny app (V2).
# From project root: Rscript app_v2/run.R
# From app_v2 folder: Rscript run.R

pkgs <- c("shiny", "bslib", "httr2", "jsonlite", "dplyr", "ggplot2", "officer")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

if (file.exists("app.R")) {
  # Already in app_v2 folder
} else if (file.exists("app_v2/app.R")) {
  setwd("app_v2")
} else if (file.exists("app/app.R")) {
  setwd("app")
} else {
  stop("Run this script from the project root or from the app_v2 folder.")
}

shiny::runApp(".", launch.browser = TRUE)
