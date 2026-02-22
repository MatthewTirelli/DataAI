# Run the Pediatric Suicide Methods Shiny app.
# From project root: Rscript app/run.R
# From app folder: Rscript run.R (or in R: setwd("app"); shiny::runApp("."))

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("httr2", quietly = TRUE)) {
  install.packages("httr2", repos = "https://cloud.r-project.org")
}

# If run via Rscript app/run.R, working dir is project root; move to app to find .env
if (file.exists("app.R")) {
  # Already in app folder
} else if (file.exists("app/app.R")) {
  setwd("app")
} else {
  stop("Run this script from the project root or from the app folder.")
}

shiny::runApp(".", launch.browser = TRUE)
