# Pediatric Suicide Methods — CDC Data Explorer
# Shiny app: CDC injury mortality API (pediatric suicide by method).
# Two views: (1) deaths by year for a chosen method; (2) methods by year.

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)

# ---- Config ----
CDC_URL <- "https://data.cdc.gov/resource/nt65-c7a7.json"

# Load .env (no dotenv package: use readRenviron per project style)
if (file.exists(".env")) {
  readRenviron(".env")
} else {
  if (file.exists("../.env")) readRenviron("../.env")
}

# ---- API ----
# Fetch pediatric suicide data: under 18 (CDC age groups < 15 and 15-19). No limit to get all years/methods.
fetch_cdc_data <- function(token) {
  if (is.null(token) || token == "") stop("Missing SOCRATA_APP_TOKEN. Add it to .env in app folder or project root.")
  req <- request(CDC_URL) %>%
    req_headers("X-App-Token" = token) %>%
    req_url_query(
      `$select` = "year,sex,age_years,injury_intent,injury_mechanism,deaths",
      `$where` = "injury_intent = 'Suicide' AND (age_years = '< 15' OR age_years = '15-19')",
      `$order` = "year ASC",
      `$limit` = "10000"
    ) %>%
    req_timeout(30)
  resp <- req_perform(req)
  if (resp_status(resp) != 200) stop("CDC API returned status ", resp_status(resp))
  resp_body_json(resp)
}

# Parse JSON list into tibble and ensure numeric types
cdc_to_df <- function(raw) {
  if (length(raw) == 0) return(dplyr::tibble(year = integer(), injury_mechanism = character(), deaths = integer()))
  d <- dplyr::bind_rows(raw)
  d %>%
    mutate(
      year = as.integer(year),
      deaths = as.integer(deaths),
      injury_mechanism = as.character(injury_mechanism)
    ) %>%
    filter(!is.na(year), !is.na(deaths), injury_mechanism != "")
}

# ---- UI ----
# page_fluid allows vertical scrolling so full graphs are visible (page_fillable clips to viewport)
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1e3a5f",
    secondary = "#2c5282",
    base_font = bslib::font_google("Inter", wght = c(400, 600)),
    heading_font = bslib::font_google("Inter", wght = 600)
  ),
  title = "Pediatric Suicide Methods — CDC Data",
  padding = 24,
  # Header
  div(
    class = "mb-4 pb-3 border-bottom",
    h1("Pediatric Suicide Methods", class = "mb-1", style = "color: #1e3a5f;"),
    p("CDC injury mortality data: pediatric (under 20; age groups &lt; 15 and 15–19) suicides by method. Choose a method to see deaths by year, or choose a year to see all methods.", class = "text-muted mb-0")
  ),
  layout_columns(
    col_widths = c(12, 12),
    row_heights = "auto",
    # Card 1: By method (deaths by year)
    card(
      card_header("Deaths by year for a method", class = "bg-primary text-white"),
      p("Select an injury method to view total deaths per year for that method.", class = "text-muted small"),
      selectInput(
        "method",
        "Injury method",
        choices = character(0),
        width = "100%"
      ),
      plotOutput("plot_by_method", height = "520px")
    ),
    # Card 2: By year (methods for one year)
    card(
      card_header("Methods for a single year", class = "bg-primary text-white"),
      p("Select a year to see a bar chart of deaths by injury method for that year.", class = "text-muted small"),
      selectInput(
        "year",
        "Year",
        choices = character(0),
        width = "100%"
      ),
      plotOutput("plot_by_year", height = "640px")
    )
  ),
  # Status / error
  uiOutput("status_ui")
)

# ---- Server ----
server <- function(input, output, session) {
  cdc_df <- reactiveVal(NULL)
  token <- Sys.getenv("SOCRATA_APP_TOKEN")

  # Load data once on start
  observe({
    if (!is.null(cdc_df())) return()
    withProgress(message = "Loading CDC data…", value = 0, {
      tryCatch({
        raw <- fetch_cdc_data(token)
        df <- cdc_to_df(raw)
        cdc_df(df)
        # Populate method dropdown (unique, sorted)
        methods <- sort(unique(df$injury_mechanism))
        updateSelectInput(session, "method", choices = methods, selected = methods[1])
        # Populate year dropdown
        years <- sort(unique(df$year), decreasing = TRUE)
        updateSelectInput(session, "year", choices = years, selected = years[1])
        output$status_ui <- renderUI({
          div(class = "alert alert-success mt-3", role = "alert",
              strong(nrow(df)), " records loaded (pediatric suicide, ages &lt; 20: &lt; 15 and 15–19).")
        })
      }, error = function(e) {
        cdc_df(dplyr::tibble())
        output$status_ui <- renderUI({
          div(class = "alert alert-danger mt-3", role = "alert",
              strong("Error loading data: "), conditionMessage(e),
              ". Check SOCRATA_APP_TOKEN in .env and try again.")
        })
      })
    })
  })

  # Plot 1: deaths by year for selected method
  output$plot_by_method <- renderPlot({
    df <- cdc_df()
    if (is.null(df) || nrow(df) == 0) return(plot_null("No data"))
    method <- input$method
    if (is.null(method) || method == "") return(plot_null("Select a method"))
    by_year <- df %>%
      filter(injury_mechanism == method) %>%
      group_by(year) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")
    if (nrow(by_year) == 0) return(plot_null("No data for this method"))
    ggplot(by_year, aes(x = factor(year), y = deaths)) +
      geom_col(fill = "#2c5282", width = 0.7) +
      labs(x = "Year", y = "Deaths", title = paste("Deaths by year —", method)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(colour = "#1e3a5f", face = "bold", size = 16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(12, 16, 12, 12)
      )
  }, res = 96, height = 500)

  # Plot 2: methods for selected year
  output$plot_by_year <- renderPlot({
    df <- cdc_df()
    if (is.null(df) || nrow(df) == 0) return(plot_null("No data"))
    yr <- input$year
    if (is.null(yr) || yr == "") return(plot_null("Select a year"))
    yr <- as.integer(yr)
    by_method <- df %>%
      filter(year == yr) %>%
      group_by(injury_mechanism) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(deaths))
    if (nrow(by_method) == 0) return(plot_null("No data for this year"))
    ggplot(by_method, aes(x = reorder(injury_mechanism, deaths), y = deaths)) +
      geom_col(fill = "#1e3a5f", width = 0.75) +
      coord_flip() +
      labs(x = NULL, y = "Deaths", title = paste("Deaths by method —", yr)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(colour = "#1e3a5f", face = "bold", size = 16),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(12, 12, 12, 8)
      )
  }, res = 96, height = 600)

  plot_null <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = msg, size = 5, colour = "gray40") +
      ggplot2::theme_void()
  }
}

shinyApp(ui, server)
