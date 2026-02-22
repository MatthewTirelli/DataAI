# Pediatric Suicide Methods + Ollama — Shiny app
# CDC Socrata (pediatric < 20: < 15 and 15-19), Poisson regression, Ollama Cloud report, docx export.
# Errors logged to app_error_log.txt in the app folder.

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(officer)

# ---- Config ----
CDC_URL <- "https://data.cdc.gov/resource/nt65-c7a7.json"
OLLAMA_URL <- "https://ollama.com/api/chat"
LOG_FILE <- "app_error_log.txt"

# Load .env
if (file.exists(".env")) {
  readRenviron(".env")
} else if (file.exists("../.env")) {
  readRenviron("../.env")
}

# ---- Backend logging ----
log_step <- function(step, message, success = TRUE) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  status <- if (success) "OK" else "ERROR"
  line <- paste0("[", ts, "] ", step, " — ", status, ": ", message, "\n")
  tryCatch(
    cat(line, file = LOG_FILE, append = TRUE),
    error = function(e) NULL
  )
}

# ---- API ----
fetch_cdc_data <- function(token) {
  if (is.null(token) || token == "") stop("Missing SOCRATA_APP_TOKEN.")
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
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1e3a5f",
    secondary = "#2c5282",
    base_font = bslib::font_google("Inter", wght = c(400, 600)),
    heading_font = bslib::font_google("Inter", wght = 600)
  ),
  title = "Pediatric Suicide Methods — CDC + Ollama",
  padding = 24,
  div(
    class = "mb-4 pb-3 border-bottom",
    h1("Pediatric Suicide Methods", class = "mb-1", style = "color: #1e3a5f;"),
    p("CDC injury mortality (pediatric &lt; 20: &lt; 15 and 15–19). Data, Poisson regression, and AI report.", class = "text-muted mb-0")
  ),
  uiOutput("env_alert_ui"),
  navset_card_tab(
    title = NULL,
    nav_panel(
      "Data & Plots",
      layout_columns(
        col_widths = c(12, 12),
        row_heights = "auto",
        card(
          card_header("Deaths by year for a method", class = "bg-primary text-white"),
          selectInput("method", "Injury method", choices = character(0), width = "100%"),
          plotOutput("plot_by_method", height = "420px")
        ),
        card(
          card_header("Methods for a single year", class = "bg-primary text-white"),
          selectInput("year", "Year", choices = character(0), width = "100%"),
          plotOutput("plot_by_year", height = "520px")
        )
      ),
      uiOutput("status_ui")
    ),
    nav_panel(
      "Poisson Regression",
      card(
        card_header("Year-over-year trend", class = "bg-primary text-white"),
        p("Total deaths ~ year. By-method models: rate ratio and p-value per method.", class = "text-muted small"),
        tableOutput("poisson_table"),
        uiOutput("poisson_status_ui")
      )
    ),
    nav_panel(
      "AI Report",
      card(
        card_header("Ollama Cloud report", class = "bg-primary text-white"),
        p("Generate an AI interpretation from the Poisson results. Then download as .docx if desired.", class = "text-muted small"),
        uiOutput("btn_generate_ui"),
        uiOutput("ai_loading_ui"),
        uiOutput("ai_report_ui"),
        uiOutput("ai_docx_ui")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  cdc_df <- reactiveVal(NULL)
  poisson_results <- reactiveVal(NULL)
  ai_report <- reactiveVal(NULL)
  token <- Sys.getenv("SOCRATA_APP_TOKEN")
  ollama_key <- Sys.getenv("OLLAMA_API_KEY")
  if (!nzchar(ollama_key)) ollama_key <- Sys.getenv("OOLAMA_API_KEY")  # fallback for common typo
  env_ok <- nzchar(token) && nzchar(ollama_key)

  output$env_alert_ui <- renderUI({
    if (env_ok) return(NULL)
    missing <- c()
    if (!nzchar(token)) missing <- c(missing, "SOCRATA_APP_TOKEN")
    if (!nzchar(ollama_key)) missing <- c(missing, "OLLAMA_API_KEY (or OOLAMA_API_KEY)")
    div(
      class = "alert alert-danger mb-3",
      strong("Missing: ", paste(missing, collapse = ", ")),
      ". Add them to .env in the app folder or project root."
    )
  })

  # ---- Data load ----
  observe({
    if (!env_ok || !is.null(cdc_df())) return()
    withProgress(message = "Loading CDC data…", value = 0, {
      tryCatch({
        raw <- fetch_cdc_data(token)
        df <- cdc_to_df(raw)
        cdc_df(df)
        log_step("Fetch", paste0(nrow(df), " records"), success = TRUE)
        methods <- sort(unique(df$injury_mechanism))
        updateSelectInput(session, "method", choices = methods, selected = methods[1])
        years <- sort(unique(df$year), decreasing = TRUE)
        updateSelectInput(session, "year", choices = years, selected = years[1])
        output$status_ui <- renderUI({
          div(class = "alert alert-success mt-3", strong(nrow(df)), " records loaded (pediatric &lt; 20).")
        })
        # Run Poisson when data is ready
        run_poisson(df)
      }, error = function(e) {
        cdc_df(dplyr::tibble())
        log_step("Fetch", conditionMessage(e), success = FALSE)
        output$status_ui <- renderUI({
          div(class = "alert alert-danger mt-3", strong("Error: "), conditionMessage(e))
        })
      })
    })
  })

  run_poisson <- function(df) {
    if (is.null(df) || nrow(df) == 0) return()
    tryCatch({
      agg <- df %>%
        group_by(year) %>%
        summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
        arrange(year)
      if (nrow(agg) < 2) stop("Need at least 2 years for regression.")
      m <- glm(total_deaths ~ year, data = agg, family = poisson)
      coef_yr <- coef(m)["year"]
      rr <- exp(coef_yr)
      pv <- summary(m)$coefficients["year", "Pr(>|z|)"]
      total_row <- data.frame(
        scope = "Total",
        rate_ratio = round(rr, 4),
        p_value = format.pval(pv, digits = 3),
        stringsAsFactors = FALSE
      )
      methods <- unique(df$injury_mechanism)
      by_method_rows <- lapply(methods, function(meth) {
        sub <- df %>% filter(injury_mechanism == meth) %>%
          group_by(year) %>% summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")
        if (nrow(sub) < 2) return(data.frame(scope = meth, rate_ratio = NA_real_, p_value = NA_character_, stringsAsFactors = FALSE))
        m2 <- tryCatch(glm(total_deaths ~ year, data = sub, family = poisson), error = function(e) NULL)
        if (is.null(m2)) return(data.frame(scope = meth, rate_ratio = NA_real_, p_value = NA_character_, stringsAsFactors = FALSE))
        rr2 <- exp(coef(m2)["year"])
        pv2 <- summary(m2)$coefficients["year", "Pr(>|z|)"]
        data.frame(scope = meth, rate_ratio = round(rr2, 4), p_value = format.pval(pv2, digits = 3), stringsAsFactors = FALSE)
      })
      tbl <- bind_rows(total_row, bind_rows(by_method_rows))
      res <- list(
        aggregated = agg,
        model_total = list(rate_ratio = rr, p_value = pv, coefficient = coef_yr),
        table = tbl,
        summary_stats = list(
          n_years = nrow(agg),
          min_year = min(agg$year),
          max_year = max(agg$year),
          total_deaths = sum(agg$total_deaths)
        )
      )
      poisson_results(res)
      log_step("Poisson", "OK", success = TRUE)
      output$poisson_status_ui <- renderUI({
        div(class = "alert alert-success mt-2", "Poisson regression completed.")
      })
    }, error = function(e) {
      log_step("Poisson", conditionMessage(e), success = FALSE)
      output$poisson_status_ui <- renderUI({
        div(class = "alert alert-danger mt-2", strong("Error: "), conditionMessage(e))
      })
    })
  }

  output$poisson_table <- renderTable({
    pr <- poisson_results()
    if (is.null(pr) || is.null(pr$table)) return(NULL)
    pr$table
  }, striped = TRUE)

  plot_null <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = msg, size = 5, colour = "gray40") +
      ggplot2::theme_void()
  }

  output$plot_by_method <- renderPlot({
    df <- cdc_df()
    if (is.null(df) || nrow(df) == 0) return(plot_null("No data"))
    method <- input$method
    if (is.null(method) || method == "") return(plot_null("Select a method"))
    by_year <- df %>% filter(injury_mechanism == method) %>%
      group_by(year) %>% summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")
    if (nrow(by_year) == 0) return(plot_null("No data for this method"))
    ggplot(by_year, aes(x = factor(year), y = deaths)) +
      geom_col(fill = "#2c5282", width = 0.7) +
      labs(x = "Year", y = "Deaths", title = paste("Deaths by year —", method)) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(colour = "#1e3a5f", face = "bold", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(12, 16, 12, 12))
  }, res = 96, height = 400)

  output$plot_by_year <- renderPlot({
    df <- cdc_df()
    if (is.null(df) || nrow(df) == 0) return(plot_null("No data"))
    yr <- input$year
    if (is.null(yr) || yr == "") return(plot_null("Select a year"))
    yr <- as.integer(yr)
    by_method <- df %>% filter(year == yr) %>%
      group_by(injury_mechanism) %>% summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(deaths))
    if (nrow(by_method) == 0) return(plot_null("No data for this year"))
    ggplot(by_method, aes(x = reorder(injury_mechanism, deaths), y = deaths)) +
      geom_col(fill = "#1e3a5f", width = 0.75) + coord_flip() +
      labs(x = NULL, y = "Deaths", title = paste("Deaths by method —", yr)) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(colour = "#1e3a5f", face = "bold", size = 16),
            panel.grid.major.y = element_blank(), plot.margin = margin(12, 12, 12, 8))
  }, res = 96, height = 500)

  # ---- AI report ----
  can_generate <- reactive({
    env_ok && !is.null(cdc_df()) && nrow(cdc_df()) > 0 && !is.null(poisson_results())
  })

  output$btn_generate_ui <- renderUI({
    if (can_generate()) {
      actionButton("btn_generate_report", "Generate AI report", class = "btn-primary mb-2")
    } else {
      div(class = "text-muted small mb-2", "Load data and wait for Poisson regression (Data & Plots tab) to enable this button.")
    }
  })

  observeEvent(input$btn_generate_report, {
    if (!can_generate()) return()
    pr <- poisson_results()
    if (is.null(pr)) return()
    results_json <- jsonlite::toJSON(list(
      summary_stats = pr$summary_stats,
      aggregated_data = pr$aggregated,
      model = pr$model_total,
      by_method_table = pr$table
    ), auto_unbox = TRUE, pretty = TRUE)
    if (nchar(results_json) > 8000) results_json <- paste0(substr(results_json, 1, 8000), "\n... (truncated)")

    prompt <- paste0(
      "Below are summary statistics, aggregated data by year, and a Poisson regression model ",
      "for pediatric suicide death counts (under 20; CDC age groups < 15 and 15-19) from a US CDC dataset. ",
      "The data are annual (one total per year); do not refer to weekly, monthly, or daily data. ",
      "Important: Population data is not included, so we can only ascertain change in counts (number of deaths per year), ",
      "not in rates. In your report, state clearly that the trend refers to counts, not rates.\n\n",
      "Write a short interpretation (2–3 paragraphs or bullet points) that includes: ",
      "(1) the trend (rate ratio per year for counts) and p-value from the model, ",
      "(2) main findings, and (3) one or two cautious recommendations. ",
      "Use clear, plain language. Do not make up numbers; only use the provided data.\n\n",
      "Data and model results:\n", results_json
    )

    output$ai_loading_ui <- renderUI({
      div(class = "alert alert-info", "Calling Ollama Cloud…")
    })
    ai_report(NULL)

    tryCatch({
      req <- request(OLLAMA_URL) %>%
        req_headers(
          "Authorization" = paste0("Bearer ", ollama_key),
          "Content-Type" = "application/json"
        ) %>%
        req_body_json(list(
          model = "gpt-oss:20b-cloud",
          messages = list(list(role = "user", content = prompt)),
          stream = FALSE
        )) %>%
        req_timeout(120)
      resp <- req_perform(req)
      if (resp_status(resp) != 200) {
        log_step("Ollama", paste0("API status ", resp_status(resp)), success = FALSE)
        stop("Ollama API returned ", resp_status(resp), ". ", resp_body_string(resp))
      }
      body <- resp_body_json(resp)
      report <- body$message$content
      if (is.null(report)) stop("No content in Ollama response.")
      ai_report(report)
      log_step("Ollama", "OK", success = TRUE)
      output$ai_loading_ui <- renderUI(NULL)
    }, error = function(e) {
      log_step("Ollama", conditionMessage(e), success = FALSE)
      output$ai_loading_ui <- renderUI({
        div(class = "alert alert-danger", strong("Error: "), conditionMessage(e))
      })
    })
  })

  output$ai_report_ui <- renderUI({
    report <- ai_report()
    if (is.null(report) || report == "") return(NULL)
    div(
      class = "mt-3 p-3 border rounded bg-light",
      style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;",
      report
    )
  })

  output$ai_docx_ui <- renderUI({
    if (is.null(ai_report()) || ai_report() == "") return(NULL)
    downloadButton("download_docx", "Download report as .docx", class = "btn-secondary mt-2")
  })

  output$download_docx <- downloadHandler(
    filename = function() {
      paste0("pediatric_suicide_ai_report_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx")
    },
    content = function(file) {
      report <- ai_report()
      if (is.null(report) || report == "") return()
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Pediatric Suicide Methods — AI Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")), style = "Normal")
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, report, style = "Normal")
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
