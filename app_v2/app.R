# Pediatric Suicide Methods + Ollama — Shiny app (V2)
# CDC Socrata pediatric < 20, Poisson regression, main-data stats, Ollama report, docx export.
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
    p("Pediatric suicide (under 20) from CDC injury mortality data. Data, Poisson regression, and AI report.", class = "text-muted mb-0")
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
          card_header("Deaths by Year for a Method", class = "bg-primary text-white"),
          selectInput("method", "Injury Method", choices = character(0), width = "100%"),
          plotOutput("plot_by_method", height = "420px")
        ),
        card(
          card_header("Methods for a Single Year", class = "bg-primary text-white"),
          selectInput("year", "Year", choices = character(0), width = "100%"),
          plotOutput("plot_by_year", height = "520px")
        )
      ),
      uiOutput("status_ui")
    ),
    nav_panel(
      "Rate Trends (Poisson)",
      card(
        card_header("Trends in Total Deaths Over Time by Method: Rate Ratios and p-Values", class = "bg-primary text-white"),
        p("Rate ratio by method (line plot); P-values in the table below.", class = "text-muted small"),
        plotOutput("poisson_plot", height = "380px"),
        h5("P-value by method", class = "mt-3 mb-2"),
        tableOutput("poisson_pvalue_table"),
        uiOutput("poisson_status_ui")
      )
    ),
    nav_panel(
      "AI Report",
      card(
        card_header("Ollama Cloud Report", class = "bg-primary text-white"),
        p("Choose what the report should focus on, then generate. Download as .docx if desired.", class = "text-muted small"),
        selectInput(
          "report_focus",
          "Report focus",
          choices = c("Year" = "year", "Method" = "method", "Overall Trends" = "poisson"),
          selected = "year",
          width = "100%"
        ),
        uiOutput("report_focus_extra_ui"),
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
          div(class = "alert alert-success mt-3", strong(nrow(df)), " records loaded (pediatric under 20).")
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
        method_of_death = "Total",
        rate_ratio = round(rr, 4),
        p_value = format.pval(pv, digits = 3, eps = 0.001),
        stringsAsFactors = FALSE
      )
      methods <- unique(df$injury_mechanism)
      by_method_rows <- lapply(methods, function(meth) {
        sub <- df %>% filter(injury_mechanism == meth) %>%
          group_by(year) %>% summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")
        if (nrow(sub) < 2) return(data.frame(method_of_death = meth, rate_ratio = NA_real_, p_value = NA_character_, stringsAsFactors = FALSE))
        m2 <- tryCatch(glm(total_deaths ~ year, data = sub, family = poisson), error = function(e) NULL)
        if (is.null(m2)) return(data.frame(method_of_death = meth, rate_ratio = NA_real_, p_value = NA_character_, stringsAsFactors = FALSE))
        rr2 <- exp(coef(m2)["year"])
        pv2 <- summary(m2)$coefficients["year", "Pr(>|z|)"]
        data.frame(method_of_death = meth, rate_ratio = round(rr2, 4), p_value = format.pval(pv2, digits = 3, eps = 0.001), stringsAsFactors = FALSE)
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

  output$poisson_plot <- renderPlot({
    pr <- poisson_results()
    if (is.null(pr) || is.null(pr$table)) return(plot_null("No Poisson results"))
    tbl <- pr$table %>% filter(!is.na(rate_ratio)) %>% arrange(rate_ratio)
    if (nrow(tbl) == 0) return(plot_null("No rate ratios to plot"))
    tbl <- tbl %>% mutate(method_of_death = factor(method_of_death, levels = method_of_death))
    ggplot(tbl, aes(x = method_of_death, y = rate_ratio, group = 1)) +
      geom_line(colour = "#1e3a5f", linewidth = 0.9) +
      geom_point(colour = "#2c5282", size = 3.5) +
      scale_y_continuous(labels = function(x) sprintf("%.3f", x)) +
      labs(x = "Method of Death", y = "Rate Ratio") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(colour = "#1e3a5f", face = "bold", size = 14),
        axis.title = element_text(colour = "#333"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(12, 12, 12, 12)
      )
  }, res = 96, height = 360)

  output$poisson_pvalue_table <- renderTable({
    pr <- poisson_results()
    if (is.null(pr) || is.null(pr$table)) return(NULL)
    pr$table %>%
      select(`Method of Death` = method_of_death, `P-value` = p_value)
  }, striped = TRUE, align = "lc", width = "100%")

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

  output$report_focus_extra_ui <- renderUI({
    focus <- input$report_focus
    df <- cdc_df()
    if (is.null(focus) || is.null(df) || nrow(df) == 0) return(NULL)
    if (focus == "year") {
      years <- sort(unique(df$year), decreasing = TRUE)
      selectInput("report_focus_year", "Year", choices = years, selected = years[1], width = "100%")
    } else if (focus == "method") {
      methods <- sort(unique(df$injury_mechanism))
      selectInput("report_focus_method", "Method", choices = methods, selected = methods[1], width = "100%")
    } else {
      div(class = "small text-muted mb-2", "Report will focus on overall rate trends and p-values (Poisson).")
    }
  })

  output$btn_generate_ui <- renderUI({
    if (can_generate()) {
      actionButton("btn_generate_report", "Generate AI Report", class = "btn-primary mb-2")
    } else {
      div(class = "text-muted small mb-2", "Load data and wait for Poisson regression (Data & Plots tab) to enable this button.")
    }
  })

  # Convert report text: **bold** and *bold* -> <strong>bold</strong>, newlines -> <br>, for display
  report_to_html <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return("")
    txt <- gsub("&", "&amp;", txt, fixed = TRUE)
    txt <- gsub("<", "&lt;", txt, fixed = TRUE)
    txt <- gsub(">", "&gt;", txt, fixed = TRUE)
    txt <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", txt)
    txt <- gsub("\\*([^*]+)\\*", "<strong>\\1</strong>", txt)
    txt <- gsub("\n", "<br>", txt, fixed = TRUE)
    txt
  }

  observeEvent(input$btn_generate_report, {
    if (!can_generate()) return()
    pr <- poisson_results()
    df <- cdc_df()
    focus <- input$report_focus
    if (is.null(pr) || is.null(df) || is.null(focus)) return()

    by_method_all <- df %>%
      group_by(injury_mechanism) %>%
      summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_deaths))
    sel_year <- if (focus == "year" && !is.null(input$report_focus_year) && nzchar(input$report_focus_year)) as.integer(input$report_focus_year) else max(df$year, na.rm = TRUE)
    sel_method <- if (focus == "method" && !is.null(input$report_focus_method)) input$report_focus_method else NULL
    breakdown_year <- df %>%
      filter(year == sel_year) %>%
      group_by(injury_mechanism) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(deaths))

    if (focus == "year") {
      total_that_year <- sum(breakdown_year$deaths, na.rm = TRUE)
      payload <- list(
        summary_stats = pr$summary_stats,
        total_pediatric_deaths_for_this_year = total_that_year,
        breakdown_for_year = list(year = sel_year, by_method = breakdown_year),
        total_deaths_by_year = pr$aggregated
      )
      prompt <- paste0(
        "Below are pediatric suicide death counts (under 20) from CDC injury mortality data for year ", sel_year, ". ",
        "CRITICAL: The total pediatric deaths for year ", sel_year, " is ", total_that_year, ". The breakdown by method sums to this same number. ",
        "Do NOT report any other total or 'annual total' for that year—there is only one total (", total_that_year, "). ",
        "The data are ONLY for ages under 20. Do not mention all-age totals or percentages. Report only counts. Do not use emojis. ",
        "Use **double asterisks** around text for bold (e.g. **Suffocation**), not single asterisks.\n\n",
        "Write a short report (2–3 paragraphs or bullet points) that: ",
        "(1) summarizes the breakdown of deaths in ", sel_year, " (which methods had the most deaths; use only numbers from the data), ",
        "(2) uses ", total_that_year, " as the only total for that year when giving context, ",
        "(3) gives one cautious recommendation. Use clear, professional language. Do not make up or infer any numbers.\n\n",
        "Data:\n", jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
      )
    } else if (focus == "method") {
      payload <- list(
        summary_stats = pr$summary_stats,
        total_deaths_by_method_all_years = by_method_all,
        selected_method_for_emphasis = sel_method
      )
      prompt <- paste0(
        "Below are pediatric suicide death counts (under 20) from CDC injury mortality data. ",
        "Focus on total deaths by injury method across all years. ",
        if (!is.null(sel_method) && nzchar(sel_method)) paste0("Emphasize the selected method: ", sel_method, ". ") else "",
        "CRITICAL: The data are ONLY for ages under 20. You do NOT have totals for all ages. Use ONLY the numbers provided below. ",
        "Report only counts (no rates). Do not use emojis.\n\n",
        "Write a short, well-formatted report (2–3 paragraphs or bullet points) that: ",
        "(1) summarizes which methods account for the most deaths overall and key totals from the data only, ",
        "(2) compares methods and highlights the selected method if applicable, ",
        "(3) gives one cautious recommendation. ",
        "Use **double asterisks** for bold (e.g. **Firearm**), not single asterisks. Use clear, professional language. Do not make up or infer any numbers.\n\n",
        "Data:\n", jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
      )
    } else {
      payload <- list(
        summary_stats = pr$summary_stats,
        total_deaths_by_year = pr$aggregated,
        poisson_model = pr$model_total,
        poisson_by_method_table = pr$table
      )
      prompt <- paste0(
        "Below are pediatric suicide death counts (under 20) from CDC injury mortality data and a Poisson regression model (total deaths ~ year, and by method). ",
        "Focus on the trend: rate ratio per year, p-value, and whether the year-over-year change is statistically significant. ",
        "CRITICAL: The data are ONLY for ages under 20. You do NOT have totals for all ages. Use ONLY the numbers and model results provided below. ",
        "Report only counts (no rates). Do not use emojis.\n\n",
        "Write a short, well-formatted report (2–3 paragraphs or bullet points) that: ",
        "(1) states the overall trend (rate ratio and p-value from the model), ",
        "(2) notes which methods show significant or notable trends if the table includes per-method results, ",
        "(3) gives one cautious recommendation. ",
        "Use **double asterisks** for bold (e.g. **rate ratio**), not single asterisks. Use clear, professional language. Do not make up or infer any numbers.\n\n",
        "Data and model results:\n", jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
      )
    }

    if (nchar(prompt) > 12000) prompt <- paste0(substr(prompt, 1, 12000), "\n... (truncated)")

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
    html <- report_to_html(report)
    div(
      class = "mt-3 p-3 border rounded bg-light",
      style = "max-height: 400px; overflow-y: auto; line-height: 1.5;",
      HTML(html)
    )
  })

  output$ai_docx_ui <- renderUI({
    if (is.null(ai_report()) || ai_report() == "") return(NULL)
    downloadButton("download_docx", "Download Report as .docx", class = "btn-secondary mt-2")
  })

  output$download_docx <- downloadHandler(
    filename = function() {
      paste0("pediatric_suicide_ai_report_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx")
    },
    content = function(file) {
      report <- ai_report()
      if (is.null(report) || report == "") return()
      report_plain <- gsub("\\*\\*([^*]*)\\*\\*", "\\1", report)
      report_plain <- gsub("\\*([^*]*)\\*", "\\1", report_plain)
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Pediatric Suicide Methods — AI Report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")), style = "Normal")
      doc <- officer::body_add_par(doc, "")
      paragraphs <- strsplit(report_plain, "(\r?\n){2,}", perl = TRUE)[[1]]
      paragraphs <- trimws(paragraphs)
      paragraphs <- paragraphs[nzchar(paragraphs)]
      for (p in paragraphs) {
        doc <- officer::body_add_par(doc, p, style = "Normal")
      }
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
