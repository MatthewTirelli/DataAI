# Pediatric Suicide Methods + Ollama — Shiny App (V2)

## Overview

This app (V2) queries the CDC injury mortality API (Socrata), shows pediatric suicide data (under 20), runs Poisson regression, and uses Ollama Cloud to generate an AI report that can describe **main data** (e.g. breakdown of deaths in a given year, total deaths by method) as well as the Poisson trend. The report is formatted with bold text and supports emojis; you can download it as .docx. For the original version, see `app_v1`.

## Data summary (API columns)

| Column            | Type    | Description                                      |
|-------------------|--------|--------------------------------------------------|
| year              | integer| Calendar year                                    |
| sex               | character | Sex (when selected in query)                   |
| age_years         | character | CDC age group (e.g. "&lt; 15", "15-19")       |
| injury_intent     | character | Intent (we filter for "Suicide")              |
| injury_mechanism  | character | Method of injury (e.g. Firearm, Suffocation)  |
| deaths            | integer| Number of deaths                                 |

The app uses **pediatric &lt; 20**: CDC groups `&lt; 15` and `15-19`. Counts are summed by year and/or by method; no population or rates in this app.

## Technical details

- **API**: CDC Socrata, `https://data.cdc.gov/resource/nt65-c7a7.json`. Request an app token at [data.cdc.gov profile](https://data.cdc.gov/profile/edit/developer_settings).
- **Ollama**: Ollama Cloud chat API (`https://ollama.com/api/chat`). You need an Ollama Cloud API key.
- **Env**: Create a `.env` file in the **app** folder (or project root) with:
  - `SOCRATA_APP_TOKEN=your_token`
  - `OLLAMA_API_KEY=your_ollama_key`
- **Logs**: Errors are appended to `app_error_log.txt` in the app folder (backend only; no log tab in the UI).

## R packages

- shiny, bslib, httr2, jsonlite, dplyr, ggplot2, officer

## File structure

```
app/
├── app.R              # Shiny UI and server (data, Poisson, Ollama, docx)
├── run.R              # Launcher (installs deps if needed, runs app)
├── methods.py         # Standalone Python pipeline (same data, Poisson, optional Ollama)
├── requirements.txt  # Python deps for methods.py
├── .env.example       # Template for .env
├── .env               # Your tokens (do not commit)
├── app_error_log.txt  # Backend error log (created at runtime; add to .gitignore)
├── README.md          # This file
└── SIMPLE.md          # Short run/submit guide (for the student)
```

## Usage

1. **R packages** (from R or via run.R):
   ```r
   install.packages(c("shiny", "bslib", "httr2", "jsonlite", "dplyr", "ggplot2", "officer"), repos = "https://cloud.r-project.org")
   ```

2. **.env**: Copy `.env.example` to `.env` and set `SOCRATA_APP_TOKEN` and `OLLAMA_API_KEY`.

3. **Run the app** (from project root):
   ```bash
   Rscript app/run.R
   ```
   Or from R:
   ```r
   setwd("app")
   shiny::runApp(".")
   ```

4. **Optional — run the Python pipeline** (reproducibility / API script):
   ```bash
   cd app && pip install -r requirements.txt && python methods.py
   ```
   Writes `methods_results.json` and `methods_report.md` (and uses the same `app_error_log.txt`).

## Tabs

- **Data & Plots**: Deaths by year for a chosen method; deaths by method for a chosen year.
- **Poisson Regression**: Table of rate ratio and p-value for total and by injury method.
- **AI Report**: Generate Ollama report from Poisson results; download as .docx.
