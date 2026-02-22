# Pediatric Suicide Methods — Shiny App

## 📌 Overview

This Shiny app uses the **CDC injury mortality API** (Socrata, `data.cdc.gov`) to show **pediatric suicide** data (ages under 20: CDC groups &lt; 15 and 15–19, covering through 18) by injury method. It provides two views: deaths by year for a chosen method, and deaths by method for a chosen year. The UI uses a sleek blue theme and is built for clarity and ease of use.

## ✨ Features

- **API integration** — Fetches pediatric suicide data from CDC on load (no manual refresh).
- **Deaths by method** — Choose an injury method (e.g. firearm, suffocation) and see a bar chart of deaths per year.
- **Methods by year** — Choose a year and see a horizontal bar chart of all injury methods for that year.
- **Error handling** — Clear message if `SOCRATA_APP_TOKEN` is missing or the API fails.
- **Modern UI** — Blue theme (bslib), responsive layout, and simple controls.

## ⚙️ Requirements

- R 4.0+
- Packages: `shiny`, `bslib`, `httr2`, `jsonlite`, `dplyr`, `ggplot2`
- A **Socrata app token** for `data.cdc.gov` ([request one here](https://data.cdc.gov/profile/edit/developer_settings))

## 📦 Installation

1. Install R dependencies:

```bash
Rscript -e "install.packages(c('shiny','bslib','httr2','jsonlite','dplyr','ggplot2'), repos = 'https://cloud.r-project.org')"
```

2. Copy `.env.example` to `.env` in the `app` folder and add your token:

```bash
cp app/.env.example app/.env
# Edit app/.env and set SOCRATA_APP_TOKEN=your_token
```

## 🚀 Usage

**From project root:**

```bash
Rscript app/run.R
```

**From R (project root):**

```r
setwd("app")
shiny::runApp(".")
```

**From R (already in `app` folder):**

```r
shiny::runApp(".")
```

The app will open in your browser. Data loads once at startup. Use the two dropdowns to switch method and year; charts update automatically.

## 📁 Project Structure

```
app/
├── app.R          # Shiny UI and server logic, CDC API fetch
├── run.R          # Launcher (sets working dir, runs app)
├── .env.example   # Template for SOCRATA_APP_TOKEN
├── .env           # Your token (create from .env.example; do not commit)
└── README.md      # This file
```

## 📊 Output

- **Left card** — Bar chart of **deaths by year** for the selected injury method (e.g. “Firearm”, “Suffocation”).
- **Right card** — Horizontal bar chart of **deaths by injury method** for the selected year.
- **Status** — Message under the cards: record count on success, or an error if the token is missing or the API request fails.

All data is restricted to **pediatric** suicides (CDC age groups &lt; 15 and 15–19, i.e. under 20 / through 18) in the CDC dataset.
