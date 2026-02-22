# SIMPLE — What the app does, how to run, how to submit

*(This file is just for you: step-by-step flow, running locally, and submission.)*

---

## What the app does at each step

1. **Startup**  
   The app loads `.env` from the app folder or project root and checks for `SOCRATA_APP_TOKEN` and `OLLAMA_API_KEY`. If either is missing, a red banner appears at the top and the app does not call the APIs.

2. **Data load**  
   When both tokens are present, the app fetches pediatric suicide data from the CDC Socrata API (age groups &lt; 15 and 15–19, i.e. under 20). It parses the JSON into a table and fills the method and year dropdowns. Any error is written to `app_error_log.txt` and shown in the Data tab.

3. **Data & Plots tab**  
   You pick an injury method and see a bar chart of deaths by year. You pick a year and see a bar chart of deaths by method for that year.

4. **Poisson regression**  
   As soon as data is loaded, the app runs a Poisson model: total deaths ~ year, and one model per injury method. Results (rate ratio and p-value) are shown in the **Poisson Regression** tab. Success or failure is logged to `app_error_log.txt`.

5. **AI report**  
   In the **AI Report** tab, once data and Poisson are ready, a “Generate AI report” button appears. Clicking it sends the Poisson results (as JSON) to Ollama Cloud with a fixed prompt (pediatric under 20, counts not rates, 2–3 paragraphs). The report appears in the app. You can then click “Download report as .docx” to save it.

6. **Errors**  
   There is no error log tab in the app. All step outcomes (and errors) are written to **`app/app_error_log.txt`**. Open that file in a text editor, or run `tail -f app/app_error_log.txt` from the project root while using the app, to see exactly where something failed.

---

## How to run from your computer

1. **Open a terminal** and go to the project root (the folder that contains the `app` folder):
   ```bash
   cd /path/to/DataAI
   ```

2. **Create `.env`** in the project root or inside `app/` with:
   - `SOCRATA_APP_TOKEN=your_cdc_token`
   - `OLLAMA_API_KEY=your_ollama_cloud_key`  
   (Get CDC token from data.cdc.gov; Ollama key from Ollama Cloud.)

3. **Run the app:**
   ```bash
   Rscript app/run.R
   ```
   The first time, R may install missing packages (shiny, bslib, httr2, jsonlite, dplyr, ggplot2, officer). The app opens in your browser.

4. **If something doesn’t work**  
   - Check **`app/app_error_log.txt`** first (see which step failed).  
   - Then check **`.env`** (both tokens set, no typos; the variable is `OLLAMA_API_KEY`, not `OOLAMA`).

---

## How to submit (per professor)

- **Deliverable**: One **.docx** file submitted via Canvas by the due date.
- **Contents** (from HOMEWORK1.md):
  1. **Writing (25 pts)** — Short explanation in your own words: what the software does, how the parts work together, design choices or challenges (3+ paragraphs).
  2. **Git repository links (25 pts)** — Working links to: API query script (`app/methods.py`), Shiny app code (`app/app.R`), AI reporting (same app; you can link to the AI section or `app.R`), and main application file (`app/run.R` or `app/app.R`).
  3. **Screenshots/outputs (25 pts)** — At least 3–4: Shiny app with data, AI-generated report, app interface with controls, and (if applicable) error handling.
  4. **Documentation (25 pts)** — Data summary table (columns, types, descriptions), technical details (API, keys, packages, file structure), and usage (how to install dependencies, set .env, run the app). The app **README.md** covers this; you can summarize or refer to it in the .docx.

Put all four sections into a single Word document and upload that to Canvas.
