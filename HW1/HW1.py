# HW1.py
# HW1 – Pediatric suicide methods pipeline
# Single-file pipeline: Socrata fetch → clean → aggregate by year → Poisson model → JSON → Ollama report.
# Matthew Tirelli

# API data columns (for homework Data Summary table):
#   year (int): calendar year
#   injury_mechanism (str): method of injury (e.g. firearm, suffocation)
#   total_deaths (int): number of deaths (from SoQL sum(deaths))

# 0. SETUP ###################################

## 0.1 Load Packages ############################

import os
import json
import requests
import pandas as pd
import numpy as np
from pathlib import Path
from dotenv import load_dotenv
import statsmodels.api as sm

# Load .env from project root first, then from script dir (so .env next to HW1.py also works)
_PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = Path(__file__).resolve().parent
load_dotenv(dotenv_path=_PROJECT_ROOT / ".env")
load_dotenv(dotenv_path=OUT_DIR / ".env")

## 0.2 Environment checks #####################

def _check_env():
    """Fail fast with clear message if required env vars are missing. Never print secret values."""
    token = os.getenv("SOCRATA_APP_TOKEN")
    key = os.getenv("OLLAMA_API_KEY")
    if not token:
        raise ValueError(
            "Step 0 (Setup) failed: SOCRATA_APP_TOKEN not set. Add it to .env in the project root or in 03_query_ai/."
        )
    if not key:
        raise ValueError(
            "Step 0 (Setup) failed: OLLAMA_API_KEY not set. Add it to .env in the project root or in 03_query_ai/."
        )


# 1. Socrata fetch (SoQL) ###################################

def fetch_socrata():
    """Request pediatric suicide data from CDC Socrata API. Returns list of dicts."""
    step = "Step 1 (Socrata fetch)"
    url = "https://data.cdc.gov/resource/nt65-c7a7.json"
    params = {
        "$select": "year, injury_mechanism, sum(deaths) as total_deaths",
        "$where": "injury_intent = 'Suicide' AND age_years = '< 15'",
        "$group": "year, injury_mechanism",
        "$order": "year ASC, total_deaths DESC",
        "$limit": 2000,
    }
    headers = {"X-App-Token": os.getenv("SOCRATA_APP_TOKEN")}

    try:
        response = requests.get(url, headers=headers, params=params, timeout=30)
    except requests.RequestException as e:
        raise RuntimeError(f"{step} failed: Request error. {e}") from e

    if response.status_code != 200:
        raise RuntimeError(
            f"{step} failed: API returned {response.status_code}. "
            "Check your app token and rate limits. Body (truncated): "
            f"{response.text[:300]}"
        )

    try:
        data = response.json()
    except json.JSONDecodeError as e:
        raise RuntimeError(f"{step} failed: Response is not valid JSON. {e}") from e

    if not data:
        raise RuntimeError(
            f"{step} failed: No records returned. Check $where filter and dataset availability."
        )

    expected = {"year", "injury_mechanism", "total_deaths"}
    first_keys = set(data[0].keys())
    if not expected.issubset(first_keys):
        raise RuntimeError(
            f"{step} failed: Expected columns {expected}. Got: {first_keys}."
        )

    return data


# 2. Clean + type coercion ###################################

def clean_and_coerce(data):
    """Convert API response to DataFrame and coerce types. Returns DataFrame."""
    step = "Step 2 (Clean and coerce)"
    df = pd.DataFrame(data)

    # Coerce types; Socrata often returns strings
    for col in ["year", "total_deaths"]:
        if col not in df.columns:
            raise RuntimeError(
                f"{step} failed: Expected column '{col}' not found. Columns: {list(df.columns)}."
            )
        df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(subset=["year", "total_deaths"])
    df["year"] = df["year"].astype(int)
    df["total_deaths"] = df["total_deaths"].astype(int)

    if df["total_deaths"].min() < 0:
        raise RuntimeError(f"{step} failed: Found negative total_deaths. Check data.")

    year_min, year_max = df["year"].min(), df["year"].max()
    if year_min < 1990 or year_max > 2030:
        raise RuntimeError(
            f"{step} failed: Year range [{year_min}, {year_max}] looks wrong. Check data."
        )

    if "injury_mechanism" in df.columns:
        df["injury_mechanism"] = df["injury_mechanism"].astype(str).str.strip()

    return df


# 3. Aggregate to unit of analysis (year) ###################################

def aggregate_by_year(df):
    """Sum total_deaths by year. Returns DataFrame with columns year, total_deaths."""
    step = "Step 3 (Aggregate)"
    agg = (
        df.groupby("year", as_index=False)
        .agg(total_deaths=("total_deaths", "sum"))
        .sort_values("year")
    )

    if len(agg) < 2:
        raise RuntimeError(
            f"{step} failed: Need at least 2 years for regression. Got {len(agg)}."
        )
    if agg["total_deaths"].max() <= 0:
        raise RuntimeError(f"{step} failed: All total_deaths are zero. Check data.")

    return agg


# 4. Fit Poisson model ###################################

def fit_poisson(agg):
    """Fit Poisson regression: total_deaths ~ year. Returns dict with coefficient, rate_ratio, p_value."""
    step = "Step 4 (Poisson model)"
    X = sm.add_constant(agg[["year"]])
    y = agg["total_deaths"]

    try:
        model = sm.GLM(y, X, family=sm.families.Poisson()).fit()
    except Exception as e:
        raise RuntimeError(
            f"{step} failed: Model did not converge or error. {e}. "
            "Check for zero-inflation or collinearity."
        ) from e

    if not model.converged:
        raise RuntimeError(f"{step} failed: Model did not converge.")

    coef_year = float(model.params["year"])
    rate_ratio = float(np.exp(coef_year))
    p_value = float(model.pvalues["year"])

    return {
        "coefficient": coef_year,
        "rate_ratio": rate_ratio,
        "p_value": p_value,
        "converged": model.converged,
    }


# 5. Generate results object (JSON) ###################################

def build_results(agg, model_output):
    """Build results dict and return JSON string. Also return dict for saving."""
    summary = {
        "n_years": int(len(agg)),
        "min_year": int(agg["year"].min()),
        "max_year": int(agg["year"].max()),
        "total_deaths": int(agg["total_deaths"].sum()),
        "mean_deaths_per_year": float(agg["total_deaths"].mean()),
    }
    results = {
        "summary_stats": summary,
        "aggregated_data": agg.to_dict(orient="records"),
        "model": model_output,
    }
    return results, json.dumps(results, indent=2)


# 6. Ollama writes interpretation/report ###################################

def ollama_report(results_json, max_chars=8000):
    """Send results to Ollama Cloud and return the model's report text. Never log API key."""
    step = "Step 6 (Ollama report)"
    key = os.getenv("OLLAMA_API_KEY")
    if not key:
        raise ValueError(
            f"{step} failed: OLLAMA_API_KEY not set. Add it to .env in the project root."
        )

    # Truncate if huge to avoid token limits
    payload = results_json if len(results_json) <= max_chars else results_json[:max_chars] + "\n... (truncated)"
    prompt = (
        "Below are summary statistics, aggregated data by year, and a Poisson regression model "
        "for pediatric suicide death counts (age < 15) from a US CDC dataset. "
        "The data are annual (one total per year); do not refer to weekly, monthly, or daily data. "
        "Important: Population data is not included in this analysis, so we can only ascertain "
        "change in counts (number of deaths per year), not in rates (deaths per population). In your report, "
        "state clearly that the trend refers to counts, not rates.\n\n"
        "Write a short interpretation (2–3 paragraphs or bullet points) that includes: "
        "(1) what the trend is (rate ratio per year for counts) and the p-value from the model, "
        "(2) main findings, and (3) one or two cautious recommendations. "
        "Use clear, plain language. Do not make up numbers; only use the provided data.\n\n"
        f"Data and model results:\n{payload}"
    )

    url = "https://ollama.com/api/chat"
    headers = {
        "Authorization": f"Bearer {key}",
        "Content-Type": "application/json",
    }
    body = {
        "model": "gpt-oss:20b-cloud",
        "messages": [{"role": "user", "content": prompt}],
        "stream": False,
    }

    try:
        response = requests.post(url, headers=headers, json=body, timeout=120)
    except requests.RequestException as e:
        raise RuntimeError(f"{step} failed: Request error. {e}") from e

    if response.status_code != 200:
        raise RuntimeError(
            f"{step} failed: Ollama API error {response.status_code}. "
            f"Body (truncated): {response.text[:300]}"
        )

    try:
        out = response.json()
        report = out["message"]["content"]
    except (KeyError, json.JSONDecodeError) as e:
        raise RuntimeError(f"{step} failed: Could not parse Ollama response. {e}") from e

    return report


# Main ###################################

def main():
    _check_env()

    # 1. Fetch
    data = fetch_socrata()

    # 2. Clean
    df = clean_and_coerce(data)

    # 3. Aggregate
    agg = aggregate_by_year(df)

    # 4. Model
    model_output = fit_poisson(agg)

    # 5. Results JSON
    results_dict, results_json = build_results(agg, model_output)
    out_json = OUT_DIR / "HW1_results.json"
    with open(out_json, "w", encoding="utf-8") as f:
        f.write(results_json)
    print(f"Saved {out_json}")

    # 6. Ollama report
    report = ollama_report(results_json)
    print("\n--- AI Report ---\n")
    print(report)

    out_report = OUT_DIR / "HW1_report.md"
    with open(out_report, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"\nSaved {out_report}")
    print("\nDone.")


if __name__ == "__main__":
    main()
