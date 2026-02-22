# methods.py — Pediatric suicide pipeline (pediatric = under 20: age groups < 15 and 15-19).
# Socrata fetch → clean → aggregate by year → Poisson model → JSON. Optional Ollama report.
# For reproducibility and API-query documentation. Use same age filter as the Shiny app.

import os
import json
import requests
import pandas as pd
import numpy as np
from pathlib import Path
from dotenv import load_dotenv
import statsmodels.api as sm

_PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = Path(__file__).resolve().parent
load_dotenv(dotenv_path=_PROJECT_ROOT / ".env")
load_dotenv(dotenv_path=OUT_DIR / ".env")

LOG_FILE = OUT_DIR / "app_error_log.txt"


def _log(step, message, success=True):
    from datetime import datetime
    ts = datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S UTC")
    status = "OK" if success else "ERROR"
    line = f"[{ts}] {step} — {status}: {message}\n"
    try:
        with open(LOG_FILE, "a", encoding="utf-8") as f:
            f.write(line)
    except Exception:
        pass


def _check_env():
    token = os.getenv("SOCRATA_APP_TOKEN")
    key = os.getenv("OLLAMA_API_KEY") or os.getenv("OOLAMA_API_KEY")  # fallback for typo
    if not token:
        raise ValueError(
            "SOCRATA_APP_TOKEN not set. Add it to .env in the project root or app folder."
        )
    if not key:
        raise ValueError(
            "OLLAMA_API_KEY (or OOLAMA_API_KEY) not set. Add it to .env in the project root or app folder."
        )


def fetch_socrata():
    """Pediatric suicide data from CDC Socrata: age < 15 OR 15-19 (i.e. under 20)."""
    step = "Step 1 (Socrata fetch)"
    url = "https://data.cdc.gov/resource/nt65-c7a7.json"
    params = {
        "$select": "year, injury_mechanism, sum(deaths) as total_deaths",
        "$where": "injury_intent = 'Suicide' AND (age_years = '< 15' OR age_years = '15-19')",
        "$group": "year, injury_mechanism",
        "$order": "year ASC, total_deaths DESC",
        "$limit": 2000,
    }
    headers = {"X-App-Token": os.getenv("SOCRATA_APP_TOKEN")}

    try:
        response = requests.get(url, headers=headers, params=params, timeout=30)
    except requests.RequestException as e:
        _log(step, str(e), success=False)
        raise RuntimeError(f"{step} failed: Request error. {e}") from e

    if response.status_code != 200:
        _log(step, f"API returned {response.status_code}", success=False)
        raise RuntimeError(
            f"{step} failed: API returned {response.status_code}. "
            f"Body (truncated): {response.text[:300]}"
        )

    try:
        data = response.json()
    except json.JSONDecodeError as e:
        _log(step, str(e), success=False)
        raise RuntimeError(f"{step} failed: Response is not valid JSON. {e}") from e

    if not data:
        _log(step, "No records returned", success=False)
        raise RuntimeError(f"{step} failed: No records returned.")

    expected = {"year", "injury_mechanism", "total_deaths"}
    if not expected.issubset(set(data[0].keys())):
        _log(step, f"Expected columns {expected}", success=False)
        raise RuntimeError(f"{step} failed: Expected columns {expected}.")

    _log(step, f"{len(data)} records", success=True)
    return data


def clean_and_coerce(data):
    step = "Step 2 (Clean and coerce)"
    df = pd.DataFrame(data)
    for col in ["year", "total_deaths"]:
        if col not in df.columns:
            raise RuntimeError(f"{step} failed: Missing column '{col}'.")
        df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(subset=["year", "total_deaths"])
    df["year"] = df["year"].astype(int)
    df["total_deaths"] = df["total_deaths"].astype(int)

    if df["total_deaths"].min() < 0:
        raise RuntimeError(f"{step} failed: Negative total_deaths found.")
    if "injury_mechanism" in df.columns:
        df["injury_mechanism"] = df["injury_mechanism"].astype(str).str.strip()

    _log(step, "OK", success=True)
    return df


def aggregate_by_year(df):
    step = "Step 3 (Aggregate)"
    agg = (
        df.groupby("year", as_index=False)
        .agg(total_deaths=("total_deaths", "sum"))
        .sort_values("year")
    )
    if len(agg) < 2:
        raise RuntimeError(f"{step} failed: Need at least 2 years. Got {len(agg)}.")
    _log(step, "OK", success=True)
    return agg


def fit_poisson(agg):
    step = "Step 4 (Poisson model)"
    X = sm.add_constant(agg[["year"]])
    y = agg["total_deaths"]
    try:
        model = sm.GLM(y, X, family=sm.families.Poisson()).fit()
    except Exception as e:
        _log(step, str(e), success=False)
        raise RuntimeError(f"{step} failed: {e}") from e

    if not model.converged:
        _log(step, "Model did not converge", success=False)
        raise RuntimeError(f"{step} failed: Model did not converge.")

    out = {
        "coefficient": float(model.params["year"]),
        "rate_ratio": float(np.exp(model.params["year"])),
        "p_value": float(model.pvalues["year"]),
        "converged": bool(model.converged),
    }
    _log(step, "OK", success=True)
    return out


def build_results(agg, model_output):
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


def ollama_report(results_json, max_chars=8000):
    """Call Ollama Cloud for interpretation. Uses pediatric under 20 (age < 15 and 15-19)."""
    step = "Step 6 (Ollama report)"
    key = os.getenv("OLLAMA_API_KEY") or os.getenv("OOLAMA_API_KEY")
    if not key:
        raise ValueError("OLLAMA_API_KEY (or OOLAMA_API_KEY) not set.")

    payload = results_json if len(results_json) <= max_chars else results_json[:max_chars] + "\n... (truncated)"
    prompt = (
        "Below are summary statistics, aggregated data by year, and a Poisson regression model "
        "for pediatric suicide death counts (under 20; CDC age groups < 15 and 15-19) from a US CDC dataset. "
        "The data are annual (one total per year); do not refer to weekly, monthly, or daily data. "
        "Important: Population data is not included, so we can only ascertain change in counts (number of deaths per year), "
        "not in rates (deaths per population). In your report, state clearly that the trend refers to counts, not rates.\n\n"
        "Write a short interpretation (2–3 paragraphs or bullet points) that includes: "
        "(1) what the trend is (rate ratio per year for counts) and the p-value from the model, "
        "(2) main findings, and (3) one or two cautious recommendations. "
        "Use clear, plain language. Do not make up numbers; only use the provided data.\n\n"
        f"Data and model results:\n{payload}"
    )

    url = "https://ollama.com/api/chat"
    headers = {"Authorization": f"Bearer {key}", "Content-Type": "application/json"}
    body = {
        "model": "gpt-oss:20b-cloud",
        "messages": [{"role": "user", "content": prompt}],
        "stream": False,
    }

    try:
        response = requests.post(url, headers=headers, json=body, timeout=120)
    except requests.RequestException as e:
        _log(step, str(e), success=False)
        raise RuntimeError(f"{step} failed: Request error. {e}") from e

    if response.status_code != 200:
        _log(step, f"API {response.status_code}", success=False)
        raise RuntimeError(f"{step} failed: Ollama API {response.status_code}. {response.text[:300]}")

    try:
        out = response.json()
        report = out["message"]["content"]
    except (KeyError, json.JSONDecodeError) as e:
        _log(step, str(e), success=False)
        raise RuntimeError(f"{step} failed: Could not parse response. {e}") from e

    _log(step, "OK", success=True)
    return report


def main():
    _check_env()
    data = fetch_socrata()
    df = clean_and_coerce(data)
    agg = aggregate_by_year(df)
    model_output = fit_poisson(agg)
    results_dict, results_json = build_results(agg, model_output)

    out_json = OUT_DIR / "methods_results.json"
    with open(out_json, "w", encoding="utf-8") as f:
        f.write(results_json)
    print(f"Saved {out_json}")

    report = ollama_report(results_json)
    print("\n--- AI Report ---\n")
    print(report)
    out_report = OUT_DIR / "methods_report.md"
    with open(out_report, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"\nSaved {out_report}")
    print("Done.")


if __name__ == "__main__":
    main()
