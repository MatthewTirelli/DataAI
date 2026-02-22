import os
import requests
from dotenv import load_dotenv

"""
API name: Socrata Open Data API (SODA) via data.cdc.gov
Dataset: CDC injury mortality dataset (dataset id inferred from endpoint below)
Endpoint: https://data.cdc.gov/resource/nt65-c7a7.json

Query goal:
- Pediatric suicides (age_years < 18)
- Include method via injury_mechanism
- Return 20 records for inspection / reporting
"""

def main():
    load_dotenv()

    app_token = os.getenv("SOCRATA_APP_TOKEN")
    if not app_token:
        raise ValueError("Missing SOCRATA_APP_TOKEN in .env")

    url = "https://data.cdc.gov/resource/nt65-c7a7.json"

    # Pediatric suicides + method
    params = {
        "$select": (
            "year,sex,age_years,race,injury_intent,injury_mechanism,"
            "deaths,population,age_specific_rate,unit"
        ),
        "$where": "injury_intent = 'Suicide' AND age_years = '< 15'",
        "$order": "year DESC",
        "$limit": 20
    }

    headers = {"X-App-Token": app_token}

    response = requests.get(url, headers=headers, params=params, timeout=30)

    # Required for assignment
    print("HTTP status code:", response.status_code)

    # Fail loudly if not successful
    try:
        response.raise_for_status()
    except requests.HTTPError:
        print("Error body (truncated):", response.text[:500])
        raise

    data = response.json()

    print("Number of records returned:", len(data))
    if not data:
        print("No records returned. Check filters or available years.")
        return

    print("Keys in first record:", list(data[0].keys()))

    print("\nFirst 3 records:")
    for row in data[:3]:
        print(row)

if __name__ == "__main__":
    main()