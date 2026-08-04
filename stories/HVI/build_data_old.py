"""
build_data.py  (v2)
====================
Builds the data files consumed by the Nova Scotia Heat Vulnerability Index
dashboard from your working data table.

This version expects a single spreadsheet with one row per community
cluster and these columns (row 1 is a merged category header, so the
real header is row 2 -> header=1 when read with pandas):

    id, region, name,
    Age < 9 yrs, Age > 65 yrs, Indigenous, African NS, Active Transport,
    Outdoor Workers, Chronic Conditions,                      <- Sensitivity
    Heatwave Score, Air Quality PM2.5, Air Quality NO2, EHS Response,  <- Exposure
    Material Security, Social Connectivity, Social Diversity,
    Green Space, Blue Space,                                  <- Adaptive Capacity
    HVI, Normalized HVI,
    Exposure (normalized), Sensitivity (normalized), Adaptive Capacity (normalized)

Geometry: Excel cannot store geometry, so this script does NOT read
boundaries from the spreadsheet. Instead it joins your attribute table
(by `id`) onto an existing boundaries.geojson (54 features, one per
cluster, each with a single `id` property). Point BOUNDARIES_PATH at
whichever boundaries.geojson you're using — the one already generated
from ComEnviron.shp, or a better one if you export real per-cluster
geometry later (e.g. from your merged shapefile: save it as GeoJSON
with `id` matching this table's `id`, and it'll drop in with no other
changes needed).

Outputs (written to OUT_DIR):
  - community_data.json   one record per cluster: id, name, overall
                           HVI, and every sub-indicator (0-100 scale)
  - community_data.csv    same data, flattened, for spreadsheet use
  - boundaries.geojson    copied through unchanged (only re-run the
                           geometry step separately if you get new
                           geometry)

Usage:
    pip install pandas openpyxl --break-system-packages
    python3 build_data.py
"""

import json
import os

import pandas as pd

# ------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------

WORKING_DATA_PATH = "working_data_Aug3.xlsx"
BOUNDARIES_PATH = "boundaries.geojson"   # swap in real per-cluster geometry here if/when you have it
OUT_DIR = "."

# Maps the (whitespace-trimmed) spreadsheet column name for each
# sub-indicator to a short, code-friendly key, and the ring (dial tier)
# it belongs to: 1 = Exposure, 2 = Sensitivity, 3 = Adaptive Capacity.
INDICATOR_COLUMNS = {
    "heatWave":        ("Heatwave Score",      1),
    "airQualityPM25":  ("Air Quality PM2.5",   1),
    "airQualityNO2":   ("Air Quality NO2",     1),
    "emsResponse":     ("EHS Response",        1),
    "age9":            ("Age < 9 yrs",         2),
    "age65":           ("Age > 65 yrs",        2),
    "indigenous":      ("Indigenous",          2),
    "africanNS":       ("African NS",          2),
    "activeTransport": ("Active Transport",    2),
    "outdoorWorkers":  ("Outdoor Workers",     2),
    "chronicConditions": ("Chronic Conditions", 2),
    "materialSecurity": ("Material Security",  3),
    "socialConnect":   ("Social Connectivity", 3),
    "socialDiversity": ("Social Diversity",    3),
    "greenSpace":      ("Green Space",         3),
    "blueSpace":       ("Blue Space",          3),
}
OVERALL_COLUMN = "Normalized HVI"   # 0-1 scale in the sheet; scaled to 0-100 below


# ------------------------------------------------------------------------
# STEP 1 — load & clean the working data spreadsheet
# ------------------------------------------------------------------------

def load_working_data(path):
    df = pd.read_excel(path, header=1)           # row 0 is a merged category header
    df = df.dropna(subset=["id"]).reset_index(drop=True)
    df.columns = [c.strip() for c in df.columns]  # strips stray leading spaces (" African NS" etc.)
    df["id"] = df["id"].astype(int)
    df["name"] = df["name"].str.strip()
    return df


# ------------------------------------------------------------------------
# STEP 2 — build one record per community
# ------------------------------------------------------------------------

def build_community_records(df, indicator_columns, overall_column):
    records = []
    for _, row in df.iterrows():
        vals = {
            key: round(float(row[col]) * 100, 1)
            for key, (col, ring) in indicator_columns.items()
        }
        records.append({
            "id": int(row["id"]),
            "name": row["name"],
            "overall": round(float(row[overall_column]) * 100, 1),
            "vals": vals,
        })
    return records


# ------------------------------------------------------------------------
# STEP 3 — export
# ------------------------------------------------------------------------

def export_json(records, path):
    with open(path, "w") as f:
        json.dump(records, f, separators=(",", ":"))


def export_csv(records, path, indicator_columns):
    rows = []
    for r in records:
        row = {"id": r["id"], "name": r["name"], "overall": r["overall"]}
        row.update(r["vals"])
        rows.append(row)
    cols = ["id", "name", "overall"] + list(indicator_columns.keys())
    pd.DataFrame(rows)[cols].to_csv(path, index=False)


def copy_boundaries(src_path, dst_path, valid_ids):
    with open(src_path) as f:
        gj = json.load(f)
    ids_in_file = {f["properties"]["id"] for f in gj["features"]}
    missing = valid_ids - ids_in_file
    extra = ids_in_file - valid_ids
    if missing:
        print(f"  WARNING: {len(missing)} community ids have no matching boundary polygon: {sorted(missing)}")
    if extra:
        print(f"  NOTE: boundaries.geojson has {len(extra)} extra polygon(s) not in the attribute table: {sorted(extra)}")
    with open(dst_path, "w") as f:
        json.dump(gj, f, separators=(",", ":"))


# ------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------

def main():
    print("Loading working data...")
    df = load_working_data(WORKING_DATA_PATH)
    print(f"  {len(df)} community clusters, {len(INDICATOR_COLUMNS)} sub-indicators")

    print("Building community records...")
    records = build_community_records(df, INDICATOR_COLUMNS, OVERALL_COLUMN)

    json_path = os.path.join(OUT_DIR, "community_data.json")
    csv_path = os.path.join(OUT_DIR, "community_data.csv")
    boundaries_out = os.path.join(OUT_DIR, "boundaries.geojson")

    export_json(records, json_path)
    export_csv(records, csv_path, INDICATOR_COLUMNS)

    print(f"Joining against boundary geometry ({BOUNDARIES_PATH})...")
    copy_boundaries(BOUNDARIES_PATH, boundaries_out, {r["id"] for r in records})

    print("Wrote:")
    print(f"  {json_path}     ({os.path.getsize(json_path) / 1024:.1f} KB)")
    print(f"  {csv_path}      ({os.path.getsize(csv_path) / 1024:.1f} KB)")
    print(f"  {boundaries_out}     ({os.path.getsize(boundaries_out) / 1024:.1f} KB)")


if __name__ == "__main__":
    main()
