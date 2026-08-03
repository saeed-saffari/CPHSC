# Nova Scotia Heat Vulnerability Index — dashboard

## Files
- `ns-heat-vulnerability-dashboard.html` — the dashboard. No data embedded;
  it fetches the two files below at load time.
- `community_data.json` — one record per community (54 total): id, name,
  overall HVI, and every sub-indicator, 0–100 scale.
- `community_data.csv` — same data, flattened, for opening in Excel/Sheets.
- `boundaries.geojson` — one polygon per community, joined to
  `community_data.json` by `id`.
- `build_data.py` — regenerates the three files above from your working
  spreadsheet. Re-run it whenever the underlying data changes:
  ```
  pip install pandas openpyxl --break-system-packages
  python3 build_data.py
  ```
  If you get real per-cluster geometry later (e.g. exporting your merged
  shapefile as GeoJSON with `id` matching the spreadsheet), just replace
  `boundaries.geojson` with it and re-run — no code changes needed.

## Hosting on GitHub Pages
1. Put all four files (`.html`, `community_data.json`, `boundaries.geojson`,
   and optionally `community_data.csv`/`build_data.py`) in the same folder
   of a GitHub repo — e.g. repo root, or a `/docs` folder.
2. In the repo's Settings → Pages, set the source to that folder/branch.
3. GitHub Pages serves everything over `https://` from the same origin, so
   the dashboard's `fetch('community_data.json')` and
   `fetch('boundaries.geojson')` calls work with no extra configuration —
   no CORS setup, no server code needed.
4. Open `https://<username>.github.io/<repo>/ns-heat-vulnerability-dashboard.html`.

## Testing locally before pushing
Opening the HTML file directly (double-click / `file://`) will NOT work —
browsers block `fetch()` of local files for security. Instead, from the
folder containing all three files, run:
```
python3 -m http.server 8000
```
then open `http://localhost:8000/ns-heat-vulnerability-dashboard.html`.
