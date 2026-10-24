# Naiia Health — static website

Open the published website at https://saeed-saffari.github.io/CPHSC/stories/healthlocate/ .

The page loads `data/boundaries.geojson` and `data/community-profiles.json` on the first community lookup, then caches them in memory. These files were regenerated directly from the original shapefile and Health Atlas CSV, not extracted from the earlier single HTML export.

- `data/boundaries.geojson`: 301 full-detail Community Environ boundaries, transformed to WGS84 longitude/latitude. No simplification.
- `data/community-profiles.json`: calculated indicators, grades, provincial averages, and community summaries keyed by Community Environ ID.
- `data/source/NSHealthAtlasDataEnvirons.csv`: original local source CSV, unchanged.
- `data/metadata.json`: source hashes, CRS, and dataset availability.
- `scripts/build_data.py`: reproducible data exporter. Requires the original HealthLocate checkout and its Python dependencies.

Original project: https://github.com/MarianaMera12/HealthLocate/tree/343563b90eaec729173ef618a91104a7d81a487f . The shapefile components remain in that source repository. The CSV was downloaded from its `main` branch. Metadata records the exact local input hashes.

To rebuild, with the original project and data in place:

```sh
/path/to/HealthLocate/.venv/bin/python scripts/build_data.py /path/to/HealthLocate .
```

To view locally, run `python3 -m http.server 8000` in this website folder, then open http://127.0.0.1:8000 . Double-clicking index.html cannot load separate JSON files in most browsers.

Maps, address autocomplete, and nearby services require internet and public API availability, including browser CORS support. No private API keys are included. Leaflet is loaded from unpkg; map tiles are from OpenStreetMap with attribution. Address lookups use Nova Scotia Open Data; services use OpenStreetMap Overpass.

The ALE dataset is not present, so transit indicators remain pending. The original backend's profile calculations are retained, including its fixed placeholder environment grade pending the EQI composite; that grade is not a calculated environmental assessment.
