"""Regenerate web data directly from the original HealthLocate shapefile and CSV.
Usage: python build_data.py /path/to/HealthLocate /path/to/website
Run with the HealthLocate virtual environment (requirements.txt).
"""
import hashlib
import json
import shutil
import sys
from pathlib import Path

source = Path(sys.argv[1]).resolve()
target = Path(sys.argv[2]).resolve()
sys.path.insert(0, str(source))
import backend

out = target / 'data'
(out / 'source').mkdir(parents=True, exist_ok=True)
def write(name, data):
    (out / name).write_text(json.dumps(data, ensure_ascii=False, allow_nan=False, separators=(',', ':')), encoding='utf-8')
features, profiles = [], {}
wgs = backend.GDF.to_crs(4326)
for (_, original), (_, row) in zip(backend.GDF.iterrows(), wgs.iterrows()):
    ce_id = int(row['id'])
    point = backend.gpd.GeoSeries([original.geometry.representative_point()], crs=backend.GDF.crs).to_crs(4326).iloc[0]
    profile = backend.profile(point.y, point.x)
    assert profile['ce_id'] == ce_id, f'Community mismatch: {ce_id}'
    for field in ['ce_geometry', 'address', 'community', 'lat', 'lng']:
        profile.pop(field, None)
    profiles[str(ce_id)] = profile
    features.append({'type': 'Feature', 'id': ce_id, 'bbox': list(row.geometry.bounds), 'properties': {'id': ce_id, 'name': profile['ce_name']}, 'geometry': backend.shapely.geometry.mapping(row.geometry)})
assert len(profiles) == len(features) == len(backend.GDF)
write('boundaries.geojson', {'type': 'FeatureCollection', 'features': features})
write('community-profiles.json', {'schema_version': 1, 'profiles': profiles})
shutil.copy2(source / 'data/NSHealthAtlasDataEnvirons.csv', out / 'source/NSHealthAtlasDataEnvirons.csv')
inputs = [source / 'data/NSHealthAtlasDataEnvirons.csv', source / 'backend.py', *sorted((source/'shapefiles').glob('ComEnviron.*'))]
if (source/'data/ale_community_level.csv').exists():
    inputs.append(source/'data/ale_community_level.csv')
    shutil.copy2(inputs[-1], out/'source/ale_community_level.csv')
write('metadata.json', {'community_count': len(features), 'boundary_crs': 'EPSG:4326', 'simplified': False, 'source_repository': 'https://github.com/MarianaMera12/HealthLocate', 'profile_method': 'Original backend.profile with provincial means calculated from the source atlas CSV', 'ale_available': backend.ALE_CSV.exists(), 'inputs': {str(p.relative_to(source)): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}})
print(f'Generated {len(features)} complete boundaries and profiles from primary files.', flush=True)
