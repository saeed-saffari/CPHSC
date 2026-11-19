"""Split the source-derived web datasets into exact per-community bundles.
Run after rebuilding boundaries.geojson and community-profiles.json.
"""
import json
from pathlib import Path

site = Path(__file__).resolve().parents[1]
data = site / 'data'
boundaries = json.loads((data / 'boundaries.geojson').read_text())
profiles = json.loads((data / 'community-profiles.json').read_text())['profiles']
folder = data / 'communities'
folder.mkdir(exist_ok=True)
index = []
for feature in boundaries['features']:
    key = str(feature['properties']['id'])
    profile = profiles[key]
    assert str(profile['ce_id']) == key
    bundle = {'geometry': feature['geometry'], 'profile': profile}
    (folder / f'{key}.json').write_text(json.dumps(bundle, ensure_ascii=False, separators=(',', ':'), allow_nan=False))
    index.append({'id': feature['properties']['id'], 'bbox': feature['bbox']})
assert len(index) == len({item['id'] for item in index})
(data / 'community-index.json').write_text(json.dumps(index, separators=(',', ':'), allow_nan=False))
print(f'Generated {len(index)} exact community bundles and a {(data / "community-index.json").stat().st_size:,}-byte index.')
