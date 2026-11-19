/* Experimental supplied-CSV pharmacy lookup. Load after data-api.js, before the cache. */
(function () {
  'use strict';
  const originalFetch = window.localFetch;
  let directoryPromise;
  const escape = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const safeURL = value => /^https?:\/\//i.test(value || '') ? escape(value) : null;
  // RFC-style CSV quoting: commas, escaped quotes, CRLF and embedded newlines.
  function parseDirectoryCSV(text) {
    const rows = []; let row = [], cell = '', quoted = false;
    text = text.replace(/^\uFEFF/, '');
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (quoted) {
        if (c === '"' && text[i + 1] === '"') { cell += '"'; i++; }
        else if (c === '"') quoted = false;
        else cell += c;
      } else if (c === '"' && cell === '') quoted = true;
      else if (c === ',') { row.push(cell); cell = ''; }
      else if (c === '\n' || c === '\r') {
        if (c === '\r' && text[i + 1] === '\n') i++;
        row.push(cell); if (row.some(value => value.trim())) rows.push(row);
        row = []; cell = '';
      } else cell += c;
    }
    if (quoted) throw Error('Invalid CSV quoting');
    if (cell || row.length) { row.push(cell); rows.push(row); }
    const header = rows.shift();
    const required = ['place_id', 'name', 'address', 'latitude', 'longitude', 'business_status'];
    if (!header || required.some(key => !header.includes(key))) throw Error('Missing pharmacy CSV columns');
    const ids = new Set();
    return rows.map(values => {
      if (values.length !== header.length) throw Error('Invalid pharmacy CSV row');
      const p = Object.fromEntries(header.map((key, i) => [key, values[i].trim()]));
      const lat = Number(p.latitude), lng = Number(p.longitude);
      if (!p.place_id || ids.has(p.place_id) || !p.latitude || !p.longitude ||
          !Number.isFinite(lat) || !Number.isFinite(lng) || Math.abs(lat) > 90 || Math.abs(lng) > 180)
        throw Error('Invalid pharmacy ID or coordinates');
      ids.add(p.place_id);
      return {...p, latitude:lat, longitude:lng, regular_hours:[],
        last_checked:(p.last_address_check || '').split('T')[0] || 'date unavailable'};
    });
  }
  function loadDirectory() {
    if (!directoryPromise) directoryPromise = fetch('./data/source/hrm_pharmacies.csv', {cache:'no-cache'})
      .then(r => { if (!r.ok) throw Error('Directory unavailable'); return r.text(); })
      .then(parseDirectoryCSV)
      .catch(error => { directoryPromise = null; throw error; });
    return directoryPromise;
  }
  function distance(lat, lng, a, b) {
    const rad = Math.PI / 180;
    const h = Math.sin((a - lat) * rad / 2) ** 2 + Math.cos(lat * rad) * Math.cos(a * rad) * Math.sin((b - lng) * rad / 2) ** 2;
    return 2 * 6371 * Math.asin(Math.min(1, Math.sqrt(h)));
  }
  async function fallback(path, note) {
    const r = await originalFetch(path);
    if (!r.ok) return r;
    const items = await r.json();
    return new Response(JSON.stringify(items.map(item => ({...item, source_note: note}))), {headers:{'Content-Type':'application/json'}});
  }
  window.localFetch = async function (path) {
    const u = new URL(path, 'https://local.invalid');
    if (u.pathname !== '/api/services' || u.searchParams.get('category') !== 'pharmacy') return originalFetch(path);
    const latText = u.searchParams.get('lat'), lngText = u.searchParams.get('lng');
    const lat = Number(latText), lng = Number(lngText);
    if (!latText || !lngText || !Number.isFinite(lat) || !Number.isFinite(lng) || Math.abs(lat) > 90 || Math.abs(lng) > 180)
      return new Response('{"detail":"Invalid coordinates."}', {status:400});
    let directory;
    try { directory = await loadDirectory(); }
    catch { return fallback(path, 'Source: OpenStreetMap. The supplied pharmacy directory could not be loaded.'); }
    const nearest = directory.filter(p => p.business_status === 'OPERATIONAL')
      .map(p => ({...p, distance:distance(lat, lng, p.latitude, p.longitude)}))
      .filter(p => p.distance <= 10)
      .sort((a, b) => a.distance - b.distance || a.place_id.localeCompare(b.place_id)).slice(0, 5);
    if (!nearest.length) return fallback(path, 'Source: OpenStreetMap. The supplied Halifax-region sample has no eligible entries within 10 km.');
    const results = nearest.map(p => ({
      name:escape(p.name), address:escape(p.address), phone:escape(p.phone), website:safeURL(p.website),
      distance_km:Math.round(p.distance * 10) / 10,
      hours:p.regular_hours.length ? escape(p.regular_hours.join('; ') + ' (recorded ' + p.hours_last_checked + ')') : null,
      maps_url:`https://www.google.com/maps/dir/?api=1&destination=${p.latitude},${p.longitude}`,
      source_note:'Source: supplied Halifax-region pharmacy directory (sample coverage). Listings checked ' + escape(p.last_checked) + '.',
    }));
    return new Response(JSON.stringify(results), {headers:{'Content-Type':'application/json'}});
  };
})();
