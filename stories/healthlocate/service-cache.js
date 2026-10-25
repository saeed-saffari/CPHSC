/* Naiia Health: service search cache.
   Remembers nearby-service results so switching between Pharmacy / Mental Health / etc.,
   and printing, never searches the same place twice. Kept in memory only (cleared on refresh,
   nothing about patients is stored in the browser).
   Load AFTER data-api.js and BEFORE app.js. To switch it off, remove its <script> line. */
(function () {
  'use strict';
  var origFetch = window.localFetch;
  if (typeof origFetch !== 'function') return;

  var PREFETCH = true;            // load the other service types quietly after a profile opens
  var TTL = 30 * 60 * 1000;       // keep results for 30 minutes
  var cache = new Map(), runId = 0;
  var JSON_HDR = { 'Content-Type': 'application/json' };

  function keyFor(u) {
    var p = u.searchParams, lat = Number(p.get('lat')), lng = Number(p.get('lng')), cat = p.get('category');
    if (!cat || !isFinite(lat) || !isFinite(lng)) return null;
    return cat + '|' + lat.toFixed(3) + '|' + lng.toFixed(3);   // ~100 m
  }

  function cachedServices(path, key) {
    var hit = cache.get(key);
    if (!hit || Date.now() - hit.t > TTL) {
      var entry = hit = {
        t: Date.now(),
        p: origFetch(path).then(function (r) {
          return r.text().then(function (text) {
            if (!r.ok) { var e = new Error(text); e.status = r.status; e.text = text; throw e; }
            return text;
          });
        })
      };
      cache.set(key, entry);
      // never keep failures or empty answers, so the next click tries again
      entry.p.then(
        function (text) { if (text.trim() === '[]' && cache.get(key) === entry) cache.delete(key); },
        function () { if (cache.get(key) === entry) cache.delete(key); }
      );
    }
    // the pending request is shared: simultaneous clicks reuse it. Each caller gets its own Response.
    return hit.p.then(
      function (text) { return new Response(text, { status: 200, headers: JSON_HDR }); },
      function (e) { return new Response(e.text || '{"detail":"Service lookup failed."}', { status: e.status || 502, headers: JSON_HDR }); }
    );
  }

  function prefetch(lat, lng) {
    if (!PREFETCH) return;
    var id = ++runId;                       // a newer address cancels an older prefetch
    setTimeout(function () {
      var cats = [].map.call(document.querySelectorAll('.svc-cat'), function (b) { return b.getAttribute('data-cat'); }).filter(Boolean), i = 0;
      (function next() {                    // one at a time, to stay gentle on the free servers
        if (id !== runId || i >= cats.length) return;
        window.localFetch('/api/services?lat=' + lat + '&lng=' + lng + '&category=' + cats[i++])
          .catch(function () {})
          .then(function () { setTimeout(next, 400); });
      })();
    }, 1500);
  }

  window.localFetch = function (path) {
    var u;
    try { u = new URL(path, 'https://local.invalid'); } catch (e) { return origFetch(path); }
    if (u.pathname === '/api/services') {
      var key = keyFor(u);
      if (key) return cachedServices(path, key);
    }
    if (u.pathname === '/api/profile') {
      var lat = u.searchParams.get('lat'), lng = u.searchParams.get('lng');
      return origFetch(path).then(function (r) { if (r.ok) prefetch(lat, lng); return r; });
    }
    return origFetch(path);
  };
})();
