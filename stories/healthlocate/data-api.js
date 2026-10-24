// Loads data generated from the primary shapefile and Health Atlas CSV.
let communityDataPromise;
function loadCommunityData() {
 if (!communityDataPromise) {
  communityDataPromise = Promise.all(['./data/boundaries.geojson', './data/community-profiles.json'].map(async path => {
   const r = await fetch(path);
   if (!r.ok) throw Error('Community data could not be loaded. Please refresh and try again.');
   return r.json();
  })).then(([boundaries, data]) => boundaries.features.map(f => {
   const profile = data.profiles[String(f.properties.id)];
   if (!profile) throw Error('Community data is incomplete.');
   return {geometry:f.geometry, bbox:f.bbox, profile};
  })).catch(error => {
   communityDataPromise = null;
   throw Error(location.protocol === 'file:' ? 'Open this website on GitHub Pages or through a local web server to load community data.' : error.message);
  });
 }
 return communityDataPromise;
}
const escapeHTML = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
function inRing(x,y,ring) {
  let inside=false;
  for(let i=0,j=ring.length-1;i<ring.length;j=i++) {
    const [xi,yi]=ring[i], [xj,yj]=ring[j];
    if ((yi>y)!==(yj>y) && x<(xj-xi)*(y-yi)/(yj-yi)+xi) inside=!inside;
  }
  return inside;
}
function inGeometry(x,y,g) {
  const polygons=g.type==='Polygon'?[g.coordinates]:g.coordinates;
  return polygons.some(p=>inRing(x,y,p[0])&&!p.slice(1).some(r=>inRing(x,y,r)));
}
const response = (data,status=200) => new Response(JSON.stringify(data),{status,headers:{'Content-Type':'application/json'}});
async function localFetch(path) {
 const u=new URL(path,'https://local.invalid'), p=u.searchParams;
 try {
  if(u.pathname==='/api/profile') {
   const lat=Number(p.get('lat')),lng=Number(p.get('lng'));
   const communityData=await loadCommunityData();
   const found=communityData.find(c=>lng>=c.bbox[0]&&lng<=c.bbox[2]&&lat>=c.bbox[1]&&lat<=c.bbox[3]&&inGeometry(lng,lat,c.geometry));
   if(!found) return response({detail:'No Community Environ for this point.'},404);
   return response({...found.profile,ce_geometry:found.geometry,lat,lng,address:p.get('address')||'',community:p.get('community')||''});
  }
  if(u.pathname==='/api/suggest') {
   const q=(p.get('q')||'').replace(/['"\\;]/g,'').trim();
   if(!q) return response([]);
   const match=q.match(/^(\d+)\s*(.*)$/),where=[];
   if(match) {
    const digits=match[1], ranges=[];
    for(let len=digits.length;len<=6;len++) {
     const low=Number(digits)*10**(len-digits.length),high=low+10**(len-digits.length)-1;
     ranges.push(low===high?`civicnum=${low}`:`civicnum between ${low} and ${high}`);
    }
    where.push('('+ranges.join(' OR ')+')');
    if(match[2]) where.push(`upper(strname) like '${match[2].toUpperCase()}%'`);
   } else where.push(`upper(strname) like '${q.toUpperCase()}%'`);
   const params=new URLSearchParams({'$select':'civicnum,strname,strsuffix,comm,lat,long','$where':where.join(' AND '),'$order':match?'civicnum,strname':'strname,civicnum','$limit':'10'});
   const r=await fetch('https://data.novascotia.ca/resource/tntn-er5g.json?'+params,{signal:AbortSignal.timeout(15000)});
   if(!r.ok) throw Error('Address service unavailable.');
   const rows=await r.json();
   return response(rows.filter(r=>Number.isFinite(Number(r.lat))&&Number.isFinite(Number(r.long))).map(r=>{
    const street=[r.strname,r.strsuffix].filter(Boolean).join(' '), address=`${r.civicnum} ${street}`;
    return {label:`${address}, ${r.comm||''}`,address,community:r.comm||'',lat:Number(r.lat),lng:Number(r.long)};
   }));
  }
  if(u.pathname==='/api/services') {
   const filters={pharmacy:['["amenity"="pharmacy"]','["shop"="chemist"]','["pharmacy"="yes"]'],mental_health:['["healthcare"="psychotherapist"]','["healthcare"="counselling"]','["amenity"="clinic"]["healthcare"="mental_health"]'],physiotherapy:['["healthcare"="physiotherapist"]'],walkin:['["amenity"="clinic"]','["amenity"="doctors"]'],laboratory:['["healthcare"="laboratory"]','["amenity"="laboratory"]']}[p.get('category')];
   if(!filters) return response({detail:'Unknown category'},400);
   const lat=Number(p.get('lat')),lng=Number(p.get('lng'));
   const query='[out:json][timeout:25];('+filters.map(f=>`node${f}(around:10000,${lat},${lng});way${f}(around:10000,${lat},${lng});`).join('')+');out center tags;';
   let elements;
   for(const host of ['overpass-api.de','overpass.private.coffee','overpass.kumi.systems']) {
    try {
     const r=await fetch(`https://${host}/api/interpreter`,{method:'POST',body:new URLSearchParams({data:query}),signal:AbortSignal.timeout(40000)});
     if(!r.ok)continue;
     const data=await r.json(); if(data.remark&&!data.elements?.length)continue;
     elements=data.elements||[]; break;
    } catch {}
   }
   if(!elements)throw Error('Service lookup is temporarily unavailable.');
   const seen=new Set(), results=[];
   for(const el of elements) {
    const t=el.tags||{}, a=el.lat??el.center?.lat,b=el.lon??el.center?.lon;
    if(!t.name||a==null||b==null||seen.has(t.name.toLowerCase()))continue;
    seen.add(t.name.toLowerCase());
    const rad=Math.PI/180,h=Math.sin((a-lat)*rad/2)**2+Math.cos(lat*rad)*Math.cos(a*rad)*Math.sin((b-lng)*rad/2)**2;
    const address=[ [t['addr:housenumber'],t['addr:street']].filter(Boolean).join(' '),t['addr:city']].filter(Boolean).join(', ');
    const website=t.website||t['contact:website'];
    results.push({name:escapeHTML(t.name),address:escapeHTML(address),distance_km:Math.round(2*6371*Math.asin(Math.min(1,Math.sqrt(h)))*10)/10,phone:escapeHTML(t.phone||t['contact:phone']),hours:escapeHTML(t.opening_hours),website:website&&/^https?:\/\//i.test(website)?escapeHTML(website):null,maps_url:`https://www.google.com/maps/dir/?api=1&destination=${a},${b}`});
   }
   return response(results.sort((a,b)=>a.distance_km-b.distance_km).slice(0,5));
  }
  return response({detail:'Unknown endpoint'},404);
 } catch(e) {return response({detail:e.message||'Public data service unavailable. Check your internet connection.'},502);}
}
