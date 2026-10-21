// Writes a redirect page into out/ for every old route in redirects.json.
// A static export cannot send HTTP redirects, so each page is a meta-refresh
// with a canonical link; a small script preserves and remaps the URL hash.
import fs from "node:fs";
import path from "node:path";
import { DOCS_SRC, PAGES_DIR, OUT_DIR, fileForRoute, routeFor, slug, fail } from "./lib.mjs";

const registry = JSON.parse(fs.readFileSync(path.join(DOCS_SRC, "redirects.json"), "utf8"));
const problems = [];
let written = 0;

function routeOfTarget(target, where) {
  const [file, anchor] = target.split("#");
  const abs = path.join(PAGES_DIR, file);
  if (!fs.existsSync(abs)) {
    problems.push(`${where}: target file does not exist: ${file}`);
    return null;
  }
  if (anchor && !headingSlugs(abs).has(anchor)) {
    problems.push(`${where}: anchor #${anchor} not found in ${file}`);
    return null;
  }
  return routeFor(abs) + (anchor ? "#" + anchor : "");
}

const slugCache = new Map();
function headingSlugs(file) {
  if (!slugCache.has(file)) {
    const text = fs.readFileSync(file, "utf8").replace(/```[\s\S]*?```/g, "");
    const set = new Set();
    for (const h of text.matchAll(/^#{1,6}\s+(.+?)\s*$/gm)) {
      const custom = /\[#([\w-]+)\]\s*$/.exec(h[1]);
      set.add(custom ? custom[1] : slug(h[1]));
    }
    slugCache.set(file, set);
  }
  return slugCache.get(file);
}

for (const [oldRoute, spec] of Object.entries(registry.redirects)) {
  if (fileForRoute(oldRoute)) {
    problems.push(`${oldRoute}: a page still exists at this route; remove it or the redirect`);
    continue;
  }
  const to = routeOfTarget(spec.to, oldRoute);
  if (!to) continue;
  const anchors = {};
  for (const [oldAnchor, target] of Object.entries(spec.anchors || {})) {
    // "file.mdx" sends the old anchor to the top of that file; "file.mdx#x" to a section.
    const r = routeOfTarget(target, `${oldRoute}#${oldAnchor}`);
    if (r) anchors[oldAnchor] = r;
  }
  // An unmapped old anchor is kept only if the destination has that heading;
  // otherwise the redirect lands on `to` instead of a dead hash.
  const known = [...headingSlugs(path.join(PAGES_DIR, spec.to.split("#")[0]))];
  const html = `<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>Moved: ${oldRoute}</title>
<link rel="canonical" href="${to}">
<meta name="robots" content="noindex">
<meta http-equiv="refresh" content="0; url=${to}">
<script>
(function(){var m=${JSON.stringify(anchors)};var k=${JSON.stringify(known)};var h=location.hash.slice(1);var t=${JSON.stringify(to)};
if(h){t=m[h]||(k.indexOf(h)>=0?t.split('#')[0]+'#'+h:t);}location.replace(t);})();
</script>
</head><body><p>This page moved to <a href="${to}">${to}</a>.</p></body></html>
`;
  const outFile = path.join(OUT_DIR, oldRoute.replace(/^\//, "") + ".html");
  fs.mkdirSync(path.dirname(outFile), { recursive: true });
  if (fs.existsSync(outFile)) {
    problems.push(`${oldRoute}: build already produced ${path.relative(DOCS_SRC, outFile)}`);
    continue;
  }
  fs.writeFileSync(outFile, html);
  written++;
}

console.log(`wrote ${written} redirect pages`);
fail(problems, "redirects");
