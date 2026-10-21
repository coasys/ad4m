// Writes a redirect page into out/ for every old route in redirects.json.
// A static export cannot send HTTP redirects, so each page is a meta-refresh
// with a canonical link; a small script preserves and remaps the URL hash.
import fs from "node:fs";
import path from "node:path";
import { DOCS_SRC, PAGES_DIR, OUT_DIR, fileForRoute, routeFor, fail } from "./lib.mjs";

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
  return routeFor(abs) + (anchor ? "#" + anchor : "");
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
    const r = routeOfTarget(target.includes("#") ? target : `${target}#${oldAnchor}`, `${oldRoute}#${oldAnchor}`);
    if (r) anchors[oldAnchor] = r;
  }
  const html = `<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>Moved: ${oldRoute}</title>
<link rel="canonical" href="${to}">
<meta name="robots" content="noindex">
<meta http-equiv="refresh" content="0; url=${to}">
<script>
(function(){var m=${JSON.stringify(anchors)};var h=location.hash.slice(1);var t=${JSON.stringify(to)};
if(h){t=m[h]||(t.split('#')[0]+'#'+h);}location.replace(t);})();
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
