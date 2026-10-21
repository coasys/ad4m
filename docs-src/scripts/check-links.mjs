// Checks every internal link in the built site (out/): the target page exists
// and, when the link has a fragment, the target has an element with that id.
// Redirect pages written by write-redirects.mjs count as existing targets.
import fs from "node:fs";
import path from "node:path";
import { OUT_DIR, walk, fail } from "./lib.mjs";

const htmlFiles = walk(OUT_DIR, (p) => p.endsWith(".html"));
const problems = [];
const idCache = new Map();

function fileFor(route) {
  const clean = route.replace(/\/$/, "");
  const cands = [
    path.join(OUT_DIR, clean + ".html"),
    path.join(OUT_DIR, clean, "index.html"),
    path.join(OUT_DIR, clean),
    path.join(OUT_DIR, (clean || "index") + ".html"),
  ];
  if (route === "/" || route === "") cands.unshift(path.join(OUT_DIR, "index.html"));
  return cands.find((c) => fs.existsSync(c) && fs.statSync(c).isFile()) || null;
}

function idsOf(file) {
  if (!idCache.has(file)) {
    const html = fs.readFileSync(file, "utf8");
    const ids = new Set();
    for (const m of html.matchAll(/\sid="([^"]+)"/g)) ids.add(decodeURIComponent(m[1]));
    for (const m of html.matchAll(/<a\s[^>]*name="([^"]+)"/g)) ids.add(m[1]);
    idCache.set(file, ids);
  }
  return idCache.get(file);
}

let checked = 0;
let skippedGenerated = 0;
for (const file of htmlFiles) {
  const rel = path.relative(OUT_DIR, file).replace(/\\/g, "/");
  const isRedirect = rel !== "404.html" && /meta http-equiv="refresh"/.test(fs.readFileSync(file, "utf8").slice(0, 400));
  if (isRedirect) continue;
  // Links typedoc writes inside the generated jsdoc/ tree are not ours to fix: typedoc emits
  // dotted member anchors (#agent.addTrustedAgents) that Nextra's slugger renders as
  // id="agentaddtrustedagents", and it links to source files that are not in the site.
  // Links from content pages INTO jsdoc/ are still checked, because we wrote those.
  if (rel.startsWith("jsdoc/")) {
    skippedGenerated++;
    continue;
  }
  const html = fs.readFileSync(file, "utf8");
  // Only the page body: Next's data scripts repeat hrefs; the anchors in the rendered body are what readers click.
  const body = html.split(/<script id="__NEXT_DATA__"/)[0];
  for (const m of body.matchAll(/<a\s[^>]*href="([^"]+)"/g)) {
    const href = m[1].replace(/&amp;/g, "&");
    if (/^(https?:|mailto:|tel:|javascript:)/.test(href)) continue;
    if (href.startsWith("#")) {
      if (!idsOf(file).has(decodeURIComponent(href.slice(1)))) problems.push(`${rel}: missing anchor ${href}`);
      checked++;
      continue;
    }
    let [route, hash] = href.split("#");
    if (!route.startsWith("/")) route = "/" + path.posix.normalize(path.posix.join(path.posix.dirname("/" + rel), route)).replace(/^\//, "");
    route = route.split("?")[0];
    if (/\.(png|jpe?g|gif|svg|ico|webp|pdf|txt|json|xml|wit)$/i.test(route)) {
      if (!fs.existsSync(path.join(OUT_DIR, route))) problems.push(`${rel}: missing file ${href}`);
      checked++;
      continue;
    }
    const target = fileFor(route);
    checked++;
    if (!target) {
      problems.push(`${rel}: broken link ${href}`);
      continue;
    }
    if (hash && !/meta http-equiv="refresh"/.test(fs.readFileSync(target, "utf8").slice(0, 400))) {
      if (!idsOf(target).has(decodeURIComponent(hash))) problems.push(`${rel}: missing anchor ${href}`);
    }
  }
}

console.log(`checked ${checked} links in ${htmlFiles.length - skippedGenerated} pages (${skippedGenerated} generated jsdoc pages skipped)`);
fail(problems, "link check");
