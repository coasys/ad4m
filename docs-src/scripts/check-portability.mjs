// Enforces docs-src/PORTABILITY.md on every content page under pages/.
// Exit 1 with a list of problems; exit 0 silently otherwise.
import fs from "node:fs";
import path from "node:path";
import { PAGES_DIR, contentPages, readFrontmatter, stripCode, fail } from "./lib.mjs";

const ALLOWED_SOURCE = "nextra/components";
const ALLOWED_NAMES = new Set(["Callout", "Tabs", "Tab"]);
const ALLOWED_FRONTMATTER = new Set(["title", "description", "sidebar_label", "draft"]);
const ALLOWED_HTML = new Set(["details", "summary", "br", "img", "sup", "sub", "kbd"]);
const EXTERNAL = /^(https?:|mailto:|tel:)/i;

const problems = [];
const pages = contentPages();

for (const file of pages) {
  const rel = path.relative(PAGES_DIR, file).replace(/\\/g, "/");
  const src = fs.readFileSync(file, "utf8");
  const { data, body } = readFrontmatter(src);

  if (!data) {
    problems.push(`${rel}: missing frontmatter (title, description)`);
  } else {
    for (const k of ["title", "description"]) {
      if (!data[k]) problems.push(`${rel}: frontmatter.${k} missing`);
    }
    for (const k of Object.keys(data)) {
      if (!ALLOWED_FRONTMATTER.has(k)) problems.push(`${rel}: frontmatter key "${k}" is not portable`);
    }
  }

  const text = stripCode(body);
  const lines = text.split("\n");
  lines.forEach((line, i) => {
    const n = i + 1;
    const imp = /^\s*import\s+(.*?)\s+from\s+["']([^"']+)["']/.exec(line);
    if (imp) {
      const [, what, from] = imp;
      const names = what.replace(/[{}]/g, "").split(",").map((s) => s.trim()).filter(Boolean);
      if (from !== ALLOWED_SOURCE || !what.trim().startsWith("{") || names.some((n) => !ALLOWED_NAMES.has(n))) {
        problems.push(`${rel}:${n}: import not in the portable component set: ${line.trim()}`);
      }
    } else if (/^\s*import\s/.test(line)) {
      problems.push(`${rel}:${n}: import not in the portable component set: ${line.trim()}`);
    }
    if (/^\s*export\s/.test(line)) problems.push(`${rel}:${n}: export statements are not portable`);

    for (const tag of line.matchAll(/<([A-Za-z][\w-]*)\b/g)) {
      const name = tag[1];
      if (/^[A-Z]/.test(name)) {
        if (!ALLOWED_NAMES.has(name)) problems.push(`${rel}:${n}: component <${name}> is not in the portable set`);
      } else if (!ALLOWED_HTML.has(name.toLowerCase())) {
        problems.push(`${rel}:${n}: raw HTML <${name}> is not allowed`);
      }
    }

    // Markdown links and images: [text](url) / ![alt](url)
    for (const m of line.matchAll(/(!?)\[[^\]]*\]\(([^)\s]+)(?:\s+"[^"]*")?\)/g)) {
      const isImage = m[1] === "!";
      const url = m[2];
      if (EXTERNAL.test(url) || url.startsWith("#")) continue;
      if (url.startsWith("/jsdoc/")) continue; // generated per shell
      const [target, anchor] = url.split("#");
      if (isImage) {
        if (!target.startsWith("./") && !target.startsWith("../")) {
          problems.push(`${rel}:${n}: image must be relative to the page: ${url}`);
          continue;
        }
      } else {
        if (target.startsWith("/")) {
          problems.push(`${rel}:${n}: site-absolute link, use a file-relative path with extension: ${url}`);
          continue;
        }
        if (!/\.(mdx|md)$/.test(target)) {
          problems.push(`${rel}:${n}: page link must carry the .mdx/.md extension: ${url}`);
          continue;
        }
      }
      const abs = path.resolve(path.dirname(file), target);
      if (!fs.existsSync(abs)) problems.push(`${rel}:${n}: link target does not exist: ${url}`);
      else if (anchor && !isImage && /\.(mdx|md)$/.test(abs)) {
        const t = fs.readFileSync(abs, "utf8");
        const headings = [...stripCode(readFrontmatter(t).body).matchAll(/^#{1,6}\s+(.+?)\s*$/gm)].map((h) => slugOf(h[1]));
        const custom = [...t.matchAll(/\[#([\w-]+)\]/g)].map((c) => c[1]);
        if (!headings.includes(anchor) && !custom.includes(anchor)) {
          problems.push(`${rel}:${n}: anchor #${anchor} not found in ${target}`);
        }
      }
    }
    for (const m of line.matchAll(/<img\s[^>]*src=["']([^"']+)["']/g)) {
      const url = m[1];
      if (EXTERNAL.test(url)) continue;
      if (!url.startsWith("./") && !url.startsWith("../")) problems.push(`${rel}:${n}: <img src> must be relative to the page: ${url}`);
      else if (!fs.existsSync(path.resolve(path.dirname(file), url))) problems.push(`${rel}:${n}: image does not exist: ${url}`);
    }
  });
}

function slugOf(text) {
  return text
    .toLowerCase()
    .replace(/\s*\[#[\w-]+\]\s*$/, "")
    .replace(/<[^>]+>/g, "")
    .replace(/[`*_~]/g, "")
    .replace(/[^\p{L}\p{N}\s-]/gu, "")
    .trim()
    .replace(/\s+/g, "-");
}

console.log(`checked ${pages.length} content pages`);
fail(problems, "portability check");
