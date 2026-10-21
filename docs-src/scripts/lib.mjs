// Shared helpers for the docs-src scripts. No dependencies.
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const DOCS_SRC = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
export const REPO_ROOT = path.resolve(DOCS_SRC, "..");
export const PAGES_DIR = path.join(DOCS_SRC, "pages");
export const OUT_DIR = path.join(DOCS_SRC, "out");

/** Directories under pages/ that are generated or shell-owned, not portable content. */
export const GENERATED_PREFIXES = ["jsdoc/", "reference/mcp-topics/"];
export const GENERATED_FILES = new Set([
  "reference/language-interface-spec.md",
  "reference/host-contract.md",
  "reference/social-conventions.md",
]);

export function walk(dir, pred = () => true, acc = []) {
  if (!fs.existsSync(dir)) return acc;
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) walk(p, pred, acc);
    else if (pred(p)) acc.push(p);
  }
  return acc;
}

export function contentPages() {
  return walk(PAGES_DIR, (p) => /\.(mdx|md)$/.test(p)).filter((p) => {
    const rel = path.relative(PAGES_DIR, p).replace(/\\/g, "/");
    if (GENERATED_PREFIXES.some((pre) => rel.startsWith(pre))) return false;
    if (GENERATED_FILES.has(rel)) return false;
    return true;
  });
}

/**
 * Minimal YAML frontmatter reader: `key: value` lines only.
 * `errors` lists lines the real YAML parser (gray-matter in the Nextra build,
 * and every other site generator) would reject or read differently: a plain
 * scalar that contains `: ` or ` #`, starts with a YAML indicator, or a quoted
 * scalar that does not close. Every page must be plain enough that both
 * readers agree, so these are reported as portability problems.
 */
export function readFrontmatter(src) {
  const m = /^---\r?\n([\s\S]*?)\r?\n---\r?\n/.exec(src);
  if (!m) return { data: null, body: src, errors: [] };
  const data = {};
  const errors = [];
  m[1].split(/\r?\n/).forEach((line, i) => {
    if (!line.trim()) return;
    const kv = /^([A-Za-z_][\w-]*):\s*(.*)$/.exec(line);
    if (!kv) {
      errors.push(`frontmatter line ${i + 1} is not a \`key: value\` line: ${line.trim()}`);
      return;
    }
    const [, key, raw] = kv;
    const v = raw.trim();
    if (/^"/.test(v)) {
      if (!/^"(?:[^"\\]|\\.)*"$/.test(v)) errors.push(`frontmatter.${key}: double-quoted value does not close cleanly`);
      data[key] = v.slice(1, -1).replace(/\\(.)/g, "$1");
    } else if (/^'/.test(v)) {
      if (!/^'(?:[^']|'')*'$/.test(v)) errors.push(`frontmatter.${key}: single-quoted value does not close cleanly`);
      data[key] = v.slice(1, -1).replace(/''/g, "'");
    } else {
      if (/^[-?:,\[\]{}#&*!|>%@`]/.test(v) || /:\s/.test(v) || /\s#/.test(v) || /:$/.test(v)) {
        errors.push(`frontmatter.${key}: value needs double quotes to be valid YAML (contains ": ", " #" or starts with a YAML indicator)`);
      }
      data[key] = v;
    }
  });
  return { data, body: src.slice(m[0].length), errors };
}

/** Remove fenced code blocks and inline code so samples are not scanned. */
export function stripCode(body) {
  return body
    .replace(/```[\s\S]*?```/g, (block) => block.replace(/[^\n]/g, " "))
    .replace(/~~~[\s\S]*?~~~/g, (block) => block.replace(/[^\n]/g, " "))
    .replace(/`[^`\n]*`/g, (s) => " ".repeat(s.length));
}

/** Route (as Next serves it) for a page file. */
export function routeFor(pageFile) {
  const rel = path.relative(PAGES_DIR, pageFile).replace(/\\/g, "/");
  let route = "/" + rel.replace(/\.(mdx|md)$/, "");
  return route.replace(/\/index$/, "") || "/";
}

/** Page file for a route, if one exists. */
export function fileForRoute(route) {
  const base = path.join(PAGES_DIR, route === "/" ? "index" : route.replace(/^\//, ""));
  for (const cand of [base + ".mdx", base + ".md", path.join(base, "index.mdx"), path.join(base, "index.md")]) {
    if (fs.existsSync(cand)) return cand;
  }
  return null;
}

/** GitHub-style slug for a heading, as Nextra/rehype-slug produce it. */
export function slug(text) {
  return text
    .toLowerCase()
    .replace(/<[^>]+>/g, "")
    .replace(/[`*_~]/g, "")
    .replace(/[^\p{L}\p{N}\s-]/gu, "")
    .trim()
    .replace(/\s+/g, "-");
}

export function fail(msgs, what) {
  if (msgs.length) {
    console.error(`\n${what}: ${msgs.length} problem(s)\n`);
    for (const m of msgs) console.error("  - " + m);
    console.error();
    process.exit(1);
  }
  console.log(`${what}: ok`);
}
