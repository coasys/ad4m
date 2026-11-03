// Copies documents that are owned elsewhere in the repo into pages/reference/
// so the site renders them from their authoritative paths (PORTABILITY.md §8).
// Output is gitignored; run before `next build`.
import fs from "node:fs";
import path from "node:path";
import { DOCS_SRC, REPO_ROOT, PAGES_DIR } from "./lib.mjs";

const GITHUB_BLOB = "https://github.com/coasys/ad4m/blob/dev/";

function withFrontmatter(title, description, body) {
  const esc = (s) => s.replace(/"/g, '\\"');
  return `---\ntitle: "${esc(title)}"\ndescription: "${esc(description)}"\n---\n\n${body}`;
}

function firstH1(body, fallback) {
  const m = /^#\s+(.+?)\s*$/m.exec(body);
  return m ? m[1].replace(/[`*_]/g, "") : fallback;
}

// 1. MCP documentation topics (served to agents by the executor's get_documentation tool).
const topicsSrc = path.join(REPO_ROOT, "rust-executor/src/mcp/docs");
const topicsDst = path.join(PAGES_DIR, "reference/mcp-topics");
fs.rmSync(topicsDst, { recursive: true, force: true });
fs.mkdirSync(topicsDst, { recursive: true });
const order = ["overview", "usage", "models", "architecture", "flux"];
const meta = {};
const topics = fs.readdirSync(topicsSrc).filter((f) => f.endsWith(".md"));
for (const name of [...order.filter((o) => topics.includes(o + ".md")), ...topics.map((t) => t.replace(/\.md$/, "")).filter((t) => !order.includes(t))]) {
  const body = fs.readFileSync(path.join(topicsSrc, name + ".md"), "utf8");
  const title = firstH1(body, name);
  const note = `> Rendered from \`rust-executor/src/mcp/docs/${name}.md\`. The executor serves this text to agents through the \`get_documentation\` tool (topic \`${name}\`); edit it there.\n\n`;
  fs.writeFileSync(
    path.join(topicsDst, name + ".md"),
    withFrontmatter(title, `MCP documentation topic "${name}", as served by the executor's get_documentation tool.`, note + body)
  );
  meta[name] = title;
}
fs.writeFileSync(path.join(topicsDst, "_meta.json"), JSON.stringify(meta, null, 2) + "\n");

// 2. Language interface spec, host contract and social conventions (docs-src/*.md, consumed by code links).
const specs = [
  ["language-interface-spec.md", "Language interface specification", "The ALDK Language interface: lifecycle, capabilities, operations and semantics, explained alongside the normative WIT."],
  ["host-contract.md", "Host contract", "What the executor guarantees to a Language and what it requires from it."],
  ["ad4m-social-conventions.md", "Social conventions", "Conventions Languages and apps follow for DMs, friends and other shared patterns.", "social-conventions.md"],
];
for (const [src, title, description, dstName] of specs) {
  const from = path.join(DOCS_SRC, src);
  if (!fs.existsSync(from)) continue;
  let body = fs.readFileSync(from, "utf8");
  // Links between the three files and to the WIT: keep them working from the new location.
  body = body
    .replace(/\]\(\.?\/?ad4m-lang\.wit\)/g, `](${GITHUB_BLOB}docs-src/ad4m-lang.wit)`)
    .replace(/\]\(\.?\/?ad4m-social-conventions\.md(#[^)]*)?\)/g, "](./social-conventions.md$1)")
    .replace(/\]\(\.?\/?host-contract\.md(#[^)]*)?\)/g, "](./host-contract.md$1)")
    .replace(/\]\(\.?\/?language-interface-spec\.md(#[^)]*)?\)/g, "](./language-interface-spec.md$1)")
    .replace(/\]\(\.?\/?language-interface-migration-plan\.md(#[^)]*)?\)/g, `](${GITHUB_BLOB}docs-src/language-interface-migration-plan.md)`);
  const note = `> Rendered from \`docs-src/${src}\`, which code and other repos link to; edit it there.\n\n`;
  fs.writeFileSync(path.join(PAGES_DIR, "reference", dstName || src), withFrontmatter(title, description, note + body));
}

console.log(`copied ${Object.keys(meta).length} MCP topics and ${specs.length} spec files into pages/reference/`);
