const path = require("path");

const PAGES_DIR = path.join(__dirname, "pages");

/**
 * Content pages link to each other with file-relative paths that carry the
 * extension (`../concepts/queries.mdx#where-operators`), see PORTABILITY.md.
 * This remark plugin maps such a link to the route Next.js serves it at.
 * It is the only place where this shell's file→URL mapping lives.
 */
function remarkRelativeMdxLinks() {
  return (tree, file) => {
    if (!file.path) return;
    const fromDir = path.dirname(file.path);
    const visit = (node) => {
      if (!node || typeof node !== "object") return;
      if (
        (node.type === "link" || node.type === "definition") &&
        typeof node.url === "string"
      ) {
        node.url = rewrite(node.url, fromDir);
      }
      if (Array.isArray(node.children)) node.children.forEach(visit);
    };
    visit(tree);
  };
}

function rewrite(url, fromDir) {
  const m = /^(\.{1,2}\/[^#?]*?\.(?:mdx|md))(#.*)?$/.exec(url);
  if (!m) return url;
  const abs = path.resolve(fromDir, m[1]);
  const rel = path.relative(PAGES_DIR, abs);
  if (rel.startsWith("..")) return url;
  let route = "/" + rel.replace(/\\/g, "/").replace(/\.(mdx|md)$/, "");
  route = route.replace(/\/index$/, "") || "/";
  return route + (m[2] || "");
}

const withNextra = require("nextra")({
  theme: "nextra-theme-docs",
  themeConfig: "./theme.config.tsx",
  mdxOptions: {
    remarkPlugins: [remarkRelativeMdxLinks],
  },
});

module.exports = withNextra({
  images: { unoptimized: true },
  output: "export",
});
