# AD4M developer docs (`docs-src`)

Source of https://docs.ad4m.dev. Nextra 2 on Next 13, static export.

## Layout

| Path | What |
|---|---|
| `pages/index.mdx` | Landing page: what AD4M is, three entrances |
| `pages/start/` | Quickstarts: build an app, connect an agent, run an executor |
| `pages/tutorial/` | The step-by-step tutorial |
| `pages/concepts/` | One idea per page, in reading order |
| `pages/how-to/` | Task pages, grouped by job |
| `pages/reference/` | Tables and exact names; `mcp-topics/` and the three spec files are generated at build time |
| `pages/jsdoc/` | typedoc output (generated) |
| `PORTABILITY.md` | The content contract every page follows, enforced by `scripts/check-portability.mjs` |
| `redirects.json` | Old route → new file; `scripts/write-redirects.mjs` writes redirect pages into `out/` |
| `scripts/` | Build helpers, no dependencies |
| `language-interface-spec.md`, `host-contract.md`, `ad4m-social-conventions.md`, `ad4m-lang.wit` | The Language interface spec; linked from code, rendered into Reference |

A page is one kind of thing: a tutorial step, a concept, a how-to or a
reference page. Keep it in the matching directory. Every claim describes
behaviour on `dev`; when a page and the code disagree, fix the page in the
PR that changes the code.

## Commands

```bash
pnpm install --filter ./docs-src --filter ./core   # from the repo root
cd docs-src
pnpm run dev                 # local preview at http://localhost:3000
pnpm run check:portability   # frontmatter, imports, relative links, images
pnpm run build               # checks + typedoc + next build + redirect pages → out/
pnpm run check:links         # every internal link and anchor in out/
pnpm run publish:pages       # build + link check, then replace ../docs (what GitHub Pages serves)
```

CI (`.github/workflows/docs.yml`) runs the portability check, the build and
the link check on every PR touching `docs-src/`, the MCP topics or `core/src`.
Deployment is still manual (`publish:pages` and commit `docs/`) until the
Pages source is switched to Actions.

## Writing rules in one paragraph

Frontmatter `title` and `description`; one `#` heading equal to the title;
links to other pages are file-relative with the extension
(`../concepts/queries.mdx#where-operators`); images sit next to the page;
the only imports are `Callout`, `Tabs`, `Tab` from `nextra/components`.
`PORTABILITY.md` has the full contract and the reason: the same files are
meant to be imported unchanged into the Coasys docs site later.
