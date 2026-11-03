# AD4M developer docs: overhaul proposal

Date: 2026-10-21. Author: Data (Fable 5.1). Blind critique by Lal (Astra), folded in and marked **[Lal]**.
Status: direction approved by Nico on 2026-10-21. Phase 1 is tracked in issue #1503 and this PR.
Checked against `coasys/ad4m` 13e1c50b8 (the `dev` head at 19:30) and the live site docs.ad4m.dev. "Present on dev" below means present at that commit; it is not a claim about a released package.

## Summary (one screen)

**What is wrong today**

1. The docs are organised by AD4M's internals (agents, languages, expressions, perspectives, neighbourhoods, social DNA, flows), not by the reader's job. A developer who wants to store and query data reads six concept pages before reaching models and queries. An agent builder has no entrance at all: MCP is the 19th page.
2. One page, `developer-guides/model-classes`, is 1659 lines and holds most of what developers need: decorators, relations, queries, abstract classes, subscriptions, transactions, JSON Schema. Abstract classes are section 32 of 48 on that page and absent from the navigation. Built-in abstract classes (TextContent, Container) and containment are mentioned once, in the MCP page.
3. Nothing explains the type system: properties vs relations, literals vs signed expressions, cardinality, abstract classes and `implements`, aliases, built-ins, how a record is recognised as an instance.
4. The pipeline is broken. The built site (51 MB under `docs/`) is committed by hand; no CI builds it; the "API Reference" link on the live site is a 404; the last rebuild was 2026-10-12. Six spec files under `docs-src/` are not on the site.
5. About 110 claims are wrong or missing against `dev` (appendix A, audit file). The SPARQL examples use a pattern the store does not write; the connect import, the CLI commands and the neighbourhood template parameter are wrong. The tutorial is four pages about raw links, two under 30 lines.

**The proposal**

- Three job entrances on the landing page: **Build an app · Connect an agent · Run an executor** **[Lal]**, with Diátaxis underneath (Tutorial, Concepts, How-to, Reference), the split Django, Cloudflare and Stripe use. Page tree in section 3.
- Concept order: agent and executor → perspectives and links → models and identity → types and values → queries → abstract classes and aliases → containment → neighbourhoods → social DNA and flows → agents (MCP vs in-executor) → hosted deployment → language protocols last.
- One domain for Coasys developer docs: **docs.coasys.org**, sections `/ad4m` and `/we`, later `/app`. Content stays in each product repo; a small `coasys/docs` repo holds the shell and builds pinned revisions of both. Astro Starlight is the candidate, to be confirmed by a spike. Redirects from every docs.ad4m.dev URL and anchor. App user docs are a separate site later.
- Three views, one named authority per fact **[Lal]**: the website for app developers and operators; the executor's `get_documentation` topics for agents at runtime, compiled into the binary and rendered on the website from the same files; Josh's mental-models tree (#1493) for contributors, rendered as Internals. One shared term file feeds all three.
- Phase 1 tonight inside the current Nextra site: the new tree, the split of the big pages, the new type and query pages, a repaired quickstart for app and agent, old URLs and anchors preserved, wrong pages fixed, MCP topics rendered, a build and link check in CI. Phase 2: the Coasys site and the move. Phase 3: generated reference.

**Decisions Nico needs to make** (section 8): domain, tooling, where content lives, what happens to #1493, whether phase 1 runs tonight.

---

## 1. The docs today

Source: `docs-src/pages` (Nextra 2 on Next 13), 25 pages, 8526 lines, plus six markdown files outside `pages/` that the site cannot serve. Built output `docs/` is committed (217 files, 51 MB). No workflow in `.github/workflows` or `.circleci` builds or deploys it. The site does build on Node 24 (install, typedoc and `next build` all exit 0 in a fresh worktree tonight).

| Page | Lines | Last change | Verdict |
|---|---|---|---|
| index | 351 | 05-20 Josh | Rewrite to one screen with the three entrances. Content moves to Concepts. |
| installation | 99 | 09-30 Marvin | Keep, move to Start; fix the connect example. |
| spanning-layer | 176 | 09-07 Data | Merge into "What AD4M is". |
| auth | 203 | 10-08 Marvin | Split: concept into "Agent, executor and capabilities", capability tables into Reference. |
| agents | 131 | 10-01 Josh | Merge into "Agent, executor and capabilities". |
| languages | 443 | 09-29 Data | Keep as the last concept. Authoring parts become How-to. |
| expressions | 191 | 09-24 Marvin | Expression basics (envelope, provenance, resolution) move into "Types and values"; the rest merges into languages. **[Lal]** |
| perspectives | 280 | 10-13 Josh | Keep as concept; SPARQL and Prolog sections move to Queries. |
| neighbourhoods | 310 | 10-01 Josh | Keep as concept; setup code becomes How-to; remove the dead interface. |
| social-dna | 598 | 09-24 Data | Shorten to the idea and the enforcement boundary. SHACL tables to Reference. |
| flows | 594 | 10-13 Data | Keep. Fix the file references. |
| multi-user | 438 | 05-13 Josh | Split: concept (hosted deployment, data isolation, sign-up) and How-to (DNS, TLS, firewall, wHOT). |
| developer-guides/model-classes | 1659 | 10-15 Data | Split into Models and identity, Types and values, Queries, Abstract classes and aliases, Containment, four How-tos, Reference/model-query, Reference/decorators, Reference/built-in interfaces. |
| developer-guides/writing-a-flow | 553 | 10-11 Marvin | Keep as How-to. |
| developer-guides/sparql-queries | 395 | 05-20 Josh | How-to + reference; every example rewritten. |
| developer-guides/hooks | 310 | 10-01 Josh | How-to; remove `useClient`. |
| developer-guides/batch-operations | 227 | 05-13 Josh | How-to, merge with the transactions section. |
| developer-guides/ai | 421 | 10-01 James | Split: concept paragraph into "Agents", rest How-to; fix model ids and VAD claims. |
| developer-guides/mcp | 377 | 10-15 Data | Shrink to enable + auth + connect, keeping the executor-vs-plugin ownership of each tool; tool list rendered from the executor's topics. |
| developer-guides/cli | 303 | 10-08 Marvin | Reference, checked command by command. |
| developer-guides/executor-config | 212 | 10-16 Josh | Reference. |
| tutorial/1–4 | 118, 87, 23, 27 | 02-12 to 10-01 | Replace with the task-board tutorial; old pages redirect. |
| developer-guides/react_backup | 8 KB file | | Delete. |
| docs-src/*.md (language-interface-spec 926, host-contract 324, headless-executor 448, running-an-executor 227, migration-plan 690, social-conventions 513, design/*) | 3128 | | Not on the site. The spec and host contract are linked from `languages.mdx` and from `core/src/language/Language.ts` as GitHub URLs, so they stay where they are and are rendered into Reference **[Lal]**. headless/running become How-to; migration plan and design draft are planning docs. |

What has no page at all (present on dev): the type system as a topic; built-ins `TextContent` and `Container`, the `parent: { id }` containment scope and bounded traversal; the executor's `get_documentation` topics; wakers beyond one table row; the RPC method and event list (generated into `core/src/generated/api/`); errors; troubleshooting; a glossary.

## 2. What well-liked doc sites do, and what we take

| Site | What people like | What we take |
|---|---|---|
| Django, Cloudflare, Gatsby (Diátaxis) | Four kinds of page, never mixed: tutorial, how-to, explanation, reference. You always know what kind of page you are on. | The four sections, under job entrances so readers do not have to learn the taxonomy first **[Lal]**. The rule that a page is one kind. `model-classes` is all four at once; that is why it is 1659 lines. |
| Stripe | One short "how it works" page per concept with a diagram, then code. The API reference is generated from the API definition, every object described once. | Reference generated from source: RPC methods and events from `RpcMethods.ts`/`Events.ts`, MCP tools from the tool definitions, SDK from typedoc. Hand-written reference drifts; ours already has. |
| Rust Book, Svelte tutorial | One narrative path that builds one thing, one new idea per chapter, every chapter ends with running code. | One tutorial that builds a shared task board, from "connect" to "let an agent in". |
| Tailwind, Astro Starlight | Landing page is one screen with a few doors. Search first. "On this page" table of contents. Short pages. | Three doors. Search that works offline (Pagefind). A target of one idea per page; no fixed line budget, since `flows` is 594 good lines **[Lal]**. |
| Anthropic, Stripe (`llms.txt`) | The docs are plain markdown that agents can read whole. | Markdown as the source. The same files feed the executor's documentation tool and an `llms.txt`. |

What we do not take: Stripe's two-column code layout (our SDK is one language), full versioned copies from day one (section 6.4).

## 3. Proposed page tree

Order inside a section is the recommended reading order, not a prerequisite chain: the agent entrance does not depend on the app tutorial **[Lal]**.

```
/                       What AD4M is in five sentences, one diagram, three doors:
                        Build an app · Connect an agent · Run an executor; Reference always in the nav
Start
  Build an app          install; connect locally or to a hosted executor (the two flows differ);
                        first model, first query                                          (from installation)
  Connect an agent      enable MCP; discover a perspective, read its schema, read text under a
                        container through TextContent, create a record, see it in the app;
                        then wakers, interpretation, flows. Each step says who owns it:
                        executor MCP, SDK, or OpenClaw plugin                              (new) [Lal]
  Run an executor       local / headless / hosted; credential; where data lives               (from cli, headless)
Tutorial: a shared task board
  1 Connect, your agent, capabilities
  2 A perspective and the Task model
  3 A Board that holds tasks: a recognised class whose children relation maps to
    Container.children, and canonical has_child                                            [Lal]
  4 Querying: where, order, paginate, include, subscribe
  5 Share it: publish a neighbourhood, join from a second executor
  6 A flow: task status the group agrees on, a role granted by a flow
  7 An agent joins: describe_perspective, instance_create, a waker
Concepts: data and interoperability
  What AD4M is               spanning layer, agent-centric, one diagram               (from index + spanning-layer)
  Agent, executor and capabilities                                                    (from agents + auth)
  Perspectives and links     signed links, local vs shared data                       (from perspectives)
  Models and identity        subject classes, Ad4mModel, SHACL underneath, instance ids,
                             how a record is recognised as an instance (predicates, required
                             fields, flags, initial values, structural fallback)       (from social-dna + model-classes) [Lal]
  Types and values           property vs relation, cardinality, literal vs signed-expression
                             vs custom-language storage, expression URIs and provenance (NEW) [Lal]
  Queries and live results   model query vs SPARQL (vs Prolog), subscriptions, verified links
  Abstract classes and aliases  cross-app interfaces: matching by property and relation
                             names and aliases (not predicates, not datatypes), implements as
                             an SDK check, writing through; built-ins as the example   (NEW; from model-classes §Abstract classes) [Lal]
  Containment and navigation canonical has_child, a holder's own children relation, scope,
                             reverse traversal, attach/detach vs delete; a record can have
                             several holders                                           (NEW) [Lal]
Concepts: collaboration and execution
  Neighbourhoods and trust                                                            (from neighbourhoods)
  Social DNA and flows       classes recognise, flows gate; what is and is not enforced on
                             incoming links                                           (from social-dna, short) [Lal]
  Flows                      (flows.mdx)
  Agents                     external MCP vs in-executor interpretation and auto-processors (from mcp + ai)
  Hosted deployment          the model API is shared; connection, auth and lifecycle differ (from multi-user) [Lal]
  Language protocols         for protocol authors                                     (from languages + expressions)
How-to, grouped by job
  Data        define a model (decorators | SHACL JSON | JSON Schema) · write through an abstract
              class · filter by author · paginate · subscribe · include · sparse fields ·
              SPARQL traverse and aggregate · batch writes
  UI          React and Vue hooks
  Sharing     publish a neighbourhood · join by URL
  Rules       write a flow: a role granted by a flow                                  (writing-a-flow)
  Agents      run the MCP server · connect OpenClaw or Claude · configure AI models · transcription
  Operations  headless / Docker · multi-user host (DNS, TLS, firewall, wHOT) · CLI
  Languages   write a Language (TypeScript | Rust) · publish it
  Troubleshooting  cannot connect · record missing · child missing · cannot write ·
              stale subscription · schema changed under existing records · incompatible versions [Lal]
Reference
  SDK API (typedoc, fixed)
  Decorators and options
  Model query: where operators (incl. some/none, producedByFlow), order (dotted keys need limit),
    include and $ projections, properties, links, parent scopes (ids, transitive, maxDepth,
    direction, levels), several classes, includeUnverified, subscriptions
  Built-in interfaces: TextContent, Container, exact URIs and behaviour                  [Lal]
  SHACL vocabulary: ad4m:// predicates, sh: subset, JSON wire format
  RPC methods and events (generated; phase 3)
  MCP tools, and the documentation-tool topics rendered from rust-executor/src/mcp/docs
  CLI · Executor config · Environment variables
  Language interface spec · Host contract (rendered from their current paths)
  Errors
  Glossary (from the shared term file)
Internals (contributors)  → mental-models tree (#1493), AGENTS.md
```

Why this order: it follows what a developer does. Storage, then shape, then queries, then interfaces across apps, then sharing, then rules, then agents. Language authoring comes last because an app developer never writes one; the meaning of an expression and the storage choice (`resolveLanguage` unset, `"literal"`, or a language address) come early because every model author makes that choice **[Lal]**.

Why three pages instead of one "type system" page **[Lal]**: there are two different questions that today's text blurs. "Does a concrete class answer an abstract interface?" is decided by property and relation names and aliases (`model_query/abstract_classes.rs:153-214`); `implements` is an SDK-side check (`core/src/model/decorators.ts:618-647`). "Is this node an instance of a concrete class?" is decided by predicates, required fields, flags, initial values and a conditional structural fallback (`model_query/sparql_builder.rs:993-1101`). Containment is a third thing: `has_child` plus a holder's own recognised children relation (`model_query/containment.rs:85-126`), and one record can sit under four holders (`tests/js/tests/model/model-containment.test.ts:185-202`), so the docs must not sell it as a tree with a unique parent.

Everything in these pages describes behaviour present on dev at 13e1c50b8. Adoption and default-predicate mapping from the type-system research (d14) are design only and stay out; the holon pages (d19) are design and get a label, not a concept page.

## 4. The three views

| View | Reader | Lives | Authority for |
|---|---|---|---|
| Website (`docs-src`) | app developer, operator | `coasys/ad4m` | public semantics, examples, how-tos |
| `get_documentation` topics (`rust-executor/src/mcp/docs`, 5 topics, 1255 lines, compiled in with `include_str!`) | an agent at runtime with the MCP tools | `coasys/ad4m` | the runtime tool workflow and the rules for writing data |
| Mental models (#1493, `mental-models/`, 75 pages + glossary) | a contributor changing the executor | `coasys/ad4m` | why the implementation is the way it is, where in the code |

The three overlap in concepts by necessity; they differ in depth and task. Rules **[Lal, revised]**:

1. **One authority per fact.** Each concept has an owning page in one view; the others restate briefly and link. A contributor page may explain public behaviour in a sentence; a concept page may say why something exists in a sentence.
2. **One term file.** A shared `terms` file (stable term ids, one-line normative definitions, contributor shorthand marked as such) at the same commit feeds the website glossary, the runtime overview topic and the mental-models glossary. The runtime topics keep their own short definitions compiled into the binary: an offline agent must not depend on a live glossary that may be newer than its executor. A small, useful term file does not wait for all 162 terms or for #1493 to merge; the team's term process decides the words.
3. **The MCP topics are rendered, not copied.** The site build copies `rust-executor/src/mcp/docs/*.md` into Reference. The MCP how-to shrinks to enable, authenticate, connect, and keeps saying which tools are the executor's and which are the OpenClaw plugin's (wakers). The template manual decided on 10-21 is planned: a markdown file becomes a served topic only when it is wired into the `DocTopic` enum in `docs.rs`.
4. **Concept pages link down.** Each concept page ends with "How it is built →" to the owning mental-model page (James's click-to-expand).
5. **Coasys-wide top level** (James) is a chooser and one paragraph on how AD4M, We and the app fit. Shared terms are canonical under AD4M; We keeps its own explanations and entry paths.

## 5. The night PR (phase 1)

Inside the current Nextra site. It touches markdown, `_meta.json`, the docs build script and one GitHub workflow; no site generator change, so it merges regardless of the hosting decision. A demonstrable cut, not a promise **[Lal]**:

- The three entrances and the new tree from section 3.
- A repaired app quickstart (`getAd4mClient`, correct capabilities) and an agent quickstart against the real MCP tools.
- `model-classes` split into Models and identity, Types and values, Queries, Abstract classes and aliases, Containment, the How-tos and the Reference pages. A migration checklist per page: facts retained, obsolete claims removed, examples that run.
- `index`, `spanning-layer`, `agents`, `auth`, `expressions` folded as in section 1.
- Every WRONG item in appendix A fixed on a page the PR touches. SPARQL examples rewritten from `sparql_store.rs` and the tests. The CLI page checked command by command against `cli/src`.
- **Old URLs and anchors preserved** **[Lal]**: an inventory of today's 25 routes and the section anchors of the big pages, a redirect registry (one file, reused at the domain move), alias pages at every old route (a static export cannot send HTTP redirects, so these are meta-refresh pages with a canonical link), and a check that every old URL resolves.
- Build script copies the MCP topics into Reference. Spec files rendered from their current paths. `react_backup`, old tutorial, migration plan removed from `docs-src`.
- Tutorial chapters 1–4 only if they run from a fresh project against the pinned SDK tonight; otherwise the old tutorial stays until they do. Chapters 5–7 follow in a second PR.
- A GitHub workflow that builds `docs-src` and checks links on every PR. Deploying from CI needs the Pages source switched to Actions: a separate small PR after Nico flips the setting.

Not in phase 1: the Starlight move, the domain, We content, generated RPC reference, versions.

## 6. Hosting under Coasys

### 6.1 Domain

| Option | For | Against |
|---|---|---|
| **A. docs.coasys.org/ad4m, /we, /app** (recommended) | One DNS record, one deploy, one search index across products. The pattern of docs.stripe.com and docs.astro.build. | Needs the DNS record (none exists today). |
| B. coasys.org/docs/... | Looks like one site with the marketing pages. | coasys.org is a separate static site; a path needs a proxy or the docs built into that site's deploy. Couples two release cycles. |
| C. Keep docs.ad4m.dev, add a We site | No migration. | Two sites, two searches, the opposite of what Nico asked for. |

### 6.2 Where the content lives

Recommended: **content stays in each product repo** (`coasys/ad4m/docs-src`, `coasys/we/docs` plus a We-owned manifest), and a small **`coasys/docs`** repo holds the shell, theme, landing page and the build. Why: the rule Josh wrote into #1493 ("a PR that changes behaviour updates the page in the same PR") only works when the page is in the repo with the code. Separate content PRs raise the chance that behaviour and docs ship apart; they do not guarantee accuracy either way **[Lal]**.

The shell needs a contract before it owns deployment **[Lal]**: a named owner and backup, CODEOWNERS per product section; an import manifest per product (which files, which markdown and component subset, how assets and relative links are preserved); builds from **explicit pinned revisions** `{shell SHA, AD4M SHA, We SHA}` recorded on the deployed site, not "whatever `dev` is now"; product PRs get an assembled-site preview and link check; dispatches carry the revision; one deploy at a time, last-good rollback, a manual rebuild; "Edit this page" links into the owning repo at that commit.

We's docs are not a folder to copy wholesale **[Lal]**: at We's own `docs/README.md` the system documentation lives in `packages/<system>/README.md` and `CONVENTIONS.md`, and the tree mixes current guides with plans and superseded pages. We curates its manifest (public guides, selected package reference, internals, excluded material) and chooses its own entry paths ("contribute a surface, write a module, embed an app").

Alternative: one `coasys/docs` repo with all content. Simpler build; every behaviour change then needs a cross-repo PR.

### 6.3 Tooling

| | Astro Starlight (candidate) | Docusaurus | Stay on Nextra, upgrade to 4 |
|---|---|---|---|
| Multi-product sections | `starlight-sidebar-topics`: one sidebar per product, a switcher | Built-in multi-instance docs plugin | Not designed for it |
| Markdown-first (agents read source) | Yes, plain `.md` | MDX-leaning | MDX |
| API from TypeScript | `starlight-typedoc` | `docusaurus-plugin-typedoc` | typedoc + manual link (broken today) |
| Versions | `starlight-versions` | Built in | No |
| Link checking | `starlight-links-validator` at build | plugin | No |
| Search | Pagefind, static | Algolia or local plugin | Flexsearch |
| Weight / build time | Light, static | Heavy React app | Medium |
| Migration cost | Move the pages, replace Nextra components (Tabs, Callout) | Same | Smallest |

Starlight is the candidate, not a conclusion **[Lal]**: before phase 2 a spike must show the Tabs/Callout components, imported MCP topics, a We package page, typedoc, cross-product links and search, and the redirects working together, with an owner and a recorded result. Docusaurus is the fallback if the team prefers React throughout. Upgrading Nextra is cheapest but keeps a single-product site.

### 6.4 Versions

Today the site tracks `dev` with one version string in the header. That is not a version strategy **[Lal]**: We pins its AD4M packages (`package.json` at We's dev pins SDK and Connect to `0.13.0-test-flows-1`), so the newest AD4M docs are not the docs for the SDK inside a We build. Minimum from day one: every deployed site shows the pinned revisions, dev pages are labelled "Unreleased / next", installation points at the matching artifact, and a released snapshot stays reachable. For runtime topics: the connected executor's bundled docs describe that binary; the website describes the displayed SHA. A version-selector plugin can come later.

### 6.5 Migration and redirects

1. Build the new site at docs.coasys.org with the AD4M section; the We section follows its manifest.
2. Reuse the redirect registry from phase 1 (routes and anchors) for the domain move; test every old link.
3. docs.ad4m.dev stays alive on GitHub Pages serving redirect pages (meta-refresh with a canonical link; Pages cannot send HTTP 301). Keep it until an owner checks remaining traffic; no fixed expiry.
4. Update links in the ad4m README, npm package READMEs, coasys.org, the executor's MCP overview topic, `core/src/language/Language.ts` and `theme.config.tsx` (still `perspect3vism`).
5. Stop committing `docs/`; delete the 51 MB once CI deploys.

### 6.6 User docs

User documentation for the Coasys app is a separate site (for example help.coasys.org or in-app), written for people who do not read code. The developer site links to it. Not part of this proposal beyond reserving the split.

## 7. Phases

| Phase | What | When |
|---|---|---|
| 1 | Night PR (section 5): entrances, tree, splits, new type and query pages, quickstarts, redirects, fixes, MCP topics rendered, build and link check in CI | tonight, if agreed |
| 1b | Tutorial chapters; deploy from CI; delete committed `docs/`; term file v1 | this week |
| 2 | Starlight spike with owner and result; `coasys/docs` shell with its contract; AD4M section live at docs.coasys.org; redirects; We section from We's manifest | after the Friday call, 2–3 weeks |
| 3 | Generated reference (RPC methods, events, MCP tools, SHACL vocabulary), `llms.txt`, glossary from the agreed term list, versions | after #1493 lands |

## 8. Decisions for Nico

1. **Domain:** docs.coasys.org with `/ad4m` and `/we` (recommended), or coasys.org/docs.
2. **Tooling:** Astro Starlight after a spike (recommended), Docusaurus, or stay on Nextra.
3. **Content location:** stays in each product repo with a small `coasys/docs` shell that builds pinned revisions (recommended), or one docs repo with all content.
4. **#1493:** contributor Internals section plus a shared term file that also feeds the runtime topics (recommended). Needs Josh; his Friday trim decides when.
5. **Phase 1 tonight:** yes or no, and whether the three entrances and the concept order in section 3 are what you want.

---

## Appendix A: wrong and stale claims (checked against 13e1c50b8)

Full list with file:line evidence, about 110 items: `docs-overhaul-audit-2026-10-21.md`. The audit was run by a subagent; I re-checked six claims (reifier pattern, `ad4m://initial`, auto ids, connect export, `resolveLanguage` default, Node version) and Lal independently checked five (table in her report). All held, with two wording corrections folded in below. The other items are not individually certified.

- **Pages whose audited examples align with the cited tests:** `flows`, `writing-a-flow`. Their listed omissions (semanticCheck, auto-spawn, the MCP flow tools) and outdated file references remain. GraphQL, Apollo and the old `executor/` package are gone from every page.
- **Wrong in a way that breaks a reader's first attempt:**
  - `sparql-queries` and the SPARQL sections of `social-dna` and `perspectives`: every query matches `?link a rdf:Statement`, which the executor's own writer does not produce (it writes direct triples plus `rdf:reifies` triple terms, `sparql_store.rs:565-586`), uses `ad4m:author` instead of `<ad4m://ontology/author>`, and declares no prefixes, so the queries fail to parse or match nothing **[Lal wording]**.
  - `installation`, `auth`, `multi-user`: `import Ad4mConnect from "@coasys/ad4m-connect"` (no default export; the named APIs `getAd4mClient`, `getAd4mConnect`, `connectAsGuest` have different return contracts, so the whole example is rewritten, not only the import), capability objects with lowercase verbs and a UUID in `domain`, `new Ad4mClient({adminCredential})`.
  - `cli`: invented `run-local-hc-services`, `perspectives create` (it is `add`), wrong `add-link` argument order, `ad4m dev` on the wrong binary, wrong port defaults.
  - `neighbourhoods`: a `LinkSyncAdapter` interface that no longer exists, template parameter `uuid` (it is `uid`), `applyTemplateAndPublish` with an object (it takes a JSON string), `perspective.join` and `languages.install` that do not exist.
  - `social-dna`: `getSubjectProxy` does not exist; the "stored as links" tab and the `ad4m://initial` / `ad4m://readOnly` predicates are invented; the generated shape URI is `todo://TodoShape`.
  - `model-classes`: `resolveLanguage` is not defaulted to `"literal"`; unset means the value is stored as a typed literal, `"literal"` means the value goes through the literal language and gets its own signed envelope (the link is signed either way) **[Lal wording]**; ids are `ad4m://obj/…`; `properties([...])` strips the rest; `fromJSONSchema` needs `name`; `getClassOptions` does not exist.
  - `mcp`: `dynamicClassTools` is not a config-file key; the topic list is five, not two; without an admin credential the MCP server **defaults** to loopback and accepts anonymous callers there, but an explicit `--mcp-host` overrides the bind and presented credentials are still checked **[Lal wording]**.
  - `hooks`: `useClient` does not exist. `ai`: transcription model ids are `whisper_tiny` etc.; VAD parameters are ignored. `installation`: Node 22, not 18.
- **Present on dev but undocumented:** built-ins `TextContent` and `Container`, the `parent: { id }` containment scope and the holder's own children relation; bounded traversal (`transitive`, `maxDepth`, `direction`, `levels`); `$` include projections and `$count` ordering; dotted order keys (need `limit`); relation `some`/`none`; `includeAll`/`deepQuery`; `polymorphic`/`instantiateAs` on `@HasMany`; property `options`, `datatype`, `alias`, `identity`, `interpretationHint`; flow `semanticCheck` and auto-spawn; the interpretation and auto-processor API; the seven MCP flow tools, `language_meta`, `--mcp-host`, the TLS MCP listener; telepresence, signals and SFU; `ANTHROPIC`/`OLLAMA` AI providers.
- **Pipeline:** `/jsdoc` is a 404 on the live site; the typedoc JSDoc itself has examples calling a `.run()` that does not exist.

## Appendix B: the concept walk-through, page by page

| Page | The question it answers | The one example | The one diagram |
|---|---|---|---|
| What AD4M is | Why does my app talk to a local runtime instead of a server? | A chat app reading messages from a perspective at `localhost:12000` | The spanning layer: apps above, languages below, one executor per agent in the middle |
| Agent, executor and capabilities | Who am I, and what may an app do on my behalf? | `agent.me()`, a capability request, the token; local vs hosted connection | Executor box: keys, perspectives, languages, the RPC and MCP ports |
| Perspectives and links | Where does data live, and what is a link? | Add one signed link, read it back as a `LinkExpression` with author and timestamp | Three triples with their signatures; local and shared halves |
| Models and identity | How do I stop thinking in links, and how does the executor know a node is a Task? | `Task` with `@Property`/`@HasMany`, save, find; the same links underneath; what marks an instance | Class → SHACL shape → links; the recognition tiers |
| Types and values | What is a property, a relation, a literal, a signed expression? | One property stored three ways: unset, `"literal"`, a language address | Value: typed literal vs expression envelope (data, author, timestamp, proof) |
| Queries and live results | How do I get exactly the rows I want, and keep them fresh? | `where` + `order` + `limit` + `include` + `subscribe` on `Task`; the same in SPARQL | Model query → SPARQL → store; where verified links are filtered |
| Abstract classes and aliases | How does a view or an agent read records from an app it does not know? | `Readable` matched by `Message.body` through an alias; `TextContent` reading both | What abstract matching compares (names, aliases) and what it does not (predicates, datatypes, directions) |
| Containment and navigation | What holds what, and how do I scope a query to it? | A Board holding tasks through its own relation and through `has_child`; one task under two holders | Holders → children edges; scope and reverse traversal |
| Neighbourhoods and trust | How do two people see the same graph? | Publish with the local link language template, join by URL from a second executor | Two executors, one link language, the shared and local halves |
| Social DNA and flows | Where do a group's rules live, and what is actually enforced? | A `Task` status flow with a `requires` guard; a link that no class recognises | Classes recognise, flows gate; the boundary |
| Flows | How does a group agree on a transition? | existing page | existing |
| Agents | How does a bot work with my data without knowing my app? | `describe_perspective` → `instance_create`; a waker; `get_documentation` | Agent ↔ MCP ↔ executor (executor tools vs plugin wakers vs in-executor interpretation) |
| Hosted deployment | What changes when the executor is hosted? | Sign-up, login, guest connect, `wss://host` with a token | One host, N agents, one executor process, isolated data |
| Language protocols | How do I attach a protocol or storage to AD4M? | `defineLanguage` with `expressionCreate`/`expressionGet`; the `<hash>://addr` scheme | Language → expressions → addresses |

## Appendix C: Lal's critique (Astra, read-only at 13e1c50b8)

Her verdict: approve the direction, not the promises as written. Eleven findings, all folded in and marked **[Lal]** above; the ones that changed the proposal most:

1. Three job entrances (app, agent, operator) instead of one app-first syllabus; an agent quickstart that does not depend on the app tutorial; a troubleshooting page.
2. "Matches by names, not predicates" describes abstract-interface compatibility only; concrete instance recognition is a different mechanism. Split into Types and values, Abstract classes and aliases, Containment, plus a built-ins reference.
3. Containment is not a tree: one child under four holders is a tested case; a holder's own children relation counts too.
4. Expression basics and the storage choice belong early; only language authoring goes late. "Multi-user changes nothing in app code" was wrong: the guest connect flow differs.
5. One authority per fact, not "no overlap"; the runtime topics keep their definitions compiled in; a markdown file is not a served topic until it is in the `DocTopic` enum.
6. We docs need a We-owned manifest; package READMEs and CONVENTIONS hold the authoring contracts.
7. Two moving `dev` branches are not a version strategy; We pins its SDK; record pinned revisions and label unreleased pages.
8. The shell needs an owner, a manifest, pinned revisions, previews, rollback.
9. Redirects with anchors belong in phase 1, not only at the domain move; the spec files have code consumers.
10. Phase 1 is not markdown-only; drop universal line budgets; Starlight needs a spike.
11. Prose that overstated: "nobody finds them", "no overlap", "drifts the day it is created", "correct pages", "all return nothing", "classes + flows = the group's rules", "no sentence dropped without a check". All rewritten.

Her five audit checks: `resolveLanguage` default, the `rdf:Statement` pattern, the connect default export, MCP loopback default, built-ins and containment present on dev. All confirmed, with the two wording corrections in appendix A.

Her own IA proposal is in her report; section 3 adopts it with two differences: the tutorial keeps seven numbered chapters, and How-to keeps a Languages group.
