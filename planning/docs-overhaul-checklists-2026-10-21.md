# Docs overhaul phase 1: per-page verification checklists

Companion to `docs-overhaul-2026-10-21.md` (design d20) and `docs-overhaul-audit-2026-10-21.md`. PR for issue #1503. Content base: `dev` 13e1c50b8.

Each section is the checklist one content agent produced for its pages: sources used, audit items fixed (audit line quoted), claims dropped as unverifiable, every API call in a sample with the file and line that verifies it, and open questions. Headings are demoted one level; the text is otherwise as written. Paths such as `/home/data/...` are the author's worktree.


---

## Checklist: agent `models` (docs phase 1, #1503)

Worktree `/home/data/code/ad4m-wt/docs-overhaul`, origin/dev 13e1c50b8. Nine new files under `docs-src/pages/`; no old page edited; nothing committed. `node docs-src/scripts/check-portability.mjs` passes for all nine (its only reports on them are link targets that other agents own: how-to/define-a-model, write-through-abstract-class, filter-by-author, paginate, subscribe, include-relations, sparse-fields, batch-writes, reference/mcp-tools, concepts/language-protocols).

Line counts: models-and-identity 175, types-and-values 192, queries 200, abstract-classes-and-aliases 143, containment 128, decorators 179, model-query 179, built-in-interfaces 104, shacl-vocabulary 199.

### concepts/models-and-identity.mdx

Sources: model-classes.mdx §Quick Start, §Properties, §How It Works Under the Hood, §Instance Identity, §Dirty Tracking, §Built-in Instance Properties, §Model Inheritance; social-dna.mdx §How Subject Classes are Defined (SHACL), "Stored as Links" tab (replaced).

Audit items fixed:
- "ids look like `literal:...`" → `ad4m://obj/<24 chars>` (`Ad4mModel.ts:757-760`).
- "legacy `.baseExpression` getter still works" → dropped; only `id` (`Ad4mModel.ts:769`).
- "shape URIs `shacl://Recipe`, `recipe://Recipe`" → `${ns}${Name}Shape`, suffix enforced (`shacl-gen.ts:84-86`, `shacl_parser.rs:1626-1637`); namespace = scheme of first property's `through` (`shacl-gen.ts:60-70`).
- "Stored as Links tab is invented" → real links: `ad4m://self -ad4m://has_shacl-> literal:string:shacl://Task -ad4m://shacl_shape_uri-> board://TaskShape`, `rdf://type ad4m://SubjectClass`, property shapes `board://Task.title`, counts `literal:1^^xsd:integer` (`shacl_parser.rs:1641-1653, 1667-1680, 1732, 1811-1820`).
- "`ad4m://initial`" → initial values are `addLink` entries in `ad4m://constructor` (`shacl-gen.ts:264-282`, `SHACLShape.ts:449-457`).
- "Attempting to update a flag throws" (old 520) → `save()` skips flags (`Ad4mModel.ts:2035-2039`).
- MISSING "how a record is recognised as an instance" → tiers table from `sparql_builder.rs:993-1101`.

Claims removed as unverifiable: none carried that I could not check; the old "Every node in this graph is a URL pointing to an Expression" sentence dropped (values are typed literals by default).

APIs in samples and where verified: `@Model/@Flag/@Property` (`decorators.ts:706, 545, 807`); `Task.register` (`Ad4mModel.ts:2398`); `Task.create` (`2273`); `findAll` (`1491`); `new Task(p, id)` (`752`); `isDirty/changedFields` (`941, 958`); `subjectClassesOf` (`PerspectiveProxy.ts:720`); inheritance via `sh:node` (`shacl-gen.ts:107-110`; `model-inheritance.test.ts:63-72`, models `TestBaseModel/TestDerivedModel`). Fields `author/createdAt/updatedAt/timestamp/__links` (`Ad4mModel.ts:524-538`). Union/abstract/polymorphic `__subjectClass(es)` (`Ad4mModel.ts:52-56`, `model-union.test.ts:108,167`).

Open question: the stored-links listing omits the `ad4m://has_subject_class` link `addSdna` writes (`sdna.rs:107`); it is in the SHACL reference. Fine to leave the concept page abridged?

### concepts/types-and-values.mdx

Sources: model-classes.mdx §Properties (table), §Relations, §Ordered relations, §Flags, §Literals for Simple Values, §transform, §Relation Filtering; expressions.mdx §Anatomy, §Global Addressing, §Signature Verification (kept), §Expression Types (corrected).

Audit items fixed:
- "`@Property` defaults to `resolveLanguage: 'literal'`" (229, 244, 1625) → not defaulted; three modes table (`decorators.ts:312-324`, `query-sparql.ts:233-239`, `Ad4mModel.ts:1645-1655`).
- expressions.mdx "`literal:base64encoded-content`" → `literal:string:<percent-encoded>` (`Literal.ts:29-51`); the `"literal"` mode envelope is `literal:json:<percent-encoded {author,timestamp,data,proof}>` (`languages/mod.rs:2591-2605`, `literal.rs:22-40`).
- "`resolveLanguage: 'markdown'`" (1632) → must be `"literal"` or an address; example uses a language constant.
- "Quick Start stores strings in `@HasMany` without datatype" → `datatype: "xsd:string"` shown (`hasmany-hydration.test.ts:44`; `decorators.ts:970-986`).
- "custom Prolog/SPARQL getter" (336) → getter is SPARQL; Prolog disabled (`rust-executor/AGENTS.md:54`).
- "ordering: declaration only" (JSDoc) → implemented; `ad4m://collection_order` (`model-ordering.test.ts:42, 128-200`).
- MISSING `options`, `datatype`, `alias`, `identity`, `interpretationHint`, relation `polymorphic/instantiateAs/datatype/readOnly` → covered in tables.

Removed: expressions.mdx "Reference Expressions … `http://example.com/resource`" category (no code evidence); "verification flags computed on-the-fly against the current DID document" narrowed to "computed when resolved".

APIs verified: `Literal.from/fromUrl/toUrl/get` (`Literal.ts:13-80`); `fileToDataUri` export (`shacl/index.ts:52`, `index.ts`); `@HasMany/@HasOne/@BelongsToOne/@BelongsToMany` (`decorators.ts:1242-1479`); default `through` `ad4m://has_child` (`1230`); `sh:nodeKind IRI`/`sh:class` (`shacl-gen.ts:318-325`, `SHACLShape.ts:532-536`); `datatype` excludes `target` (`decorators.ts:1142-1151`); `ExpressionProof` fields (`Expression.ts:6-15`). Store typed literals (`sparql_store.rs:104-140`).

Open question: the `image` example uses a `FILE_STORAGE_LANGUAGE` placeholder constant (not an SDK export); the old page did the same. Coordinator may want to name the real address or a comment.

### concepts/queries.mdx

Sources: model-classes.mdx §Querying through §Real-time Subscriptions (all subsections), §Query Engine; perspectives.mdx SPARQL section not used (its examples are wrong).

Audit items fixed:
- "`properties([...])`: other fields stay at defaults" (924) → removed, `[]` throws (`model-query.test.ts:1151-1181`; `Ad4mModel.ts:1499`).
- "`getClassOptions`" → not mentioned (see decorators ref).
- SPARQL example written with direct triples and no `rdf:Statement` (`sparql_store.rs:565-586`); `perspective.querySparql` (`PerspectiveProxy.ts:665`).
- MISSING: `some/none` (`model_query/types.rs:47-51, 184-187`; not in TS `WhereOps` `types.ts:29-47` → `as any`; `model-subscriptions.test.ts:510`), `producedByFlow` (`types.ts:65-81`), `$` projections and `$count` order (`types.ts:321-373`; `model-query.test.ts:434-470`), dotted order needs limit (`model-query.test.ts:475-511`; `query.rs:505-520, 589`), `includeAll` (`types.ts:385-391`; `Ad4mModel.ts:1126-1135`), `deepQuery` (`types.ts:405-412`), parent scopes incl. containment `{ id }` (`types.ts:107-286`; `bounded-traversal.test.ts:131-317`; `model-containment.test.ts:185-202`), `count()` = `limit: 0` (`Ad4mModel.ts:1620-1628`), `findAllOf/queryOf` (`1391, 1413`).
- "Prolog" → stated disabled (`rust-executor/AGENTS.md:54`).

APIs verified: builder methods (`ModelQueryBuilder.ts:125-599`); `subscribe/dispose` semantics (`81-85, 467-500`); author table (`model-where-author.test.ts` header; `types.ts:41-47`); `links/LinkRow` (`types.ts:457-500`); `linkStatus` (`types.ts:413-446`; `model-link-status.test.ts:175-204`); `includeUnverified` (`types.ts:447-466`).

Open question: "the executor resolves [dotted/`$`] order keys only in the paged plan" is my reading of `query.rs:505-564` plus the tests' `limit: 10`; memory note from #1386 agrees. Worth one sentence from Nico if he knows a sharper rule.

### concepts/abstract-classes-and-aliases.mdx

Sources: model-classes.mdx §Abstract Classes, §Writing Through an Abstract Class; `TextContent.ts`/`Container.ts` JSDoc.

Audit items: MISSING built-ins → covered; proposal's "matching by names and aliases, not predicates/datatypes/directions" (`abstract_classes.rs:153-214`); `implements` SDK-only (`decorators.ts:618-647`).

APIs verified: `@Model({ abstract, implements })` (`decorators.ts:581-612, 706-713`); `alias` (`decorators.ts:410-415, 1090-1097`); `registerAll` (`Ad4mModel.ts:2418`); example adapted from `model-abstract.test.ts:22-76, 118-140`; `updateSubject/removeSubject` (`PerspectiveProxy.ts:1588, 1649`); `TextContent/Container` exports (`core/src/index.ts:23-24`); `builtinUri` read by `queriedAs()` (`Ad4mModel.ts:1114-1117`).

Removed: old table row "MCP: tools generated for `Readable` (its reads only)" (per-class tools are opt-in; not re-verified tonight).

### concepts/containment.mdx

Sources: new; `containment.rs:1-126` module doc, `Container.ts` JSDoc, `model-containment.test.ts`.

APIs verified: `create(..., { parent: { id } })` writes own relation else `has_child` (`model-containment.test.ts:205-220`); `parent: { id }` / `{ ids }` / `direction: "in"` (`170-202`); four holders (`185-202`); stray-predicate rule (`222-290`); `Container.findAll({ where: { id } })` (`121-125`); transitive containment nearest-first, `levels` refused, `limitPerAnchor` refused with transitive (`types.ts:166-178`); `perspective.add(new Link(...))` (`PerspectiveProxy.ts:786`); `delete()` removes incoming links (`Ad4mModel.ts:2181-2230`, old page claim re-checked: destructor path + incoming cleanup).

Open question: I state `maxDepth` works on a containment traversal from `types.ts:177` ("bound it with maxDepth"); no JS test exercises it. Rust tests may.

### reference/decorators.mdx

Sources: model-classes.mdx §All property options, §Relation types, §Filtering options, §Static Convenience Methods, §Introspection, §Dynamic Models from JSON Schema, §PerspectiveProxy Introspection APIs.

Audit items fixed: `getModelMetadata()` shape (`types.ts:854-861`); relation `getter` combines with `target`/`where`, with `through` only when `readOnly` (`decorators.ts:1156-1203`); `fromJSONSchema` `name` required (`json-schema.ts:47, 279-281`), cascades (`108-205`), `className` x-ad4m unused; "at least one property must be required" → all-optional OK (`model-from-json-schema.test.ts:335-360`); `getClassOptions` → does not exist, `getClassShape` returns `options` (`PerspectiveProxy.ts:2085-2150`); `@Model interpretationHint` (`decorators.ts:605-611`); every `PropertyOptions` field (`268-415`) and `RelationOptions` field (`891-1097`); `ordering` implemented; transform builders (`shacl/builders.ts:13-84`, `shacl/index.ts:52`); `count`, `registerAll` (`Ad4mModel.ts:1620, 2418`); `HasManyMethods` (`decorators.ts:1110-1116`); HasOne gets add/remove/set (`1340-1356`); JSON Schema type mapping incl. `literal:json:{}` / `ad4m://undefined` initials (`json-schema.ts:296-360`); `author` property refused (`269-271`).

Open question: `prologGetter/prologSetter` listed with "Prolog disabled; prologSetter unsupported in SHACL generation" (`shacl-gen.ts:236-240`). Keep or drop entirely?

### reference/model-query.mdx

Sources: model-classes.mdx §Querying…§Transactions; batch-operations.mdx §Using Transactions, §Manual Batch Control; `types.ts` 20-500.

Audit items fixed: batch "thrown transaction not discarded on executor" → stated; `perspective.discardBatch` RPC exists (`RpcMethods.ts:268`), no SDK wrapper (grep `core/src`); `transaction` body (`Ad4mModel.ts:2452-2460`); all MISSING query rows (see queries page); `preferClasses` (`types.ts:392-400`); raw RPC `modelQuery(className | string[], queryJson)` → `{ instances, totalCount }` (`PerspectiveProxy.ts:686-688`), `modelSubscribe` (`757`); wire reshaping (`Ad4mModel.ts:1119-1200`: `includeAll` expansion, `projections`, parent normalization). Link write calls `add/addLinks/remove/removeLinks/update` (`PerspectiveProxy.ts:786-873`); `commitBatch` → `{ additions, removals }` (`Links.ts:23-26`).

Open question: I did not document `Query.count?: boolean` (undocumented in `types.ts:399`) or the `polymorphic` wire flag beyond one sentence.

### reference/built-in-interfaces.mdx

Sources: `hardwired_sdna/text_content.json`, `container.json`; `builtin_classes.rs:1-60`; `containment.rs:85-126`; MCP tool descriptions `instances/children.rs:50-95`, `transcript.rs:87-140`, `create.rs:32-39`, `query.rs:27-51`, `describe.rs:185`. Tool parameter names match the tool schemas (`perspective_id, parent, child, limit`). Links to `./mcp-topics/overview.md` and `usage.md` per brief.

### reference/shacl-vocabulary.mdx

Sources: social-dna.mdx Turtle tab and key-concepts table (both corrected); model-classes.mdx §Wire Format, §How SHACL Maps to AD4M.

Audit items fixed: Turtle `sh:datatype` on a relation → `sh:nodeKind sh:IRI` (`shacl-gen.ts:318-325`); `ad4m:resolveLanguage` emitted only when set (`shacl-gen.ts:218`); datatypes `xsd://string|decimal|boolean` (`176-188`); `ad4m://initial`/`ad4m://readOnly` → do not exist, real predicate list (`SHACLShape.ts:421-745`; parser emission counts via grep of `shacl_parser.rs`); `ad4m://getter` carries any getter (`shacl-gen.ts:222-224`); "`maxCount 1` generates `set_` tool" rows dropped (per-class MCP tools opt-in). JSON keys from `toJSON()` (`SHACLShape.ts:847-893`); Turtle rules from `toTurtle()` (`374-414`): booleans/counts bare, `sh:in` list + `ad4m:in`, blank-node property shapes with `sh:name`. Registry links (`shacl_parser.rs:1641-1670`, `sdna.rs:107`).

Claims flagged: the Turtle sample is hand-derived from `toTurtle()` (no built SDK in the worktree to run it), marked "abridged"; statement order inside a property shape may differ from real output. Coordinator could regenerate it after `pnpm build` of core.

### Not done / out of scope

- No how-to pages (not in my list). Links to them assume the brief's tree.
- `reference/mcp-tools.mdx` is another agent's; my tables name five tools and their parameters from the Rust sources only.

---

## Checklist: agent `collab3` (docs phase 1, #1503)

Worktree `/home/data/code/ad4m-wt/docs-overhaul`, origin/dev 13e1c50b8. Three new files; eleven existing pages verified and fixed in place; no old page edited; nothing committed. `node docs-src/scripts/check-portability.mjs` reports nothing for the three new pages.

Line counts: concepts/language-protocols 158, how-to/write-a-language 209, how-to/publish-a-language 92.

### concepts/language-protocols.mdx

Sources: languages.mdx (all sections), expressions.mdx §Global Addressing, language-interface-spec.md §1-§4, §7, §9-§10, host-contract.md overview, ad4m-social-conventions.md (link only), rust-executor/src/languages/{language_runtime,mod,capability}.rs, js_core/{language_bootstrap.js,host.js,mod.rs}, ad4m-ldk/js, ad4m-ldk/rust.

Headings kept from the old page so un-mapped redirect anchors still land: `what-a-language-is`, `templates`, `security-model`, `the-core-languages`. Exports / Imports split kept as `### Exports: what the executor calls` and `### Imports: what a Language calls`.

Audit items fixed:
- languages: "`perspectiveSync()` export (115)" → `perspectiveSyncSync`/`Render`/`CurrentRevision` (`language_bootstrap.js:350-380`, `language_runtime.rs:314-334`).
- languages: "`ad4m.languages.install` (354)" → not used; install is `languages.byAddress` (how-to page).
- expressions: "`Expression.language` field" → not mentioned.
- neighbourhoods "LinkSyncAdapter" → link languages described as flat capability exports.

Claims removed as unverifiable or false:
- "Partial implementations are a load-time error" (spec §3): detection is per function (`language_runtime.rs:310-334`), no error found.
- "A runtime that does not support an extension refuses to load a Language targeting its world" and the WIT world names: not found in the executor loader.
- "There is no callback registration" (spec §1): the executor still wires `linkSyncAddCallback` etc. (`language_runtime.rs:262-290`); left out rather than stated.
- Old "Templates" section (four external template repos, pre-v1.0): not verifiable tonight; replaced by the template-variable mechanism.
- `ad4m_language!` sample with `http_post_json` to example.com: replaced by a KV sample (how-to).
- Old core-Language table row "perspective-diff-sync is the default"; replaced by "known link languages" (`mainnet_seed.json`). Dropped the perspective language row (purpose not verified; alias exists `languages/mod.rs:756`).

Verified (file:line):
- address = content hash, `Qm` + base58btc CIDv1 sha256: `languages/mod.rs:454-469`; `save_language_bundle` 533-552.
- `did`/`lang`/`neighbourhood`/`perspective` aliases: `languages/mod.rs:753-756`; `did` URLs not prefixed: 2680-2690.
- named or default export: `language_runtime.rs:185-196`; `init()` without args: `language_bootstrap.js:333`.
- export list: `language_bootstrap.js:350-380`; capability set: `capability.rs:18-56`.
- `defineLanguage` + flat names: `ad4m-ldk/js/src/defineLanguage.ts:144-233`; pattern `export const {…} = language`: `bootstrap-languages/centralized-file-storage/index.ts:19-103`.
- imports list: `ad4m-ldk/js/src/imports.ts:19-63`; sync signatures `host.d.ts:12-67`; KV keys prefixed by language address `host.js:438-441`; httpFetch 10 s `host.js:124,152`.
- signing as the acting agent: `language_runtime.rs:17-60`.
- template variables: `languages/mod.rs:1131-1190`; markers in `bootstrap-languages/centralized-p-diff-sync/index.ts:13`, `server-link-language/index.ts:57-61`.
- install + trust: `languages/mod.rs:1695-1975` (proof check 1745-1756, hash check 1808-1813, trusted 1783, template path 1842-1970, `AgentIsUntrusted` 1871/1917); `RuntimeClient.addTrustedAgents` `core/src/runtime/RuntimeClient.ts:29`.
- sandbox: `js_core/mod.rs:96-150` (fs = storage dir, + CWD for system languages; net allowed; env/run/ffi/sys denied); one thread per language `language_runtime.rs:17-20`.
- publish = expression in the language language: `api/languages_ws.rs:163-181`.
- `expression.create` returns `<lang>://<addr>`: `languages/mod.rs:2680-2690`; `proof.valid` on get: `tests/js/tests/flat-language.test.ts:48-53`.
- kits not published: `npm view @coasys/ad4m-ldk` 404, crates.io `ad4m-ldk` does not exist (checked 2026-10-21).

Open questions:
- The page says both kits "are not yet published to npm or crates.io" (true tonight). If a release is imminent, the coordinator may prefer to drop that sentence.

### how-to/write-a-language.mdx

Sources: languages.mdx §Authoring a Language (both kits), ad4m-ldk/js/README.md, ad4m-ldk/rust/README.md, spec §9-§10, the test Languages.

Audit items fixed: old Rust sample used `http_post_json`/`http_get` against example.com and returned the POST result as the address; replaced by a KV sample modelled on `tests/rust-languages/test-wasm-language/src/lib.rs:80-127`. TS sample now exports the flat names (old sample only `export default`, which also works but differs from every bootstrap Language).

Verified:
- TS sample: `defineLanguage` (`defineLanguage.ts:22-29, 144-187`), imports sync (`host.d.ts:16,39,54-55`); flat-export alternative `tests/js/languages/note-store-flat/index.ts`.
- Rust sample: `Language`/`ExpressionCapability` (`ad4m-ldk/rust/src/traits.rs:12-72`), `rt::hash/storage_get/storage_put/agent_create_signed_expression_typed` (`imports.rs:59,87-89,198-203`), `?` conversions `errors.rs:55-75`, `Address = String` `types.rs:6`, macro idents `macros.rs:167-288`, `holochain_signal` option `macros.rs:39-45`; macro needs `wasm_bindgen`, `serde_json`, `serde_wasm_bindgen` in the user crate (grep of `::crate::` paths in `macros.rs`).
- Bundling: esbuild config = `bootstrap-languages/centralized-file-storage/esbuild.ts`; `@coasys/ad4m-ldk` build = `tsc` (`ad4m-ldk/js/package.json`), workspace member (`pnpm-workspace.yaml:8`). Rust steps = `tests/rust-languages/build.sh`, `inline-wasm.mjs:11-29` (basename rule), wasm-bindgen-cli 0.2.117.
- Round trip = `tests/js/tests/flat-language.test.ts:22-53, 111-160`.
- Error text "Language bundle does not export init()": `language_runtime.rs:192-195`.

Not compiled: neither sample was built tonight (no build allowed). The Rust sample is a reduction of the test language; the coordinator may want one `cargo check --target wasm32-unknown-unknown` on it.

### how-to/publish-a-language.mdx

Sources: languages.mdx §Publishing, installing, using; cli/src/languages.rs, expression.rs, runtime.rs; api/languages_ws.rs.

Audit items fixed:
- languages: "`ad4m.languages.install(meta.address)` (354)" → `languages.byAddress` installs (`languages_ws.rs:45-62`).
- agents: "`expression.create(languageAddress, content)`" → `(content, languageAddress)` (`ExpressionClient.ts:165-168`).
- neighbourhoods: "`applyTemplateAndPublish(addr, {…})`" → JSON string (`LanguageClient.ts:29`).
- cli: commands checked (`cli/src/languages.rs:6-46`, `expression.rs:7-18`, `runtime.rs:7-16`, `ad4m.rs:86-157`).

Verified: `LanguageMetaInput(name, description)` + optional fields (`core/src/language/LanguageMeta.ts:13-24`); `publish(path, meta)` → `LanguageMeta` with `author` (`LanguageClient.ts:33`, `languages_ws.rs:135-200`); path read on the executor host (`languages_ws.rs:147`); user sessions refused for publish/remove/writeSettings (`languages_ws.rs:140,277,292`, `guards.rs:14-21`); capability `language`/`CREATE` (`defs.rs:134-140`); trust error text (`languages/mod.rs:1862-1865`); template-clone hash check (`mod.rs:1966-1975`); `meta/all/remove` (`LanguageClient.ts:21,37,45`); `ExpressionRendered.data` is a JSON string (`core/src/expression/Expression.ts:59-61`; test asserts `JSON.stringify(content)`); CLI publish prompts and local pre-read (`languages.rs:112-145`).

Open question: whether `languages.meta(address)` installs as a side effect (`get_language_expression`) was not traced; the page only says it reads the meta.

---

## Checklist: howto agent (docs phase 1, coasys/ad4m#1503)

Worktree /home/data/code/ad4m-wt/docs-overhaul at 2f612ec72 (content base origin/dev 13e1c50b8). Paths below are repo-relative. Links and anchors on all eight pages checked with a slugger script (`.scratch/docs-phase1/howto-tools/anchors.mjs`): all targets and anchors exist. `scripts/check-portability.mjs` reports nothing for `how-to/`.

---

### 1. how-to/filter-by-author.mdx (NEW)

**Sources:** developer-guides/model-classes.mdx §"Filtering by Author" (813-848); concepts/queries.mdx §"Filtering by author" (89-99, kept consistent); reference/model-query.mdx author table (44-54).

**Audit items fixed:** none listed for this section in the audit (the old section was correct). Old page's link `#unverified-links` replaced by `../concepts/queries.mdx#unverified-links`.

**Added beyond the old section (verified):** `contains` on the author is case-insensitive (`link_author.rs:276-281`); `not`/`contains` on a *relation* beside a per-link author is refused (`link_author_tests/refusals.rs:29-30`); the error strings; the bare form reads the earliest *verified* link (`selection_guard_tests/where_and_count.rs:15-19`); relations typed as model classes need `as any` in the typed where (`types.ts:557-563` excludes `Ad4mModel[]` keys from `PropertyKeysOf`).

**Claims removed as unverifiable:** none; the old "the instance JSON does not change" folded into "the row's `.author` is always the earliest link's" (test `model-where-author.test.ts:134`).

**API calls / claims and their verification:**
- `AuthorCondition = string | string[] | { not?, contains? }` — `core/src/model/types.ts:24-27`; nested `author` in `StringWhereOps`/`NumericWhereOps` — `types.ts:594-613`; top-level `author?: WhereCondition` — `types.ts:632-639`.
- `(await ad4m.agent.me()).did` — `core/src/agent/AgentClient.ts:52`; used in `tests/js/tests/model/model-where-author.test.ts:113`.
- `@Model`, `@Flag`, `@Property`, `Ad4mModel` exports — `core/src/model/index.ts` → `decorators.ts`; same imports in `model-where-author.test.ts:24`.
- `findAll`, `count`, `findAllAndCount`, `query().where().get()` — `Ad4mModel.ts:1491, 1620, 1554, 2485`; count/findAll/paged agree — `model-where-author.test.ts:82-91` (`idsFor`).
- nested per link / side by side / bare semantics, negation, sub-clause scope, `none` not scoped — `rust-executor/src/perspectives/model_query/link_author.rs:1-73` (module doc); tests `model-where-author.test.ts:128-207`; `link_author_tests/nested.rs` (`a_nested_author_beside_a_quantifier_scopes_the_relation_link` :445, `author_alone_means_some_link_on_the_property_by_that_author` :133, `author_and_a_value_operator_hold_on_the_same_link` :189 incl. `contains`), `link_author_tests/side_by_side.rs` (`side_by_side_does_not_scope_a_none_quantifier` :193, `side_by_side_not_negates_both_halves` :122).
- "link need not hold the current value" — `link_author.rs` module doc + old page; consistent with `nested.rs:102` `the_author_of_the_matching_value_counts_not_any_link_on_the_predicate`.
- only verified links count; guard on the same reifier — `link_author.rs:290-341` (`link_author_join`, `guard.on_reifier`), `sparql_builder.rs:490-504` (`proofValid "true"` unless `includeUnverified`); test `selection_guard_tests/where_and_count.rs:21` `selection_a_forged_admin_link_does_not_satisfy_an_author_condition`.
- refusals and messages — `link_author.rs:420-443` ("where: a per-link `author` ... is answered in the store"); `sparql_builder.rs:1365-1370` ("has a nested `author`, but it is not a property stored as a link"); `link_author.rs:176-178, 199-201` (malformed author / `eq cannot be combined`); tests `refusals.rs:10-185`, `model-where-author.test.ts:209-221`.
- bare beside only timestamp/getter is not refused — `side_by_side.rs:238, 278`.

**Open questions:** none.

---

### 2. how-to/sparse-fields.mdx (NEW)

**Sources:** model-classes.mdx §"Sparse Fieldsets with properties" (916-925); concepts/queries.mdx:141-143; reference/model-query.mdx:19.

**Audit items fixed:**
- "WRONG — `properties([...])`: 'other fields stay at defaults' (924) — unrequested fields are removed, including author/createdAt; `properties: []` throws." Fixed: page says removed, metadata included, `[]` throws.
- Old claim "avoid unnecessary work (especially ... non-literal resolveLanguage that trigger network calls)" is WRONG at this commit and is reversed on the page: the executor hydrates in full, evaluates getters, then strips (`rust-executor/src/perspectives/model_query/query.rs:936-985`, `hydration.rs:366-382`).

**Claims removed as unverifiable:** network-call savings (contradicted, see above).

**API calls / claims and their verification:**
- `properties?: string[]` on `Query` — `core/src/model/types.ts:383`; typed form `properties?: PropertyKeysOf<T>[]` — `types.ts:735`; type test rejecting undeclared names — `core/src/model/types-typed-query.test.ts:335-336`; `author`/`createdAt`/`updatedAt` are `Ad4mModel` members (`Ad4mModel.ts:523-525`) and so excluded by `ModelDataKeys` (`types.ts:543-547`) → builder route.
- builder `.properties(names: PropertyKeysOf<T>[] | string[])` — `core/src/model/ModelQueryBuilder.ts:266-268`.
- unrequested fields removed incl. metadata; `id` kept — `tests/js/tests/model/model-query.test.ts:1162-1196`; `hydration.rs:368` (`id`, `baseExpression` always kept); SDK deletes constructor-set own props — `Ad4mModel.ts:251-262`.
- metadata by name — `model-query.test.ts:1198-1210`.
- include kept although unlisted — `query.rs:972-978`; test `model-query.test.ts:1259-1276`; nested `properties` in include — test :1244-1257, SDK passes `nestedProperties` — `Ad4mModel.ts:347-358`.
- `$` projections and `__links` attached after the trim — `query.rs:986-1010` (comment "After `filter_properties`, so a `properties` selection cannot strip it").
- relation methods and `save()` still work; dirty tracking writes only the change — `model-query.test.ts:1212-1242`.
- `Object.hasOwn` vs `in` — test comment `model-query.test.ts:1170-1172`.
- `properties: []` throws in `findAll` (and `findOne`, which calls `findAll`) — `Ad4mModel.ts:1498-1500, 1533`; test :1151-1160. Not thrown by the builder's `get()` or by `paginate`/`findAllAndCount` (the only check is at 1498), so the page names only `findAll`/`findOne`.
- `deepQuery: false` skips getter-backed scalars only — `rust-executor/src/perspectives/model_query/getters.rs:323-330`.

**Open questions:**
- `reference/model-query.mdx:19` says "`[]` throws" without saying where; with the builder or `paginate` it does not throw (the executor then returns ids only). Coordinator may want "in `findAll`/`findOne`" there too.

---

### 3. how-to/batch-writes.mdx (NEW)

**Sources:** developer-guides/batch-operations.mdx (whole); model-classes.mdx §"Transactions" (1279-1298); concepts/perspectives-and-links.mdx:80 and reference/model-query.mdx:159-173 (both consistent with the code; no change needed there).

**Audit items fixed:**
- "batch: STALE (minor) — a thrown transaction is not discarded on the executor. `Ad4mModel.ts:2452-2460`" — page section "When a transaction throws".
- "perspectives: MISSING — batches ..." — covered here (the concept page links here).

**Old claims dropped:** "All operations are applied together or none are applied" for manual batches with try/catch (the catch does not discard; replaced by the exact behaviour); "Updates are performed using a oneshot channel"; Prolog mentions; "Batch operations don't provide explicit locking"/network-retry advice (not verifiable as stated); the Migration Guide.

**API calls / claims and their verification:**
- `Ad4mModel.transaction(perspective, fn)` → `createBatch`, `fn({ batchId })`, `commitBatch`, returns result; no discard on throw — `core/src/model/Ad4mModel.ts:2452-2460`; rethrow + nothing persisted — `tests/js/tests/model/model-transactions.test.ts:212-238`.
- `Model.create(perspective, data, { parent?, batchId? })` — `Ad4mModel.ts:2273-2306`; self-batching with `parent` and no batch — `Ad4mModel.ts:2283-2295`.
- `instance.save(batchId?)`, no re-read inside a batch — `Ad4mModel.ts:1794-1810`; new instance without batch opens its own — `Ad4mModel.ts:1820-1823`; `instance.delete(batchId?)` — `Ad4mModel.ts:2181`.
- static `update(perspective, id, data)` / `delete(perspective, id)` take no batch id — `Ad4mModel.ts:2325-2381`.
- relation methods `addX/removeX/setX(value, batchId?)` — `core/src/model/decorators.ts:1110-1114, 1275-1282`; `addRelationValue` → `executeAction(..., batchId)` — `Ad4mModel.ts:1712-1738`.
- `perspective.add(link, status = 'shared', batchId?)`, `addLinks`, `removeLinks`, `addLinkExpression`, `update`, `remove`, `createBatch`, `commitBatch(): LinkExpressionMutations`, `linkMutations` (no batch id) — `core/src/perspectives/PerspectiveProxy.ts:786, 801, 814, 828, 842, 853, 873, 910-918`; `executeAction(..., batchId)` — :317; `createSubject(class, id, values?, batchId?)` — :1536-1541; `updateSubject(..., batchId)` — :1588; `removeSubject(class, id, batchId)` — :1649.
- `LinkExpressionMutations { additions, removals }` — `core/src/links/Links.ts:23-30`; `Link` class export — `core/src/index.ts:14`.
- writes invisible until commit; model updates and deletes too — `tests/js/tests/prolog-and-literals.test.ts:2097-2225` (active); executor holds batched links in `batch_store` — `rust-executor/src/perspectives/perspective_instance.rs:2053-2066`. (The raw-link batch tests in `tests/js/tests/perspective.ts:395` are `describe.skip`.)
- one commit to store, subscriptions and link language — `perspective_instance.rs:6787-6900` (`commit_batch`: one `spawn_commit_and_handle_error` for the shared half, one `persist_link_diff`).
- re-sign at commit — `perspective_instance.rs:6826-6829` (`create_signed_expression(link.data.normalize(), context)` for every addition).
- `No batch found with given UUID` — :6801; `Batch not found` — :1869, 2057, 2118, 2346, 2469; batch store is per perspective instance — :617, 719.
- remove/update of a link only in the open batch fails — `remove_link` batch branch looks up the store (`perspective_instance.rs:1865-1885`, "Link not found"); `update_link` (:2311-2335).
- abandoned batches swept on a later `create_batch` after `BATCH_TIMEOUT_SECS = 300` — :478, 6743-6761.
- `perspective.discardBatch` RPC → `bool` — `rust-executor/src/api/perspectives_ws.rs:865-877, 2665`; generated type `core/src/generated/api/RpcMethods.ts:268`; no wrapper in `PerspectiveClient.ts`/`PerspectiveProxy.ts` (grep); Rust test `perspective_instance.rs:7787` `discard_batch_removes_pending_batch_and_is_idempotent`.

**Open questions:**
- Re-signing at commit means `addLinkExpression(foreignLink, status, batchId)` stores the link under the committer's signature, losing the original author and proof (`perspective_instance.rs:6828`). The page states it as a trap. It looks like a bug worth an issue; no SDK path hits it today (`loadSnapshot` adds one by one without a batch, `PerspectiveProxy.ts:992-994`), but an app that batches a hand-over would rewrite authorship. Coordinator: confirm you want it stated, or file it and soften.

---

### 4. how-to/define-a-model.mdx (VERIFY, written by the killed agent)

**Sources (of the original author):** model-classes.mdx Quick Start, SHACL/JSON Schema options; concepts/models-and-identity.mdx.

**Fixed in place:**
- Callout "numbers as `xsd:decimal`" was WRONG: the link target is `literal:number:N` (`rust-executor/src/perspectives/perspective_instance.rs:5547-5555`), indexed as `xsd:integer` for whole numbers and `xsd:decimal` otherwise (`rust-executor/src/perspectives/sparql_store.rs:54-66`); only the shape's `sh:datatype` is `xsd://decimal`. Reworded.
- Namespace rule: first property *or flag* `through`, relation fallback (`core/src/model/shacl-gen.ts:60-86`; flags are in the property registry, `decorators.ts:569-571`). Shape-URI table row said "first `through` is `board://title`"; in the sample it is the flag's `board://type`. Fixed.
- "an array of strings becomes a relation" → "an array property" (`core/src/model/json-schema.ts:294-299, 392-398`).
- Raw SHACL sample has no flag/minCount; added one sentence that it is recognized only by the structural fallback (`sparql_builder.rs:968-992`, tier 4).

**Verified (unchanged):** named exports (`core/src/index.ts:33,49`; `decorators.ts:545,706,807,1109,1242`); `registerAll` = one `addSdnaBatch` RPC (`Ad4mModel.ts:2418-2423`, `PerspectiveProxy.ts:1204-1239`); second register skipped per proxy, keyed by class name (`PerspectiveProxy.ts:2025-2045`, `:1218, 1236-1238`); `addComments(instance)` (`decorators.ts:1110, 1274-1276`, `Ad4mModel.ts:1662-1666`); include returns instances (`tests/js/tests/model/model-query.test.ts:708-719`); `fromJSONSchema(schema, { name, namespace })`, empty name throws "options.name is required and cannot be empty" (`Ad4mModel.ts:2543-2548`, `json-schema.ts:47-56, 279-281`); `xsd://string`/`xsd://decimal`, `minCount 1` (`json-schema.ts:238-247, 289`, `shacl-gen.ts:176-194`); `SHACLShape(targetClass)` / `(shapeUri, targetClass)` (`core/src/shacl/SHACLShape.ts:321-336`); `addProperty` fields incl. `relationKind` (`SHACLShape.ts:140-240`); `addShacl` = `addSdna(name, '', 'subject_class', json)` (`PerspectiveProxy.ts:1196, 1267-1269`); `createSubject(class, id, values?, batchId?)` (`:1536-1541`); `modelQuery` → `{ instances, totalCount }` (`:686`); `toJSON` (`SHACLShape.ts:847-893`); MCP `add_model(class_name, shacl_json)` (`rust-executor/src/mcp/tools/perspectives.rs:65-67`); shape-URI suffix rule and `NoteShape` error (`shacl_parser.rs:1622-1637`, tests :2410, :2436-2446); `subjectClasses()`, `getClassShape()` (`PerspectiveProxy.ts:1457, 2085`); `ad4m://obj/<24>` (`Ad4mModel.ts:762`).

**Claims removed:** none.

**Open questions:** none. (Side note, code not docs: the JSDoc at `decorators.ts:1102-1107` shows a singular `addComment`; the generated name is `addComments`.)

---

### 5. how-to/write-through-abstract-class.mdx (VERIFY)

**Fixed in place:** the quoted `implements` error used `'text'`; the code emits backticks: ``Message implements Readable but has no property or alias named `text` `` (`core/src/model/decorators.ts:640`; test `core/src/model/abstract-classes.test.ts:77`).

**Verified (unchanged):** `abstract: true`, `implements` (`decorators.ts:582-603, 709-713`); `alias: { Readable: "text" }` (`decorators.ts:415, 631-632`); executor matches by names regardless of `implements` (`rust-executor/src/perspectives/model_query/abstract_classes.rs:8-25`); abstract `through` never written (`tests/js/tests/model/model-abstract.test.ts:140, 157`); `__subjectClass`/`__subjectClasses` (`model_query/union.rs:340-343`, `Ad4mModel.ts:52-56`); `save()` dispatch (`Ad4mModel.ts:1796-1799`, `perspective_instance/abstract_classes.rs:290-417`, test `model-abstract.test.ts:133-140`); relation methods through a row (`Ad4mModel.ts:1672-1681`, test :147-159); `updateSubject`/`removeSubject` with a class-name string (`PerspectiveProxy.ts:1588, 1649-1665`); `delete()` runs own destructor (`Ad4mModel.ts:2181-2191`, test :142-144); create throws with the quoted text (`Ad4mModel.ts:1814-1821`, `perspective_instance/abstract_classes.rs:243-251`, test `abstract-classes.test.ts:127`); the three executor error strings in "What fails" (`perspective_instance/abstract_classes.rs:268-272, 322-326, 392-397, 800-806`); non-abstract `implements` throws (`decorators.ts:619-624`, test :119); callout: no value conversion, write goes to every conforming class (`perspective_instance/abstract_classes.rs:9-10, 253-279, 337-358`); MCP `instance_update`/`instance_remove` through an abstract class (`rust-executor/src/mcp/tools/instances/update.rs:63-69, 143-149`).

**Claims removed:** none. **Unverified by a test but kept:** `where: { text: { contains: "@data" } }` on the abstract class; only equality is tested (`model-abstract.test.ts:130`), `contains` is a general operator (`types.ts:36`). Coordinator may swap the sample to equality if strict.

**Open questions:** none.

---

### 6. how-to/paginate.mdx (VERIFY)

**Fixed in place:** added that the paged plan (and so a dotted or `$` order) also needs a `where` the store can evaluate whole; a bare `author`, `timestamp`, a getter property or an unexpressible operator sends the query to the fallback sort (`rust-executor/src/perspectives/model_query/query.rs:563-564` `can_push_pagination = all_where_pushable && order_fully_pushable`; `sparql_builder.rs:1299-1302` bare `author` "matched after hydration").

**Verified (unchanged):** imports (`core/src/index.ts:33`, `decorators.ts:545, 807, 1109, 1313`); `HasManyMethods<"pinned">` on a `@HasOne` is the pattern the tests use (`decorators.ts:1374-1382`, `tests/js/tests/model/models.ts:94-96`; there is no `HasOneMethods`); `paginate(perspective, pageSize, pageNumber, query?)`, 1-based, `{ results, totalCount, pageSize, pageNumber }`, offset formula (`Ad4mModel.ts:1569-1592`, `types.ts:770`); `findAllAndCount` (`Ad4mModel.ts:1554-1562`), total ignores window (`query.rs:868-888`); `count` sends `limit: 0` (`Ad4mModel.ts:1620-1628`); builder `.paginate` (`ModelQueryBuilder.ts:589-593`); default order timestamp ASC (`query.rs:632-634, 924-928`); dotted order needs a window and a single key (`query.rs:505-562, 622-626, 660-686`; test `model-query.test.ts:475-511`); `$count` order (`query.rs:527-539`; test :434-473); `TypedOrder` rejects dotted keys (`types.ts:660-667`).

**Claims removed:** none.

---

### 7. how-to/subscribe.mdx (VERIFY)

**Fixed in place:** `countSubscribe`/`paginateSubscribe` "call back on every update" made concrete: also on a reconnect or resync with the same result (`core/src/perspectives/LiveQuery.ts:91-93, 136-143, 172-188` — `#replace` calls `onResult` once started), and `paginateSubscribe` on any change to the whole set (`ModelQueryBuilder.ts:641-653, 667-690`).

**Verified (unchanged):** builder methods (`ModelQueryBuilder.ts:125, 141, 222, 376`); `subscribe` returns initial rows, callback not called for them (`ModelQueryBuilder.ts:467-497`, `LiveQuery.ts:66-67, 141-142`); dedupe by JSON fingerprint for `subscribe` only (`ModelQueryBuilder.ts:472-489`); executor sends nothing for an unchanged re-run (`perspective_instance.rs:6266`); delta apply (`LiveQuery.ts:36-46`, `perspective_instance/subscriptions.rs:143-161`); triggers incl. include edits, `some`/`none`, `links`, unrelated class not waking (`perspective_instance/subscription_triggers.rs:414-421, 595-622`; `subscription_triggers/tests.rs:182-206, 345-362, 366-422, 657-681`; `tests/js/tests/model/model-subscriptions.test.ts:407, 500, 553`); abstract-class subscription picks up later classes (`perspective_instance_abstract_tests.rs:528-582`); one subscription per builder (`ModelQueryBuilder.ts:42, 468, 561, 639`); `dispose` both sides (`ModelQueryBuilder.ts:81-86`, `LiveQuery.ts:127-133`); connection close ends subscriptions (`rust-executor/src/api/ws_rpc.rs:453-458`); reconnect test `model-subscriptions.test.ts:219`; `useLiveQuery` in `@coasys/ad4m-react-hooks` / `@coasys/ad4m-vue-hooks` (`ad4m-hooks/react/package.json:2`, `ad4m-hooks/react/src/index.ts:6,14`, `ad4m-hooks/vue/src/index.ts:5`).

**Claims removed:** none. Edge case not added: an update arriving while `subscribe` is still opening is delivered after the snapshot via the buffer (`LiveQuery.ts:136-143`); too fine for a how-to.

---

### 8. how-to/include-relations.mdx (VERIFY)

**Fixed in place:**
- `{ from, limit: 1 }` "one id or `null`" was WRONG for a typed relation: the SDK tags `targetClassName` (`core/src/model/Ad4mModel.ts:173-188`) and the executor replaces ids by hydrated objects (`rust-executor/src/perspectives/model_query/projection.rs:304-360`). Now "the first member or `null`", plain objects when the relation has a target class.
- Sub-query `limit` caps the total across all parent rows, not per row: one sub-query over the union of target ids (`model_query/relations.rs:440-495`, then per-row split :507-533). Added to step 3.
- To-one relation gives `null` (not an empty list) for a non-conforming target and past the depth limit (`relations.rs:535-536`). Added to both sentences.
- Polymorphic include refuses `limit`/`offset` (`relations.rs:182-210`). Added.

**Verified (unchanged):** decorator exports (`decorators.ts:1109, 1242, 1313, 1413, 1477`); generated `addPinned`/`addReactions` (`decorators.ts:1275-1283, 1374-1382`); bare relations are ids (`model-query.test.ts:721-738`); include one level / nested / reverse (`model-query.test.ts:740-764, 872, 922`); non-conforming targets dropped (`relations.rs:530-533`); inherit `linkStatus`/`includeUnverified` (`relations.rs:128-164`); `new Post(perspective, id)` (`Ad4mModel.ts:752`); `instance.get({ comments: true })` (`Ad4mModel.ts:2149-2161`); `MAX_INCLUDE_DEPTH = 8` with warning (`model_query/utils.rs:188`, `query.rs:374-384`); `polymorphic`/`instantiateAs` (`decorators.ts:1014, 1040`; `tests/js/tests/model/model-polymorphic.test.ts:263, 281-302, 476-496`); untyped include error names the relation (`relations.rs:474-485`); projection `order` honours only `id`, issue #1417 open (`projection.rs:656-683, 759-771`).

**Claims removed:** none.

---

### Cross-page findings for the coordinator (pages not in my list)

1. `concepts/queries.mdx:130` shows `$latest: { from: "comments", order: { createdAt: "DESC" }, limit: 1 }` and :136 says "one id or null (limit 1 unwraps)". Both are wrong at this commit: projection `order` honours only `id` (#1417, `projection.rs:656-683`), and a typed `from` returns a hydrated plain object, not an id (`projection.rs:304-360`). `reference/model-query.mdx:82` ("`1` unwraps the value to a single id or `null`") has the same id/object error. The SDK's own JSDoc at `core/src/model/types.ts:338` is also stale.
2. `concepts/queries.mdx:118` / `reference/model-query.mdx:71`: an include sub-query's `limit`/`offset` apply to all parent rows together, not per row (`relations.rs:440-533`). Worth one clause on the reference row.
3. `reference/model-query.mdx:19`: "`[]` throws" holds only for `findAll`/`findOne` (`Ad4mModel.ts:1498`).
4. `concepts/perspectives-and-links.mdx:80` and `reference/model-query.mdx:159-173` on batches agree with the code; no change needed.

---

## Checklist: entrances (verification pass, 2026-10-22)

Agent: `entrances` (third run; the first two were killed before writing a checklist). Worktree `/home/data/code/ad4m-wt/docs-overhaul` at WIP commit 2f612ec72 on origin/dev 13e1c50b8. Nothing committed. Paths below are repo-relative.

Global checks, all 16 pages:
- `node docs-src/scripts/check-portability.mjs`: no problem reported on any of the 16 pages (links, anchors, imports, frontmatter). The remaining failures are old pages (`spanning-layer.mdx` etc.) the coordinator deletes.
- Frontmatter `title` + `description` present on all 16; exactly one `# H1` per page and it equals `title` (scripted check).
- Audit (planning/docs-overhaul-audit-2026-10-21.md) grep over the 16 pages for `12001`, `Node.js 18`, `useClient`, `import Ad4mConnect`, `can: ["read"]`, `dynamicClassTools` (as config key), `run-local-hc-services`, `perspectives create`, `getSubjectProxy`, `"Whisper"`: none left (the one `useClient` hit is the sentence saying it does not exist).

---

### concepts/agents.mdx (written by the dead agents, not verified before; full pass)

Sources: rust-executor/src/mcp/tools/*, core/src/perspectives/WakerSubscriptionManager.ts, PerspectiveProxy.ts, AutoProcessor.ts, TextContent.ts, Container.ts, plugins/ad4m/*.

Audit items fixed on this page:
- "ai: MISSING — `runInterpretation`, `runInterpretationWithHarness`, `addAutoProcessor`, `removeAutoProcessor`. `PerspectiveProxy.ts:363-463`" — section "Interpretation inside the executor".
- "MCP: `language_meta`, the seven flow tools, `--mcp-host`, TLS listener." — pointed to mcp-tools.mdx; flows mentioned as votable over MCP.

Claims removed: none needed. No edits made: every claim checked out.

Verified:
- `get_documentation` docs compiled in: `include_str!` in mcp/tools/docs.rs:17-21; topics enum docs.rs:26.
- `list_perspectives`, `describe_perspective`, `instance_transcript`, `get_my_did`, `instance_create(parent)`: router mcp/tools/mod.rs:313-419; params create.rs:17-33, transcript.rs:31-39.
- validation names property/type/cardinality: create.rs:38-55 (`validate_properties`), validate.rs.
- `TextContent` = `ad4m://TextContent`, property `text`, alias example: core/src/perspectives/TextContent.ts:13-34.
- `Container` = `ad4m://Container`, has_child + own children relation: core/src/perspectives/Container.ts:5-41.
- `generate_waker_query(parent)` → model query over Container in `query_class`: mcp/tools/subscriptions.rs:92-130; `get_mention_waker_config` returns `query`, whole word, case-insensitive, skips own links: subscriptions.rs:195-290 (`"query": query`).
- `WakerSubscriptionManager` options `perspectiveClient`, `QuerySubscriptionProxy`, `logger`, `onWake(sub, result, mentions)`, debounce 2000, `ancestorDepth` 3, `retryPendingMs` 30000, `getPending()`: WakerSubscriptionManager.ts:55-88, 171-177, 429. `subscribe({id,type:"mention",perspective,channel,query})`: :10-24, :197. `QuerySubscriptionProxy` export: PerspectiveProxy.ts:57 (re-exported by core/src/index.ts:18). Plugin re-export: plugins/ad4m/wakerSubscriptionManager.ts.
- plugin auto-subscribes on join/publish: plugins/ad4m/index.ts:404-457, 792.
- Bearer header: mcp/tools/mod.rs:73-88; `Perspective not found: <id>` on denial: mod.rs:619-644.
- `runInterpretation(transcript, basePrefix, classes?)`: PerspectiveProxy.ts:363; `TranscriptTurn {speaker,text}`: core/src/generated/api/TranscriptTurn.ts:6. `addAutoProcessor` config fields `processorId`, `sourceScopeQuery` (`SELECT ?speaker ?text ?timestamp`), `interpretationClasses`, `debounceMs`, `batchMax`, `claimTtlMs`: AutoProcessor.ts:132-192. `removeAutoProcessor` stops for everyone (Shared config): PerspectiveProxy.ts:446-463. `interpretationOverlays`/`acceptInterpretation`/`rejectInterpretation(base, property?)`: :478-518. `runInterpretationWithHarness(transcript, basePrefix, maxToolCalls, ...)`: :408.
- Ports 3001 / 12000: rust-executor/src/lib.rs:807, config.rs:450.

Open questions: none.

### reference/mcp-tools.mdx (not verified before; full pass)

Sources: rust-executor/src/mcp/tools/**, plugins/ad4m/staticToolDefs.ts, plugins/ad4m/index.ts, cli/src/run_config.rs.

Audit items fixed:
- "STALE — static list at 40 omits `instance_remove_from_collection`, `instance_transcript`, `add_child`, `get_children`, `execute_commands`. `mcp/tools/mod.rs:469-480`" — all 44 listed.
- "WRONG — `dynamicClassTools: true` in the config file (48) — only `--dynamic-class-tools` / `AD4M_DYNAMIC_CLASS_TOOLS`." — "there is no config-file key".
- "MISSING — `language_meta`" — row present.
- "MISSING — MCP flow tools `add_flow`, `get_flows`, `flow_state`, `flow_actions`, `flow_valid_outputs`; and the fact that MCP cannot propose" — Flows table + sentence.
- "STALE — `maxCount 1` 'generates `set_` tool' ... per-class MCP tools are opt-in" — Per-class section.

Fixes made:
- Auth column note: added that on an executor without an admin credential an unauthenticated caller passes (`check_auth_with_header` step 4, mod.rs:258-262). The page said every non-auth tool refuses until authenticated.
- Per-class names: property and collection names are lowercased too, and abstract classes get only read tools (dynamic.rs:50-70, 197-261).

Verified, tool by tool:
- Count 44: router mod.rs:313-419 (44 `with_route`); every `#[tool]` fn enumerated by grep, 44 names, all on the router.
- AUTH_TOOLS (8 "none" rows): mod.rs:126-135. `Authentication required. ...` text: mod.rs:188.
- Params, all from the `*Params` structs: perspectives.rs:23-67; instances/create.rs:17-33, query.rs:14-45 (limit default 100), update.rs:16-36, collections.rs:18-43, transcript.rs:31-39 (default 50, max 500), children.rs:24-46 (default 100, max 500), commands.rs:19-27, describe.rs:19-21; flows.rs:24-69; profiles.rs:23-62 (`mime_type` default image/png at :352); neighbourhoods.rs:30-52 (`name` default via `default_neighbourhood_name`, alias `link_language_template`); languages.rs:17-19; subscriptions.rs:22-48 (`parent` alias `parent_address`); auth.rs:21-81; docs.rs:69-72.
- `list_perspectives` fields `uuid,name,shared_url,has_neighbourhood`: perspectives.rs:169-174.
- `request_capability` asks ALL_CAPABILITY, code only when caller may permit: auth.rs:160-185; `caller_can_permit` mod.rs:539-544.
- `auth_status` reports token + `executor_locked`: auth.rs:310-398.
- `execute_commands` has no `validate_properties` call: commands.rs:30-75.
- `generate_waker_query` non-parent path SPARQL over class predicates or `predicate`/`target_value`: subscriptions.rs:115-170.
- Hidden per-class tool error text: mod.rs:471-487. Harness always sees dynamic tools: harness_bridge.rs:42-55.
- `--dynamic-class-tools` / `AD4M_DYNAMIC_CLASS_TOOLS`: cli/src/run_config.rs:236-244; no key in `ExecutorConfigFile` (config_file.rs:176-225).
- OpenClaw column: the 26 names in plugins/ad4m/staticToolDefs.ts (parsed with node) match the 26 "yes" rows exactly. Plugin-local tools: index.ts:703 (`ad4m_get_sample_config`), 809, 839, 878, 961, 1008, 1039; params `perspective_id` / `expression_address` / `file_path` there.
- Callout `Perspective not found`: mod.rs:619-644.

Open questions: none.

### how-to/troubleshooting.mdx (not verified before; full pass)

Every quoted error string was grepped verbatim:
- `Refusing to start: no admin credential ...`: rust-executor/src/config.rs:380-385 (`NO_ADMIN_CREDENTIAL_ERROR`), raised at :320-325.
- `AD4M_ADMIN_CREDENTIAL_FILE`: config_file.rs:272-290; file mode check config_file.rs:~340 (`mode & 0o077`), one trailing newline dropped.
- `REST API server failed, exiting:` + exit 1: lib.rs:905-915; `bind_api_listeners` api/mod.rs:161.
- `API HTTPS listener on port {tls_port} could not start`: api/mod.rs:187.
- `MCP server error`: lib.rs:819. `MCP: could not bind the HTTPS MCP listener on {addr}`: mcp/server.rs:380.
- `Executor is locked: the wallet has not been unlocked since the last restart. ...`: agent/capabilities/token.rs:22, :69. `... ask the operator to unlock it, then reconnect`: agent/session.rs:90. `... its admin has not unlocked the agent yet ...`: user_management.rs:31. `executor_locked`: mcp/tools/auth.rs:369-389.
- `ad4m agent unlock --passphrase`: cli/src/agent.rs:15-22; `is_unlocked` print :75. `agent.unlock(passphrase)`: core/src/agent/AgentClient.ts:84. `hintFor`: WakerSubscriptionManager.ts:~95.
- `Capability is not matched, you have capabilities: ..., expected: ...`: capabilities/mod.rs:117 (`require_capability` :111); `Capability {with, can}` Debug shape types.rs:49-68.
- `Unauthorized access` / `Unauthorized access: the token has expired`: session.rs:81-82; also capabilities/mod.rs:142. `revokeToken(requestId)`: AgentClient.ts:253.
- `Language not created by trusted agent: ...`: languages/mod.rs:1863; trust rule (own DID or trusted list; template whose source author is trusted) :1778-1782, :1888-1912. `addTrustedAgents`: core/src/runtime/RuntimeClient.ts:29.
- `Holochain conductor not available`: holochain_service/holochain_service_extension.rs:223 (+5 sites); conductor start on generate/unlock gated by `run_holochain`: api/agent_ws.rs:420, :537; `still starting after 120s`: holochain_service/interface.rs:491.
- Multi-user table: user_management.rs:308/321/332/352 (`Multi-user mode is not enabled`), :208-229 (`Invalid credentials`), :115/:125 (`User already exists`), :357 (`Invalid verification code`), `check_executor_unlocked` :28.
- MCP table: mod.rs:188, :471-487, :589-639; `resolve_host` server.rs:142-166.
- Docs build table: docs-src/scripts/check-portability.mjs:22, :42/:45, :72, :76 (extension), :81, anchor message; `check:portability` runs `copy-generated.mjs` first: docs-src/package.json:10, :12.
- `RUST_LOG` respected: rust-executor/src/logging.rs:98-124.

Fixes made:
- MCP row "`auth_status` says `authenticated: false` while calls succeed": the old fix text ("the session stores the credential on the first protected call") is stale: the auth tools now adopt a header credential too (mod.rs:180-185). The real cause of `authenticated: false` with working calls is a session holding the admin credential, which `decode_jwt` cannot decode, so `auth_status` reports `Token set but invalid - could not decode` (auth.rs:310-325, :376-384; token.rs:59-84). Row rewritten to that.
- Multi-user row sources: added `signup_user`, `request_login_code` (user_management.rs:316-335).
- Docs-build table: added the `page link must carry the .mdx/.md extension` message (check-portability.mjs:76).

Open questions:
- The `auth_status`-with-admin-credential answer (`authenticated: false`, "Token set but invalid") looks like a code bug worth an issue: an operator-authenticated session reads as unauthenticated and the plugin's setup consults `auth_status` (plugins/ad4m/setup.ts:466). Docs now describe current behaviour.
- `NO_ADMIN_CREDENTIAL_ERROR` tells the user to set "`adminCredential` in the config", but the config file refuses that key (config_file.rs:230-235). executor-config.mdx already carries a warning; a one-line code fix would remove it.

### index.mdx (lighter pass)

- Ports RPC 12000 / MCP 3001: config.rs:450, lib.rs:807. Links: all to existing new-tree files (portability check). No API calls. Audit "index: STALE — 'RuntimeConfig JSON' (52)": not present. No changes.

### start/build-an-app.mdx (lighter pass)

Audit items fixed (already on the page, confirmed):
- "installation: WRONG — 'Node.js 18+' (95); `>=22`. `core/package.json:35`. STALE — 'v0.10.1 RC (March 2025)' (5); 0.13.1-staging.2." — core/package.json:35, :69; ui/package.json:85.
- "WRONG — `import Ad4mConnect from "@coasys/ad4m-connect"` — no default export; `getAd4mClient`/`getAd4mConnect({ appInfo, capabilities })`" — connect/src/index.ts:18, :54, :67, :71 (`isEmbedded`).
- "WRONG — `can: ["read"]`, `domain: "perspective-uuid"` — verbs uppercase, domains fixed names, UUID in `pointers`" — defs.rs:5-27.
- "auth: WRONG — `new Ad4mClient({adminCredential})` (153); signature `(baseUrl, token?, options?)`" — Ad4mClient.ts:35-39.

Fixes made:
- Model sample: `status: string = ""` → `status!: string;`. `create` saves every non-undefined field (Ad4mModel.ts:1836-1855), so the `""` default would have been written instead of `initial: "task://open"` and the following `.where({ status: "task://open" })` query would not match. Tests declare `initial` properties the same way (tests/js/tests/shacl-rpc.ts:33-34, prolog-and-literals.test.ts:141-143). Added one sentence saying so.
- "Remote node" → "Remote Node" (connect/src/components/views/ConnectionOptions.ts:175).

Verified: `Ad4mConnectOptions {appInfo{name,description,url,iconPath?}, capabilities}` connect/src/types.ts:3-31; `getAd4mConnect` returns `{core, client}` index.ts:18; `connectAsGuest(options, hostUrl)` index.ts:54; `agent.requestCapability`/`generateJwt` + `localStorage` ("ad4m-token") connect/src/core.ts:605-630; six digits capabilities/mod.rs:454-457; `agent.me()` AgentClient.ts:52; `perspective.add(name)` PerspectiveClient.ts:170; `@Model`, `@Property({through, initial})` decorators.ts:290-299; `register` Ad4mModel.ts:2398; `create(perspective, data)` :2273; `ad4m://obj/<id>` :762; builder `where/order/limit/get/first/count/subscribe/dispose` ModelQueryBuilder.ts:81, 125-173, 398-560; order values `ASC|DESC` model/types.ts:660-667; Launcher port search 12000-13000 ui/src-tauri/src/lib.rs:224.

Open questions: none.

### start/connect-an-agent.mdx (lighter pass)

Fixes made:
- "`auth_status` ... answers `authenticated: false` until the first protected call carries a matching header" → stale (auth tools adopt the header, mod.rs:180-185); replaced with the admin-credential behaviour (see troubleshooting).
- `list_perspectives` returns `shared_url`/`has_neighbourhood`, not a `neighbourhood` field (perspectives.rs:169-174).
- Wakers: "links matching a SPARQL subscription" → mentions are SPARQL, children are a model query over `Container` (subscriptions.rs:92-130; WakerSubscriptionManager.ts:15-22).
- "Claude (Desktop or Code)" → "Claude Code": the repo has no evidence for a Desktop HTTP `mcpServers` entry; dropped Desktop.

Verified: `--enable-mcp`, `--mcp-port` run_config.rs:222-225; bind rules server.rs:142-166, `MCP_HOST` read only from env server.rs:238 (no CLI flag: no `mcp_host` anywhere in cli/src); IP-literal parse server.rs:270 (`SocketAddr` parse); TLS on +1 only with credential server.rs:180-200; Launcher MCP toggle/port 3001 ui/src/components/Settings.tsx:94-97; describe output keys `name,class_uri,properties,collections`, `classes`, `flows` instances/describe.rs:77-80, :175-180, property keys instances/mod.rs:236-240.

Audit items: "STALE — `AD4M_ADMIN_CREDENTIAL` 'required' (29)" and Lal's loopback wording ("defaults to loopback without a credential, an explicit host overrides it and presented credentials are still checked") — both reflected.

### start/run-an-executor.mdx (lighter pass)

Fixes made: "Precedence: file, then variable, then flag" → "A flag beats its variable, and a variable beats the file" (unambiguous; run_config.rs `overlay`, ad4m_executor.rs:128).

Verified: subcommands `init/run/config print/log/dev` cli/src/ad4m_executor.rs:109-125; `init -d/--data-path` :115-119; `--app-data-path` run_config.rs:81-88; defaults `localhost` true config.rs:307, `run_dapp_server` true :280-282 on 8080 lib.rs:663, `run_holochain` true config.rs:~290, auto-permit true without `--config` run_config.rs:363-364; TLS listener 0.0.0.0 and plain narrows api/mod.rs:161-182; `AD4M_UNLOCK_PASSPHRASE_FILE` config_file.rs:274, lib.rs:225; data paths `ad4m_db.sqlite` lib.rs:478, `ad4m/h` + `ad4m/languages` lib.rs:445-453 + init.rs:89-90, `mainnet_seed.seed` init.rs:66, `executor-port` + `ad4m.log` ui/src-tauri/src/config.rs:16-44, `launcher-state.json` ui/src-tauri/src/app_state.rs:13, `~/.ad4m-cli/cap_token` cli/src/startup.rs:11, :52; Launcher fresh credential each start ui/src-tauri/src/lib.rs:247 (Uuid); hc ports from 2000/1337 lib.rs:610-611.

Audit items: "cli: WRONG — `--run-dapp-server` default false (55); default true" and "cli: WRONG — hc admin/app ports 1234/8888 (65-66)" — correct here.

### concepts/what-ad4m-is.mdx (lighter pass)

Fixes made:
- "a write through a model or an `instance_*` tool is validated against the shape" — only the MCP path validates (instances/create.rs:55 `validate_properties`); I found no shape validation on the SDK `createSubject` path. Reworded: instance_* writes are validated, a model writes the links its class declares.
- System languages list made exact from rust-executor/src/mainnet_seed.json keys (agent, perspective, neighbourhood, language-language bundle, known link languages).

Verified: `perspective.all()` PerspectiveClient.ts:62; `new LinkQuery({source})` LinkQuery.ts:2-30; `get()` returns `LinkExpression[]` PerspectiveProxy.ts:344; `author/timestamp/data/proof` core/src/links/Links.ts:44-75.
Audit: "spanning-layer: LIKELY WRONG — 'SDNA enforced by all members'" — page states social DNA does not reject incoming links. "index: ... `get()` comments show bare links where `LinkExpression`s return" — fixed.

### concepts/agent-executor-capabilities.mdx (lighter pass)

Fixes made:
- Multi-user default set: "everything above except ... `runtime.user_management` `READ` on `*`" was wrong: `get_user_default_capabilities` contains nothing from `runtime.user_management` (defs.rs:505-567). Rewritten, plus the anonymous set from `anonymous_capabilities` (capabilities/mod.rs:300-318).
- Auto-permit code logging: "otherwise it logs where to find it" → a debug line says to set `AD4M_LOG_SECRETS` (api/agent_ws.rs:661-679).

Verified: domain/verb table against every `pub static ref` in defs.rs (scripted dump; matches row by row, including `runtime.friend_status`, `runtime.my_status`, `runtime.hc_agent_info`, `runtime.exception`, user_management `READ` pointer `enabled` :460); `Agent {did, perspective, directMessageLanguage}` core/src/agent/Agent.ts:26-36; `byDID` AgentClient.ts:89, `updatePublicPerspective` :162, `generate` :69, `unlock` :84, `requestCapability` :233, `permitCapability` :237, `generateJwt` :241, `getApps` :245, `removeApp` :249, `revokeToken` :253, `createUser` :266, `loginUser` :270; `AuthInfoInput` fields Agent.ts:187-201; `Apps {requestId, token, revoked?, auth}` Agent.ts:152-161; `expression.create(content, languageAddress)` ExpressionClient.ts:165; `friendSendMessage(did, Perspective)` RuntimeClient.ts:95; token on WS URL core/src/apiClient.ts:150-151; `/api/v1/ws` rust-executor/src/api/mod.rs:3; Launcher calls `permitCapability` ui/src/components/Auth.tsx:33.

Audit items fixed: "auth 141-144 ... `can: ["read"]`", "auth: WRONG — `new Ad4mClient({adminCredential})`", "auth: MISSING — `AD4M_ADMIN_CREDENTIAL_FILE`, `--auto-permit-cap-requests`, `getApps`/`removeApp`/`revokeToken`", "agents: WRONG — `expression.create(languageAddress, content)` (64) → `(content, lang)`", "agents: WRONG — `runtime.sendMessage(did, "Hello!")` (73) → `friendSendMessage(did, Perspective)`", "multi-user: WRONG — `requestCapabilityForUser` / `generateJwtForUser`" (page uses `createUser`/`loginUser` only).

### how-to/mcp-server.mdx (lighter pass)

Fixes made:
- Header auth: "first protected call ... stores it" → "first call with a valid header" (mod.rs:180-185, :279-310).
- Capability row: dropped "or the executor's stdout with `AD4M_LOG_SECRETS=1`". For an MCP request that is not auto-permitted the executor only publishes the request event (capabilities/mod.rs:348-376); no code is printed. The operator approves in the Launcher.
- Locked executor: "fail with a 'key not found' error" → `Executor is locked: ...` (token.rs:22; user_management.rs:28-34, :193), with a link to troubleshooting.
- auth_status paragraph rewritten (admin-credential session reads `authenticated: false`).
- Owner table: "a SPARQL subscription" → SPARQL for mentions, model query over Container for children.

Verified: bind table server.rs:142-200 (`resolve_host`, `resolve_plain_host`), TLS listener 0.0.0.0 server.rs:370; config-file `mcp_enabled`/`mcp_port` config_file.rs:205-207 + run_config.rs:353-354; eight AUTH_TOOLS mod.rs:126-135; `--dynamic-class-tools` run_config.rs:236-244.
Audit items: all five mcp.mdx lines (dynamicClassTools key, topics, credential "required", static list, MISSING language_meta/MCP_HOST/loopback rule/TLS +1).

Open question: the audit says "`--mcp-host`/`MCP_HOST`". There is no `--mcp-host` CLI flag (no `mcp_host` in cli/src; lib.rs:806-812 leaves `McpServerConfig.host` at its default `None`). The pages say "no `--mcp-host` flag". But a server.rs:412-417 log message and server.rs:180 doc text still mention `--mcp-host`. Worth a code-side cleanup or a real flag.

### how-to/connect-an-agent-framework.mdx (lighter pass)

Fixes made:
- External mode: "you approve the request in the Launcher and enter the six-digit code" is not what setup does. Setup calls `request_capability`; with an inline code it calls `generate_jwt` and prints the JWT config; otherwise it explains the cause and prints a placeholder (plugins/ad4m/setup.ts:425-475). Added the `multiUser` sign-up path (setup.ts:340-422).
- Waker description: SPARQL vs model query, as above.
- Callout about header-only clients: rewritten (the header is adopted on the first call; `auth_status` reports only a JWT).
- "Claude Desktop and Claude Code read an `mcpServers` map" → Claude Code (`.mcp.json`) only (Desktop unverifiable from the repo).

Verified: package `@coasys/openclaw-ad4m` plugins/ad4m/package.json:2; `openclaw ad4m-setup` index.ts:1511-1524; config keys `mode, mcpEndpoint, allowInsecureHttp, token, agentPassphrase, executorUrl, wakeUrl, debounceMs, multiUser, email, password` plugins/ad4m/types.ts:18-53; managed mode spawns + generates + unlocks executor.ts:522, agent.ts:84, :101.

### how-to/ai-models.mdx (lighter pass)

Fixes made:
- Pre-configured model table: added the kalosm presets (`llama_*`, `mistral_*`, `solar_*`), which `build_local_llama` also accepts (ai_service/mod.rs:640-690).
- `maxNumCtx` applies only to Ollama (ai_service/mod.rs:802-816).
- Added the cleartext-key refusal (`Refusing to send an API key over plain HTTP. Use https, or omit the key.`, ai_service/providers/http.rs:70-71; enforced mod.rs:792-799).
- Hosted-user refusal list: added `discoverModels`, which also needs `CREATE` (api/ai_ws.rs:94-98).

Verified: `AIClient` methods AIClient.ts:21-155; `ModelInput`, `LocalModelInput`, `ModelApiInput`, `TokenizerSourceInput` core/src/ai/AITypes.ts:26-62; apiType strings `OPEN_AI|ANTHROPIC|OLLAMA` rust-executor/src/types/core.rs:517-541; Anthropic `{base}/v1/messages` providers/anthropic.rs:1, :77; Ollama `/api/chat` providers/ollama.rs:84; local file path when no repo mod.rs:723-740; `cuda`/`metal` features rust-executor/Cargo.toml:30-33; `"default"` model id mod.rs:585, :1236; `embed("bert", ...)` tests/js/tests/ai.ts:546; `AIModelLoadingStatus.progress` core/src/ai/Tasks.ts:55-60; `AITask.taskId` Tasks.ts:34-40; capabilities per method api/ai_ws.rs:46-160 and :300-460.
Audit items fixed: "ai: STALE — 'OpenAI-compatible only' (125) — `ANTHROPIC`, `OLLAMA` exist; `discoverModels` undocumented."

### how-to/transcription.mdx (lighter pass)

No changes. Verified: `openTranscriptionStream(modelId, cb, params?)` + `transcription-text` event AIClient.ts:111-130; model ids ai_service/mod.rs:1945-1967; fallback chain (DB model → first TRANSCRIPTION model → Tiny) :1970-2002; params ignored (`_params`) :2004-2008 and Silero VAD :2076-2125; 16 kHz :2085; shared model per size :122, :2015; 30 s idle close :87 (`TRANSCRIPTION_TIMEOUT_SECS = 30`), :481-482; `feedTranscriptionStream(string|string[], Float32Array)` AIClient.ts:155; `TRANSCRIBE` api/ai_ws.rs:415-442; in hosted default set defs.rs:562.
Audit items fixed: "ai: WRONG — transcription model id `"Whisper"`", "ai: WRONG — VAD params 'configure' detection".

### how-to/hooks.mdx (lighter pass)

No changes. Verified: exports ad4m-hooks/react/src/index.ts:1-16, vue/src/index.ts; peers react/package.json:21-24, vue/package.json:18-20; `useLiveQuery` options/returns/load-more/`preserveReferences`/parent-empty/string model react/src/useLiveQuery.ts:36-270; `Scope` forms core/src/model/types.ts:114-134; `useMe` react/src/useMe.ts:19-80 (events `agent-status-changed`, `agent-updated`); `useAgent(props)` react/src/useAgent.ts:5-54; `usePerspectives` events + `sharedUrl` react/src/usePerspectives.ts:43-106; `usePerspective` `synced` never set react/src/usePerspective.ts:10-28; Vue `ComputedRef`/`shallowRef` vue/src/useLiveQuery.ts:79-143, `useAgent` positional + `did` added vue/src/useAgent.ts:6-30, `useMe` returns `{status, me, profile}` vue/src/useMe.ts:42; `toCustomElement(Component, propNames, options)` + `shadow` react/src/register.js:3-8.
Audit items fixed: "hooks: WRONG — `useClient` (5-31) does not exist", "hooks: MISSING — `preserveReferences`, `toCustomElement`".
Open question (code, not docs): react/src/useLiveQuery.ts imports a type `ParentScope` from `@coasys/ad4m`, which core no longer exports (the type is `Scope`, core/src/model/types.ts:114).

### reference/cli.mdx (lighter pass)

No changes. Verified: bins cli/Cargo.toml:16-22 (main.rs not a bin); `ad4m-executor` domains + `log` unhandled ad4m_executor.rs:109-200; dev subcommands dev.rs:9-20, writes `mainnet_seed.json` bootstrap_publish.rs:176; RunArgs flags, shorts (`-a`, `-n`, `-l`, `-p`, `-c`), envs, bare-able flags (`num_args = 0..=1` only on insecure + dynamic-class-tools) run_config.rs:76-311; blank-value errors :50-75; hc defaults config.rs:292-296, relay resolution holochain_service/mod.rs:1017-1027; `connect_holochain`/`log_holochain_metrics` unused outside config.rs (grep); seed path fixed lib.rs:534-538; SFU defaults run_config.rs:248-311 + `unwrap_or(8)` lib.rs:846; dapp 127.0.0.1:8080 dapp_server.rs:16; `ad4m` global flags + empty-token commands ad4m.rs:36-140; cap-token flow and prompt text startup.rs:49-101; agent/perspectives/neighbourhoods/languages/expression/runtime enums cli/src/agent.rs:7-60, perspectives.rs:22-100 (add-link `status` ignored :197-214, `_` filter :223-224, dna_type → rust-client/src/perspective_proxy.rs:109-115), neighbourhoods.rs:7-15, languages.rs:7-45, expression.rs:7-18, runtime.rs:7-50; REPL repl.rs:12-60, :85-235, :395, :487-620, :185-211, :686.
Audit items fixed: all five "cli:" lines and "executor-config: MISSING — `--sfu-local-did`, ...".

### reference/executor-config.mdx (lighter pass)

Fixes made:
- Secret-file permission: "Mode `0600` or `0400`" → no permission bits for group or others (`mode & 0o077 != 0` refused, config_file.rs:~340); kept the two examples.
- `INTERNAL_API_TOKEN`: added "required with a shared wallet or database backend" (lib.rs:364-398 `expect`).
- Flag-only list: added `--connect-holochain`, `--log-holochain-metrics` (not `--hc-*`, also no config key).

Verified: `ExecutorConfigFile` keys config_file.rs:176-225, `TlsSettings`/`SmtpSettings`/`MultiUserSettings` :144-175; inline-secret refusal :230-241; unknown key + empty value :420-449; TLS port `port + 1` :455-466; secrets table `from_env` :268-300; 39 flag variables (counted `env =` in RunArgs); `log_config` defaults logging.rs:55-58, `RUST_LOG` wins :98-103; env vars config.rs:220-240, memory_diagnostics.rs:195; token separation lib.rs:421-431; `Ad4mConfig` camelCase config.rs:73, :86; `config print` redaction run_config.rs:~440-455; Launcher `executor_config` ui/src-tauri/src/app_state.rs:341-380 (`localhost: !tls_enabled`), fresh credential ui/src-tauri/src/lib.rs:247, :328; unlock-at-startup outcomes lib.rs:225-252.
Audit: "multi-user: STALE — ... TLS/SMTP only as Launcher settings" — config-file keys documented here.

---

### For the coordinator

1. The `auth_status` answer for an admin-credential session (`authenticated: false`, "Token set but invalid - could not decode") is now documented as current behaviour on four pages (troubleshooting, mcp-server, connect-an-agent, connect-an-agent-framework). It reads like a bug; if someone fixes it before the docs PR merges, those four sentences must change.
2. There is no `--mcp-host` flag at 13e1c50b8 (only `MCP_HOST`), though the audit and two strings in mcp/server.rs name one. The pages say "no `--mcp-host` flag".
3. build-an-app's model sample changed `status: string = ""` to `status!: string;`. The old default would have overwritten `initial`. I inferred this from `Ad4mModel.ts:1836-1855` and the tests' declaration style; I did not run it.
