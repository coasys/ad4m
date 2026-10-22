# AD4M docs audit against `origin/dev` 13e1c50b8 (2026-10-21)

Companion to `docs-overhaul-proposal-2026-10-21.md`. Produced by a code audit (Opus subagent, two sub-audits) and spot-checked by Data: `rdf:Statement` never written (`sparql_store.rs:23,430` writes `rdf:reifies`), no `ad4m://initial` / `ad4m://readOnly` predicate anywhere in `rust-executor/src` or `core/src`, auto ids are `ad4m://obj/<id>` (`Ad4mModel.ts:738`), `@coasys/ad4m-connect` has no default export (`connect/src/index.ts:18,54,67`), `resolveLanguage` is not defaulted (`decorators.ts:312-324`), Node `>=22` (`core/package.json:35`), `getSubjectProxy` is not a `PerspectiveProxy` method.

Paths relative to the repo root. Format: `WRONG | STALE | MISSING — claim — evidence`.

## Headline

- GraphQL, Apollo, the old `executor/` package and `@perspect3vism/*` are gone from the docs. One `perspect3vism` leftover in `tutorial/2` and two in `theme.config.tsx`.
- `flows.mdx` and `writing-a-flow.mdx` are correct (derived from `flow-task-handover.test.ts`).
- `model-classes.mdx` is mostly current; its errors cluster in storage defaults, the SHACL examples, a few removed APIs, and large query features with no docs at all.
- Worst pages: `sparql-queries` (every query uses a pattern the executor's writer does not produce, and declares no prefixes, so they fail to parse or match nothing; Lal's wording), `cli` (invented subcommands, wrong argument order), `neighbourhoods` (an interface that no longer exists, `uuid` for `uid`), `social-dna` (`getSubjectProxy`, invented storage links).
- Lal (Astra) independently checked five items at 13e1c50b8: `resolveLanguage` default, the `rdf:Statement` pattern, the connect default export, MCP loopback, built-ins/containment. All confirmed; two wording corrections: the MCP server **defaults** to loopback without a credential, an explicit `--mcp-host` overrides it and presented credentials are still checked (`mcp/server.rs:145-159`, `mcp/tools/mod.rs:225-265`); unset `resolveLanguage` stores a typed literal while `"literal"` gives the value its own signed envelope, the link is signed either way (`core/src/model/query-sparql.ts:233-239`, `Ad4mModel.ts:1645-1655`).

## developer-guides/model-classes.mdx

- WRONG — `@Property` defaults to `resolveLanguage: "literal"` (229, 244, 1625) — not defaulted; unset = typed-literal storage, `"literal"` = signed envelope. `core/src/model/decorators.ts:312-324, 753-755, 809-813`
- WRONG — ids look like `"literal:..."` (712, 716) — auto ids are `ad4m://obj/<24 chars>`. `Ad4mModel.ts:752-762`
- WRONG — "legacy `.baseExpression` getter still works" (724) — no such getter. `Ad4mModel.ts:769`
- WRONG — `properties([...])`: "other fields stay at defaults" (924) — unrequested fields are removed, including author/createdAt; `properties: []` throws. `tests/js/tests/model/model-query.test.ts:1151-1181`
- WRONG — `getModelMetadata()` shape (1325-1327) — it is `{ className, properties: Record, relations: Record }`. `core/src/model/types.ts:854-861`
- WRONG — relation `getter` "mutually exclusive with `target`/`through`" (1374) — combines with `target` and `where`, and with `through` when `readOnly: true`. `decorators.ts:1156-1203, 895-930`
- WRONG — `fromJSONSchema` `name?` optional with a className cascade (1452, 1475, 1546) — `name` is required, empty throws. `core/src/model/json-schema.ts:47-48, 279-281`
- STALE — JSON Schema "at least one property must be required or have an initial value" (1558) — all-optional schemas work. `tests/js/tests/model/model-from-json-schema.test.ts:335`
- WRONG — `perspective.getClassOptions(...)` (1215-1217) — does not exist; `getClassShape`, `listRegisteredClasses` do. `PerspectiveProxy.ts:2062, 2085`
- WRONG — shape URIs `shacl://Recipe`, `recipe://Recipe` (88, 116, 587) — generated shapes are `${ns}${Name}Shape`, enforced. `core/src/model/shacl-gen.ts:93-95`; `rust-executor/src/perspectives/shacl_parser.rs:1630`
- WRONG — Turtle: `sh:datatype xsd:string` on a relation, `ad4m:resolveLanguage "literal"` on a plain property (594, 598) — relations get `sh:nodeKind sh:IRI`; datatypes are `xsd://string|decimal|boolean`; resolveLanguage emitted only when set. `shacl-gen.ts:176-188, 218, 305-310`
- WRONG — mapping table lists `ad4m://initial`, `ad4m://readOnly` (619, 621) — neither exists; `initial` goes into `ad4m://constructor` actions, read-only is `ad4m://writable`. Real predicates: writable, getter, conformanceConditions, constructor, identity, alias, abstract, ordering, transform. `shacl-gen.ts:270-287`
- STALE — `ad4m://getter` "for conformance filtering" (622) — carries any custom getter. `shacl-gen.ts:222-224`
- STALE — `maxCount 1` "generates `set_` tool", relations "generate `add_`/`remove_`" (613-614) — per-class MCP tools are opt-in. `cli/src/run_config.rs:235-244`
- STALE — Quick Start stores plain strings in `@HasMany ingredients` without `datatype` (22-32) — string-valued relations need `datatype: "xsd:string"` per the options docs. `decorators.ts:970-986` (inferred from code; confirm with a test)
- STALE — `resolveLanguage: "markdown"` (1632) — must be `"literal"` or a language address. `decorators.ts:313-323`
- STALE — "custom Prolog/SPARQL getter" (336) — `getter` is SPARQL; Prolog uses undocumented `prologGetter`/`prologSetter`. `decorators.ts:326-341`
- MISSING — property options `options` (sh:in), `datatype`, `alias`, `interpretationHint`, `identity`, `prologGetter`, `prologSetter`. `decorators.ts:358-415`
- MISSING — `@Model` option `interpretationHint`. `decorators.ts:582-612`
- MISSING — relation options `polymorphic` + `instantiateAs` on `@HasMany`, `datatype`, `readOnly`, `interpretationHint`, `alias`. `decorators.ts:931-1097`; `tests/js/tests/model/model-polymorphic.test.ts:235-476`
- MISSING — traversal parent scopes `parent: { ids, transitive, maxDepth, direction, limitPerAnchor, levels }`. `core/src/model/types.ts:144-286`; `tests/js/tests/model/bounded-traversal.test.ts:131-317`
- MISSING — containment scope `parent: { id }` and the built-in abstract classes `TextContent`, `Container` (SDK exports). `types.ts:107-142`; `core/src/index.ts:23-24`; `rust-executor/src/perspectives/model_query/builtin_classes.rs:1-35`; `tests/js/tests/model/model-containment.test.ts:125-269`
- MISSING — `$` include projections `{ from, count, transitive, where, limit, order }` and ordering by `$count`. `types.ts:321-373`; `model-query.test.ts:434-470`
- MISSING — dotted `rel.prop` order keys (need `limit`/`offset`; need `as any`). `model-query.test.ts:475-511`; `model_query/query.rs:508`
- MISSING — relation quantifiers `some`/`none` in `where` (also absent from TS `WhereOps`). `model_query/types.rs:184-187`; `core/src/model/types.ts:29-47`
- MISSING — `where: { producedByFlow }` outside flows.mdx. `types.ts:65-81`
- MISSING — `includeAll`, `deepQuery`. `types.ts:385-412`; `ModelQueryBuilder.ts:307`
- MISSING — static `count()`, `Ad4mModel.registerAll()`. `Ad4mModel.ts:1620, 2418`
- STALE (JSDoc → /jsdoc) — examples call `.run()` on the query builder (no such method); `ordering` JSDoc says "declaration only" but ordering is implemented. `decorators.ts:536, 684, 1049-1056`; `tests/js/tests/model/model-ordering.test.ts:128-313`

## social-dna.mdx

- WRONG — `perspective.getSubjectProxy(...)` (158-159, 360) — does not exist.
- WRONG — Turtle example (190-214): shape URI, `ad4m:initial`, `resolveLanguage` emission, `sh:datatype` on a collection. `shacl-gen.ts:93-95, 176-188, 218, 305-310`
- WRONG — "Stored as Links" tab is invented (221-233): no `ad4m://has_sdna`, `ad4m://sdna_type`, `sdna_Todo_prop_0`, `literal:number:1`. Real: `ad4m://self -ad4m://has_shacl-> literal:string:shacl://Todo -ad4m://shacl_shape_uri-> todo://TodoShape`; property shapes `todo://Todo.state`; counts `literal:1^^xsd:integer`. `shacl_parser.rs:1641-1653, 1732, 1811-1820`
- WRONG — key-concepts table `ad4m://initial`, `ad4m://readOnly` (249, 251).
- WRONG — SPARQL examples use `?link a rdf:Statement` with no PREFIX lines (478-504, 558-565); the store writes direct triples plus `?r rdf:reifies <<( s p o )>>`. `sparql_store.rs:13-14, 425-466`
- OK — flows section (`consensusRule`, `availableFlows`, `addFlow`, `sdnaFlows`). `core/src/shacl/SHACLFlow.ts:475`; `PerspectiveProxy.ts:1045, 1081, 1359`

## flows.mdx / developer-guides/writing-a-flow.mdx

- STALE — guarantees cite `fold.rs`, `atom.rs`, `receipt.rs`, `verify.rs` (569-576); these are directories now. `rust-executor/src/perspectives/flow_instance/`
- MISSING — `FlowState.semanticCheck` (LLM yes/no gate after `requires`). `SHACLFlow.ts:239`; `flow_semantic_check.rs:1-12`; `flow_evaluator.rs:21-41`
- MISSING — automatic flow spawn after an interpretation pass. `flow_spawn.rs:1-17`; `interpretation/run.rs:899`
- MISSING — `outputTypes`, `creationHint`, `context` fields. `SHACLFlow.ts` after 351
- MISSING — `getFlow(name)`, `sdnaFlows()`, `availableFlows(expr)`, `FlowInstance.startedAtMillis`. `PerspectiveProxy.ts:1045, 1081, 1415`; `FlowInstance.ts:562`
- MISSING — MCP flow tools `add_flow`, `get_flows`, `flow_state`, `flow_actions`, `flow_valid_outputs`; and the fact that MCP cannot propose, start or mint. `rust-executor/src/mcp/tools/flows.rs:79-459`

## developer-guides/mcp.mdx

- WRONG — `dynamicClassTools: true` in the config file (48) — only `--dynamic-class-tools` / `AD4M_DYNAMIC_CLASS_TOOLS`. `config_file.rs:182-224`; `run_config.rs:235-244, 426`
- STALE — topics "overview / architecture" (128) — overview, architecture, usage, flux, models. `mcp/tools/docs.rs:29-44`
- STALE — `AD4M_ADMIN_CREDENTIAL` "required" (29) — without it MCP binds loopback and trusts every caller. `mcp/server.rs:118-145`
- STALE — static list at 40 omits `instance_remove_from_collection`, `instance_transcript`, `add_child`, `get_children`, `execute_commands`. `mcp/tools/mod.rs:469-480`
- MISSING — `language_meta`; `--mcp-host`/`MCP_HOST`; loopback vs 0.0.0.0 rule; TLS MCP listener on `--mcp-port + 1`. `tools/languages.rs:28-31`; `server.rs:142-200, 238, 340-350`
- OK — bootstrap tool list, dynamic tool naming, hidden-tool error, waker tools attributed to the OpenClaw plugin.

## developer-guides/sparql-queries.mdx

- WRONG — every block uses `rdf:Statement` reification (returns nothing). `sparql_store.rs:425-466`
- WRONG — `ad4m:author` / `ad4m:timestamp` (17-18, 102, 118, 133, 175, 278) — real: `<ad4m://ontology/author>`, `<ad4m://ontology/timestamp>`. `sparql_store.rs:13-14`
- WRONG — prefixes without PREFIX lines; the executor injects none. `sparql_store.rs:1329-1341`
- WRONG — `FILTER(?timestamp > "…"^^xsd:dateTime)` (134) — timestamps are plain strings. `sparql_store.rs:187-189`
- STALE — "only SELECT" (381) — ASK, CONSTRUCT, DESCRIBE accepted. `sparql_store.rs:370-379`
- WRONG — `modelQuery()` as an auto-builder (337, 377) — raw `(className, queryJson)` RPC. `PerspectiveProxy.ts:686`

## hooks / batch-operations / ai

- hooks: WRONG — `useClient` (5-31) does not exist; exports are `useAgent`, `useMe`, `usePerspective`, `usePerspectives`, `useLiveQuery`, `toCustomElement`. `ad4m-hooks/react/src/index.ts:7-14`
- hooks: MISSING — `preserveReferences`, `toCustomElement`. `useLiveQuery.ts:46`
- batch: STALE (minor) — a thrown transaction is not discarded on the executor. `Ad4mModel.ts:2452-2460`
- ai: STALE — "OpenAI-compatible only" (125) — `ANTHROPIC`, `OLLAMA` exist; `discoverModels` undocumented. `types/core.rs:519-523`; `AIClient.ts:37`
- ai: WRONG — transcription model id `"Whisper"` (61-62, 230, 349) — `whisper_tiny`, `whisper_small`, …; unknown ids fall back to Tiny silently. `ai_service/mod.rs:1945-2000`
- ai: WRONG — VAD params "configure" detection (235-378) — ignored; hard-coded Silero. `ai_service/mod.rs:2004-2008, 2107-2125`
- ai: MISSING — `runInterpretation`, `runInterpretationWithHarness`, `addAutoProcessor`, `removeAutoProcessor`. `PerspectiveProxy.ts:363-463`

## cli / executor-config

- cli: WRONG — `--run-dapp-server` default false (55); default true. `rust-executor/src/config.rs:280-282`
- cli: WRONG — hc admin/app ports 1234/8888 (65-66); first free port from 2000 / 1337. `rust-executor/src/lib.rs:610-611`
- cli: WRONG — `run-local-hc-services` (116-118) — subcommands are `log`, `dev`, `init`, `run`, `config print`. `cli/src/ad4m_executor.rs:109-125`
- cli: WRONG — `perspectives create` (151) → `add`; `add-link` argument order (161) is `id source target [predicate]`; `query-links --source` (166) is positional; REPL `add Alice knows Bob` (185) is `add_link(src, pred, status, tgt)`; `ad4m dev` (292, 295) exists only on `ad4m-executor`. `cli/src/perspectives.rs:22-62`; `cli/src/repl.rs:14-16, 126-129`; `cli/src/ad4m.rs:57-94`
- executor-config: MISSING — `--sfu-local-did`, `--sfu-max-participants-per-node`, `--sfu-cascade-*`, `--sfu-preferred-node` (flag-only). `run_config.rs:249-311`

## installation / auth / multi-user / neighbourhoods / perspectives / languages / agents / expressions / spanning-layer / index / tutorial

- installation: WRONG — "Node.js 18+" (95); `>=22`. `core/package.json:35`. STALE — "v0.10.1 RC (March 2025)" (5); 0.13.1-staging.2.
- installation 70-84, auth 46-64, multi-user 305-350: WRONG — `import Ad4mConnect from "@coasys/ad4m-connect"` — no default export; `getAd4mClient`/`getAd4mConnect({ appInfo, capabilities })`. `connect/src/index.ts:18, 54, 67`
- auth 141-144, installation 88: WRONG — `can: ["read"]`, `domain: "perspective-uuid"` — verbs uppercase, domains fixed names, UUID in `pointers`. `rust-executor/src/agent/capabilities/defs.rs:5-27, 244-252`
- auth: WRONG — `new Ad4mClient({adminCredential})` (153); signature `(baseUrl, token?, options?)`. `Ad4mClient.ts:35-39`. MISSING — `AD4M_ADMIN_CREDENTIAL_FILE`, `--auto-permit-cap-requests`, `getApps`/`removeApp`/`revokeToken`.
- multi-user: WRONG — `requestCapabilityForUser` / `generateJwtForUser` (422, 425) — `requestCapability` / `generateJwt`. `AgentClient.ts:233-242`. STALE — legacy `wss://host:12001/api/v1/ws` (347, 359); TLS/SMTP only as Launcher settings; `listUsers` fields incomplete.
- neighbourhoods: WRONG — `LinkSyncAdapter` interface (25-56) is gone; link languages are flat capability exports. `docs-src/language-interface-spec.md:184-193`. WRONG — template param `uuid` (137, 155, 167, 202) → `uid`. `rust-executor/src/languages/mod.rs:1370`. WRONG — `applyTemplateAndPublish(addr, {…})` takes a JSON string. `LanguageClient.ts:29`. WRONG — `languages.isInstalled`/`install`, `perspective.join` (99-104) do not exist. MISSING — telepresence, signals, SFU (`onlineAgents`, `setOnlineStatus`, `sendSignal`, `sendBroadcast`, `sfuStartRoom`). `NeighbourhoodClient.ts:46-163`
- perspectives: MISSING — batches, `snapshot`/`loadSnapshot`, link `status`, `perspective.on` events. `PerspectiveProxy.ts:786, 910-915, 952, 975-984`
- languages: WRONG — `perspectiveSync()` export (115) → `perspectiveSyncSync`/`Render`/`CurrentRevision`. WRONG — `ad4m.languages.install` (354).
- agents: WRONG — `expression.create(languageAddress, content)` (64) → `(content, lang)`. `ExpressionClient.ts:165`. WRONG — `runtime.sendMessage(did, "Hello!")` (73) → `friendSendMessage(did, Perspective)`. `RuntimeClient.ts:95`
- expressions: WRONG — literal "literal:base64…" (83, 151) → `literal:string:<percent-encoded>`. `core/src/Literal.ts:29-51`. WRONG — `Expression.language` field (27, 47); only `ExpressionRendered`. WRONG — dead link `/developer-guides/languages` (191).
- spanning-layer: STALE — MCP "auto-generated tools" (162) are opt-in. LIKELY WRONG — "SDNA enforced by all members" (113, 125, 143): no code rejects incoming links against SHACL (not fully verified).
- index: STALE — "RuntimeConfig JSON" (52) does not exist; `get()` comments show bare links where `LinkExpression`s return. `PerspectiveProxy.ts:344`
- tutorial/1, 2, 4: WRONG — dead links `/getting-started`, `api.md#…`. tutorial/2: `uuid` → `uid`; `perspect3vism/ad4m-seeds` link and `QmeBD9n9…` address not in current seeds. `rust-executor/src/mainnet_seed.json`

## Files outside `pages/`

- `host-contract.md` and `language-interface-spec.md` are linked as GitHub URLs from `languages.mdx:93, 168, 388, 391` and from AGENTS.md. Moving them means updating those links.
- Not linked from any page: `ad4m-social-conventions.md` (referenced by the .wit and the spec), `language-interface-migration-plan.md` (referenced from `rust-executor/src/js_core/languages_extension.rs`), `running-an-executor.md` and `headless-executor.md` (referenced by each other, `deploy/staging/*`, CHANGELOG), `design/language-interface-refactor-proposal-v2.md` (referenced nowhere).
- `pages/developer-guides/react_backup` is one extensionless 8 KB file (old `@ad4m/react` guide). Nextra ignores it. Delete.
- `_meta.json` files match the page files. `jsdoc` is typedoc output, gitignored, and 404 on the live site.

## Undocumented features present on `dev`

- Bounded traversal parent scopes. `core/src/model/types.ts:156-286`; `tests/js/tests/model/bounded-traversal.test.ts:131-317`
- Containment scope `{ id }`, built-ins `Container`/`TextContent`, writing under a holder's own children relation. `builtin_classes.rs:1-35`; `model-containment.test.ts:125-269`
- `$` include projections and ordering by `$x`. `types.ts:321-373`; `model-query.test.ts:434-470`
- Dotted relation-property order keys. `model-query.test.ts:475-511`
- Relation `some`/`none` quantifiers. `model_query/types.rs:184-187`
- `includeAll`, `deepQuery`. `types.ts:385-412`
- `@HasMany({ polymorphic, instantiateAs })`; relation `datatype`/`readOnly`; property `identity`/`interpretationHint`/`alias`/`datatype`/`options`. `decorators.ts:358-415, 931-1097`
- Flow `semanticCheck`; auto flow-spawn; `outputTypes`/`creationHint`/`context`.
- Interpretation / auto-processor API. `PerspectiveProxy.ts:363-463`; `tests/js/tests/model/auto-processor.test.ts`, `run-interpretation*.test.ts`
- MCP: `language_meta`, the seven flow tools, `--mcp-host`, TLS listener.
- Telepresence / signals / SFU client API. `NeighbourhoodClient.ts:46-163`
- AI providers `ANTHROPIC`/`OLLAMA`, `discoverModels`.
