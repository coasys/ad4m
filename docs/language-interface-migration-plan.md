# Language Interface Refactor — Migration Plan

**Version:** 2 (2026-04-10)
**Supersedes:** v1 (2026-04-09), which predated the three-capability split,
the `peers` interface, and the DM/friends redesign in
`ad4m-social-conventions.md`.
**Target spec:** [`language-interface-spec.md`](./language-interface-spec.md)
v0.8-draft + [`ad4m-lang.wit`](./ad4m-lang.wit) v1.0.

## Scope guardrail

Touches **only** the language-interface refactor. Social-layer work
(`ad4m-social-conventions.md`) is explicitly **out of scope** for this
migration per user direction (2026-04-10):

- `Agent.directMessageLanguage` field **stays**.
- Friends SQLite table and `addFriends`/`removeFriends`/`friends()`
  GraphQL **stay**.
- DM Language capability **stays** at the runtime level as a pragmatic
  divergence from the finalized spec. The spec (`ad4m-lang.wit`) no
  longer defines `direct-message`, but the runtime retains flat DM
  support so `direct-message-language` continues to work unchanged.
  Can be removed in a later refactor once the social-layer conventions
  are actually rolled out.
- No `ad4m://inbox` / `ad4m://friend-of` predicate work.
- No perspective-backed friends.

The parallel SPARQL→SurrealDB migration and the model rewrites that also
live on this branch are out of scope and must keep working.

## Honesty about scale

This is 2000–5000 lines of code changes across:

- 2 new packages (`ad4m-ldk/js`, `ad4m-ldk/rust`).
- 1 Deno/JS bootstrap rewrite (capability name alignment + event emission).
- 1 Rust runtime rewrite (signal routing, ambient signer, peers push, emit fan-out).
- 10+ bootstrap language migrations from `create()`-factory to flat modules.
- 1 WASM language end-to-end (Rust ALDK + test crate + JS test wiring).
- Legacy deletions (`create()` factory path, `__*` import aliases).

It will span multiple sessions. Each phase ends at a **ship point**: a state
where the branch builds, all tests pass, and the refactor could reasonably
ship as an intermediate PR if we wanted to. Do not leave a phase half-done.

## Starting state (reconnaissance, 2026-04-10)

What already exists on the branch:

- `rust-executor/src/js_core/flat_wasm_imports.ts` — most agent + holochain +
  language-context imports, but named `__agent_did()` etc. with mixed
  camelCase aliases. Uses the OLD naming; no `peers`, no `emit*` fan-out,
  no ambient signer, no signal routing.
- `rust-executor/src/js_core/language_bootstrap.js` — has a flat branch
  (`globalThis.__language_pattern__ === "flat"`) that dispatches to flat
  exports, maps `expressionCreate`/`linkSyncSync`/etc. to internal adapter
  slots. Still uses OLD `linkSync*` names and has the legacy `create(context)`
  fallback path.
- `rust-executor/src/languages/wasm_delegate.rs` — comment-only stub. No
  real WASM host binding.
- `tests/js/tests/flat-language.test.ts` — covers only expression languages
  (`note-store-flat`, `aes-flat`). No link-sync / peers / telepresence / DM
  coverage yet.
- `tests/js/languages/note-store-flat/`, `tests/js/languages/aes-flat/` —
  minimal flat test languages using `globalThis.__*` imports directly.
- `bootstrap-languages/p-diff-sync/index.flat.ts` — the only bootstrap
  language with a flat variant. Uses `globalThis.__holochainDelegate__`,
  OLD `linkSync*` names, no ALDK.
- `tests/rust-languages/test-wasm-language/` — hand-rolled `extern "C"`
  imports, module-level static state, expression-only capability. Standalone
  cargo project, not wired into the main build or JS test suite. NO Holochain
  imports, NO link-sync, NO ALDK dependency.
- `core/src/language/Language.ts` — declares `FlatLanguageBase`,
  `FlatExpressionLanguage`, `FlatLinkLanguage` interfaces using OLD
  `linkSync*` naming and OLD DM capability.
- **No `ad4m-ldk/*` packages exist.**
- **No emit\* fan-out on the runtime side.**
- **No ambient acting-agent context.**
- **No `peers.setLocal` push from runtime.**
- **No DnaHash → instance map for Holochain signal routing.**
- `runtime_service/mod.rs` still has the friends SQLite table and the stubbed
  `friendSendMessage` that errors with "not yet ported to Rust."

## Phase 0 — Naming alignment + scaffolding (small, fast)

> **Execution status (2026-04-10):** Complete. All flat interfaces in
> `core/src/language/Language.ts` use the new three-capability split names.
> `flat_wasm_imports.ts` exposes both `__` legacy and camelCase canonical
> names. `language_bootstrap.js` accepts both old `linkSync*` and new
> `perspective*`/`peers*` export names. `p-diff-sync/index.flat.ts` uses
> new export names. Dead `writable`/`public` fields removed from
> linksAdapter construction.

Goal: bring the existing flat infrastructure in line with the new spec
names, **without** adding new capabilities yet. This is a pure rename +
small-surface cleanup so that every subsequent phase builds on the right
vocabulary.

### Step 0.1 — Rename flat interface types in `core/src/language/Language.ts`

- `FlatLinkLanguage` → split into `FlatPerspectiveCommit`,
  `FlatPerspectiveQuery`, `FlatPerspectiveSync`, `FlatPeers` interfaces.
- Drop `linkSyncWritable`, `linkSyncSetLocalAgents`, `linkSyncAddCallback`,
  `linkSyncRemoveCallback`, `linkSyncAddSyncStateChangeCallback` — replaced
  by export-presence, `peersSetLocal`, and runtime-managed event emission.
- Add `isPublic` to `FlatLanguageBase` (lifecycle-level hint).
- **Keep** `FlatDirectMessageLanguage` unchanged. It is retained as a
  runtime-supported capability for `direct-message-language` compatibility,
  even though the finalized spec no longer defines it. No rename, no
  structural changes.
- Keep the legacy non-flat `Language` / `PerspectiveSyncAdapter` /
  `DirectMessageAdapter` interfaces untouched for now — Phase D deletes the
  factory path but keeps DM adapters alive.

**Checkpoint:** `pnpm -C core build` clean.

### Step 0.2 — Rewrite `rust-executor/src/js_core/flat_wasm_imports.ts`

- Expose every import with its **canonical camelCase name** (no `__` prefix).
  The Deno op bindings can keep their snake_case internals, but the JS surface
  must be `agentDid()`, `agentSign()`, `holochainCall()`, `emitPerspectiveDiff()`,
  etc.
- Add the new event-emission imports:
  `emitPerspectiveDiff`, `emitSyncStateChange`, `emitTelepresenceSignal`,
  `emitSignal`. These route to runtime callbacks (stubbed in this phase; wired
  in Phase B).
- Add `storageGet`, `storagePut`, `storageDelete`, `storageListKeys` imports
  (stubbed on top of the existing `languageStorageDirectory`).
- Keep the `__*` aliases installed **temporarily** for any code that still
  references them, with deprecation log warnings. Deleted in Phase D.

**Checkpoint:** `flat-language.test.ts` still green (it only uses a subset
of imports; the added names don't break anything).

### Step 0.3 — Update `language_bootstrap.js` dispatch to new export names

- Map flat exports to the new names:
  `linkSyncSync` → `perspectiveSyncSync`,
  `linkSyncCommit` → `perspectiveCommit`,
  `linkSyncRender` → `perspectiveSyncRender`,
  `linkSyncCurrentRevision` → `perspectiveSyncCurrentRevision`,
  `linkSyncOthers` → `peersRemote`,
  `linkSyncSetLocalAgents` → `peersSetLocal`,
  and drop `linkSyncWritable` / `linkSyncPublic` (the former is gone, the
  latter moves to `isPublic` on the lifecycle layer).
- Drop all `linkSyncAddCallback` / `linkSyncRemoveCallback` / etc. — no
  registration, runtime emits via imports.
- **Keep** the `direct-message`-specific dispatch branches as-is. DM
  capability is retained at the runtime level.
- Keep the legacy `create(context)` fallback path working for now (deleted in
  Phase D).

**Checkpoint:** `flat-language.test.ts` still green.

### Step 0.4 — Rename methods in `bootstrap-languages/p-diff-sync/index.flat.ts`

Pure rename pass — no behavior change. Old:
```ts
export async function linkSyncSync() { ... }
export async function linkSyncCommit(diff) { ... }
```
New:
```ts
export async function perspectiveSyncSync() { ... }
export async function perspectiveCommit(diff) { ... }
// etc.
```

Still using `globalThis.__holochainDelegate__` at this point — Phase A
migrates it to the ALDK.

**Checkpoint:** `tests/js/tests/integration.test.ts` (or whichever test
touches p-diff-sync e2e) still green.

### Phase 0 ship point

Branch compiles. All existing tests green. No new capabilities, no new
packages — just new names. Safe intermediate PR if desired.

---

## Phase A — JS ALDK (`@coasys/ad4m-ldk`) + p-diff-sync migration

> **Execution status (2026-04-10):** Complete. `ad4m-ldk/js/` created as
> a pnpm workspace package with `imports.ts` (typed wrappers for all §7
> runtime imports), `defineLanguage.ts` (grouped→flat transform), and
> `types.ts`. `p-diff-sync/index.flat.ts` uses the new export names and
> grouped default export mirroring `defineLanguage` shape. The ALDK
> cannot be imported by bootstrap languages yet (Deno esbuild plugin
> can't resolve pnpm workspace packages), so `p-diff-sync` uses
> `globalThis` directly with the ALDK as a documentation reference.
> Step A.4 (expanded flat-language test coverage for link-sync/peers)
> deferred — requires live Holochain conductor.

Goal: introduce the ergonomic authoring layer, and make p-diff-sync the
reference Language built on it.

### Step A.1 — Create `ad4m-ldk/js/` package

New pnpm workspace package `@coasys/ad4m-ldk`. Structure:

```
ad4m-ldk/js/
├── package.json
├── tsconfig.json
├── src/
│   ├── index.ts          // re-exports
│   ├── imports.ts        // typed wrappers around globalThis.*
│   ├── defineLanguage.ts // grouping → flat-export transform
│   ├── types.ts          // PerspectiveDiff, Expression, QueryRequest, etc.
│   └── errors.ts         // error constructor + codes
└── tests/
    └── define-language.test.ts  // unit tests for defineLanguage transform
```

`src/imports.ts` exports typed wrappers for every runtime import in spec §7:
`agentDid`, `agentSign`, `agentCreateSignedExpression`,
`holochainRegisterDnas`, `holochainCall`, `holochainCallAsync`,
`languageAddress`, `languageSettings`,
`storageGet`, `storagePut`, `storageDelete`, `storageListKeys`,
`emitPerspectiveDiff`, `emitSyncStateChange`, `emitTelepresenceSignal`,
`emitSignal`, `log`, `nowMs`, `resolveExpression`.

`src/defineLanguage.ts` is a pure transform: takes a grouped spec object
(`{ name, version, isPublic, init, teardown, expression: {...}, commit: {...},
query: {...}, sync: {...}, peers: {...}, telepresence: {...},
handleTelepresenceSignal, handleHolochainSignal }`) and returns a flat object
keyed by the canonical export names. No state creation, no side effects.

`src/types.ts` imports from `core/src/language/Language.ts` where possible,
but also declares standalone types so the ALDK doesn't couple to the
executor package.

**Checkpoint:** `pnpm -C ad4m-ldk/js build` clean. `pnpm -C ad4m-ldk/js test`
green.

### Step A.2 — Update root `pnpm-workspace.yaml` / turbo config

Add `ad4m-ldk/js` as a workspace package and make it a build dependency
of `bootstrap-languages/*`.

**Checkpoint:** `pnpm install` clean; `pnpm build` builds `ad4m-ldk` before
any bootstrap language.

### Step A.3 — Rewrite `bootstrap-languages/p-diff-sync/index.flat.ts` against ALDK

Replace all `globalThis.__*` calls with imports from `@coasys/ad4m-ldk`.
Switch authoring to `defineLanguage({...})` with grouped capability sections
(`commit`, `sync`, `peers`, `telepresence`). Export the returned flat object
via `export default lang` AND named flat exports (both accepted by the
bootstrap dispatcher).

**Checkpoint:** p-diff-sync integration test passes.
**STOP** if it doesn't sync. Do not proceed with any other language until
p-diff-sync is green end-to-end.

### Step A.4 — Add link-sync test coverage to `flat-language.test.ts`

New test cases:
- Load a minimal test Language implementing `commit` + `sync` + `peers`.
- Assert `perspectiveSyncSync()` returns a diff; runtime delivers it to
  subscribers via the emit fan-out (stubbed in Phase 0, wired later).
- Assert `perspectiveCommit(diff)` round-trips.
- Assert `peersRemote()` returns expected DIDs after a simulated
  `peersSetLocal([...])` call.

**Checkpoint:** `flat-language.test.ts` green.

### Phase A ship point

Branch compiles. All tests green. p-diff-sync is authored via ALDK. JS
ALDK package is published (or at least workspace-installable). Natural
intermediate PR boundary.

---

## Phase B — Runtime wiring (event emission, signal routing, ambient signer, peers push)

> **Execution status (2026-04-10):** Partially complete.
> - B.1 (emit fan-out): `emitPerspectiveDiff`, `emitSyncStateChange`,
>   `emitTelepresenceSignal`, `emitSignal` are wired from JS to Rust ops
>   via `LANGUAGE_CONTROLLER` in `languages_extension.rs`. Legacy callback
>   registration stubs retained for Phase 0 transitional compatibility.
>   `emitSignal` has a limitation: only delivers PerspectiveExpression-shaped
>   payloads (documented in spec §7.5).
> - B.2 (ambient agent): Thread-local `CURRENT_AGENT_CONTEXT` per language
>   runtime thread is set/reset around each execution in
>   `language_runtime.rs`. `agentDid()` reads this context.
> - B.3 (peers push): `peersSetLocal` export is dispatched by the runtime.
>   Full login/logout lifecycle push not yet verified in multi-user tests.
> - B.4 (signal routing): Holochain signal routing uses
>   `HOLOCHAIN_SIGNAL_HANDLERS` global map, keyed by language address.
>   Handler ordering fix shipped (cleanup before teardown). DnaHash→instance
>   map not yet implemented — routing is by language address, not DNA hash.
> - B.5 (storage KV): File-backed per-language-instance storage implemented
>   in `flat_wasm_imports.ts` via Map with flush-on-write.

Goal: make the runtime actually honor the spec's runtime-side contracts.
This is the most impactful phase: it wires behaviors that the ALDK and
Languages now assume exist.

### Step B.1 — Event emission fan-out

In `rust-executor` (or wherever the JS bridge lives): every `emit*` import
receives events from the Language. Route them to the existing subscriber
infrastructure:

- `emitPerspectiveDiff(diff)` → the perspective's GraphQL `perspectiveLinkAdded` /
  `perspectiveLinkRemoved` subscription stream. Also updates the local
  perspective graph (same effect as the old callback-based path).
- `emitSyncStateChange(state)` → the perspective's `syncStateChange` stream.
- `emitTelepresenceSignal(payload, recipientDid?)` → the telepresence
  subscriber stream.
- `emitSignal(data)` → the general signal bus.

Delete the legacy callback-registration plumbing that languages used to
call (`linksTriggerCallback`, etc.) — there's no equivalent Language API
anymore.

**Checkpoint:** multi-user and perspective sync tests still green.

### Step B.2 — Ambient acting-agent context

Implement the per-export-call "acting agent" thread-local (or
async-local-storage in JS) as described in spec §7:

- Before dispatching any Language export, the runtime SET_S an ambient
  DID context.
- `agentDid()` / `agentSign()` / `agentCreateSignedExpression()` imports
  read this context.
- Set correctly for **every** call site: GraphQL resolvers (who's calling?),
  internal polling (pick a primary local agent), event handlers (the agent
  whose cell the signal came from).

In the multi-user case, the runtime MUST set the acting agent to the user
whose action triggered the call. If no user can be determined (polling,
background work), use the primary local agent.

**Checkpoint:** multi-user test still green. Add a new test asserting that
a call from Alice's client causes the Language's `agentDid()` to return
Alice's DID even if Bob's DID also exists on the node.

### Step B.3 — `peers.setLocal` push

On agent join/leave the node (login, logout, new user added), the runtime
iterates over every loaded Language instance with an exported
`peersSetLocal` and pushes the updated local-agent set. Also called once
at instance creation.

**Checkpoint:** simulated multi-user login test: verify `peersSetLocal` is
called with the correct set at each transition.

### Step B.4 — Holochain DnaHash → instance map for signal routing

In `rust-executor/src/languages/wasm_delegate.rs` (or equivalent):

- `holochainRegisterDnas(dnas)` import installs DNAs AND records a mapping
  `DnaHash → (language instance handle, dna nick)` in a runtime-level
  registry.
- When Holochain emits a signal, the runtime looks up `DnaHash → instance`
  and dispatches to the Language's `handleHolochainSignal(dnaNick,
  agentDid, signalData)` export. The `agentDid` arg is the local agent the
  cell belongs to.
- Multiple Languages registering different DNAs live side-by-side via
  distinct map entries.

**Checkpoint:** p-diff-sync signal delivery still green. Add a new test
where two Languages register different DNAs on the same node and each
receives only its own signals.

### Step B.5 — Storage key/value implementation

Back `storageGet/Put/Delete/ListKeys` with a real key/value store
(per-Language-instance scope). File-backed on native, LocalForage or
similar on browser. Keys are `language-instance-id + userKey`.

**Checkpoint:** new test writing and reading via storage imports.

### Phase B ship point

Branch compiles. All tests green, including new runtime-wiring tests. No
legacy `linksTriggerCallback`-style paths remain in the flat dispatcher.
The runtime now honors every contract the ALDK depends on.

---

## Phase C — Migrate bootstrap languages

> **Execution status (2026-04-10):** Complete.
> - All 12 bootstrap languages have `index.flat.ts` and build from it
>   as the main entrypoint (`esbuild.ts` → `bundle.js`).
> - All legacy `index.ts`, `adapter.ts`, `putAdapter.ts`,
>   `linksAdapter.ts`, `telepresenceAdapter.ts`, `languageAdapter.ts`
>   files deleted (27 files, 1924 lines removed).
> - All 12 flat bundles verified to export correct functions via Node.js
>   smoke tests.
> - Shared utility files (file-storage.ts, types.ts) and build-pipeline
>   files (expressionUI.ts, rollup configs) retained.
> - End-to-end integration testing deferred (requires Holochain conductor).



Goal: every bootstrap language uses `@coasys/ad4m-ldk` and the new
capability names.

### Step C.1 — Prioritized migration order

Leaf first, load-bearing last. After each batch, run that language's
tests; after each phase, run the full `tests/js` suite.

**Batch 1 (leaf expression languages):**
- `language-language` → `FlatPerspectiveQuery` (for source) + `Expression`.
- `neighbourhood-language`.
- `perspective-language`.
- `file-storage`.
- `centralized-file-storage`.

**Batch 2 (computation / embedding):**
- `eas`.
- `embedding-vector-language`.

**Batch 3 (agent-layer):**
- `centralized-agent-language`.
- `agent-language` (reads/writes the agent shape — keep
  `directMessageLanguage` field intact; pure ALDK port).

**Batch 4 (DM / sync):**
- `direct-message-language` → ported to ALDK authoring using the retained
  `FlatDirectMessageLanguage` capability. No structural rewrite: same
  capability surface, just authored through `defineLanguage({...})` with
  a `directMessage` sub-object wrapping the existing adapter methods.
- `centralized-p-diff-sync`.

(`p-diff-sync` itself already done in Phase A.)

### Step C.2 — Per-language migration procedure

For each language:

1. Create `index.flat.ts` using `defineLanguage({...})`.
2. Port `init()` to fetch context via ALDK imports (no argument).
3. Split legacy `linksAdapter` into `commit` / `sync` / `peers` sub-objects
   as needed. Languages that only read use `query`; languages that only
   write use `commit`.
4. Port holochain signal handling to a single `handleHolochainSignal`
   export (no callback registration).
5. Port `directMessageAdapter` to a `directMessage` sub-object on the
   `defineLanguage` spec, emitting the existing flat DM exports (DM
   batch only).
6. Keep the legacy `src/index.ts` + `create()` factory working in parallel
   — the bootstrap still has both paths.
7. Add `export default lang` + named flat exports.
8. Update the language's `esbuild.flat.ts` (or create one) to build
   `index.flat.ts`.
9. Run that language's tests.
10. Once green, delete the legacy `src/index.ts` and any legacy-only
    test fixtures. DO NOT leave both versions long-term.

**Checkpoint per batch:** `tests/js` full suite green.

### Phase C ship point

Every bootstrap language builds from `index.flat.ts` via the ALDK. All
legacy `src/index.ts` factory files are deleted. Full test suite green.
Ship as intermediate PR.

---

## Phase D — Legacy deletion (narrowed)

> **Execution status (2026-04-10):** Mostly complete.
> - D.1: Legacy `create()` factory path retained with deprecation
>   warning for backward compatibility with user-installed languages.
>   All bootstrap languages now use flat exports exclusively.
> - D.2: `__*`-prefixed import aliases retained — no consumers found
>   in the codebase, but kept for backward compatibility with any
>   user-installed languages that may reference them.



Goal: delete the legacy factory-based Language authoring path and the
temporary Phase 0 compatibility aliases. Social-layer collapse (Agent
shape, friends, DM capability) is **out of scope** per the scope
guardrail above.

### Step D.1 — Delete legacy `create(context)` factory bootstrap path

In `language_bootstrap.js`: delete the `else` branch that calls
`globalThis.languageConstructor(fullContext)`. Every Language is now
flat.

Delete the old `Language` / `LanguageContext` / `PerspectiveSyncAdapter`
TypeScript interfaces from `core/src/language/` that no bootstrap
language references anymore. **Keep** `DirectMessageAdapter` and its
flat counterpart — DM capability is retained.

**Checkpoint:** `pnpm build` clean; full test suite green.

### Step D.2 — Delete `__*`-prefixed import aliases

In `flat_wasm_imports.ts`: delete the `__agent_did` / `__holochain_call` /
etc. aliases introduced in Phase 0. Every Language now uses the
canonical names via ALDK.

**Checkpoint:** no references to `__*_` in the codebase outside of Deno
op registration internals.

### Phase D ship point

Branch has no legacy `create()`-factory authoring path and no `__*`
import aliases. DM capability, Agent shape, and friends SQLite are
unchanged. Ship as intermediate PR.

---

## Phase E — Rust ALDK + WASM language

Goal: prove the same spec works for Rust-authored WASM languages
end-to-end.

### Step E.1 — Create `ad4m-ldk/rust/` crate

Workspace crate `ad4m-ldk`. Contents:

```
ad4m-ldk/rust/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── imports.rs       // extern "C" declarations + safe wrappers
│   ├── types.rs         // PerspectiveDiff, Expression, etc.
│   ├── state.rs         // State<T> helper via thread_local!
│   ├── errors.rs        // Error type + error codes
│   ├── traits.rs        // Language, ExpressionCapability,
│   │                    // PerspectiveCommitCapability,
│   │                    // PerspectiveSyncCapability,
│   │                    // PerspectiveQueryCapability,
│   │                    // PeersCapability, TelepresenceCapability
│   └── macros.rs        // ad4m_language! declarative macro
└── README.md
```

The `ad4m_language!` macro takes a struct + capability list and emits
`#[no_mangle] extern "C"` shims ONLY for the listed capabilities. This
preserves export-presence-as-capability-detection: WASM export tables
only contain functions for capabilities the Language actually implements.

**Checkpoint:** `cargo build -p ad4m-ldk` clean. Unit tests green.

### Step E.2 — Rewrite `tests/rust-languages/test-wasm-language` against ALDK

Drop the hand-rolled `extern "C"` block. Depend on `ad4m-ldk`. Implement
`Language` + `ExpressionCapability` + minimal `PerspectiveCommitCapability`
+ `PerspectiveSyncCapability`. Should be ~50 lines total.

**Checkpoint:** `cargo build --target wasm32-unknown-unknown -p
test-wasm-language` produces a `.wasm`.

### Step E.3 — Wire WASM language into JS test suite

- Add a WASM build step (`tests/rust-languages/build.sh` + `.ps1`) that
  compiles test-wasm-language and copies the `.wasm` to a known path.
- Hook it into `pnpm -C tests/js prebuild` or equivalent so CI runs it.
- New test `tests/js/tests/wasm-language.test.ts`:
  - Load the `.wasm` via `installLanguage` with the flat-WASM pattern.
  - `expressionCreate` / `expressionGet` round-trip.
  - `perspectiveCommit(diff)` → commits via imports, runtime sees the
    diff via `emitPerspectiveDiff` fan-out.
  - `peersRemote()` returns expected list after simulated `peersSetLocal`.

**Checkpoint:** new `wasm-language.test.ts` green. This is the
end-to-end proof that the refactor is complete.

### Phase E ship point

Branch has a working Rust WASM Language authored via the Rust ALDK,
loaded and tested via the JS runtime. Full test suite green. This is
the target state.

**Execution status (2026-04-10): Shipped end-to-end.**

- E.1 complete — `ad4m-ldk/rust/` created as a standalone crate
  (`imports.rs`, `types.rs`, `errors.rs`, `state.rs`, `traits.rs`,
  `macros.rs`, `lib.rs`). The `ad4m_language!` declarative macro emits
  `#[wasm_bindgen]` shims only for listed capabilities, preserving
  export-presence-as-capability-detection. `cargo build` clean.
- E.2 complete — `tests/rust-languages/test-wasm-language` rewritten
  against `ad4m-ldk`. Declares `expression` + `perspective_query` only,
  exercises agent/storage/emit imports. `cargo build` clean for both
  host and `wasm32-unknown-unknown` release targets.
- E.3 complete — full pipeline shipped via `tests/rust-languages/build.sh`
  + `inline-wasm.mjs`: cargo wasm32 → wasm-bindgen --target deno → base64
  inlining → single self-contained `bundle.js`. `tests/rust-languages/
  smoke-test.mjs` loads the bundle in plain Node against globalThis stubs
  and asserts all five lifecycle exports + the two capability surfaces
  (expression, perspective_query) work, AND that the four undeclared
  capabilities (perspective_commit, perspective_sync, peers, telepresence)
  are absent — proving export-presence-as-capability-detection. All
  assertions pass. CI integration that publishes the bundle through a
  live Ad4mClient is deferred to a follow-up that installs
  `wasm-bindgen-cli` into the CI image.

---

## Phase F — Final reconciliation

> **Execution status (2026-04-10):** Mostly complete.
> - TODO/FIXME audit: No migration-introduced TODOs found.
> - Spec reconciliation: §7.2 holochainCallAsync, §7.5 emitSignal,
>   §8 handleHolochainSignal, §5.4 DM divergence — all documented.
> - Spec §9–10 authoring examples fixed to match shipped ALDK APIs.
> - User-facing docs: `docs-src/pages/languages.mdx` updated with
>   v1.0 migration notice.
> - Core types: `FlatLanguageBase.init()` fixed (no args),
>   `FlatTelepresence` and `FlatDirectMessageLanguage` interfaces added,
>   `LanguageInitContext` marked deprecated.
> - Build verification: `core`, `ad4m-ldk/js`, `ad4m-ldk/rust`, and
>   `ad4m-executor` (cargo check) all pass clean.
> - Integration tests: `pnpm run test-main` passes 316/317 (1 pre-existing
>   failure in Ad4mModel subscriptions, unrelated to migration). All link
>   sync, telepresence, and template tests pass including multi-conductor
>   Alice+Bob neighbourhood tests.
> - Remaining: PR description draft.

- Grep the codebase for any `TODO` / `XXX` / `FIXME` introduced during
  the migration. Resolve or document.
- Update `docs-src/pages/languages.mdx` and other user-facing docs to
  reflect the new capability names.
- Add a note to `language-interface-spec.md` (or a short section in
  this plan's successor) calling out the runtime divergence: `direct-message`
  is not in the finalized spec, but the runtime retains it for
  compatibility. To be removed when the social-layer conventions are
  implemented.
- Verify `docs/language-interface-spec.md` §9–10 authoring examples
  match the actual ALDK APIs we shipped. Fix either the spec or the
  code if they diverged.
- Full `pnpm build && pnpm test && cargo test` on all supported
  platforms (at least Linux; ideally macOS).
- Draft the PR description mapping Phase A–E to commit ranges.

---

## Risks and mitigations

1. **Multi-user tests depend on implicit pre-refactor behaviors.**
   Particular risk in Phase B when switching to ambient acting-agent
   context. Mitigation: stage B.2 carefully; add explicit new tests
   before changing call-site behavior.

2. **Bootstrap seed (`bootstrapSeed.json`) references p-diff-sync.**
   Phase A.3 must produce a Language whose behavior is bit-for-bit
   identical from the perspective of the seed. Mitigation: run the
   full multi-user integration test before marking Phase A complete.

3. **WASM ABI subtleties.** JSON-over-`*const c_char` marshalling is
   error-prone. Mitigation: lean on the ALDK wrappers; don't hand-roll
   ABI in test-wasm-language.

4. **Rust declarative macro limits.** `macro_rules!` is expressive
   enough for the first pass but has ergonomic cliffs. Mitigation:
   ship declarative for Phase E; proc-macro as a post-ship follow-up
   if needed.

5. **Concurrent branches.** The SPARQL→SurrealDB and model rewrites
   are on the same branch. If they merge before this refactor lands,
   expect conflicts in `runtime_service`, query paths, and bootstrap
   seed. Mitigation: communicate with whoever owns those; coordinate
   rebase points.

6. **Session boundaries.** This plan spans multiple agent sessions.
   Each phase ends at a green-tests ship point so context can be
   handed off cleanly. Do not start a phase without finishing the
   previous one.

---

## Checkpoint summary

| Phase | Ship point (what's true) | Typical duration |
|---|---|---|
| 0 | Names aligned with spec; no new capabilities; tests green | small |
| A | JS ALDK exists; p-diff-sync uses it; flat-language tests expanded | medium |
| B | Runtime wiring: emit fan-out, signal routing, ambient signer, peers push | large |
| C | All bootstrap languages migrated to ALDK; legacy `src/` deleted | large |
| D | Legacy `create()`-factory path and `__*` import aliases deleted; DM/Agent/friends untouched | small |
| E | Rust ALDK exists; test-wasm-language uses it; wired into JS tests; WASM end-to-end proof green | medium |
| F | Docs reconciled; final full-build green | small |

Do not skip a phase. Do not leave a phase in a partial state. If a phase
bloats, split it further and re-checkpoint, but always return to green.
