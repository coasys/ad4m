STATUS: DONE

# Acorn S2-0 — mount-aware `instance*` write path

`Ad4mModel` writes go through the `perspective.instance*` RPCs (Nico's route,
11-18); the shared write core behind them is mount-aware, so a mounted
class's `call` constructor / setters / destructor reach its language.

Branch `acorn/s2-0-instance-writes` in this worktree, based on
`origin/acorn/a9-mount-lifecycle` with `origin/feat/register-shapes`
(`6c2ab01e8`) merged in. Nothing pushed (no credentials on this box).

## Commits

- `afe89d126` — merge `feat/register-shapes` into the a9 base (conflicts below)
- `d8d710315` — test(executor): mounted lifecycle writes through the instance* path
- `3cd30d37e` — feat(executor): mount-aware instance* writes
- `07d889a41` — feat(sdk): route Ad4mModel writes through the instance* RPCs

## Merge conflicts (10 files, one hunk each)

- `core/src/generated/api/RpcMethods.ts` — kept `PerspectiveMountParams`
  (a9's mount RPCs), dropped `PerspectiveNamedShacl` (register-shapes
  removed the `perspective.getShacl*` methods it typed).
- `core/src/model/Ad4mModel.ts` — `save()` create region: kept both
  sides' write path; the S2-0 SDK switch rewrote it anyway.
- `core/src/perspectives/PerspectiveClient.ts` — kept the
  `PerspectiveCreateSubjectResult` signature beside
  `perspective.registerShapes`.
- `core/src/perspectives/PerspectiveProxy.ts` — `createSubject` kept the
  result object a mount create needs (its minted address), folded with
  the new class-name resolution.
- `rust-executor/src/api/AGENTS.md` — guard-table count set to the
  merged table (241 rows).
- `rust-executor/src/api/perspectives_ws.rs` — dropped the removed
  `getShaclNames`/`getShaclTargetClass`/`getShacl` registrations; kept
  a9's `flowFired`, `syncMount`, `mounts`.
- `rust-executor/src/api/tests/method_matrix_tests.rs` — took
  register-shapes' `flowFired` param shape.
- `rust-executor/src/mcp/tools/instances/create.rs` — took the shared
  `instances::create` route; mount-awareness lives in the shared core.
- `rust-executor/src/perspectives/mod.rs` — union of the module decls.
- `rust-executor/src/perspectives/perspective_instance.rs` — union of
  `mod` decls (a9's `mounts` + register-shapes' `shapes`).

## Lal's five failure modes → the fixes

1. **Batch vs `call` actions.** `run_call_actions` still refuses any
   batch; the smaller correct change is to open no batch of our own when
   the write carries calls — `create` (`instances/write.rs:402-483`,
   `has_calls` from `ctor_is_call` and each prop's `SetterKind::Call`)
   and `update` (same partition, `write.rs:653+`). A caller's batch that
   contains calls is still refused by `check_batch`; its staged link
   writes still stage into it (`links_batch`, `write.rs:479-483`).
   Keeping the refusal preserves the no-partial-side-effects invariant a
   batch implies but a language call can't honour.
2. **Concrete `instanceRemove` runs the destructor.** `write.rs:820-846`:
   `destructor_kind` → `remove_subject` before the `remove_all_links_of`
   cascade; a `call` destructor runs unbatched, link destructors stage
   into the caller's batch.
3. **`instanceCreate` returns the constructor's address.** `write.rs:484-502`:
   `create_subject` answers `Option<String>` — `Some(minted)` for a `call`
   constructor, `None` for link constructors — `minted.unwrap_or(base_uri)`.
4. **Collections reach `call` setters as the bare member list.** Concrete
   `update_subject` already passes values verbatim (no `{action,value}`
   envelope); `write.rs`'s update puts a collection with a declared
   setter in `subject_values` as the array (and into `initial` on a `call`
   constructor, `write.rs:444-465`), so the call gets `{tags:[ids]}` —
   `[]` included, which clears.
5. **Ordinary-model compat.** `update` still refuses an empty patch
   (`write.rs:626`) and an uncommitted instance (`require_instance`,
   `write.rs:644`) — the SDK keeps both working: a no-change save makes
   no call, and every batched write stays on the legacy
   `executeAction`/`updateSubject`/`createSubject`/`removeSubject` path
   (`Ad4mModel.ts` — `writeRelation` ~1815, `save()` ~1977+). So
   create-then-save in the caller's batch is unchanged.

## Fixes found beyond the five

- **Mount aliases under register-shapes.** `alias_mount_class_names`
  read the a9 `ad4m://shacl_shape_uri` name link; the register-shapes
  class node carries `ad4m://shape` — `mounts.rs:666-695` now reads it
  from the target-class node and points the `literal:string:shacl://`
  alias at it.
- **`add_sdna`/`add_sdna_batch`/`SdnaType` restored** in
  `perspective_instance/sdna_writes.rs` (register-shapes removed them
  but mount installs and the a9 tests call them). It also registers a
  `literal:string:shacl://<name>` class node when the declared name
  differs from the target-class basename, as the mount path does.
- **Setter lookup through the shape, not a URI guess.**
  `has_setter` (`instances/mod.rs:487-507`) resolves the property node
  via `property_node` (now `pub(crate)`,
  `perspective_instance.rs:5251`) — `shape_uri → sh://property` — so a
  mount alias finds `mounttest://Thing.title`, not
  `{alias}.title`. It also accepts the `ad4m://adder` + `ad4m://remover`
  pair every relation gets: a scalar `hasOne` writes as a one-member
  replace (`scalar_replaces`, `write.rs:408-443` create / `653+` update).
- **Setter-less collections and scalars go through a synthesized
  `collectionSetter`.** `replace_collection_without_setter`
  (`abstract_classes.rs:634-668`) — used by update's `plain_collections`,
  create's `scalar_replaces`, and create's `plain_collections`
  (`write.rs:542-579`), so a declared ordering gets its entries on
  create too (`apply_collection_ordering` runs inside `execute_commands`,
  `perspective_instance.rs:5160-5171`).
- **`literal:json:` passthrough.** `coerce_scalar`
  (`instances/validate.rs:202-217`) accepts objects and arrays on an
  open (`string`) datatype — the encoding the subject writers always
  used for `metadata = {...}` props; typed scalars still refuse them.
- **`mount_lifecycle_tests.rs`** — `get_subject_classes_from_shacl`
  (removed by register-shapes) → `class_names()`.

## SDK routing (`core/src/model/Ad4mModel.ts`)

- `save()` unbatched: `instanceCreate` (new rows) / `instanceWrites`
  (changes, `Ad4mModel.ts:2041`); batched stays legacy.
- `setProperty`, `writeRelation` (`:1810`), `delete()` (`:2375`,
  `instanceRemove` when a destructor could exist, `removeSubject` in a
  batch), and the static `update`/`delete` go through `instance*`.
- Relation dispatch: forward collection → full member list (`[]`
  clears) in the patch, or the incremental
  `instanceAddToCollection`/`instanceRemoveFromCollection` for an
  adder/remover envelope; `hasOne` → its scalar member; `getter`,
  `readOnly` and reverse (`belongsTo*`) → legacy `executeAction` (the
  SHACL marks them read-only). Reverse relations are deferred to
  `writeRelation` after create, as before.
- Custom `resolveLanguage`/envelope props stay deferred
  (`deferredExpressionProps`, `:1977`) — unchanged batch behaviour.

## Tests

Rust (`cargo test --release --lib -- --test-threads=1`,
`CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`):

- `perspectives::instances::` + `call_actions` filter: **48 passed**
  incl. the new `instance_writes` mount tests (constructor address,
  grouped setters+delta, destructor, local flag), collection replace
  `[]`, and `update_replaces_a_scalar_relation_member` (hasOne).
- `mount` filter (a9 mount/mount-lifecycle/ownership suites): **60
  passed** (1 ignored pre-existing).

JS `test-model` (`LOCAL_MODE=true`, `EXECUTOR_PATH=.../release/ad4m-executor`):

- Before the SDK/executor compat fixes: **22 failures** (readOnly /
  notACollection / wrongType / ordering patterns).
- After: **310 passing, 0 failing, 31 pending.**

Builds: `cargo check --release` clean; `cd core && pnpm build` clean;
`cargo fmt --all -- --check` clean; release executor at
`/home/data/code/ad4m-acorn-target/release/ad4m-executor` rebuilt from
the final tree; Deno snapshot rebuilt (`rust-executor pnpm build`) —
no `js_core/*.js` changed so its content is unchanged.
`core/src/generated/api/*` untouched — no RPC contract changed.

## Open risks

- **`hasOne` clear (`= null` / remove)**: a scalar patch cannot express
  "no member", so the SDK keeps the legacy remover write for it
  (executor-side remove still works via `instanceRemoveFromCollection`
  on the same predicate). If Acorn relies on `instanceUpdate` clearing a
  `hasOne`, that needs a `null` convention.
- **Structured values on typed scalars** are refused (`wrongType`); only
  the open `string` datatype keeps the legacy `literal:json:` storage.
- **`instanceUpdate` on an abstract class** validates against the
  abstract shape and dispatches per concrete class — collections
  without a concrete-settable predicate are refused, matching the
  existing abstract invariant.
- **We-side tests**: the 5 write-through tests are still skipped —
  un-skipping is the next step (per the task rules).
