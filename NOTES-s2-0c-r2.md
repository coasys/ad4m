STATUS: DONE

# Acorn S2-0c round 2: mount-aware instance* writes, Lal's review fixed

Branch `acorn/s2-0-instance-writes`, on top of round 1 (`92690f459`). Nothing
pushed, no PR, nothing posted. Builds and tests used `-j 4` and
`CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`. Logs are in `/tmp/s2-0c-r2/`.

## Commits

| Item | Commit | What |
|---|---|---|
| 1 | `4bbdeac5f` fix(executor): refuse a caller batch on instance* writes that call a language | New `InstanceError::CallsInBatch` (RPC 400, MCP text). `refuse_batch_for_calls` runs in create, update and concrete remove before any call or link write. The trailing links no longer stage into the caller's batch. Regenerated `InstanceError.ts`. Round 1 had also left `InstanceUpdate.ts` / `PerspectiveInstanceUpdateParams.ts` stale; they are regenerated here too. |
| 5 (executor) | `95a433926` fix(executor): choose the instance* batch from the actions that run | An abstract `instanceUpdate` now asks `abstract_update_calls`, through `update_through_abstract`, whether a call runs; if one does, it opens no batch. An abstract `instanceRemove` with a call destructor refuses a caller batch (`abstract_remove_calls`). On create, a setter-less collection goes to `create_subject` only when `ctor_is_call`. |
| 2, 3, 4, 5 (SDK) | `35dc46f38` fix(sdk): route Ad4mModel writes by declared call actions, by class IRI | One commit: all four SDK fixes hang on the same routing switch (`callBacked()`), and splitting it would have made intermediate commits that do not work. Details below. |
| — | (this commit) docs | This file, plus the batch invariant in `perspectives/instances/AGENTS.md`. |

That is not one commit per item: item 5 is split executor/SDK, and SDK items 2 to 4 share a commit with it.

### SDK commit in detail (`core/src/model/Ad4mModel.ts`)

- `Ad4mModel.hasCallActions()` reports whether the class's shape declares a `call` constructor, destructor, setter, adder or remover. It is cached per class.
- Item 4: `fromSHACL` keeps the registered shape (`sourceShape`), and `hasCallActions` reads it. Without that, the shape the decorator generates holds link actions only.
- Item 2: `getTargetClass()` of a `fromSHACL` class is now the shape's own `targetClass`. Before, it was `namespace + name`, which is wrong whenever the name differs from the IRI's local name. The mount does exactly that: `MountThing` for `mounttest://Thing`; round 1 would have sent `mounttest://MountThing`. Every instance* call sends `queriedAs()`.
- Item 5: only call-backed classes, and not rows read through an abstract class, use instance*:
  - create: `instanceCreate` (`callCreate`)
  - update: one `instanceUpdate` patch (`callUpdate`)
  - `setProperty`: `instanceUpdate`
  - relation writes: `callRelation`
  - `delete()`: `instanceRemove`
  A caller's batch is passed on, and the executor refuses it. Every other class is byte-for-byte back on its pre-round-1 code (`git checkout afe89d126 -- Ad4mModel.ts`, then the call branches added). Ordinary deletes are scoped again, and batched/unbatched behaviour is as before.
- Item 3: on a call-backed class, a collection in `save()` is its full member list, `[]` included. `add*`/`remove*`, and adder/remover envelopes in `save()`, become a full-list replace through the declared setter: current members (read fresh) plus or minus the ids. Nothing goes through `instanceAddToCollection`/`RemoveFromCollection`, which write raw membership links. A `hasOne` clear and reverse relations keep the legacy write, as in round 1.
- `call-constructor.test.ts` had not compiled since the merge: it imported `./query-cache`, which dev removed. I dropped the import.

## Red lines (each test fails before its fix)

Rust (`/tmp/s2-0c-r2/red-rust.txt`, `red-rust-abstract-remove.txt`), all in `call_actions_tests::instance_writes`:

- `create_refuses_a_caller_batch_before_calling`: `a create with a call constructor ran in the caller's batch: Ok(InstanceCreated { … base_uri: "…://t1" … })`
- `update_refuses_a_caller_batch_before_calling`: `an update with a call setter ran in the caller's batch: Ok(InstanceUpdated { … })`
- `remove_refuses_a_caller_batch_before_calling`: `a remove with a call destructor ran in the caller's batch: Ok(InstanceRemoved { … links_removed: Some(0) })`
- `abstract_update_calls_without_a_batch`: `Failed { class_name: "app://TextContent", step: UpdateSubject, error: call actions cannot run inside a batch }`. The built-in `ad4m://TextContent` is not in the instance* registry (`UnknownClass`), so the test registers an app abstract `app://TextContent` that the mount's `TextContent` alias answers to.
- `abstract_remove_refuses_a_caller_batch_before_calling` (only the remove hunk reverted): `Err(Failed { …, step: RemoveSubject, error: call actions cannot run inside a batch })` where `CallsInBatch` was expected.
- `a_call_setter_does_not_swallow_a_plain_collection_on_create`: `the plain collection was skipped: []`

Core jest, `core/src/model/call-backed-writes.test.ts` (`/tmp/s2-0c-r2/red-core.txt`). At `92690f459`, 11 of 11 fail:

- IRI on create: expected `a://Task`, received `Task`. IRI on update/setProperty/setRelation/delete: 3 instance* calls instead of 4 (one went through raw links), all under `Task`.
- save of `tags = []`: no `instanceUpdate` (it went out as `executeCommands`).
- add / remove / adder envelope: `unexpected RPC instanceAddToCollection` / `instanceRemoveFromCollection`.
- fromSHACL delete with no flag: no `instanceRemove` (raw link removal).
- Caller batch: no `instanceUpdate` with `batchId` (silent legacy write).
- Ordinary class: `hasCallActions` missing. Unbatched update/setProperty/delete sent `instanceUpdate, instanceUpdate, instanceRemove`; create sent `instanceCreate`.

## Test counts (after)

- `cargo test --release --lib call_actions`: 20 passed.
- `cargo test --release --lib mount` (the round-1 mount suite): 60 passed, 1 ignored (as in round 1).
- `cargo test --release --lib perspectives::instances::`: 34 passed. `instance_writes`: 10 passed.
- `cargo test --release --lib instances`, as the brief asked: the process aborts (SIGABRT, a libc++ `vector[] index out of bounds` in V8) at `mounts_tests::mounted_records_are_instances` when it runs after the MCP tests in one process; that test passes in the `mount` run. With `--skip mounted_records_are_instances`: 149 passed, 5 failed. The 5 are all `mcp::tools::instances::tests`:
  - `a_property_stored_through_a_language_takes_the_value_the_language_does`
  - `create_failing_after_the_subject_is_staged_leaves_no_instance`
  - `instance_tools_round_trip_typed_data`
  - `template_update_writes_nothing_the_class_schema_refuses`
  - `validation_names_property_type_and_cardinality`

  I checked out `92690f459`'s `rust-executor/src` and ran the same filter: the same 5 fail there (`/tmp/s2-0c-r2/rust-mcp-instances-92690f4.txt`), so they predate this round. The likely causes are round 1's `literal:json:` passthrough in `coerce_scalar`, which lets `"maybe"`-style values through, and the synthesized collection replace on create. Round 1 did not report them because it filtered on `perspectives::instances::`. **Not fixed here; they need a follow-up.**
- `committed_sdk_rpc_methods_are_current` + `perspective_instances_ws`: 25 passed.
- Core jest: 41 suites, 1378 passed, 1 skipped. At `92690f459` it was 13 failed and 1 suite that did not compile, including #1666's `class a write addresses › creates the record under the model's target class IRI`.
- JS `test-model` (`LOCAL_MODE=true`, `EXECUTOR_PATH` = release binary of this branch): 310 passing, 0 failing, 31 pending.
- `rustfmt --check` on the 6 changed `.rs` files: clean. `tsc --noEmit` in `core`: clean.

## Demo path (real executor from this branch)

Script: `/tmp/s2-0c-r2/s2-0c-demo.test.ts`, not committed; run from `tests/js` with ts-mocha. Log: `/tmp/s2-0c-r2/demo.txt`. It starts a fresh local executor (`startAgent`, `LOCAL_MODE`), publishes and mounts the `mount-test` language fixture, the stand-in for stock Acorn, and drives `Ad4mModel.fromSHACL(<registered mounttest://Thing shape>, "MountThing")`. "Stock state" is the language's own record (`expression.get(…, alwaysGet = true)`) and its call log. The perspective mirror is not used for it.

| Step | Result |
|---|---|
| mount | `true`. Shapes: `MountThing` (alias) + `Thing` → `mounttest://Thing`. `hasCallActions()` = true |
| create (`save()`) | id `<lang>://t1` (the minted address). Language: `create {title:"a", tags:["x","y"]}`. Stock `{title:"a", done:false, tags:["x","y"]}` |
| update by returned id (`Thing.update(p, id, {title:"b"})`) | Language: `update t1 {title:"b"}`. Stock title `b` |
| clear collection to empty (`tags = []; save()`) | Language: `update t1 {tags:[]}`. Stock `tags: []`. Model reads `tags: []` |
| delete via a fresh `fromSHACL` class | Language: `delete t1 {}` (the destructor ran) |
| stock-state check | `expression.get` of `t1` = `null`. Full call log: create, update(title), update(tags: []), delete |
| resync (`syncMount`) | `{added: 0, removed: 0}`. `findAll` = `[]` |

The demo passed (1 passing). Without `alwaysGet`, `expression.get` answers from the executor's expression cache and shows the pre-update record. A reader of stock state must bypass that cache.

Setting up the demo: `tests/js/tst-tmp` had stale system-language bundles ("does not export init()"), so I ran `prepare-test-local`. It needs an `hc` on PATH (I used `/home/data/code/ad4m-sfu5/.hc-toolchain/bin`; local mode only symlinks it). It also needs the server-link and centralized-agent bundles: building them needs an `ad4m-ldk` build, so I copied them from `ad4m-calls-a` and removed them afterwards. The tracked files prepare rewrote (`deno.lock`, the two seed JSONs) are restored.

## Open

- The 5 MCP instance-tool failures above (they predate this round), and the in-process V8 abort when the whole `instances` filter runs in one process.
- `fromSHACL` still does not turn a scalar `min_count` into `required` metadata (Lal noted it). I left it out: `required` changes query conformance for every imported class, and delete no longer depends on it for call-backed shapes.
- `hasOne` clear on a call-backed class still uses the legacy remover (a scalar patch cannot say "no member").
- On a call-backed class, add/remove read the current members, then replace the whole list: a concurrent writer between the read and the write can be overwritten. The mount shape declares only a setter, so a full-list replace is its only collection write.
- A batched write on a call-backed class now fails with `callsInBatch` instead of silently running outside the batch. Callers that batch mounted writes must drop the batch.
- The shared release binary at `/home/data/code/ad4m-acorn-target/release/ad4m-executor` was rebuilt from this branch. An acorn rig executor (PID 754940, not mine) was running from that path; I left it alone, and it keeps its old inode.
