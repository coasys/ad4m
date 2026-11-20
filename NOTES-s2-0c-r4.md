STATUS: DONE

# Acorn S2-0c round 4: Lal's round-2 review (255988e63)

Branch `acorn/s2-0-instance-writes`, on round 3 (`abe7e288f`). The tree was clean at the start. Nothing pushed.
`CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`, `CARGO_BUILD_JOBS=4`. Logs: `/tmp/s2-0c-r4/`.

## Commits

| Item | Commit | What |
|---|---|---|
| 1 (P1) | `353e0ff6b` fix(executor): classify an abstract instanceUpdate's calls from its declared actions | `abstract_update_calls` (`abstract_classes.rs`) no longer runs `update_through_abstract`. It walks the declared actions the same way: `actions_for` per target class, `Op::of`, and an empty adder/remover list makes no call. It resolves no value, so it creates no expression. `membership_names` is shared by both. The mount-test fixture now has a logged `expressionCreate`, so tests can count expressions. |
| 2 (P1) | `96743f0e8` fix: run a declared call adder or remover on add and remove | **Executor:** `add_to_collection` / `remove_from_collection` run a declared `call` adder / remover (`run_member_calls`, `call_actions.rs`). The call gets `{collection: [member]}`, as through an abstract class. A caller's batch is refused (`callsInBatch`) before the call. No membership link of ours is written; the language's delta writes it. **SDK:** on a call-backed row, `add`/`remove` and adder/remover envelopes in `save()` go to `instanceAddToCollection` / `instanceRemoveFromCollection` per member when the shape declares that op (`declaredMemberWrite`). A full-list replace through the setter is only the fallback when the shape does not declare it, and without a setter the executor refuses that replace (`collectionNotSettable`, round 3). The mount-test fixture can declare a call adder/remover for `tags` (`setShapes` `tags` override, interactions `addTags`/`removeTags`). |
| 2 | `3e5bd4c50` test(executor): the mount-test record offers addTags and removeTags | Follow-up: `languages::mount_tests::update_and_delete_are_offered_on_a_record_address` lists the fixture's record interactions, which now include the two new ones. |
| 3 (P2) | `be8751d54` fix(sdk): hasCallActions and getTargetClass read the same imported shape | New `importedShape()`: walk up to the nearest class with its own `sourceShape`, and stop at the first class decorated with `@Model`, which has its own generated shape. `getTargetClass`, `hasCallActions` (through `ownShape()`) and `declaredMemberWrite` all read it. Side effect: an undecorated subclass of an imported class now addresses the shape's `targetClass`. Before, it got the parent's generated `namespace + name`. |
| — | (this commit) docs | This file. |

Item 2 is one commit for both executor and SDK: the SDK route needs the executor change to do anything, and the reverse is true too.

## Red lines (each test fails before its fix)

Item 1 (`red-1.txt`), `call_actions_tests::instance_writes`:
- `abstract_update_creates_each_expression_once`: `left: 2, right: 1`. Two `expressionCreate "hi"`: the preflight, then the write.
- `a_mixed_abstract_update_creates_no_expression_when_refused`: the record is both a mounted Thing (call setter) and an `app://Note` (link setter, `resolve_language` = the fixture). With a caller batch, `expressionCreate "bye"` had already reached the language before `callsInBatch`. After the fix: none before the refusal. Unbatched: exactly one `expressionCreate` and one `update` call.

Item 2:
- Rust (`red-2rust.txt`): `collection_writes_run_a_call_adder_and_remover` got `Ok(CollectionItemAdded { … links_added: 1 … })` where `callsInBatch` was expected. It wrote a raw membership link into the caller's batch and called nothing.
- Core (`red-core.txt`): 4 of 4 failed in `call-backed-writes.test.ts › a declared call adder or remover runs`:
  - add(), remove() and the adder envelope sent a full-list `instanceUpdate` instead of `instanceAddToCollection` / `instanceRemoveFromCollection`;
  - the batch case did the same.

Item 3 (`red-core.txt`), `› a subclass reads the shape it addresses`:
- decorated subclass `hasCallActions()`: expected `false`, received `true`;
- undecorated subclass `getTargetClass()`: expected `mounttest://Thing`, received `mounttest://MountThing`.

## Counts

| Suite | Round 3 | Round 4 |
|---|---|---|
| `mcp::tools::instances::tests` | 72 | 72 passed |
| `call_actions` | 20 | 23 passed (+3 new) |
| `perspectives::instances::` | 34 | 34 passed |
| `instance_writes` | 10 | 13 passed (+3 new) |
| `mount` | 60, 1 ignored | 60 passed, 1 ignored (with the flake below) |
| `abstract` | — | 37 passed |
| `perspective_instances_ws` | 24 | 24 passed |
| `committed_sdk_rpc_methods_are_current` | 1 | 1 passed (no contract or doc type changed) |
| core jest | 41 suites, 1378 + 1 skipped | 41 suites, **1384 passed**, 1 skipped (+6 new) |
| JS `test-model` (`LOCAL_MODE=true`, this branch's release executor) | 310 / 0 / 31 pending (r2) | **310 passing, 0 failing, 31 pending** |

- `rustfmt --check` is clean on every `.rs` file changed this round, and `tsc --noEmit` in `core` is clean.
- **Flaky mount test, not from this round:** `mount_lifecycle_tests::a_foreign_reifier_of_a_mount_triple_survives` fails about 1 run in 4 in isolation. That is 5/20 at this head and **4/20 at round 3's `abe7e288f`** (I checked out its `rust-executor/src` and rebuilt). It is an a9 test. The user's link and the mount's mirror link can land in the same millisecond with the same triple and author, so they share one identity, and the sync removes the user's copy along with the mirror. It needs its own fix, in the a9 mount sync or in the test's timestamps.

## Demo path (real executor from this branch)

- **Setup:** round 2's script `/tmp/s2-0c-r2/s2-0c-demo.test.ts`, unchanged, with a release executor rebuilt from `be8751d54` (`cli cargo build --release`). `core` was rebuilt too. Log: `/tmp/s2-0c-r4/demo.txt`. 1 passing.

| Step | Result |
|---|---|
| mount | `true`. Shapes `MountThing` (alias) + `Thing` → `mounttest://Thing`. `hasCallActions` true |
| create | id `<lang>://t1`. Language `create {title:"a", tags:["x","y"]}`. Stock `{title:"a", done:false, tags:["x","y"]}` |
| update by id | Language `update t1 {title:"b"}`. Stock title `b` |
| clear to empty | Language `update t1 {tags:[]}`. Stock `tags: []`. Model reads `tags: []` |
| delete via a fresh fromSHACL class | Language `delete t1 {}` |
| stock-state check | `expression.get(t1, alwaysGet)` = `null`. Call log: create, update(title), update(tags: []), delete |
| resync | `{added: 0, removed: 0}`. `findAll` = `[]` |

- **Stand-in, not stock Acorn:** as Lal notes, this is the mount-test fixture. `acorn-ad4m/language/src/shapes.ts` gives every writable property a call setter and declares no adders, so items 2 and 3 do not change Acorn's own path.
- **Test environment:** `tests/js/tst-tmp` had stale system-language bundles again ("does not export init()"), so I ran `prepare-test-local`. It needs `hc` on PATH (`/home/data/code/ad4m-sfu5/.hc-toolchain/bin`) and the server-link + centralized-agent bundles, copied from `ad4m-calls-a` and removed afterwards. I restored the tracked files it rewrote (`deno.lock`, `tests/js/bootstrapSeed.json`, `tests/js/publishBootstrapSeed.json`).

## Process note

Before the `test-model` run I issued `pkill -9 -f "ad4m-acorn-target/release/ad4m-executor"`. That kills by path, not by my PIDs. A `pgrep` on that path just before showed no process. The pattern matched the background shell's own command line, which killed itself; I then reran `test-model` without it. No other process held that path at the time.

## Open

- Lal's note stands: `callUpdate` sends legacy relation fallbacks (reverse relations, a `hasOne` clear) before its call patch, and now also runs declared member writes before it. A mixed save is not all-or-nothing and should not be advertised as such.
- A declared **link** adder/remover on a call-backed class now goes through `instanceAddToCollection` / `instanceRemoveFromCollection`. Those write the membership link directly, not the declared link actions (an existing executor path). That is the same link for an `addLink` adder.
- The SIGABRT from round 3 (template tool-bundle isolate + first language isolate) is unchanged.
