STATUS: DONE

# Acorn S2-0c round 5 (addendum): Lal's review of round 3 (abe7e288f)

Branch `acorn/s2-0-instance-writes`, on round 4 (`6292ead85`). The tree was clean at the start. Nothing pushed.
`CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`, `CARGO_BUILD_JOBS=4`. Logs: `/tmp/s2-0c-r5/`.

Lal reviewed `abe7e288f` (round 3). Findings 1-3 of that review repeat round 2's and were fixed in round 4. Findings 2 (its MCP half) and 4 are new.

## Re-checks: the three repeated findings, at head `e9f81d53e`

| Lal r3 finding | Status at head | Fixed in | Test (passes at head, failed before its fix) |
|---|---|---|---|
| 1. P1 abstract preflight creates expressions before the refusal and twice on success | **Fixed** | `353e0ff6b` | `call_actions_tests::instance_writes::abstract_update_creates_each_expression_once` (was 2, now 1); `…::a_mixed_abstract_update_creates_no_expression_when_refused` (the refusal now comes before any `expressionCreate`; an accepted update makes exactly 1 `expressionCreate` + 1 `update` call) |
| 2. P1 call adder/remover not dispatched (SDK half) | **Fixed** | `96743f0e8` | Rust `call_actions_tests::instance_writes::collection_writes_run_a_call_adder_and_remover`; core `call-backed-writes.test.ts › a declared call adder or remover runs…` (add() runs the adder, remove() runs the remover, adder envelope per member, batch passed on). Lal's case of a link setter plus a call adder/remover is covered by the same rule: `declaredMemberWrite` looks at the declared adder/remover before any setter fallback. |
| 3. P2 decorated subclass inherits the call discriminator | **Fixed** | `be8751d54` | core `call-backed-writes.test.ts › a subclass reads the shape it addresses` (`a decorated subclass is its own link-backed class`, `an undecorated subclass is the imported class`) |

All of these pass in this round's runs (`after-instance_writes.txt`, `call-backed-verbose.txt`).

## New items

| Item | Commit | Fix |
|---|---|---|
| 1. P1 MCP `instance_add_to_collection` / `instance_remove_from_collection` on a call-backed collection | `42da20e85` fix(mcp): the collection tools say when a language call made the write | Since round 4 (`96743f0e8`) both tools reach the shared `add_to_collection` / `remove_from_collection`, which run a declared `call` adder/remover: the language runs, no membership link is written, and a caller's batch is refused. The reply still said only `links_added: 0` / `links_removed: 0`, which the tool description defined as a no-op ("already a member"). Now `CollectionItemAdded` / `CollectionItemRemoved` carry `languageCall` (true when the declared op is a call). The MCP replies add `language_call: true`, and both tool descriptions say what it means; the plugin's captured copies (`plugins/ad4m/staticToolDefs.ts`) are updated too. Generated TS is regenerated (`CollectionItemAdded.ts`, `CollectionItemRemoved.ts`), and the client mocks carry the field. |
| 2. P2 abstract `instanceUpdate` refuses a collection the record's class can set | `e9f81d53e` fix(executor): an abstract instanceUpdate sets a collection the record's class can set | `write::update` decided "settable" from `property_setter_kind` on the class named, before `is_abstract`. For `Folderish` that read the abstract shape's own setter, which does not exist. Now, for an abstract class, the record is found first (`require_instance`). A collection is settable when a class of the record it reaches declares a way to set it, chosen the same way `update_through_abstract` chooses: the class's setter, or a `collectionSetter` on its link adder's predicate (`abstract_collection_settable`: `actions_for(Set)` per write target, declared actions only, nothing resolved). Concrete classes are unchanged. Side effect: an abstract update of a missing record now answers `notFound` before validation. |

## Red lines (each test fails before its fix)

- **Item 1** (`red-1-head.txt`), `mcp::tools::instances::tests::collection_tools_run_a_call_adder_and_remover`, at `6292ead85`:
  `assertion failed: {"success":true,"class_name":"MountThing",…,"collection":"tags","item_uri":"x","links_added":0}`, `left: Null, right: true`. The reply did not say the language made the write.
  - The test also asserts that the language got `addTags` and `removeTags` with `{tags:["x"]}`.
  - The dispatch itself was red at round 3: round 4's `collection_writes_run_a_call_adder_and_remover` failed with `links_added: 1`, a raw link and no call (NOTES-s2-0c-r4.md). So at `abe7e288f` these MCP tools wrote raw links and the language never ran, as Lal said.
- **Item 2** (`red-2.txt`), `abstract_tests::instance_update_through_an_abstract_class_uses_the_concrete_collection_setter`:
  `Validation { class_name: "Folderish", errors: [ValidationError { property: "children", problem: CollectionNotSettable, … received: Some(Array ["ad4m://c1","ad4m://c2"]) }] }`.
  - After the fix, `f1`'s `ns://folder_child` is exactly `[c1, c2]` (the old `c0` is gone), and nothing is written to `ns://children`.

## Counts

| Suite | Round 4 | Round 5 |
|---|---|---|
| `mcp::tools::instances::tests` | 72 | **73** passed (+1 new) |
| `call_actions` | 23 | 23 passed |
| `perspectives::instances::` | 34 | 34 passed |
| `instance_writes` | 13 | 13 passed |
| `mount` | 60, 1 ignored | 60 passed, 1 ignored |
| `abstract` | 37 | **38** passed (+1 new) |
| `perspective_instances_ws` | 24 | 24 passed |
| `committed_sdk_rpc_methods_are_current` | 1 | 1 passed (after regenerating) |
| core jest | 1384 + 1 skipped | 41 suites, 1384 passed, 1 skipped |
| JS `test-model` (`LOCAL_MODE=true`, this branch's release executor) | 310 / 0 / 31 pending | **310 passing, 0 failing, 31 pending** |

- `rustfmt --check` is clean on every `.rs` file changed this round, and `tsc --noEmit` in `core` is clean.
- The a9 flake `a_foreign_reifier_of_a_mount_triple_survives` (see the round-4 notes) passed in this run.

## Demo path

- **Setup:** round 2's script, unchanged, with a release executor rebuilt from `e9f81d53e`. `core` was rebuilt too. Log: `/tmp/s2-0c-r5/demo.txt`. 1 passing.
- **Results:**
  - mount `true`;
  - create → `<lang>://t1`, language `create {title:"a", tags:["x","y"]}`;
  - update by id → `update t1 {title:"b"}`;
  - clear → `update t1 {tags:[]}`; stock and model `tags: []`;
  - delete via a fresh `fromSHACL` class → `delete t1 {}`;
  - stock `null`;
  - resync `{added:0, removed:0}`; no rows.
- **Environment:** `tst-tmp` was stale again (the round-4 `test-model` run rewrote it). I ran `prepare-test-local` as in round 4, with the server-link + centralized-agent bundles copied from `ad4m-calls-a` and removed afterwards. I restored the tracked files it rewrote (`deno.lock`, `tests/js/bootstrapSeed.json`, `tests/js/publishBootstrapSeed.json`).

## Open (unchanged)

- A mixed SDK `save()` is not all-or-nothing:
  - legacy reverse/`hasOne`-clear writes go first;
  - declared member writes go next;
  - the call patch goes last.
- The round-3 V8 SIGABRT (template tool-bundle isolate + first language isolate) is not fixed.
- The a9 reifier test is flaky, about 1 run in 4.
