STATUS: DONE

# Acorn S2-0c round 3: the 5 MCP instance-tool failures

Branch `acorn/s2-0-instance-writes`, on top of round 2 (`255988e63`). Nothing
pushed. `CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`, `CARGO_BUILD_JOBS=4`.
Logs: `/tmp/s2-0c-r3/`.

## Where the tests come from

The 5 tests are in all three: a9 (`origin/acorn/a9-mount-lifecycle` = `c9d4a8de1`),
#1666 (`6c2ab01e8`), and the branch. `perspectives/instances/{validate,write,mod}.rs`
changed only in round 1 (`3cd30d37e`) and round 2 (`4bbdeac5f`, `95a433926`).
Since `afe89d126`, the only change in them is in those commits.

## Bisect (scratch branch, one hunk reverted per build, cumulative)

| Step | Hunk reverted | `mcp::tools::instances::tests` |
|---|---|---|
| head `255988e63` | none | 67 passed, 5 failed (`head-mcp.txt`) |
| A | `coerce_scalar` passthrough, `validate.rs:202-217` | 69 / 3. Fixed `a_property_…` and `template_update_…` (`bisect-A.txt`) |
| B | + `validate_properties` collections on Update, `validate.rs:327-352` | 71 / 1. Fixed `instance_tools_round_trip_typed_data` and `validation_names_…` (`bisect-B.txt`) |
| C | + create's synthesized collection replace, `write.rs:556-582` | 72 / 0 (`bisect-C.txt`) |

The same scratch state with A+B+C reverted against the Acorn suites
(`scratch-*.txt`): mount 60 ok. `call_actions` lost
`instance_writes::update_calls_the_setters_and_applies_the_delta`, the call-setter
collection replace (`tags`). `perspectives::instances::` lost round 1's
`update_replaces_a_collections_member_list`. So A and C can go back as they were.
B needs a narrower rule (below).

## Cause per test (file:line at `255988e63`)

- **validation_names_property_type_and_cardinality**: two causes.
  - First half: `["a","b"]` on `name` (`xsd:string`) passed, so only 1 error instead of 2. `perspectives/instances/validate.rs:202-217` (round 1's `literal:json:` passthrough for arrays and objects on `string`).
  - Second half: `messages` on Update was accepted, and the test expects `collectionNotSettable`. `validate.rs:327-352` (round 1: both modes take collection member lists).
- **a_property_stored_through_a_language_takes_the_value_the_language_does**: a file object on the literal `title` was accepted. Cause: `validate.rs:202-217`.
- **template_update_writes_nothing_the_class_schema_refuses**: `template_update` saved a file object into the literal `schema` property ("Saved as version 2"). Cause: `validate.rs:202-217`.
- **instance_tools_round_trip_typed_data**: `instance_update` with `messages` succeeded. The test expects the error that points at `instance_add_to_collection` (`tests.rs:1321`). Causes: `validate.rs:327-352`, plus `write.rs:722-735` (the setter-less replace in update).
- **create_failing_after_the_subject_is_staged_leaves_no_instance**: create wrote the setter-less `messages` through `replace_collection_without_setter` → `execute_commands` (`write.rs:556-582`), not through `add_link`. The #981 fault injected at the 4th `add_link` never fired, and the create succeeded.

## Commits

| Commit | Fixes | What |
|---|---|---|
| `1f3d2b920` fix(executor): a string property refuses arrays and objects again | A: validation (1st half), a_property, template_update | `coerce_scalar` is back to #1666's version. A property stored through a language still takes any value. |
| `a03b8b70a` fix(executor): create writes a setter-less collection as item links again | C: create_failing | One `add_link` per item into the create's batch, as on #1666. A call constructor still gets the whole record. A collection with a declared setter still goes through `create_subject`. |
| `f0fb1c1aa` fix(executor): instanceUpdate takes a collection only through a declared setter | B: round_trip, validation (2nd half) | See below. |
| (this commit) docs | | This file. |

### The B rule (the one judgement call)

The two sides wanted opposite things:
- #1666's tests: a collection on update is refused.
- Round 1's `update_replaces_a_collections_member_list`: a collection with **no setter** (`adder`/`remover` only) is replaced through a synthesized `collectionSetter`.

Acorn needs neither the refusal nor the setter-less replace. It needs a collection with a declared **call setter** to take its full member list, `[]` included. I checked Acorn's real shapes (`acorn-ad4m/language/src/shapes.ts:203-217`). Every writable property, collections included (`tags`, `topPriorityOutcomes`), gets the call setter. Read-only properties get none. The mount fixture has the same shape. So the rule is:

- `validate_properties(.., Update)` refuses every collection again (`collectionNotSettable`), as on #1666. The MCP template tools use this.
- New `validate_update(shape, props, settable)`. `write::update` passes the collections that `property_setter_kind` reports with an `ad4m://setter` (a call or a `collectionSetter`). Only those take a member list.
- The setter-less replace branch in update is gone. A scalar `hasOne` still writes as a one-member replace (`update_replaces_a_scalar_relation_member` passes).
- **Round 1's test changed (not weakened silently):** `update_replaces_a_collections_member_list` now uses `counts` with a declared `collectionSetter`. It still checks replace, `[]` clears, and that the untouched collection stays. It now also asserts that the setter-less `flags` is refused with `collectionNotSettable` and that nothing is written. Red line before the fix (`red-3.txt`): `a setter-less collection must be refused on update: Ok(InstanceUpdated { … updated_properties: ["flags"] })`.
- The `InstanceUpdate` doc comment says this now. `core/src/generated/api/InstanceUpdate.ts` and `PerspectiveInstanceUpdateParams.ts` are regenerated (doc text only, no type change).

What this costs the SDK: on a call-backed class, `callUpdate`/`callRelation` (`Ad4mModel.ts:1841`, `:2106`) send every forward collection as a full list. Before, a setter-less one was raw-replaced, bypassing the language. Now it is refused with `collectionNotSettable`. No Acorn or fixture shape has such a collection. Ordinary classes stay on the legacy writes (round 2), so they are not affected.

What round 1 loses (no Acorn or test dependency found):
- An `instanceCreate` of a link-constructor class no longer writes ordering entries for a setter-less ordered collection.
- An open `string` property no longer takes an object as `literal:json:` through `instance*`.

Both were for the ordinary-class SDK route that round 2 took back.

## Counts

| Suite | Before (`255988e63`) | After (`f0fb1c1aa`) |
|---|---|---|
| `mcp::tools::instances::tests` | 67 passed, 5 failed | **72 passed** |
| `call_actions` | 20 (r2 notes) | 20 passed |
| `perspectives::instances::` | 34 | 34 passed |
| `instance_writes` | 10 | 10 passed |
| `mount` | 60, 1 ignored | 60 passed, 1 ignored |
| `perspective_instances_ws` | 24 (r2 counted 25 with the next row) | 24 passed |
| `committed_sdk_rpc_methods_are_current` | 1 | 1 passed (after regenerating) |
| `instances` (whole filter) | SIGABRT; with `--skip mounted_records_are_instances` 149 passed, 5 failed | still SIGABRT (below); with the skip **154 passed, 0 failed** |
| core jest | 41 suites, 1378 passed, 1 skipped | 41 suites, 1378 passed, 1 skipped |

`rustfmt --check` is clean on the 4 changed `.rs` files, and the build adds no new warnings. The JS `test-model` suite and the demo were not rerun this round. The Rust write path for call-backed classes is covered by `instance_writes`, and ordinary classes do not reach the changed code from the SDK.

## The SIGABRT (`mounted_records_are_instances`)

- **The trigger is one test.** Bisecting the 72 MCP tests found it: `mcp::tools::instances::tests::the_template_documentation_is_served_to_anyone`, then `perspectives::perspective_instance::mounts_tests::mounted_records_are_instances`. With `--exact` on just those two, the process aborts. Every other subset passes. The `template` filter (46 tests, the doc test included, mount test last) also passes, so other tests in between avoid it.
- **gdb backtrace:** the abort is on thread `lang-QmzSYwde` in `JsCore::new_for_language` → `JsRuntime::new` → `v8::Isolate::New` → `Snapshot::Initialize` → `SharedHeapDeserializer::DeserializeStringTable` → `ReadReadOnlyHeapRef` (libc++ `vector[] index out of bounds`). At that moment the `bundle-template` thread (the template tool bundle's bare `JsRuntime`, `tool_bundles/sandbox.rs`) is still alive.
  - Likely reading: the first language isolate is created from Deno's snapshot while another isolate, made from a different snapshot, still holds V8's process-wide shared/read-only heap. Its shared string table then indexes the wrong read-only heap.
  - None of this code is in round 1, round 2 or the merge resolution: it is tool bundles (dev) against language runtimes.
- **6c2ab01e8 alone:** it has no mount tests (`mounted_records_are_instances` / `load_mount_test_language` are a9-only), so the abort cannot happen there in this form. I ran the doc test followed by each later-sorting language-touching test that #1666 has (`mcp::tools::neighbourhoods::*`, `page_before_resolve_tests::*`; binary `ad4m-acorn-base-target/…-01f1072283ef6781`). All passed, but they may never create a language isolate, so that proves nothing.
- **a9 alone:** not run. a9 has both tests, but no a9 test binary was built, and a build plus a Deno snapshot did not fit in the time cap. Since the mechanism involves none of the round-1/2 code, I expect a9 aborts the same way. That is a guess; confirm with `rust_executor-<a9> --test-threads=1 --exact <doc test> <mount test>`.
- **Not fixed:** this is V8 isolate/snapshot lifetime, not a small change. Possible directions:
  - the test side: have the doc test drop its bundle runtime, or make the mount helpers create a language isolate before any bundle runs;
  - the production side: create the bundle sandbox from the same snapshot family.

  Production may have the same order dependence: a template tool call before the first language loads. That is unlikely, since boot loads languages first.
