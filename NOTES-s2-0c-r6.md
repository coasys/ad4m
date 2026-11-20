STATUS: DONE

# Acorn S2-0c round 6: the "collection replace writes through" live failure

Branch `acorn/s2-0-instance-writes`, on round 5 (`74e7c0964`). The tree was clean at the start. Nothing pushed.
`CARGO_TARGET_DIR=/home/data/code/ad4m-acorn-target`, `CARGO_BUILD_JOBS=4`. Logs: `/tmp/s2-0c-r6/`.

## Summary

**The executor did not regress.** We's "Untag" click sent **no RPC at all**. The SDK's static
`update()` compared `tags: []` with a read of the record taken from the perspective's mirror. That
read could still show `[]` because the preceding "Tag" had not reached the mirror yet, so the
update looked like "no change" and nothing was sent; stock kept the tag. It is a timing race: it
reproduces against round 3's and round 4's executor code alike, and its outcome follows the timing,
not the executor revision. The fix is in the SDK (`cdd52af8c`). **The We tarball
`.local/coasys-ad4m-0.13.1-staging.15-s2-0c.tgz` must be repacked from this branch's `core`**, and
the We web app rebuilt, to get it.

## What We sends (frame trace)

I ran the live suite against throwaway executor builds that log every `perspective.*` RPC with its
params: an env-gated `log::warn!` at `HandlerMap::dispatch`, built to `/tmp/s2-0c-r6/ad4m-executor-*-trace`,
with the source reverted after each build. The trace line never reached a commit.

- Tag: `perspective.instanceUpdate {className: "literal:string:shacl://AcornOutcome", baseUri: "<lang>://<hash>", properties: {tags: ["<lang>://<tag hash>"]}}`
- Untag, when it is sent: the same with `properties: {tags: []}`.
- In the failing run (`executor-r4-load.log`) only the Tag `instanceUpdate` appears. No Untag frame arrives in the 120 s before the next test's `instanceRemove`. In passing runs both appear.

The tarball's SDK (round-2 lib) path is:
1. We's `record.update` calls the model class's static `update(perspective, id, data)` (`lib/index.js:22690` in the tarball; `core/src/model/Ad4mModel.ts:2643` at `74e7c0964`).
2. It runs `new this(...)`, `get()`, `Object.assign(data)`, `save()`.
3. `save()` (call-backed: `callUpdate`) sends `changedValues()`, the fields that differ from the snapshot `get()` just took (`changedFields`, `Ad4mModel.ts:997`; tarball `:21230`).

A mounted record lives in its language. The perspective mirrors it from each call's delta (`apply_call_delta`). Stock, however, shows the change as soon as the zome call commits inside the language call, before that delta reaches the mirror. So:
- the test sees the tag on stock and clicks Untag;
- `get()` reads `tags: []` from a mirror that is not current yet, or from We's subscribed copy of the record;
- `[] == []`, so there is no change and no RPC.

## Round 3 vs round 4 executor

- **Code:** between `abe7e288f` and `be8751d54` the executor changed only:
  - `abstract_update_calls` (abstract classes only);
  - the call dispatch in `add_to_collection` / `remove_from_collection`.

  The concrete `instanceUpdate` path that Tag and Untag take is untouched (`git diff abe7e288f be8751d54 -- rust-executor/src`).
- **Runs** (rig + `node --test apps/we-web/tests/live/acorn.live.mjs`, `WE_DEV_FEATURES=true`, We unchanged):

| Executor | Box | Result |
|---|---|---|
| head (`74e7c0964` code) + trace line | quiet | 8/8 |
| round 4 (`be8751d54`) + trace line | quiet | 8/8 |
| round 4 (`be8751d54`) + trace line | under load (release `cargo test --no-run` rebuilds in a loop, `/tmp/s2-0c-r6/load.sh`) | **7/8**, Untag never sent |
| round 3 (`abe7e288f`) + trace line | under load | 8/8 |
| head, no trace line | quiet, run 1 | **7/8** |
| head, no trace line | quiet, run 2 | **7/8** |

- **Reading:** the result follows timing, not the executor revision. Round 4's code passes and fails depending on load, and even one log line per RPC moved head from failing to passing. The reported 3/3 failures against round 4 ran at about 05:10-05:17 local (stock timestamps 04:10:55Z / 04:11:35Z). Round 5's release test builds were running into the same target at that time (05:05-05:28), and that run's executor log showed the Acorn language queue backed up ("Execute waited 12051 ms in queue").

## Fix

| Commit | What |
|---|---|
| `cdd52af8c` fix(sdk): a static update of a call-backed class writes the fields it is given | On a class with call actions, static `update()` marks the keys of `data` as changed (`_given`, which `changedFields` reads), so they go out whatever the read shows. `_given` is cleared after `save()`. Ordinary classes keep the diff: their snapshot comes from the store they write, so it cannot lag, and an unchanged value still writes nothing. |
| `1bde07be5` test(executor): pin We's Untag frame on a mount | Rust `call_actions_tests::instance_writes::untag_by_the_alias_iri_clears_the_collection_in_the_language`: `instanceUpdate` by the alias IRI `literal:string:shacl://MountThing` with `tags: []` on a record with a tag. The language gets `update {tags: []}` and the record reads back with no tags. **It is a pin, not a red line.** The executor handled this frame correctly in rounds 3 to 5, so no executor test can fail before the fix; the failing test is in the SDK. |

## Red line (SDK, `red-sdk.txt`)

`core/src/model/call-backed-writes.test.ts › static update() writes the fields it is given (round 6: Acorn Untag)`, before `cdd52af8c`:
- `sends tags = [] even when the read already shows []`: no `instanceUpdate` at all (Expected `[{… properties: {tags: []}}]`, Received `[]`);
- `sends only the given fields`: likewise none.
- The new ordinary-class case (`a static update() of an unchanged value writes nothing`) passed before and after.

## Live results with the fix

We's installed SDK lib (`node_modules/.pnpm/@coasys+ad4m@file+.local+…tgz/node_modules/@coasys/ad4m/lib`) was replaced by this branch's `core/lib`, and `apps/we-web/dist` rebuilt (`pnpm --filter @we/app-web run build`). The rig used the release executor rebuilt from `cdd52af8c` (06:08). That commit only changed `core`, so the executor code is round 5's.

| Run | Result |
|---|---|
| quiet, run 1 | **8/8** (`live-fixed-1.txt`) |
| quiet, run 2 | **8/8** (`live-fixed-2.txt`) |
| under load | **8/8** (`live-fixed-load.txt`) |

The same executor with We's own SDK in the same conditions failed 2/2 (table above).

**We was then restored byte-for-byte:** both directories are back from `/tmp/s2-0c-r6/we-backup/` (`diff -r` identical). So **the We checkout still carries the round-2 SDK and still fails until the tarball is repacked**: `cd core && pnpm build && pnpm pack`, put the tarball at We's `.local/`, `pnpm install`, `pnpm build:web`. The repacked SDK also brings rounds 4 and 5 (declared adder/remover dispatch, `importedShape`, `languageCall` in the collection result types).

## Round-4 suites (after)

| Suite | Result |
|---|---|
| `mcp::tools::instances::tests` | 73 passed |
| `call_actions` | 24 passed (+1 pin) |
| `perspectives::instances::` | 34 passed |
| `instance_writes` | 14 passed (+1 pin) |
| `mount` | 60 passed, 1 ignored |
| `abstract` | 38 passed |
| `perspective_instances_ws` | 24 passed |
| `committed_sdk_rpc_methods_are_current` | 1 passed |
| core jest | 41 suites, **1387 passed** (+3), 1 skipped; `tsc --noEmit` clean |
| JS `test-model` | 310 passing, 0 failing, 31 pending |
| demo path (round 2's script) | 1 passing. Create → `<lang>://t1`, clear → stock `tags: []`, delete → stock `null`, resync `{0, 0}` |

`rustfmt --check` is clean on the changed `.rs` file. `prepare-test-local` was rerun as in rounds 4 and 5: the borrowed bundles were removed afterwards and the three rewritten tracked files restored.

## Process notes

- **Shared binary:** the shared release binary held my traced builds for a while. Its last build is from `cdd52af8c` without the trace line (06:08). The traced copies are only under `/tmp/s2-0c-r6/`.
- **Stuck wait loops:** two of my background wait loops used `pgrep -f` on a pattern that matched their own command line, so they never ended. One hit the background time limit; I killed the other (PID 1346853, mine). Neither held anything else.
- **Other sessions:** another session's `cargo test` (`polymorphic_ sanitise_alike`) was running at the same time; I did not touch it.

## Open

- **Stale reads for other writes:** the same stale-read hazard applies to an instance `save()` after a read on a call-backed class, i.e. a row fetched before another writer changed it. That is ordinary optimistic-write behaviour. Only static `update()`, an explicit patch, now ignores the read.
- **Unchanged from earlier rounds:**
  - a mixed SDK `save()` is not all-or-nothing;
  - the round-3 V8 SIGABRT;
  - the flaky a9 reifier test.
