# Consolidation notes — MCP tool surface (feat/static-instance-tools, PR #973)

Worker summary for Nico. Branch `feat/static-instance-tools`, base `dev`.
Nothing pushed. Commits listed at the bottom; review order = commit order.

## What was removed / ported (audit)

### `subjects.rs` (deleted)

| Old tool                 | Disposition                                                                                                   |
| ------------------------ | ------------------------------------------------------------------------------------------------------------- |
| `query_subjects`         | covered by `instance_query` (typed, filterable, paginated; old one returned bare address list)                 |
| `get_subject_data`       | covered by `instance_get`                                                                                     |
| `create_subject`         | covered by `instance_create` (validated, atomic; `base_uri` instead of `expression_address`)                  |
| `execute_commands`       | **kept**, moved verbatim to `instances/commands.rs` (Nico's explicit keep)                                    |
| `set_subject_property`   | covered by `instance_update`                                                                                  |
| `get_subject_collection` | covered by `instance_get` (collections come back resolved on the instance)                                    |
| `add_to_collection`      | covered by `instance_add_to_collection`                                                                       |
| `remove_from_collection` | **ported as `instance_remove_from_collection`** (was the missing counterpart)                                 |
| `get_subject_children`   | covered by `get_children` (unfiltered) + `instance_query(class_name, parent)` (the `child_class_name` filter) |
| `delete_subject`         | covered by `instance_remove` (same cascade, but refuses when class/URI don't match)                            |

### `children.rs` (deleted)

| Old tool                   | Disposition                                                                                                                      |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `add_child`                | **ported as `add_child`** (`instances/children.rs`) — same name; params `parent` / `child` (were `parent_address` / `child_address`) |
| `get_children`             | **ported as `get_children`** (`instances/children.rs`) — same name; param `parent`; children carry `id` (was `address`); new `limit` + `total_count` |
| `get_children_body_parsed` | **ported as `instance_transcript`** — same transcript output (timestamp, display name, DID, text), newest-N in chronological order; text property defaults to `body` then the class identity property, values resolved through `model_query` |

### `dynamic.rs` (kept behind `dynamicClassTools`; coverage check only)

| Per-class tool             | Static equivalent                                                        |
| -------------------------- | ------------------------------------------------------------------------ |
| `{class}_create`           | `instance_create` (incl. `parent`)                                       |
| `{class}_query`            | `instance_query`                                                         |
| `{class}_list`             | `instance_query(parent=…)` (+ `get_children` for the raw, unfiltered list) |
| `{class}_get`              | `instance_get`                                                           |
| `{class}_update` (dispatch-only, never advertised) | `instance_update`                                |
| `{class}_delete`           | `instance_remove`                                                        |
| `{class}_set_{prop}`       | `instance_update`                                                        |
| `{class}_get_{coll}`       | `instance_get` (collection is on the instance)                           |
| `{class}_add_{coll}`       | `instance_add_to_collection`                                             |
| `{class}_remove_{coll}`    | `instance_remove_from_collection` (new)                                  |

Untouched, as instructed: `execute_commands` (moved only), `describe_perspective`,
`describe_flows`, `get_models`, `infer`, the `dynamic_class_tools` flag machinery
and `dynamic.rs` itself.

## Decisions not covered by the brief (please review)

1. **Names for the children ops: kept `add_child` / `get_children`.** They are
   class-agnostic, the brief allowed those names, and they match the existing
   perspective-level verb_noun style (`add_link` / `query_links`). Their
   *parameters* moved to the static-surface vocabulary (`parent`, `child`,
   `id`), so callers of the old `parent_address` / `child_address` form need a
   one-word change. The JS tests were updated accordingly.
2. **`get_children_body_parsed` → `instance_transcript`.** The docs-skill
   branch flags exactly this as an open gap ("a formatted-transcript
   convenience tool … not yet in the static surface"). Class-aware, so it took
   the `instance_` prefix. Name is a judgement call; the mechanism does not
   depend on it.
3. **`link_target` now passes `literal:…` / `did:…` through** instead of
   literal-wrapping them again. Surfaced by the new `get_children` test: an id
   read back from the store (`literal:string:x`) was being re-wrapped to
   `literal:string:literal%3Astring%3Ax`. The old `maybe_encode_literal` had
   the same bug; it was invisible because add and get double-wrapped
   consistently. This also affects `instance_create(base_uri=…)` /
   `instance_query(parent=…)` for such inputs — strictly an improvement.
4. **`get_children` got a `limit` (default 100, max 500, most-recent-N) and
   `total_count`.** The old tool returned everything; unbounded output on a
   busy channel is the same LLM-context hazard `instance_query` already
   guards against. Small-set callers see no difference.
5. **`get_documentation` is in `AUTH_TOOLS`** (callable without a session
   token) — the setup doc is what tells a cold agent how to authenticate.
   The texts are static repo content, nothing perspective-scoped.
6. **Docs live in `rust-executor/src/mcp/docs/{overview,architecture,setup}.md`**
   (compiled in with `include_str!`) rather than pointing `include_str!` at
   `plugins/…`. The plugin skill files in this branch are the *old*
   (dynamic-tools) text and carry `ad4m_`-prefixed names and OpenClaw setup;
   `architecture.md` / `setup.md` were seeded from the rewritten versions on
   `docs/ad4m-skill-static-tools` (prefixes stripped, plugin-only paragraphs
   removed), `overview.md` was written fresh for the executor surface. Expect
   your plugin fold to supersede the text; the mechanism (topic enum, tests
   that every topic is non-trivial and that the overview names every static
   tool) is what matters.
7. **Commit order: split first, then port, then remove.** The brief lists
   remove before split; doing the split first made the port/remove diffs
   small and file-local. Same end state.
8. **JS tests** `mcp-http.test.ts` / `mcp-auth.test.ts` were *ported* (not
   deleted) to the static names, since they exercised the removed tools
   directly and would otherwise fail on first run. `mcp-static-tools.test.ts`
   untouched.

## Open questions

- `instance_query` has no `order` parameter; `model_query` supports
  `order: [["timestamp","desc"]]` natively, and the docs-skill SKILL.md
  already *claims* "most recent first" for `instance_query(parent=…)`, which
  is not what it does (default is timestamp ascending when `limit` is set).
  `instance_transcript` covers the "newest N" need; adding `order` to
  `instance_query` would be a five-line change if you want it.
- No `remove_child` was added (there was none before; `instance_remove`
  cascades, `instance_remove_from_collection` covers declared collections).
  Raw un-parenting of a non-instance node is only possible via `execute_commands`.
- `plugins/ad4m/skills/ad4m/references/mcp.md` and `SKILL.md` in *this*
  branch still document the removed tools — left for your plugin fold.

## Test results

All Rust runs in release mode, `--test-threads=1`, from `rust-executor/`.

### 1. `cargo test --release -p ad4m-executor mcp:: -- --test-threads=1`

After the port commit (`894d0b8e1`), before the removals:

```
test result: ok. 43 passed; 0 failed; 0 ignored; 0 measured; 1374 filtered out; finished in 0.44s
```

After the removal + `get_documentation` commits (`12bb3a192`, `0d859ec7f`) — the
final state of the branch (log: `.consolidation-mcp-tests3.log`):

```
test result: ok. 47 passed; 0 failed; 0 ignored; 0 measured; 1374 filtered out; finished in 0.42s
```

The 47 include the `side_effects` parity tests (every registered `#[tool]`
has a row, no stale rows), the new `instances/tests.rs` cases, and the four
`docs.rs` tests.

### 2. Full suite: `cargo test --release -- --test-threads=1` (log: `.consolidation-rust-full.log`)

```
test result: FAILED. 1386 passed; 1 failed; 34 ignored; 0 measured; 0 filtered out; finished in 334.30s
```

The one failure is `perspectives::interpretation_e2e::auto_processor_two_users_one_executor_no_double_processing`:

```
[e2e] interpretation against model 'gemma3:12b' at http://localhost:11434/v1
[e2e] attempt 3/3: winner (did:key:z6Mkr7qapsJeApzkanefZs7QNPXnZXc7oqQD78DaH2dEXEva) never signalled Processed
thread '…no_double_processing' panicked at rust-executor/src/perspectives/interpretation_e2e.rs:2269:5:
two-user (real background loop) no-double-processing e2e failed after 3 attempts
```

I am confident this is environmental, not caused by this branch:

- It is a real-LLM test against Ollama at `localhost:11434`. During this
  run Ollama was **down** (`curl http://localhost:11434/api/tags` → HTTP 000,
  no `ollama` process). The other 19 LLM e2e tests are `#[ignore = "llm-e2e"]`
  and did not run; the one `interpretation_e2e` test that passed needs no LLM.
- The interpretation pass narrows the harness tool surface to
  `<class>_`-prefixed read tools only (`run.rs`, `allowed_class_prefixes`), so
  none of the static tools this branch adds/removes/renames are visible to
  that LLM — the changed surface cannot reach this code path.
- The identical failure (same assertion, same "never signalled Processed"
  after 3 attempts) is in `.rust-tests.log` from 2026-09-06 14:07, i.e. a
  run **before** any consolidation commit; `.rust-tests3.log` / `.rust-tests4.log`
  from 15:17 the same day show it passing when the model was reachable.
- Re-running only that test in isolation on the final branch state
  reproduced the same 3/3 timeout (`finished in 282.30s`), consistent with
  the model being unreachable rather than a flaky race.

Please re-run that one test with Ollama up before merging; everything
else is green.

### 3. `cargo fmt --all` (repo root)

Ran after the last code commit: **no changes** — every touched file had
been `rustfmt`-ed per commit — so there is no separate fmt commit.
`cargo fmt --all -- --check` exits 0.

### 4. JS suites

`tests/js/tests/mcp-http.test.ts` / `mcp-auth.test.ts` were ported (commit
`f80b17a8f`) but **not executed** here: they need the freshly built
executor binary plus the prepared test languages, and the earlier JS runs in
this worktree (`.js-tests3.log`) were already at 62 passing / 15 failing on
the *unrelated* integration suite before this work. `mcp-static-tools.test.ts`
is untouched. Suggested check: `cd tests/js && pnpm run test-mcp-static &&
pnpm run test-mcp-http && pnpm run test-mcp-auth` after `cargo build --release`.

Not part of the brief, left as-is: the pre-existing working-tree
modifications to `tests/js/bootstrapSeed.json` / `publishBootstrapSeed.json`
(test-preparation artifacts) and the untracked `.*.log` / `.verify*` files.

## Commits (oldest first, all local, nothing pushed)

```
be9c1dc97 refactor(mcp): split instances.rs into an instances/ module
894d0b8e1 feat(mcp): port collection removal, children ops and the transcript reader to the static surface
12bb3a192 refactor(mcp)!: remove the *_subject tool family; the static instance_* surface replaces it
0d859ec7f feat(mcp): get_documentation tool + initialize instructions for cold agents
f80b17a8f test(js): port the MCP HTTP/auth suites to the static tool names
```

Review order = this order. The `!` on the removal commit marks the
breaking change for MCP clients still calling `*_subject` tools.
