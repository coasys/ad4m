# Option-A merge brief — #883 `feature/generic-extraction-tree`

You are a detached `claude` worker. Your task: **reconcile the tree-aware/upsert/relations write path from #883 onto the new `create_subject`-based interpretation architecture from #879 (post-#884), inside a single merge commit on branch `feature/generic-extraction-tree`**. This is "Option A" per Nico's decision 2026-08-07 16:54.

## Context (must-read)

- **Strategy doc:** `/home/data/clawd/memory/interpretation-framework-strategy.md`
- **Live state:** `/home/data/clawd/memory/working-buffer.md`
- **#883 PR body:** `gh pr view 883` (goal: tree-aware — attach/grow/create; upsert + relations)
- **#879 (base) is at `d9043fb10`** on remote `origin/feature/generic-extraction`. Interpretation module fully renamed; #884's `create_subject` migration already landed.
- **#883 (this branch) is at `9ff9e95e1`** on `origin/feature/generic-extraction-tree`. Still using OLD extraction/* architecture (pre-#884).
- **Merge-base:** `5b583857987ba47211f1ef1dc94124d12d280309`. #883 adds 1857 lines across 7 files vs this base.

## Your workspace

- Worktree: `/home/data/code/ad4m/.worktrees/extraction-tree`
- Branch: `feature/generic-extraction-tree`
- Repo: `/home/data/code/ad4m`
- Marvin LLM tunnel: `ssh -N -L 11434:localhost:11434 marvin@marvin &` (model `gemma3:12b`, base URL `http://localhost:11434/v1`).

## The merge

```
cd /home/data/code/ad4m/.worktrees/extraction-tree
git merge origin/feature/generic-extraction --no-commit --no-ff
```

You will get:

- **5 modify/delete conflicts** — #883 modified `extraction/{graph.rs,mod.rs,run.rs,tests.rs}` and `extraction_test_support.rs`; #879 DELETED all of these (renamed to `interpretation/*`). **You must `git rm` the old `extraction/*` files** and PORT #883's semantic additions to the `interpretation/*` files.
- **2 content conflicts** on `interpretation/prompt.rs` and `interpretation_e2e.rs` — files auto-created by the rename, both #879 and #883 modified them. Resolve inline.

## What #883 adds semantically (this is what you MUST preserve, renamed to `interpretation`)

### 1. `ProposedInstance.id` — upsert marker

```rust
pub struct ProposedInstance {
    pub class: String,
    #[serde(default)]
    pub id: Option<String>,   // <-- ADD THIS FIELD
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}
```

Presence of `id` means "update this existing instance's scalar fields"; absence means "create a new instance". Update leaves the type flag in place.

### 2. `ExtractionOp` enum → **rename to `InterpretationOp`**

```rust
pub enum InterpretationOp {
    Create { base: String, links: Vec<Link> },
    Update { base: String, set: Vec<Link> },  // patch scalars, keep type flag
    AddLinks { source: String, links: Vec<Link> },  // additive relations
}
```

### 3. `plan_extraction_ops_with_context` → **rename to `plan_interpretation_ops_with_context`**

Two-pass planner in `interpretation/graph.rs`:
- **Pass 1:** for each ProposedInstance, if `id.is_some()` emit `Update` (patch scalar links only), else emit `Create`.
- **Pass 2:** iterate relation properties per class; resolve relation refs — the LLM may emit either `new:<Class>:<n>` ordinals (referring to the nth Create op emitted for that class this run) OR existing instance ids — into real link targets. Emit `AddLinks { source, links: [(pred, target)] }` per resolved relation.

Helpers: `resolve_relation_links`, `normalize_refs`, `resolve_ref`, `ids_from_context`.

### 4. `strip_noop_updates`

Async fn in `interpretation/run.rs` — takes a `PerspectiveInstance` + `Vec<InterpretationOp>`, drops any `Update` whose new field values already match the perspective's current state per predicate (target set equality). See #883's diff for exact logic. Ignores Create/AddLinks.

### 5. `filter_already_present` modification

- **Skip when `inst.id.is_some()`** — upserts always survive.
- Filter **in place** (preserve LLM output order) so `new:<Class>:<n>` ordinals in Pass 2 line up correctly.

### 6. `existing_instance_context` — MERGE with #879's `existing_instance_identities`

**#879 currently has `existing_instance_identities` (returns `HashMap<class, HashSet<identity_value>>` for dedup).** **#883 wanted `existing_instance_context` (returns `Vec<{id, title, class}>` rows for prompt display + ref resolution).**

**Reconcile:** the interpretation now needs BOTH: dedup by identity property AND display of existing-instance IDs to the LLM (so it can reference them for updates/relations). Design: extend the query to return `{id, identity_value, class}` rows. Feed to the prompt (LLM sees the ids) AND derive the identity-value HashSet for `filter_already_present`. Callers: `build_interpretation_input`, `plan_interpretation_ops_with_context` (needs ids for `ids_from_context`), `filter_already_present` (needs identity-value sets).

### 7. Prompt additions (`interpretation/prompt.rs`)

- Per-class forward-relation rendering (the shape's `include_relations`).
- System-prompt Relations paragraph explaining ref syntax (`new:<Class>:<n>` + existing `id`).
- Existing-instance table now displays `id` per row (LLM references it for updates + relations).

### 8. Test SDNA (`interpretation_test_support.rs`)

Add relation-typed SDNA fixtures: `ConversationSubgroup {name, summary}`, `SemanticRelationship {relevance, subject, object}`, etc. Constructor + setter actions per SDNA (real subject classes — required by create_subject). See #883's `extraction_test_support.rs` +152 lines for the exact SDNA additions.

### 9. Unit tests (`interpretation/tests.rs`)

Port #883's 616 lines of tests, **rewritten against the new `create_subject`-based write path**. Tests for planner (`plan_interpretation_ops_with_context`), `strip_noop_updates`, `filter_already_present` order preservation, relations resolution. Delete any test that referenced deleted primitives (`instance_links`, `place_instances`, `apply_extraction_raw`).

### 10. E2E tests (`interpretation_e2e.rs`)

Port #883's 405 lines. Rename mechanically. These target real LLM behaviour (grouping/topics/relations) and should mostly work after rename since they exercise `run_interpretation` end-to-end. Run them against Marvin.

### 11. `run_interpretation` — new dispatch

Current `run_interpretation` in `interpretation/run.rs` (205 lines) does: parse → filter_already_present → create_subject each surviving instance in a batch.

**New shape:**
1. Parse (unchanged).
2. Build `existing` context (with ids + identity values).
3. `filter_already_present` (order-preserving, id-preserving).
4. `plan_interpretation_ops_with_context(instances, shapes, &existing_context)` → `Vec<InterpretationOp>`.
5. `strip_noop_updates(perspective, ops)`.
6. Execute ops inside one batch:
   - **Create:** `create_subject(SubjectClassOption { class_name }, base, initial_values, batch)` (unchanged).
   - **Update:** patch scalar links on an existing base — investigate whether this can go through `create_subject` (probably not — no constructor call needed). Likely: use `perspective.set_property` or write links directly (add + remove old scalars for the same predicate). **This is the trickiest reconciliation** — figure it out from the code in `perspective_instance.rs` and model_query getters/setters. The semantic is: same effect as `create_subject` would have for that predicate on that base, minus minting the type flag.
   - **AddLinks:** additive `perspective.add_link(link)` for each link in the op. No dedup.
7. commit_batch.
8. Read back links per instance (unchanged).

## Verification (in order)

```bash
cd /home/data/code/ad4m
export PATH="$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"

# 1. cargo check
cd .worktrees/extraction-tree
cargo check -p ad4m-executor --tests 2>&1 | tail -30

# 2. Unit tests (no LLM)
cd rust-executor
cargo test --release -p ad4m-executor --lib perspectives::interpretation -- --test-threads=1 2>&1 | tail -50

# 3. Full e2e vs Marvin (LLM). Tunnel first:
# ssh -N -L 11434:localhost:11434 marvin@marvin &
INTERPRETATION_E2E_BASE_URL=http://localhost:11434/v1 \
INTERPRETATION_E2E_MODEL=gemma3:12b \
cargo test --release -p ad4m-executor --lib perspectives::interpretation_e2e -- --test-threads=1 --nocapture 2>&1 | tail -80

# 4. Core tsc
cd /home/data/code/ad4m/core
npx tsc --noEmit 2>&1 | tail -10
```

## Commit + push discipline

- **Small commits per logical step** (planner, run.rs dispatch, prompt, tests, ...) into the merge branch. The FINAL commit is the merge commit itself with all conflicts resolved.
- **Commit sequence idea:**
  1. `wip: begin Option-A merge — accept #879 interpretation/* as base`
  2. `feat(interpretation): port upsert data model (ProposedInstance.id + InterpretationOp)`
  3. `feat(interpretation): port two-pass planner + relations resolver`
  4. `feat(interpretation): port strip_noop_updates + update dispatch in run.rs`
  5. `feat(interpretation): port prompt relations paragraph + per-class rendering`
  6. `feat(interpretation): port test SDNA + unit tests + e2e`
  7. Final: `merge feature/generic-extraction (Option A) — reconcile tree write path onto create_subject`
- **DO NOT PUSH** until all three verification steps pass. Then force-with-lease push `feature/generic-extraction-tree`.

## Rules of engagement (read the strategy doc)

- **Nico monitors progress via terse `[log]` lines.** Emit progress into `/tmp/t883-worker.log` at each phase transition; the main session polls it and relays to Nico.
- Write a sentinel `/tmp/t883-DONE.md` when finished (green + pushed) OR when hard-blocked (with the exact blocker).
- If create_subject can't cleanly handle updates, document the design decision in `PATH_A_UPDATE_DESIGN.md` in the worktree and proceed with the best option; do not spin.
- Preserve `owner` as NOT identity (from #879's identity/dedup work).
- Never call CI "green" — say "passes locally" for local.
- Rename `extractionHint`/`ExtractionHint`/`runExtraction`/`EXTRACTION_*` to interpretation variants everywhere you touch.
- Do NOT touch anything on #879 (`feature/generic-extraction`) or #881 — only your worktree.

## Start

1. `cd /home/data/code/ad4m/.worktrees/extraction-tree`
2. `git status` (verify clean start)
3. Read `/home/data/clawd/memory/interpretation-framework-strategy.md`
4. Read `/home/data/clawd/memory/working-buffer.md`
5. Run the merge command above; iterate.

Good luck. — Data (main session)
