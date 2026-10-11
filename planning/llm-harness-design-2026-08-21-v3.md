# LLM harness — design v3

*2026-08-21, revision after Nico↔James call ~12:11 CEST. Supersedes [[planning/llm-harness-design-2026-08-21-v2.md]]. Delta-only where possible — refer to v2 for unchanged sections.*

**What changed from v2**

1. **Writes ARE exposed — as propose-tools, not commit-tools.** LLM can propose creates and link-attachments; they land as overlays (`InterpretationSubjectProposal` / `InterpretationLinkOverlay`), not as committed graph mutations. v2's "no write tools" was wrong.
2. **Relation-shaped read tools.** `{Class}_children_via_{relation}` per has-many (walks the user-defined ontology), not the generic `{Class}_get_{coll}`.
3. **Run-level accept is the MVP semantics.** Accept/reject one whole `InterpretationRun`; per-overlay accept deferred (would ripple into schema + API).
4. **Branch strategy corrected.** Base on `feat/openai-compat-endpoint` (#854), **merge #881 in without touching it**, build on top. v2's "no stacking" line was wrong.
5. **Skill/system-prompt copy is where MCP tool-set gaps get spotted.** Nico's open ask — do it while writing the copy, not before.
6. **Cross-work coordination flagged**: depends on has_one/has_many resolution ([[project_relations_has_one_has_many]]); coordinate with James's weekend @ada-model PRs.

Everything else in v2 (OpenAI wire shape, grammar-constrained decoding via Josh's `tool_grammar.rs`, reuse MCP `#[tool]` macros as single source of truth, `ToolProvider` trait, harness loop shape, tool cap default 20, skip Task B) stands as written.

---

## 1. Revised tool surface

### Per-class dynamic (generated from SHACL registration + relation graph)

**Reads:**
- `{Class}_query(filter)` — filter-based lookup for dedup + reference
- `{Class}_get(id)` — hydrate one instance
- `{Class}_children_via_{relation}(id)` — one tool per has-many/has-one relation, ontology-shaped (`Channel_messages(channel_id)` beats `query_links(source=channel_id, predicate="message")`). Saves tokens and matches how the user modelled the world.

**Writes (propose-only, overlay-emitting):**
- `{Class}_propose_create(props)` → writes `InterpretationSubjectProposal` under current `InterpretationRun`. No graph mutation.
- `{Class}_propose_link_child(parent_id, relation, child_id_or_proposal_ref)` → writes `InterpretationLinkOverlay`. Handles both link-to-existing and link-to-just-proposed.

**Always-on (non-class-specific):**
- `list_subject_classes` — filtered to classes in scope for this run
- `get_children(id)` — generic fallback for ontology-untyped nav

### Deliberately out of scope

- Direct writes to the underlying perspective (no `add_link`, `add_perspective`, `_create`, `_delete`, `_set_*`). LLM only proposes; commits happen on run accept.
- Neighbourhood publish/join, auth, profile, language install. Orthogonal.
- Flow tools — flow-runner is a separate consumer; extraction pass doesn't touch flows.

### Per-flow-step allowlist (unchanged from v2)

A flow step can narrow the tool set (e.g. summarisation step → no tools; rebuttal step → read tools only). Config on the flow step, not global.

---

## 2. Accept semantics — MVP

- **Run-level accept only.** UI shows the full `InterpretationRun` (proposals + overlays); user accepts or rejects the whole run.
- **On accept:** each `InterpretationSubjectProposal` becomes a real subject instance; each `InterpretationLinkOverlay` becomes a real link. Run marked accepted.
- **On reject:** overlays/proposals deleted. No graph change.
- **Per-overlay accept:** deferred. Requires schema field for per-overlay state + API surface. Not in this PR.

Ties into [[project_interpretation_overlay_accept_semantics]] — accept = commit + delete overlay; the "no accepted-flag" decision from 08-12 still holds.

---

## 3. Branch + rollout (corrected)

1. **Base branch:** `feat/openai-compat-endpoint` (#854). Currently in review; Josh + Lal + Marvin have resolved change requests; Nico wants to eyeball before merging.
2. **Merge #881 into the harness branch.** Do not touch #881's commits. This is the [[feedback_stacked_pr_no_cross_cherrypick]] pattern: land fixes on merge-first branch, downstream picks them up via merge, no cross-cherry-pick.
3. **Cherry-pick from Josh's stacked AI-service PR:** `tool_grammar.rs` (grammar-constrained decoding for local kalosm) and `/v1` tool-calling extensions. Flag to Josh in advance so he can steer away from anything he'd rather I leave alone.
4. **Harness PR:** single PR, based on the above, targets `dev` after #906 → #881 → #854 → AI-service-stack all merge.
5. **CI:** every push runs the standard integration matrix on Marvin. Real-LLM e2e (§5 in v2) runs via `-L 11434:` tunnel to Marvin's Ollama.

Order of operations: **finish today's #906+#881+harness harness-branch scaffolding while CI infra is broken; push draft PR with design doc + skeleton once Marvin is back; wait for #906/#881 to land before real implementation work.**

---

## 4. Dependencies + coordination

- **[[project_relations_has_one_has_many]]** — dynamic tool generation reads the relation semantics. If has_one/has_many gets refactored, `{Class}_children_via_{relation}` generator changes with it. Track that PR's shape before finalising tool naming.
- **James's weekend @ada-model PRs** — both areas touch relation hydration. Sync with him before finalising tool signatures for `{Class}_children_via_{relation}`. Not a blocker, but a courtesy check.
- **Josh's AI-service stacked PR** — cherry-pick source; wait for it to land or fork just the two files (`tool_grammar.rs` + `/v1` tool-calling handler diffs) if it drags.
- **Nico's eyeball on #906 + #854** before merges. Not autonomous.

---

## 5. Nico's open ask — MCP tool-set gap audit

**Do this while writing the system-prompt / skill copy for the harness**, not before. The copy is where you notice "the LLM needs to do X and there's no tool for it." Rush the copy first-draft, list every tool it would want to reach for, cross-reference against the actual MCP surface, and propose additions in a follow-up commit or issue.

Candidates already suspected:
- **`link_query_by_predicate_and_target(perspective, predicate, target)`** — reverse-index lookup is currently N calls of `query_links`. Genuinely missing.
- **`class_of(subject_id)`** — LLM often needs "what class is this instance?" before deciding which `{Class}_*` tools to call. Cheap and obvious.
- **`interpretation_run_state()`** — LLM should be able to see what it's already proposed in this run before proposing a dup. Might be self-referential enough to just include in the system prompt each iteration; decide when writing the loop.

None of these block v3 — flag them in the PR body as follow-up work.

---

## 6. Everything unchanged from v2

- OpenAI tool-calling wire shape (`tools`/`tool_choice` on request, `tool_calls[]` on response, `role: "tool"` results). No sentinel fenced blocks.
- Grammar-constrained decoding via Josh's `tool_grammar.rs` for local kalosm models.
- `ToolProvider` trait as the shared seam between MCP transport and harness. Single source of truth for tool metadata + dispatch.
- Harness loop shape (§2 in v2): loop up to `max_calls`, prompt with tools, execute tool_calls, append tool results, terminate when LLM returns no tool_calls or cap hit.
- `AutoProcessorConfig.maxToolCalls: Option<u32>` default 20; `AutoProcessorConfig.disableHarness: Option<bool>` default None (harness always on).
- No tool caching. Not worth the LRU + invalidation bookkeeping.
- Sizing: ~6-7 days. Skip Task B — every line of Task B becomes part of `{Class}_query` or `{Class}_children_via_{relation}`.
- `chat_gpt_lib_rs` `tools:` support on `ChatInput` — verify at implementation time; may add 0.5-1 day if crate bump/switch needed.
- Test plan (unit + real-LLM e2e on Marvin) — extend with:
  - `e2e_proposes_subject_via_class_propose_create` (asserts overlay written, no graph mutation)
  - `e2e_proposes_link_child_via_relation_tool` (asserts overlay written under correct relation)
  - `e2e_run_accept_commits_all_overlays` (asserts overlays materialise into real subjects + links)
  - `e2e_run_reject_discards_all_overlays` (asserts no graph mutation, overlays deleted)

---

## 7. Open questions

1. **`chat_gpt_lib_rs` — does the pinned version accept `tools:` on `ChatInput`?** Verify at impl time. Impact: 0-1 day.
2. **Cherry-pick or wait for Josh's AI-service stacked PR?** Depends on timing. Cherry-pick `tool_grammar.rs` + `/v1` handler diffs if it drags >3 days.
3. **`{Class}_propose_create` — does it need to enforce SHACL validation on the props?** Probably yes (garbage in = garbage overlay). Cheap: reuse SHACL validation already in the write path, just short-circuit before commit.
4. **How does the LLM see other overlays already proposed in this run?** Include serialised `InterpretationRun` snapshot in system prompt each iteration vs. new tool `interpretation_run_state()`? Decide during loop implementation.
5. **Per-model max-tools trim for small local models** (gemma3:12b, phi-3) — defer until we see it fail.

None block starting once #906/#881/#854 land.

---

*Nico: this replaces v2. Six deltas in the header, tool surface in §1, accept semantics in §2, branch plan in §3, coordination in §4, your gap-audit ask in §5. If anything's wrong or missed, say so and I'll iterate before touching code. Not writing code until #906/#881/#854 are on `dev`.*
