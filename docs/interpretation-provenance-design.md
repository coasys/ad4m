# Interpretation Provenance & "Suggested Data" — Design (for sign-off)

**Status:** DRAFT for Nico + James. Nothing implemented yet. Lands in **#883** (the write path) because it's the one property that can't be backfilled once #885 goes continuous.

**Constraint:** we do NOT assign per-write DIDs (needs agent-language changes, out of scope). We differentiate LLM-inferred vs human data with **link tags + a couple of subject classes**.

---

## 1. The problem

Interpreted instances/updates are written under the human's DID, indistinguishable from deliberate human assertions. Under continuous auto-processing that means:
- human corrections get silently overwritten by the next pass,
- you can't tell LLM data from human data → can't re-derive, upgrade, audit, or show "suggested" in the UI,
- it can't be backfilled later.

## 2. Mental model — Git-style staging (Nico's framing)

- **Accepted baseline** = the last state a *human* blessed — like the committed tree.
- **Proposed change** = what the LLM has inferred since — always diffable against the accepted baseline, like the working tree / an open PR.
- **Accept** = advance the accepted baseline to include the proposal (materialise).
- **Reject** = discard the proposal, keep the baseline.

Every LLM write is tagged so the diff *"LLM-proposed vs last human-accepted"* is always computable.

## 3. Scope: applies to CREATES **and** UPDATES

- **New instance** = a proposed *create* (the whole node is "unaccepted" until a human blesses or edits it).
- **Property change on an existing instance** = a proposed *update* on that property.

Both carry provenance. Neither is hidden — they're live and readable — but both are flagged *proposed* until accepted. (This directly answers "is the stamp only for new instances?" → no, updates too.)

## 4. Data shape

Mix of **plain link tags** (cheap, per-instance, queryable by predicate) and **two subject classes** (so the UI can `model_query` the review queue, per Nico's ask).

### 4a. `InterpretationRun` — subject class, one per pass
| prop | meaning |
|---|---|
| `run_id` (identity) | UUID of the pass |
| `model` | e.g. `gemma3:12b` |
| `prompt_version` | hash/id of system-prompt + few-shots (James's ask) |
| `ran_at` | timestamp |
| `agent` | DID it ran as |

### 4b. Per-instance provenance — link tags on the instance node
- `ad4m://inferred_by  → <run_id>`   (which run last produced/touched it)
- `ad4m://inferred_from → <source item ids>`  (the transcript turns it derived from)
- `ad4m://inferred_at  → <ts>`

Model + prompt_version are reachable via the run, so they're not repeated per node.

### 4c. `Suggestion` — subject class, one per pending proposed change
| prop | meaning |
|---|---|
| `target` | the instance node it modifies (or parent it would create under) |
| `kind` | `create` \| `update` |
| `prop` | for `update`: which property |
| `proposed_value` | the value the LLM wants |
| `run` | → `InterpretationRun` |
| `status` | `pending` \| `accepted` \| `rejected` |

UI: `model_query(Suggestion where status = pending)` → the whole review queue, searchable.

### 4d. Accepted baseline — per interpreted property
- `ad4m://accepted/<prop>  → { value, by <human DID>, at }`

The diff shown in the UI = *current value* vs *accepted value*.

## 5. Per-property lifecycle (the 3 states)

1. **Human-authored** (never LLM-touched): current = human value; no `inferred` stamp; `accepted` = current. → LLM may only **Suggest**, never overwrite.
2. **LLM-inferred, unaccepted**: current = inferred; `inferred` stamp present; `accepted` absent or ≠ current. → UI shows "suggested". The LLM may **refine its own inference in place** (overwrite) each pass — this is what keeps the rolling-summary working autonomously. Human can **Accept** (`accepted` := current) or **Reject** (revert to `accepted`, or delete a never-accepted create).
3. **Accepted, then LLM proposes a new change**: `accepted` = human-blessed value; a `Suggestion` holds the LLM's new value beside it. UI shows the diff. **Accept** → advance `accepted` + materialise; **Reject** → drop the `Suggestion`.

**The one rule that protects humans:** the LLM overwrites in place **only when the current value is still its own prior inference** (current == last-inferred, not human-diverged). The moment a human edits or accepts-then-the-LLM-differs, further LLM changes become **Suggestions**, never overwrites.

## 6. Accept semantics (Nico's question)

- **Accept** a suggestion → the proposed value is written to the real property and `accepted/<prop>` advances to it, authored/timestamped by the accepting human. It becomes ordinary human-owned data — **and can be edited again** (a later human edit just moves the baseline; a later LLM change becomes a new suggestion).
- So yes: "written as if the human did it, still changeable." Exactly the Git model — accepting is a commit; you can commit again later.

## 7. Autonomous vs. fully-staged — the one real decision

- **(A) Auto-materialise own inferences, stage only on conflict** *(recommended default)*: the LLM writes live and refines its own inferences with zero human clicks (auto-summary just works); only human-touched values get the Suggestion (staged) treatment. Provenance still tags everything so the UI can render "inferred, not yet human-accepted."
- **(B) Stage everything**: nothing the LLM writes is "real" until a human accepts. Safest, but needs a human in the loop for *anything* to materialise — kills fully-autonomous operation (e.g. a summarizer bot).

Recommendation: **(A) as default, (B) selectable per-processor** via config, since Flux's live-summary wants (A) but a "review before it lands" flow wants (B).

## 8. Open questions for James + Nico
1. **Link-tags vs subject-classes granularity.** Proposed hybrid: `InterpretationRun` + `Suggestion` as subject classes (queryable), per-instance `inferred_by`/`inferred_from`/`accepted` as plain predicates (cheap, still predicate-queryable). OK?
2. **Baseline granularity:** per-property `accepted` (precise — human edits one field, LLM keeps refining others) vs per-instance (simpler, coarser). Lean per-property.
3. **Default mode (A) vs (B)** and whether it's per-processor config.
4. **Reject of a never-accepted create:** hard-delete the node, or keep it with `status = rejected` for audit?
5. **Where does the accepted baseline live** for a brand-new inferred instance that's never been human-touched — implicitly "accepted = nothing" (whole node is a pending create) until first human action?

## 9. Incremental landing plan (once signed off)
1. `InterpretationRun` + the three `inferred_*` tags on every write (additive, non-breaking) — the un-backfillable core.
2. The "overwrite-only-your-own-inference" gate on Update routing (protects human edits) + `accepted/<prop>` on accept.
3. `Suggestion` subject class + accept/reject executor ops + the UI query surface (this part likely spans into #881's API).
