# Interpretation Provenance & "Suggested Data" — Design v2 (for sign-off)

**Status:** DRAFT for Nico + James. Nothing implemented yet. Lands in **#883** (the write path) — it's the one property that can't be backfilled once #885 goes continuous.

**Constraint:** no per-write DIDs (needs agent-language changes, out of scope). We differentiate LLM vs human data with **subject classes** — kept as close as possible to "just writing/updating subject classes", no bespoke link magic.

---

## 1. The problem
Interpreted instances/updates are written under the human's DID, indistinguishable from deliberate human assertions. Under continuous auto-processing that means human corrections get silently overwritten, you can't tell LLM data from human data (no re-derive / upgrade / audit / "suggested" UI), and it can't be backfilled later.

## 2. Core idea — an **overlay subject class** on the same base

The whole mechanism is **one extra subject class instantiated over the *same base URI* as the instance the LLM writes** — an `InterpretationOverlay`. It carries the provenance + the LLM's value snapshot + (for updates) the proposed new values. A node can conform to several subject classes at once, so a node is e.g. *both* a `Task` **and** an `InterpretationOverlay`.

Git-staging analogy holds, but realised as the overlay:
- The **real instance** = the working data readers/UI see and edit normally.
- The **overlay** = the "this came from the LLM / here's what it proposed" layer, diffable against the real instance.
- **Accept** = drop the overlay (create) / carry its value over then drop it (update) → plain accepted data.
- **Reject** = drop the overlay (+ the base, for a not-yet-accepted create).

This keeps it all subject-class read/write + `model_query`, and makes acceptance a *link deletion*, not a rewrite — minimal churn.

## 3. Two subject classes

### 3a. `InterpretationRun` — one per pass
`run_id` (identity) · `model` (e.g. gemma3:12b) · `prompt_version` (hash of system-prompt + few-shots) · `ran_at` · `agent` (DID).

### 3b. `InterpretationOverlay` — instantiated over the instance's base URI
| prop | meaning |
|---|---|
| `run` | → the `InterpretationRun` that wrote it |
| `kind` | `create` \| `update` — how the engine knows it minted the node vs patched an existing one |
| `inferred/<predicate>` → value | the LLM's value for each affected property. For a **create**: a snapshot of every value written. For an **update**: the proposed new value(s). Parallel predicates (`ad4m://interp/inferred/<realPredicate>`) so they never collide with the real property links and stay `model_query`-able. |

That's it — the overlay is provenance **and** last-inferred baseline **and** suggestion, in one instance. No separate `Suggestion` node, no `accepted/<prop>` shadow.

## 4. How it behaves (per write)

Every LLM write instantiates/updates the overlay over the base. Behaviour depends on `write_mode` (§6) and whether the value is still the LLM's own:

**Create (new instance):**
- Write the real instance (its real class + values) **and** an overlay `{kind: create, run, inferred/<p> = <same values>}`.
- The overlay-with-identical-values is how we know it's LLM-created *and* untouched. Humans read/edit the instance normally; they never touch the overlay.
- **Human-change detection:** real `<p>` == overlay `inferred/<p>` → still the LLM's; real `<p>` ≠ overlay `inferred/<p>` → a human edited it. (Answers the old "baseline for a brand-new LLM instance" question — the overlay *is* the baseline.)

**Update (existing instance):**
- If the target value is still the LLM's own (real == overlay `inferred/<p>`) **and** mode is `AutoMaterialize`: overwrite the real prop in place *and* bump overlay `inferred/<p>` to match (the LLM refining its own inference — rolling-summary keeps working autonomously, zero clicks).
- If a human has diverged (real ≠ overlay `inferred/<p>`), **or** mode is `Stage`: **leave the real prop untouched** and set overlay `inferred/<p> = <proposed new value>`. UI shows real (kept) vs overlay (suggested).

**The one rule that protects humans:** the engine only overwrites a real value in place when it's still identical to what the overlay last recorded. The instant a human changes it, further LLM changes go into the overlay as suggestions, never overwrite.

## 5. Accept / reject

- **Accept a create** → delete the overlay. The instance remains as plain, human-owned data (editable again; a later LLM change re-adds an overlay suggestion).
- **Accept an update** → copy overlay `inferred/<p>` → real `<p>`, then remove that overlay suggestion (remove the whole overlay if nothing else pending).
- **Reject a create** → delete the base node + overlay (if never accepted).
- **Reject an update** → remove the overlay suggestion, leaving the real (human) value.

All four are pure link add/removes over subject classes — the executor exposes `acceptInterpretation(base [,prop])` / `rejectInterpretation(base [,prop])` ops (surface likely spans into #881's API for the UI).

## 6. `write_mode` parameter (DECIDED — both modes)
- **`AutoMaterialize`** *(default)*: LLM writes live, refines its own inferences with zero clicks; only human-diverged values get staged into the overlay (§4).
- **`Stage`**: nothing materialises directly — every create/update stays in the overlay as a suggestion until a human accepts.

Plumbing: Rust `write_mode: WriteMode` param on `run_interpretation[_with_strategy]` (default `AutoMaterialize` → existing callers unchanged); `ad4m://write_mode` on `AutoProcessorConfig` (#885); same option on the TS `AutoProcessor` / `runInterpretation` (#881). "Live summary" → AutoMaterialize; "suggest for review" → Stage — same engine, one flag.

## 7. Why this is nice
- **All subject classes** → the UI queries `model_query(InterpretationOverlay)` for "all proposed/AI data", filters `kind`, joins `run` for model/prompt version. No special link scanning.
- **Minimal churn** → acceptance deletes the overlay; the instance links are untouched.
- **Baseline is free** → the overlay's `inferred/<p>` doubles as "what the LLM last wrote", so human-vs-LLM diffs need no separate shadow.
- **Re-derivable / auditable** → `run` → model + prompt_version + sources.

## 8. Remaining small choices (I'll default unless you say otherwise)
1. **Inferred-value encoding:** parallel `ad4m://interp/inferred/<predicate>` links (RDF-clean, queryable) — *recommended* — vs a single JSON snapshot prop (fewer links, opaque). Lean parallel links.
2. **Stage-mode create materialisation:** write the real instance immediately but flagged by the overlay (visible/queryable, "pending"), vs hold the values only in the overlay until accept (base not a real `Task` until accepted). Lean *write-then-flag* so the UI can render it in place. 
3. **Overlay identity** across passes: one overlay per base (updated in place each pass) — recommended — vs one per run (history). Lean one-per-base; the `run` link records the latest producer.

## 9. Landing plan (once signed off)
1. `InterpretationRun` + `InterpretationOverlay` subject classes (hard-wired SDNA) + instantiate the overlay on every create/update with `inferred/<p>` snapshot — additive, non-breaking (readers ignore the overlay).
2. The human-diverged gate on Update routing (real vs overlay `inferred/<p>`) + `write_mode`.
3. `acceptInterpretation` / `rejectInterpretation` executor ops + the UI/API query surface (into #881).
