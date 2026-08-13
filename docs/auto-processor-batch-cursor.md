# AutoProcessor: keep the batch, persist processed turns

**Status:** DRAFT. Builds on the AutoProcessor watcher (P-B2) and the
[`InterpretationRun`](interpretation-provenance-design.md) provenance node.
Nothing in this doc is implemented yet.

Two related gaps in the polling watcher, fixed together:

1. The watch tick hashes SPARQL rows into IDs and throws the rows away.
   `run_one_pass` then re-runs `source_scope_query` and interprets whatever
   that returns *now* — not the drained batch. `batch_max` does not cap the
   LLM input.
2. “Already processed” is an in-memory `HashSet` on the watch loop. It is
   lost on restart, not shared with peers, and does not survive partitions.
   `source_scope_query` is only the conversation scope; it does not filter
   processed turns.

---

## 1. What the code does today

Every 500ms `run_auto_processor_tick`:

1. Runs `gather_transcript_sparql(cfg.source_scope_query)` →
   `Vec<(speaker, text)>`.
2. Hashes each row with `turn_id(speaker, text)` (SHA-256 prefix of
   `speaker || \0 || text`).
3. Feeds **only the hash** into `WatcherState::record_item`.
4. On drain, passes `item_ids: &[String]` into `run_one_pass`.
5. `run_one_pass` uses those IDs for the claim key, events, and author
   election — then **gathers the SPARQL again** and hands the full live
   transcript to interpretation.

After `Won` or `BackedOff`, the IDs go into a process-local
`processed_per_processor` set so the next tick does not re-enqueue them.
That set is not in the graph.

`ProcessingClaim` is a time-boxed lock on the **batch-key** (hash of the
whole ID set). It expires after `claim_ttl_ms`. It is not a durable
“these turns are done” record. A later batch with a different composition
(`[m1,m2]` vs `[m1,m2,m3]`) is a different claim node, so a partitioned
peer can re-interpret turns the other side already finished.

Interpretation `DedupStrategy` only dedups *output* instance identities
after the LLM has already run. It does not skip input turns.

---

## 2. Keep the batch in memory

`WatcherState` stores the turn payload next to the id, not the id alone.

```text
ProcessorPending.items: Vec<PendingTurn>
PendingTurn { id, speaker, text, timestamp }
```

`timestamp` is the link’s `ad4m://ontology/timestamp` (RFC3339), bound by
the scope query as `?timestamp`. It goes into `turn_id` (§3d), the LLM
prompt, and the window filter (§3e).

`record_item` still dedupes on `id` and only advances debounce on a
genuinely new id (the poller re-gathers the whole scope every tick).
`drain_ready_batch` still applies `debounce_ms` / `batch_min` /
`batch_max` / `max_wait_ms`, and returns `Vec<PendingTurn>` (FIFO, capped
at `batch_max`).

`run_one_pass` takes that vector:

- `item_ids` for claim / events / `batch_key` stay `turn.id`.
- `batch_authors` stay `turn.speaker` (no side map).
- The interpretation transcript is the drained `PendingTurn`s —
  **no second SPARQL**. Prompt turns are
  `{ speaker, text, timestamp }` (display names are a later follow-up).

The drained window is exactly what the LLM sees. A row that arrived after
drain waits for the next batch. A row that vanished from the live graph
after it was recorded is still in the batch (we claimed that content).
`batch_max` actually bounds LLM context.

`source_scope_query` remains “what is in this conversation”. It is run
once per tick to discover new turns, not again inside the pass.

---

## 3. Persist processed turns on `InterpretationRun`

The in-memory set is replaced by a neighbourhood-shared record of which
source turns this processor has consumed. That record lives on the run
node we already mint per pass — not a second copy on the AutoProcessor.

### 3a. Shape additions

`InterpretationRun` today: `run_id` · `model` · `prompt_version` ·
`ran_at`. Add:

| prop | meaning |
|---|---|
| `processor` | → the AutoProcessor instance (`ad4m://autoprocessor/<id>`). Scopes the cursor when several processors share a perspective. One-shot / manual interpretation omits it. |
| `sources` | collection of turn IDs this pass consumed (`ad4m://interp/sources` → `literal:string:<turn_id>`). |

This is the `sources` the provenance design already named for
re-derive / audit. It is also the processed cursor: the union of
`sources` across **in-window** runs of one processor (§3e).

No `processed` collection on `AutoProcessor`. Each AD4M link is a
reified statement plus author/timestamp metadata, full-arc gossiped.
Duplicating every turn ID onto the processor would bloat the store for
every later query, for a lookup that is already a cheap index walk.

The `processor` link is one extra link **per pass**, not per turn — the
join key, not a second copy of the IDs.

**Type predicate.** `InterpretationRun` is flagged with `ad4m://type` →
`ad4m://interpretation-run` (see its SDNA), **not** `rdf://type`.
`AutoProcessor` / `ProcessingClaim` use `rdf://type`. The cursor query
must match the run node’s actual predicate; do not “correct” it to
`rdf://type`.

### 3b. When it is written

On `PassOutcome::Won` only, after interpretation has applied. The run
is minted as today; the pass also sets `processor` and appends each
drained `turn.id` to `sources`.

`BackedOff` does **not** write sources and does **not** freeze IDs
locally. The winner’s `sources` arriving via link sync is what lets the
loser drop those turns. If the winner crashes, the claim TTL expires and
a peer may retry — correct, because nothing durable said “done”.

A **manual / one-shot** `run_interpretation` (no `processor` link) does
not participate in this cursor. If those turns also match an
AutoProcessor’s `source_scope_query`, the auto-processor will treat them
as unprocessed and run again. Output identity-dedup is the only guard.
That is intentional: the cursor is per-processor, not global “this text
has been interpreted.”

### 3c. `claim_ttl_ms` vs sync latency

Because BackedOff writes nothing, the loser depends on the winner’s
`sources` syncing in before its own claim expires. If `claim_ttl_ms` is
shorter than neighbourhood link-sync latency, the loser’s claim dies
first → a duplicate pass, caught only by output identity-dedup.

This is the same TTL-vs-sync window already documented on
`ProcessingClaim` (`claim.rs`). It is accepted, not newly introduced:

- Size `claim_ttl_ms` **≳ expected neighbourhood sync latency + a
  pass’s worst-case runtime**.
- Recommended default: **10 minutes** (tests today use 60s; that is
  tight for a real neighbourhood).
- The residual same-round race (both peers claim before either sees the
  other) still falls back to output dedup.

### 3d. Turn ID includes timestamp

```text
turn_id = SHA-256(speaker || \0 || text || \0 || timestamp)[..16]
```

Identical content at different times is a **new** turn. Two “yes” / “+1”
/ “ok” from the same speaker ten minutes apart must both be processed
(rolling summaries, task extraction, etc.). Collapsing them was an
accident of hashing `(speaker, text)` only; persisting that hash as a
cross-peer cursor would have made it a global, durable semantic.

`(speaker, text, timestamp)` still collapses a true duplicate of the
same link (same author, body, and link timestamp) — that is the intended
idempotency. Message URI would be stronger against clock-collision; we
are not requiring `?id` in the scope query. Timestamp is the minimum
extra identity that keeps content-hash IDs workable.

`source_scope_query` therefore binds **`?speaker`, `?text`, and
`?timestamp`**. The known-good query takes both speaker and timestamp
from the body-link reifier (`ad4m://ontology/author` /
`ad4m://ontology/timestamp`). A query that omits `?timestamp` is invalid
for the AutoProcessor gatherer (fail the tick with a log, do not silently
collapse). Apps may still bind `?speaker` from a model-specific author
property if they need a logical author other than the link signer.

### 3e. Bound both queries by a time window

An unbounded cursor — union of `sources` over every run forever — does
not scale. A long-lived Flux channel is 10k–100k turns; loading that
set on every cache-miss, and growing it forever in the graph scan, is
the real cost Query 2’s “two index hops” hid.

The same pressure already applies to `source_scope_query`: it cannot
return the whole channel history to the LLM. **Both** the scope and the
subtract must be recent-window operations.

`AutoProcessorConfig.source_window_ms` is **optional**. Omit the SDNA
property (`None`) for **no window**: every gathered turn is a candidate
and the cursor is the unbounded union of this processor's run sources.
Set a positive millis value to bound both sides. `<= 0` is invalid and
the processor is not loaded.

When set, the window is the engine-owned bound:

- **Scope (in Rust, after gather):** drop rows whose `timestamp` is
  older than `now - source_window_ms`. Apps should *also* `FILTER` in
  SPARQL so Oxigraph never materialises the rest; the Rust drop is the
  safety net when they don’t.
- **Cursor:** only load `sources` from runs of this processor whose
  `ran_at >= now - source_window_ms`. `ran_at` is already on
  `InterpretationRun` (unix millis, stored as a string).

Because `turn_id` includes timestamp, a message at `T` can only have
been processed by a run with `ran_at >= T`. Filtering `ran_at` to the
same window as the scope is therefore sufficient: in-window turns cannot
have been marked processed by out-of-window runs.

That proof is causal, not a hard exactly-once guarantee. `timestamp` is
the **author’s** clock (RFC3339 on the link reifier);
`ran_at` is the **processing peer’s** clock (unix millis). Under
cross-peer clock skew the stored numbers can invert at the window
boundary: a turn just inside the scope window, marked by a run whose
`ran_at` fell just outside → cursor misses it → one re-process. Bounded
by skew magnitude; output identity-dedup is the boundary fallback.

Expanding `source_window_ms` later can re-process turns that aged out of
the previous window (their `sources` are not loaded). That is the
compaction tradeoff; shrinking is always safe.

Old `InterpretationRun` nodes and their `sources` links may stay in the
graph for provenance/audit. They simply are not read by the tick.

### 3f. How the tick reads it

Two SPARQL queries per processor per tick (or on cache miss); subtract
in Rust. Do **not** wrap the user’s `source_scope_query` with
`FILTER NOT EXISTS` / SPARQL `SHA256`.

**Known-good scope query** (copy this; a missing `?timestamp` fails the
tick). Author and timestamp are **not** on the quoted triple. The store
writes a reifier node (`sparql_store.rs`): direct `?m <ns://body> ?text`,
plus `<link:HASH> rdf:reifies <<( ?m <ns://body> ?text )>>`, plus
`ad4m://ontology/author` and `ad4m://ontology/timestamp` on that reifier
(the same fields [`gather_transcript`] reads off the link expression).
Reach them through `rdf:reifies`:

```sparql
# 1. Scope — from AutoProcessorConfig (app-owned).
#    MUST bind ?speaker ?text ?timestamp.
#    SHOULD FILTER ?timestamp to the recent window.
SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
  FILTER (?timestamp >= "…rfc3339 of now - source_window_ms…")
}

# 2. Cursor — engine-owned. `ran_at` is OPTIONAL because it only matters
#    when a window is set, and is filtered in Rust: the stored value is a
#    `literal:string:` IRI, so `xsd:integer(?ran_at)` does not apply to it.
SELECT ?id ?ran_at WHERE {
  ?run <ad4m://type> <ad4m://interpretation-run> .
  ?run <ad4m://interp/processor> <ad4m://autoprocessor/{processor_id}> .
  ?run <ad4m://interp/sources> ?id .
  OPTIONAL { ?run <ad4m://interp/ran_at> ?ran_at }
}
```

With a window set, a row whose `ran_at` is missing or unparseable is
**dropped** rather than kept. The cursor is a suppression list, so the
safe direction is to re-process (output identity-dedup catches the
duplicate) rather than to retire a turn that was never interpreted.

(`<<( … )>>` is the RDF 1.2 quoted-triple shape this store’s own queries
use. Annotating the quoted triple directly with
`ad4m://ontology/timestamp` binds nothing.)

The watch loop may keep a RAM `HashSet` of processed IDs and refresh
when new run/source links land; that is a cache of (2), not a second
store, and not a substitute for the `ran_at` bound when a window is set.

Tick:

1. Gather transcript (query 1) → `(speaker, text, timestamp)` rows.
   Drop rows older than `source_window_ms` **when that field is set**.
2. Load processed IDs (query 2, or cache). Apply the `ran_at` window
   only when `source_window_ms` is set.
3. For each row, `id = turn_id(speaker, text, timestamp)`. If `id` is
   processed, skip. Else `record_item` with the full `PendingTurn`.
4. Drain a ready batch of `PendingTurn`s → `run_one_pass` (no re-gather).
5. On Won, write `processor` + `sources` on the new `InterpretationRun`.

`source_scope_query` stays an app concern (which conversation, plus a
time filter). Processed state stays engine bookkeeping. Apps do not
write `FILTER NOT EXISTS { processed }`.

---

## 4. Why per-item sources beat the claim as a cursor

| failure | claim (`batch_key` of the set) | `InterpretationRun.sources` |
|---|---|---|
| Executor restart | claim may have expired; in-memory set is gone → full scope looks new | in-window sources are in the shared graph → skip those IDs |
| Partition: A processed `[m1,m2]`, B later drains `[m1,m2,m3]` | different `batch_key` → B re-interprets `m1`,`m2` | B’s tick subtracts `m1`,`m2` → only `m3` is new |
| Claim TTL elapsed after a successful pass | same set is claimable again | sources still present (if still in-window) → no re-enqueue |
| Winner crashes mid-pass | TTL expiry lets another peer retry | no sources written → retry is correct |
| Same-round simultaneous claim | both may run the LLM (accepted residual race) | both may write sources for the same IDs (idempotent collection); output identity-dedup is still the safety net for duplicate instances |
| `claim_ttl_ms` < sync latency | loser retries before winner’s sources arrive → duplicate pass | same race; output dedup is the fallback (§3c) |

The claim stays the lock for “I am working on this batch *now*”. Sources
are the durable watermark after the lock expires.

---

## 5. Settled choices

1. **Keep the batch payload in `WatcherState`.** `run_one_pass` does not
   re-run `source_scope_query`. **Decided.**
2. **Processed IDs live only on `InterpretationRun.sources`.** No
   denormalized `AutoProcessor.processed` collection. **Decided.**
3. **`InterpretationRun.processor`** links a run to its AutoProcessor so
   the cursor SPARQL is scoped. Manual interpretation omits it and does
   **not** suppress a watching AutoProcessor (§3b). **Decided.**
4. **Write sources only on Won.** BackedOff waits for synced sources or
   claim TTL + retry. Size `claim_ttl_ms` ≳ sync latency + pass runtime
   (default 10 minutes). Residual race → output dedup. **Decided.**
   BackedOff additionally holds its ids back *locally* for `claim_ttl_ms`.
   The winner's `sources` are the durable record, but they only arrive
   once links sync; without the local hold the loser re-gathers and
   re-claims the same batch every debounce window, and `try_claim` writes
   its claim before reading the holders, so that is pure link churn. The
   TTL is the claim TTL: past it the winner's claim has expired anyway
   and retrying *is* the crashed-claimant recovery path.
5. **Subtract in Rust** (scope query + windowed cursor query). Do not
   fold the hash/filter into the user’s SPARQL. **Decided.**
6. **Turn ID is `hash(speaker, text, timestamp)`.** Identical content at
   different times is a new turn; the same link (same timestamp) is
   idempotent. `source_scope_query` must bind `?timestamp`. **Decided.**
7. **Optional `source_window_ms`.** Omit (`None`) = unbounded gather and
   cursor. When set, both scope and cursor are bounded by that window
   using turn `timestamp` and run `ran_at`. Window sufficiency holds
   modulo cross-peer clock skew; output-dedup is the boundary fallback.
   **Decided.**

---

## 6. Landing plan

1. Extend gather to require `?timestamp` via the **reifier** pattern
   (§3f); `PendingTurn` in `WatcherState`; drain returns payloads;
   `run_one_pass` interprets the drained transcript (including
   timestamps in the prompt). Update unit tests that feed bare IDs.
   Point `source_scope_query` docs at the known-good query so app
   authors do not have to reverse-engineer `rdf:reifies`.
2. `turn_id(speaker, text, timestamp)`. Tests: two identical bodies at
   different timestamps are distinct; the same triple is not.
3. Optional `source_window_ms` on `AutoProcessorConfig` (`None` → no
   window). When set, Rust drops out-of-window gathered rows and the
   cursor filters `ran_at`.
4. Extend `InterpretationRun` SDNA with `processor` (optional, single)
   and `sources` (collection). Wire both on AutoProcessor Won. Cursor
   query uses `ad4m://type` / `ad4m://interpretation-run`.
5. Replace `processed_per_processor` with the windowed cursor SPARQL
   (plus optional RAM cache of in-window IDs).
6. Tests: restart-equivalent empty RAM set still skips in-window sourced
   IDs; two processors do not share a cursor; `[m1,m2]` then `[m1,m2,m3]`
   only interprets `m3`; `batch_max` is the LLM transcript length;
   a turn older than `source_window_ms` is neither enqueued nor required
   in the cursor result; a one-shot run does not suppress auto; two
   “yes”s at different times both enqueue.

### Later PR (`feature/generic-extraction-ws-ts`)

Step 1 lands on `feature/interpretation-auto-processor`. The WS/TS client
branch does not exist here yet; rebase/merge this cut onto it and apply:

1. **`run_interpretation_handler`** — convert API
   `TranscriptTurn { speaker, text }` (ts-rs, unchanged) into
   interpretation `TranscriptTurn::from_speaker_text`. Do **not** add
   `timestamp` to the public WS type unless one-shot callers need it;
   AutoProcessor gather is the path that binds timestamp.
2. **`AddAutoProcessorRequest.source_scope_query`** (and
   `core/.../AutoProcessor.ts` `sourceScopeQuery` JSDoc) — document
   `SELECT ?speaker ?text ?timestamp` and point at
   `BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY` (reifier `ontology/author` +
   `ontology/timestamp`, not `ns://author`).
3. **JS tests** `SCOPE_QUERY` in `auto-processor.test.ts` and
   `auto-processor-neighbourhood.ts` — same reifier query as §3f,
   otherwise gather will fail the tick once this cut is merged.
4. **`sourceWindowMs?`** on `AddAutoProcessorConfig` / the WS request —
   optional; omit for no window (unbounded gather + cursor).
