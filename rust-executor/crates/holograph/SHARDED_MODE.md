# `SHARDED_MODE.md` — Holograph v1.5 / v2 design

> **Status:** design doc. This is the spec the v1.5 sharded-mode work will
> follow. **Currently shipped Holograph is v1**: FULL replication on every
> peer, single-doc-per-space, two op classes wired but not yet sharded. v2
> is the multi-document-routing follow-on whose op-id derivation changes
> are forward-incompatible with v1 — see §4.
>
> **North star:** `HOLOGRAPH.md` (lives in the project's design docs, not in
> the crate tree). This file extends it with the sharding-specific design
> choices that needed pinning down once Wake-19 (PR-E) committed the
> two-op-class scaffold in code.

## 0. TL;DR

- **Validation regime (v1.5):** **Option A — arc-local validation.** Each
  peer validates only the ancestry ops whose loc falls inside its declared
  storage arc. Head ops carry signatures peers trust without re-validating
  the underlying ancestry. Failure mode and mitigation in §3.
- **Multi-doc routing (v2):** extend the 4-byte op-id trailer to 8 bytes
  carrying `(doc_id_hash[4], op_class_tag[4])`. v1 trailers stay valid as
  "default doc, class X". The op-id derivation change is the forward-
  incompatibility break and forces a snapshot-boundary upgrade. Details
  in §4.
- **Receipt model (v1.5):** silent fetch + retry under D's
  `FetchFallbackPolicy`. "Missed-arc" reputation / SF slashing receipts are
  v2-or-later. §5.
- **Locked-in by v1 / Wake-19** (constraints v1.5 must respect): two op
  classes encoded in the op-id trailer; Head loc = 0 via
  `set_loc_callback`; dominance walk in `KvOpStore`; sled is the storage
  backend; `holograph_envelope_decoder` is the canonical op decoder. §2.
- **Cross-author Head conflict** (open question surfaced while writing
  §3): two peers concurrently publishing Heads with the same Ancestry
  target both survive in `current_heads()`. v1.5 needs an explicit
  tie-breaker rule. §6.

## 1. Scope

This doc covers Holograph's storage + replication design for two coming
phases:

- **v1.5** — turn on partial-arc replication. Same single-doc-per-space
  semantics as v1, but peers declare non-FULL storage arcs and the K2
  gossip / fetch path filters by arc. Op-class routing (Wake-19's
  loc-callback) starts mattering: Heads continue to FULL-replicate via
  the loc-0 sector, Ancestry shards by arc-overlap. This is the first
  point at which Holograph behaves differently from a "K2 with a
  perspective-diff op-store" baseline.
- **v2** — multi-document-per-space. Today every K2 Space holds one
  perspective document; v2 lifts that. Forces an op-id derivation
  change because `doc_id` joins the trailer. Upgrade plan in §4.

Out of scope:

- Synergy-Fuel mint/slash mechanics. Holograph is substrate-neutral;
  receipts are economic primitives whose policy lives elsewhere.
- SHACL validation rules. Lives in the Language layer.
- Graph DIDs. Separate Living Web spec; orthogonal to substrate.
- BlockLace structural-chaining (`Block` with `self_pointer` +
  `hash_pointers`). Deferred per 2026-06-02 scope narrowing.

If a section of this doc starts to drift into one of the above, stop
and put it in the right doc.

## 2. Constraints inherited from v1 (Wake-13..Wake-19)

The bullets below are **already committed in code**. v1.5 must respect
them; revisiting them is a larger break than just "turn on sharding."

- **Two op classes — Ancestry, Head.** Encoded in
  `OpEnvelope.op_class`, defaulted to `Ancestry` so legacy
  pre-Wake-19 envelopes decode the same way. The discriminant for the
  `LocCb` lives in the op-id trailer, not in the envelope payload, because
  K2's `LocCb = fn(&Bytes) -> u32` only sees op-id bytes. Don't try to
  add a third class without changing the op-id derivation.
- **Op-id trailer tags — `ANCESTRY_OP_TAG = [0xdb; 4]`,
  `HEAD_OP_TAG = [0xa1; 4]`.** Ancestry tag matches the legacy v1
  trailer so existing op-ids stay byte-stable across the upgrade. A
  third trailer pattern is the natural way to add a third class later
  without a forced re-hash.
- **Head ops route to loc=0.** Via `OpId::set_loc_callback` installed at
  executor startup. Every peer whose storage arc covers loc=0 replicates
  every Head. v1's FULL arc covers loc=0; v1.5 sharded mode must keep
  this true (§3.5).
- **Dominance walk lives in `KvOpStore`, not in K2.** `register_head`
  consults the `heads_by_ancestry` sled tree and walks parents via
  `get_op_bytes_blocking`. K2 only ferries ops between peers; the
  "current head" semantic is host-side. K2 has no opinion about which
  ops are leaves.
- **`holograph_envelope_decoder` is the canonical op decoder.** It is
  the single source of truth for `bytes → (op_id, timestamp)`. Tests
  that hand-roll an alternative bypass break in subtle ways the moment
  the op-id derivation has a switch (see the Wake-19 wake-19-summary.md
  note on the `space_two_node.rs` test-decoder fix). The same trap
  awaits the v2 trailer extension.
- **Sled is the storage backend.** No in-memory-only production path.
  Restart-survives-state is required (Wake-18 D4). v1.5 and v2
  schema changes must include a migration that preserves the property.
- **`OpEnvelope.head_pointer: Option<Bytes>`** is `Some(ancestry_op_id)`
  on Head envelopes and `None` on Ancestry. Future op classes get their
  own optional pointer field; don't reuse this one.
- **Lock-contention recovery in `KvOpStore::open`** (Wake-18 D1) is
  preserved across both phases. The 5-retry exponential backoff is the
  reason concurrent space construction doesn't deadlock; sharding
  doesn't change that.

## 3. Validation regime — v1.5

When sharding lands, the question "which peers validate which ops"
stops being trivial. v1 every peer holds everything, so every peer
validates everything; v1.5 peers only hold an arc-slice of ancestry
ops, so blanket validation isn't possible. Four candidate regimes:

### 3.1 Options considered

- **Option A — arc-local validation.** Each peer validates the ancestry
  ops whose loc is inside its storage arc. Head ops carry signatures
  peers trust without re-validating the referenced ancestry chain.
- **Option B — snapshot-anchored validation.** Peers validate only at
  snapshot boundaries (every N ops a fresh snapshot is published; each
  snapshot carries an aggregate signature). Between snapshots, trust
  is implicit.
- **Option C — sampled validation.** Each peer validates a random K%
  of incoming ancestry ops regardless of arc. Catches forged ops
  probabilistically.
- **Option D — receipt-required validation.** Every ancestry op carries
  N attestations from validators inside its arc. Peers refuse to
  integrate ops without enough receipts.

### 3.2 Recommendation: Option A for v1.5, B for v2

Rationale:

- **A is the cheapest delta from v1.** v1's per-peer validation is
  already arc-local in the trivial sense (`arc = FULL`). v1.5 keeps the
  same code path; only the arc value changes.
- **A composes with Wake-19's Head FULL-replication.** Heads are
  signed and FULL-replicated; the signature is what peers outside the
  ancestry arc verify. Trust transfers via the Head, not via every
  ancestry op.
- **B layers on top of A cleanly.** Once snapshots become regular,
  validators at the boundary produce one signed aggregate; A's
  arc-local validation still happens, B adds a coarser checkpoint.
  Easier to ship A first and B second than the reverse.
- **C is statistical, not deterministic.** Probabilistic catch is
  fine for fraud detection but doesn't give a deterministic "ok to
  serve" answer; that conflicts with K2's gossip contract where each
  peer must decide yes/no per op.
- **D requires a receipt protocol whose threshold-of-N decision is a
  whole separate design.** Defers to a later phase, post-SF
  integration.

### 3.3 Failure mode of Option A

A malicious peer P announces a Head signed by P for an ancestry op O.
Peer Q outside O's arc:

1. Receives the Head via the loc-0 sector (it's a Head, so it's
   FULL-replicated).
2. Verifies P's signature on the Head envelope. Passes — signature
   validity ≠ Ancestry validity.
3. Records the Head in `current_heads`.
4. `render` against this Head requires fetching the ancestry chain.
   When Q tries to fetch O from peers in O's arc, the chain doesn't
   verify (signatures bad / parents missing / etc.). Q fails to
   render but has *already* accepted the Head as current.

Mitigation paths, in order of ship-likelihood:

- **(M1) Lazy validation on render.** When Q tries to render, it
  fetches the ancestry chain. If a chain op fails to verify, Q
  evicts the Head + the offending op-id from its local view and
  surfaces a warning. Currents heads is recomputed.
- **(M2) Spot-checks at Head ingest.** Q optionally fetches the
  immediate parent ancestry op of every new Head and validates it
  before accepting the Head into `current_heads`. Costs one fetch
  per Head; bounds the "fake current head" window.
- **(M3) Snapshot-anchored validation (Option B).** At snapshot
  boundaries every K ops a fresh aggregate signature is required.
  Detects long-range forgeries that lazy validation might miss if
  the malicious peer keeps the chain short.

v1.5 ships M1. M2 is opt-in via `SpaceConfig` (off by default — adds
a fetch per Head, which on a chatty space could double K2 load).
M3 is the v2 work.

### 3.4 What v1.5 validation looks like in code

`HolographIntegrationQueue::integrate_one` already arc-filters incoming
ops (Wake-13). It also has a `SigVerifier` trait whose v1 impl is
`AlwaysValid`. For v1.5:

- `SigVerifier::verify(envelope) -> bool` becomes the **load-bearing
  validation hook**. Wire a real Ed25519 verifier here; reject ops with
  bad signatures at the queue boundary, no different from how
  `AlwaysValid` accepts them today.
- Arc filter (`!arc.contains(op_id.loc())`) is already in place; with
  v1's FULL arc it lets everything through, with v1.5 a partial arc it
  drops out-of-arc ops on the floor without queue work.
- M1 (lazy validation on render) lives in p-diff-sync's `render`
  rather than in the substrate. The substrate emits ops to the
  language; the language is what discovers the chain is invalid.
  Reporting back is a new method on `HolographSpace` itself —
  `HolographSpace::evict_head(head_op_id, reason)` — called by the
  language when render hits an unverifiable chain. This is the
  inverse direction from `NotifyUp` (substrate → language); v1.5
  ships both.

### 3.5 Arc-coverage commitment (from Wake-19 E7)

Head FULL-replication assumes every peer's `tgt_storage_arc` covers
loc=0. v1's `DhtArc::FULL = Arc(0, u32::MAX)` does trivially. v1.5
sharded peers MUST honour one of these:

1. **Mandatory Head-sector arc.** Every peer's `tgt_storage_arc` is
   `min(declared_arc, head_sector) ∪ declared_arc`, where `head_sector`
   is a small tunable arc around loc=0 (default: `Arc(u32::MAX - K,
   K)` for some K = 2^28 or so). Cost: ~1 LOC in the arc-claim policy.
2. **Per-peer Head loc.** Pick the Head loc as
   `(peer_pubkey_hash mod u32)` so each peer is its own gossip target
   for its own Heads. Different shape; needs more design.

**v1.5 ships option 1.** Simpler to reason about; doesn't require a
schema change on the Head op-id trailer; preserves the load-bearing
"every peer can answer `current_heads()` from local sled" property.
Option 2 is parked as a v2-and-later optimization if loc=0 becomes a
gossip hotspot.

### 3.6 Validation regime — what we don't decide

- The threshold for "enough receipts" if we ever pick D. Out of scope.
- Per-author / per-time-window validation rate limiting. Lives in the
  Language, not the substrate.
- Cross-doc validation (when v2 multi-doc lands, can a peer in doc A's
  arc validate doc B's ops? No, but the routing already excludes
  cross-doc traffic via the trailer). §4 explains.

## 4. Multi-doc routing — v2

Today: one K2 Space holds one perspective-diff document. v2 lifts that.
Forces an op-id derivation change.

### 4.1 Data model

- **K2 Space** stays the unit of K2 gossip / fetch. One Space =
  one Holograph **neighborhood**.
- **`doc_id`** (perspective UUID, hashed to 4 bytes) becomes the
  first-class sharding key alongside `op_class`.
- **`SpaceConfig.doc_ids: Vec<DocId>`** declares which docs the
  neighborhood hosts. v1 = `vec![DocId::default()]`. v2 = N entries.

### 4.2 Op-id derivation change

v1 op-id is 36 bytes:

```
[0..32]  SHA-256(envelope_bytes_or_payload)
[32..36] 4-byte class tag
```

v2 op-id stays 36 bytes but carves the trailer:

```
[0..28]  SHA-256(envelope_bytes_or_payload)[0..28]  — 28 byte digest
[28..32] doc_id_hash[0..4]                         — which doc
[32..36] class tag (ANCESTRY_OP_TAG or HEAD_OP_TAG)
```

Legacy v1 op-ids (where `[28..32]` is the next 4 bytes of SHA-256)
are interpreted as "doc_id = default doc, class X". A v2 peer that
receives a v1 envelope synthesizes `doc_id = DocId::default()` and
re-derives the op-id with the new layout — but the re-derived op-id
**differs** from the v1 op-id because `[0..32]` of SHA-256 is now
`[0..28]`. **This is the forward-incompatibility break.**

### 4.3 Upgrade plan

The op-id change forces a coordinated v2 upgrade boundary:

1. **Snapshot at the boundary.** Before any peer switches to v2 op-id
   derivation, every neighborhood publishes a v1 snapshot covering
   the full DAG. Peers archive the v1 snapshot.
2. **v2 op-ids start fresh.** Post-snapshot, every commit uses v2
   derivation. The v1 history is reachable only via the archived
   snapshot.
3. **No mixed-mode peers.** A v2 peer talking to a v1 peer can't
   reconcile because the op-ids don't line up. The neighborhood
   upgrades atomically (coordinated via the AD4M Language version
   field).
4. **Migration cost.** Linear in op count for the snapshot generation,
   one-shot, idempotent.

### 4.4 loc-callback change

v2's loc-callback reads `(doc_id_hash, op_class_tag)` from
`op_id[28..36]`:

- Head (any doc): loc = `doc_id_hash_as_u32 mod head_sector_size` —
  each doc gets its own Head sector. Peers in `doc_id`'s neighborhood
  declare an arc covering that sector. **Removes** the
  "every-peer-covers-loc=0" requirement (§3.5) since each doc's
  Heads live in its own sector.
- Ancestry: loc = default xor-fold of the full 36-byte op-id (so the
  ancestry loc is still spread across the ring, but skewed by the
  doc_id prefix).

### 4.5 KvOpStore schema change

Every sled tree key extends to `(doc_id_hash, op_id_bytes)`:

- `ops: (doc_id, op_id) → OpRecord`
- `heads_by_ancestry: (doc_id, ancestry_op_id) → head_op_id`
- `current_heads: (doc_id, head_op_id) → ()`
- `slice_hashes: (doc_id, arc_bytes, slice_id) → hash_bytes`

`KvOpStore::current_head_op_ids(doc_id)` becomes per-doc.
`KvOpStore::open` migrates v1 trees by prefixing every key with
`DocId::default()`. The migration runs once on first open after
upgrade; an "upgraded" sentinel key prevents re-migration.

### 4.6 What v2 multi-doc doesn't change

- The two op classes — Heads and Ancestry — stay. Per doc.
- The dominance walk stays inside `KvOpStore`; per-doc dominance
  identical to v1 dominance.
- The `holograph_envelope_decoder` shape stays (in/out: bytes ↔
  (op_id, timestamp)). The op-id derivation inside the decoder
  changes; the surface doesn't.
- The Language API (commit, render, currentRevision, etc.) is unaffected
  — `doc_id` rides as an existing optional `OpEnvelope.doc_id` field
  (already in v1, populated only in v2).

## 5. Receipt model — v1.5

When peer A asks peer B for an Ancestry op B should hold (per arc) but
doesn't, what happens?

### 5.1 v1 behaviour (today)

`HolographIntegrationQueue::tick_fallback` (Wake-13 + Wake-18 D2)
already implements:

- On parent-missing, request from `source` (the gossip-source peer).
- If `source` doesn't deliver within `FetchFallbackPolicy.initial_timeout`,
  round-robin alternative arc-overlap peers via the watcher loop.
- Cap on `FetchFallbackPolicy.max_attempts` + `retry_budget`. When the
  cap is hit, the pending entry is dropped and
  `NotifyUp::notify_parent_fetch_permanent_failure` fires.

That's the v1 "permanent failure" semantic. No receipts, no
reputation, no economic side-effects — just a logged warning + an
upstream signal.

### 5.2 v1.5 — keep silent + retry, with three policy knobs

v1.5 doesn't add receipts. It does surface three knobs already in
Wake-18 D2's `FetchFallbackPolicy`:

- `initial_timeout` — how long to wait on the original source.
- `max_attempts` — peer cap before declaring permanent failure.
- `retry_budget` — wall-clock cap.

Tuning recommendations for sharded mode:

| Scenario | initial_timeout | max_attempts | retry_budget |
|---|---|---|---|
| Small neighborhood (<10 peers) | 5s | 3 | 30s |
| Medium (10-100 peers) | 5s | 5 | 60s |
| Large (>100 peers) | 2s | 10 | 120s |

The default `FetchFallbackPolicy::default()` (5s / 3 / 30s) matches the
"small neighborhood" row; v1.5 surfaces this via `SpaceConfig` so
deployments can pick.

### 5.3 What v1.5 doesn't ship — the SF angle

Receipts that feed into Synergy-Fuel mint/slash decisions are an SF
question, not a Holograph one. Holograph's job is to deliver
substrate-level "we tried, we failed" signals; the policy decision
"missed-arc deserves a slash" is upstream.

When SF integration starts:

1. `NotifyUp::notify_parent_fetch_permanent_failure` gets an extension
   `notify_missed_arc(peer_url, op_id)` for each peer that failed to
   serve an op it should have held.
2. The SF Language module subscribes to those events and mints/slashes
   per its own policy.
3. Holograph stays neutral on the economic semantics.

Until then, v1.5 logs + retries + drops, just like v1.

### 5.4 The receipt-of-answer question

The original wake-20 prompt asks: "Peer A queries `current_heads(doc_id)`.
Peer B answers — receipt-of-answer is implicit (K2 gossip)."

That's correct for v1.5. K2's gossip protocol is the receipt: if B
gossiped the Head, B is on the gossip path; A logs nothing special.
Explicit receipts would require a request/response layer K2 doesn't
have; building one is post-v2 work.

## 6. Open questions surfaced while writing this doc

Each gated on the listed piece of work.

- **Cross-author Head conflict.** Two peers A and B independently
  commit Ancestry ops with the same parent, then each publishes a
  Head. By the Wake-19 dominance walk, neither Head's target is on
  the other's parent walk (siblings), so both Heads survive in
  `current_heads()`. For a multi-author neighborhood that's the
  correct "we forked, please merge" semantic — but `render()`'s
  algorithm needs a deterministic tie-breaker when picking which
  Head to render from. **Gate:** v1.5 multi-author neighborhood
  test. Tentative rule: render from `min(current_heads_by_lex_order)`,
  matching how p-diff-sync's existing `Workspace::build_diffs`
  resolves the "two latest" case.
- **Snapshot cadence.** Wake-16 D snapshots are commit-side
  (`commit.rs::generate_snapshot`). v1.5 wants per-arc snapshot
  cadence (peers in a hot doc snapshot more often). **Gate:**
  disk-budget data from a production-ish run. Defer to the post-v1.5
  ops review.
- **Partial-arc claim shape for Head territory.** §3.5 picks option
  (1) "mandatory Head sector" for v1.5. The 'K' parameter (how big
  the Head sector is) is open. **Gate:** v1.5 first sharded
  end-to-end test. Tentative: K = 2^28 (1/16 of the ring).
- **Per-doc gossip rate.** v2 multi-doc means K2 gossip carries N
  docs' worth of traffic. Does K2 do anything sensible when one
  doc is hot and the others are quiet, or does it gossip uniformly?
  **Gate:** read of `kitsune2_gossip` source + v2 capacity test.
- **Migration tooling for v2 op-id change.** §4.3 sketches the
  snapshot-at-boundary plan. The actual `migrate_v1_to_v2` binary
  is unwritten. **Gate:** v2 spec sign-off.

## 7. Non-goals (recap)

This doc deliberately does NOT cover:

- SF mint/slash policy (lives in SF Language, not Holograph).
- SHACL validation rules (lives in language layer).
- Graph DIDs (separate Living Web spec).
- BlockLace structural chaining (deferred per 2026-06-02 scope
  narrowing).
- AD4M Language wire-protocol changes (Language API stays stable).
- p-diff-sync algorithm crate changes (the algorithm is substrate-
  neutral; sharding is a substrate concern).

If something here turns out to require touching one of the above,
the design needs another round before code lands.

## 8. Implementation phasing

A coarse plan for the work this doc enables:

1. **v1.5-a — partial arc, single doc.** Wire real Ed25519
   verification in `SigVerifier`. Wire arc claims. First sharded
   end-to-end test (3 peers, quarter-ring arcs).
2. **v1.5-b — M1 lazy validation on render.** Add
   `NotifyUp::notify_invalid_ancestry`; p-diff-sync render evicts on
   chain failure.
3. **v1.5-c — opt-in M2 spot-checks at Head ingest.** Add
   `SpaceConfig.validate_head_parent_on_ingest: bool`.
4. **v2-a — multi-doc op-id migration.** Snapshot-at-boundary +
   schema change. **Atomic per neighborhood.**
5. **v2-b — per-doc Head sectors.** Removes the loc=0 hot spot.
6. **v2+ — receipts / SF integration.** Post-SF spec.

Each step is one wake's worth of work modulo testing time. The
phasing is sequential — phase N's code path is incompatible with
phase N-1 above v1.5-a, so phasing matters for deployments.

## 9. Open hook for HOLOGRAPH.md

Per the v1 north-star (`HOLOGRAPH.md`, project design docs root),
Holograph's substrate-neutrality and sharding-readiness commitments
are the load-bearing claims. This doc operationalises both:
"sharding-ready" becomes a concrete v1.5 migration; "substrate-
neutral" stays true because none of the §3-§5 choices require
forking K2.
