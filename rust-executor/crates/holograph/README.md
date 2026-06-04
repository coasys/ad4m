# `holograph`

AD4M's Kitsune2-backed substrate for the perspective-diff-sync DAG —
the v1 Holograph spike (see SPIKE.md). Replaces a Holochain conductor
with a sled-backed `KvOpStore` + `HolographIntegrationQueue` +
`HolographSpace`, driving the same `Workspace` / `Snapshot` algorithm
crate that the HDK retriever does.

This crate is part of the six-PR Holograph stack:

- **PR-A** — algorithm crate extraction (substrate-neutral DAG ops)
- **PR-B** — this crate
- **PR-C** — AD4M `holograph-link` Language + JS wires
- **PR-D** — production polish (sled recovery, fetch fallback,
  graceful shutdown, restart-survives, iroh relay env hook)
- **PR-E** — two op classes (Ancestry vs Head, sharding-ready)
- **PR-F** — `SHARDED_MODE.md` design doc (v1.5 / v2 plan)

## Op classes

Wake-19 splits ops into two classes:

- **Ancestry** — the diff payload op, identified by content hash of
  the payload. Routed by the default xor-fold location, so the
  op-ids spread evenly across the DHT ring. In v1.5 sharded mode,
  each peer holds an arc-slice of Ancestry ops proportional to its
  declared storage arc.
- **Head** — a tiny pointer announcing "this Ancestry op-id is a
  current leaf of the DAG." Routed by the
  installed `OpId::set_loc_callback` to a **fixed loc-0 sector** so
  every peer whose arc covers location 0 replicates every Head.
  This keeps `current_heads()` a constant-time read for late joiners
  without forcing every peer to hold the entire Ancestry DAG.

The class is encoded in two places:

1. The CBOR `OpEnvelope.op_class` field (`Ancestry` is the default
   and is `skip_serializing_if`-elided so the wire shape stays
   byte-stable with pre-Wake-19 envelopes).
2. The trailing 4 bytes of the 36-byte op-id —
   `ANCESTRY_OP_TAG = [0xdb; 4]` (matching the legacy v1 trailer) or
   `HEAD_OP_TAG = [0xa1; 4]`. K2's `LocCb` only sees the raw op-id
   bytes, never the envelope payload, so the trailer is what the
   loc-callback inspects.

### K2 arc-coverage caveat

The Head routing strategy assumes **every peer's declared storage arc
covers loc=0**. K2's `DhtArc::FULL = Arc(0, u32::MAX)` does, so v1's
FULL-arc deployments are fine.

A v1.5 peer declaring something like `Arc(u32::MAX / 2, u32::MAX)`
(an upper-half-only arc that excludes loc=0) **would not replicate
Heads** and would fail to learn the current leaves from gossip. v1.5
sharded mode must therefore guarantee every peer's storage-arc
claim includes loc=0 — either via an explicit "Head sector" minimum
arc, or by computing per-peer `tgt_storage_arc` so loc=0 is always
covered.

Two viable approaches when we get there:

1. **Mandatory Head sector** — every peer's `tgt_storage_arc` is
   either FULL or `Arc::full_with_head_sector()` (a tiny guaranteed
   sub-arc around 0). Cost: ~1 LOC change in the arc-claim policy.
2. **Per-peer Head loc** — pick the Head loc as
   `(peer_pubkey_hash mod u32)` so each peer is its own gossip
   target for its own Heads. Different shape; needs more design.

v1.5 will pick option 1. Documented in `SPIKE.md` parking lot.

## Configuration

Per-space behaviour is set via `SpaceConfig`
(see `src/config.rs`). The v1 default is
`SpaceConfig::full_replication_single_doc()` —
every node holds every op, single shared document, 5s gossip cadence.
Tests and v1.5 sharded deployments build their own configs.

### Environment variables

Wake-18 D6 surfaces a small set of env-driven overrides for
deployment-time tuning. All are optional; unset means "use the
hard-coded default."

| Variable | Default | Purpose |
|---|---|---|
| `HOLOGRAPH_IROH_RELAY` | none | Iroh relay URL for cross-process transport. Resolved into `SpaceConfig.iroh_relay_url` by `HolographSpace::new` when the config field is `None`. Empty / whitespace-only is treated as unset. See `resolve_iroh_relay()`. |
| `HOLOGRAPH_IROH_RELAY_URL` | none | Older alias for `HOLOGRAPH_IROH_RELAY`. Used as a back-compat fallback (existing wiring in `holograph_wires.rs` reads this). New deployments should prefer the shorter name. |
| `HOLOGRAPH_IROH_PLAINTEXT` | `0` | Permit plain-text (`ws://`) relay connections. Spike-only; production should use TLS (`wss://`). |
| `HOLOGRAPH_BOOTSTRAP_URL` | derived from relay | Bootstrap server URL for `CoreBootstrap`. Defaults to the relay URL with any `/relay` suffix stripped (matches `kitsune2-bootstrap-srv`'s pattern). |
| `HOLOGRAPH_BOOTSTRAP_BACKOFF_MIN_MS` | `500` | Minimum re-bootstrap interval. Spike tightens K2's default (5000ms) so two-conductor convergence fits within the 15s test deadline. |

### Programmatic overrides

`SpaceConfig.fetch_fallback_policy: FetchFallbackPolicy` lifts the
multi-peer fetch-fallback knobs into one structured policy:

```rust
FetchFallbackPolicy {
    initial_timeout: Duration::from_secs(5),  // grace before fallback
    max_attempts:    3,                       // peer cap (lifetime)
    retry_budget:    Duration::from_secs(30), // wall-clock cap
}
```

When either cap is hit, the pending entry is dropped and
`NotifyUp::notify_parent_fetch_permanent_failure` fires so upstream
layers can surface a "given up" signal.

## Lifecycle

`HolographSpace` is the top-level handle. It's `Arc`-wrapped and shared
across the K2 stack + AD4M language wires:

```rust
let space: Arc<HolographSpace> = HolographSpace::new(cfg);
// ... use it ...
let remaining = space.shutdown().await?;  // graceful drain + flush
```

`shutdown()`:

1. Sets a flag that makes `on_local_commit` reject new commits.
2. Stops the integration queue's fallback watcher.
3. Drains the queue (10s timeout).
4. Flushes the sled DB so the snapshot is durable.
5. Closes the `LocalCommitTarget` (transport teardown).

`Drop for HolographSpace` is the safety net for "process exit before
shutdown was called" — best-effort synchronous flush, logged on error,
never panics.

## Further reading

- **`SHARDED_MODE.md`** — design doc for the v1.5 / v2 sharded-mode work
  that picks up where this crate's v1 leaves off. Covers the validation
  regime, multi-document routing, receipt model, and the constraints
  v1's code already locks in.
