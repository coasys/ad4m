# `holograph`

AD4M's Kitsune2-backed substrate for the perspective-diff-sync DAG —
the v1 Holograph spike (see SPIKE.md). Replaces a Holochain conductor
with a sled-backed `KvOpStore` + `HolographIntegrationQueue` +
`HolographSpace`, driving the same `Workspace` / `Snapshot` algorithm
crate that the HDK retriever does.

This crate is part of the four-PR Holograph stack:

- **PR-A** — algorithm crate extraction (substrate-neutral DAG ops)
- **PR-B** — this crate
- **PR-C** — AD4M `holograph-link` Language + JS wires
- **PR-D** — production polish (sled recovery, fetch fallback,
  graceful shutdown, restart-survives, iroh relay env hook)

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
