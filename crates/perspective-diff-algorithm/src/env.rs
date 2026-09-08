//! Substrate-agnostic side-effecting environment for `pull`, `commit`,
//! and `render`.
//!
//! `WorkspaceRetriever` / `RevisionsRetriever` / `SnapshotRetriever`
//! cover the pure read/write needed to walk the DAG and persist
//! diff/snapshot entries. The wake-23 Step-2 move pulls the rest of
//! pull/commit/render into the algorithm crate, which means the
//! substrate-specific calls those modules made — `emit_signal`,
//! `hash_entry` + `create_link` for snapshot links,
//! `send_remote_signal`, `sys_time()` — also need to live behind a
//! trait. `PullCommitEnv` is that trait.
//!
//! On the Holochain path, the impl forwards to `hdk::prelude::*`. On
//! the upcoming Kitsune2 path the impl forwards to the holograph
//! transport. For tests the mock retriever's impl is a thin in-memory
//! shim.

use crate::errors::AlgoResult;
use perspective_diff_types::{Hash, HashBroadcast, PerspectiveDiff, Snapshot};

pub trait PullCommitEnv {
    /// Current host time. On Holochain this calls `sys_time()` and
    /// re-shapes the result into a `chrono::DateTime<Utc>`. Used by
    /// `update_current_revision` (which stamps the new revision)
    /// and by the chunk-availability retry loop.
    fn now() -> AlgoResult<chrono::DateTime<chrono::Utc>>;

    /// Monotonic milliseconds since some host-defined epoch. Used only
    /// by the commit-time chunk-availability retry loop, which polls
    /// every `RETRY_DELAY_MS` until each chunk shows up locally.
    fn sys_time_ms() -> AlgoResult<i64>;

    /// Fire a `PerspectiveDiff` signal at the local subscriber (UI).
    /// Equivalent of HDK's `emit_signal(diff)`.
    fn emit_diff_signal(diff: PerspectiveDiff) -> AlgoResult<()>;

    /// Fire a `HashBroadcast` signal at the local subscriber. The
    /// broadcast is also relayed onward via
    /// `send_hash_broadcast_to_active_agents`.
    fn emit_broadcast_signal(broadcast: HashBroadcast) -> AlgoResult<()>;

    /// Send the given `HashBroadcast` to every currently-active agent
    /// known to this node. Substrate-specific delivery (HDK
    /// `send_remote_signal` on the Holochain path).
    fn send_hash_broadcast_to_active_agents(broadcast: HashBroadcast) -> AlgoResult<()>;

    /// Persist a `Snapshot` to the substrate and link it to the
    /// `diff_action_hash` that "owns" it. On Holochain this writes the
    /// snapshot entry via `create_entry` then issues a
    /// `create_link(hash_entry(diff), hash_entry(snapshot),
    /// LinkTypes::Snapshot, "snapshot")`. The algorithm crate doesn't
    /// distinguish entry-hash vs action-hash — the substrate handles
    /// the conversion.
    fn create_snapshot_and_link(diff_action_hash: Hash, snapshot: Snapshot) -> AlgoResult<()>;
}
