//! Per-perspective mount metadata.
//!
//! An in-memory registry of graphs the node holds, keyed by content-hash IRI
//! (`graph://<hash>`). Each entry records provenance (source URI, trust level)
//! and the proof bundle that justified the mount. Provenance is deliberately
//! NOT written as triples into the graph's canonical set — that would mutate the
//! content hash and pollute the snapshot with agent-local metadata. This table
//! is the sanctioned home for it.
//!
//! State is process-local and does not persist across restarts, matching the
//! named-graph registry model on the base branch.

use crate::types::{MountedGraphEntry, SnapshotProof, TrustLevel};
use std::collections::HashMap;
use std::sync::RwLock;

/// In-memory map from content-hash IRI → mount metadata.
#[derive(Default)]
pub struct MountTable {
    entries: RwLock<HashMap<String, MountedGraphEntry>>,
}

impl MountTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record (or overwrite) an entry keyed by its content-hash IRI.
    pub fn insert(&self, entry: MountedGraphEntry) {
        self.entries
            .write()
            .unwrap()
            .insert(entry.graph_iri.clone(), entry);
    }

    /// Fetch a single entry by content-hash IRI.
    pub fn get(&self, graph_iri: &str) -> Option<MountedGraphEntry> {
        self.entries.read().unwrap().get(graph_iri).cloned()
    }

    /// True if the given content-hash IRI is currently held.
    pub fn contains(&self, graph_iri: &str) -> bool {
        self.entries.read().unwrap().contains_key(graph_iri)
    }

    /// Remove an entry (idempotent — returns the removed entry if it was present).
    pub fn remove(&self, graph_iri: &str) -> Option<MountedGraphEntry> {
        self.entries.write().unwrap().remove(graph_iri)
    }

    /// Snapshot of all current entries, sorted by content-hash IRI for a
    /// deterministic listing.
    pub fn list(&self) -> Vec<MountedGraphEntry> {
        let mut all: Vec<MountedGraphEntry> =
            self.entries.read().unwrap().values().cloned().collect();
        all.sort_by(|a, b| a.graph_iri.cmp(&b.graph_iri));
        all
    }
}

/// Build a `local` mount entry for a locally-produced snapshot
/// (locally-produced exports resolve to `local` trust).
pub fn local_entry(
    graph_iri: String,
    source: String,
    snapshot_proofs: Vec<SnapshotProof>,
    mounted_at: String,
) -> MountedGraphEntry {
    MountedGraphEntry {
        graph_iri,
        graph_did: None,
        source,
        trust_level: TrustLevel::Local,
        snapshot_proofs,
        mounted_at,
    }
}

/// Build an `external` mount entry for a remote snapshot whose proof bundle has
/// been verified at materialisation time (remote fetches with verified proof
/// resolve to `external` trust).
pub fn external_entry(
    graph_iri: String,
    source: String,
    snapshot_proofs: Vec<SnapshotProof>,
    mounted_at: String,
) -> MountedGraphEntry {
    MountedGraphEntry {
        graph_iri,
        graph_did: None,
        source,
        trust_level: TrustLevel::External,
        snapshot_proofs,
        mounted_at,
    }
}
