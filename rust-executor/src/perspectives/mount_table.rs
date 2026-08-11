//! Per-perspective mount metadata.
//!
//! An in-memory registry of graphs the node holds, keyed by commit IRI
//! (`graph://<hash(diff, parents)>`). Each entry records provenance (source
//! URI, trust level), the proof bundle, and optional diff-DAG lineage
//! (parent commit IRIs, snapshot hash). Provenance is deliberately NOT
//! written as triples into the graph's canonical set — this table is the
//! sanctioned home for it.
//!
//! State is process-local and does not persist across restarts, matching the
//! named-graph registry model on the base branch.

use crate::types::{MountedGraphEntry, SnapshotProof, TrustLevel};
use std::collections::HashMap;
use std::sync::RwLock;

/// In-memory map from commit IRI → mount metadata.
#[derive(Default)]
pub struct MountTable {
    entries: RwLock<HashMap<String, MountedGraphEntry>>,
}

impl MountTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record (or overwrite) an entry keyed by its commit IRI.
    pub fn insert(&self, entry: MountedGraphEntry) {
        self.entries
            .write()
            .unwrap()
            .insert(entry.graph_iri.clone(), entry);
    }

    /// Fetch a single entry by commit IRI.
    pub fn get(&self, graph_iri: &str) -> Option<MountedGraphEntry> {
        self.entries.read().unwrap().get(graph_iri).cloned()
    }

    /// True if the given commit IRI is currently held.
    pub fn contains(&self, graph_iri: &str) -> bool {
        self.entries.read().unwrap().contains_key(graph_iri)
    }

    /// Remove an entry (idempotent — returns the removed entry if it was present).
    pub fn remove(&self, graph_iri: &str) -> Option<MountedGraphEntry> {
        self.entries.write().unwrap().remove(graph_iri)
    }

    /// Snapshot of all current entries, sorted by commit IRI for a
    /// deterministic listing.
    pub fn list(&self) -> Vec<MountedGraphEntry> {
        let mut all: Vec<MountedGraphEntry> =
            self.entries.read().unwrap().values().cloned().collect();
        all.sort_by(|a, b| a.graph_iri.cmp(&b.graph_iri));
        all
    }
}

/// Build a `local` mount entry for a locally-produced commit.
pub fn local_entry(
    graph_iri: String,
    source: String,
    snapshot_proofs: Vec<SnapshotProof>,
    mounted_at: String,
    parents: Option<Vec<String>>,
    snapshot_hash: Option<String>,
) -> MountedGraphEntry {
    MountedGraphEntry {
        graph_iri,
        graph_did: None,
        source,
        trust_level: TrustLevel::Local,
        snapshot_proofs,
        mounted_at,
        parents,
        snapshot_hash,
    }
}

/// Build an `external` mount entry for a remote commit whose proof bundle has
/// been verified at materialisation time.
pub fn external_entry(
    graph_iri: String,
    source: String,
    snapshot_proofs: Vec<SnapshotProof>,
    mounted_at: String,
    parents: Option<Vec<String>>,
    snapshot_hash: Option<String>,
) -> MountedGraphEntry {
    MountedGraphEntry {
        graph_iri,
        graph_did: None,
        source,
        trust_level: TrustLevel::External,
        snapshot_proofs,
        mounted_at,
        parents,
        snapshot_hash,
    }
}
