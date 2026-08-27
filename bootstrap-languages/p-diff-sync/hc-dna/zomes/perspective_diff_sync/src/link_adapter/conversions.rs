//! Integrity-zome ↔ algorithm-crate mirror-type conversions.
//!
//! Step 13b-C phase 2 (the wake-15 consolidation): now that
//! `perspective_diff_algorithm::Workspace` is the canonical Workspace,
//! every interaction with the algorithm crate from p-diff-sync passes
//! through these helpers.
//!
//! The integrity-zome types decorate `Serialize` / `Deserialize` with
//! `SerializedBytes` / `app_entry!` (HDI). The algorithm-crate mirrors
//! have identical field shapes but no HDI decoration. Conversions are
//! field-by-field copies (cheap; no serde round-trip).
//!
//! `Hash ↔ HoloHash<Action>` uses the HoloHash raw-39-byte form so
//! round-trips are byte-exact.

use hdk::prelude::*;
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::{
    ExpressionProof, HashReference, LinkExpression, LocalHashReference, PerspectiveDiff,
    PerspectiveDiffEntryReference, Snapshot, Triple,
};

use crate::Hash;

// ---- Hash ------------------------------------------------------------

pub fn hash_to_algo(h: &Hash) -> algo::Hash {
    let raw = h.get_raw_39();
    let mut buf = [0u8; 39];
    buf.copy_from_slice(&raw);
    algo::Hash::from_raw_39(buf)
}

pub fn hash_from_algo(h: &algo::Hash) -> Hash {
    HoloHash::from_raw_39(h.as_bytes().to_vec())
}

// ---- LinkExpression --------------------------------------------------

pub fn link_to_algo(l: LinkExpression) -> algo::LinkExpression {
    algo::LinkExpression {
        author: l.author,
        data: algo::Triple {
            source: l.data.source,
            target: l.data.target,
            predicate: l.data.predicate,
        },
        timestamp: l.timestamp,
        proof: algo::ExpressionProof {
            signature: l.proof.signature,
            key: l.proof.key,
        },
    }
}

pub fn link_from_algo(l: algo::LinkExpression) -> LinkExpression {
    LinkExpression {
        author: l.author,
        data: Triple {
            source: l.data.source,
            target: l.data.target,
            predicate: l.data.predicate,
        },
        timestamp: l.timestamp,
        proof: ExpressionProof {
            signature: l.proof.signature,
            key: l.proof.key,
        },
    }
}

// ---- PerspectiveDiff --------------------------------------------------

pub fn diff_to_algo(d: PerspectiveDiff) -> algo::PerspectiveDiff {
    algo::PerspectiveDiff {
        additions: d.additions.into_iter().map(link_to_algo).collect(),
        removals: d.removals.into_iter().map(link_to_algo).collect(),
    }
}

pub fn diff_from_algo(d: algo::PerspectiveDiff) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: d.additions.into_iter().map(link_from_algo).collect(),
        removals: d.removals.into_iter().map(link_from_algo).collect(),
    }
}

// ---- PerspectiveDiffEntryReference -----------------------------------

pub fn entry_ref_to_algo(e: PerspectiveDiffEntryReference) -> algo::PerspectiveDiffEntryReference {
    algo::PerspectiveDiffEntryReference {
        diff: diff_to_algo(e.diff),
        parents: e
            .parents
            .map(|ps| ps.iter().map(hash_to_algo).collect::<Vec<_>>()),
        diffs_since_snapshot: e.diffs_since_snapshot,
        diff_chunks: e
            .diff_chunks
            .map(|cs| cs.iter().map(hash_to_algo).collect::<Vec<_>>()),
    }
}

pub fn entry_ref_from_algo(
    e: algo::PerspectiveDiffEntryReference,
) -> PerspectiveDiffEntryReference {
    PerspectiveDiffEntryReference {
        diff: diff_from_algo(e.diff),
        parents: e
            .parents
            .map(|ps| ps.iter().map(hash_from_algo).collect::<Vec<_>>()),
        diffs_since_snapshot: e.diffs_since_snapshot,
        diff_chunks: e
            .diff_chunks
            .map(|cs| cs.iter().map(hash_from_algo).collect::<Vec<_>>()),
    }
}

// ---- Snapshot --------------------------------------------------------

pub fn snapshot_to_algo(s: Snapshot) -> algo::Snapshot {
    algo::Snapshot {
        diff_chunks: s.diff_chunks.iter().map(hash_to_algo).collect(),
        included_diffs: s.included_diffs.iter().map(hash_to_algo).collect(),
    }
}

pub fn snapshot_from_algo(s: algo::Snapshot) -> Snapshot {
    Snapshot {
        diff_chunks: s.diff_chunks.iter().map(hash_from_algo).collect(),
        included_diffs: s.included_diffs.iter().map(hash_from_algo).collect(),
    }
}

// ---- HashReference / LocalHashReference ------------------------------

pub fn hash_ref_to_algo(r: HashReference) -> algo::HashReference {
    algo::HashReference {
        hash: hash_to_algo(&r.hash),
        timestamp: r.timestamp,
    }
}

#[allow(dead_code)]
pub fn hash_ref_from_algo(r: algo::HashReference) -> HashReference {
    HashReference {
        hash: hash_from_algo(&r.hash),
        timestamp: r.timestamp,
    }
}

pub fn local_hash_ref_to_algo(r: LocalHashReference) -> algo::LocalHashReference {
    algo::LocalHashReference {
        hash: hash_to_algo(&r.hash),
        timestamp: r.timestamp,
    }
}

pub fn local_hash_ref_from_algo(r: algo::LocalHashReference) -> LocalHashReference {
    LocalHashReference {
        hash: hash_from_algo(&r.hash),
        timestamp: r.timestamp,
    }
}
