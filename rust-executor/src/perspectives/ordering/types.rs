//! Types for CRDT collection ordering.

use serde::{Deserialize, Serialize};

/// The item a first entry points back at. Not a real URI — nothing is ever
/// stored under it; it only marks where the chain starts.
pub const LIST_HEAD: &str = "ad4m://list_head";

/// The predicate ordering links are stored under, sourced on the collection's
/// **parent**.
pub const COLLECTION_ORDER_PREDICATE: &str = "ad4m://collection_order";

/// One parsed `ad4m://collection_order` link.
///
/// A **position hint**, not a membership record. The collection's data links are
/// the sole authority on what is *in* it; an entry naming an item that is not
/// there is traversed for chain continuity and left out of the output, and an
/// item with no entry is appended by timestamp. Neither is an error, and neither
/// needs a tombstone to express.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OrderingEntry {
    /// Scopes the entry to one relation — a parent may own several ordered
    /// collections, and they all share this predicate.
    pub predicate: String,
    /// The item this entry positions.
    pub item: String,
    /// `{timestamp}_{agentDid}`, timestamp zero-padded to 16 digits so that
    /// **string comparison equals numeric comparison**. Without the padding,
    /// tiebreaking would order differently on different machines, which is the
    /// one thing a CRDT may not do.
    pub pid: String,
    /// The item this one follows, or [`LIST_HEAD`] for the first.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub after: Option<String>,
    /// Reserved for a fractional-index strategy. Unimplemented: the linked-list
    /// form encodes the causal graph (two entries sharing an `after` are a fork)
    /// and a position string does not, which is the property the partitioned-
    /// conversation view will need.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub position: Option<String>,
}

/// Ordering mutations to apply.
///
/// **Additive only.** Stale entries are never removed — `reconstruct` ignores
/// them by keeping only the highest `pid` per item. That is crash-safe (a
/// partial write leaves a stale entry, never a lost position), halves the sync
/// operations a move costs, and means a reader that has seen only some of the
/// entries still gets a coherent order.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct OrderingDiff {
    pub add: Vec<OrderingEntry>,
}

/// How a relation is ordered, as declared in SHACL.
#[derive(Debug, Clone, PartialEq)]
pub struct OrderingConfig {
    pub strategy: String,
}

/// Build a `pid` for an entry.
///
/// `seq` disambiguates entries minted in the same millisecond by one agent —
/// generating a whole collection's order happens well inside a clock tick.
pub fn make_pid(timestamp_ms: u64, seq: u64, agent_did: &str) -> String {
    format!("{:016}_{}", timestamp_ms.saturating_add(seq), agent_did)
}
