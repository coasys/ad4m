//! CRDT ordering for `@HasMany` collections.
//!
//! A `@HasMany` is a set of links, and hydration sorts them by link timestamp.
//! That is fine for an append-only collection — a transcript, a message thread —
//! where timestamp order *is* the order. It cannot express a sequence somebody
//! chose: a kanban column, a playlist, the blocks of a post.
//!
//! Worth being precise about what exists today, because it looks like ordering
//! and is not: `collectionSetter` deletes every link for the relation and re-adds
//! them in array order, restamping every timestamp. Single-agent, that produces
//! the right sequence by accident. Concurrently, the two agents' writes
//! interleave by wall clock and neither array survives.
//!
//! # Shape of the solution
//!
//! Ordering is declared in the type system (SHACL) and lives entirely in the
//! executor, so every writer gets it — the ORM, MCP agents, raw GraphQL callers,
//! another app sharing the neighbourhood. A client-side implementation would
//! order only what that client wrote.
//!
//! Entries are stored on the **parent**, under
//! [`COLLECTION_ORDER_PREDICATE`](types::COLLECTION_ORDER_PREDICATE):
//!
//! - they arrive with the parent's own links, so reconstruction needs no extra
//!   query;
//! - an item may sit in several ordered collections, and `(parent, predicate)`
//!   scopes its position to one of them;
//! - ordering is a property of the relationship, not of the child.
//!
//! # The invariant everything else follows from
//!
//! **Ordering entries are position hints over a membership set the data links
//! already define.** The data links say what is *in* the collection; the entries
//! say only where things go. An entry for an absent item is inert, an item with
//! no entry is appended by timestamp, and neither is an error.
//!
//! That is why there are no tombstones. Deletion is `item ∉ data_links`, which
//! `reconstruct` can see for itself — no `deleted` flag, no copied `after`, no
//! extra link per removal, and no accumulating class of entries needing
//! collection. It is also strictly more correct on resurrect: a tombstone's
//! higher `pid` would keep an item hidden after its data link came back.

pub mod linked_list;
pub mod types;

use deno_core::anyhow::{anyhow, Error};

pub use types::{
    make_pid, OrderingConfig, OrderingDiff, OrderingEntry, COLLECTION_ORDER_PREDICATE, LIST_HEAD,
};

/// How a collection's order is represented and reconstructed.
///
/// `data_links` is `(target, timestamp)` per link rather than the full link
/// expression: it is what both call sites already hold — `hydrate_one` groups
/// exactly this, and `get_links` can project it — and it keeps the whole module
/// testable without a store.
pub trait OrderingStrategy: Send + Sync {
    /// The collection's items, in order.
    ///
    /// `data_links` is the membership set and the sole authority on what belongs;
    /// `ordering` supplies positions. Members with no entry are appended by
    /// timestamp.
    fn reconstruct(
        &self,
        data_links: &[(String, String)],
        ordering: &[OrderingEntry],
    ) -> Vec<String>;

    /// Entries for a whole array — first save, or a collection that was
    /// unordered until its SHACL declared otherwise.
    fn generate_full(
        &self,
        items: &[String],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> Vec<OrderingEntry>;

    /// The minimal entries that turn the current order into the desired one.
    fn diff(
        &self,
        current_ordering: &[OrderingEntry],
        current_members: &[(String, String)],
        desired_items: &[String],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> OrderingDiff;

    /// An entry placing one new item at the end — for `addLink`, which has no
    /// array to diff against.
    fn generate_append(
        &self,
        item_uri: &str,
        current_ordering: &[OrderingEntry],
        current_members: &[(String, String)],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> OrderingEntry;
}

/// Build the strategy a config names.
pub fn create_strategy(strategy: &str) -> Result<Box<dyn OrderingStrategy>, Error> {
    match strategy {
        "linkedList" => Ok(Box::new(linked_list::LinkedListStrategy::new())),
        "fractionalIndex" => Err(anyhow!(
            "ordering strategy 'fractionalIndex' is reserved but not implemented: it cannot \
             represent the causal graph that 'linkedList' encodes"
        )),
        other => Err(anyhow!("unknown ordering strategy '{other}'")),
    }
}

/// Parse the ordering entries a parent's `ad4m://collection_order` links carry,
/// keeping only those scoped to `predicate`.
///
/// A malformed entry is skipped rather than failing the read: it is one item's
/// position, and losing it demotes that item to the by-timestamp tail rather
/// than making the whole collection unreadable.
pub fn parse_ordering_entries(targets: &[String], predicate: &str) -> Vec<OrderingEntry> {
    targets
        .iter()
        .filter_map(|t| {
            let json = t
                .strip_prefix("literal:json:")
                .or_else(|| t.strip_prefix("literal://json:"))?;
            let decoded = urlencoding::decode(json)
                .map(|s| s.to_string())
                .unwrap_or_else(|_| json.to_string());
            match serde_json::from_str::<OrderingEntry>(&decoded) {
                Ok(entry) => Some(entry),
                Err(e) => {
                    log::warn!("ordering: skipping unparseable entry: {e}");
                    None
                }
            }
        })
        .filter(|e| e.predicate == predicate)
        .collect()
}

/// Render an entry as the link target it is stored under.
///
/// The JSON is percent-encoded, because that is what every other reader of a
/// `literal:json:` target assumes — [`parse_ordering_entries`] decodes, and so
/// does the store on the way in. Written raw, an item URI containing `%20`
/// would come back holding a space and stop matching the membership link it
/// positions.
pub fn encode_ordering_entry(entry: &OrderingEntry) -> Result<String, Error> {
    Ok(format!(
        "literal:json:{}",
        urlencoding::encode(&serde_json::to_string(entry)?)
    ))
}

#[cfg(test)]
mod tests;
