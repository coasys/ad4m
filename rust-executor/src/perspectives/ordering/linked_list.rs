//! RGA (Replicated Growable Array) ordering over parent-sourced entries.
//!
//! Each item records the item it follows. Concurrent inserts at the same
//! predecessor fork, and reconstruction linearises the fork deterministically by
//! `pid`.
//!
//! # Why a linked list rather than fractional indices
//!
//! On the criteria usually cited — no chain traversal, O(1) moves, no cleanup —
//! fractional indexing wins. It is not chosen because `{item, after, pid}`
//! **encodes the causal graph** and a position string destroys it: two entries
//! sharing an `after` are a fork, and an entry whose `after` names an item from
//! another fork is a merge. That is what a future partitioned-conversation view
//! needs, where linearising two independently-coherent threads renders both
//! unreadable. It is the property not to trade away.

use std::collections::{HashMap, HashSet};

use super::types::{make_pid, OrderingDiff, OrderingEntry, LIST_HEAD};
use super::OrderingStrategy;

pub struct LinkedListStrategy;

impl LinkedListStrategy {
    pub fn new() -> Self {
        LinkedListStrategy
    }
}

impl Default for LinkedListStrategy {
    fn default() -> Self {
        Self::new()
    }
}

/// Keep one entry per item — the highest `pid` wins.
///
/// This is what resolves a concurrent move: two agents each add an entry for the
/// same item, and every peer picks the same one. It is also why nothing is ever
/// deleted; superseded entries are simply never selected.
fn latest_per_item(ordering: &[OrderingEntry]) -> Vec<&OrderingEntry> {
    let mut winners: HashMap<&str, &OrderingEntry> = HashMap::new();
    for entry in ordering {
        winners
            .entry(entry.item.as_str())
            .and_modify(|best| {
                if entry.pid > best.pid {
                    *best = entry;
                }
            })
            .or_insert(entry);
    }
    let mut out: Vec<&OrderingEntry> = winners.into_values().collect();
    // Deterministic input order for everything downstream.
    out.sort_by(|a, b| a.item.cmp(&b.item));
    out
}

impl OrderingStrategy for LinkedListStrategy {
    fn reconstruct(
        &self,
        data_links: &[(String, String)],
        ordering: &[OrderingEntry],
    ) -> Vec<String> {
        let members: HashSet<&str> = data_links.iter().map(|(t, _)| t.as_str()).collect();
        let winners = latest_per_item(ordering);

        // predecessor → the items claiming to follow it.
        let mut after_map: HashMap<&str, Vec<&OrderingEntry>> = HashMap::new();
        for entry in &winners {
            let key = entry.after.as_deref().unwrap_or(LIST_HEAD);
            after_map.entry(key).or_default().push(entry);
        }
        // Concurrent inserts at one predecessor are a fork; higher pid first, so
        // every peer linearises it the same way.
        for followers in after_map.values_mut() {
            followers.sort_by(|a, b| b.pid.cmp(&a.pid));
        }

        let mut out: Vec<String> = Vec::new();
        let mut visited: HashSet<&str> = HashSet::new();
        // Depth-first from the head, emitting on visit. The explicit stack keeps
        // a long collection off the call stack, and `visited` makes corrupted
        // data (a cycle) a truncated read rather than a hang.
        //
        // Emitting when an item is *popped* rather than when it is pushed is
        // what makes a fork come out in pid order: the whole of the higher-pid
        // branch has to be walked before the lower-pid sibling is reached.
        let mut stack: Vec<&str> = vec![LIST_HEAD];
        while let Some(current) = stack.pop() {
            // An entry whose item is not in the collection is traversed — later
            // items may hang off it — but not emitted. This is what makes
            // tombstones unnecessary: the data links already say who is a
            // member.
            if current != LIST_HEAD && members.contains(current) {
                out.push(current.to_string());
            }
            if let Some(followers) = after_map.get(current) {
                // Pushed in reverse so the highest pid is popped first.
                for entry in followers.iter().rev() {
                    let item = entry.item.as_str();
                    if visited.insert(item) {
                        stack.push(item);
                    }
                }
            }
        }

        // Members with no surviving entry go last, oldest first. Covers the
        // unordered→ordered migration, a peer that wrote data links without
        // ordering ones, and a crash between the two writes.
        let mut unpositioned: Vec<&(String, String)> = data_links
            .iter()
            .filter(|(t, _)| !visited.contains(t.as_str()))
            .collect();
        unpositioned.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
        let mut seen_tail: HashSet<&str> = HashSet::new();
        for (target, _) in unpositioned {
            if seen_tail.insert(target.as_str()) {
                out.push(target.clone());
            }
        }

        out
    }

    fn generate_full(
        &self,
        items: &[String],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> Vec<OrderingEntry> {
        let mut entries = Vec::with_capacity(items.len());
        let mut previous = LIST_HEAD.to_string();
        for (i, item) in items.iter().enumerate() {
            entries.push(OrderingEntry {
                predicate: predicate.to_string(),
                item: item.clone(),
                pid: make_pid(now_ms, i as u64, agent_did),
                after: Some(previous.clone()),
                position: None,
            });
            previous = item.clone();
        }
        entries
    }

    fn diff(
        &self,
        current_ordering: &[OrderingEntry],
        current_members: &[(String, String)],
        desired_items: &[String],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> OrderingDiff {
        // What the order *would* be after this save if no new entry were
        // written: the existing entries, read against the membership the save
        // is establishing. Items being added have no link timestamp yet, so
        // they sort to the tail — which is where an unpositioned item goes.
        let known_ts: HashMap<&str, &str> = current_members
            .iter()
            .map(|(t, ts)| (t.as_str(), ts.as_str()))
            .collect();
        let projected_members: Vec<(String, String)> = desired_items
            .iter()
            .enumerate()
            .map(|(i, item)| {
                let ts = known_ts
                    .get(item.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("{:016}", now_ms.saturating_add(i as u64)));
                (item.clone(), ts)
            })
            .collect();
        let projected = self.reconstruct(&projected_members, current_ordering);

        // Nothing to say. Covers a save that changed nothing, an append that the
        // by-timestamp tail already places correctly, and — the one worth
        // naming — a *removal*, where dropping the data link is the entire
        // operation and the stale entry is inert.
        if projected == desired_items {
            return OrderingDiff::default();
        }

        // Write an entry for every item whose predecessor is not already what
        // the desired order asks for.
        //
        // Not a longest-increasing-subsequence diff, which is what an earlier
        // draft of this called for. LIS answers "which items are already in the
        // right relative order", and for a linked list that is the wrong
        // question: `after` is a pointer, so moving one item silently changes
        // the predecessor of the item that followed it. Leaving that neighbour
        // alone because it was in the LIS leaves it pointing at the moved item —
        // producing, in the simplest reordering there is (`[A,B,C,D]` →
        // `[A,C,B,D]`), a chain where B follows C and C follows B.
        //
        // Comparing predecessors gets the same economy honestly: an untouched
        // run costs nothing because every predecessor in it still matches, one
        // append costs one entry, and one move costs about three — the item, the
        // one it left, and the one it landed behind.
        let projected_predecessors = predecessors(&projected);
        let mut add = Vec::new();
        let mut previous = LIST_HEAD.to_string();
        let mut seq = 0u64;
        for item in desired_items {
            let already_there = projected_predecessors
                .get(item.as_str())
                .map(|p| p == &previous)
                .unwrap_or(false);
            if !already_there {
                add.push(OrderingEntry {
                    predicate: predicate.to_string(),
                    item: item.clone(),
                    pid: make_pid(now_ms, seq, agent_did),
                    after: Some(previous.clone()),
                    position: None,
                });
                seq += 1;
            }
            previous = item.clone();
        }

        OrderingDiff { add }
    }

    fn generate_append(
        &self,
        item_uri: &str,
        current_ordering: &[OrderingEntry],
        current_members: &[(String, String)],
        predicate: &str,
        agent_did: &str,
        now_ms: u64,
    ) -> OrderingEntry {
        let order = self.reconstruct(current_members, current_ordering);
        let after = order
            .last()
            .cloned()
            .unwrap_or_else(|| LIST_HEAD.to_string());
        OrderingEntry {
            predicate: predicate.to_string(),
            item: item_uri.to_string(),
            pid: make_pid(now_ms, 0, agent_did),
            after: Some(after),
            position: None,
        }
    }
}

/// Each item mapped to the item before it in `order` (or the head).
fn predecessors(order: &[String]) -> HashMap<&str, String> {
    let mut out = HashMap::new();
    let mut previous = LIST_HEAD.to_string();
    for item in order {
        out.insert(item.as_str(), previous.clone());
        previous = item.clone();
    }
    out
}
