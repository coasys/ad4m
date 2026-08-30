//! Ordering tests, with the concurrent cases stated explicitly.
//!
//! The tombstone-free design rests on one claim — that membership is already
//! derivable from the data links, so a `deleted` flag adds nothing — and CRDT
//! reasoning is exactly where a confident-sounding wrong answer survives review.
//! The `deletion` module below is that claim's evidence.

use super::linked_list::LinkedListStrategy;
use super::types::{OrderingEntry, LIST_HEAD};
use super::*;

const PRED: &str = "we://children";

fn entry(item: &str, after: &str, pid: &str) -> OrderingEntry {
    OrderingEntry {
        predicate: PRED.to_string(),
        item: item.to_string(),
        pid: pid.to_string(),
        after: Some(after.to_string()),
        position: None,
    }
}

/// Data links as `(target, timestamp)`, timestamps ascending by position.
fn members(items: &[&str]) -> Vec<(String, String)> {
    items
        .iter()
        .enumerate()
        .map(|(i, t)| (t.to_string(), format!("{:016}", 1000 + i)))
        .collect()
}

fn s() -> LinkedListStrategy {
    LinkedListStrategy::new()
}

// ---- reconstruction ------------------------------------------------------

#[test]
fn reconstructs_a_simple_chain() {
    let ordering = vec![
        entry("a", LIST_HEAD, "0000000000000001_did:x"),
        entry("b", "a", "0000000000000002_did:x"),
        entry("c", "b", "0000000000000003_did:x"),
    ];
    assert_eq!(
        s().reconstruct(&members(&["c", "a", "b"]), &ordering),
        vec!["a", "b", "c"],
        "order comes from the chain, not from link order",
    );
}

#[test]
fn empty_collection_is_empty() {
    assert_eq!(s().reconstruct(&[], &[]), Vec::<String>::new());
}

#[test]
fn members_without_entries_are_appended_oldest_first() {
    // The unordered→ordered migration, and any peer that wrote data links
    // without ordering ones.
    let ordering = vec![entry("b", LIST_HEAD, "0000000000000001_did:x")];
    assert_eq!(
        s().reconstruct(&members(&["a", "b", "c"]), &ordering),
        vec!["b", "a", "c"],
        "positioned items first, then the rest by link timestamp",
    );
}

#[test]
fn a_cycle_truncates_rather_than_hangs() {
    let ordering = vec![
        entry("a", "b", "0000000000000001_did:x"),
        entry("b", "a", "0000000000000002_did:x"),
    ];
    let out = s().reconstruct(&members(&["a", "b"]), &ordering);
    // Neither is reachable from the head, so both fall to the tail. The point
    // is that this returns at all.
    assert_eq!(out.len(), 2);
}

// ---- concurrency ---------------------------------------------------------

#[test]
fn concurrent_inserts_at_one_predecessor_linearise_by_pid() {
    // Two agents each insert after `a`, neither having seen the other.
    let ordering = vec![
        entry("a", LIST_HEAD, "0000000000000001_did:x"),
        entry("b", "a", "0000000000000009_did:agentB"),
        entry("c", "a", "0000000000000005_did:agentA"),
    ];
    assert_eq!(
        s().reconstruct(&members(&["a", "b", "c"]), &ordering),
        vec!["a", "b", "c"],
        "higher pid first, so every peer agrees",
    );
}

#[test]
fn concurrent_moves_of_one_item_resolve_last_write_wins() {
    // A moves `b` after `c`; X moves `b` after `d`. Inherently conflicting —
    // no CRDT merges these — so the requirement is agreement, not correctness.
    let base = vec![
        entry("a", LIST_HEAD, "0000000000000001_did:x"),
        entry("c", "a", "0000000000000002_did:x"),
        entry("d", "c", "0000000000000003_did:x"),
        entry("b", "a", "0000000000000004_did:x"),
    ];
    let mut peer1 = base.clone();
    peer1.push(entry("b", "c", "0000000000000010_did:A"));
    peer1.push(entry("b", "d", "0000000000000011_did:X"));

    let mut peer2 = base.clone();
    // Same entries, opposite arrival order.
    peer2.push(entry("b", "d", "0000000000000011_did:X"));
    peer2.push(entry("b", "c", "0000000000000010_did:A"));

    let m = members(&["a", "b", "c", "d"]);
    assert_eq!(s().reconstruct(&m, &peer1), s().reconstruct(&m, &peer2));
    assert_eq!(s().reconstruct(&m, &peer1), vec!["a", "c", "d", "b"]);
}

// ---- deletion, without tombstones ----------------------------------------

mod deletion {
    use super::*;

    #[test]
    fn a_removed_member_disappears_from_the_order() {
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
            entry("c", "b", "0000000000000003_did:x"),
        ];
        // b's data link is gone; its entry remains and is simply not a member.
        assert_eq!(
            s().reconstruct(&members(&["a", "c"]), &ordering),
            vec!["a", "c"],
        );
    }

    #[test]
    fn an_insert_after_a_deleted_item_still_resolves() {
        // The case tombstones existed for: agent A deletes `b` while agent X,
        // not knowing, inserts `d` after it. `d` must not be orphaned.
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
            entry("c", "b", "0000000000000003_did:x"),
            entry("d", "b", "0000000000000009_did:X"),
        ];
        assert_eq!(
            s().reconstruct(&members(&["a", "c", "d"]), &ordering),
            vec!["a", "d", "c"],
            "the chain is traversed through the absent item, which is what \
             kept `d` reachable — no tombstone needed to bridge it",
        );
    }

    #[test]
    fn deleting_the_head_keeps_the_rest() {
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
        ];
        assert_eq!(s().reconstruct(&members(&["b"]), &ordering), vec!["b"]);
    }

    #[test]
    fn deleting_a_whole_run_keeps_the_ends() {
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
            entry("c", "b", "0000000000000003_did:x"),
            entry("d", "c", "0000000000000004_did:x"),
        ];
        assert_eq!(
            s().reconstruct(&members(&["a", "d"]), &ordering),
            vec!["a", "d"],
        );
    }

    #[test]
    fn two_agents_deleting_the_same_item_is_idempotent() {
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
        ];
        // Deletion writes nothing, so "both deleted it" is indistinguishable
        // from "one did" — which is the desired outcome and needs no merge.
        assert_eq!(s().reconstruct(&members(&["a"]), &ordering), vec!["a"]);
    }

    #[test]
    fn resurrect_restores_the_original_position() {
        // Strictly better than the tombstone design, where the tombstone's
        // higher pid would keep the item hidden after its data link returned.
        let ordering = vec![
            entry("a", LIST_HEAD, "0000000000000001_did:x"),
            entry("b", "a", "0000000000000002_did:x"),
            entry("c", "b", "0000000000000003_did:x"),
        ];
        assert_eq!(
            s().reconstruct(&members(&["a", "c"]), &ordering),
            vec!["a", "c"]
        );
        assert_eq!(
            s().reconstruct(&members(&["a", "b", "c"]), &ordering),
            vec!["a", "b", "c"],
            "b comes back where it was",
        );
    }
}

// ---- generation and diffing ----------------------------------------------

#[test]
fn generate_full_chains_the_array() {
    let entries = s().generate_full(
        &["a".into(), "b".into(), "c".into()],
        PRED,
        "did:x",
        1_700_000_000_000,
    );
    assert_eq!(entries[0].after.as_deref(), Some(LIST_HEAD));
    assert_eq!(entries[1].after.as_deref(), Some("a"));
    assert_eq!(entries[2].after.as_deref(), Some("b"));
    assert!(
        entries[0].pid < entries[1].pid && entries[1].pid < entries[2].pid,
        "pids are zero-padded so string order is numeric order",
    );
    assert_eq!(
        s().reconstruct(&members(&["c", "b", "a"]), &entries),
        vec!["a", "b", "c"],
    );
}

#[test]
fn a_pid_keeps_its_sequence_out_of_its_timestamp() {
    // Folded into the timestamp, the last entry of a batch minted at t and the
    // first of one minted a millisecond later would be the same pid — and
    // `latest_per_item` resolves equal pids by arrival order, which is exactly
    // the machine-dependent tiebreak the padding exists to rule out.
    assert_ne!(
        make_pid(1_000, 1, "did:x"),
        make_pid(1_001, 0, "did:x"),
        "different (timestamp, seq) pairs must never collide",
    );
    assert!(
        make_pid(1_000, 1, "did:x") < make_pid(1_001, 0, "did:x"),
        "and the later timestamp still sorts higher",
    );
    assert!(
        make_pid(1_000, 2, "did:x") < make_pid(1_000, 10, "did:x"),
        "seq is padded too, so its string order is numeric order",
    );
}

#[test]
fn diff_of_an_unchanged_array_writes_nothing() {
    let ordering = s().generate_full(&["a".into(), "b".into(), "c".into()], PRED, "did:x", 1_000);
    let m = members(&["a", "b", "c"]);
    let diff = s().diff(
        &ordering,
        &m,
        &["a".into(), "b".into(), "c".into()],
        PRED,
        "did:x",
        2_000,
    );
    assert!(
        diff.add.is_empty(),
        "a save that changed nothing costs nothing"
    );
}

#[test]
fn diff_moves_only_what_has_to_move() {
    // [A,B,C,D] → [A,C,B,D] — the case that shows why this cannot be a
    // longest-increasing-subsequence diff. LIS says only B has to move, and
    // moving only B leaves C still pointing at B while B now points at C: a
    // two-item cycle where the list used to be.
    let ordering = s().generate_full(
        &["a".into(), "b".into(), "c".into(), "d".into()],
        PRED,
        "did:x",
        1_000,
    );
    let m = members(&["a", "b", "c", "d"]);
    let diff = s().diff(
        &ordering,
        &m,
        &["a".into(), "c".into(), "b".into(), "d".into()],
        PRED,
        "did:x",
        2_000,
    );

    let moved: Vec<&str> = diff.add.iter().map(|e| e.item.as_str()).collect();
    assert!(moved.contains(&"b"), "b moved: {moved:?}");
    assert!(
        !moved.contains(&"a"),
        "a's predecessor is unchanged, so it must be left alone — regenerating \
         it would clobber a remote agent's move with a higher pid: {moved:?}",
    );

    let mut merged = ordering.clone();
    merged.extend(diff.add.clone());
    assert_eq!(
        s().reconstruct(&m, &merged),
        vec!["a", "c", "b", "d"],
        "and the diff actually produces the requested order",
    );
}

#[test]
fn diff_positions_a_new_item() {
    let ordering = s().generate_full(&["a".into(), "b".into()], PRED, "did:x", 1_000);
    let m = members(&["a", "b", "n"]);
    let diff = s().diff(
        &ordering,
        &m,
        &["a".into(), "n".into(), "b".into()],
        PRED,
        "did:x",
        2_000,
    );
    let mut merged = ordering.clone();
    merged.extend(diff.add.clone());
    assert_eq!(s().reconstruct(&m, &merged), vec!["a", "n", "b"]);
}

#[test]
fn diff_needs_no_entry_to_remove_an_item() {
    let ordering = s().generate_full(&["a".into(), "b".into(), "c".into()], PRED, "did:x", 1_000);
    let diff = s().diff(
        &ordering,
        &members(&["a", "c"]),
        &["a".into(), "c".into()],
        PRED,
        "did:x",
        2_000,
    );
    assert!(
        diff.add.is_empty(),
        "dropping the data link is the whole deletion: {:?}",
        diff.add,
    );
}

#[test]
fn generate_append_lands_at_the_end() {
    let ordering = s().generate_full(&["a".into(), "b".into()], PRED, "did:x", 1_000);
    let e = s().generate_append("c", &ordering, &members(&["a", "b"]), PRED, "did:x", 2_000);
    assert_eq!(e.after.as_deref(), Some("b"));

    let mut merged = ordering.clone();
    merged.push(e);
    assert_eq!(
        s().reconstruct(&members(&["a", "b", "c"]), &merged),
        vec!["a", "b", "c"],
    );
}

#[test]
fn generate_append_into_an_empty_collection_starts_the_chain() {
    let e = s().generate_append("a", &[], &[], PRED, "did:x", 1_000);
    assert_eq!(e.after.as_deref(), Some(LIST_HEAD));
}

// ---- scoping and encoding ------------------------------------------------

#[test]
fn entries_are_scoped_to_their_relation() {
    // One parent may own several ordered collections; they share the predicate
    // the links are stored under, so the entry has to say which it belongs to.
    let mut other = entry("z", LIST_HEAD, "0000000000000001_did:x");
    other.predicate = "we://comments".to_string();
    let targets = vec![
        encode_ordering_entry(&entry("a", LIST_HEAD, "0000000000000001_did:x")).unwrap(),
        encode_ordering_entry(&other).unwrap(),
    ];

    let parsed = parse_ordering_entries(&targets, PRED);
    assert_eq!(parsed.len(), 1);
    assert_eq!(parsed[0].item, "a");
}

#[test]
fn an_unparseable_entry_is_skipped_not_fatal() {
    let targets = vec![
        "literal:json:{not json".to_string(),
        encode_ordering_entry(&entry("a", LIST_HEAD, "0000000000000001_did:x")).unwrap(),
    ];
    assert_eq!(parse_ordering_entries(&targets, PRED).len(), 1);
}

#[test]
fn entries_round_trip_through_their_link_target() {
    let e = entry("a", LIST_HEAD, "0000000000000001_did:x");
    let encoded = encode_ordering_entry(&e).unwrap();
    assert_eq!(parse_ordering_entries(&[encoded], PRED), vec![e]);
}

#[test]
fn an_item_uri_holding_a_percent_sequence_survives_the_round_trip() {
    // The reader percent-decodes, so an unencoded payload would hand back
    // "ad4m://item one" — an item that matches no membership link, silently
    // demoting it to the by-timestamp tail.
    let e = entry(
        "ad4m://item%20one",
        "ad4m://after%2Fslash",
        "0000000000000001_did:x",
    );
    let encoded = encode_ordering_entry(&e).unwrap();
    assert_eq!(parse_ordering_entries(&[encoded], PRED), vec![e]);
}

#[test]
fn fractional_index_is_reserved_but_refused() {
    assert!(create_strategy("linkedList").is_ok());
    let err = match create_strategy("fractionalIndex") {
        Ok(_) => panic!("fractionalIndex must not be constructible"),
        Err(e) => e.to_string(),
    };
    assert!(err.contains("causal graph"), "{err}");
    assert!(create_strategy("nonsense").is_err());
}
