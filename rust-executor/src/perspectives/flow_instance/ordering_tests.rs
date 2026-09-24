//! The #1000 pinning tests: every ordering decision compares parsed
//! instants, never timestamp strings.
//!
//! Link timestamps are client-asserted and the two clients in this repo
//! already disagree on RFC 3339 flavour (`to_rfc3339()` → `…+00:00`,
//! `toISOString()` → `….000Z`), so inside a sub-second collision string
//! order is client-library order. Each test here uses a pair whose string
//! order and instant order disagree (see [`super::time`]'s own tests for
//! the pairs), so each one FAILS on string comparison and passes on parsed
//! comparison.
//!
//! Deliberately written against the public surface only — no reference to
//! the fixed code's new API — so the module compiles against the unfixed
//! sources too and the failures can be demonstrated, not just claimed.

use super::atom::fixtures::{atom_of, honest_proposal, link, ALICE, BOB, T1, T2};
use super::atom::{Vote, ACCEPTED_BY_PREDICATE, PROPOSER_PREDICATE};
use super::fold::{fold, VouchedAtom};
use super::roles::{RoleGrantWindow, RoleRevocation};
use crate::perspectives::shacl_parser::SHACLFlow;

/// Earlier instant, later string: `'Z'` (0x5A) sorts after `'.'` (0x2E).
const WHOLE_SECOND: &str = "2026-01-01T00:00:00Z";
/// 100ms into the same second — the earlier *string* of the two.
const POINT_ONE: &str = "2026-01-01T00:00:00.100Z";
/// The same instant as `WHOLE_SECOND`, written with an offset: earlier
/// instant than `POINT_ONE`, but string-later than both `Z` forms.
const OFFSET_FORM: &str = "2026-01-01T02:00:00+02:00";

#[test]
fn votes_order_by_instant_not_by_string() {
    // Alice mints at +100ms; Bob votes at +0ms but in the whole-second
    // format, which string-sorts AFTER Alice's. Bob voted first.
    let mut links = honest_proposal(ALICE, "review", "approved", "h1", POINT_ONE);
    links.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, true, WHOLE_SECOND));
    assert_eq!(
        atom_of(&links).expect("atom").votes,
        vec![
            Vote {
                did: BOB.into(),
                at: WHOLE_SECOND.into()
            },
            Vote {
                did: ALICE.into(),
                at: POINT_ONE.into()
            },
        ],
        "the earlier instant is the earlier vote, whatever format its client wrote"
    );
}

#[test]
fn proposed_at_is_the_earliest_instant_not_the_earliest_string() {
    // Two proposer-signed links: the offset form is the earlier instant
    // and the later string. String min() picks the wrong one.
    let mut links = honest_proposal(ALICE, "review", "approved", "h1", POINT_ONE);
    links.push(link(PROPOSER_PREDICATE, ALICE, ALICE, true, OFFSET_FORM));
    let atom = atom_of(&links).expect("atom");
    assert_eq!(
        atom.proposed_at, OFFSET_FORM,
        "the offset form names the earlier instant"
    );
}

#[test]
fn an_unparseable_vote_timestamp_cannot_become_the_earliest_vote() {
    // "" string-sorts before every real timestamp; parsed, it is nothing.
    let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
    links.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, true, ""));
    assert_eq!(
        atom_of(&links).expect("atom").votes,
        vec![Vote {
            did: ALICE.into(),
            at: T1.into()
        }],
        "a vote that cannot be placed in time is dropped, not sorted first"
    );
}

/// `review → approved` with `{n: 2}` — the shape where the n-th voter's
/// timestamp becomes `settled_at`.
fn n2_flow() -> SHACLFlow {
    serde_json::from_value(serde_json::json!({
        "name": "Review",
        "namespace": "review://",
        "states": [
            { "name": "review", "value": 0.0 },
            { "name": "approved", "value": 1.0, "consensusRule": { "n": 2 } },
        ],
        "transitions": [
            { "action_name": "Approve", "from_state": "review", "to_state": "approved", "actions": [] },
        ],
    }))
    .expect("fixture flow parses")
}

fn vouched(uri: &str, votes: &[(&str, &str)]) -> VouchedAtom {
    let votes: Vec<Vote> = votes
        .iter()
        .map(|(did, at)| Vote {
            did: (*did).to_string(),
            at: (*at).to_string(),
        })
        .collect();
    VouchedAtom {
        atom: super::atom::TransitionAtom {
            uri: uri.to_string(),
            from_state: "review".to_string(),
            to_state: "approved".to_string(),
            proposer: votes.first().map(|v| v.did.clone()).unwrap_or_default(),
            proposed_at: votes.first().map(|v| v.at.clone()).unwrap_or_default(),
            evidence_hash: "seal".to_string(),
            // `approved` is terminal, and a terminal edge pools votes per
            // outputs commitment (#1108/#1118) — one shared commitment keeps
            // these tests about ordering, not grouping.
            outputs_hash: Some("shared-outputs-hash".to_string()),
            outputs: Vec::new(),
            votes: votes.clone(),
        },
        eligible_votes: votes,
    }
}

#[test]
fn settlement_time_is_the_nth_instant_across_mixed_formats() {
    // Alice votes at the earlier instant in the offset form (string-LAST),
    // Bob 100ms later in the `.100Z` form (string-FIRST). The 2nd distinct
    // voter by instant is Bob, so the edge settles at Bob's timestamp.
    // String order would call Alice the 2nd voter instead.
    let derived = fold(
        "review",
        &n2_flow(),
        &[vouched("p1", &[(ALICE, OFFSET_FORM), (BOB, POINT_ONE)])],
    );
    assert_eq!(derived.state, "approved");
    assert_eq!(
        derived.settled[0].settled_at, POINT_ONE,
        "the n-th voter is the n-th by instant, not by string"
    );
}

#[test]
fn a_grant_window_gates_by_instant_not_by_string() {
    // Granted at 00:00:00Z written in the offset form; the vote lands 100ms
    // later. As strings the vote sorts BEFORE the grant, so string
    // comparison reads the vote as pre-grant and drops it.
    let window = RoleGrantWindow {
        instance_id: "r1".into(),
        granted_at: OFFSET_FORM.into(),
        revocations: vec![],
    };
    assert!(
        window.open_at(POINT_ONE),
        "a vote 100ms after the grant is inside the window, whatever formats the clients used"
    );
}

#[test]
fn an_unparseable_revocation_fails_closed() {
    // An authorised tombstone exists but cannot be placed in time. String
    // comparison reads `at < "garbage"` as true and leaves the window OPEN;
    // the safe reading of an undatable revocation is "revoked".
    let window = RoleGrantWindow {
        instance_id: "r1".into(),
        granted_at: T1.into(),
        revocations: vec![RoleRevocation {
            by: ALICE.into(),
            at: "garbage".into(),
        }],
    };
    assert!(
        !window.open_at(T2),
        "a revocation that cannot be placed in time closes the window, it is not ignored"
    );
}
