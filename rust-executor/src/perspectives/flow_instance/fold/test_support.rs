//! Fixtures shared by the fold tests.

use super::{DerivedState, VouchedAtom};
use crate::perspectives::flow_instance::atom::{TransitionAtom, Vote};
pub(super) use crate::perspectives::flow_instance::test_support::{ALICE, BOB, T1, T2, T3};
use crate::perspectives::shacl_parser::SHACLFlow;

/// `review ⇄ changes_requested`, plus `review → approved` whose `{n}` is
/// configurable, so every quorum case fits one fixture.
pub(super) fn review_flow(approved_n: Option<u32>) -> SHACLFlow {
    let mut approved = serde_json::json!({ "name": "approved", "value": 1.0 });
    if let Some(n) = approved_n {
        approved["consensusRule"] = serde_json::json!({ "n": n });
    }
    serde_json::from_value(serde_json::json!({
        "name": "Review",
        "namespace": "review://",
        "states": [
            { "name": "review", "value": 0.0 },
            { "name": "changes_requested", "value": 0.5 },
            approved,
        ],
        "transitions": [
            { "action_name": "Request", "from_state": "review", "to_state": "changes_requested", "actions": [] },
            { "action_name": "Resubmit", "from_state": "changes_requested", "to_state": "review", "actions": [] },
            { "action_name": "Approve", "from_state": "review", "to_state": "approved", "actions": [] },
        ],
    }))
    .expect("fixture flow parses")
}

/// One atom on `from → to` carrying exactly the listed `(did, at)` votes
/// as eligible. Fields the fold never reads carry placeholders. Every
/// fixture atom shares one outputs commitment, so on a terminal edge the
/// per-commitment grouping collapses to the plain pooling these tests
/// are about; [`vouched_committing`] is the fixture for the grouping
/// itself.
pub(super) fn vouched(uri: &str, from: &str, to: &str, votes: &[(&str, &str)]) -> VouchedAtom {
    vouched_committing(uri, from, to, Some("shared-outputs-hash"), votes)
}

/// [`vouched`] with an explicit outputs commitment (`None`: the proposer
/// signed none).
pub(super) fn vouched_committing(
    uri: &str,
    from: &str,
    to: &str,
    outputs_hash: Option<&str>,
    votes: &[(&str, &str)],
) -> VouchedAtom {
    let votes: Vec<Vote> = votes
        .iter()
        .map(|(did, at)| Vote {
            did: did.to_string(),
            at: at.to_string(),
        })
        .collect();
    VouchedAtom {
        atom: TransitionAtom {
            uri: uri.to_string(),
            from_state: from.to_string(),
            to_state: to.to_string(),
            proposer: votes.first().map(|v| v.did.clone()).unwrap_or_default(),
            proposed_at: votes.first().map(|v| v.at.clone()).unwrap_or_default(),
            evidence_hash: "seal".to_string(),
            outputs_hash: outputs_hash.map(str::to_string),
            outputs: Vec::new(),
            votes: votes.clone(),
        },
        eligible_votes: votes,
    }
}

pub(super) fn walked(derived: &DerivedState) -> Vec<(&str, &str)> {
    derived
        .settled
        .iter()
        .map(|e| (e.from_state.as_str(), e.to_state.as_str()))
        .collect()
}
