//! Fixtures shared by the receipt tests.

use super::EvidencePreimage;
use crate::perspectives::flow_evaluator::{evidence_hash, EvidenceItem};
pub(super) use crate::perspectives::flow_instance::test_support::{
    did_of, hash_of, out_item, out_items, signed_proposal, signed_terminal_proposal, signed_vote,
    T1, T2, T3,
};
use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
use crate::perspectives::shacl_parser::SHACLFlow;
use serde_json::Value;
/// Persona names rather than DIDs: every proposal below is now signed for
/// real, because `mint` folds through
/// [`ReadSet::reverified`](crate::perspectives::flow_instance::ReadSet::reverified)
/// and a `did:key:alice` placeholder signs nothing.
pub(super) const ALICE: &str = "alice";
pub(super) const BOB: &str = "bob";

pub(super) const INSTANCE: &str = "ad4m://flow/instance/i1";
pub(super) const BASE: &str = "ad4m://task/t1";
pub(super) const DELIVERABLE: &str = "coasys://Deliverable";
/// The node the honest run's final proposal names as its output.
pub(super) const OUTPUT: &str = "ad4m://deliverable/d1";
/// A node the honest run never committed to.
pub(super) const ATTACKER: &str = "ad4m://attacker/node";

/// `open → done`, `done` terminal. `extra` is spliced into the states and
/// transitions so one fixture covers the branch and multi-hop shapes.
pub(super) fn flow_json(states: Value, transitions: Value) -> SHACLFlow {
    serde_json::from_value(serde_json::json!({
        "name": "Delivery",
        "namespace": "coasys://",
        "states": states,
        "transitions": transitions,
    }))
    .expect("fixture flow parses")
}

/// `open → done`, `done` terminal and guarded. The guard is incidental to
/// the outputs since #1104; `unguarded_flow` is the same without it.
pub(super) fn two_state_flow() -> SHACLFlow {
    flow_json(
        serde_json::json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        serde_json::json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    )
}

/// One proposal, self-proposed and therefore self-voted: under the
/// default `{ n: 1 }` rule that is a settled edge. `proposer` is a
/// persona *name*; the links are signed with that persona's real key.
/// `nonce` salts the content-addressed URI the fixture computes — the
/// old per-fixture URI strings serve as nonces now.
pub(super) fn proposal(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    seal: &str,
    at: &str,
) -> ProposalLinks {
    let (uri, links) = signed_proposal(nonce, proposer, from, to, seal, at);
    ProposalLinks { uri, links }
}

/// A proposal into a terminal state: signed like [`proposal`], and
/// additionally naming `outputs` and signing `committed` as their
/// `outputs_hash`. Honest when `committed == hash_of(outputs)`.
#[allow(clippy::too_many_arguments)]
pub(super) fn committing(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    seal: &str,
    outputs: &[&str],
    committed: &str,
    at: &str,
) -> ProposalLinks {
    let (uri, links) =
        signed_terminal_proposal(nonce, proposer, from, to, seal, outputs, committed, at);
    ProposalLinks { uri, links }
}

/// Alice's honest final proposal `open → done`, committing to [`OUTPUT`].
pub(super) fn final_proposal(at: &str) -> ProposalLinks {
    committing(
        "ad4m://p/1",
        ALICE,
        "open",
        "done",
        &delivered().seal,
        &[OUTPUT],
        &hash_of(&[OUTPUT]),
        at,
    )
}

/// Each id's preimage as the fixture graph holds it ([`out_items`]).
pub(super) fn outs(ids: &[&str]) -> Vec<EvidenceItem> {
    out_items(ids)
}

pub(super) fn read_set(genesis: &str, proposals: Vec<ProposalLinks>) -> ReadSet {
    ReadSet {
        instance_uri: INSTANCE.to_string(),
        subject: BASE.to_string(),
        genesis: genesis.to_string(),
        proposals,
        role_grants: Vec::new(),
    }
}

pub(super) fn completed() -> ReadSet {
    read_set("open", vec![final_proposal(T1)])
}

/// What `done`'s guard matched on the honest run: one deliverable.
pub(super) fn delivered() -> EvidencePreimage {
    deliverables(&[OUTPUT])
}

pub(super) fn deliverables(ids: &[&str]) -> EvidencePreimage {
    preimage(
        &[DELIVERABLE],
        ids.iter()
            .map(|id| item(id, DELIVERABLE, &format!("{{\"id\":\"{id}\"}}")))
            .collect(),
    )
}

pub(super) fn preimage(class_names: &[&str], items: Vec<EvidenceItem>) -> EvidencePreimage {
    let class_names: Vec<String> = class_names.iter().map(|s| s.to_string()).collect();
    EvidencePreimage {
        seal: evidence_hash(&class_names, &items),
        class_names,
        items,
    }
}

pub(super) fn item(id: &str, class_name: &str, content: &str) -> EvidenceItem {
    EvidenceItem {
        id: id.to_string(),
        class_name: class_name.to_string(),
        content: content.to_string(),
    }
}
