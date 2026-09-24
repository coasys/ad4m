//! Tests of `verify_receipt`, by topic. The fixtures and the two control
//! tests live here; each topic is a `mod` below.

mod ingest;
mod outputs;
mod post_cosign_swap;
mod refusals;
use super::verdict::VerdictKind;
use super::*;
use crate::perspectives::flow_evaluator::{evidence_hash, EvidenceItem};
use crate::perspectives::flow_instance::atom::ACCEPTED_BY_PREDICATE;
use crate::perspectives::flow_instance::receipt::EvidencePreimage;
use crate::perspectives::flow_instance::roles::{RoleGrantEvidence, RoleInstanceHistory};
use crate::perspectives::flow_instance::test_support::{
    did_of, hash_of, out_item, out_items, out_ref, signed_link, signed_terminal_proposal,
    signed_vote, T1, T2, T3,
};
use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
use crate::types::DecoratedLinkExpression;
use crate::types::LinkExpression;
use serde_json::{json, Value};

const INSTANCE: &str = "ad4m://flow/instance/i1";
const BASE: &str = "ad4m://task/t1";
const ALICE: &str = "alice";
const BOB: &str = "bob";
/// A third genuine signer, for the fixtures that have to show a forged
/// link being dropped *without* taking the honest links beside it.
const CAROL: &str = "carol";
const REVIEWER: &str = "coasys://Reviewer";
/// What most fixture flows' terminal `done` state requires. Incidental
/// to the outputs since #1104: they are what the final proposal names.
const DELIVERABLE: &str = "coasys://Deliverable";
/// The node every honest final proposal names as the run's output.
const OUTPUT: &str = "ad4m://deliverable/d1";
/// A node the run never sealed, named by whoever re-mints it.
const ATTACKER: &str = "ad4m://attacker/node";
/// Earlier than any grant link a test writes — the fallback dating a
/// dropped grant link must *not* be allowed to fall back to.
const INSTANCE_CREATED: &str = "2025-12-01T00:00:00.000Z";

// ---- fixtures --------------------------------------------------------

fn flow_json(states: Value, transitions: Value) -> SHACLFlow {
    serde_json::from_value(json!({
        "name": "Delivery",
        "namespace": "coasys://",
        "states": states,
        "transitions": transitions,
    }))
    .expect("fixture flow parses")
}

/// `open → done`, `done` terminal, default `{ n: 1 }` quorum.
fn two_state_flow() -> SHACLFlow {
    flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    )
}

fn catalogue(flows: Vec<SHACLFlow>) -> HashMap<String, SHACLFlow> {
    flows.into_iter().map(|f| (f.flow_uri(), f)).collect()
}

/// One proposal out of `open` into a terminal state, self-proposed and
/// therefore self-voted, naming [`OUTPUT`] and committing to it.
/// `proposer` is a persona name; the links carry that persona's real
/// signature. `nonce` salts the content-addressed URI the fixture
/// computes — the old per-fixture URI strings serve as nonces now.
fn proposal(nonce: &str, proposer: &str, to: &str, at: &str) -> ProposalLinks {
    let (uri, links) = final_links(nonce, proposer, "open", to, at);
    ProposalLinks { uri, links }
}

/// An honest proposal into terminal `to`, committing to [`OUTPUT`]:
/// its content-addressed URI and its links.
fn final_links(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    at: &str,
) -> (String, Vec<DecoratedLinkExpression>) {
    committed_links(
        nonce,
        proposer,
        from,
        to,
        &[OUTPUT],
        &hash_of(&[OUTPUT]),
        at,
    )
}

/// A proposal into terminal `to` naming `outputs` and signing
/// `committed` as their hash. Honest when `committed ==
/// hash_of(outputs)`.
fn committed_links(
    nonce: &str,
    proposer: &str,
    from: &str,
    to: &str,
    outputs: &[&str],
    committed: &str,
    at: &str,
) -> (String, Vec<DecoratedLinkExpression>) {
    signed_terminal_proposal(nonce, proposer, from, to, &seal(), outputs, committed, at)
}

/// Each id's preimage as the fixture graph holds it ([`out_items`]).
fn outs(ids: &[&str]) -> Vec<EvidenceItem> {
    out_items(ids)
}

/// What `Verified.outputs` reports for `ids`.
fn refs(ids: &[&str]) -> Vec<OutputRef> {
    ids.iter().map(|id| out_ref(id)).collect()
}

/// What `done`'s guard matched on the honest run, as a mint carries it.
fn delivered() -> EvidencePreimage {
    deliverables(&[OUTPUT])
}

/// The seal every fixture proposal into `done` carries.
fn seal() -> String {
    delivered().seal
}

fn deliverables(ids: &[&str]) -> EvidencePreimage {
    sealed_over(
        &[DELIVERABLE],
        ids.iter()
            .map(|id| EvidenceItem {
                id: id.to_string(),
                class_name: DELIVERABLE.to_string(),
                content: format!("{{\"id\":\"{id}\"}}"),
            })
            .collect(),
    )
}

fn sealed_over(class_names: &[&str], items: Vec<EvidenceItem>) -> EvidencePreimage {
    let class_names: Vec<String> = class_names.iter().map(|s| s.to_string()).collect();
    EvidencePreimage {
        seal: evidence_hash(&class_names, &items),
        class_names,
        items,
    }
}

fn read_set(proposals: Vec<ProposalLinks>, role_grants: Vec<RoleGrantEvidence>) -> ReadSet {
    ReadSet {
        instance_uri: INSTANCE.to_string(),
        subject: BASE.to_string(),
        genesis: "open".to_string(),
        proposals,
        role_grants,
    }
}

fn completed() -> ReadSet {
    read_set(vec![proposal("ad4m://p/1", ALICE, "done", T1)], Vec::new())
}

fn mint(flow: &SHACLFlow, rs: ReadSet) -> FlowReceipt {
    FlowReceipt::mint(flow, rs, outs(&[OUTPUT]), vec![delivered()])
        .expect("the fixture read-set mints")
}

// ---- the happy path, as the control for everything below --------------

/// The control: honest material, the reader's own copy of the flow, and
/// the fold re-derives what the receipt claims — including *who* settled
/// it, which is the "n distinct eligible DIDs" a receipt actually asserts.
///
/// Red if `verify_receipt` reports `terminal_state` from the receipt
/// rather than from its own fold — e.g. `terminal_state:
/// receipt.terminal_state.clone()` in the `Verified` arm — because then
/// `StateMismatch` below could never distinguish the two.
#[test]
fn an_honest_receipt_verifies_and_names_the_quorum_that_settled_it() {
    let flow = two_state_flow();
    let receipt = mint(&flow, completed());

    let verdict = verify_receipt(&catalogue(vec![flow]), &receipt);
    assert_eq!(
        verdict,
        ReceiptVerdict::Verified {
            terminal_state: "done".into(),
            outputs: refs(&[OUTPUT]),
            voters: vec![did_of(ALICE).to_string()],
        },
        "got: {verdict}"
    );
    assert!(
        receipt.speaks_for(&out_ref(OUTPUT)),
        "the binding a `granted_by` edge is checked against"
    );
    assert!(
        !receipt.speaks_for(&out_ref(BASE)),
        "the run's subject is not an output unless the proposer named it"
    );
}

/// **`speaks_for` takes the class, not just the id** (#1108 review,
/// should-fix). An output is an instance *of a class* — the same node
/// read through another class is other content, which is the PR's own
/// argument for hashing `(class, id, content)`. A receipt whose quorum
/// agreed to `(Deliverable, X)` must not answer for X as an instance of
/// anything else, or a #1076 grant check could be satisfied by a receipt
/// whose voters saw X through a narrow class that shows almost nothing.
///
/// Red while `speaks_for` compares ids only.
#[test]
fn speaks_for_requires_the_class_not_just_the_id() {
    let receipt = mint(&two_state_flow(), completed());
    assert!(
        receipt.speaks_for(&out_ref(OUTPUT)),
        "control: the committed (class, id) is spoken for"
    );
    assert!(
        !receipt.speaks_for(&OutputRef {
            class_name: "coasys://Role".to_string(),
            id: OUTPUT.to_string(),
        }),
        "the same id under another class is other content, and the quorum \
         never agreed to it"
    );
}

/// Helper for the FlowUnknown fixture: a second flow whose URI differs.
trait Rename {
    fn tap_rename(self, name: &str) -> Self;
}
impl Rename for SHACLFlow {
    fn tap_rename(mut self, name: &str) -> Self {
        self.name = name.to_string();
        self
    }
}
