//! Tests of `verify_receipt`, by topic. The fixtures and the two control
//! tests live here; each topic is a `mod` below.

mod ingest;
mod outputs;
mod post_cosign_swap;
mod refusals;
use super::verdict::VerdictKind;
use super::*;
use crate::perspectives::flow_evaluator::evidence_hash;
use crate::perspectives::flow_instance::atom::ACCEPTED_BY_PREDICATE;
use crate::perspectives::flow_instance::receipt::EvidencePreimage;
use crate::perspectives::flow_instance::roles::{RoleGrantEvidence, RoleInstanceHistory};
use crate::perspectives::flow_instance::test_support::{
    deliverables, delivered, did_of, flow_json, hash_of, out_item, out_ref, outs, signed_link,
    signed_terminal_proposal, signed_vote, two_state_flow, ATTACKER, BASE, DELIVERABLE, INSTANCE,
    OUTPUT, T1, T2, T3,
};
use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
use crate::types::DecoratedLinkExpression;
use crate::types::LinkExpression;
use serde_json::json;

const ALICE: &str = "alice";
const BOB: &str = "bob";
/// A third genuine signer, for the fixtures that have to show a forged
/// link being dropped *without* taking the honest links beside it.
const CAROL: &str = "carol";
const REVIEWER: &str = "coasys://Reviewer";
/// Earlier than any grant link a test writes — the fallback dating a
/// dropped grant link must *not* be allowed to fall back to.
const INSTANCE_CREATED: &str = "2025-12-01T00:00:00.000Z";

// ---- fixtures --------------------------------------------------------

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

/// What `Verified.outputs` reports for `ids`.
fn refs(ids: &[&str]) -> Vec<OutputRef> {
    ids.iter().map(|id| out_ref(id)).collect()
}

/// The seal every fixture proposal into `done` carries.
fn seal() -> String {
    delivered().seal
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
            settled_at: T1.into(),
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

// ---- (0a) the settle time --------------------------------------------

/// `settled_at` is the quorum time of the edge that **completed** the run,
/// not of the one that started it.
///
/// The single-edge fixture above cannot tell those apart — its first
/// settled edge is also its last — so a walk with two hops at different
/// times is the only shape that pins it. It matters because this value
/// becomes `granted_at` for a `producedByFlow` role: reporting the first
/// hop would date a grant from the moment the run *began* to be decided,
/// opening an eligibility window over a stretch in which the run had not
/// completed and the grant did not exist.
///
/// Red with `derived.settled.first()` in `verify_receipt`, which the
/// happy-path test above is blind to.
#[test]
fn a_multi_hop_run_settles_at_the_edge_that_completed_it() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "mid", "value": 0.5 },
            { "name": "done", "value": 1.0 },
        ]),
        json!([
            { "action_name": "Start", "from_state": "open", "to_state": "mid", "actions": [] },
            { "action_name": "Finish", "from_state": "mid", "to_state": "done", "actions": [] },
        ]),
    );
    let (start_uri, start_links) =
        crate::perspectives::flow_instance::test_support::signed_proposal(
            "ad4m://p/2",
            ALICE,
            "open",
            "mid",
            &seal(),
            T1,
        );
    let (finish_uri, finish_links) = final_links("ad4m://p/1", ALICE, "mid", "done", T2);
    let two_hops = read_set(
        vec![
            ProposalLinks {
                uri: finish_uri,
                links: finish_links,
            },
            ProposalLinks {
                uri: start_uri,
                links: start_links,
            },
        ],
        Vec::new(),
    );
    let receipt = mint(&flow, two_hops);

    let verdict = verify_receipt(&catalogue(vec![flow]), &receipt);
    let ReceiptVerdict::Verified { settled_at, .. } = &verdict else {
        panic!("the two-hop walk completes — got: {verdict}");
    };
    assert_eq!(
        settled_at, T2,
        "the run completed when the SECOND hop reached quorum, not the first"
    );
}

// ---- (0b) a walk that took no edge ------------------------------------

/// A flow whose genesis state has no transitions out of it is terminal
/// from the moment an instance exists. The fold "reaches" that state by
/// standing still: no atom, no vote, no quorum, nobody deciding anything.
///
/// Both sides refuse it, and the test asserts both — an asymmetric rule is
/// the defect this arc keeps guarding against, and here it would be
/// invisible until a receipt minted cleanly on one replica and failed
/// everywhere it was presented.
///
/// The hand-built receipt is the only way to reach the verify side at all,
/// which is the point: that arm exists for material `mint` would not have
/// produced.
///
/// Both sides answer through #1108's final-edge commitment check
/// ([`OutputsCommitment::NoFinalEdge`]): a walk with no settled edge has no
/// final edge, so nothing committed to any output. Red if that arm is
/// dropped on either side: the fold reaches `done`, `done` is terminal, and
/// a receipt asserting a completion with an empty voter list verifies.
#[test]
fn a_run_in_which_nobody_voted_is_not_a_completion() {
    let standing_still = flow_json(json!([{ "name": "done", "value": 1.0 }]), json!([]));
    // Genesis is this flow's own initial state, so `fold_read_set`'s
    // genesis check passes and the zero-edge refusal is what answers.
    let empty = ReadSet {
        genesis: "done".into(),
        ..read_set(Vec::new(), Vec::new())
    };
    assert_eq!(
        fold_read_set(&standing_still, &empty.reverified())
            .expect("a stateless walk folds")
            .settled
            .len(),
        0,
        "precondition: the walk takes no edge, and `done` is terminal anyway"
    );

    let err = FlowReceipt::mint(
        &standing_still,
        ReadSet {
            genesis: "done".into(),
            ..empty.clone()
        },
        outs(&[OUTPUT]),
        Vec::new(),
    )
    .expect_err("a completion nobody voted on is not a completion");
    assert!(
        format!("{err:#}").contains("the walk settled no edge"),
        "the refusal must name the missing quorum, got: {err:#}"
    );

    // The same material as a receipt that arrived from elsewhere, which is
    // the only way to reach the verifier's arm.
    let hand_built = FlowReceipt {
        flow_uri: standing_still.flow_uri(),
        flow_dna_hash: flow_dna_hash(&standing_still).expect("hash"),
        terminal_state: "done".into(),
        outputs: outs(&[OUTPUT]),
        read_set: ReadSet {
            genesis: "done".into(),
            ..empty
        },
        evidence_preimage: Vec::new(),
    };
    assert_eq!(
        verify_receipt(&catalogue(vec![standing_still]), &hand_built),
        ReceiptVerdict::NoFinalEdge,
        "and the verifier refuses exactly what mint refuses"
    );
}
