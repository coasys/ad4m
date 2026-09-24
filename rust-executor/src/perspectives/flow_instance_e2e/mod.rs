//! Live-store coverage for the derived state: real perspective, real SDNA,
//! real signatures, no LLM.
//!
//! Every test here is named for the attack or the ruling it pins. Before the
//! fold, each of the forgeries below moved a flow, deleted an honest
//! proposal, suppressed a mint or fabricated history with a single link that
//! any neighbourhood member can write.
//!
//! One of them pins *intended* behaviour that reads like a hole and is not
//! one: deleting a settled vote regresses the state (the graph is the truth
//! and the state follows it). The role-revocation tests pin the CORRECT
//! behaviour introduced by tombstone revocation: a revocation is an explicit
//! signed link, and eligibility is gated as-of each vote's own timestamp, so
//! revocation only affects votes cast AFTER it — settled edges stay settled.

use super::flow_classes::{advance_flow_instance_state, FLOW_CURRENT_STATE_PREDICATE};
use super::flow_context::load_shacl_flows;
use super::flow_evaluator::recompute_evidence_hash;
use super::flow_evaluator_e2e::{
    literal, second_agent, seed_flow, seed_satisfied_fixture, Fixture,
};
use super::flow_instance::accept::{accept_flow_proposal, load_outputs, reject_flow_proposal};
use super::flow_instance::atom::{
    outputs_hash, OutputRef, OutputsRefusal, TransitionAtom, ACCEPTED_BY_PREDICATE, FIRED_MARK,
    RESOLVED_AS_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE, TO_STATE_PREDICATE,
};
use super::flow_instance::fold::DerivedState;
use super::flow_instance::grant::GrantContext;
use super::flow_instance::pass::{run_flow_consensus_pass, FireOutcome};
use super::flow_instance::propose::propose_flow_transition;
use super::flow_instance::{fold_read_set, FlowInstance, ReadSet};
use crate::agent::signatures::TestSigner;
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus, PerspectiveDiff};

mod cache_and_marks;
mod content_addressed_uri;
mod granted_by;
mod manual_path;
mod outputs_commitment;
mod produced_by;
mod proposer_and_evidence;
mod quorum_and_read_set;
mod roles;
mod state_follows_links;
const TASK: &str = "ad4m://task/1";

// ---------------------------------------------------------------------------
// Helpers — each names the attacker capability it stands in for.
// ---------------------------------------------------------------------------

fn acting_did(f: &Fixture) -> String {
    crate::agent::did_for_context(&f.ctx).expect("did_for_context")
}

/// A wrong value in this replica's OWN cache — what a stale or corrupted
/// local `currentState` looks like. (A peer's write is a different shape,
/// since the cache is `Local` now: see
/// `a_peer_written_shared_cache_is_overridden_not_deleted`.)
async fn forge_cached_state(f: &mut Fixture, state: &str) {
    advance_flow_instance_state(&mut f.perspective, &f.instance_uri, state, None, &f.ctx)
        .await
        .expect("forge currentState");
}

/// The evidence seal the engine would compute for `to_state` right now.
async fn seal_for(f: &Fixture, to_state: &str) -> String {
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let records = f.instances().await;
    recompute_evidence_hash(
        &f.perspective,
        &flows[&f.flow_uri],
        &records[0],
        to_state,
        &acting_did(f),
    )
    .await
    .expect("recompute_evidence_hash")
    .hash()
    .expect("guard satisfied")
}

/// Write a proposal the way the engine writes one: real seal, real evidence.
async fn propose(f: &mut Fixture, id: &str, from: &str, to: &str) -> String {
    let seal = seal_for(f, to).await;
    f.write_proposal(id, from, to, &[TASK.to_string()], &seal)
        .await
}

/// A proposal with a non-empty seal this replica cannot reproduce — what a
/// proposal that synced ahead of the evidence it cites looks like locally.
async fn propose_unverifiable(f: &mut Fixture, id: &str, from: &str, to: &str) -> String {
    f.write_proposal(id, from, to, &[], "a-seal-this-replica-cannot-reproduce")
        .await
}

/// A `resolved_as → "fired"` mark this replica did not derive — written
/// `Shared`, as any member can write one on any proposal; that is precisely
/// why the fold never reads it, and why the pass counts only `Local` marks.
async fn forge_fired_mark(f: &mut Fixture, proposal_uri: &str) {
    f.link(
        proposal_uri,
        RESOLVED_AS_PREDICATE,
        &literal(FIRED_MARK),
        LinkStatus::Shared,
    )
    .await;
}

/// Give a state its own `consensusRule`, as the flow author would.
async fn set_consensus_rule(f: &mut Fixture, state_uri: &str, rule: &str) {
    f.link(
        state_uri,
        "ad4m://consensusRule",
        &literal(rule),
        LinkStatus::Local,
    )
    .await;
}

async fn consensus_pass(f: &mut Fixture) -> Vec<FireOutcome> {
    run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None, None).await
}

async fn proposal_exists(f: &Fixture, uri: &str) -> bool {
    !f.links_by_predicate(uri).await.is_empty()
}

/// A second agent's vote, delivered exactly as sync would deliver it: signed
/// by a real key that is not this replica's.
async fn sync_vote_from(f: &mut Fixture, signer: &TestSigner, proposal_uri: &str) {
    let vote = signer.sign(
        Link {
            source: proposal_uri.to_string(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: signer.did.clone(),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(vote), LinkStatus::Shared, None)
        .await
        .expect("sync a second agent's vote");
}

/// Another agent's whole proposal, delivered as sync would deliver it: every
/// link signed by that agent's own key, so the identity checks see a real
/// second proposer rather than one this replica wrote in their name.
async fn sync_proposal_from(
    f: &mut Fixture,
    signer: &TestSigner,
    id: &str,
    from: &str,
    to: &str,
    seal: &str,
) -> String {
    // An empty outputs commitment: every fixture flow's target here is
    // terminal, and a co-signer refuses a terminal proposal with none
    // (#1104). Harmless on a non-terminal target, where nobody reads it.
    let empty = outputs_hash(&[]);
    sync_committed_proposal_from(f, signer, id, from, to, seal, &[], Some(&empty)).await
}

/// [`sync_proposal_from`] with the outputs a proposal into a terminal state
/// names, and the `outputs_hash` it signs (`None`: it signs none). Honest
/// when `committed` is `outputs_hash(outputs)`; anything else is a proposer
/// whose commitment does not match what it names.
#[allow(clippy::too_many_arguments)]
async fn sync_committed_proposal_from(
    f: &mut Fixture,
    signer: &TestSigner,
    id: &str,
    from: &str,
    to: &str,
    seal: &str,
    outputs: &[OutputRef],
    committed: Option<&str>,
) -> String {
    use super::flow_instance::atom::{
        proposal_uri, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE, FROM_STATE_PREDICATE,
        OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSAL_NONCE_PREDICATE, PROPOSER_PREDICATE,
    };
    // The peer computes the content-addressed URI exactly as the engine does
    // (#1108); `id` is their nonce. A peer that did not would sync a
    // proposal no replica reads as an atom.
    let instance_uri = f.instance_uri.clone();
    let uri = proposal_uri(&instance_uri, from, to, seal, committed, &signer.did, id);
    let mut links = vec![
        (PROPOSER_PREDICATE, signer.did.clone()),
        (FLOW_INSTANCE_PREDICATE, instance_uri),
        (FROM_STATE_PREDICATE, literal(from)),
        (TO_STATE_PREDICATE, literal(to)),
        (EVIDENCE_HASHES_PREDICATE, literal(seal)),
        (PROPOSAL_NONCE_PREDICATE, literal(id)),
    ];
    links.extend(
        outputs
            .iter()
            .map(|r| (OUTPUT_PREDICATE, literal(&r.encode()))),
    );
    if let Some(committed) = committed {
        links.push((OUTPUTS_HASH_PREDICATE, literal(committed)));
    }
    for (predicate, target) in links {
        let signed = signer.sign(
            Link {
                source: uri.clone(),
                predicate: Some(predicate.to_string()),
                target,
            }
            .normalize(),
        );
        f.perspective
            .add_link_expression(LinkExpression::from(signed), LinkStatus::Shared, None)
            .await
            .expect("sync a foreign proposal link");
    }
    uri
}

/// A peer's `resolved_as → "fired"` mark, delivered as sync would deliver
/// it: signed by their key, and therefore `Shared` on this replica.
async fn sync_fired_mark_from(f: &mut Fixture, signer: &TestSigner, proposal_uri: &str) {
    let mark = signer.sign(
        Link {
            source: proposal_uri.to_string(),
            predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
            target: literal(FIRED_MARK),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(mark), LinkStatus::Shared, None)
        .await
        .expect("sync a peer's fired mark");
}

/// Every `currentState` link on the fixture's instance, whoever wrote it.
async fn current_state_links(f: &Fixture) -> Vec<crate::types::DecoratedLinkExpression> {
    links_of(f, &f.instance_uri)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some(FLOW_CURRENT_STATE_PREDICATE))
        .collect()
}

async fn links_of(f: &Fixture, source: &str) -> Vec<crate::types::DecoratedLinkExpression> {
    f.perspective
        .get_links(&LinkQuery {
            source: Some(source.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
}

/// `review ⇄ changes_requested`, plus `review → approved`. Every state
/// carries the same guard so any edge can be sealed, which lets these tests
/// drive multi-hop and cyclic histories explicitly.
fn review_flow() -> serde_json::Value {
    let guard = serde_json::json!([{ "className": "ns://Task", "count": { "min": 1 } }]);
    serde_json::json!({
        "name": "Review",
        "namespace": "review://",
        "states": [
            { "name": "review", "value": 0.0, "requires": guard },
            { "name": "changes_requested", "value": 0.5, "requires": guard },
            { "name": "approved", "value": 1.0, "requires": guard },
        ],
        "transitions": [
            { "action_name": "Request", "from_state": "review", "to_state": "changes_requested", "actions": [] },
            { "action_name": "Resubmit", "from_state": "changes_requested", "to_state": "review", "actions": [] },
            { "action_name": "Approve", "from_state": "review", "to_state": "approved", "actions": [] },
        ],
    })
}

async fn seed_review_flow() -> Fixture {
    let mut f = seed_flow(review_flow(), "review").await;
    f.seed_task(TASK, "Review the onboarding doc").await;
    f
}

/// Settle one declared edge and return the proposal that did it.
async fn settle(f: &mut Fixture, id: &str, from: &str, to: &str) -> String {
    let uri = propose(f, id, from, to).await;
    let outcomes = consensus_pass(f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "{from} → {to} must settle exactly once: {outcomes:?}"
    );
    uri
}

fn walked(derived: &DerivedState) -> Vec<(String, String)> {
    derived
        .settled
        .iter()
        .map(|e| (e.from_state.clone(), e.to_state.clone()))
        .collect()
}

/// Link timestamps are millisecond RFC3339 and the as-of gate compares them,
/// so steps whose order matters must not land in the same millisecond.
async fn tick() {
    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
}

const OWNER_RULE: &str = r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;

async fn grant_owner_role(f: &mut Fixture) {
    let me = acting_did(f);
    f.link(TASK, "ns://owner", &literal(&me), LinkStatus::Local)
        .await;
}

/// This replica's agent tombstones its own grant on `role_instance`.
async fn revoke_own_role(f: &mut Fixture, role_instance: &str) {
    let me = acting_did(f);
    f.link(
        role_instance,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&me),
        LinkStatus::Shared,
    )
    .await;
}

/// Whether this replica's agent has an `acceptedBy` on `proposal`.
async fn we_voted_on(f: &Fixture, proposal: &str) -> bool {
    let me = acting_did(f);
    f.links_by_predicate(proposal)
        .await
        .get(ACCEPTED_BY_PREDICATE)
        .is_some_and(|targets| targets.iter().any(|t| *t == me))
}

/// `id` as an output of the fixture's `ns://Task` class.
fn task_ref(id: &str) -> OutputRef {
    OutputRef {
        class_name: "ns://Task".to_string(),
        id: id.to_string(),
    }
}

/// The commitment an honest proposer signs over `refs`: their content as
/// this replica reads it now, through the loader every voter uses.
async fn honest_commitment(f: &Fixture, refs: &[OutputRef]) -> String {
    let loaded = load_outputs(&f.perspective, refs)
        .await
        .expect("load outputs");
    let items: Vec<_> = refs
        .iter()
        .map(|r| {
            loaded
                .get(r)
                .cloned()
                .unwrap_or_else(|| panic!("{r:?} is not an instance on this replica"))
        })
        .collect();
    outputs_hash(&items)
}
