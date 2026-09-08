//! Live-store coverage for the derived state: real perspective, real SDNA,
//! real signatures, no LLM.
//!
//! Every test here is named for the attack or the ruling it pins. Before the
//! fold, each of the forgeries below moved a flow, deleted an honest
//! proposal, suppressed a mint or fabricated history with a single link that
//! any neighbourhood member can write.
//!
//! Two of them pin *intended* behaviour that reads like a hole and is not
//! one: deleting a settled vote regresses the state (the graph is the truth
//! and the state follows it), and revoking a role can un-settle an old edge
//! (roles are re-derived live). Both are rulings, and both are visible here
//! rather than silent.

use super::flow_classes::advance_flow_instance_state;
use super::flow_context::load_shacl_flows;
use super::flow_evaluator::recompute_evidence_hash;
use super::flow_evaluator_e2e::{literal, seed_flow, seed_satisfied_fixture, Fixture};
use super::flow_instance::accept::accept_flow_proposal;
use super::flow_instance::atom::{
    ACCEPTED_BY_PREDICATE, FIRED_MARK, RESOLVED_AS_PREDICATE, TO_STATE_PREDICATE,
};
use super::flow_instance::fold::DerivedState;
use super::flow_instance::pass::{run_flow_consensus_pass, FireOutcome};
use super::flow_instance::{fold_read_set, ReadSet};
use crate::agent::signatures::TestSigner;
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};

const TASK: &str = "ad4m://task/1";

// ---------------------------------------------------------------------------
// Helpers — each names the attacker capability it stands in for.
// ---------------------------------------------------------------------------

fn acting_did(f: &Fixture) -> String {
    crate::agent::did_for_context(&f.ctx).expect("did_for_context")
}

/// What a peer's forged `currentState` write looks like on the graph. The
/// setter drops the existing link, so afterwards the only `currentState` on
/// the instance is the attacker's value.
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

/// The `resolved_as → "fired"` mark. Any member can write it on any
/// proposal; that is precisely why the fold never reads it.
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
    use super::flow_instance::atom::{
        EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE, FROM_STATE_PREDICATE,
        PROPOSER_PREDICATE,
    };
    let uri = format!("ad4m://flow/proposal/{id}");
    let instance_uri = f.instance_uri.clone();
    for (predicate, target) in [
        (PROPOSER_PREDICATE, signer.did.clone()),
        (FLOW_INSTANCE_PREDICATE, instance_uri),
        (FROM_STATE_PREDICATE, literal(from)),
        (TO_STATE_PREDICATE, literal(to)),
        (EVIDENCE_HASHES_PREDICATE, literal(seal)),
    ] {
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

// ---------------------------------------------------------------------------
// The cache is a cache
// ---------------------------------------------------------------------------

/// Test 9. A peer writes the one link that used to BE the state, and no
/// engine decision moves: the fold starts at genesis, the evaluator still
/// mints from genesis, and the pass heals the cache. The forged value here is
/// terminal, which before the fold suppressed every mint on this instance.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_current_state_moves_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "the fold starts at genesis"
    );
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "the forgery is on the graph"
    );

    let minted = f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(
        (
            outcomes[0].from_state.as_str(),
            outcomes[0].to_state.as_str()
        ),
        ("identified", "scoped"),
        "the mint left the DERIVED state, not the forged one"
    );
    assert_eq!(outcomes[0].contributing_proposal_uris, vec![minted]);
    assert_eq!(f.derived().await.state, "scoped");
}

/// The pass's whole job, and its idempotency: it writes the cache and the
/// marks, and a second run has nothing left to record.
#[tokio::test(flavor = "multi_thread")]
async fn the_pass_writes_the_cache_and_the_marks_and_then_has_nothing_to_do() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the default {{n: 1}} rule settles at mint"
    );
    assert_eq!(outcomes[0].voters, vec![acting_did(&f)]);
    assert_eq!(f.cached_state().await, "scoped", "the cache was written");
    assert!(
        f.read_set().await.marked_proposals().contains(&minted),
        "and the settling proposal was marked"
    );
    assert_eq!(f.derived().await.state, "scoped");

    let rerun = consensus_pass(&mut f).await;
    assert!(
        rerun.is_empty(),
        "re-run recorded something twice: {rerun:?}"
    );
    assert!(
        proposal_exists(&f, &minted).await,
        "the pass deletes nothing, ever"
    );
}

// ---------------------------------------------------------------------------
// The fired mark is an index
// ---------------------------------------------------------------------------

/// Test 10. The mark is bookkeeping, so it cannot move the state in either
/// direction: marking an unquorate proposal does not fabricate history, and
/// the absence of a mark does not hide a quorate one from the fold.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_fired_mark_moves_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let unquorate = propose(&mut f, "unquorate", "identified", "scoped").await;
    forge_fired_mark(&mut f, &unquorate).await;

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "1 < n = 2 is not a consensus event"
    );
    assert!(derived.settled.is_empty());

    // The other direction: quorum with nobody having marked anything.
    let bob = TestSigner::generate();
    sync_vote_from(&mut f, &bob, &unquorate).await;
    let derived = f.derived().await;
    assert_eq!(
        derived.state, "scoped",
        "an unmarked but quorate edge is history the moment its votes exist"
    );
    assert_eq!(derived.settled[0].voters.len(), 2);
}

// ---------------------------------------------------------------------------
// Atom fields belong to the proposer
// ---------------------------------------------------------------------------

/// A foreign `to_state` appended to an honest proposal: hydration is
/// last-timestamp-wins across authors, so the engine used to read the
/// attacker's value while still attributing the proposal to its author.
#[tokio::test(flavor = "multi_thread")]
async fn a_foreign_field_override_neither_redirects_nor_destroys_a_proposal() {
    let mut f = seed_satisfied_fixture(None).await;
    let honest = f.mint_one().await;

    let mallory = TestSigner::generate();
    let override_link = mallory.sign(
        Link {
            source: honest.clone(),
            predicate: Some(TO_STATE_PREDICATE.to_string()),
            target: literal("shipped"),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(
            LinkExpression::from(override_link),
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("foreign to_state link");

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the honest edge must still settle: {outcomes:?}"
    );
    assert_eq!(
        outcomes[0].to_state, "scoped",
        "only the proposer's value counts"
    );
    assert!(proposal_exists(&f, &honest).await);
}

// ---------------------------------------------------------------------------
// Quorum, accept, and the read-set
// ---------------------------------------------------------------------------

/// Test 11. The only test that walks accept → pass → fold end to end with a
/// real second key. Another agent's proposal syncs in carrying their vote;
/// this replica's own agent co-signs it through `accept_flow_proposal`, which
/// is the second distinct voter, and the edge settles.
#[tokio::test(flavor = "multi_thread")]
async fn n2_second_signer_accept_settles_and_replays() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "Bob's own vote alone is 1 < n = 2"
    );
    assert_eq!(f.derived().await.state, "identified");

    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("accept must land this replica's vote and sweep");
    assert_eq!(fired.len(), 1, "n = 2 met must settle: {fired:?}");
    assert!(fired[0].voters.contains(&bob.did));
    assert!(fired[0].voters.contains(&acting_did(&f)));

    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");
    assert_eq!(derived.settled.len(), 1);
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "the fold re-counts the votes"
    );
    assert_eq!(
        f.links_by_predicate(&proposal)
            .await
            .get(ACCEPTED_BY_PREDICATE)
            .map(Vec::len),
        Some(1),
        "one accept, one vote link"
    );

    // Voting again on an edge that has already settled is refused as stale:
    // the proposal leaves `identified` and the instance is in `scoped`.
    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("a settled edge must not accept more votes");
    assert!(format!("{err:#}").contains("stale"), "got {err:#}");
}

/// The read-set is the proof: serialise everything the engine read, fold the
/// JSON back on a machine with no perspective, and reach the same verdict.
/// This is what a minted Synergy token would carry as its backing.
#[tokio::test(flavor = "multi_thread")]
async fn a_serialised_read_set_re_derives_the_same_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    f.mint_one().await;
    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");

    let read_set = f.read_set().await;
    assert!(
        read_set
            .role_grants
            .iter()
            .any(|g| g.eligible && g.did == acting_did(&f) && !g.rows.is_empty()),
        "the role rows the verdict rested on belong in the proof: {read_set:?}"
    );

    let json = serde_json::to_string(&read_set).expect("a read-set serialises");
    let parsed: ReadSet = serde_json::from_str(&json).expect("and deserialises");
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    assert_eq!(
        fold_read_set(&flows[&f.flow_uri], &parsed),
        derived,
        "an off-perspective verifier must reach the same verdict"
    );
}

// ---------------------------------------------------------------------------
// Roles
// ---------------------------------------------------------------------------

/// Test 12. A vote from outside the rule's `fromRole` counts for nothing, and
/// the same vote counts the moment its author enters the role.
#[tokio::test(flavor = "multi_thread")]
async fn a_non_role_member_vote_does_not_count() {
    let mut f = seed_satisfied_fixture(None).await;
    // Eligible = "there is a Task this DID owns". The seeded task has no
    // owner, so nobody is in the role yet.
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    f.mint_one().await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "a proposer outside the role vouches for nothing"
    );
    assert!(consensus_pass(&mut f).await.is_empty());

    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "and inside the role, the same vote settles the edge"
    );
}

/// Test 20. The consequence of resolving roles against the CURRENT graph,
/// pinned so it is visible rather than silent: revoking a voter's role after
/// an edge settled un-settles it. Fixing this needs platform work (as-of
/// queries and tombstoned role rows); until then a minted token's backing
/// records which role rows the verdict rested on.
#[tokio::test(flavor = "multi_thread")]
async fn removing_a_voters_role_after_settle_unsettles_the_edge() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.derived().await.state, "scoped");

    let owner_links: Vec<LinkExpression> = links_of(&f, TASK)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some("ns://owner"))
        .map(LinkExpression::from)
        .collect();
    assert!(!owner_links.is_empty());
    f.perspective
        .remove_links(owner_links, None)
        .await
        .expect("revoke the role");

    assert_eq!(
        f.derived().await.state,
        "identified",
        "roles are re-derived live, so revoking one un-settles the edge it decided"
    );
}

// ---------------------------------------------------------------------------
// Evidence: checked when I sign, never re-checked afterwards
// ---------------------------------------------------------------------------

/// Test 13. The cited content changed between mint and vote, so this replica
/// refuses to co-sign — and writes nothing at all. This is the check that
/// used to run at fire time, where it deleted other people's proposals.
#[tokio::test(flavor = "multi_thread")]
async fn accept_refuses_a_stale_seal_and_writes_no_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let minted = f.mint_one().await;

    // The cited task is edited: same ID, different content, different seal.
    f.link(
        TASK,
        "ns://title",
        &literal("Onboard someone else"),
        LinkStatus::Local,
    )
    .await;

    let err = accept_flow_proposal(&mut f.perspective, &minted, &f.ctx)
        .await
        .expect_err("a replica must not co-sign evidence it cannot reproduce");
    assert!(
        format!("{err:#}").contains("cannot reproduce"),
        "the error must name the reason: {err:#}"
    );
    assert!(
        !f.links_by_predicate(&minted)
            .await
            .contains_key(ACCEPTED_BY_PREDICATE),
        "no vote link may reach the graph on a refusal"
    );
    assert!(
        proposal_exists(&f, &minted).await,
        "and the proposal itself is left untouched"
    );
}

/// Test 14. History is never re-run against the live graph. Editing a task a
/// finished transition cited must not roll the flow back, or any edit to old
/// evidence would unwind completed work.
#[tokio::test(flavor = "multi_thread")]
async fn editing_evidence_after_settle_does_not_roll_back() {
    let mut f = seed_satisfied_fixture(None).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.derived().await.state, "scoped");

    f.link(
        TASK,
        "ns://title",
        &literal("Onboard someone else"),
        LinkStatus::Local,
    )
    .await;

    assert_eq!(
        f.derived().await.state,
        "scoped",
        "a settled transition stays settled"
    );
}

/// Test 15. A proposal that arrived before the evidence it cites is not
/// destroyed. Its seal does not recompute here — that is what a partial view
/// looks like — and the pass leaves every one of its links in place, because
/// a replica may only ever refuse its own action.
#[tokio::test(flavor = "multi_thread")]
async fn a_proposal_syncing_ahead_of_its_evidence_is_not_destroyed() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let ahead = propose_unverifiable(&mut f, "ahead-of-evidence", "identified", "scoped").await;
    let before = links_of(&f, &ahead).await.len();

    let outcomes = consensus_pass(&mut f).await;
    assert!(
        outcomes.is_empty(),
        "1 < n = 2 must not settle: {outcomes:?}"
    );
    assert_eq!(
        links_of(&f, &ahead).await.len(),
        before,
        "not one link of the proposal may be removed"
    );
    assert_eq!(f.derived().await.state, "identified");
    assert_eq!(f.cached_state().await, "identified");
}

// ---------------------------------------------------------------------------
// The state is a function of the links present now
// ---------------------------------------------------------------------------

/// Test 16. The ruling, stated as a test: state is a function of the links
/// that exist right now, so deleting a settled vote recomputes the state
/// without it and the flow stands where it stood before that vote. The
/// engine does not defend against this — the cache is even healed backwards
/// to match. Hardening history is the job of the snapshot taken when a token
/// is minted, not of this engine.
#[tokio::test(flavor = "multi_thread")]
async fn deleting_a_settled_vote_recomputes_the_earlier_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let minted = f.mint_one().await;
    let bob = TestSigner::generate();
    sync_vote_from(&mut f, &bob, &minted).await;
    consensus_pass(&mut f).await;
    assert_eq!(f.derived().await.state, "scoped");
    assert_eq!(f.cached_state().await, "scoped");

    let votes: Vec<LinkExpression> = links_of(&f, &minted)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE))
        .map(LinkExpression::from)
        .collect();
    assert_eq!(votes.len(), 1, "exactly Bob's vote is on the graph");
    f.perspective
        .remove_links(votes, None)
        .await
        .expect("delete the settling vote");

    assert_eq!(
        f.derived().await.state,
        "identified",
        "without Bob's vote the edge never reached quorum"
    );
    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "and the cache follows the fold, backwards as readily as forwards"
    );
}

// ---------------------------------------------------------------------------
// Determinism across replicas, and cycles
// ---------------------------------------------------------------------------

/// Test 17. Two replicas holding the same links derive the same state, even
/// when one of them never ran a pass and its cache says otherwise. This is
/// what makes the fold, rather than the link, the thing replicas agree on.
#[tokio::test(flavor = "multi_thread")]
async fn two_replicas_with_the_same_links_derive_the_same_state() {
    let mut a = seed_review_flow().await;
    settle(&mut a, "h1", "review", "changes_requested").await;
    settle(&mut a, "h2", "changes_requested", "review").await;
    settle(&mut a, "h3", "review", "approved").await;
    let derived_a = a.derived().await;
    assert_eq!(derived_a.state, "approved");
    assert_eq!(
        walked(&derived_a),
        vec![
            ("review".into(), "changes_requested".into()),
            ("changes_requested".into(), "review".into()),
            ("review".into(), "approved".into()),
        ],
        "a cycle consumes one atom per visit"
    );

    // Replica B: same definition, same instance, none of the history. It
    // receives A's proposal links in reverse order, exactly as sync would
    // deliver them in whatever order the network chose.
    let mut b = seed_review_flow().await;
    let mut proposal_links = Vec::new();
    for uri in [
        "ad4m://flow/proposal/h1",
        "ad4m://flow/proposal/h2",
        "ad4m://flow/proposal/h3",
    ] {
        proposal_links.extend(links_of(&a, uri).await);
    }
    proposal_links.reverse();
    for link in proposal_links {
        b.perspective
            .add_link_expression(LinkExpression::from(link), LinkStatus::Shared, None)
            .await
            .expect("sync link into replica B");
    }

    let derived_b = b.derived().await;
    assert_eq!(derived_b.state, derived_a.state);
    assert_eq!(walked(&derived_b), walked(&derived_a));
    assert_eq!(
        b.cached_state().await,
        "review",
        "B never ran a pass, so its cache lags — and the fold does not care"
    );
}
