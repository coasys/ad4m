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
use super::flow_instance::accept::{accept_flow_proposal, reject_flow_proposal};
use super::flow_instance::atom::{
    ACCEPTED_BY_PREDICATE, FIRED_MARK, RESOLVED_AS_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE,
    TO_STATE_PREDICATE,
};
use super::flow_instance::fold::DerivedState;
use super::flow_instance::pass::{run_flow_consensus_pass, FireOutcome};
use super::flow_instance::propose::propose_flow_transition;
use super::flow_instance::{fold_read_set, FlowInstance, ReadSet};
use crate::agent::signatures::TestSigner;
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus, PerspectiveDiff};

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
    let empty = super::flow_instance::atom::outputs_hash(&[]);
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
    outputs: &[&str],
    committed: Option<&str>,
) -> String {
    use super::flow_instance::atom::{
        EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE, FROM_STATE_PREDICATE,
        OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSER_PREDICATE,
    };
    let uri = format!("ad4m://flow/proposal/{id}");
    let instance_uri = f.instance_uri.clone();
    let mut links = vec![
        (PROPOSER_PREDICATE, signer.did.clone()),
        (FLOW_INSTANCE_PREDICATE, instance_uri),
        (FROM_STATE_PREDICATE, literal(from)),
        (TO_STATE_PREDICATE, literal(to)),
        (EVIDENCE_HASHES_PREDICATE, literal(seal)),
    ];
    links.extend(outputs.iter().map(|id| (OUTPUT_PREDICATE, id.to_string())));
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
// The cache and the marks are this replica's own (#987)
// ---------------------------------------------------------------------------

/// Nothing the pass records ever leaves this replica: the mint's initial
/// cache, the healed cache and the fired marks are all `Local` links, and the
/// cache stays single-valued across writes.
#[tokio::test(flavor = "multi_thread")]
async fn the_cache_and_the_marks_are_local_links() {
    let mut f = seed_satisfied_fixture(None).await;
    let initial = current_state_links(&f).await;
    assert_eq!(initial.len(), 1, "the mint wrote one cache link");
    assert_eq!(initial[0].status, Some(LinkStatus::Local));

    let minted = f.mint_one().await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);

    let healed = current_state_links(&f).await;
    assert_eq!(
        healed.len(),
        1,
        "the old cache link is replaced, not joined"
    );
    assert_eq!(healed[0].status, Some(LinkStatus::Local));
    assert_eq!(f.cached_state().await, "scoped");

    let marks: Vec<_> = links_of(&f, &minted)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some(RESOLVED_AS_PREDICATE))
        .collect();
    assert_eq!(marks.len(), 1, "one settling atom, one mark");
    assert_eq!(marks[0].status, Some(LinkStatus::Local));
}

/// A `Shared` `currentState` from a peer (what the pre-#987 executor wrote)
/// is neither believed nor deleted: hydration withholds it because the class
/// declares `currentState` `local: true`, this replica keeps deriving its own
/// value, and the peer's link stays — the pass deletes nothing shared.
///
/// The withholding is #1028's `local_status_filter`, not anything this module
/// does: the peer's link is the *later* one, so a latest-wins hydration would
/// serve `"scoped"` here. That is what the pre-#1028 executor did, and what
/// `local_cached_state` had to read raw links to work around.
#[tokio::test(flavor = "multi_thread")]
async fn a_peer_written_shared_cache_is_overridden_not_deleted() {
    let mut f = seed_satisfied_fixture(None).await;
    let bob = TestSigner::generate();
    let peer_cache = bob.sign(
        Link {
            source: f.instance_uri.clone(),
            predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
            target: literal("scoped"),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(peer_cache), LinkStatus::Shared, None)
        .await
        .expect("sync a peer's currentState");
    // The peer's link really is on the graph, and really does say something
    // else — so the assertion below is about withholding, not about a link
    // that never arrived.
    assert!(
        current_state_links(&f).await.iter().any(|l| {
            l.status == Some(LinkStatus::Shared)
                && l.author == bob.did
                && l.data.target == literal("scoped")
        }),
        "the peer's Shared currentState is on the graph"
    );
    assert_eq!(
        f.cached_state().await,
        "identified",
        "hydration serves our own Local cache and withholds the peer's Shared one"
    );

    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "our own derivation is written and wins"
    );
    let links = current_state_links(&f).await;
    assert!(
        links
            .iter()
            .any(|l| l.status == Some(LinkStatus::Shared) && l.author == bob.did),
        "the peer's link is left in place: {links:?}"
    );
    assert_eq!(
        links
            .iter()
            .filter(|l| l.status == Some(LinkStatus::Local))
            .count(),
        1,
        "and exactly one Local cache link is ours"
    );
}

/// A peer's mark cannot mute this replica's once-only `FireOutcome`: only a
/// `Local` mark says "I already recorded this", so a `Shared` one — forged
/// or honest — is not ours, and the event still fires here exactly once.
#[tokio::test(flavor = "multi_thread")]
async fn a_peer_written_shared_mark_does_not_mute_our_fire_outcome() {
    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.mint_one().await;
    let bob = TestSigner::generate();
    sync_fired_mark_from(&mut f, &bob, &minted).await;

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the peer's mark is not this replica's: {outcomes:?}"
    );
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "our own mark, once written, is"
    );
}

/// The flow instance every OTHER replica sees: the creator's cache is `Local`, so a
/// synced `FlowInstance` carries no `currentState` at all. It must still load
/// as an instance — `currentState` is optional on the shape — with the empty
/// state meaning "not yet derived here", and the pass then fills it.
#[tokio::test(flavor = "multi_thread")]
async fn an_instance_without_a_cache_still_loads_and_the_pass_fills_it() {
    let mut f = seed_satisfied_fixture(None).await;
    let cache: Vec<LinkExpression> = current_state_links(&f)
        .await
        .into_iter()
        .map(LinkExpression::from)
        .collect();
    assert!(!cache.is_empty());
    f.perspective
        .remove_links(cache, None)
        .await
        .expect("drop the creator's local cache");

    let records = f.instances().await;
    assert_eq!(
        records.len(),
        1,
        "the flow instance is recognised with or without its cache"
    );
    assert_eq!(
        records[0].current_state, "",
        "absent cache = not yet derived"
    );

    consensus_pass(&mut f).await;
    assert_eq!(
        f.cached_state().await,
        "identified",
        "the pass writes the fold's answer"
    );
    assert_eq!(
        current_state_links(&f).await[0].status,
        Some(LinkStatus::Local)
    );
}

/// Deliver links the way the link language delivers them: through
/// `diff_from_link_language`, which is where the sync trigger hangs.
async fn sync_in(f: &Fixture, links: Vec<LinkExpression>) {
    f.perspective
        .diff_from_link_language(PerspectiveDiff::from_additions(links))
        .await
        .expect("diff_from_link_language");
}

/// Poll the cache until it reads `state` or the budget runs out; `true`
/// when it got there. The sync-triggered pass is debounced and runs on a
/// spawned task, so a test cannot await it directly.
async fn cache_reaches(f: &Fixture, state: &str) -> bool {
    for _ in 0..50 {
        if f.cached_state().await == state {
            return true;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
    false
}

/// A peer's vote arriving through sync — not through this replica's own
/// accept or mint — must bring this replica's cache and marks up to date
/// by itself: they are `Local`, so nobody else can. No pass is called here;
/// the one `diff_from_link_language` queues does the work.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_vote_triggers_this_replicas_own_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let minted = f.mint_one().await;
    assert_eq!(f.cached_state().await, "identified", "1 < n = 2 at mint");

    let bob = TestSigner::generate();
    let vote = bob.sign(
        Link {
            source: minted.clone(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: bob.did.clone(),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(vote)]).await;

    assert!(
        cache_reaches(&f, "scoped").await,
        "the synced vote must trigger the pass that heals the cache"
    );
    assert!(
        f.read_set().await.marked_proposals().contains(&minted),
        "and that pass marks the settling proposal"
    );
    assert_eq!(
        current_state_links(&f).await.len(),
        1,
        "single Local cache link, as always"
    );
}

/// A synced chat message queues nothing: the trigger is keyed on the flow
/// vocabulary, so ordinary traffic never re-derives a flow. Pinned by the
/// cache staying put where a pass would have healed it.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_chat_message_does_not_trigger_a_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;
    let bob = TestSigner::generate();
    let chat = bob.sign(
        Link {
            source: "flux://message/1".to_string(),
            predicate: Some("flux://body".to_string()),
            target: literal("hello"),
        }
        .normalize(),
    );
    sync_in(&f, vec![LinkExpression::from(chat)]).await;
    tokio::time::sleep(std::time::Duration::from_millis(800)).await;
    assert_eq!(
        f.cached_state().await,
        "scoped",
        "no pass ran: the wrong cache was not healed"
    );
}

/// The flow instance as sync delivers it to every other replica: no `Local` cache.
async fn drop_local_cache(f: &mut Fixture) {
    let cache: Vec<LinkExpression> = current_state_links(f)
        .await
        .into_iter()
        .map(LinkExpression::from)
        .collect();
    f.perspective
        .remove_links(cache, None)
        .await
        .expect("drop the local cache");
}

/// Copy every link of `proposal_uris` from replica `from` into replica
/// `to`, as sync would (Shared, signatures intact), without going through
/// the sync trigger so the test controls when the pass runs.
async fn replicate_proposals(from: &Fixture, to: &mut Fixture, proposal_uris: &[&str]) {
    for uri in proposal_uris {
        for link in links_of(from, uri).await {
            to.perspective
                .add_link_expression(LinkExpression::from(link), LinkStatus::Shared, None)
                .await
                .expect("replicate a proposal link");
        }
    }
}

/// Catch-up is silent. A replica joining a flow with history finds every
/// settled edge unmarked — marks are per replica — and must not report
/// them all as new. Its first pass over the instance marks them and writes
/// the cache without emitting; the next edge to settle is reported. The
/// creating replica is not a newcomer (it wrote its cache at the mint), so
/// its first settle IS reported — pinned by
/// `the_pass_writes_the_cache_and_the_marks_and_then_has_nothing_to_do`.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomers_first_pass_catches_up_silently_then_reports_normally() {
    let mut a = seed_review_flow().await;
    let h1 = settle(&mut a, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut a, "h2", "changes_requested", "review").await;

    // Replica B: same definition, the flow instance as sync delivers it (no
    // cache), and A's history.
    let mut b = seed_review_flow().await;
    drop_local_cache(&mut b).await;
    replicate_proposals(&a, &mut b, &[&h1, &h2]).await;

    let first = consensus_pass(&mut b).await;
    assert!(first.is_empty(), "catch-up is silent, got {first:?}");
    assert_eq!(
        b.cached_state().await,
        "review",
        "but the cache is written (review → changes_requested → review)"
    );
    let marked = b.read_set().await.marked_proposals();
    assert!(
        marked.contains(&h1) && marked.contains(&h2),
        "and the settled history is marked: {marked:?}"
    );
    assert!(
        consensus_pass(&mut b).await.is_empty(),
        "nothing left to record after catch-up"
    );

    // An edge that settles after catch-up is an event for B.
    let h3 = settle(&mut a, "h3", "review", "approved").await;
    replicate_proposals(&a, &mut b, &[&h3]).await;
    let later = consensus_pass(&mut b).await;
    assert_eq!(
        later.len(),
        1,
        "an edge settling after catch-up fires here once: {later:?}"
    );
    assert_eq!(
        (later[0].from_state.as_str(), later[0].to_state.as_str()),
        ("review", "approved")
    );
    assert_eq!(b.cached_state().await, "approved");
    assert!(consensus_pass(&mut b).await.is_empty(), "and only once");
}

/// A newcomer with nothing to catch up on: the first pass writes the cache
/// (silently, trivially) and the FIRST edge to settle afterwards is
/// reported — catch-up must not eat the first real event.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomer_with_no_history_reports_the_first_settle_after_its_first_pass() {
    let mut f = seed_satisfied_fixture(None).await;
    drop_local_cache(&mut f).await;
    assert!(consensus_pass(&mut f).await.is_empty());
    assert_eq!(f.cached_state().await, "identified");

    f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(f.cached_state().await, "scoped");
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

    // Lock the camelCase wire shape so the TS `FlowFireOutcome` interface
    // can never drift from what `serde_json::to_value(fired)` produces.
    let wire = serde_json::to_value(&fired).expect("serialize fired outcomes");
    let obj = wire[0]
        .as_object()
        .expect("outcome must serialize as object");
    assert_eq!(obj.len(), 5, "unexpected field count on the wire: {obj:?}");
    assert_eq!(wire[0]["instanceUri"], fired[0].instance_uri.as_str());
    assert_eq!(wire[0]["fromState"], fired[0].from_state.as_str());
    assert_eq!(wire[0]["toState"], fired[0].to_state.as_str());
    assert_eq!(wire[0]["voters"].as_array().map(Vec::len), Some(2));
    assert!(wire[0]["contributingProposalUris"]
        .as_array()
        .is_some_and(|a| a.contains(&serde_json::Value::String(proposal.clone()))));

    // Voting again on an edge that has already settled is refused as stale:
    // the proposal leaves `identified` and the instance is in `scoped`.
    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("a settled edge must not accept more votes");
    assert!(format!("{err:#}").contains("stale"), "got {err:#}");
}

/// The read-set travels: serialise everything the engine read, fold the JSON
/// back on a machine with no perspective, and reach the same verdict. This is
/// what a minted Synergy token would carry as its backing — signed links for
/// the proposals and votes, and signed links for the role history too, from
/// which the reader recomputes the windows instead of trusting ours.
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
    let records = f.instances().await;
    assert_eq!(
        read_set.subject, records[0].subject,
        "the run's base expression travels: without it a reader cannot substitute `$flow.base` \
         and would apply a different authority rule than we did"
    );
    let evidence = read_set
        .role_grants
        .iter()
        .find(|g| g.did == acting_did(&f))
        .unwrap_or_else(|| panic!("the voter's role evidence belongs in the proof: {read_set:?}"));
    // The reshape's whole point: what travels is raw material, not a
    // `granted_at` the minter computed. The rule carries
    // `didProperty: "owner"` and the fixture writes the assignment link
    // `TASK --ns://owner--> literal(did)`, so the assignment link itself must
    // travel. `asserted_instance_timestamp` is the *fallback* for instances
    // that carry no assignment link, and accepting it here would let the
    // assertion pass on a read-set where nothing travels at all — which is
    // exactly the hole #1065's first review found: `grant_links` was empty for
    // every `didProperty` role because the store query used the property
    // *name* where the graph holds the RDF *predicate*. No disjunction.
    assert!(
        !evidence.instances.is_empty(),
        "the voter's role query must have matched at least one instance: {evidence:?}"
    );
    assert!(
        evidence.instances.iter().all(|i| !i.grant_links.is_empty()),
        "every role instance must carry the assignment links the reader dates the grant from: \
         {evidence:?}"
    );
    // And the carried links must be real links — author, timestamp and
    // signature material present — not default-filled shells that happen to
    // satisfy the type. `.all()` over an empty iterator is vacuously true, so
    // this assert only means anything because of the one above.
    assert!(
        evidence
            .instances
            .iter()
            .flat_map(|i| i.grant_links.iter().chain(i.revocation_links.iter()))
            .all(|l| !l.author.is_empty()
                && !l.timestamp.is_empty()
                && !l.proof.signature.is_empty()),
        "the links themselves travel, author and signature intact: {evidence:?}"
    );

    let json = serde_json::to_string(&read_set).expect("a read-set serialises");
    let parsed: ReadSet = serde_json::from_str(&json).expect("and deserialises");
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let flow = &flows[&f.flow_uri];
    assert_eq!(
        fold_read_set(flow, &parsed).expect("the carried evidence resolves off-perspective"),
        derived,
        "an off-perspective verifier must reach the same verdict"
    );

    // The reader's own translation input must match the one this replica used
    // — the same role query has to mean the same thing on both sides, or the
    // authority rule diverges silently.
    assert_eq!(
        parsed.as_record(flow),
        FlowInstance::from_record(&records[0], flow).as_record(),
        "the record rebuilt from carried fields must equal the live one"
    );
}

// ---------------------------------------------------------------------------
// Roles
// ---------------------------------------------------------------------------

/// Test 12. A vote from outside the rule's `fromRole` counts for nothing, and
/// a grant written *after* that vote does not retroactively enfranchise it —
/// eligibility is as-of each vote's own timestamp, the same rule the
/// revocation tests pin from the other side. A vote cast once the grant is
/// already in place settles the edge.
///
/// The middle assertion used to read `scoped`, and passed only because of the
/// bug #1065's review found: the grant-link query used the `didProperty`
/// *name* where the graph holds the RDF predicate, so `grant_links` came back
/// empty for every `didProperty` role and `granted_at` fell back to the
/// instance's own (much earlier) timestamp. Under that fallback every grant
/// looked retroactive. The assertion was a mirror of the defect, not a
/// contract.
#[tokio::test(flavor = "multi_thread")]
async fn a_non_role_member_vote_does_not_count() {
    const OWNER_RULE_HERE: &str =
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;

    let mut f = seed_satisfied_fixture(None).await;
    // Eligible = "there is a Task this DID owns". The seeded task has no
    // owner, so nobody is in the role yet.
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE_HERE).await;
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
        "identified",
        "the grant postdates the vote, so it cannot reach back and make it count"
    );

    // Same rule, same single vote — but cast while the grant is already live.
    let mut g = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut g, "delivery://Delivery.scoped", OWNER_RULE_HERE).await;
    g.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&g)),
        LinkStatus::Local,
    )
    .await;
    g.mint_one().await;
    assert_eq!(
        g.derived().await.state,
        "scoped",
        "inside the role at the time of the vote, the same vote settles the edge"
    );
}

/// Test 20. Tombstone revocation: a role revocation is an explicit signed link
/// (`ad4m://flow/role_grant_revoked`), never a deletion. Because eligibility is
/// gated as-of each vote's own timestamp, a tombstone written AFTER a vote
/// cannot un-settle the edge that vote produced. The grant instance stays in the
/// graph, newcomers read the same history, and replicas always converge.
#[tokio::test(flavor = "multi_thread")]
async fn revoking_a_role_after_settlement_does_not_unsettle_the_edge() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    // Grant the role; the vote timestamp will be strictly later (wall-clock).
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "edge settles while voter holds the role"
    );

    // Revoke via tombstone (timestamp > vote timestamp — sequential write).
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // A newcomer deriving from scratch must reach the same state.
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "tombstone revocation does not un-settle history: the vote pre-dates the revocation"
    );
}

/// Test 21. Newcomer convergence: a replica that first derives AFTER a
/// tombstone is written reaches the same settled state as one that derived
/// before. Both are represented by sequential `derived()` calls — the fold
/// always re-derives from scratch so there is no separate newcomer code path.
#[tokio::test(flavor = "multi_thread")]
async fn newcomer_replica_converges_to_same_state_after_revocation() {
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
    let state_before = f.derived().await.state.clone();
    assert_eq!(state_before, "scoped");

    // Tombstone the role after settlement.
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // Re-derive from scratch: this is what a newcomer does.
    assert_eq!(
        f.derived().await.state,
        state_before,
        "newcomer convergence: revocation does not rewrite settled history"
    );
}

/// Test 22. A vote cast AFTER a tombstone revocation does not count. The
/// revocation only gates votes whose `at` timestamp follows the tombstone's.
#[tokio::test(flavor = "multi_thread")]
async fn late_syncing_revocation_stops_counting_votes_that_arrive_after_it() {
    let mut f = seed_satisfied_fixture(None).await;
    // n=2: need two distinct eligible voters to settle.
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":2,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    let bob = TestSigner::generate();

    // Grant Alice's role and write her proposal (1 vote, need 2 to settle).
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    let seal = seal_for(&f, "scoped").await;
    // write_proposal takes a short ID; the full URI is ad4m://flow/proposal/<id>.
    let proposal_id = "revoke-timing";
    let proposal_uri = format!("ad4m://flow/proposal/{proposal_id}");
    f.write_proposal(
        proposal_id,
        "identified",
        "scoped",
        &[TASK.to_string()],
        &seal,
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "1 < n=2, not settled"
    );

    // Revoke Alice's role (tombstone, after her vote — strictly later wall-clock).
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // Add Bob to the role (grant timestamp > revocation) and have Bob vote.
    f.link(TASK, "ns://owner", &literal(&bob.did), LinkStatus::Local)
        .await;
    let bob_vote = bob.sign(
        Link {
            source: proposal_uri.clone(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: bob.did.clone(),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(bob_vote), LinkStatus::Shared, None)
        .await
        .expect("sync Bob's vote");

    // Alice's vote: T_alice < T_revoke → still counts (pre-revocation).
    // Bob's vote: T_bob > T_bob_grant > T_revoke, Bob's grant never revoked → counts.
    // Two eligible votes → n=2 settled.
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "Alice's pre-revocation vote + Bob's post-grant vote reach n=2"
    );
}

/// Link timestamps are millisecond RFC3339 and the as-of gate compares them,
/// so steps whose order matters must not land in the same millisecond.
async fn tick() {
    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
}

const OWNER_RULE: &str = r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;

/// Gate every state of the review flow on "owns the task", so each edge of a
/// multi-hop history is a role-gated vote.
async fn seed_owner_gated_review_flow(rule: &str) -> Fixture {
    let mut f = seed_review_flow().await;
    for state in ["review", "changes_requested", "approved"] {
        set_consensus_rule(&mut f, &format!("review://Review.{state}"), rule).await;
    }
    f
}

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

/// A peer's tombstone revoking `revoked` on `role_instance`, delivered as sync would
/// deliver it: signed by the peer's real key.
async fn sync_revocation_from(
    f: &mut Fixture,
    signer: &TestSigner,
    role_instance: &str,
    revoked: &str,
) {
    let tombstone = signer.sign(
        Link {
            source: role_instance.to_string(),
            predicate: Some(ROLE_GRANT_REVOKED_PREDICATE.to_string()),
            target: literal(revoked),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(tombstone), LinkStatus::Shared, None)
        .await
        .expect("sync a peer's revocation");
}

/// A revocation gates only votes cast after it. The edge settled while the
/// agent held the role stays settled — full DerivedState unchanged, and the
/// pass reports nothing new — and a vote the same agent casts afterwards on a
/// gated edge settles nothing. (Fails on the live-evaluation fold: there the
/// post-revocation vote still counts.)
#[tokio::test(flavor = "multi_thread")]
async fn a_revocation_gates_later_votes_and_leaves_settled_history_alone() {
    let mut f = seed_owner_gated_review_flow(OWNER_RULE).await;
    grant_owner_role(&mut f).await;
    tick().await;
    settle(&mut f, "p1", "review", "changes_requested").await;
    let before = f.derived().await;
    assert_eq!(before.state, "changes_requested");

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    assert_eq!(
        f.derived().await,
        before,
        "a revocation must not un-settle history"
    );
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "nothing new settled"
    );

    tick().await;
    propose(&mut f, "p2", "changes_requested", "review").await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "a vote cast after the revocation settles nothing"
    );
    let after = f.derived().await;
    assert_eq!(
        after.state, "changes_requested",
        "post-revocation vote ignored: {after:?}"
    );
    assert_eq!(walked(&after), walked(&before));
}

/// A replica deriving for the first time after the revocation — every
/// derivation here is from scratch — reaches exactly the pre-revocation
/// state, and so does an off-perspective verifier folding the serialised
/// read-set, which now records the revocation it took into account.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomer_deriving_after_a_revocation_converges_on_the_settled_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    grant_owner_role(&mut f).await;
    tick().await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    let before = f.derived().await;
    assert_eq!(before.state, "scoped");

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    assert_eq!(
        f.derived().await,
        before,
        "derived from scratch after == before"
    );
    let read_set = f.read_set().await;
    let json = serde_json::to_string(&read_set).expect("serialises");
    let parsed: ReadSet = serde_json::from_str(&json).expect("deserialises");
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    assert_eq!(
        fold_read_set(&flows[&f.flow_uri], &parsed).expect("the carried evidence resolves"),
        before
    );
    assert!(
        read_set.role_grants.iter().any(|g| g.did == acting_did(&f)
            && g.instances
                .iter()
                .any(|i| i.revocation_links.iter().any(|l| l.compute_proof_valid()))),
        "the read-set carries the tombstone link the verdict took into account — \
         its signature verifying from the carried material alone (the plain form \
         has no verdict flag to read), unfiltered by authority, so the reader \
         applies that rule itself: {read_set:?}"
    );
}

/// A revocation carries the grant's own authority rule. With the role pinned
/// to instances this agent authored (`where.author`), a peer's tombstone on
/// the instance is not a revocation — the grant stays live for later votes — while
/// this agent's own tombstone ends it.
#[tokio::test(flavor = "multi_thread")]
async fn a_revocation_from_outside_the_grants_authority_is_ignored() {
    let mut f = seed_review_flow().await;
    let me = acting_did(&f);
    let admin_only = format!(
        r#"{{"n":1,"fromRole":{{"className":"ns://Task","didProperty":"owner","where":{{"author":"{me}"}}}}}}"#
    );
    for state in ["review", "changes_requested", "approved"] {
        set_consensus_rule(&mut f, &format!("review://Review.{state}"), &admin_only).await;
    }
    grant_owner_role(&mut f).await;
    tick().await;
    settle(&mut f, "p1", "review", "changes_requested").await;

    tick().await;
    let mallory = TestSigner::generate();
    sync_revocation_from(&mut f, &mallory, TASK, &me).await;
    tick().await;
    settle(&mut f, "p2", "changes_requested", "review").await;
    assert_eq!(
        f.derived().await.state,
        "review",
        "an outsider's tombstone is not a revocation"
    );

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    tick().await;
    propose(&mut f, "p3", "review", "approved").await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "the admin's own tombstone ends the grant"
    );
    assert_eq!(f.derived().await.state, "review");
}

// ---------------------------------------------------------------------------
// Evidence: checked when I sign, never re-checked afterwards
// ---------------------------------------------------------------------------

/// A peer publishes an `acceptedBy` that names us as both voter AND author,
/// carrying a signature that does not verify. The write path's idempotency
/// check must read it the way the fold does — through `signed_by` — or the
/// forgery becomes a lockout: our own accept no-ops because "we already
/// voted", while the fold ignores the unverifiable link, so the edge can
/// never reach `{n: 2}`. Ported from #967, where comparing `l.author` alone
/// was the hole.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_vote_claiming_our_authorship_does_not_suppress_our_own() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    let me = acting_did(&f);

    // The attack: a synced link may claim any author, and the executor keeps
    // the failed verdict rather than dropping it.
    f.perspective
        .add_link_expression(
            LinkExpression {
                author: me.clone(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                data: Link {
                    source: proposal.clone(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: me.clone(),
                },
                proof: crate::types::ExpressionProof {
                    key: format!("{me}#key"),
                    signature: "not-a-signature".to_string(),
                },
                status: Some(LinkStatus::Shared),
            },
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("sync a forged vote claiming our authorship");

    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "the fold does not count the forgery, so Bob's vote is still 1 < n = 2"
    );

    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("our own vote must land despite the forgery");
    assert_eq!(
        fired.len(),
        1,
        "with a real second signature the edge settles: {fired:?}"
    );
    assert!(fired[0].voters.contains(&me));
    assert_eq!(f.derived().await.state, "scoped");

    assert!(
        links_of(&f, &proposal).await.iter().any(|l| {
            l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE)
                && l.data.target == me
                && l.proof.valid == Some(true)
        }),
        "a genuinely signed self-authored vote must reach the graph"
    );
}

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

/// Nico's deletion ruling, pinned on the *write path* rather than on raw
/// links: retracting our own vote through `reject_flow_proposal` moves a
/// settled flow back. The engine has no "already fired, refuse" guard, and
/// must not grow one — a `resolved_as → "fired"` mark is an index any member
/// can write, so guarding on it would both read a forgeable link as authority
/// and contradict the semantics that state follows the links present now.
#[tokio::test(flavor = "multi_thread")]
async fn rejecting_our_own_settled_vote_regresses_the_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("our vote settles the edge at n = 2");
    assert_eq!(fired.len(), 1, "precondition: the edge settled");
    assert_eq!(f.derived().await.state, "scoped");

    reject_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("a fired proposal is not immutable — our own vote stays ours");

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "with our vote gone the edge is 1 < n = 2 again, so the flow stands where it stood"
    );
    assert!(
        derived.settled.is_empty(),
        "nothing settled survives the retraction: {:?}",
        derived.settled
    );
    assert!(
        f.links_by_predicate(&proposal)
            .await
            .get(ACCEPTED_BY_PREDICATE)
            .is_none(),
        "our acceptedBy link is gone; Bob's proposal links are untouched"
    );
}

/// Reject deletes what this DID *signed*, not what merely names it. A peer
/// can publish a link claiming our authorship with an unverifiable proof; it
/// is not our action, so retracting it is not ours to do either — the same
/// rule that stops the forgery suppressing our vote in `accept`.
#[tokio::test(flavor = "multi_thread")]
async fn reject_leaves_a_forged_link_claiming_our_did_alone() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    let me = acting_did(&f);

    f.perspective
        .add_link_expression(
            LinkExpression {
                author: me.clone(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                data: Link {
                    source: proposal.clone(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: me.clone(),
                },
                proof: crate::types::ExpressionProof {
                    key: format!("{me}#key"),
                    signature: "not-a-signature".to_string(),
                },
                status: Some(LinkStatus::Shared),
            },
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("sync a forged vote claiming our authorship");

    let err = reject_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("we signed nothing on this proposal, so there is nothing of ours to retract");
    assert!(
        format!("{err:#}").contains("no link signed by"),
        "got {err:#}"
    );

    assert_eq!(
        f.links_by_predicate(&proposal)
            .await
            .get(ACCEPTED_BY_PREDICATE)
            .map(Vec::len),
        Some(1),
        "the forgery is still on the graph — invisible to the fold, but not ours to delete"
    );
}

// ---------------------------------------------------------------------------
// The manual path: two humans, one button
// ---------------------------------------------------------------------------
//
// The dedup key `(evidence_hash, instance, to_state)` carries no proposer, so
// the second agent to press a button matches the first agent's proposal. These
// pin what that must mean: join it, do not skip it. Before the fix the second
// agent minted nothing, voted on nothing, and was told the empty vec the API
// documents as "queued for other voters", so `{n: 2}` was unreachable for two
// humans pressing one button.

/// Every proposal on the graph targeting `to_state`, live or settled.
async fn proposals_to(f: &Fixture, to_state: &str) -> Vec<String> {
    let mut uris: Vec<String> = f
        .perspective
        .get_links(&LinkQuery {
            predicate: Some(TO_STATE_PREDICATE.to_string()),
            target: Some(literal(to_state)),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| l.data.source)
        .collect();
    uris.sort();
    uris.dedup();
    uris
}

async fn accepted_by_count(f: &Fixture, proposal: &str) -> usize {
    f.links_by_predicate(proposal)
        .await
        .get(ACCEPTED_BY_PREDICATE)
        .map_or(0, Vec::len)
}

/// Two distinct DIDs reach for the propose API on ONE edge, and nobody calls
/// `accept`. Bob proposed on his replica and it synced in carrying his vote;
/// this replica's agent presses the same button. Same graph ⇒ same seal ⇒ the
/// whole dedup key matches, so this is exactly the collision that used to
/// discard the second vote in silence.
#[tokio::test(flavor = "multi_thread")]
async fn two_dids_proposing_one_edge_reach_quorum_without_anybody_accepting() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "Bob's own vote alone is 1 < n = 2"
    );

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("propose must land this replica's vote on the edge");

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "scoped",
        "two distinct DIDs on one edge IS quorum at n = 2"
    );
    assert_eq!(derived.settled.len(), 1);
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "both agents' votes are counted"
    );
    assert_eq!(
        derived.settled[0].atom_uris,
        vec![bobs.clone()],
        "one proposal, co-signed — not a second, unreachable twin"
    );
    assert_eq!(
        proposals_to(&f, "scoped").await,
        vec![bobs.clone()],
        "no duplicate proposal was written"
    );
    assert_eq!(
        accepted_by_count(&f, &bobs).await,
        1,
        "exactly one co-sign link, by this replica"
    );
}

/// The same agent pressing the button twice stays a no-op. This is the other
/// half of the fix: joining an existing proposal must be conditional on not
/// having voted on it, or a re-press would append a redundant `acceptedBy`
/// link to the agent's own proposal. It also must not become a second vote —
/// `atom::valid_votes` dedups per DID, and this pins that from the outside.
#[tokio::test(flavor = "multi_thread")]
async fn a_second_propose_by_the_same_did_writes_nothing_and_is_still_one_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("first propose mints");
    let after_first = proposals_to(&f, "scoped").await;
    assert_eq!(after_first.len(), 1, "one proposal after the first press");

    propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect("second propose is a no-op, not an error");

    assert_eq!(
        proposals_to(&f, "scoped").await,
        after_first,
        "re-pressing must not mint a twin"
    );
    assert_eq!(
        accepted_by_count(&f, &after_first[0]).await,
        0,
        "the proposer's own vote IS the proposal; re-pressing must add no acceptedBy link"
    );
    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "one DID is 1 of 2 however many times they press"
    );
    assert!(derived.settled.is_empty());
}

/// Two terminal branches out of one state: whichever settles first forecloses
/// the other, which is the shape the fold reports as contention.
fn fork_flow() -> serde_json::Value {
    let guard = serde_json::json!([{ "className": "ns://Task", "count": { "min": 1 } }]);
    serde_json::json!({
        "name": "Fork",
        "namespace": "fork://",
        "states": [
            { "name": "start", "value": 0.0, "requires": guard },
            { "name": "left", "value": 1.0, "requires": guard },
            { "name": "right", "value": 1.0, "requires": guard },
        ],
        "transitions": [
            { "action_name": "GoLeft", "from_state": "start", "to_state": "left", "actions": [] },
            { "action_name": "GoRight", "from_state": "start", "to_state": "right", "actions": [] },
        ],
    })
}

/// A contested instance refuses a manual proposal, as the engine pass already
/// refuses to mint into one (#998). Two edges out of `start` carry quorum, so
/// the fold can never settle a third; minting would have handed the caller an
/// empty result indistinguishable from "queued, waiting for other voters".
#[tokio::test(flavor = "multi_thread")]
async fn proposing_into_a_contested_instance_is_refused() {
    let mut f = seed_flow(fork_flow(), "start").await;
    f.seed_task(TASK, "Fork the road").await;
    let instance = f.instance_uri.clone();

    propose(&mut f, "left-1", "start", "left").await;
    propose(&mut f, "right-1", "start", "right").await;
    consensus_pass(&mut f).await;

    let derived = f.derived().await;
    assert!(
        derived.contested.is_some(),
        "fixture must actually be contested, else this test proves nothing: {derived:?}"
    );
    assert_eq!(derived.state, "start", "the walk stopped without choosing");

    let err = propose_flow_transition(&mut f.perspective, &instance, "left", &[], None, &f.ctx)
        .await
        .expect_err("a contested instance must refuse a new proposal");
    assert!(
        format!("{err:#}").contains("contested"),
        "the error must name the reason: {err:#}"
    );
}

/// The return shape. `Vec<FireOutcome>` could not tell "your vote landed,
/// waiting for others" from "you had already voted" from "it fired" — all
/// three were the empty vec or indistinguishable from it — and it discarded
/// the proposal URI a co-signer needs. Every branch is exercised here,
/// including the camelCase wire shape the TS `FlowProposeResult` mirrors.
#[tokio::test(flavor = "multi_thread")]
async fn propose_outcome_distinguishes_fired_queued_and_no_op() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    // 1. Queued: the vote landed, the edge is short of quorum.
    let queued =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("mint");
    assert!(queued.minted && queued.recorded_vote);
    assert!(queued.outcomes.is_empty(), "1 of 2 must not fire");
    assert_eq!(queued.derived_state, "identified");
    assert!(!queued.contested);
    assert!(
        proposals_to(&f, "scoped")
            .await
            .contains(&queued.proposal_uri),
        "the URI names the proposal actually written — a co-signer's handle"
    );

    // 2. No-op: same agent, same button, nothing written.
    let repeat =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("re-press");
    assert!(
        !repeat.minted && !repeat.recorded_vote,
        "a re-press is distinguishable from a queued vote: {repeat:?}"
    );
    assert_eq!(
        repeat.proposal_uri, queued.proposal_uri,
        "a no-op still names the live proposal"
    );

    // 3. The wire shape the TS `FlowProposeResult` mirrors. Locked by field
    //    count as well as by name, so a field added here without a matching
    //    TS field fails in Rust rather than silently at a client.
    //    (The join branch needs a second key; it is the next test.)
    let wire = serde_json::to_value(&queued).expect("serialize");
    let obj = wire.as_object().expect("object");
    assert_eq!(obj.len(), 6, "unexpected field count on the wire: {obj:?}");
    for key in [
        "proposalUri",
        "minted",
        "recordedVote",
        "outcomes",
        "derivedState",
        "contested",
    ] {
        assert!(
            obj.contains_key(key),
            "missing `{key}` on the wire: {obj:?}"
        );
    }
}

/// The join branch's return shape: `minted: false` (we did not write it) with
/// `recordedVote: true` (we voted on it) and the fire in `outcomes`. Nothing
/// in the old `Vec<FireOutcome>` could say the first two.
#[tokio::test(flavor = "multi_thread")]
async fn joining_someone_elses_proposal_reports_minted_false_and_a_recorded_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    consensus_pass(&mut f).await;

    let joined =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("join");
    assert_eq!(joined.proposal_uri, bobs, "we joined Bob's proposal");
    assert!(!joined.minted, "we did not write it");
    assert!(joined.recorded_vote, "we voted on it");
    assert_eq!(joined.outcomes.len(), 1, "quorum met in this call");
    assert_eq!(joined.outcomes[0].to_state, "scoped");
    assert_eq!(joined.derived_state, "scoped");
    assert!(!joined.contested);
}

// ---------------------------------------------------------------------------
// The outputs commitment on the live store (#1104)
// ---------------------------------------------------------------------------

/// Whether this replica's agent has an `acceptedBy` on `proposal`.
async fn we_voted_on(f: &Fixture, proposal: &str) -> bool {
    let me = acting_did(f);
    f.links_by_predicate(proposal)
        .await
        .get(ACCEPTED_BY_PREDICATE)
        .is_some_and(|targets| targets.iter().any(|t| *t == me))
}

/// Required test (b) through the production co-sign path: Bob names the
/// Task as the output but signs a hash over the Task and another node. The
/// seal reproduces, so only the outputs check can refuse, and it does,
/// writing nothing.
///
/// Red without the `check_outputs_commitment` call in
/// `accept_flow_proposal`.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_an_outputs_hash_that_does_not_match_the_named_outputs() {
    use super::flow_instance::atom::{outputs_hash, OutputsRefusal};
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let widened = outputs_hash(&[TASK.to_string(), "ad4m://attacker/node".to_string()]);
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[TASK],
        Some(&widened),
    )
    .await;

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("a mismatched outputs commitment must not be co-signed");
    let expected = OutputsRefusal::HashMismatch {
        named: vec![TASK.to_string()],
        committed: widened,
        recomputed: outputs_hash(&[TASK.to_string()]),
    };
    assert!(
        format!("{err:#}").contains(&expected.to_string()),
        "the refusal must be the hash mismatch, got: {err:#}"
    );
    assert!(
        !we_voted_on(&f, &proposal).await,
        "a refusal writes nothing"
    );
    assert_eq!(f.derived().await.state, "identified");
}

/// Required test (c) through the production co-sign path: the commitment
/// matches, but the named output is not a node on this replica.
///
/// Red if `nodes_in_graph` counts the proposal's own `output` link as
/// evidence the node exists (which is how this test first failed: naming a
/// node made it "present"), or if the existence check is dropped from
/// `check_outputs_commitment`.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_a_named_output_that_is_not_in_the_graph() {
    use super::flow_instance::atom::{outputs_hash, OutputsRefusal};
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    const NOWHERE: &str = "ad4m://deliverable/never-written";
    let named = [TASK, NOWHERE];
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &named,
        Some(&outputs_hash(
            &named.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
        )),
    )
    .await;

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("an output this replica cannot see must not be co-signed");
    let expected = OutputsRefusal::OutputNotInGraph {
        id: NOWHERE.to_string(),
    };
    assert!(
        format!("{err:#}").contains(&expected.to_string()),
        "the refusal must name the missing node, got: {err:#}"
    );
    assert!(
        !we_voted_on(&f, &proposal).await,
        "a refusal writes nothing"
    );

    // Control: the same proposal naming only the Task is co-signed.
    let honest = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-2",
        "identified",
        "scoped",
        &seal,
        &[TASK],
        Some(&outputs_hash(&[TASK.to_string()])),
    )
    .await;
    accept_flow_proposal(&mut f.perspective, &honest, &f.ctx)
        .await
        .expect("an honest commitment to an existing node is co-signed");
    assert_eq!(f.derived().await.state, "scoped");
}

/// A proposal into a terminal state that commits to no outputs is refused:
/// co-signing it would complete a run no receipt can bind to anything.
///
/// Red if a missing commitment is read as the empty set.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_a_terminal_proposal_with_no_outputs_commitment() {
    use super::flow_instance::atom::OutputsRefusal;
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[],
        None,
    )
    .await;

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("an uncommitted terminal proposal must not be co-signed");
    assert!(
        format!("{err:#}").contains(&OutputsRefusal::Uncommitted.to_string()),
        "got: {err:#}"
    );
    assert!(!we_voted_on(&f, &proposal).await);
}

/// The proposer's side. `propose` into a terminal state writes the named
/// outputs and their hash, sorted and deduplicated, and a voter reading the
/// atom back finds a matching commitment. Naming a node that is not in the
/// graph is refused before anything is written.
///
/// Red if `propose` does not pass `outputs` to the writer (the atom reads
/// `outputs_hash: None`), or skips its existence check.
#[tokio::test(flavor = "multi_thread")]
async fn propose_commits_to_the_named_outputs_and_refuses_a_missing_one() {
    use super::flow_instance::atom::{outputs_hash, TransitionAtom};
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    f.seed_task("ad4m://task/2", "Ship it").await;

    let err = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &["ad4m://deliverable/never-written".to_string()],
        None,
        &f.ctx,
    )
    .await
    .expect_err("a proposer must not name an output it cannot see");
    assert!(
        format!("{err:#}").contains("is not in this replica's graph"),
        "got: {err:#}"
    );
    assert!(
        f.read_set().await.proposals.is_empty(),
        "nothing is written on refusal"
    );

    let named = vec![
        "ad4m://task/2".to_string(),
        TASK.to_string(),
        "ad4m://task/2".to_string(),
    ];
    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &named,
        None,
        &f.ctx,
    )
    .await
    .expect("propose");
    assert!(out.minted);
    let links = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(out.proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("links");
    let atom = TransitionAtom::from_links(&instance, &out.proposal_uri, &links).expect("an atom");
    let expected = vec![TASK.to_string(), "ad4m://task/2".to_string()];
    assert_eq!(atom.outputs, expected, "written sorted and deduplicated");
    assert_eq!(atom.outputs_hash, Some(outputs_hash(&expected)));
}

/// A run does not end in a non-terminal state, so `propose` refuses to name
/// outputs there, even ones that exist, and writes nothing. The same call
/// without outputs goes through, so the refusal is the outputs rule and not
/// some other guard on the edge.
///
/// Red if `propose` drops the non-terminal branch: the call then writes a
/// proposal with no commitment and reports `minted: true`.
#[tokio::test(flavor = "multi_thread")]
async fn propose_refuses_outputs_on_a_non_terminal_state() {
    use super::flow_instance::atom::TransitionAtom;
    let mut f = seed_review_flow().await;
    let instance = f.instance_uri.clone();

    let err = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "changes_requested",
        &[TASK.to_string()],
        None,
        &f.ctx,
    )
    .await
    .expect_err("a non-terminal state has no outputs to name");
    assert!(
        format!("{err:#}").contains("is not terminal, so a run does not end there"),
        "the refusal must be the non-terminal outputs rule, got: {err:#}"
    );
    assert!(
        f.read_set().await.proposals.is_empty(),
        "nothing is written on refusal"
    );

    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "changes_requested",
        &[],
        None,
        &f.ctx,
    )
    .await
    .expect("the same edge without outputs is proposable");
    assert!(out.minted);
    let links = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(out.proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("links");
    let atom = TransitionAtom::from_links(&instance, &out.proposal_uri, &links).expect("an atom");
    assert!(atom.outputs.is_empty());
    assert_eq!(
        atom.outputs_hash, None,
        "no commitment on a non-terminal edge"
    );
}

/// An open proposal on this edge that names other outputs is neither joined
/// (that would sign outputs the caller did not name) nor twinned (the final
/// edge would carry two commitments, and no receipt for the run could
/// verify). The caller is told.
///
/// Red if `live_proposal_role` ignores the commitment: the call then joins
/// Bob's proposal and reports `minted: false`.
#[tokio::test(flavor = "multi_thread")]
async fn propose_neither_joins_nor_twins_a_proposal_naming_other_outputs() {
    use super::flow_instance::atom::outputs_hash;
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[TASK],
        Some(&outputs_hash(&[TASK.to_string()])),
    )
    .await;

    let err = propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect_err("different outputs must not be joined or twinned");
    assert!(
        format!("{err:#}").contains(&format!("{bobs} on this edge name different outputs")),
        "got: {err:#}"
    );
    assert_eq!(f.read_set().await.proposals.len(), 1, "no twin was minted");
    assert!(!we_voted_on(&f, &bobs).await, "and Bob's was not signed");

    // Control: naming the same outputs joins Bob's proposal.
    let joined = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[TASK.to_string()],
        None,
        &f.ctx,
    )
    .await
    .expect("join");
    assert_eq!(joined.proposal_uri, bobs);
    assert!(!joined.minted && joined.recorded_vote);
}

// ---------------------------------------------------------------------------
// The other side of the proposer-less key: the engine must NOT reach quorum
// ---------------------------------------------------------------------------
//
// `find_live_proposals`' key carries no proposer, and the two tests above rely
// on that: it is what lets a second human co-sign instead of minting an
// unreachable twin. The cost of that choice is that the key is *shared* with
// `run_engine_proposal_pass`, and this is the test that pins what the engine
// owes in exchange.
//
// Nothing automated co-signs. `accept_flow_proposal` — the only production
// writer of `acceptedBy` — has exactly two callers, `api/perspectives_ws.rs`
// and `mcp/tools/flows.rs`, and both are a request arriving from outside. So
// the engine contributes at most ONE vote to an edge however many replicas run
// its pass, and `consensusRule {n: 2}` means two agents, not two machines.
//
// The failure this guards is silent: make the shared key proposer-aware — a
// plausible "fix" for some future twin-mint bug — and N replicas mint N
// proposals carrying N distinct proposer votes. `{n: 2}` is then satisfied by
// two robots agreeing with themselves, nothing errors, and no other test in
// this file goes red, because every one of them drives the *manual* path.

/// One GUARDED edge, the engine pass run three times: twice as this replica
/// and once as a second DID. Exactly one proposal, exactly one vote, and the
/// `{n: 2}` edge does not move.
#[tokio::test(flavor = "multi_thread")]
async fn the_engine_pass_never_reaches_quorum_by_itself_however_many_dids_run_it() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    // `scoped` carries `requires: [ns://Task count.min 1]` and the fixture
    // seeded the Task, so this edge is guarded AND satisfied — the only shape
    // the engine ever mints on.
    let minted = f.run_pass(&[], None).await;
    assert_eq!(
        minted.len(),
        1,
        "the engine mints the first proposal: {minted:?}"
    );

    let rerun = f.run_pass(&[], None).await;
    assert!(
        rerun.is_empty(),
        "the same replica re-running its pass must find its own proposal: {rerun:?}"
    );

    // A second replica's evaluator over the same graph: same guard, same
    // evidence, same seal, so the whole dedup key matches — and a different
    // acting DID, which is exactly what the key deliberately ignores.
    let bob = second_agent("engine-replica-bob@example.com");
    let bobs_did = crate::agent::did_for_context(&bob).expect("did_for_context(bob)");
    assert_ne!(
        bobs_did,
        acting_did(&f),
        "the fixture must really be two DIDs"
    );
    let bobs = f.run_pass_as(&bob).await;
    assert!(
        bobs.is_empty(),
        "a second DID's engine pass must find the live proposal, not mint its own: {bobs:?}"
    );

    assert_eq!(
        proposals_to(&f, "scoped").await,
        minted,
        "three passes, one proposal"
    );
    assert_eq!(
        accepted_by_count(&f, &minted[0]).await,
        0,
        "nothing automated co-signs — `accept_flow_proposal` is reached only from a client"
    );

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "one engine vote is 1 of 2; `{{n: 2}}` must mean two agents, not two machines"
    );
    assert!(
        derived.settled.is_empty(),
        "no edge settled: {:?}",
        derived.settled
    );
}

/// **The real `get_links` seam, not a stub of it.**
///
/// Every other test that touches role grants asserts on a *derived state*, and
/// the unit suite in `flow_instance/roles.rs` hands `resolve` grant links that
/// it constructed itself — links the production path could not have fetched.
/// So when `didProperty` resolution was broken (a property **name** sent to
/// `get_links`, which wants an RDF **predicate**), a fully green suite said
/// nothing: the seam that was broken was precisely the seam the tests stubbed.
/// Since #1027 that meant every `didProperty` role grant was dated from the
/// instance's own timestamp instead of the assignment link — a wider
/// eligibility window than any rule asked for.
///
/// This test walks the production path and pins the contract at the store
/// boundary itself, so the next spelling drift is a red test rather than a
/// silently widened window:
///
/// 1. the property **name** finds the assignment link through the class shape;
/// 2. the predicate spelling finds the same link (a hand-written SDNA may use
///    either, and a role rule must not gate differently depending on which);
/// 3. a name the class does not declare is an `Err`, never an empty predicate;
/// 4. the window `resolve` recomputes is dated from the **assignment**, and is
///    strictly later than the fallback it used to silently take.
///
/// Fails on `8bb33678d~1` at assertion 1: `grant_links` comes back empty.
#[tokio::test(flavor = "multi_thread")]
async fn a_did_property_grant_link_travels_through_the_real_store() {
    use super::flow_evaluator::{requires_query_input, RequiresQueryable};
    use super::flow_instance::roles::resolve_role_grants;
    use super::flow_instance::time::parse_link_timestamp;
    use super::shacl_parser::ModelQuery;

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    // The instance's own links are written first; the assignment comes after,
    // so the two datings are distinguishable and the fallback is the earlier.
    tick().await;
    grant_owner_role(&mut f).await;
    let me = acting_did(&f);

    // 1. The store boundary: `owner` is the SDNA property NAME; the graph
    //    holds `ns://owner`. Before the fix this vector was empty.
    let by_name = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("owner"), &me)
        .await
        .expect("role_grant_links by property name");
    assert_eq!(
        by_name.grant_links.len(),
        1,
        "the assignment link must be reachable by the didProperty NAME the SDNA declares, \
         not only by the predicate the graph stores: {:?}",
        by_name.grant_links
    );
    assert_eq!(
        by_name.grant_links[0].data.predicate.as_deref(),
        Some("ns://owner"),
        "and the link found must be the assignment itself"
    );

    // 2. Either spelling, one answer.
    let by_predicate = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("ns://owner"), &me)
        .await
        .expect("role_grant_links by predicate");
    assert_eq!(
        by_predicate.grant_links, by_name.grant_links,
        "name and predicate spellings must resolve to the same links"
    );

    // 3. Unresolvable fails closed rather than degrading to an empty
    //    predicate — which is what "granted since forever" looked like.
    let err = f
        .perspective
        .role_grant_links("ns://Task", TASK, Some("noSuchProperty"), &me)
        .await
        .expect_err("a didProperty the class does not declare must be an Err");
    assert!(
        format!("{err:#}").contains("noSuchProperty"),
        "the error must name the property that could not be resolved: {err:#}"
    );

    // 4. End to end: the evidence that travels in a receipt carries the
    //    assignment, and the window is dated from it.
    let role: ModelQuery =
        serde_json::from_str(r#"{"className":"ns://Task","didProperty":"owner"}"#)
            .expect("role query");
    let record = f.instances().await.remove(0);
    let evidence = resolve_role_grants(
        &f.perspective,
        "delivery://Delivery.scoped",
        &role,
        &record,
        std::slice::from_ref(&me),
    )
    .await
    .expect("resolve_role_grants");

    let instance = evidence[0]
        .instances
        .iter()
        .find(|i| i.instance_id == TASK)
        .expect("the owned task is a matched role instance");
    assert_eq!(
        instance.grant_links.len(),
        1,
        "the receipt must carry the assignment link, not just the instance's word: {:?}",
        instance.grant_links
    );

    let translated = requires_query_input(&role, &record, &me).expect("role query translates");
    let grant = evidence[0].resolve(&translated).expect("resolve");
    let window = grant
        .windows
        .iter()
        .find(|w| w.instance_id == TASK)
        .expect("a window for the owned task");
    assert_eq!(
        window.granted_at, instance.grant_links[0].timestamp,
        "granted_at is the assignment link's own timestamp"
    );

    let fallback = instance
        .asserted_instance_timestamp
        .clone()
        .expect("the instance is datable, so the fallback exists and is the wrong answer");
    assert!(
        parse_link_timestamp(&window.granted_at) > parse_link_timestamp(&fallback),
        "the assignment must date the grant STRICTLY LATER than the instance fallback \
         ({} vs {}) — taking the fallback is what widened every didProperty window",
        window.granted_at,
        fallback
    );
}

// ---------------------------------------------------------------------------
// The manual path: two candidates on one dedup key, foreign one first
// ---------------------------------------------------------------------------

/// Two guard-identical edges into ONE state. The dedup key
/// `(evidence_hash, instance, to_state)` carries no `from_state`, so a
/// proposal on `elsewhere → merged` shares the whole key of a call proposing
/// `here → merged`. What orders the two in the store is their URIs, which say
/// nothing about which edge either sits on.
///
/// Two separate things are being held apart here, and collapsing either one
/// breaks the test:
///
/// * **`requires` is IDENTICAL on every state.** That is the mechanism: the
///   seal is computed from the target state's guard, so guard-identical edges
///   produce the same `evidence_hash` and therefore the same dedup key. Give
///   the states different guards and the two proposals stop colliding, and
///   the test stops covering anything.
/// * **`value` is DISTINCT on every state.** `value` does not enter the seal
///   — it is only the state ordering. But genesis is `states[0]`
///   (`flow_spawn::initial_state_of`) and the parser's sort by `value` is
///   *stable*, so equal values leave the tie to graph link-discovery order,
///   which `shacl_parser` itself documents as arbitrary. `here` and
///   `elsewhere` both at `0.0` therefore made the folded genesis undefined
///   rather than `here`, and CI folded it to `elsewhere`.
///
/// Note that `seed_flow`'s `initial_state` argument cannot rescue this: it
/// writes the `currentState` **cache**, and the fold never reads the cache —
/// `read_set` takes its genesis from the flow definition alone.
fn merge_flow() -> serde_json::Value {
    let guard = serde_json::json!([{ "className": "ns://Task", "count": { "min": 1 } }]);
    serde_json::json!({
        "name": "Merge",
        "namespace": "merge://",
        "states": [
            // Lowest value, so genesis is `here` — deterministically, which is
            // the whole point of not sharing a value with `elsewhere`.
            { "name": "here", "value": 0.0, "requires": guard },
            { "name": "elsewhere", "value": 0.5, "requires": guard },
            { "name": "merged", "value": 1.0, "requires": guard },
        ],
        "transitions": [
            { "action_name": "FromHere", "from_state": "here", "to_state": "merged", "actions": [] },
            { "action_name": "FromElsewhere", "from_state": "elsewhere", "to_state": "merged", "actions": [] },
        ],
    })
}

/// A joinable proposal sitting BEHIND a foreign one is still the one co-signed.
///
/// This is the ordering the fix exists for, and the only test that pins it:
/// two live proposals share the call's dedup key, the FOREIGN one is first in
/// scan order, and the joinable one is second. A first-match lookup — or a
/// classification loop replaced by `.first()`, or one that `break`s on the
/// first non-joinable candidate — classifies the foreign proposal, never
/// reaches Bob's, and mints. That mint is invariant 4 broken in the one shape
/// it exists to cover: the vote is split across two atoms on one edge, and the
/// next press mints again.
///
/// Every other test on this path has exactly one candidate, so all of them
/// pass on the broken code.
#[tokio::test(flavor = "multi_thread")]
async fn a_joinable_proposal_behind_a_foreign_one_is_still_the_one_co_signed() {
    let mut f = seed_flow(merge_flow(), "here").await;
    f.seed_task(TASK, "Merge the two branches").await;
    set_consensus_rule(&mut f, "merge://Merge.merged", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    // One seal for `merged`, so both proposals below carry the whole key.
    let seal = seal_for(&f, "merged").await;

    // Scan order is `find_live_proposals`' sort, which is by URI — NOT the
    // write order, and not the store's own iteration order, which is
    // arbitrary. So the ids are what put the foreign proposal first.
    let carol = TestSigner::generate();
    let foreign =
        sync_proposal_from(&mut f, &carol, "foreign-1", "elsewhere", "merged", &seal).await;
    let bob = TestSigner::generate();
    let joinable = sync_proposal_from(&mut f, &bob, "joinable-1", "here", "merged", &seal).await;

    assert!(
        foreign < joinable,
        "this test only exercises the ordering bug while the FOREIGN proposal is scanned \
         first, and scan order is the URI sort; rename the two above until it is — do not \
         drop this assertion, without it the test can pass vacuously"
    );
    assert_eq!(
        f.derived().await.state,
        "here",
        "one vote each at n = 2 settles nothing, so the instance is still in genesis — and \
         genesis must be `here`, because that is the edge the call below is on. If this \
         reads `elsewhere`, `merge_flow`'s state VALUES have been collapsed back together \
         and genesis has gone arbitrary; fix the values, do NOT flip this expectation — \
         with genesis `elsewhere` the joinable proposal sorts FIRST and the ordering bug \
         is no longer exercised at all"
    );

    let out = propose_flow_transition(&mut f.perspective, &instance, "merged", &[], None, &f.ctx)
        .await
        .expect("propose must reach past the foreign candidate");

    assert_eq!(
        out.proposal_uri, joinable,
        "the proposal on OUR edge is the one co-signed, not the one leaving `elsewhere`"
    );
    assert!(
        !out.minted,
        "minting past a joinable proposal splits the vote and mints again on the next \
         press: {out:?}"
    );
    assert!(out.recorded_vote, "our vote landed on Bob's proposal");
    assert_eq!(
        out.outcomes.len(),
        1,
        "two DIDs on one edge IS quorum at n = 2"
    );
    assert_eq!(out.outcomes[0].to_state, "merged");

    let mut both = vec![foreign.clone(), joinable.clone()];
    both.sort();
    assert_eq!(
        proposals_to(&f, "merged").await,
        both,
        "no third proposal was written"
    );
    assert_eq!(
        accepted_by_count(&f, &joinable).await,
        1,
        "exactly one co-sign, on the joinable proposal"
    );
    assert_eq!(
        accepted_by_count(&f, &foreign).await,
        0,
        "and none on the foreign one — signing it would vote on an edge we are not on"
    );

    let derived = f.derived().await;
    assert_eq!(derived.state, "merged", "the edge settled");
    assert_eq!(derived.settled.len(), 1);
    assert_eq!(
        derived.settled[0].atom_uris,
        vec![joinable],
        "settled by the co-signed proposal alone"
    );
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "both DIDs on the `here → merged` edge are counted"
    );
}
