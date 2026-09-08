//! Live-store coverage for the derived state: real perspective, real SDNA,
//! real signatures, no LLM.
//!
//! Every test here pins a specific forgery. Before the fold, each of them
//! moved a flow, deleted an honest proposal, suppressed a mint, or
//! fabricated history with a single link that any neighbourhood member can
//! write — the sibling assertions say which. The one gap the fold cannot
//! close is characterised (and `#[ignore]`d) at the bottom.

use super::flow_classes::advance_flow_instance_state;
use super::flow_consensus::{run_flow_consensus_pass, FireOutcome};
use super::flow_context::load_shacl_flows;
use super::flow_evaluator::recompute_evidence_hash;
use super::flow_evaluator_e2e::{literal, seed_flow, seed_satisfied_fixture, Fixture};
use super::flow_instance::{
    DerivedState, ACCEPTED_BY_PREDICATE, FIRED_MARK, RESOLVED_AS_PREDICATE, TO_STATE_PREDICATE,
};
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
/// setter drops the existing link and writes the new one, so after this the
/// only `currentState` on the instance is the attacker's value.
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

/// A proposal with a non-empty but arbitrary seal — history is never
/// re-run against the live graph (Clock B), so a forged-history test needs
/// a seal that merely exists.
async fn propose_unverified(f: &mut Fixture, id: &str, from: &str, to: &str) -> String {
    f.write_proposal(id, from, to, &[], "not-a-real-seal").await
}

/// The `resolved_as → "fired"` mark, written by whoever is running this
/// test. Any member can write it on any proposal; that is the point.
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

/// Fire one declared edge and return the proposal that did it.
async fn fire(f: &mut Fixture, id: &str, from: &str, to: &str) -> String {
    let uri = propose(f, id, from, to).await;
    let outcomes = consensus_pass(f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "{from} → {to} must fire exactly once: {outcomes:?}"
    );
    uri
}

fn walked(derived: &DerivedState) -> Vec<(String, String)> {
    derived
        .shells
        .iter()
        .map(|s| (s.from_state.clone(), s.to_state.clone()))
        .collect()
}

// ---------------------------------------------------------------------------
// A forged `currentState` changes nothing
// ---------------------------------------------------------------------------

/// The whole point of the slice: a peer writes the one link that used to BE
/// the state, and no engine decision moves. The forged value is terminal,
/// which before the fold suppressed every mint on this instance.
#[tokio::test(flavor = "multi_thread")]
async fn forged_current_state_is_ignored() {
    let mut f = seed_satisfied_fixture(None).await;
    forge_cached_state(&mut f, "scoped").await;

    let derived = f.derived().await;
    assert_eq!(derived.state, "identified", "the fold starts at genesis");
    assert_eq!(
        derived.cache_agrees,
        Some(false),
        "and reports the disagreement rather than healing it"
    );

    // The engine still mints from `identified` and still fires the edge.
    let minted = f.mint_one().await;
    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "got {outcomes:?}");
    assert_eq!(
        (
            outcomes[0].from_state.as_str(),
            outcomes[0].to_state.as_str()
        ),
        ("identified", "scoped")
    );
    assert_eq!(outcomes[0].contributing_proposal_uris, vec![minted]);
    assert_eq!(f.derived().await.state, "scoped");
}

/// One forged link used to hard-delete every honest live proposal: the
/// supersession partition compared `fromState` against the cache, so
/// forging the cache to any other state made the whole frontier "stale".
#[tokio::test(flavor = "multi_thread")]
async fn forged_current_state_does_not_delete_the_frontier() {
    let mut f = seed_satisfied_fixture(None).await;
    // n = 2 so the honest proposal cannot fire and clear itself — what is
    // being pinned is that it SURVIVES the pass.
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let honest = f.mint_one().await;

    forge_cached_state(&mut f, "scoped").await;
    let outcomes = consensus_pass(&mut f).await;

    assert!(outcomes.is_empty(), "1 < n = 2 must not fire: {outcomes:?}");
    assert!(
        proposal_exists(&f, &honest).await,
        "the honest proposal must not be deleted as superseded"
    );
    assert_eq!(
        f.atom_bag().await.frontier("identified").len(),
        1,
        "and must still be countable on the frontier"
    );
}

/// The supersession partition is right about the proposals that really ARE
/// stale, and it decides that on the fold: with the cache forged, the
/// proposal leaving the derived state survives and the one leaving another
/// state is deleted.
#[tokio::test(flavor = "multi_thread")]
async fn superseded_partition_uses_the_fold_not_the_cache() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let live = propose(&mut f, "live", "identified", "scoped").await;
    let stale = propose_unverified(&mut f, "stale", "scoped", "identified").await;
    forge_cached_state(&mut f, "scoped").await;

    consensus_pass(&mut f).await;

    assert!(
        proposal_exists(&f, &live).await,
        "a proposal leaving the DERIVED state is live, whatever the cache says"
    );
    assert!(
        !proposal_exists(&f, &stale).await,
        "a proposal leaving another state is superseded and deleted"
    );
}

/// A forged cache must not steer what the engine proposes: not into
/// silence (a terminal state has no reachable next state) and not onto an
/// edge the flow never reached.
#[tokio::test(flavor = "multi_thread")]
async fn forged_current_state_does_not_suppress_or_misdirect_mints() {
    let mut f = seed_review_flow().await;
    forge_cached_state(&mut f, "changes_requested").await;

    let minted = f.run_pass(&[], None).await;
    assert!(!minted.is_empty(), "mints must not be suppressed");

    let bag = f.atom_bag().await;
    for uri in &minted {
        let atom = bag
            .atoms
            .iter()
            .find(|a| &a.uri == uri)
            .unwrap_or_else(|| panic!("minted proposal {uri} must be an atom"));
        assert_eq!(
            atom.from_state, "review",
            "proposals must leave the derived state, not the forged one"
        );
    }
    assert!(
        !bag.atoms.iter().any(|a| a.to_state == "review"),
        "nothing may be proposed along the forged state's outgoing edge"
    );
}

/// The happy path: after an honest fire the cache mirrors the fold, so the
/// tamper alarm is quiet when nothing is wrong.
#[tokio::test(flavor = "multi_thread")]
async fn fold_matches_the_cache_after_an_honest_fire() {
    let mut f = seed_satisfied_fixture(None).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;

    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");
    assert_eq!(derived.cache_agrees, Some(true));
    assert_eq!(
        walked(&derived),
        vec![("identified".into(), "scoped".into())]
    );
}

// ---------------------------------------------------------------------------
// A forged `resolved_as` mark is not history
// ---------------------------------------------------------------------------

/// The mark decides which proposals the fold CONSIDERS, and nothing else:
/// a proposal that never reached quorum is not history just because
/// somebody labelled it fired.
#[tokio::test(flavor = "multi_thread")]
async fn forged_resolved_as_does_not_make_history() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let unquorate = propose(&mut f, "unquorate", "identified", "scoped").await;

    forge_fired_mark(&mut f, &unquorate).await;

    let derived = f.derived().await;
    assert_eq!(derived.state, "identified", "1 < n = 2 is not a shell");
    assert!(derived.shells.is_empty());
}

/// The other half of the same rule: a mark on a proposal that DOES satisfy
/// every check advances the fold exactly as the engine's own fire would.
/// The mark is an index, so it cannot be the thing that grants authority —
/// and it cannot be the thing that withholds it either.
///
/// Not the same as harmless, and the name says so: the proposal is
/// quorate, so a peer marking it early only skips the live pass's
/// re-check of the cited evidence (Clock A). See [`marked_fired`] for the
/// window that leaves open and why closing it is a later design question.
#[tokio::test(flavor = "multi_thread")]
async fn forged_resolved_as_on_a_quorate_proposal_folds_without_clock_a() {
    let mut f = seed_satisfied_fixture(None).await;
    let quorate = propose(&mut f, "quorate", "identified", "scoped").await;

    forge_fired_mark(&mut f, &quorate).await;

    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");
    assert_eq!(
        derived.cache_agrees,
        Some(false),
        "the cache still says identified — the fold moved without it"
    );
}

/// Marked but not vouchable: an undeclared edge, and an empty seal. Both
/// are the shapes a client can write directly, and neither may fold.
#[tokio::test(flavor = "multi_thread")]
async fn marked_proposals_still_have_to_pass_every_check() {
    let mut f = seed_satisfied_fixture(None).await;

    let undeclared = propose_unverified(&mut f, "undeclared", "identified", "shipped").await;
    let unsealed = f
        .write_proposal("unsealed", "identified", "scoped", &[], "")
        .await;
    for uri in [&undeclared, &unsealed] {
        forge_fired_mark(&mut f, uri).await;
    }

    assert_eq!(
        f.derived().await.state,
        "identified",
        "neither an undeclared edge nor an empty seal may fold"
    );

    // And the pass leaves both alone: history is never deleted, however
    // little the fold thinks of it.
    consensus_pass(&mut f).await;
    assert!(proposal_exists(&f, &undeclared).await);
    assert!(proposal_exists(&f, &unsealed).await);
}

// ---------------------------------------------------------------------------
// Atom fields belong to the proposer
// ---------------------------------------------------------------------------

/// Mallory appends a later `to_state` to Alice's honest proposal. Model
/// hydration is last-timestamp-wins across authors, so the engine used to
/// read Mallory's value while still attributing the proposal to Alice —
/// which either fired the wrong edge or, when the recomputed seal no longer
/// matched, hard-deleted Alice's proposal.
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

    let bag = f.atom_bag().await;
    let atom = bag
        .atoms
        .iter()
        .find(|a| a.uri == honest)
        .expect("Alice's proposal is still an atom");
    assert_eq!(atom.to_state, "scoped", "only the proposer's value counts");

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(
        outcomes.len(),
        1,
        "the honest edge must still fire: {outcomes:?}"
    );
    assert_eq!(outcomes[0].to_state, "scoped");
    assert!(proposal_exists(&f, &honest).await);
}

// ---------------------------------------------------------------------------
// Quorum and history
// ---------------------------------------------------------------------------

/// One mint, one accept by a real second signer, one shell carrying both
/// DIDs. The fold re-counts the votes rather than trusting that a fire
/// happened.
#[tokio::test(flavor = "multi_thread")]
async fn n2_accept_then_fold() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let minted = f.mint_one().await;

    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "one DID must not clear n = 2"
    );

    let bob = TestSigner::generate();
    let vote = bob.sign(
        Link {
            source: minted.clone(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: bob.did.clone(),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(vote), LinkStatus::Shared, None)
        .await
        .expect("second-signer vote");

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "n = 2 met must fire: {outcomes:?}");

    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");
    assert_eq!(derived.shells.len(), 1);
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(derived.shells[0].eligible_voters, expected);
}

/// Three fires, replayed in the order they happened — including the return
/// to `review`, which must not re-consume the atom that left it the first
/// time.
#[tokio::test(flavor = "multi_thread")]
async fn shells_replay_in_order_through_a_cycle() {
    let mut f = seed_review_flow().await;
    fire(&mut f, "h1", "review", "changes_requested").await;
    fire(&mut f, "h2", "changes_requested", "review").await;
    let third = fire(&mut f, "h3", "review", "changes_requested").await;

    let derived = f.derived().await;
    assert_eq!(derived.state, "changes_requested");
    assert_eq!(
        walked(&derived),
        vec![
            ("review".into(), "changes_requested".into()),
            ("changes_requested".into(), "review".into()),
            ("review".into(), "changes_requested".into()),
        ]
    );
    assert_eq!(
        derived.shells[2].atoms[0].uri, third,
        "the revisit must consume the NEW atom, not replay the first one"
    );

    // And forging the cache back to the start still changes nothing.
    forge_cached_state(&mut f, "review").await;
    assert_eq!(f.derived().await.state, "changes_requested");
}

/// History quorum is re-checked against the role rule, not assumed from the
/// fact that a fire once happened: a marked atom whose only voter is
/// outside `fromRole` is not a shell, and becomes one exactly when the
/// voter enters the role.
#[tokio::test(flavor = "multi_thread")]
async fn from_role_gates_history_atoms() {
    let mut f = seed_satisfied_fixture(None).await;
    // Eligible = "there is a Task owned by this DID". The seeded task has
    // no owner yet, so nobody is in the role.
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    let quorate = propose(&mut f, "role", "identified", "scoped").await;
    forge_fired_mark(&mut f, &quorate).await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "a proposer outside the role does not vouch for anything"
    );

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
        "and inside the role, the same atom is a shell"
    );
}

/// Two replicas holding the same atoms derive the same state, even when one
/// of them never saw the fire and its cache still says otherwise. This is
/// what makes the fold, rather than the link, the thing replicas agree on.
#[tokio::test(flavor = "multi_thread")]
async fn two_replicas_with_the_same_atoms_derive_the_same_state() {
    let mut a = seed_review_flow().await;
    fire(&mut a, "h1", "review", "changes_requested").await;
    fire(&mut a, "h2", "changes_requested", "review").await;
    fire(&mut a, "h3", "review", "approved").await;
    let derived_a = a.derived().await;
    assert_eq!(derived_a.state, "approved");

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
        proposal_links.extend(
            a.perspective
                .get_links(&LinkQuery {
                    source: Some(uri.to_string()),
                    ..Default::default()
                })
                .await
                .expect("get proposal links"),
        );
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
        derived_b.cache_agrees,
        Some(false),
        "B never fired, so its cache lags — and the fold does not care"
    );
}

// ---------------------------------------------------------------------------
// The two clocks
// ---------------------------------------------------------------------------

/// Clock B: history is never re-run against the live graph. Editing a task
/// a finished transition cited must not roll the flow back — otherwise any
/// edit to old evidence would unwind completed work.
#[tokio::test(flavor = "multi_thread")]
async fn editing_evidence_after_a_fire_does_not_roll_the_flow_back() {
    let mut f = seed_satisfied_fixture(None).await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.derived().await.state, "scoped");

    // The cited task changes: its hydrated title is now something else, so
    // the seal the proposal carries no longer matches the live graph.
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
        "a fired transition stays fired"
    );
}

// ---------------------------------------------------------------------------
// The gap the fold cannot close
// ---------------------------------------------------------------------------

/// Threat E, characterised rather than fixed: any member may remove any
/// link, including links they did not author
/// (`perspective_instance::remove_links` matches on
/// `(source, predicate, target, author, timestamp)` with no ownership
/// rule). Deleting a fired atom deletes a step of the history, and a fold
/// cannot defend against the disappearance of its own inputs.
///
/// Ignored because it asserts the CURRENT, unwanted behaviour. Un-ignore it
/// (inverted) when the link language, or a pinning slice, closes this.
#[tokio::test(flavor = "multi_thread")]
#[ignore = "documents an accepted gap: any peer can delete a fired atom and regress the fold"]
async fn deleting_a_fired_atom_regresses_the_fold() {
    let mut f = seed_satisfied_fixture(None).await;
    let fired = f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(f.derived().await.state, "scoped");

    let links = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(fired.clone()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    f.perspective
        .remove_links(links.into_iter().map(LinkExpression::from).collect(), None)
        .await
        .expect("delete the fired atom");

    assert_eq!(
        f.derived().await.state,
        "identified",
        "the fold regresses — nothing in this slice prevents it"
    );
}
