use super::*;
use crate::perspectives::flow_context::gather_active_flow_contexts;
use crate::perspectives::flow_instance::local_cached_state;

// ---------------------------------------------------------------------------
// The fold counts only Shared consensus material (#812 review)
// ---------------------------------------------------------------------------

/// A vote nobody else can see settles nothing. Bob's proposal syncs in Shared
/// with one vote of the two Scoped needs; Alice adds hers as a Local link,
/// which never leaves her store. Her pass must not fire the edge, and her
/// `currentState` stays where her peers' does. Before, her own pass folded
/// her Local graph in: it fired, and her cache said `scoped`, a state no peer
/// could reach and that the auto-processor would have put into a Shared
/// extraction.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_vote_does_not_settle_the_voters_own_edge() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
    assert!(consensus_pass(&mut f).await.is_empty(), "1 < n");

    let alice = acting_did(&f);
    f.link(&proposal, ACCEPTED_BY_PREDICATE, &alice, LinkStatus::Local)
        .await;

    let fired = consensus_pass(&mut f).await;
    assert!(fired.is_empty(), "a Local vote fired the edge: {fired:?}");
    let own = f.perspective.clone().for_own_bookkeeping(alice);
    assert_eq!(
        local_cached_state(&own, &f.instance_uri)
            .await
            .unwrap()
            .as_deref(),
        Some("identified"),
        "Alice's cache moved on her Local vote"
    );
    assert_eq!(
        f.derived().await.state,
        "identified",
        "a fold on Alice's own view counted her Local vote"
    );

    // Control: the same vote, Shared, settles the edge.
    sync_vote_from(&mut f, &TestSigner::generate(), &proposal).await;
    assert_eq!(consensus_pass(&mut f).await.len(), 1);
    assert_eq!(f.cached_state().await, "scoped");
}

/// The acting calls count votes as the fold does. With Alice's Local vote on
/// Bob's proposal, neither her accept nor her propose on the same edge may
/// take it for the vote she has already cast: each writes her Shared vote,
/// and that vote settles the edge.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_vote_does_not_stand_in_for_the_voters_shared_one() {
    async fn bobs_proposal_with_alices_local_vote() -> (Fixture, String) {
        let mut f = seed_satisfied_fixture(None).await;
        set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
        let seal = seal_for(&f, "scoped").await;
        let bob = TestSigner::generate();
        let proposal =
            sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
        let alice = acting_did(&f);
        f.link(&proposal, ACCEPTED_BY_PREDICATE, &alice, LinkStatus::Local)
            .await;
        (f, proposal)
    }

    let (mut f, proposal) = bobs_proposal_with_alices_local_vote().await;
    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("Alice accepts");
    assert_eq!(fired.len(), 1, "accept: {fired:?}");

    let (mut f, proposal) = bobs_proposal_with_alices_local_vote().await;
    let instance = f.instance_uri.clone();
    let outcome =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("Alice proposes");
    assert_eq!(
        (
            outcome.proposal_uri,
            outcome.recorded_vote,
            outcome.outcomes.len()
        ),
        (proposal, true, 1),
        "propose: (joined, vote written, edges fired)"
    );
}

/// A role you grant yourself in your own Local graph is not a role anyone
/// else can see, so a vote it enables counts for nothing. Same single vote,
/// same rule: with the grant Local the edge holds; with it Shared it settles.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_self_granted_role_does_not_count_toward_a_gated_edge() {
    const OWNER_GATE: &str =
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;
    let mut outcomes = Vec::new();
    for status in [LinkStatus::Local, LinkStatus::Shared] {
        let mut f = seed_satisfied_fixture(None).await;
        set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_GATE).await;
        let me = acting_did(&f);
        f.link(TASK, "ns://owner", &literal(&me), status.clone())
            .await;
        f.mint_one().await;
        let fired = consensus_pass(&mut f).await;
        outcomes.push((status, f.derived().await.state, fired.len()));
    }
    assert_eq!(
        outcomes,
        vec![
            (LinkStatus::Local, "identified".to_string(), 0),
            // Today's rule, not the intended one: a member may grant
            // themselves a role. Who may grant one is #1063 / #1159, which
            // will change this row.
            (LinkStatus::Shared, "scoped".to_string(), 1),
        ],
        "(grant status, derived state, edges fired)"
    );
}

// ---------------------------------------------------------------------------
// Local links written before #1357 (#1420 review)
// ---------------------------------------------------------------------------
//
// Before #1357 a Local link went into the default graph with `status: Local`,
// and it stays there until #1359 moves it. Every agent's view reads the
// default graph, so a graph filter alone does not drop these links.

/// A vote written Local before #1357 settles nothing, and the acting calls do
/// not take it for the voter's own vote.
#[tokio::test(flavor = "multi_thread")]
async fn a_pre_1357_local_vote_counts_for_nothing() {
    async fn bobs_proposal_with_alices_pre_1357_vote() -> (Fixture, String) {
        let mut f = seed_satisfied_fixture(None).await;
        set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
        let seal = seal_for(&f, "scoped").await;
        let bob = TestSigner::generate();
        let proposal =
            sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;
        let alice = acting_did(&f);
        let ctx = f.ctx.clone();
        f.perspective
            .add_pre_1357_local_link(
                Link {
                    source: proposal.clone(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: alice,
                },
                &ctx,
            )
            .await
            .expect("Alice's pre-#1357 vote");
        (f, proposal)
    }
    async fn alices_shared_votes(f: &Fixture, proposal: &str) -> usize {
        let alice = acting_did(f);
        f.perspective
            .get_links(&LinkQuery {
                source: Some(proposal.to_string()),
                predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                ..Default::default()
            })
            .await
            .expect("votes")
            .into_iter()
            .filter(|l| l.data.target == alice && l.status != Some(LinkStatus::Local))
            .count()
    }

    let (mut f, proposal) = bobs_proposal_with_alices_pre_1357_vote().await;
    let fired = consensus_pass(&mut f).await;
    assert!(fired.is_empty(), "a pre-#1357 Local vote fired: {fired:?}");
    assert_eq!(f.derived().await.state, "identified");

    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("Alice accepts");
    assert_eq!(
        (alices_shared_votes(&f, &proposal).await, fired.len()),
        (1, 1),
        "accept: (Alice's shared votes, edges fired)"
    );

    let (mut f, proposal) = bobs_proposal_with_alices_pre_1357_vote().await;
    let instance = f.instance_uri.clone();
    let outcome =
        propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
            .await
            .expect("Alice proposes");
    assert_eq!(
        (
            outcome.proposal_uri,
            outcome.recorded_vote,
            outcome.outcomes.len()
        ),
        (proposal, true, 1),
        "propose: (joined, vote written, edges fired)"
    );
}

/// A co-owner's `currentState` cache written before #1357 is no other
/// agent's cache. Carol has no cache of her own, so her auto-processor
/// derives the state; before, it read Bob's legacy value as hers.
#[tokio::test(flavor = "multi_thread")]
async fn a_pre_1357_cache_is_not_the_readers_own() {
    use ad4m_client::literal::Literal;
    let mut f = seed_satisfied_fixture(None).await;
    let bob = second_agent("bob-legacy-cache@e2e.test");
    let instance = f.instance_uri.clone();
    f.perspective
        .add_pre_1357_local_link(
            Link {
                source: instance,
                predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
                target: Literal::from_string("scoped".to_string()).to_url().unwrap(),
            },
            &bob,
        )
        .await
        .expect("Bob's pre-#1357 cache");
    let carol = crate::agent::did_for_context(&second_agent("carol-legacy-cache@e2e.test"))
        .expect("Carol's DID");
    let subject = f.instances().await[0].subject.clone();

    let own = f.perspective.clone().for_own_bookkeeping(carol.clone());
    assert_eq!(
        local_cached_state(&own, &f.instance_uri).await.unwrap(),
        None,
        "Bob's legacy cache read as Carol's own"
    );
    let tick_view = f.perspective.clone().for_viewer(carol).for_shared_reads();
    let contexts =
        gather_active_flow_contexts(&tick_view, std::slice::from_ref(&subject), None).await;
    assert_eq!(contexts.len(), 1, "{contexts:?}");
    assert_eq!(contexts[0].current_state, "identified");
}

/// A fired mark written before #1357 is no other agent's mark. Alice has a
/// cache, so her pass is not a catch-up, and the edge Bob's proposal settles
/// is new to her. Before, Bob's legacy mark hid it as already recorded.
#[tokio::test(flavor = "multi_thread")]
async fn a_pre_1357_fired_mark_is_not_the_readers_own() {
    let mut f = seed_satisfied_fixture(None).await;
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(
        &mut f,
        &TestSigner::generate(),
        "bob-1",
        "identified",
        "scoped",
        &seal,
    )
    .await;
    let bob = second_agent("bob-legacy-mark@e2e.test");
    f.perspective
        .add_pre_1357_local_link(
            Link {
                source: proposal.clone(),
                predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                target: literal(FIRED_MARK),
            },
            &bob,
        )
        .await
        .expect("Bob's pre-#1357 mark");

    assert!(
        marked(&f).await.is_empty(),
        "Bob's legacy mark read as Alice's"
    );
    let fired = consensus_pass(&mut f).await;
    assert_eq!(fired.len(), 1, "Alice's pass reported no edge: {fired:?}");
    assert_eq!(f.cached_state().await, "scoped");
}

// ---------------------------------------------------------------------------
// The acting calls read guards and outputs on shared graphs (#1420 review)
// ---------------------------------------------------------------------------

/// A `Task` in Alice's own Local graph: evidence no peer can see.
async fn seed_local_task(f: &mut Fixture, uri: &str) {
    let ctx = f.ctx.clone();
    f.perspective
        .create_subject(
            crate::perspectives::perspective_instance::SubjectClassOption {
                class_name: Some("Task".to_string()),
                query: None,
            },
            uri.to_string(),
            Some(serde_json::json!({ "title": "Private draft" })),
            None,
            &ctx,
            Some(crate::perspectives::sparql_store::LOCAL_GRAPH_ALIAS.to_string()),
        )
        .await
        .expect("seed a Local Task");
}

/// A guard met only by a Local item is not met. Alice can't propose on it,
/// and she can't co-sign a proposal whose seal only her Local item reproduces:
/// either would put a proposal on the shared graph that no peer can co-sign.
#[tokio::test(flavor = "multi_thread")]
async fn a_guard_met_only_by_a_local_item_is_not_met() {
    let mut f = crate::perspectives::flow_evaluator_e2e::seed_fixture(None).await;
    seed_local_task(&mut f, TASK).await;
    let instance = f.instance_uri.clone();
    let err = propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect_err("propose on a guard met only by a Local Task");
    assert!(
        format!("{err:#}").contains("not satisfied"),
        "propose: {err:#}"
    );

    // Bob's proposal, sealed over the Local Task as Alice's replica sees it.
    let mut f = crate::perspectives::flow_evaluator_e2e::seed_fixture(None).await;
    seed_local_task(&mut f, TASK).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(
        &mut f,
        &TestSigner::generate(),
        "bob-1",
        "identified",
        "scoped",
        &seal,
    )
    .await;
    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("accept a seal only a Local Task reproduces");
    assert!(
        format!("{err:#}").contains("cannot reproduce"),
        "accept: {err:#}"
    );
}

/// An output in Alice's own Local graph is not an instance any voter can
/// load, so a proposal naming it is refused before it is written.
#[tokio::test(flavor = "multi_thread")]
async fn a_local_output_is_not_an_output() {
    let mut f = seed_satisfied_fixture(None).await;
    seed_local_task(&mut f, "ad4m://task/private").await;
    let instance = f.instance_uri.clone();
    let err = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref("ad4m://task/private")],
        None,
        &f.ctx,
    )
    .await
    .expect_err("propose naming a Local output");
    assert!(
        format!("{err:#}").contains("would refuse"),
        "propose: {err:#}"
    );
}

/// The auto-processor runs as one agent and reads that agent's own cache
/// (#987's cache-first read), on the shared-reads view its tick builds
/// (#1324). Two agents on one perspective with different caches: each gets
/// their own, and neither sees the other's. No proposal exists, so the fold
/// says `identified`, and only a cache read can answer `scoped` or `done`.
/// Before, the shared-reads view read no Local graph at all, so every
/// auto-processor pass re-derived and both got `identified`.
#[tokio::test(flavor = "multi_thread")]
async fn each_agents_auto_processor_reads_its_own_cached_state() {
    let f = seed_flow(
        serde_json::json!({
            "name": "Delivery",
            "namespace": "delivery://",
            "states": [
                { "name": "identified", "value": 0.0 },
                { "name": "scoped", "value": 0.5 },
                { "name": "done", "value": 1.0 }
            ],
            "transitions": [
                { "action_name": "Scope", "from_state": "identified", "to_state": "scoped", "actions": [] },
                { "action_name": "Finish", "from_state": "scoped", "to_state": "done", "actions": [] }
            ],
        }),
        "identified",
    )
    .await;
    let alice = acting_did(&f);
    let bob_ctx = second_agent("bob-own-flow-context@e2e.test");
    let bob = crate::agent::did_for_context(&bob_ctx).unwrap();
    for (did, ctx, state) in [(&alice, &f.ctx, "scoped"), (&bob, &bob_ctx, "done")] {
        let mut own = f.perspective.clone().for_own_bookkeeping(did.clone());
        advance_flow_instance_state(&mut own, &f.instance_uri, state, None, ctx)
            .await
            .expect("write the agent's own cache");
    }
    let subject = f.instances().await[0].subject.clone();

    let mut seen = Vec::new();
    for did in [&alice, &bob] {
        // The view `run_auto_processor_tick` hands the extraction pass.
        let tick_view = f
            .perspective
            .clone()
            .for_viewer(did.clone())
            .for_shared_reads();
        let contexts =
            gather_active_flow_contexts(&tick_view, std::slice::from_ref(&subject), None).await;
        assert_eq!(contexts.len(), 1, "{contexts:?}");
        seen.push(contexts[0].current_state.clone());
    }
    assert_eq!(
        seen,
        vec!["scoped", "done"],
        "(Alice's context, Bob's context)"
    );
}
