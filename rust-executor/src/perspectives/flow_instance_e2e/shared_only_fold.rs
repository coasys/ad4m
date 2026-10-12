use super::*;
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
            (LinkStatus::Shared, "scoped".to_string(), 1),
        ],
        "(grant status, derived state, edges fired)"
    );
}
