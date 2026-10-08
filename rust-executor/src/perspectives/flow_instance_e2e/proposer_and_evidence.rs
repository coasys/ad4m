use super::*;
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
