use super::*;
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
    let h1 = settle(&mut a, "h1", "review", "changes_requested").await;
    let h2 = settle(&mut a, "h2", "changes_requested", "review").await;
    let h3 = settle(&mut a, "h3", "review", "approved").await;
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
    for uri in [&h1, &h2, &h3] {
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
