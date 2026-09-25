use super::*;
use crate::types::DecoratedLinkExpression;
// ---------------------------------------------------------------------------
// The proposal read (#1103): what `load_proposal_links` hands the fold
// ---------------------------------------------------------------------------

/// A comparable, order-free view of a link list.
fn sorted_keys(links: &[DecoratedLinkExpression]) -> Vec<String> {
    let mut keys: Vec<String> = links
        .iter()
        .map(|l| serde_json::to_string(l).expect("a link serialises"))
        .collect();
    keys.sort();
    keys
}

/// One settled proposal of ours, co-signed by Bob, with every shape a peer
/// can add to it: a validly signed foreign `to_state` (Mallory's own words),
/// a `to_state` and a `proposer` link forged in our name and in Bob's, and a
/// vote forged in Carol's name. Returns the fixture and the proposal.
async fn contested_settled_proposal() -> (Fixture, String) {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let me = acting_did(&f);
    let proposal = f.mint_one().await;

    let bob = TestSigner::generate();
    let mallory = TestSigner::generate();
    let foreign_to_state = mallory.sign(
        Link {
            source: proposal.clone(),
            predicate: Some(TO_STATE_PREDICATE.to_string()),
            target: literal("shipped"),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(
            LinkExpression::from(foreign_to_state),
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("Mallory's own to_state");
    // Counted as ours, this second `to_state` would make the atom ambiguous
    // and drop our proposal.
    sync_forged(
        &mut f,
        &me,
        &proposal,
        TO_STATE_PREDICATE,
        &literal("identified"),
    )
    .await;
    // Counted, this would make Bob a second self-claimed proposer.
    sync_forged(
        &mut f,
        &bob.did,
        &proposal,
        super::super::flow_instance::atom::PROPOSER_PREDICATE,
        &bob.did,
    )
    .await;
    sync_forged(
        &mut f,
        "did:key:carol",
        &proposal,
        ACCEPTED_BY_PREDICATE,
        "did:key:carol",
    )
    .await;
    sync_vote_from(&mut f, &bob, &proposal).await;

    let outcomes = consensus_pass(&mut f).await;
    assert_eq!(outcomes.len(), 1, "the honest edge settles: {outcomes:?}");
    (f, proposal)
}

/// The fold reads a proposal's links as the store holds them, and the atom
/// rules (`signed_by`) pick out the proposer's own verified words. This pins
/// what that read must keep: every link whose signature verifies, from every
/// author and on every predicate the proposal carries, with the status that
/// tells this replica's `Local` fired mark apart. Checked against a raw
/// `get_links` read of the same proposal, and through what the fold derives
/// from it: our proposal, our and Bob's votes, and the mark the pass wrote.
#[tokio::test(flavor = "multi_thread")]
async fn the_proposal_read_keeps_every_verified_link_the_atom_rules_read() {
    let (f, proposal) = contested_settled_proposal().await;
    let me = acting_did(&f);

    let read_set = f.read_set().await;
    assert_eq!(read_set.proposals.len(), 1, "{read_set:?}");
    assert_eq!(read_set.proposals[0].uri, proposal);

    let raw = links_of(&f, &proposal).await;
    let verified_raw: Vec<_> = raw
        .iter()
        .filter(|l| l.proof.valid == Some(true))
        .cloned()
        .collect();
    let verified_read: Vec<_> = read_set.proposals[0]
        .links
        .iter()
        .filter(|l| l.proof.valid == Some(true))
        .cloned()
        .collect();
    assert_eq!(
        sorted_keys(&verified_read),
        sorted_keys(&verified_raw),
        "the read keeps every verified link, field for field"
    );
    assert!(
        verified_read
            .iter()
            .any(|l| l.data.predicate.as_deref() == Some(RESOLVED_AS_PREDICATE)
                && l.status == Some(LinkStatus::Local)),
        "the pass's own mark is read with its Local status: {verified_read:?}"
    );

    let atoms = read_set.atoms();
    assert_eq!(atoms.len(), 1, "{atoms:?}");
    assert_eq!(atoms[0].proposer, me);
    assert_eq!(atoms[0].to_state, "scoped", "only our own to_state counts");
    let mut voters: Vec<&str> = atoms[0].votes.iter().map(|v| v.did.as_str()).collect();
    voters.sort();
    let bob_vote = raw
        .iter()
        .find(|l| {
            l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE)
                && l.proof.valid == Some(true)
                && l.data.target != me
        })
        .expect("Bob's vote is on the graph");
    let mut expected = vec![me.as_str(), bob_vote.data.target.as_str()];
    expected.sort();
    assert_eq!(voters, expected, "Carol's forged vote adds no voter");
    assert!(
        read_set.marked_proposals().contains(&proposal),
        "the pass's own mark reads as this replica's mark"
    );
    assert_eq!(f.derived().await.state, "scoped");
}

/// A link whose signature does not verify counts for nobody in the fold, so
/// carrying it in a read-set only hands every receipt reader forged material
/// to re-verify. The read withholds it, the default `model_query` applies
/// since #1113. The raw `get_links` read carried all three forgeries.
#[tokio::test(flavor = "multi_thread")]
async fn a_read_set_does_not_carry_a_link_whose_signature_does_not_verify() {
    let (f, proposal) = contested_settled_proposal().await;

    let raw = links_of(&f, &proposal).await;
    assert_eq!(
        raw.iter()
            .filter(|l| l.proof.valid != Some(true))
            .count(),
        3,
        "precondition: the three forgeries are on the graph: {raw:?}"
    );

    let read_set = f.read_set().await;
    let unverified: Vec<_> = read_set.proposals[0]
        .links
        .iter()
        .filter(|l| l.proof.valid != Some(true))
        .collect();
    assert!(
        unverified.is_empty(),
        "a read-set carries only verified proposal links: {unverified:?}"
    );
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "withholding them changes no verdict"
    );
}
