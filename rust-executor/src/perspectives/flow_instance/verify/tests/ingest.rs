//! The untrusted boundary (#1068): every carried verdict is recomputed,
//! and a broken grant signature collapses the window instead of widening it.

use super::*;
// ---- (d) a broken grant signature collapses the window ------------------

/// A role-gated flow: only a `coasys://Reviewer` may settle `done`.
fn role_gated_flow() -> SHACLFlow {
    flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            {
                "name": "done", "value": 1.0,
                "requires": [{ "className": DELIVERABLE }],
                "consensusRule": {
                    "n": 1,
                    "fromRole": { "className": REVIEWER, "didProperty": "agent" },
                },
            },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    )
}

/// `r0 --agent--> did`, the assignment link that dates a grant.
/// `valid` is honoured cryptographically: a forged link is signed with a
/// key that is not the author's. Plain [`LinkExpression`] on purpose —
/// since #1065 the role-evidence half of a read-set cannot carry a
/// verdict claim at all, so "claims to be valid" is unrepresentable and
/// a forgery has nothing left to assert but its (wrong) signature.
fn grant_link(who: &str, valid: bool) -> LinkExpression {
    signed_link("r0", "agent", did_of(who), "admin", valid, None, T2).into()
}

fn reviewer_evidence(grant: LinkExpression) -> RoleGrantEvidence {
    reviewer_evidence_from(vec![grant])
}

fn reviewer_evidence_from(grant_links: Vec<LinkExpression>) -> RoleGrantEvidence {
    RoleGrantEvidence {
        to_state: "done".into(),
        role_class: REVIEWER.into(),
        did: did_of(ALICE).into(),
        instances: vec![RoleInstanceHistory {
            instance_id: "r0".into(),
            grant_links,
            revocation_links: Vec::new(),
            // Earlier than the assignment link — the widening the
            // suppression rule exists to prevent.
            asserted_instance_timestamp: Some(INSTANCE_CREATED.into()),
        }],
    }
}

/// The fail-open direction #1063 is open on, closed at the ingest.
///
/// Grant links are the one kind nothing downstream signature-checks, so a
/// forged one has to be dropped here. Dropping alone **inverts**: with no
/// grant link left, `RoleGrantEvidence::resolve` falls back to
/// `asserted_instance_timestamp` — the instance's own creation, earlier
/// than any assignment — and the forgery buys a *wider* window than the
/// genuine link it replaced. So dropping also drops the fallback, leaving
/// `resolve` to fail closed and abort the derivation.
///
/// The receipt is minted from honest material and the grant link swapped
/// afterwards, because that is the shape of the threat: the artifact
/// arrives from elsewhere, already carrying what its sender chose.
///
/// Red under either half of the fix:
/// - drop `asserted_instance_timestamp: None` from `reverified_history`
///   (keep `history.asserted_instance_timestamp.clone()`) — the window
///   widens to `INSTANCE_CREATED`, the vote at `T3` becomes eligible and
///   the tampered receipt reports `Verified`;
/// - write the grant filter as a pass-through (skip
///   `compute_proof_valid`) — the forgery survives on its wrong-key
///   signature, and the tampered receipt reports `Verified`. (The older
///   shape of this mutation — inheriting a carried `"valid": true` — is
///   unrepresentable since #1065: the plain type has no verdict field.)
///
/// And red in the third scenario if the collapse is not the *filter's*
/// doing — see the comment there for why a verdict assertion alone cannot
/// tell those apart.
#[test]
fn a_forged_grant_link_collapses_the_eligibility_window_instead_of_widening_it() {
    let flow = role_gated_flow();
    let honest = read_set(
        // Vote at T3, grant at T2: eligible as of its own timestamp.
        vec![proposal("ad4m://p/1", ALICE, "done", T3)],
        vec![reviewer_evidence(grant_link(ALICE, true))],
    );
    let receipt = mint(&flow, honest);
    let reader = catalogue(vec![flow]);
    assert!(
        verify_receipt(&reader, &receipt).is_verified(),
        "precondition: with the genuine assignment link this receipt verifies"
    );

    // What arrives: the same receipt, its assignment link replaced by one
    // signed with somebody else's key.
    let mut tampered = receipt;
    tampered.read_set.role_grants = vec![reviewer_evidence(grant_link(ALICE, false))];

    let verdict = verify_receipt(&reader, &tampered);
    assert!(
        verdict.is_rejected(),
        "a forged assignment link must never buy eligibility, and this is a finding \
         about the MATERIAL rather than about the reader — got: {verdict}"
    );
    let ReceiptVerdict::Unfoldable { reason } = &verdict else {
        panic!(
            "the window must COLLAPSE — an unresolvable candidate aborts the derivation \
             rather than de-quorating one edge — got: {verdict}"
        );
    };
    assert!(
        reason.contains("cannot be placed in time"),
        "the refusal must name the fail-closed grant dating, got: {reason}"
    );

    // The separating case, and the reason the two above are not enough.
    //
    // `Unfoldable`/"cannot be placed in time" is also what a `resolve`
    // that failed closed on an ABSENT field would say — code that
    // collapses the window whenever anything is dropped, or that drops
    // every grant link once one is bad, passes both assertions above
    // while being wrong. The verdict is right there for a reason the test
    // never inspects.
    //
    // So: one fixture carrying both links. The forgery is dropped, the
    // genuine link SURVIVES and still dates the grant at `T2`, and the
    // vote at `T3` is eligible against it — the receipt verifies with the
    // forgery sitting right beside the link that carried it.
    //
    // That is the assertion that separates "the signature filter dropped
    // one link" from "the collapse happens for some other reason": the
    // two differ only here, because only here is there surviving material
    // for `resolve` to date a window from.
    let mut half_forged = tampered;
    half_forged.read_set.role_grants = vec![reviewer_evidence_from(vec![
        grant_link(ALICE, false),
        grant_link(ALICE, true),
    ])];
    let verdict = verify_receipt(&reader, &half_forged);
    assert!(
        verdict.is_verified(),
        "the forgery must be dropped WITHOUT poisoning the genuine link beside \
         it — a filter that collapses the window on any bad link, or a `resolve` \
         failing closed on absence, is red here and green above — got: {verdict}"
    );
}

// ---- the ingest seam itself (#1068) ------------------------------------

/// `proof.valid` is a per-replica read view over a signature. On a value
/// that arrived from elsewhere it is the *sender's* claim, and the ingest
/// replaces it with an answer this replica computed — in **both**
/// directions, which is the half a "drop what claims to be invalid" filter
/// would miss.
///
/// Red if `ReadSet::reverified` clones without calling `verify_signature`,
/// and red in the second assertion if it merely *filters* on the carried
/// verdict instead of recomputing it.
#[test]
fn the_ingest_recomputes_every_carried_verdict_rather_than_inheriting_it() {
    let forged_but_claims_valid = signed_link(
        "ad4m://p/1",
        ACCEPTED_BY_PREDICATE,
        did_of(BOB),
        BOB,
        false,
        Some(true),
        T1,
    );
    let genuine_but_claims_nothing = {
        let mut l = signed_vote("ad4m://p/1", BOB, T1);
        l.proof.valid = None;
        l.proof.invalid = None;
        l
    };

    let ingested = read_set(
        vec![ProposalLinks {
            uri: "ad4m://p/1".into(),
            links: vec![forged_but_claims_valid, genuine_but_claims_nothing],
        }],
        Vec::new(),
    )
    .reverified();

    assert_eq!(
        ingested.proposals[0].links[0].proof.valid,
        Some(false),
        "a forged link's own `\"valid\": true` is the sender's word, not a fact"
    );
    assert_eq!(
        ingested.proposals[0].links[1].proof.valid,
        Some(true),
        "and a genuine link the sender never evaluated is not thereby worthless"
    );
}

/// The vote half of #1068, end to end: a co-signature that claims to be
/// valid and is not must not reach quorum.
///
/// `{ n: 2 }` makes Bob's vote load-bearing — Alice alone cannot settle
/// `done` — so inheriting the forgery is the difference between a verified
/// receipt and a refused one.
///
/// Red with `fold_read_set(flow, &receipt.read_set)` in `verify_receipt`
/// — i.e. folding the carried value rather than the re-verified one: Bob's
/// forged `"valid": true` is inherited, quorum is reached, and the
/// tampered receipt verifies.
#[test]
fn a_forged_co_signature_that_claims_to_be_valid_does_not_reach_quorum() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "consensusRule": { "n": 2 }, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    );
    let proposal_uri = final_links("ad4m://p/1", ALICE, "open", "done", T1).0;
    let with_bobs_vote = |vote: DecoratedLinkExpression| {
        let (uri, mut links) = final_links("ad4m://p/1", ALICE, "open", "done", T1);
        links.push(vote);
        read_set(vec![ProposalLinks { uri, links }], Vec::new())
    };

    let receipt = mint(&flow, with_bobs_vote(signed_vote(&proposal_uri, BOB, T2)));
    let reader = catalogue(vec![flow]);
    assert!(
        verify_receipt(&reader, &receipt).is_verified(),
        "precondition: two genuine signatures settle a `{{n: 2}}` edge"
    );

    let mut tampered = receipt;
    tampered.read_set = with_bobs_vote(signed_link(
        &proposal_uri,
        ACCEPTED_BY_PREDICATE,
        did_of(BOB),
        BOB,
        false,
        Some(true),
        T2,
    ));

    // And the other side of the same coin: a minter handed that material
    // cannot produce the receipt in the first place. See
    // `mint_refuses_material_a_verifier_would_refuse` below.
    assert_eq!(
        verify_receipt(&reader, &tampered),
        ReceiptVerdict::StateMismatch {
            claimed: "done".into(),
            derived: "open".into(),
        },
        "a forged co-signature counts for nobody, so the edge never settles"
    );

    // `derived: "open"` is the right answer for TWO different reasons, and
    // the assertion above cannot tell them apart: the forged vote was
    // dropped and Alice's alone is short of `{n: 2}` — or every vote in a
    // read-set containing a bad signature was dropped, which is also short
    // of two. The honest precondition does not separate them either; it
    // runs on a fixture with no forgery in it at all.
    //
    // So: the forgery, and beside it enough genuine material to settle
    // anyway. Alice and Carol make quorum while Bob's forged link sits in
    // the same proposal. Only the first reading survives this.
    let mut with_a_genuine_third = tampered;
    with_a_genuine_third.read_set.proposals[0]
        .links
        .push(signed_vote(&proposal_uri, CAROL, T2));
    let verdict = verify_receipt(&reader, &with_a_genuine_third);
    assert!(
        verdict.is_verified(),
        "one forged co-signature must not disqualify the genuine votes beside \
         it — the ingest rules on links one at a time, not on read-sets — got: \
         {verdict}"
    );
}

/// **Marvin's constraint, made falsifiable.** Mint and verify must fold
/// the same material; if only verify re-verifies, the two sides fold
/// different inputs by construction. That divergence is invisible on the
/// happy path — both sides agree on honest material — and surfaces only as
/// a receipt that minted cleanly on one replica and fails on another,
/// after the artifact is durable and the minter is gone.
///
/// So the property is stated from the mint side: material a verifier would
/// refuse must not mint. Bob's co-signature claims `"valid": true` and is
/// signed with somebody else's key, `{ n: 2 }` makes it load-bearing, and
/// so the fold stays in `open` — which the flow can still leave, so there
/// is no completion to claim.
///
/// Red with `fold_read_set(flow, &read_set)` in `FlowReceipt::mint`: the
/// forged verdict is inherited, quorum is reached, and `mint` produces a
/// receipt that `a_forged_co_signature_that_claims_to_be_valid_does_not_
/// reach_quorum` shows a verifier rejects.
///
/// The closing positive control is the half that makes the refusal mean
/// anything: a `mint` that refused this *shape* rather than this forgery
/// would satisfy the `expect_err` and be caught only there.
#[test]
fn mint_refuses_material_a_verifier_would_refuse() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "consensusRule": { "n": 2 }, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    );
    let (uri, mut links) = final_links("ad4m://p/1", ALICE, "open", "done", T1);
    links.push(signed_link(
        &uri,
        ACCEPTED_BY_PREDICATE,
        did_of(BOB),
        BOB,
        false,
        Some(true),
        T2,
    ));
    let forged = read_set(vec![ProposalLinks { uri, links }], Vec::new());

    let err = FlowReceipt::mint(&flow, forged, outs(&[OUTPUT]), vec![delivered()])
        .expect_err("a quorum resting on a forged signature is not a quorum");
    assert!(
        format!("{err:#}").contains("can still transition out"),
        "the fold must stay in `open` rather than counting the forgery, got: {err:#}"
    );

    // Without this, the test is satisfied by a `mint` that refuses the
    // *shape* — two links, `{n: 2}`, this flow — rather than the forgery,
    // and it has no positive control of its own to say otherwise. The
    // same material with Bob's link genuinely signed must mint, so what
    // the refusal above turns on is the signature and nothing else.
    let (uri, mut links) = final_links("ad4m://p/1", ALICE, "open", "done", T1);
    links.push(signed_vote(&uri, BOB, T2));
    let honest = read_set(vec![ProposalLinks { uri, links }], Vec::new());
    FlowReceipt::mint(&flow, honest, outs(&[OUTPUT]), vec![delivered()])
        .expect("the same material, honestly signed, must mint");
}
