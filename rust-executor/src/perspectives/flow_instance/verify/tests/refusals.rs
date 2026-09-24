//! Every verdict about neither the ingest nor the outputs: the ratchet,
//! changed DNA, an unknown flow, a bad preimage, an empty binding, a
//! planted genesis, a non-terminal state, a contested walk.

use super::*;
// ---- (a) the ratchet ---------------------------------------------------

/// **The whole reason receipts exist.** The links a run settled on can be
/// retracted — deleting a settling vote moves the live flow back, which is
/// this engine's semantics and not a failure mode. A receipt froze those
/// links, so it keeps saying what it always said, and a reader reaches
/// that answer with no access to the graph the links came from.
///
/// The first assertion is the precondition that gives the second its
/// meaning: the same fold, over the *live* material, has already moved on.
///
/// Red with `fold_read_set(flow, &ReadSet { proposals: Vec::new(),
/// ..receipt.read_set.reverified() })` in `verify_receipt` — that is, with
/// any implementation that treats the carried proposals as a pointer to be
/// re-fetched rather than as the proof body itself. The receipt would then
/// fold to `open` and report `StateMismatch`.
#[test]
fn a_receipt_still_verifies_after_the_links_behind_it_are_retracted() {
    let flow = two_state_flow();
    let receipt = mint(&flow, completed());

    let after_retraction = read_set(Vec::new(), Vec::new());
    assert_eq!(
        fold_read_set(&flow, &after_retraction.reverified())
            .expect("an empty read-set folds")
            .state,
        "open",
        "precondition: with the settling proposal retracted the LIVE flow stands \
         where it stood before that vote"
    );

    let verdict = verify_receipt(&catalogue(vec![flow]), &receipt);
    assert!(
        verdict.is_verified(),
        "the receipt froze the links; nothing in verification is re-queried — got: {verdict}"
    );
}

// ---- (b) changed DNA, refused before the evidence step ------------------

/// Editing a flow definition edits the identity of the social organism, so
/// a receipt minted under the old DNA *should* stop verifying — and must
/// say so in its own words rather than as a finding about the material.
///
/// The second half is the ordering contract: the receipt handed over has
/// **both** a changed DNA and a broken seal, and `DnaChanged` still wins.
/// Re-running a guard's seal under rules the quorum never agreed to is not
/// a well-posed question; a verifier that answered it would report a
/// confident wrong answer.
///
/// Red if the DNA comparison moves below the seal check (or below the
/// fold) in `verify_receipt`: the second assertion then reports
/// `SealMismatch`.
#[test]
fn changed_dna_is_reported_in_its_own_words_and_before_any_evidence_step() {
    let with_rule = |n: u32| {
        flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": n }, "requires": [{ "className": DELIVERABLE }] },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    };
    let minted_under = with_rule(1);
    let reader_holds = with_rule(2);
    assert_eq!(
        minted_under.flow_uri(),
        reader_holds.flow_uri(),
        "precondition: same organism URI, different DNA — otherwise this would be \
         FlowUnknown"
    );

    let receipt = mint(&minted_under, completed());
    let reader = catalogue(vec![reader_holds]);

    let verdict = verify_receipt(&reader, &receipt);
    assert!(
        matches!(verdict, ReceiptVerdict::DnaChanged { .. }),
        "a receipt minted under other rules is refused, not folded under the new ones"
    );
    assert_eq!(
        verdict.outcome(),
        VerdictKind::Undecidable,
        "and refused as a statement about THIS replica's rules, not as a finding \
         against the receipt — which is merely old"
    );
    assert!(!verdict.is_rejected());

    // Same receipt, additionally carrying a preimage that does not
    // re-hash. The DNA answer still comes first.
    let mut also_broken = receipt;
    also_broken.evidence_preimage.push(EvidencePreimage {
        seal: "not-the-hash-of-anything".into(),
        class_names: vec![REVIEWER.into()],
        items: Vec::new(),
    });
    let verdict = verify_receipt(&reader, &also_broken);
    assert!(
        matches!(verdict, ReceiptVerdict::DnaChanged { .. }),
        "the DNA check owes its answer before any evidence step — got: {verdict}"
    );
}

// ---- (c) an unknown flow is not a finding about the receipt -------------

/// A reader who has never synced the definition has learned **nothing**
/// about the receipt. Reporting that as a fold failure would let "I do not
/// have the rules" be read as "the quorum did not settle this" — the
/// receipt slandered by the reader's own gap.
///
/// The last assertion is the one that pins that sentence, and it was
/// missing until @lal-bot-coasys pointed out that this test was a mirror:
/// naming the variant proves the variant exists, but the slander happens
/// in the **caller**, and `!is_verified()` was true for `FlowUnknown`
/// exactly as it is for `SealMismatch`. A caller writing
/// `if !verdict.is_verified() { reject }` committed the slander with this
/// test passing — and three tests in this file used that very idiom as
/// their discriminator.
///
/// Red twice over:
/// - if the catalogue miss falls into a generic failure, e.g.
///   `else { return ReceiptVerdict::Unfoldable { reason: … } }` — the
///   `assert_eq` catches it;
/// - if `FlowUnknown` is classified as a finding about the material, e.g.
///   `is_rejected` written as `!self.is_verified()` or `FlowUnknown`
///   moved into the `Rejected` arm of `outcome()` — which is the mutation
///   the doc above describes and the variant name alone could not catch.
#[test]
fn an_unsynced_flow_definition_is_its_own_verdict_not_a_failure() {
    let receipt = mint(&two_state_flow(), completed());

    for (label, reader) in [
        ("an empty catalogue", catalogue(Vec::new())),
        (
            "a catalogue holding only somebody else's flow",
            catalogue(vec![flow_json(
                json!([{ "name": "start", "value": 0.0 }]),
                json!([]),
            )
            .tap_rename("Onboarding")]),
        ),
    ] {
        let verdict = verify_receipt(&reader, &receipt);
        assert_eq!(
            verdict,
            ReceiptVerdict::FlowUnknown {
                flow_uri: "coasys://DeliveryFlow".into()
            },
            "{label}: got {verdict}"
        );
        assert_eq!(
            verdict.outcome(),
            VerdictKind::Undecidable,
            "{label}: an un-synced definition is the reader's gap"
        );
        assert!(
            !verdict.is_verified() && !verdict.is_rejected(),
            "{label}: neither verified NOR rejected — a caller must not be able to \
             reach `reject` through one predicate"
        );
    }
}

// ---- the remaining refusals --------------------------------------------

/// The seal is re-hashed from the preimage **carried in the receipt** —
/// never re-queried against a live graph. A preimage that does not
/// reproduce its seal is not the material the voters sealed, whatever the
/// graph says today.
///
/// Red if the seal check is dropped from `verify_receipt`, which would
/// make the carried preimage decorative.
#[test]
fn a_preimage_that_does_not_rehash_to_its_seal_is_refused() {
    let flow = two_state_flow();
    let mut receipt = mint(&flow, completed());
    receipt.evidence_preimage.push(EvidencePreimage {
        seal: "a-seal-nothing-here-hashes-to".into(),
        class_names: vec![REVIEWER.into()],
        items: Vec::new(),
    });

    assert_eq!(
        verify_receipt(&catalogue(vec![flow]), &receipt),
        ReceiptVerdict::SealMismatch {
            seal: "a-seal-nothing-here-hashes-to".into()
        }
    );
}

/// An emptied binding speaks for nothing. A verifier that accepted it
/// would hand out a `Verified` that no `granted_by` edge could be checked
/// against.
///
/// Red without the `outputs.is_empty()` step in `verify_receipt`: the
/// empty list then reaches the commitment step and reports
/// `OutputsNotCommitted` instead.
#[test]
fn a_receipt_that_binds_to_nothing_is_refused() {
    let flow = two_state_flow();
    let mut receipt = mint(&flow, completed());
    receipt.outputs.clear();

    assert_eq!(
        verify_receipt(&catalogue(vec![flow]), &receipt),
        ReceiptVerdict::NoOutputs
    );
}

/// **The genesis is the flow's, never the minter's.** A read-set carries
/// `genesis` as data and the fold starts walking wherever it points, so
/// left unchecked that one field skips the whole verification: plant the
/// genesis AT the terminal state and carry no proposals at all, and the
/// walk "reaches" `done` having settled nothing — a completion claim
/// with an **empty voter list**, under the correct DNA hash, binding
/// real outputs (r4077689141).
///
/// Red without the genesis check in `fold_read_set`: the fold
/// initialises at `done`, finds nothing to settle, and the receipt
/// reports `Verified` with no voters.
#[test]
fn a_genesis_planted_at_the_terminal_state_is_not_a_completion() {
    let flow = two_state_flow();
    let receipt = mint(&flow, completed());
    let reader = catalogue(vec![flow]);

    let mut forged = receipt;
    forged.read_set = ReadSet {
        instance_uri: INSTANCE.to_string(),
        subject: BASE.to_string(),
        genesis: "done".to_string(),
        proposals: Vec::new(),
        role_grants: Vec::new(),
    };

    let verdict = verify_receipt(&reader, &forged);
    assert!(
        verdict.is_rejected(),
        "a walk that starts at the finish line settled nothing and proves nothing — \
         got: {verdict}"
    );
    let ReceiptVerdict::Unfoldable { reason } = &verdict else {
        panic!(
            "the refusal is the fold's — a planted genesis is not foldable material — \
             got: {verdict}"
        );
    };
    assert!(
        reason.contains("genesis"),
        "the refusal must name the planted genesis, got: {reason}"
    );
}

/// The subtler shape of the same forgery, and the reason "did the walk
/// settle at least one edge?" is not the check: plant the genesis one
/// edge short of terminal and carry ONE genuine settled edge. The walk
/// then settles something — a no-edges-settled backstop waves it through
/// — while every quorum before the planted genesis is skipped.
///
/// Red without the genesis check in `fold_read_set`: the fold starts at
/// `doing`, takes the one carried edge, and reports `Verified` naming
/// only the final edge's voter — Alice's `open → doing` quorum simply
/// never happened.
#[test]
fn a_genesis_planted_mid_flow_cannot_skip_the_quorums_before_it() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "doing", "value": 0.5 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Start", "from_state": "open", "to_state": "doing", "actions": [] },
            { "action_name": "Finish", "from_state": "doing", "to_state": "done", "actions": [] },
        ]),
    );
    let final_edge = || {
        let (uri, links) = final_links("ad4m://p/2", BOB, "doing", "done", T2);
        ProposalLinks { uri, links }
    };
    // The honest run walks both edges…
    let (first_uri, first_links) =
        crate::perspectives::flow_instance::test_support::signed_proposal(
            "ad4m://p/1",
            ALICE,
            "open",
            "doing",
            "seal-1",
            T1,
        );
    let full = ReadSet {
        instance_uri: INSTANCE.to_string(),
        subject: BASE.to_string(),
        genesis: "open".to_string(),
        proposals: vec![
            ProposalLinks {
                uri: first_uri,
                links: first_links,
            },
            final_edge(),
        ],
        role_grants: Vec::new(),
    };
    let receipt = mint(&flow, full);
    let reader = catalogue(vec![flow]);
    assert!(
        verify_receipt(&reader, &receipt).is_verified(),
        "precondition: the full walk verifies"
    );

    // …what arrives claims it STARTED at `doing`, carrying only the
    // final edge.
    let mut forged = receipt;
    forged.read_set = ReadSet {
        genesis: "doing".to_string(),
        proposals: vec![final_edge()],
        ..forged.read_set
    };

    let verdict = verify_receipt(&reader, &forged);
    assert!(
        matches!(&verdict, ReceiptVerdict::Unfoldable { reason } if reason.contains("genesis")),
        "a planted mid-flow genesis skips every quorum before it and must be \
         refused — got: {verdict}"
    );
}

/// A receipt is a completion claim. The reader's own copy of the flow
/// decides what terminal means — add an edge out of `done` and the same
/// carried material no longer describes a completed run.
///
/// Reached only through a hand-built receipt: `mint` derives the state and
/// refuses a non-terminal one, and the DNA hash would otherwise catch the
/// edit first. That is the point — this arm exists for material that did
/// not come from `mint`.
///
/// Red without the `is_terminal_state` check in `verify_receipt`.
#[test]
fn a_state_the_readers_flow_can_leave_is_not_a_completion() {
    let minted_under = two_state_flow();
    let receipt = mint(&minted_under, completed());

    let reopenable = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            { "action_name": "Reopen", "from_state": "done", "to_state": "open", "actions": [] },
        ]),
    );
    // Re-stamp the claim so the DNA check passes and this arm is reached.
    let mut arrived = receipt;
    arrived.flow_dna_hash = flow_dna_hash(&reopenable).expect("hash");

    assert_eq!(
        verify_receipt(&catalogue(vec![reopenable]), &arrived),
        ReceiptVerdict::NotTerminal {
            state: "done".into()
        }
    );
}

/// Two declared edges out of `open` both carry quorum. Nothing that pays
/// out on a completed flow may honour a contested derivation — `mint`
/// refuses to produce one, and a verifier refuses to accept one.
///
/// Red without the `contested` arm in `verify_receipt`: the walk stops in
/// `open`, so it would degrade to `StateMismatch` — a verdict that reads
/// as "not settled yet" for a run that can never settle.
#[test]
fn a_contested_derivation_is_refused_as_contested() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
            { "name": "rejected", "value": 1.0 },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            { "action_name": "Reject", "from_state": "open", "to_state": "rejected", "actions": [] },
        ]),
    );
    let receipt = mint(&flow, completed());

    let mut arrived = receipt;
    arrived.flow_dna_hash = flow_dna_hash(&flow).expect("hash");
    arrived.read_set = read_set(
        vec![
            proposal("ad4m://p/1", ALICE, "done", T1),
            proposal("ad4m://p/2", BOB, "rejected", T2),
        ],
        Vec::new(),
    );

    let verdict = verify_receipt(&catalogue(vec![flow]), &arrived);
    let ReceiptVerdict::Contested {
        from_state,
        candidates,
    } = &verdict
    else {
        panic!("a contested derivation has not completed — got: {verdict}");
    };
    assert_eq!(from_state, "open");
    assert_eq!(candidates.len(), 2, "both settled edges are named");
}
