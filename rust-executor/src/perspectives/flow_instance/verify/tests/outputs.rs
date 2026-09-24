//! #1104: outputs are what the quorum committed to.

use super::*;
// ---- #1104: outputs are what the quorum committed to --------------------

/// **Required test (d), the #1104 attack.** A completed run's signed
/// read-set is public material inside its space. Before #1104 any member
/// could re-mint it naming their own node, and the result verified: the
/// fold, DNA hash, seals and signatures are all genuine, and `outputs`
/// was carried unchecked.
///
/// Three shapes of the same attack, each pinned to the exact verdict:
/// - the honest receipt with its `outputs` swapped for the attacker's node;
/// - a re-mint through `mint` itself naming the attacker's node;
/// - the attacker also syncs in their own later twin proposal on the
///   final edge, committing to their node. The fold counts only the atoms
///   whose votes reached quorum, so the twin commits nothing that counts.
///
/// Red on pre-#1104 code (the swapped receipt verifies), and red with
/// step 7 of `verify_receipt` removed.
#[test]
fn a_re_mint_naming_another_node_is_refused_as_outputs_not_committed() {
    let flow = two_state_flow();
    let honest = mint(&flow, completed());
    let reader = catalogue(vec![two_state_flow()]);
    assert!(
        verify_receipt(&reader, &honest).is_verified(),
        "precondition: the genuine run verifies"
    );

    let mut swapped = honest.clone();
    swapped.outputs = outs(&[ATTACKER]);
    let verdict = verify_receipt(&reader, &swapped);
    assert_eq!(
        verdict,
        ReceiptVerdict::OutputsNotCommitted {
            claimed: refs(&[ATTACKER]),
            claimed_hash: hash_of(&[ATTACKER]),
            committed: hash_of(&[OUTPUT]),
        },
        "got: {verdict}"
    );
    assert!(verdict.is_rejected(), "a finding about the material");

    let err = FlowReceipt::mint(
        &flow,
        honest.read_set.clone(),
        outs(&[ATTACKER]),
        vec![delivered()],
    )
    .expect_err("a re-mint naming another node must not mint");
    assert!(format!("{err:#}").contains("committed to"), "got: {err:#}");

    let mut with_twin = swapped;
    let (twin_uri, twin_links) = committed_links(
        "ad4m://p/attacker",
        "mallory",
        "open",
        "done",
        &[ATTACKER],
        &hash_of(&[ATTACKER]),
        T2,
    );
    with_twin.read_set.proposals.push(ProposalLinks {
        uri: twin_uri,
        links: twin_links,
    });
    assert_eq!(
        verify_receipt(&reader, &with_twin),
        ReceiptVerdict::OutputsNotCommitted {
            claimed: refs(&[ATTACKER]),
            claimed_hash: hash_of(&[ATTACKER]),
            committed: hash_of(&[OUTPUT]),
        },
        "a later twin is not counted, so its commitment binds nothing"
    );
}

/// **Required test: a re-mint with edited content.** The receipt names
/// the right output, but carries it with content other than what the
/// quorum committed to: the output was edited after completion and
/// re-read from the live graph, or the minter made the content up. Both
/// are refused as `OutputsNotCommitted`, with the same ref claimed.
///
/// Red if `verify_receipt` hashes the refs rather than the carried
/// content, or if `outputs_hash` drops the content.
#[test]
fn a_receipt_carrying_edited_output_content_is_refused_as_outputs_not_committed() {
    let flow = two_state_flow();
    let honest = mint(&flow, completed());
    let reader = catalogue(vec![two_state_flow()]);

    let mut edited = out_item(OUTPUT);
    edited.content = json!({ "id": OUTPUT, "title": "edited after completion" }).to_string();
    let mut arrived = honest.clone();
    arrived.outputs = vec![edited.clone()];
    let verdict = verify_receipt(&reader, &arrived);
    assert_eq!(
        verdict,
        ReceiptVerdict::OutputsNotCommitted {
            claimed: refs(&[OUTPUT]),
            claimed_hash: outputs_hash(&[edited.clone()]),
            committed: hash_of(&[OUTPUT]),
        },
        "got: {verdict}"
    );

    let err = FlowReceipt::mint(
        &flow,
        honest.read_set.clone(),
        vec![edited],
        vec![delivered()],
    )
    .expect_err("a mint from edited content must not mint");
    assert!(format!("{err:#}").contains("committed to"), "got: {err:#}");
}

/// **Required test (a).** Outputs no longer depend on `requires`. A
/// terminal state with no guard binds outputs exactly like a guarded one:
/// the proposer names them, the atom commits to them, and the receipt
/// verifies. No preimage is carried; the unguarded seal is over an empty
/// bag.
///
/// Red if a `requires` check comes back into `mint` or
/// `verify_receipt` (the pre-rework `NoOutputs` for an unguarded state).
#[test]
fn a_terminal_state_with_no_requires_still_produces_a_valid_receipt() {
    let unguarded = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0 },
        ]),
        json!([
            { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
        ]),
    );
    let (uri, links) = signed_terminal_proposal(
        "ad4m://p/1",
        ALICE,
        "open",
        "done",
        &evidence_hash(&[], &[]),
        &[OUTPUT],
        &hash_of(&[OUTPUT]),
        T1,
    );
    let rs = read_set(vec![ProposalLinks { uri, links }], Vec::new());
    let receipt = FlowReceipt::mint(&unguarded, rs, outs(&[OUTPUT]), Vec::new()).expect("mints");

    let verdict = verify_receipt(&catalogue(vec![unguarded]), &receipt);
    assert_eq!(
        verdict,
        ReceiptVerdict::Verified {
            terminal_state: "done".into(),
            outputs: refs(&[OUTPUT]),
            voters: vec![did_of(ALICE).to_string()],
        },
        "got: {verdict}"
    );
}

/// **Required test (e).** Quorum belongs to an edge, so the counted votes
/// into `done` can sit on twin atoms. Here Alice's atom commits to d1 and
/// Bob's to d1 and d2. No set of outputs was agreed by the whole quorum,
/// so the receipt is refused whichever set it names. Strict: there is no
/// intersection (d1 alone does not verify).
///
/// Since #1108/#1118 the refusal is the fold's: a terminal edge pools
/// votes per commitment, each commitment here has one voter, `{n: 2}` is
/// met in neither group, and the run derives `open` — a `StateMismatch`
/// before any commitment is read. The `OutputsCommitmentConflict` arm
/// is pinned directly in `receipt::tests` against a hand-built settled
/// edge.
///
/// Red if the fold pools terminal votes across commitments and
/// `final_edge_commitment` then reads only the first counted atom's hash
/// (the receipt naming d1 verifies), or takes the intersection or union.
#[test]
fn twin_final_edge_atoms_with_different_outputs_hashes_are_refused() {
    let n2 = || {
        flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 }, "requires": [{ "className": DELIVERABLE }] },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    };
    let flow = n2();
    const D2: &str = "ad4m://deliverable/d2";
    let honest = read_set(
        vec![
            proposal("ad4m://p/1", ALICE, "done", T1),
            proposal("ad4m://p/2", BOB, "done", T2),
        ],
        Vec::new(),
    );
    let receipt = mint(&flow, honest);
    let reader = catalogue(vec![n2()]);
    assert!(
        verify_receipt(&reader, &receipt).is_verified(),
        "precondition: twins that agree on their outputs verify"
    );

    let mut conflicting = receipt;
    let (rival_uri, rival_links) = committed_links(
        "ad4m://p/2",
        BOB,
        "open",
        "done",
        &[OUTPUT, D2],
        &hash_of(&[OUTPUT, D2]),
        T2,
    );
    conflicting.read_set.proposals[1] = ProposalLinks {
        uri: rival_uri,
        links: rival_links,
    };
    for named in [outs(&[OUTPUT]), outs(&[OUTPUT, D2])] {
        let mut arrived = conflicting.clone();
        arrived.outputs = named.clone();
        assert_eq!(
            verify_receipt(&reader, &arrived),
            ReceiptVerdict::StateMismatch {
                claimed: "done".into(),
                derived: "open".into(),
            },
            "one voter per commitment settles nothing; naming {named:?}"
        );
    }
    let err = FlowReceipt::mint(
        &flow,
        conflicting.read_set,
        outs(&[OUTPUT]),
        vec![delivered()],
    )
    .expect_err("mint refuses what verify refuses");
    assert!(
        format!("{err:#}").contains("can still transition out"),
        "got: {err:#}"
    );
}

/// A proposal counted on the final edge that commits to no outputs binds
/// the run to nothing, whatever the receipt names. An honest voter would
/// have refused to co-sign it (`OutputsRefusal::Uncommitted`); a receipt
/// is checked as if one did not.
///
/// The uncommitted proposal is honest about being uncommitted — its URI
/// addresses the field's absence. Stripping the `outputs_hash` links off
/// a committed proposal stopped being this test's shape with #1108: that
/// no longer un-commits the proposal, it un-atoms it (`UriMismatch`).
///
/// Since #1108/#1118 an uncommitted terminal atom contributes no votes,
/// so the run derives `open` and the verdict is a `StateMismatch` before
/// any commitment is read. The `OutputsUncommitted` arm is pinned
/// directly in `receipt::tests` against a hand-built settled edge.
///
/// Red if the fold counts an uncommitted terminal atom again and
/// `final_edge_commitment` then skips it instead of reporting it.
#[test]
fn a_final_edge_that_committed_to_no_outputs_is_refused() {
    let flow = two_state_flow();
    let mut receipt = mint(&flow, completed());
    let (uri, links) = crate::perspectives::flow_instance::test_support::signed_proposal(
        "p-uncommitted",
        ALICE,
        "open",
        "done",
        &seal(),
        T1,
    );
    receipt.read_set.proposals[0] = ProposalLinks { uri, links };

    assert_eq!(
        verify_receipt(&catalogue(vec![flow]), &receipt),
        ReceiptVerdict::StateMismatch {
            claimed: "done".into(),
            derived: "open".into(),
        }
    );
}

/// The commitment is over the outputs in any order, and over nothing
/// else. A receipt that lists the same outputs in another order still
/// verifies. A subset, a superset, and a list that repeats an output do
/// not: `mint` never writes a repeat, so one is not the committed list.
///
/// Red if `verify_receipt` compares lists instead of hashes (the
/// reordered receipt is refused), or if `outputs_hash` deduplicates.
#[test]
fn the_commitment_is_over_the_outputs_in_any_order_and_nothing_else() {
    let flow = two_state_flow();
    let three = [OUTPUT, "ad4m://deliverable/d2", "ad4m://deliverable/d3"];
    let (uri, links) = committed_links(
        "ad4m://p/1",
        ALICE,
        "open",
        "done",
        &three,
        &hash_of(&three),
        T1,
    );
    let rs = read_set(vec![ProposalLinks { uri, links }], Vec::new());
    let receipt = FlowReceipt::mint(&flow, rs, outs(&three), vec![delivered()]).expect("mints");
    let reader = catalogue(vec![flow]);

    let reordered_ids = ["ad4m://deliverable/d3", OUTPUT, "ad4m://deliverable/d2"];
    let mut reordered = receipt.clone();
    reordered.outputs = outs(&reordered_ids);
    assert_eq!(
        verify_receipt(&reader, &reordered),
        ReceiptVerdict::Verified {
            terminal_state: "done".into(),
            outputs: refs(&reordered_ids),
            voters: vec![did_of(ALICE).to_string()],
        }
    );

    for claimed_ids in [
        &three[..2],
        &[
            OUTPUT,
            "ad4m://deliverable/d2",
            "ad4m://deliverable/d3",
            ATTACKER,
        ][..],
        &[
            OUTPUT,
            "ad4m://deliverable/d2",
            "ad4m://deliverable/d3",
            OUTPUT,
        ][..],
    ] {
        let mut arrived = receipt.clone();
        arrived.outputs = outs(claimed_ids);
        assert_eq!(
            verify_receipt(&reader, &arrived),
            ReceiptVerdict::OutputsNotCommitted {
                claimed_hash: outputs_hash(&arrived.outputs),
                claimed: refs(claimed_ids),
                committed: hash_of(&three),
            },
            "claiming {claimed_ids:?}"
        );
    }
}
