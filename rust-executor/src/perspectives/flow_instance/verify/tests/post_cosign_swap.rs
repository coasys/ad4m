//! The post-co-sign swap (#1108, @lal-bot-coasys's blocker): re-signing a
//! field under the voted URI uncounts the votes.

use super::*;
// ---- the post-co-sign swap (#1108, @lal-bot-coasys's blocker) -----------

/// **Lal's attack, end to end.** Alice proposes the final edge naming
/// [`OUTPUT`], Bob co-signs — his vote names only the proposal URI — and
/// Alice then re-signs `outputs_hash`/`output` naming [`ATTACKER`] under
/// that same URI, withholding the originals from the read-set she hands
/// a verifier. Bob's genuine signature sits on the swapped material.
///
/// With a random URI this verified: nothing tied Bob's vote to what
/// Alice's links said. With a content-addressed URI the swapped fields
/// no longer address the voted URI, so the proposal is not an atom, the
/// vote does not count, and no receipt for [`ATTACKER`] can mint or
/// verify.
///
/// Red while `from_links` skips the URI recompute — the swapped read-set
/// then folds to `done` and the receipt for [`ATTACKER`] verifies. Also
/// red for the mutation that drops `outputs_hash` from the URI preimage.
#[test]
fn a_post_co_sign_outputs_swap_cannot_mint_or_verify_a_receipt() {
    use crate::perspectives::flow_instance::test_support::signed_terminal_links_at;
    let flow = two_state_flow();

    // The proposal Bob actually co-signed: committing to OUTPUT.
    let (uri, honest_links) = final_links("p-swap", ALICE, "open", "done", T1);
    let bobs_vote = signed_vote(&uri, BOB, T2);
    let mut control_links = honest_links;
    control_links.push(bobs_vote.clone());
    let control = read_set(
        vec![ProposalLinks {
            uri: uri.clone(),
            links: control_links,
        }],
        Vec::new(),
    );
    let receipt = FlowReceipt::mint(&flow, control, outs(&[OUTPUT]), vec![delivered()])
        .expect("control: the co-signed proposal mints for the outputs it named");
    assert!(
        verify_receipt(&catalogue(vec![two_state_flow()]), &receipt).is_verified(),
        "control: and verifies"
    );

    // The swap: the same URI, Alice's fields re-signed naming ATTACKER,
    // the original outputs links withheld. Bob's vote carried over.
    let mut swapped_links = signed_terminal_links_at(
        &uri,
        ALICE,
        "open",
        "done",
        &seal(),
        &[ATTACKER],
        &hash_of(&[ATTACKER]),
        "p-swap",
        T1,
    );
    swapped_links.push(bobs_vote);
    let swapped = read_set(
        vec![ProposalLinks {
            uri: uri.clone(),
            links: swapped_links,
        }],
        Vec::new(),
    );

    assert!(
        swapped.reverified().atoms().is_empty(),
        "the swapped fields do not address the voted URI, so the proposal \
         is not an atom and Bob's vote counts for nothing"
    );
    let err = FlowReceipt::mint(&flow, swapped.clone(), outs(&[ATTACKER]), vec![delivered()])
        .expect_err("no receipt for the swapped output can mint");
    assert!(
        format!("{err:#}").contains("can still transition out"),
        "the fold must stay in `open` over the swapped read-set, got: {err:#}"
    );
    let mut arrived = receipt;
    arrived.read_set = swapped;
    arrived.outputs = outs(&[ATTACKER]);
    let verdict = verify_receipt(&catalogue(vec![flow]), &arrived);
    assert!(
        !verdict.is_verified(),
        "a hand-built receipt over the swapped read-set must not verify, got: {verdict}"
    );
}

/// The same swap against the **evidence seal**: Bob co-signs Alice's
/// proposal sealed over the evidence he checked, and Alice re-signs
/// `evidence_hashes` under the same URI with a seal for other material,
/// naming the same outputs. Same answer for the same reason: the
/// re-signed fields no longer address the voted URI.
///
/// Red while `from_links` skips the URI recompute, and red for the
/// mutation that drops the seal from the URI preimage.
#[test]
fn a_post_co_sign_seal_swap_cannot_mint_or_verify_a_receipt() {
    use crate::perspectives::flow_instance::test_support::signed_terminal_links_at;
    let flow = two_state_flow();

    let (uri, _) = final_links("p-seal-swap", ALICE, "open", "done", T1);
    // The re-signed material: same outputs, another seal — the preimage
    // a dishonest proposer would carry for it. The receipt's own
    // preimage check passes; the URI is what has to refuse.
    let reframed = deliverables(&[OUTPUT, ATTACKER]);
    let mut swapped_links = signed_terminal_links_at(
        &uri,
        ALICE,
        "open",
        "done",
        &reframed.seal,
        &[OUTPUT],
        &hash_of(&[OUTPUT]),
        "p-seal-swap",
        T1,
    );
    swapped_links.push(signed_vote(&uri, BOB, T2));
    let swapped = read_set(
        vec![ProposalLinks {
            uri,
            links: swapped_links,
        }],
        Vec::new(),
    );

    assert!(
        swapped.reverified().atoms().is_empty(),
        "a re-signed seal does not address the voted URI"
    );
    let err = FlowReceipt::mint(&flow, swapped, outs(&[OUTPUT]), vec![reframed])
        .expect_err("no receipt over the swapped seal can mint");
    assert!(
        format!("{err:#}").contains("can still transition out"),
        "got: {err:#}"
    );
}

/// Twins — same edge, same fields, different **nonces** — are different
/// content addresses and stay separate atoms, and the fold still pools
/// their votes on a non-terminal edge exactly as before (#1108 must not
/// re-break what `quorum_is_counted_across_twin_proposals_on_one_edge`
/// pinned at the fold layer; this pins it through `from_links`).
#[test]
fn twins_with_distinct_nonces_still_pool_their_votes_on_one_edge() {
    let flow = flow_json(
        json!([
            { "name": "open", "value": 0.0 },
            { "name": "doing", "value": 0.5, "consensusRule": { "n": 2 } },
            { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
        ]),
        json!([
            { "action_name": "Start", "from_state": "open", "to_state": "doing", "actions": [] },
            { "action_name": "Finish", "from_state": "doing", "to_state": "done", "actions": [] },
        ]),
    );
    let twin = |nonce: &str, proposer: &str, at: &str| {
        let (uri, links) = crate::perspectives::flow_instance::test_support::signed_proposal(
            nonce, proposer, "open", "doing", "seal-1", at,
        );
        ProposalLinks { uri, links }
    };
    let rs = read_set(
        vec![twin("twin-a", ALICE, T1), twin("twin-b", BOB, T2)],
        Vec::new(),
    );
    let atoms = rs.reverified().atoms();
    assert_eq!(atoms.len(), 2, "two nonces, two atoms");
    assert_ne!(atoms[0].uri, atoms[1].uri, "two nonces, two addresses");
    let derived = fold_read_set(&flow, &rs.reverified()).expect("folds");
    assert_eq!(
        derived.state, "doing",
        "one vote on each twin still reaches `n: 2` on the shared edge"
    );
}
