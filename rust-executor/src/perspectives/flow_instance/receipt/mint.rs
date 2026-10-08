//! Minting: deriving a receipt from carried material, and the exact list
//! of things a mint refuses.

use super::{
    final_edge_commitment, flow_dna_hash, is_terminal_state, EvidencePreimage, FlowReceipt,
    OutputsCommitment, MAX_RECEIPT_BYTES,
};
use crate::perspectives::flow_evaluator::EvidenceItem;
use crate::perspectives::flow_instance::atom::{outputs_hash, OutputRef};
use crate::perspectives::flow_instance::{fold_read_set, ReadSet};
use crate::perspectives::shacl_parser::SHACLFlow;

impl FlowReceipt {
    /// Mint a receipt for a run the fold says has completed. **Pure** — no
    /// store, no clock; the caller collects the preimages (the one step that
    /// needs a perspective) and writes the result.
    ///
    /// The terminal state is a *return* of this function, not a parameter:
    /// `mint` folds the read-set itself under `flow`, so no caller can claim a
    /// state the carried material does not reach. `outputs` is a parameter:
    /// each output's `(class, id)` and the content `model_query` returned for
    /// it when the run completed. It is checked against the commitment the
    /// final edge's quorum signed ([`final_edge_commitment`]), so no caller
    /// can name an output, or content, that quorum did not commit to. An
    /// output edited since completion no longer hashes to it, so a minter
    /// needs the content as it stood then. `mint` writes the outputs sorted
    /// by `(class, id)`.
    ///
    /// Refuses, rather than minting something that would fail its own
    /// verification:
    ///
    /// - `outputs` is empty — a receipt with no binding speaks for nothing,
    ///   and the `granted_by` check a verifier runs could never pass;
    /// - a preimage does not re-hash to the seal it claims;
    /// - the fold does not reach a state with no outgoing transitions — a
    ///   receipt is a completion claim, and an intermediate edge is not one;
    /// - the fold is **contested** — two edges out of the same state both
    ///   carry quorum, and anything that pays out on a completed flow must
    ///   refuse a contested derivation;
    /// - a counted atom on the final edge carries no `outputs_hash`, or two
    ///   of them carry different ones;
    /// - `outputs` names one output twice with different content;
    /// - `outputs` does not hash to the final edge's `outputs_hash`;
    /// - the serialised receipt exceeds [`MAX_RECEIPT_BYTES`].
    pub fn mint(
        flow: &SHACLFlow,
        read_set: ReadSet,
        mut outputs: Vec<EvidenceItem>,
        evidence_preimage: Vec<EvidencePreimage>,
    ) -> anyhow::Result<FlowReceipt> {
        outputs.sort_by(|a, b| {
            (&a.class_name, &a.id, &a.content).cmp(&(&b.class_name, &b.id, &b.content))
        });
        outputs.dedup();
        if let Some(pair) = outputs
            .windows(2)
            .find(|w| OutputRef::of(&w[0]) == OutputRef::of(&w[1]))
        {
            anyhow::bail!(
                "FlowReceipt::mint: {}: output `{}` of class `{}` is given twice with different \
                 content; a receipt carries one content per output",
                read_set.instance_uri,
                pair[0].id,
                pair[0].class_name
            );
        }
        if outputs.is_empty() {
            anyhow::bail!(
                "FlowReceipt::mint: {} has no outputs to speak for; a receipt with no binding \
                 could never be honoured",
                read_set.instance_uri
            );
        }
        if let Some(bad) = evidence_preimage.iter().find(|p| !p.rehashes_to_seal()) {
            anyhow::bail!(
                "FlowReceipt::mint: {}: the carried preimage for seal `{}` does not re-hash to \
                 it, so a verifier would reject this receipt; refusing to mint it",
                read_set.instance_uri,
                bad.seal
            );
        }

        // Through the ingest seam, exactly as `verify_receipt` does. Minting
        // on the raw value and verifying on the re-verified one would fold
        // different inputs by construction — see `ReadSet::reverified`.
        let ingested = read_set.reverified();
        let derived = fold_read_set(flow, &ingested)?;
        if let Some(contested) = &derived.contested {
            anyhow::bail!(
                "FlowReceipt::mint: {} is contested in `{}` ({} settled edges out of it), so it \
                 has not completed and nothing may pay out on it",
                read_set.instance_uri,
                contested.from_state,
                contested.candidates.len()
            );
        }
        if !is_terminal_state(flow, &derived.state) {
            anyhow::bail!(
                "FlowReceipt::mint: {} folds to `{}`, which flow `{}` can still transition out \
                 of; a receipt is a completion claim, not a per-edge event",
                read_set.instance_uri,
                derived.state,
                flow.flow_uri()
            );
        }
        match final_edge_commitment(&derived, &ingested) {
            OutputsCommitment::Committed(committed) if committed == outputs_hash(&outputs) => {}
            OutputsCommitment::Committed(committed) => anyhow::bail!(
                "FlowReceipt::mint: {}: outputs {:?} hash to `{}`, but the final edge's quorum \
                 committed to `{committed}`; a receipt may only carry the outputs, and the \
                 content, that quorum agreed to",
                read_set.instance_uri,
                outputs.iter().map(OutputRef::of).collect::<Vec<_>>(),
                outputs_hash(&outputs)
            ),
            other => anyhow::bail!("FlowReceipt::mint: {}: {other}", read_set.instance_uri),
        }

        let receipt = FlowReceipt {
            flow_uri: flow.flow_uri(),
            flow_dna_hash: flow_dna_hash(flow)?,
            terminal_state: derived.state,
            outputs,
            read_set,
            evidence_preimage,
        };
        let size = receipt.body()?.len();
        if size > MAX_RECEIPT_BYTES {
            anyhow::bail!(
                "FlowReceipt::mint: {} serialises to {size} bytes, over the {MAX_RECEIPT_BYTES} \
                 byte cap; not minted (there is no spill protocol)",
                receipt.read_set.instance_uri
            );
        }
        Ok(receipt)
    }

    /// The distinct seals among the atoms the fold counted, in the order the
    /// settled edges were walked. What [`EvidencePreimage`] entries a mint
    /// has to collect.
    ///
    /// Exactly the counted atoms, nothing speculative: it folds first and
    /// keeps only atoms named in `settled[].atom_uris` — the proposals that
    /// contributed the counted votes.
    ///
    /// # The dedupe key is `seal`, and that is a contract
    ///
    /// The key is `seal` while the retained payload is `to_state` and
    /// `proposer`, so two counted atoms sharing a seal collapse to one entry
    /// and the survivor's `to_state` / `proposer` stand in for the dropped
    /// one's. Atoms *can* share a seal — two states with identical `requires`
    /// matching identical instances is the guard-identical sibling-edge shape
    /// from #1062 — so this is a live case, not a theoretical one.
    ///
    /// It is sound only because those two fields exist **solely to re-derive
    /// the seal** (see [`CountedAtom`]): anything that re-derives to the same
    /// seal is interchangeable for that purpose, so which representative
    /// survives cannot matter.
    ///
    /// That makes the invariant load-bearing: **add a field to
    /// [`CountedAtom`] that a verifier consumes and that is not hashed into
    /// the seal, and this dedupe silently drops the variant that differs.**
    /// Such a field needs either the seal widened to cover it or the dedupe
    /// key widened to include it — not a third `CountedAtom` member.
    pub fn counted_seals(flow: &SHACLFlow, read_set: &ReadSet) -> anyhow::Result<Vec<CountedAtom>> {
        // Same ingest as `mint` and `verify_receipt`: the atoms this walks
        // must be the atoms that walk counted.
        let read_set = read_set.reverified();
        let derived = fold_read_set(flow, &read_set)?;
        let counted: std::collections::BTreeSet<&str> = derived
            .settled
            .iter()
            .flat_map(|edge| edge.atom_uris.iter().map(String::as_str))
            .collect();
        let mut out: Vec<CountedAtom> = Vec::new();
        for atom in read_set.atoms() {
            if !counted.contains(atom.uri.as_str()) {
                continue;
            }
            if out.iter().any(|c| c.seal == atom.evidence_hash) {
                continue;
            }
            out.push(CountedAtom {
                proposal_uri: atom.uri,
                to_state: atom.to_state,
                proposer: atom.proposer,
                seal: atom.evidence_hash,
            });
        }
        Ok(out)
    }
}

/// One atom whose votes the fold counted, reduced to what re-deriving its
/// seal needs: the guard belongs to `to_state`, and `$did`-substituted guards
/// resolved against the **proposer** at mint time, so re-running the seal has
/// to substitute that same identity.
///
/// "Reduced to what re-deriving its seal needs" is a contract, not a
/// description — [`FlowReceipt::counted_seals`] dedupes on `seal` alone and
/// relies on every other field here being redundant given it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CountedAtom {
    pub proposal_uri: String,
    pub to_state: String,
    pub proposer: String,
    pub seal: String,
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::*;
    use crate::perspectives::flow_evaluator::evidence_hash;
    use std::collections::BTreeSet;
    /// The happy path — and the only test that can catch `mint` *asserting* a
    /// state instead of folding for one, since every other mint test asserts a
    /// refusal. "Derived, never asserted" is this module's whole claim.
    ///
    /// Red with `terminal_state: read_set.genesis.clone()` in place of
    /// `derived.state`.
    #[test]
    fn mint_derives_the_terminal_state_from_the_carried_material() {
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect("a settled run into a terminal state mints");
        assert_eq!(receipt.terminal_state, "done");
        assert_eq!(receipt.flow_uri, "coasys://DeliveryFlow");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// A receipt is a completion claim, not a per-edge event. `open → doing`
    /// settles, but `doing` can still transition out.
    ///
    /// Red without the `is_terminal_state` guard in `mint`.
    #[test]
    fn mint_refuses_a_run_that_has_not_reached_a_terminal_state() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "doing", "value": 0.5 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Start", "from_state": "open", "to_state": "doing", "actions": [] },
                { "action_name": "Finish", "from_state": "doing", "to_state": "done", "actions": [] },
            ]),
        );
        let half_way = read_set(
            "open",
            vec![proposal("ad4m://p/1", ALICE, "open", "doing", "seal-1", T1)],
        );

        let err = FlowReceipt::mint(&flow, half_way, outs(&[OUTPUT]), Vec::new())
            .expect_err("an intermediate state is not a completion");
        assert!(
            format!("{err:#}").contains("can still transition out"),
            "the error must say why `doing` is not terminal, got: {err:#}"
        );
    }

    /// Two edges out of `open` both carry quorum: the run is irreversibly
    /// stalled, and anything that pays out on a completed flow must refuse it.
    ///
    /// Red without the `contested` guard in `mint`.
    #[test]
    fn mint_refuses_a_contested_run() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
                { "name": "rejected", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                { "action_name": "Reject", "from_state": "open", "to_state": "rejected", "actions": [] },
            ]),
        );
        // Both rival edges lead into terminal states, so both proposals
        // carry commitments — an uncommitted terminal proposal settles
        // nothing since #1108/#1118 and could not contend.
        let both = read_set(
            "open",
            vec![
                committing(
                    "ad4m://p/1",
                    ALICE,
                    "open",
                    "done",
                    "seal-1",
                    &[OUTPUT],
                    &hash_of(&[OUTPUT]),
                    T1,
                ),
                committing(
                    "ad4m://p/2",
                    BOB,
                    "open",
                    "rejected",
                    "seal-2",
                    &[OUTPUT],
                    &hash_of(&[OUTPUT]),
                    T2,
                ),
            ],
        );

        let err = FlowReceipt::mint(&flow, both, outs(&[OUTPUT]), Vec::new())
            .expect_err("a contested derivation has not completed");
        assert!(
            format!("{err:#}").contains("contested"),
            "the error must name the contention, got: {err:#}"
        );
    }

    /// A receipt with no outputs speaks for nothing, and the `granted_by`
    /// check a verifier runs could never pass. Refused before anything else,
    /// whatever the run committed to.
    ///
    /// Red without the empty check at the top of `mint`.
    #[test]
    fn mint_refuses_a_receipt_with_no_outputs() {
        let empty_commitment = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &delivered().seal,
                &[],
                &hash_of(&[]),
                T1,
            )],
        );
        let err = FlowReceipt::mint(&two_state_flow(), empty_commitment, Vec::new(), Vec::new())
            .expect_err("a receipt with no outputs speaks for nothing");
        assert!(
            format!("{err:#}").contains("no outputs to speak for"),
            "the error must name the missing binding, got: {err:#}"
        );
    }

    /// Outputs no longer come from `requires`: an unguarded terminal state
    /// binds outputs exactly like a guarded one, and no preimage is needed
    /// because its seal is over an empty bag.
    ///
    /// Red if `mint` refuses an unguarded terminal state, e.g. if the pre-
    /// rework `requires` check comes back.
    #[test]
    fn mint_binds_outputs_on_a_terminal_state_with_no_requires() {
        let unguarded = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let rs = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &evidence_hash(&[], &[]),
                &[OUTPUT],
                &hash_of(&[OUTPUT]),
                T1,
            )],
        );
        let receipt =
            FlowReceipt::mint(&unguarded, rs, outs(&[OUTPUT]), Vec::new()).expect("mints");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// `mint` writes the outputs sorted and deduplicated, whatever order the
    /// caller listed them in, so two mints of one run carry one list.
    ///
    /// Red if `mint` stores `outputs` as passed.
    #[test]
    fn mint_writes_the_outputs_sorted_and_deduplicated() {
        let three = [OUTPUT, "ad4m://deliverable/d2", "ad4m://deliverable/d3"];
        let rs = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &delivered().seal,
                &three,
                &hash_of(&three),
                T1,
            )],
        );
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            rs,
            outs(&[
                "ad4m://deliverable/d3",
                OUTPUT,
                "ad4m://deliverable/d2",
                OUTPUT,
            ]),
            vec![delivered()],
        )
        .expect("mints");
        assert_eq!(receipt.outputs, outs(&three));
    }

    /// The #1104 re-mint, from the mint side: the run's material is public,
    /// but a mint naming a node the quorum did not commit to is refused.
    ///
    /// Red if `mint` skips the comparison with the final edge's commitment.
    #[test]
    fn mint_refuses_outputs_the_final_edge_did_not_commit_to() {
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[ATTACKER]),
            vec![delivered()],
        )
        .expect_err("a re-mint naming another node must not mint");
        assert!(
            format!("{err:#}").contains(&format!(
                "but the final edge's quorum committed to `{}`",
                hash_of(&[OUTPUT])
            )),
            "the error must name the commitment, got: {err:#}"
        );
    }

    /// The quorum committed to d1 as it stood. A mint carrying d1 with other
    /// content (edited since completion, or made up) is refused, although it
    /// names the same output.
    ///
    /// Red if `mint` hashes the refs instead of the carried content.
    #[test]
    fn mint_refuses_an_output_whose_content_is_not_what_was_committed() {
        let mut edited = out_item(OUTPUT);
        edited.content = serde_json::json!({ "id": OUTPUT, "title": "edited" }).to_string();
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![edited],
            vec![delivered()],
        )
        .expect_err("edited content must not mint");
        assert!(
            format!("{err:#}").contains(&format!(
                "but the final edge's quorum committed to `{}`",
                hash_of(&[OUTPUT])
            )),
            "the error must name the commitment, got: {err:#}"
        );
    }

    /// One output given twice with two contents is ambiguous: `mint` refuses
    /// rather than picking one, before it compares anything.
    ///
    /// Red if `mint` deduplicates by `(class, id)` and keeps either content.
    #[test]
    fn mint_refuses_one_output_given_twice_with_different_content() {
        let mut edited = out_item(OUTPUT);
        edited.content = serde_json::json!({ "id": OUTPUT, "title": "edited" }).to_string();
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![out_item(OUTPUT), edited],
            vec![delivered()],
        )
        .expect_err("two contents for one output must not mint");
        assert!(
            format!("{err:#}").contains("is given twice with different content"),
            "got: {err:#}"
        );
    }

    /// A caller can hand `mint` a read-set whose `genesis` points anywhere,
    /// and the fold starts walking wherever it points. Planted at `done`, an
    /// empty read-set "completes" with zero votes behind it (r4077689141).
    ///
    /// Red without the genesis check in `fold_read_set`.
    #[test]
    fn mint_refuses_a_genesis_that_is_not_the_flows_initial_state() {
        let err = FlowReceipt::mint(
            &two_state_flow(),
            read_set("done", Vec::new()),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect_err("a walk that starts at the finish line is not a completion");
        assert!(
            format!("{err:#}").contains("genesis"),
            "the error must name the planted genesis, got: {err:#}"
        );
    }

    /// Over the cap there is no spill protocol, so the completion is logged
    /// and not minted rather than written half-carried.
    ///
    /// Red without the `MAX_RECEIPT_BYTES` check in `mint`.
    #[test]
    fn mint_refuses_a_receipt_over_the_size_cap() {
        let bulky = preimage(
            &["coasys://Transcript"],
            vec![item(
                "t1",
                "coasys://Transcript",
                &"x".repeat(MAX_RECEIPT_BYTES + 1),
            )],
        );
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered(), bulky],
        )
        .expect_err("a receipt over the cap is not minted");
        assert!(
            format!("{err:#}").contains("byte cap"),
            "the error must name the cap, got: {err:#}"
        );
    }

    // ---- which atoms a mint has to collect evidence for -------------------

    /// Only atoms the fold counted are part of the claim. A proposal for an
    /// edge the flow never declared contributes no seal, and two atoms
    /// sharing a seal need it carried once.
    ///
    /// Red if `counted_seals` returns every atom in the read-set.
    #[test]
    fn counted_seals_covers_the_settled_atoms_once_each() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
                { "name": "rejected", "value": 1.0 },
            ]),
            // `open → rejected` is deliberately NOT declared: an undeclared
            // edge never settles, however many votes it collects.
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        // Twin proposals on one edge: `{n: 2}` counts distinct voters across
        // them, so both are counted and they share one seal. Both commit to
        // the same outputs — `done` is terminal, and only atoms inside the
        // quorate commitment group are counted there (#1108/#1118).
        let committed = |nonce, proposer, at| {
            committing(
                nonce,
                proposer,
                "open",
                "done",
                "seal-1",
                &[OUTPUT],
                &hash_of(&[OUTPUT]),
                at,
            )
        };
        let rs = read_set(
            "open",
            vec![
                committed("ad4m://p/1", ALICE, T1),
                committed("ad4m://p/2", BOB, T2),
                proposal("ad4m://p/3", ALICE, "open", "rejected", "seal-never", T2),
            ],
        );

        let counted = FlowReceipt::counted_seals(&flow, &rs).expect("folds");
        let seals: Vec<&str> = counted.iter().map(|c| c.seal.as_str()).collect();
        assert_eq!(
            seals,
            vec!["seal-1"],
            "one entry per distinct counted seal, and nothing for an edge the walk never took"
        );
        assert_eq!(counted[0].to_state, "done");
        assert_eq!(counted[0].proposer, did_of(ALICE));
    }

    // ---- terminal-edge vote pooling is per outputs commitment --------------
    // (#1108 re-review, @lal-bot-coasys; the fold half of #1118 option 2)

    /// `open → done`, `done` terminal, guarded, `{ n: 2 }`: the shape of
    /// every per-commitment pooling test below.
    fn n2_terminal_flow() -> SHACLFlow {
        flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 },
                  "requires": [{ "className": DELIVERABLE }] },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    /// The quorum a terminal edge needs and the verdict it produces.
    fn mint_and_verify(
        flow: SHACLFlow,
        rs: ReadSet,
        outputs: Vec<EvidenceItem>,
    ) -> anyhow::Result<crate::perspectives::flow_instance::verify::ReceiptVerdict> {
        use crate::perspectives::flow_instance::verify::verify_receipt;
        let receipt = FlowReceipt::mint(&flow, rs, outputs, vec![delivered()])?;
        let catalogue: std::collections::HashMap<String, SHACLFlow> =
            std::iter::once((flow.flow_uri(), flow)).collect();
        Ok(verify_receipt(&catalogue, &receipt))
    }

    /// `open → done` terminal at `{ n: 2 }` — Lal's exact scenario on the
    /// re-review. Mallory (eligible — the rule has no `fromRole`) posts an
    /// **uncommitted** terminal proposal at T1; Alice commits to [`OUTPUT`]
    /// at T2 and Bob co-signs her at T3. Votes on a terminal edge pool per
    /// outputs commitment, and an atom with no commitment contributes
    /// nothing, so Alice + Bob settle within their group and the run mints
    /// and verifies — Mallory's early garbage cannot poison the edge into a
    /// permanent `Uncommitted`.
    ///
    /// Red while `settle_edge` pools every atom on the edge: Mallory's T1
    /// vote counts toward quorum, her atom lands in `atom_uris`, and
    /// `final_edge_commitment` reports `Uncommitted` — no receipt, ever.
    /// Also the killing test for the mutation that pools terminal edges
    /// across commitments again.
    #[test]
    fn an_early_uncommitted_terminal_proposal_cannot_poison_the_committed_quorum() {
        let flow = n2_terminal_flow();
        let mallory = proposal(
            "p-mallory",
            "mallory",
            "open",
            "done",
            &delivered().seal,
            T1,
        );
        let mut alice = committing(
            "p-alice",
            ALICE,
            "open",
            "done",
            &delivered().seal,
            &[OUTPUT],
            &hash_of(&[OUTPUT]),
            T2,
        );
        alice.links.push(signed_vote(&alice.uri, BOB, T3));
        let rs = read_set("open", vec![mallory, alice]);

        let verdict = mint_and_verify(flow, rs, outs(&[OUTPUT]))
            .expect("Alice + Bob reach quorum inside the committed group and the run mints");
        let crate::perspectives::flow_instance::verify::ReceiptVerdict::Verified { voters, .. } =
            &verdict
        else {
            panic!("expected Verified, got: {verdict}");
        };
        let expected: Vec<String> = [did_of(ALICE), did_of(BOB)]
            .iter()
            .map(|d| d.to_string())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        assert_eq!(
            voters, &expected,
            "the quorum is Alice and Bob; Mallory's vote bound nothing"
        );
    }

    /// The same shape with Mallory carrying a **valid but different**
    /// commitment: her group holds one vote, Alice's holds two, so Alice's
    /// settles and the receipt for [`OUTPUT`] mints — the twin commitment no
    /// longer turns the settled edge `Conflicting`.
    ///
    /// Red while the fold pools across commitments: both hashes land on the
    /// settled edge and `final_edge_commitment` reports `Conflicting`.
    #[test]
    fn a_rival_commitment_short_of_quorum_cannot_conflict_the_settled_edge() {
        let flow = n2_terminal_flow();
        let mallory = committing(
            "p-mallory",
            "mallory",
            "open",
            "done",
            &delivered().seal,
            &[ATTACKER],
            &hash_of(&[ATTACKER]),
            T1,
        );
        let mut alice = committing(
            "p-alice",
            ALICE,
            "open",
            "done",
            &delivered().seal,
            &[OUTPUT],
            &hash_of(&[OUTPUT]),
            T2,
        );
        alice.links.push(signed_vote(&alice.uri, BOB, T3));
        let rs = read_set("open", vec![mallory, alice]);

        let verdict = mint_and_verify(flow, rs.clone(), outs(&[OUTPUT]))
            .expect("the committed quorum mints past the one-vote rival");
        assert!(verdict.is_verified(), "got: {verdict}");
        assert!(
            FlowReceipt::mint(
                &n2_terminal_flow(),
                rs,
                outs(&[ATTACKER]),
                vec![delivered()]
            )
            .is_err(),
            "and the rival's outputs still bind nothing"
        );
    }

    /// Twins committing the **same** outputs under different nonces pool as
    /// one group: the per-commitment rule must not split a quorum that
    /// genuinely agrees. Alice and Bob each propose `(done, OUTPUT)` with
    /// their own nonce; one vote each reaches `{ n: 2 }`.
    #[test]
    fn terminal_twins_committing_the_same_outputs_pool_as_one_group() {
        let flow = n2_terminal_flow();
        let twin = |nonce: &str, proposer: &str, at: &str| {
            committing(
                nonce,
                proposer,
                "open",
                "done",
                &delivered().seal,
                &[OUTPUT],
                &hash_of(&[OUTPUT]),
                at,
            )
        };
        let rs = read_set(
            "open",
            vec![twin("twin-a", ALICE, T1), twin("twin-b", BOB, T2)],
        );
        let verdict = mint_and_verify(flow, rs, outs(&[OUTPUT]))
            .expect("two twins with one commitment are one group and reach quorum");
        assert!(verdict.is_verified(), "got: {verdict}");
    }
}
