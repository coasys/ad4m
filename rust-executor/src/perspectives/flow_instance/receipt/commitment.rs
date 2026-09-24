//! The outputs commitment read off the fold's final edge (#1104): the one
//! reading `mint` and `verify_receipt` share.

use crate::perspectives::flow_instance::atom::OUTPUTS_HASH_PREDICATE;
use crate::perspectives::flow_instance::fold::DerivedState;
use crate::perspectives::flow_instance::ReadSet;
use std::collections::BTreeSet;
/// What the final edge's quorum committed to as the run's outputs. See
/// [`final_edge_commitment`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputsCommitment {
    /// Every atom counted on the final edge carries this `outputs_hash`.
    Committed(String),
    /// A counted atom on the final edge carries no `outputs_hash`, so its
    /// voters agreed to no outputs at all.
    Uncommitted { proposal_uri: String },
    /// Counted atoms on the final edge carry different `outputs_hash`
    /// values, sorted. Quorum belongs to the edge, so no one hash was agreed
    /// by the whole quorum.
    Conflicting { hashes: Vec<String> },
    /// The walk settled no edge, so there is no final edge to read.
    NoFinalEdge,
}

impl std::fmt::Display for OutputsCommitment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Committed(hash) => write!(f, "the final edge commits to outputs `{hash}`"),
            Self::Uncommitted { proposal_uri } => write!(
                f,
                "proposal {proposal_uri}, counted on the final edge, carries no \
                 `{OUTPUTS_HASH_PREDICATE}`, so its voters agreed to no outputs"
            ),
            Self::Conflicting { hashes } => write!(
                f,
                "the atoms counted on the final edge commit to different outputs {hashes:?}, so \
                 no set of outputs was agreed by the whole quorum"
            ),
            Self::NoFinalEdge => write!(
                f,
                "the walk settled no edge, so nothing committed to outputs"
            ),
        }
    }
}

/// **The one reading of a run's outputs commitment.** `mint` and
/// `verify_receipt` both call it, so the two sides cannot disagree on what a
/// receipt may speak for.
///
/// `derived` must be the fold of `ingested`, the re-verified read-set. Takes
/// the last settled edge (the one into the terminal state), and the
/// `outputs_hash` on each atom the fold counted there.
///
/// # Strict across twins
///
/// Quorum belongs to an edge, not to a proposal (see [`super::fold`]), so the
/// counted votes into the terminal state can sit on twin atoms. When twins
/// commit to different outputs, each voter agreed only to their own atom's
/// set, and no set was agreed by the whole quorum. That is
/// [`OutputsCommitment::Conflicting`] and a receipt is refused; there is no
/// intersection or union. An atom with no commitment on the final edge is
/// [`OutputsCommitment::Uncommitted`].
///
/// Since #1108/#1118 the fold pools terminal-edge votes per commitment and
/// counts no uncommitted atom there, so a settled final edge's atoms all
/// share one hash and neither refusal is reachable through
/// [`super::fold_read_set`]: twins with rival commitments each settle or
/// fall short on their own votes. Both arms stay as defence in depth
/// against a fold regression; the tests pin each against a hand-built
/// `DerivedState`.
pub fn final_edge_commitment(derived: &DerivedState, ingested: &ReadSet) -> OutputsCommitment {
    let Some(final_edge) = derived.settled.last() else {
        return OutputsCommitment::NoFinalEdge;
    };
    let mut hashes: BTreeSet<String> = BTreeSet::new();
    for atom in ingested.atoms() {
        if !final_edge.atom_uris.contains(&atom.uri) {
            continue;
        }
        match atom.outputs_hash {
            Some(hash) => {
                hashes.insert(hash);
            }
            None => {
                return OutputsCommitment::Uncommitted {
                    proposal_uri: atom.uri,
                }
            }
        }
    }
    let mut hashes: Vec<String> = hashes.into_iter().collect();
    match hashes.len() {
        1 => OutputsCommitment::Committed(hashes.remove(0)),
        // `atom_uris` names at least one atom on a settled edge; zero would
        // mean the read-set lost an atom the fold counted. Fail closed.
        0 => OutputsCommitment::NoFinalEdge,
        _ => OutputsCommitment::Conflicting { hashes },
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::super::FlowReceipt;
    use super::*;
    use crate::perspectives::flow_evaluator::evidence_hash;
    use crate::perspectives::flow_instance::fold::SettledEdge;
    use crate::perspectives::flow_instance::fold_read_set;
    /// Only the edge into the terminal state commits to outputs. The earlier
    /// edge's proposal carries no `outputs_hash` at all, which is correct
    /// for a non-terminal state and must not block the mint.
    ///
    /// Red with `derived.settled.first()` in place of `.last()` in
    /// `final_edge_commitment`: the earlier atom reads as `Uncommitted`.
    #[test]
    fn mint_reads_the_commitment_off_the_final_edge_not_an_earlier_one() {
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
        let rs = read_set(
            "open",
            vec![
                proposal("ad4m://p/1", ALICE, "open", "doing", "seal-1", T1),
                committing(
                    "ad4m://p/2",
                    BOB,
                    "doing",
                    "done",
                    "seal-2",
                    &[OUTPUT],
                    &hash_of(&[OUTPUT]),
                    T2,
                ),
            ],
        );
        let receipt = FlowReceipt::mint(&flow, rs, outs(&[OUTPUT]), Vec::new()).expect("mints");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// A final-edge proposal with no commitment binds nothing — and since
    /// #1108/#1118 it does not even settle: the fold pools terminal-edge
    /// votes per commitment and an uncommitted atom contributes nothing, so
    /// the run stays short of the terminal state and `mint` refuses it as
    /// incomplete rather than as uncommitted.
    ///
    /// [`OutputsCommitment::Uncommitted`] is thereby unreachable through the
    /// fold and stays as defence in depth; the second half pins it directly
    /// against a hand-built settled edge naming the uncommitted atom, so the
    /// arm cannot rot into "skip the atom" unnoticed.
    #[test]
    fn mint_refuses_a_final_edge_that_committed_to_no_outputs() {
        let uncommitted = proposal("ad4m://p/1", ALICE, "open", "done", &delivered().seal, T1);
        let uncommitted_uri = uncommitted.uri.clone();
        let rs = read_set("open", vec![uncommitted]);

        let derived = fold_read_set(&two_state_flow(), &rs.reverified()).expect("folds");
        assert_eq!(
            derived.state, "open",
            "an uncommitted terminal proposal settles nothing (#1108/#1118)"
        );
        let err = FlowReceipt::mint(
            &two_state_flow(),
            rs.clone(),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect_err("no commitment, no settle, no receipt");
        assert!(
            format!("{err:#}").contains("can still transition out"),
            "got: {err:#}"
        );

        // Defence in depth: were an uncommitted atom ever counted on a
        // settled final edge again, the commitment must name it, not skip it.
        let planted = DerivedState {
            state: "done".to_string(),
            settled: vec![SettledEdge {
                from_state: "open".to_string(),
                to_state: "done".to_string(),
                settled_at: T1.to_string(),
                atom_uris: vec![uncommitted_uri.clone()],
                voters: vec![did_of(ALICE).to_string()],
            }],
            contested: None,
        };
        assert_eq!(
            final_edge_commitment(&planted, &rs.reverified()),
            OutputsCommitment::Uncommitted {
                proposal_uri: uncommitted_uri
            }
        );
    }

    /// Twin atoms on the final edge that commit to different outputs: no one
    /// set was agreed by the whole quorum, so `mint` refuses whichever set
    /// the caller names. Since #1108/#1118 the refusal comes one layer
    /// earlier — each commitment is its own vote pool, neither reaches
    /// `{n: 2}`, the run never settles — instead of a settled-then-
    /// `Conflicting` dead end.
    ///
    /// [`OutputsCommitment::Conflicting`] is thereby unreachable through the
    /// fold and stays as defence in depth; the second half pins it directly
    /// against a hand-built settled edge naming both atoms, so the arm
    /// cannot rot into "read the first counted atom's hash" unnoticed.
    #[test]
    fn mint_refuses_twin_final_edge_atoms_with_different_commitments() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let seal = evidence_hash(&[], &[]);
        let alice = committing(
            "ad4m://p/1",
            ALICE,
            "open",
            "done",
            &seal,
            &[OUTPUT],
            &hash_of(&[OUTPUT]),
            T1,
        );
        let bob = committing(
            "ad4m://p/2",
            BOB,
            "open",
            "done",
            &seal,
            &[ATTACKER],
            &hash_of(&[ATTACKER]),
            T2,
        );
        let atom_uris = {
            let mut uris = vec![alice.uri.clone(), bob.uri.clone()];
            uris.sort();
            uris
        };
        let rs = read_set("open", vec![alice, bob]);

        for named in [[OUTPUT], [ATTACKER]] {
            let err = FlowReceipt::mint(&flow, rs.clone(), outs(&named), Vec::new())
                .expect_err("conflicting commitments bind nothing");
            assert!(
                format!("{err:#}").contains("can still transition out"),
                "one vote per commitment is short of `{{n: 2}}` in every group, \
                 so the run must not settle; naming {named:?}, got: {err:#}"
            );
        }

        // Defence in depth: were atoms with different commitments ever
        // counted on one settled final edge again, no hash may be picked.
        let planted = DerivedState {
            state: "done".to_string(),
            settled: vec![SettledEdge {
                from_state: "open".to_string(),
                to_state: "done".to_string(),
                settled_at: T2.to_string(),
                atom_uris,
                voters: vec![did_of(ALICE).to_string(), did_of(BOB).to_string()],
            }],
            contested: None,
        };
        let mut hashes = vec![hash_of(&[OUTPUT]), hash_of(&[ATTACKER])];
        hashes.sort();
        assert_eq!(
            final_edge_commitment(&planted, &rs.reverified()),
            OutputsCommitment::Conflicting { hashes }
        );
    }
}
