//! The outputs commitment of a proposal into a terminal state (#1104):
//! what a run's outputs are, how they are hashed, and the check every
//! voter runs over them before co-signing.

use super::{
    field_value, links_on, signed_by, AtomRejection, TransitionAtom, OUTPUTS_HASH_PREDICATE,
    OUTPUTS_HASH_TAG, OUTPUT_PREDICATE,
};
use crate::perspectives::flow_evaluator::EvidenceItem;
use crate::types::DecoratedLinkExpression;
use serde::{Deserialize, Serialize};
/// One output of a run: an instance, and the class it is an instance of.
///
/// The class is part of the name because an output's content is what
/// `model_query` returns for that class's shape, and the same node read
/// through another class is other content. Serialised camelCase
/// (`{"className", "id"}`), the shape the TS client sends.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct OutputRef {
    pub class_name: String,
    pub id: String,
}

impl OutputRef {
    /// The text an [`OUTPUT_PREDICATE`] link carries: canonical JSON, keys
    /// sorted, so one ref has one encoding.
    pub fn encode(&self) -> String {
        crate::perspectives::flow_evaluator::canonical_json(&serde_json::json!({
            "className": self.class_name,
            "id": self.id,
        }))
    }

    /// Inverse of [`encode`](Self::encode). A JSON object with exactly these
    /// two string fields parses; nothing else does.
    pub fn decode(text: &str) -> Option<Self> {
        serde_json::from_str(text).ok()
    }

    /// The ref an output preimage names.
    pub fn of(item: &EvidenceItem) -> Self {
        Self {
            class_name: item.class_name.clone(),
            id: item.id.clone(),
        }
    }
}

/// The one definition of a run's outputs commitment: the
/// [`evidence_hash`](crate::perspectives::flow_evaluator::evidence_hash)
/// framing over each output's `(class, id, canonical content)`, under
/// [`OUTPUTS_HASH_TAG`] so it can never be read as an evidence seal
/// ([`tagged_items_hash`](crate::perspectives::flow_evaluator::tagged_items_hash)).
///
/// `items` are what `model_query` returns for each named output, the same
/// shape the evidence seal hashes, so **editing an output changes the hash**.
/// Order does not change the result. Duplicates do: every writer hashes one
/// item per [`normalised_outputs`] ref, and a receipt carrying a duplicate is
/// refused rather than normalised.
pub fn outputs_hash(items: &[EvidenceItem]) -> String {
    crate::perspectives::flow_evaluator::tagged_items_hash(OUTPUTS_HASH_TAG, items)
}

/// `refs` sorted and deduplicated: the form a proposal names.
pub fn normalised_outputs(refs: &[OutputRef]) -> Vec<OutputRef> {
    let mut refs = refs.to_vec();
    refs.sort();
    refs.dedup();
    refs
}

/// Why a voter refuses to co-sign a proposal into a terminal state. Each
/// reason is its own variant so a client and a test can tell them apart.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputsRefusal {
    /// The proposal enters a terminal state but its proposer signed no
    /// [`OUTPUTS_HASH_PREDICATE`]. Nothing would bind a receipt for the run
    /// to any output, so co-signing it would complete a run no receipt can
    /// speak for.
    Uncommitted,
    /// A named output is not an instance of the class it is named as, on
    /// this replica.
    OutputNotInstance { output: OutputRef },
    /// The proposer's signed `outputs_hash` is not the hash of the named
    /// outputs' content as this replica reads it: an output was edited since
    /// the proposal, this replica has not synced an edit yet, or the proposer
    /// committed to something else.
    HashMismatch {
        /// `outputs_hash` as the proposer signed it.
        committed: String,
        /// [`outputs_hash`] of the named outputs' content on this replica.
        recomputed: String,
    },
}

impl std::fmt::Display for OutputsRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uncommitted => write!(
                f,
                "it enters a terminal state but carries no `{OUTPUTS_HASH_PREDICATE}`, so no \
                 receipt could bind the run to any output"
            ),
            Self::OutputNotInstance { output } => write!(
                f,
                "it names `{}` as an output of class `{}`, and this replica has no such \
                 instance of that class",
                output.id, output.class_name
            ),
            Self::HashMismatch {
                committed,
                recomputed,
            } => write!(
                f,
                "its `{OUTPUTS_HASH_PREDICATE}` is `{committed}`, but the outputs it names hash \
                 to `{recomputed}` on this replica: their content is not what the proposer \
                 committed to"
            ),
        }
    }
}

/// The voter's outputs check, pure. Run by `super::accept` before co-signing
/// and by `super::propose` before the proposer's own vote, so both sides of a
/// vote apply one rule.
///
/// Only a proposal into a terminal state is checked (`terminal`); anywhere
/// else a run does not end and there is nothing to bind. `content` answers
/// what this replica's `model_query` returns for a named output, `None` when
/// it is not an instance of that class. Checks in order: a commitment is
/// present, every named output is an instance of its class, and the
/// commitment is the hash of their content.
pub fn check_outputs_commitment(
    atom: &TransitionAtom,
    terminal: bool,
    content: impl Fn(&OutputRef) -> Option<EvidenceItem>,
) -> Result<(), OutputsRefusal> {
    if !terminal {
        return Ok(());
    }
    let Some(committed) = &atom.outputs_hash else {
        return Err(OutputsRefusal::Uncommitted);
    };
    let mut items = Vec::with_capacity(atom.outputs.len());
    for output in &atom.outputs {
        match content(output) {
            Some(item) => items.push(item),
            None => {
                return Err(OutputsRefusal::OutputNotInstance {
                    output: output.clone(),
                })
            }
        }
    }
    let recomputed = outputs_hash(&items);
    if &recomputed != committed {
        return Err(OutputsRefusal::HashMismatch {
            committed: committed.clone(),
            recomputed,
        });
    }
    Ok(())
}

/// The outputs `proposer` named with [`OUTPUT_PREDICATE`], sorted and
/// deduplicated. A third party's `output` link is invisible here, exactly as
/// in [`unique_field`](super::unique_field), so nobody can add an output to someone else's
/// proposal. One of the proposer's own links that does not decode rejects
/// the atom ([`AtomRejection::MalformedOutput`]).
pub(super) fn named_outputs(
    links: &[DecoratedLinkExpression],
    proposer: &str,
) -> Result<Vec<OutputRef>, AtomRejection> {
    let refs = links_on(links, OUTPUT_PREDICATE)
        .filter(|l| signed_by(l, proposer))
        .map(|l| {
            let text = field_value(&l.data.target);
            OutputRef::decode(&text).ok_or(AtomRejection::MalformedOutput(text))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(normalised_outputs(&refs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::test_support::*;
    /// What this replica's `model_query` returns for a ref: the fixture
    /// content, for the ids in `present` only.
    fn graph_with<'a>(present: &'a [&'a str]) -> impl Fn(&OutputRef) -> Option<EvidenceItem> + 'a {
        move |r: &OutputRef| {
            (r.class_name == OUT_CLASS && present.contains(&r.id.as_str())).then(|| out_item(&r.id))
        }
    }

    /// The hash is over each output's `(class, id, content)`, in any order.
    /// Editing an output's content changes it, so does reading the same id
    /// as another class, and so does the item set. It is never the evidence
    /// seal over the same items.
    ///
    /// Red if `outputs_hash` hashes ids only, drops the class or the
    /// content, is order-sensitive, or loses its domain tag.
    #[test]
    fn outputs_hash_is_over_class_id_and_content() {
        assert_eq!(hash_of(&[D2, D1]), hash_of(&[D1, D2]), "order-independent");
        assert_ne!(hash_of(&[D1]), hash_of(&[D1, D2]));
        assert_ne!(hash_of(&[D1]), hash_of(&[ATTACKER]));
        assert_ne!(hash_of(&[]), hash_of(&[D1]));

        let mut edited = out_item(D1);
        edited.content = serde_json::json!({ "id": D1, "title": "edited" }).to_string();
        assert_ne!(outputs_hash(&[edited]), hash_of(&[D1]), "content-sensitive");

        let mut other_class = out_item(D1);
        other_class.class_name = "coasys://Other".to_string();
        assert_ne!(
            outputs_hash(&[other_class]),
            hash_of(&[D1]),
            "class-sensitive"
        );

        let mut reordered_keys = out_item(D1);
        reordered_keys.content = format!(r#"{{"title":"the content of {D1}","id":"{D1}"}}"#);
        assert_eq!(
            outputs_hash(&[reordered_keys]),
            hash_of(&[D1]),
            "canonical: key order is not content"
        );

        assert_ne!(
            hash_of(&[D1]),
            crate::perspectives::flow_evaluator::evidence_hash(&[], &out_items(&[D1])),
            "an outputs hash is never an evidence seal over the same items"
        );
    }

    /// An [`OutputRef`] round-trips through its link encoding, and the
    /// encoding is canonical. Anything that is not exactly a
    /// `{className, id}` object does not decode.
    #[test]
    fn an_output_ref_encodes_canonically_and_decodes_strictly() {
        let r = out_ref(D1);
        assert_eq!(
            r.encode(),
            format!(r#"{{"className":"{OUT_CLASS}","id":"{D1}"}}"#)
        );
        assert_eq!(OutputRef::decode(&r.encode()), Some(r));
        assert_eq!(OutputRef::decode(D1), None, "a bare id is not a ref");
        assert_eq!(OutputRef::decode(r#"{"id":"x"}"#), None);
        assert_eq!(
            OutputRef::decode(r#"{"className":"c","id":"x","extra":1}"#),
            None
        );
    }

    /// The atom reads the proposer's named outputs and commitment, and only
    /// the proposer's: Mallory's `output` link on Alice's proposal names
    /// nothing. Two distinct commitments by Alice reject the atom, the same
    /// way two `to_state` values do, and so does an `output` link of hers
    /// that is not a ref.
    ///
    /// Red if `named_outputs` drops its `signed_by` filter (Mallory's node
    /// becomes an output), skips an undecodable link instead of rejecting,
    /// or if an ambiguous `outputs_hash` is read as `None`.
    #[test]
    fn an_atom_reads_only_the_proposers_named_outputs_and_commitment() {
        let mut links = with_outputs(&[D2, D1], &hash_of(&[D1, D2]));
        links.push(link(
            OUTPUT_PREDICATE,
            &literal(&out_ref(ATTACKER).encode()),
            MALLORY,
            true,
            T2,
        ));
        let atom = atom_of(&links).expect("atom");
        assert_eq!(atom.outputs, vec![out_ref(D1), out_ref(D2)]);
        assert_eq!(atom.outputs_hash, Some(hash_of(&[D1, D2])));

        let plain = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1)).expect("atom");
        assert_eq!(plain.outputs_hash, None, "no commitment is not a rejection");
        assert!(plain.outputs.is_empty());

        let mut bare = with_outputs(&[D1], &hash_of(&[D1]));
        bare.push(link(OUTPUT_PREDICATE, D2, ALICE, true, T1));
        assert_eq!(
            atom_of(&bare),
            Err(AtomRejection::MalformedOutput(D2.to_string()))
        );

        links.push(link(
            OUTPUTS_HASH_PREDICATE,
            &literal(&hash_of(&[ATTACKER])),
            ALICE,
            true,
            T2,
        ));
        assert_eq!(
            atom_of(&links),
            Err(AtomRejection::AmbiguousField(OUTPUTS_HASH_PREDICATE))
        );
    }

    /// **Required test (b).** The proposer names d1 and signs a hash over d1
    /// and the attacker's node. A voter loads d1, recomputes, and refuses.
    ///
    /// Red if `check_outputs_commitment` skips the recompute, or compares the
    /// commitment against itself.
    #[test]
    fn a_voter_refuses_an_outputs_hash_that_does_not_match_the_named_outputs() {
        let atom = atom_of(&with_outputs(&[D1], &hash_of(&[D1, ATTACKER]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&atom, true, graph_with(&[D1, ATTACKER])),
            Err(OutputsRefusal::HashMismatch {
                committed: hash_of(&[D1, ATTACKER]),
                recomputed: hash_of(&[D1]),
            })
        );
        let honest = atom_of(&with_outputs(&[D1], &hash_of(&[D1]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&honest, true, graph_with(&[D1])),
            Ok(()),
            "control: the matching commitment passes"
        );
    }

    /// The ids and the commitment match, but this replica reads d1 with other
    /// content than the proposer hashed: edited since the proposal. Refused
    /// as a hash mismatch, not as a missing instance.
    ///
    /// Red if the hash is recomputed from the named refs instead of the
    /// content this replica loaded.
    #[test]
    fn a_voter_refuses_an_output_whose_content_changed_since_the_proposal() {
        let atom = atom_of(&with_outputs(&[D1], &hash_of(&[D1]))).expect("atom");
        let mut edited = out_item(D1);
        edited.content = serde_json::json!({ "id": D1, "title": "edited" }).to_string();
        let edited_graph = |r: &OutputRef| (r == &out_ref(D1)).then(|| edited.clone());
        assert_eq!(
            check_outputs_commitment(&atom, true, edited_graph),
            Err(OutputsRefusal::HashMismatch {
                committed: hash_of(&[D1]),
                recomputed: outputs_hash(&[edited.clone()]),
            })
        );
    }

    /// **Required test (c).** The commitment matches, but one named output is
    /// not an instance of its class on this replica. The voter refuses and
    /// names it, before any hash is compared.
    ///
    /// Red if `check_outputs_commitment` skips the instance check.
    #[test]
    fn a_voter_refuses_a_named_output_that_is_not_an_instance() {
        let atom = atom_of(&with_outputs(&[D1, D2], &hash_of(&[D1, D2]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&atom, true, graph_with(&[D1])),
            Err(OutputsRefusal::OutputNotInstance {
                output: out_ref(D2)
            })
        );
    }

    /// A terminal proposal with no commitment is refused as `Uncommitted`.
    /// A non-terminal proposal is not checked at all: a run that does not end
    /// there has nothing to bind.
    ///
    /// Red if a missing commitment is treated as the empty set (it would pass
    /// as "no outputs"), or if non-terminal proposals are checked.
    #[test]
    fn only_a_terminal_proposal_must_commit_to_its_outputs() {
        let plain = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1)).expect("atom");
        assert_eq!(
            check_outputs_commitment(&plain, true, graph_with(&[])),
            Err(OutputsRefusal::Uncommitted)
        );
        let unloaded = atom_of(&with_outputs(&[D1], &hash_of(&[D2]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&unloaded, false, graph_with(&[])),
            Ok(())
        );
    }
}
