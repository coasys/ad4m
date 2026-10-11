//! The content-addressed proposal URI (#1108): the one thing a vote signs,
//! and therefore the address of every field a vote must cover.

use super::PROPOSAL_URI_TAG;
/// **The one producer of a proposal's URI**: `ad4m://flow/proposal/<hash>`,
/// where the hash is the [`content_address`](crate::perspectives::content_address::content_address)
/// of everything a co-signer's vote must cover.
///
/// A vote is `proposal_uri --acceptedBy--> voter` — it signs the URI and
/// nothing else. With a random URI that covered nothing, and the final-edge
/// proposer could re-sign `outputs_hash` (or the seal) *under the same URI*
/// after the votes landed, walking a swapped output into a verified receipt
/// (#1108 review, @lal-bot-coasys). Deriving the URI from the fields closes
/// it: a proposal whose fields do not hash to its own URI is not an atom
/// ([`AtomRejection::UriMismatch`](super::AtomRejection::UriMismatch)), so every vote the fold counts covers
/// the instance, the edge, the seal and the outputs commitment.
///
/// The preimage is positional under [`PROPOSAL_URI_TAG`]:
/// `(instance, from_state, to_state, evidence_hash, outputs_hash-or-none)`,
/// with the proposer as the address's author and the proposer-signed
/// [`PROPOSAL_NONCE_PREDICATE`](super::PROPOSAL_NONCE_PREDICATE) value as its salt. `outputs_hash` stays an
/// `Option` — absent (a non-terminal proposal) is not the empty string.
///
/// Every producer routes through here — the engine pass and the manual
/// propose path via `flow_classes::write_flow_transition_proposal`, and the
/// test fixtures — so there is exactly one definition of "the URI for these
/// fields".
pub fn proposal_uri(
    instance_uri: &str,
    from_state: &str,
    to_state: &str,
    evidence_hash: &str,
    outputs_hash: Option<&str>,
    proposer: &str,
    nonce: &str,
) -> String {
    let hash = crate::perspectives::content_address::content_address(
        PROPOSAL_URI_TAG,
        proposer,
        &[
            Some(instance_uri),
            Some(from_state),
            Some(to_state),
            Some(evidence_hash),
            outputs_hash,
        ],
        nonce,
    );
    crate::perspectives::flow_classes::flow_transition_proposal_uri(&hash)
}

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::perspectives::flow_instance::test_support::*;
    /// **The URI is the fields, or the proposal is not an atom.** A vote is
    /// `uri --acceptedBy--> did`, so a URI that does not commit to the
    /// proposer-signed fields lets the proposer swap them after the votes
    /// land. Three shapes must reject: a URI addressed for other fields (the
    /// swap), a legacy random-UUID URI (a vote on it covers nothing), and a
    /// proposal with no signed nonce at all (nothing to recompute from).
    ///
    /// Red while `from_links` skips the recompute: every shape here parses.
    #[test]
    fn an_atom_whose_uri_is_not_its_fields_content_address_is_rejected() {
        let links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        let expected = addressed_uri(&links);
        assert!(
            atom_of(&links).is_ok(),
            "control: under its own content address the proposal is an atom"
        );

        // The swap / legacy shape: same signed fields, other URI.
        assert_eq!(
            TransitionAtom::from_links(INSTANCE, PROPOSAL, &links),
            Err(AtomRejection::UriMismatch {
                expected: expected.clone()
            }),
            "a URI the fields do not address is rejected, and the rejection \
             names what they do address"
        );
        assert_eq!(
            TransitionAtom::from_links(
                INSTANCE,
                "ad4m://flow/proposal/8f0e1a44-3d3c-4e0a-9c9c-3f5a1b2c3d4e",
                &links
            ),
            Err(AtomRejection::UriMismatch { expected }),
            "a pre-#1108 UUID URI is the same rejection — a vote on it covers nothing"
        );

        // No nonce: nothing to recompute the address from. `MissingField`,
        // not `UriMismatch` — the reader must not invent a salt.
        let mut without_nonce = honest_proposal(ALICE, "review", "approved", "h1", T1);
        without_nonce.retain(|l| l.data.predicate.as_deref() != Some(PROPOSAL_NONCE_PREDICATE));
        assert_eq!(
            atom_of(&without_nonce),
            Err(AtomRejection::MissingField(PROPOSAL_NONCE_PREDICATE))
        );
    }

    /// The address covers every field a vote must cover: change one — the
    /// outputs commitment, the seal, the edge, the nonce — and the URI that
    /// was right before is wrong now. This is the property the swap tests in
    /// `verify` exercise end to end; here it is pinned per field.
    ///
    /// Red while `from_links` skips the recompute, and red for the mutations
    /// that drop `outputs_hash` or the seal from the URI preimage.
    #[test]
    fn re_signing_any_addressed_field_under_the_old_uri_rejects_the_atom() {
        let original = with_outputs(&[D1], &hash_of(&[D1]));
        let uri = addressed_uri(&original);
        assert!(
            TransitionAtom::from_links(INSTANCE, &uri, &original).is_ok(),
            "control: the unswapped fields address `{uri}`"
        );

        // Replace one proposer-signed field wholesale — the withheld-and-
        // replaced shape, where the original links never reach the reader.
        let resigned = |predicate: &'static str, value: &str| {
            let mut links = original.clone();
            links.retain(|l| l.data.predicate.as_deref() != Some(predicate));
            links.push(link(predicate, &literal(value), ALICE, true, T2));
            TransitionAtom::from_links(INSTANCE, &uri, &links)
        };

        let swapped_outputs = resigned(OUTPUTS_HASH_PREDICATE, &hash_of(&[ATTACKER]));
        assert!(
            matches!(swapped_outputs, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed outputs commitment no longer addresses the voted URI: {swapped_outputs:?}"
        );
        let swapped_seal = resigned(EVIDENCE_HASHES_PREDICATE, "h2-reframed-evidence");
        assert!(
            matches!(swapped_seal, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed evidence seal no longer addresses the voted URI: {swapped_seal:?}"
        );
        let swapped_edge = resigned(TO_STATE_PREDICATE, "rejected");
        assert!(
            matches!(swapped_edge, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed target state no longer addresses the voted URI: {swapped_edge:?}"
        );
    }
}
