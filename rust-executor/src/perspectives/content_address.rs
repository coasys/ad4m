//! One generic content address over an author's signed fields.
//!
//! A content address makes a node's **URI** a commitment to the fields its
//! author signed: change any field and the URI changes, so anything that
//! signs the URI — a flow co-signer's `acceptedBy` link, say — has signed the
//! fields too, without carrying them. That closes the swap the flow engine's
//! #1108 review found: a vote on a *random* URI covers nothing, and the
//! author could re-point the fields under it after the votes landed.
//!
//! This module is deliberately tiny and knows nothing about flows. The flow
//! proposal URI ([`super::flow_instance::atom::proposal_uri`]) is its first
//! consumer; #1119 plans to lift content addressing into `Ad4mModel` /
//! `model_query`, and a generic `(tag, author, fields, nonce)` helper is the
//! shape that lift can reuse.
//!
//! # Digest layout
//!
//! SHA256 over, in order:
//!
//! 1. `u64::MAX` — the same sentinel [`tagged_items_hash`] opens with, so no
//!    content address can be read as an evidence seal (whose first eight
//!    bytes are a class-name *count*; see `flow_evaluator::evidence_hash`).
//! 2. the framed `tag` — the domain, so two consumers with different tags can
//!    never collide with each other, nor with `tagged_items_hash`'s tags.
//! 3. the framed `author` — the address binds *whose* fields these are.
//! 4. the field count, then each field: one presence byte (`0` = absent,
//!    `1` = present) and, when present, the framed value. The presence byte
//!    is what makes `None` distinct from `Some("")`, and the count is what
//!    keeps a consumer from appending fields without changing the domain.
//! 5. the framed `nonce` — the author's uniqueness salt, so one author can
//!    address two nodes with identical fields.
//!
//! Every value goes through [`frame`](super::flow_evaluator::frame) —
//! length-prefixed, the framing `evidence_hash` and `tagged_items_hash`
//! already use — so no value's content can shift bytes across a boundary.

use sha2::{Digest, Sha256};

/// Hex SHA256 content address of `author`'s `fields` under `tag`, salted
/// with `nonce`. See the module doc for the exact digest layout and the
/// collision argument.
///
/// `fields` is positional: the *meaning* of each slot is the caller's
/// convention under its `tag`, and an absent optional field must stay in its
/// slot as `None` rather than being dropped.
pub(crate) fn content_address(
    tag: &str,
    author: &str,
    fields: &[Option<&str>],
    nonce: &str,
) -> String {
    use super::flow_evaluator::frame;
    let mut hasher = Sha256::new();
    hasher.update(u64::MAX.to_le_bytes());
    frame(&mut hasher, tag);
    frame(&mut hasher, author);
    hasher.update((fields.len() as u64).to_le_bytes());
    for field in fields {
        match field {
            Some(value) => {
                hasher.update([1u8]);
                frame(&mut hasher, value);
            }
            None => hasher.update([0u8]),
        }
    }
    frame(&mut hasher, nonce);
    hex::encode(hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TAG: &str = "test-domain/v1";

    fn addr(fields: &[Option<&str>]) -> String {
        content_address(TAG, "did:key:alice", fields, "n1")
    }

    /// Every input moves the address: tag, author, each field, and the
    /// nonce. This is the whole point — a vote that covers the address
    /// covers all of them.
    #[test]
    fn every_input_is_covered() {
        let base = addr(&[Some("a"), Some("b")]);
        assert_eq!(base, addr(&[Some("a"), Some("b")]), "deterministic");
        assert_ne!(
            base,
            content_address("other/v1", "did:key:alice", &[Some("a"), Some("b")], "n1"),
            "tag"
        );
        assert_ne!(
            base,
            content_address(TAG, "did:key:mallory", &[Some("a"), Some("b")], "n1"),
            "author"
        );
        assert_ne!(base, addr(&[Some("a"), Some("x")]), "field value");
        assert_ne!(
            base,
            content_address(TAG, "did:key:alice", &[Some("a"), Some("b")], "n2"),
            "nonce"
        );
    }

    /// The presence byte: an absent field is not the empty string, and it is
    /// not droppable. Dropping `outputs_hash` from a proposal's preimage
    /// must change the URI, whatever the neighbouring fields hold.
    #[test]
    fn an_absent_field_is_distinct_and_stays_in_its_slot() {
        assert_ne!(addr(&[None]), addr(&[Some("")]));
        assert_ne!(addr(&[Some("a"), None]), addr(&[Some("a")]));
        assert_ne!(addr(&[None, Some("a")]), addr(&[Some("a"), None]));
    }

    /// Framing: field content cannot shift across a boundary, so two field
    /// lists whose concatenation agrees still address differently.
    #[test]
    fn field_boundaries_are_framed() {
        assert_ne!(
            addr(&[Some("ab"), Some("c")]),
            addr(&[Some("a"), Some("bc")])
        );
    }

    /// Domain separation against the existing flow hashes: the same strings
    /// can never produce an evidence seal or an outputs commitment. Both
    /// comparisons are by construction (sentinel + framed tag), and this
    /// pins it.
    #[test]
    fn a_content_address_is_never_an_evidence_seal_or_an_outputs_hash() {
        use crate::perspectives::flow_evaluator::{evidence_hash, EvidenceItem};
        let item = EvidenceItem {
            id: "id".into(),
            class_name: "class".into(),
            content: "{}".into(),
        };
        let seal = evidence_hash(&["class".to_string()], &[item.clone()]);
        let outputs = crate::perspectives::flow_instance::atom::outputs_hash(&[item]);
        let address = addr(&[Some("class"), Some("id"), Some("{}")]);
        assert_ne!(address, seal);
        assert_ne!(address, outputs);
    }
}
