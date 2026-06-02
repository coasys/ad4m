//! CBOR-encoded op envelope.
//!
//! The envelope is the on-the-wire shape of a perspective-diff op. Carries
//! the parent op-ids, the actual diff payload (opaque bytes here; decoded
//! by the algorithm crate), an author public key, a signature over the
//! parents+payload, and an optional `doc_id`.
//!
//! Wire encoding is CBOR via `ciborium`. Op-ids are serialized as raw byte
//! strings rather than going through `kitsune2_api::OpId`'s base64-string
//! serde — base64 forces ~33% bloat over raw bytes and (more importantly)
//! the K2 impl deserializes as a borrowed `&str`, which CBOR cannot supply.
//!
//! Sharding-ready commitments honored here (SPIKE §1.5):
//!
//! 3. `doc_id: Option<Bytes>` is present in v1 but always set to `None`.
//!    CBOR's `skip_serializing_if` keeps v1 envelopes from carrying the
//!    field at all, and `#[serde(default)]` keeps them decodable once
//!    v1.5 starts populating it.

use bytes::Bytes;
use kitsune2_api::OpId;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Errors that can come out of envelope encode/decode.
#[derive(Debug, Error)]
pub enum EnvelopeError {
    #[error("CBOR encoding failed: {0}")]
    Encode(String),
    #[error("CBOR decoding failed: {0}")]
    Decode(String),
}

/// The on-the-wire op envelope.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OpEnvelope {
    /// Parent op-ids in the diff DAG (raw bytes).
    pub parents: Vec<Bytes>,
    /// Opaque diff payload — the algorithm crate decodes this further.
    pub payload: Bytes,
    /// The author's public key (raw bytes — encoding scheme is the
    /// AD4M agent service's concern, not ours).
    pub author_pubkey: Bytes,
    /// Signature over `parents || payload || doc_id?`.
    pub signature: Bytes,
    /// Optional doc_id for multi-doc-per-space substrates. v1 leaves
    /// this `None`; v1.5 sharded mode populates it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub doc_id: Option<Bytes>,
}

impl OpEnvelope {
    /// Build an envelope from typed `OpId` parents.
    pub fn new(
        parents: impl IntoIterator<Item = OpId>,
        payload: Bytes,
        author_pubkey: Bytes,
        signature: Bytes,
        doc_id: Option<Bytes>,
    ) -> Self {
        Self {
            parents: parents.into_iter().map(Bytes::from).collect(),
            payload,
            author_pubkey,
            signature,
            doc_id,
        }
    }

    /// View parents as typed `OpId`s.
    pub fn parent_op_ids(&self) -> Vec<OpId> {
        self.parents.iter().cloned().map(OpId::from).collect()
    }

    /// Encode the envelope to CBOR bytes.
    pub fn encode(&self) -> Result<Vec<u8>, EnvelopeError> {
        let mut buf = Vec::new();
        ciborium::into_writer(self, &mut buf).map_err(|e| EnvelopeError::Encode(e.to_string()))?;
        Ok(buf)
    }

    /// Decode the envelope from CBOR bytes.
    pub fn decode(b: &[u8]) -> Result<Self, EnvelopeError> {
        ciborium::from_reader(b).map_err(|e| EnvelopeError::Decode(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn opid(b: &[u8]) -> OpId {
        OpId::from(Bytes::copy_from_slice(b))
    }

    #[test]
    fn round_trip_no_doc_id() {
        let env = OpEnvelope::new(
            [opid(b"parent-one"), opid(b"parent-two")],
            Bytes::from_static(b"diff-payload"),
            Bytes::from_static(b"pubkey"),
            Bytes::from_static(b"sig"),
            None,
        );
        let bytes = env.encode().expect("encode");
        let decoded = OpEnvelope::decode(&bytes).expect("decode");
        assert_eq!(env, decoded);
        assert_eq!(decoded.parent_op_ids().len(), 2);
        assert_eq!(decoded.parent_op_ids()[0], opid(b"parent-one"));
    }

    #[test]
    fn round_trip_with_doc_id() {
        let env = OpEnvelope::new(
            std::iter::empty(),
            Bytes::from_static(b"first"),
            Bytes::from_static(b"pubkey"),
            Bytes::from_static(b"sig"),
            Some(Bytes::from_static(b"doc-42")),
        );
        let bytes = env.encode().expect("encode");
        let decoded = OpEnvelope::decode(&bytes).expect("decode");
        assert_eq!(env, decoded);
        assert_eq!(decoded.doc_id.as_deref(), Some(&b"doc-42"[..]));
    }

    /// An envelope encoded without `doc_id` must remain decodable when v1.5
    /// starts populating the field — i.e. `doc_id` must be optional at the
    /// CBOR level, not just at the Rust level.
    #[test]
    fn legacy_envelope_without_doc_id_decodes() {
        let env_v1 = OpEnvelope::new(
            [opid(b"p")],
            Bytes::from_static(b"x"),
            Bytes::from_static(b"pk"),
            Bytes::from_static(b"sg"),
            None,
        );
        let bytes = env_v1.encode().expect("encode");
        let decoded = OpEnvelope::decode(&bytes).expect("decode");
        assert!(decoded.doc_id.is_none());
    }

    /// Garbage bytes should produce a decode error, not a panic.
    #[test]
    fn malformed_bytes_error() {
        let result = OpEnvelope::decode(&[0xff, 0x00, 0x42]);
        assert!(matches!(result, Err(EnvelopeError::Decode(_))));
    }
}
