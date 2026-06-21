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

/// Wake-19 E1: two op classes — `Ancestry` carries the diff payload and
/// rides the hash-derived location, `Head` is a tiny pointer to a
/// current-leaf Ancestry op that lives in a fixed location-zero sector
/// so every peer replicates it.
///
/// The op-class lives in the envelope so v1.5's sharded mode can route
/// the two classes differently without changing the data layer. v1 has
/// FULL arc everywhere so the routing distinction is a no-op today; the
/// `OpId::set_loc_callback` installed by the executor inspects the
/// op-id's trailing tag (`HEAD_OP_TAG` vs `ANCESTRY_OP_TAG`) and only
/// fires the loc-0 path on Head ops. v1.5 sharded peers see the same
/// scaffolding light up.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
pub enum OpClass {
    /// The diff payload op. Carries parents + diff bytes. v1's
    /// only-existing class before this PR; default for backward
    /// compatibility (legacy envelopes without an `op_class` field
    /// decode to this variant).
    #[default]
    Ancestry,
    /// Pointer-to-ancestry op announcing "this peer considers
    /// `head_pointer` the current head." Replicated FULL across the
    /// arc so every peer can answer `current_heads()` without a full
    /// op scan.
    Head,
}

/// Trailing-4-byte tag baked into the op-id so the K2 loc-callback can
/// route an op without decoding the envelope payload (LocCb only sees
/// the raw op-id bytes — see `kitsune2_api::id::LocCb`).
///
/// `ANCESTRY_OP_TAG` matches the legacy v1 tag (`[0xdb; 4]`) so the
/// op-ids of every existing Ancestry op stay identical across the
/// upgrade.
pub const ANCESTRY_OP_TAG: [u8; 4] = [0xdb, 0xdb, 0xdb, 0xdb];

/// Distinct trailer for Head ops. Pattern chosen for visual
/// distinguishability from `ANCESTRY_OP_TAG` in hex dumps. The
/// loc-callback only checks this exact byte pattern; nothing else
/// relies on the value.
pub const HEAD_OP_TAG: [u8; 4] = [0xa1, 0xa1, 0xa1, 0xa1];

/// Wake-19 E2 — `OpId::set_loc_callback` impl. Routes Head ops to
/// the fixed loc-0 sector (so every peer whose arc covers loc=0
/// replicates them) and falls back to K2's default xor-fold for
/// Ancestry ops (whose loc spreads across the ring naturally).
///
/// LocCb signature is `fn(&Bytes) -> u32`. It only ever sees the
/// raw op-id bytes — not the envelope payload — so the routing
/// decision has to be encoded in the op-id trailer. Op-ids are
/// 36 bytes (`SHA-256 + 4-byte tag`); we read bytes 32..36.
pub fn holograph_loc_callback(op_id_bytes: &bytes::Bytes) -> u32 {
    if op_id_bytes.len() >= 36 && op_id_bytes[32..36] == HEAD_OP_TAG {
        // Head ops live in the fixed loc-0 sector.
        return 0;
    }
    // Default xor-fold (matches K2's `default_loc`). Reimplemented
    // here so we don't have to reach into a `pub(crate)` symbol from
    // the K2 api crate.
    let mut out = [0u8; 4];
    for (i, b) in op_id_bytes.iter().enumerate() {
        out[i % 4] ^= b;
    }
    u32::from_le_bytes(out)
}

/// Install the holograph loc-callback into K2. Safe to call multiple
/// times (subsequent calls are no-ops; K2's `set_loc_callback` is a
/// one-shot OnceLock setter).
///
/// Returns `true` if this call won the OnceLock race (i.e., the
/// callback was just installed by us), `false` if K2 already had a
/// callback set — including by a previous call to this very function.
pub fn install_loc_callback() -> bool {
    kitsune2_api::OpId::set_loc_callback(holograph_loc_callback)
}

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
    /// Authoring timestamp in microseconds since Unix epoch — set by the
    /// creator at commit time, propagated unchanged so every peer derives
    /// the same `Timestamp` from the same envelope bytes. Defaults to 0
    /// for envelopes encoded before this field was added.
    #[serde(default)]
    pub created_at_micros: i64,
    /// Optional doc_id for multi-doc-per-space substrates. v1 leaves
    /// this `None`; v1.5 sharded mode populates it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub doc_id: Option<Bytes>,
    /// Wake-19 E1 — op class. `Ancestry` (the diff payload) or `Head`
    /// (pointer to a current-leaf Ancestry op). Defaults to `Ancestry`
    /// when decoding a legacy envelope without this field. The
    /// `skip_serializing_if = is_ancestry` keeps legacy envelopes
    /// byte-stable: an Ancestry envelope serialized by the new code
    /// hashes to the same bytes as one serialized by the old code
    /// (modulo the also-skip-on-default `head_pointer`).
    #[serde(default, skip_serializing_if = "OpClass::is_ancestry")]
    pub op_class: OpClass,
    /// Wake-19 E1 — only `Some` when `op_class == Head`. Names the
    /// Ancestry op-id this Head announces as the current leaf.
    /// `skip_serializing_if = is_none` keeps Ancestry envelopes
    /// byte-stable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub head_pointer: Option<Bytes>,
}

impl OpClass {
    /// Used by `skip_serializing_if` so a default-`Ancestry` envelope
    /// doesn't emit the `op_class` field — keeps the on-the-wire shape
    /// byte-stable with pre-Wake-19 envelopes.
    pub fn is_ancestry(&self) -> bool {
        matches!(self, OpClass::Ancestry)
    }
}

impl OpEnvelope {
    /// Build an Ancestry envelope from typed `OpId` parents. Wake-19
    /// E1: defaults `op_class = Ancestry`, `head_pointer = None`.
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
            created_at_micros: 0,
            doc_id,
            op_class: OpClass::Ancestry,
            head_pointer: None,
        }
    }

    /// Same as `new`, but with an explicit authoring timestamp.
    pub fn new_at(
        parents: impl IntoIterator<Item = OpId>,
        payload: Bytes,
        author_pubkey: Bytes,
        signature: Bytes,
        doc_id: Option<Bytes>,
        created_at_micros: i64,
    ) -> Self {
        Self {
            parents: parents.into_iter().map(Bytes::from).collect(),
            payload,
            author_pubkey,
            signature,
            created_at_micros,
            doc_id,
            op_class: OpClass::Ancestry,
            head_pointer: None,
        }
    }

    /// Wake-19 E1 — build a `Head` envelope pointing at `ancestry_op_id`.
    /// Payload is empty (the pointer + author + signature is the entire
    /// payload). Parents is empty because the Head is a free-standing
    /// announcement, not part of the diff DAG.
    pub fn new_head(
        ancestry_op_id: OpId,
        author_pubkey: Bytes,
        signature: Bytes,
        created_at_micros: i64,
    ) -> Self {
        Self {
            parents: Vec::new(),
            payload: Bytes::new(),
            author_pubkey,
            signature,
            created_at_micros,
            doc_id: None,
            op_class: OpClass::Head,
            head_pointer: Some(Bytes::from(ancestry_op_id)),
        }
    }

    /// View the `head_pointer` as a typed `OpId`. Returns `None` for
    /// Ancestry envelopes.
    pub fn head_pointer_op_id(&self) -> Option<OpId> {
        self.head_pointer.as_ref().map(|b| OpId::from(b.clone()))
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

    /// Wake-19 E1 + E5 — a pre-Wake-19 envelope (encoded with the old
    /// field set, *without* `op_class` or `head_pointer`) must decode
    /// to `op_class = Ancestry`, `head_pointer = None`.
    ///
    /// We construct the legacy bytes by serialising a private struct
    /// with only the pre-E1 fields so the test isn't entangled with
    /// the current OpEnvelope's serde shape.
    #[test]
    fn legacy_pre_class_envelope_decodes_as_ancestry() {
        #[derive(serde::Serialize)]
        struct LegacyOpEnvelope {
            parents: Vec<Bytes>,
            payload: Bytes,
            author_pubkey: Bytes,
            signature: Bytes,
            #[serde(default)]
            created_at_micros: i64,
            #[serde(skip_serializing_if = "Option::is_none")]
            doc_id: Option<Bytes>,
        }

        let legacy = LegacyOpEnvelope {
            parents: vec![Bytes::from_static(b"parent-id")],
            payload: Bytes::from_static(b"diff-bytes"),
            author_pubkey: Bytes::from_static(b"pk"),
            signature: Bytes::from_static(b"sg"),
            created_at_micros: 1_700_000_000_000_000,
            doc_id: None,
        };
        let mut buf = Vec::new();
        ciborium::into_writer(&legacy, &mut buf).expect("encode legacy");

        let decoded = OpEnvelope::decode(&buf).expect("decode legacy");
        assert_eq!(decoded.op_class, OpClass::Ancestry);
        assert!(decoded.head_pointer.is_none());
        assert!(decoded.head_pointer_op_id().is_none());
        assert_eq!(decoded.parents.len(), 1);
        assert_eq!(decoded.payload, Bytes::from_static(b"diff-bytes"));
    }

    /// Wake-19 E1 — a fresh-encoded Ancestry envelope is *byte-stable*
    /// against the legacy serialised form. This is what keeps Ancestry
    /// op-ids identical across the upgrade: the SHA-256 input doesn't
    /// change, so neither does the op-id.
    #[test]
    fn ancestry_envelope_bytes_match_legacy_shape() {
        #[derive(serde::Serialize)]
        struct LegacyOpEnvelope {
            parents: Vec<Bytes>,
            payload: Bytes,
            author_pubkey: Bytes,
            signature: Bytes,
            #[serde(default)]
            created_at_micros: i64,
            #[serde(skip_serializing_if = "Option::is_none")]
            doc_id: Option<Bytes>,
        }
        let legacy = LegacyOpEnvelope {
            parents: vec![Bytes::from_static(b"p")],
            payload: Bytes::from_static(b"x"),
            author_pubkey: Bytes::from_static(b"pk"),
            signature: Bytes::from_static(b"sg"),
            created_at_micros: 42,
            doc_id: None,
        };
        let mut legacy_buf = Vec::new();
        ciborium::into_writer(&legacy, &mut legacy_buf).expect("encode legacy");

        let modern = OpEnvelope::new_at(
            [opid(b"p")],
            Bytes::from_static(b"x"),
            Bytes::from_static(b"pk"),
            Bytes::from_static(b"sg"),
            None,
            42,
        );
        let modern_buf = modern.encode().expect("encode modern");

        assert_eq!(
            legacy_buf, modern_buf,
            "Ancestry envelope serialisation must stay byte-stable; \
             a divergence here means existing op-ids will change after \
             upgrading."
        );
    }

    /// Wake-19 E2 — `holograph_loc_callback` routes Head-tagged op-ids
    /// to loc=0 and falls through to the default xor-fold otherwise.
    #[test]
    fn loc_callback_routes_head_to_zero() {
        // Build an op-id whose trailing 4 bytes match `HEAD_OP_TAG`.
        let mut head_bytes = vec![0u8; 36];
        head_bytes[..32].copy_from_slice(&[7u8; 32]);
        head_bytes[32..].copy_from_slice(&HEAD_OP_TAG);
        let head_loc = holograph_loc_callback(&Bytes::from(head_bytes));
        assert_eq!(head_loc, 0, "Head ops route to loc=0");

        // Same payload bytes, Ancestry tag — should NOT be loc=0.
        let mut anc_bytes = vec![0u8; 36];
        anc_bytes[..32].copy_from_slice(&[7u8; 32]);
        anc_bytes[32..].copy_from_slice(&ANCESTRY_OP_TAG);
        let anc_loc = holograph_loc_callback(&Bytes::from(anc_bytes.clone()));
        assert_ne!(
            anc_loc, 0,
            "Ancestry ops fall through to xor-fold, which for non-zero \
             input shouldn't collide with 0"
        );

        // Sanity: xor-fold matches K2's default impl.
        let mut expected = [0u8; 4];
        for (i, b) in anc_bytes.iter().enumerate() {
            expected[i % 4] ^= b;
        }
        assert_eq!(anc_loc, u32::from_le_bytes(expected));
    }

    /// Wake-19 E1 — a Head envelope round-trips with `head_pointer`
    /// populated.
    #[test]
    fn head_envelope_round_trip() {
        let env = OpEnvelope::new_head(
            opid(b"target-ancestry-op"),
            Bytes::from_static(b"pk"),
            Bytes::from_static(b"sg"),
            1_700_000_000_000_000,
        );
        assert_eq!(env.op_class, OpClass::Head);
        let bytes = env.encode().expect("encode");
        let decoded = OpEnvelope::decode(&bytes).expect("decode");
        assert_eq!(decoded, env);
        assert_eq!(
            decoded.head_pointer_op_id().unwrap(),
            opid(b"target-ancestry-op")
        );
        // Sanity: head has no diff payload.
        assert!(decoded.payload.is_empty());
        assert!(decoded.parents.is_empty());
    }
}
