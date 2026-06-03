//! Substrate-agnostic mirrors of the p-diff-sync integrity-zome wire types.
//!
//! These types are byte-for-byte compatible with their counterparts in
//! `perspective_diff_sync_integrity` (same serde shape), but live in the
//! algorithm crate so the DAG-walk modules can manipulate them without
//! dragging in HDK / HDI / `holo_hash` / `SerializedBytes`.
//!
//! p-diff-sync provides `From<integrity::T>` / `Into<integrity::T>`
//! conversions at the HDK boundary. The algorithm operates on these
//! pure-serde types internally.
//!
//! Step 13a of the holograph spike: introduced as the foundation for
//! widening the Step 1.5 algorithm-crate extraction beyond `topo_sort`.

use serde::{Deserialize, Serialize};

/// Triple (source/target/predicate) carried by every link expression.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Triple {
    pub source: Option<String>,
    pub target: Option<String>,
    pub predicate: Option<String>,
}

/// Signature/key pair attached to expressions for AD4M's
/// expression-proof scheme.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

/// A single signed link expression — the atomic unit of a perspective.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct LinkExpression {
    pub author: String,
    pub data: Triple,
    pub timestamp: String,
    pub proof: ExpressionProof,
}

/// A diff between two perspective states: which links to add and remove.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

impl PerspectiveDiff {
    pub fn new() -> Self {
        Self::default()
    }

    /// Total number of additions + removals in this diff. Used by the
    /// chunking logic to know when to start a new chunk.
    pub fn total_diff_number(&self) -> usize {
        self.additions.len() + self.removals.len()
    }
}

/// 39-byte action-hash mirror. p-diff-sync uses
/// `HoloHash<holo_hash::hash_type::Action>` whose raw form is exactly 39
/// bytes; we keep the same width so conversions are byte-copies and so
/// the `NULL_NODE` sentinel keeps its 36-byte payload (`Vec<0xdb>` + the
/// HoloHash type/loc trailer in the integrity zome).
///
/// The integrity-zome wire shape preserves the trailing 3 bytes via
/// HoloHash's own `Serialize` impl; this mirror uses serde's standard
/// byte-array support for the same width.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Hash(#[serde(with = "serde_byte_array")] pub [u8; 39]);

impl Hash {
    pub fn from_raw_39(bytes: [u8; 39]) -> Self {
        Self(bytes)
    }

    /// Pack a 36-byte value with three trailing zero bytes — the shape
    /// `ActionHash::from_raw_36(v)` produces inside the integrity zome.
    pub fn from_raw_36(bytes_36: &[u8]) -> Self {
        assert_eq!(bytes_36.len(), 36, "from_raw_36 expects 36 bytes");
        let mut buf = [0u8; 39];
        buf[..36].copy_from_slice(bytes_36);
        Self(buf)
    }

    pub fn as_bytes(&self) -> &[u8; 39] {
        &self.0
    }
}

impl std::fmt::Debug for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Short hex prefix — full 39 bytes is noisy in test output and
        // p-diff-sync's existing Debug is via HoloHash::base64.
        write!(
            f,
            "Hash({:02x}{:02x}{:02x}…)",
            self.0[0], self.0[1], self.0[2]
        )
    }
}

impl std::fmt::Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for b in &self.0 {
            write!(f, "{:02x}", b)?;
        }
        Ok(())
    }
}

/// Reserved sentinel used by `Workspace::collect_until_common_ancestor`
/// when one side of a BFS reaches a chainless leaf and the other side
/// also reached a leaf — i.e. the two trees never share a real common
/// ancestor. Matches the integrity-zome `ActionHash::from_raw_36(vec![0xdb;36])`
/// byte pattern.
pub fn null_node() -> Hash {
    Hash::from_raw_36(&[0xdb; 36])
}

mod serde_byte_array {
    use serde::de::{Error, SeqAccess, Visitor};
    use serde::{Deserializer, Serializer};
    use std::fmt;

    pub fn serialize<S: Serializer>(bytes: &[u8; 39], ser: S) -> Result<S::Ok, S::Error> {
        // serde_bytes-style: emit as a byte array; falls back to a Vec
        // on text formats. Matches HoloHash's bincode/messagepack shape.
        ser.serialize_bytes(bytes)
    }

    struct BytesVisitor;

    impl<'de> Visitor<'de> for BytesVisitor {
        type Value = Vec<u8>;
        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("a byte sequence or array")
        }
        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E> {
            Ok(v.to_vec())
        }
        fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E> {
            Ok(v)
        }
        fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            let mut out = Vec::with_capacity(seq.size_hint().unwrap_or(39));
            while let Some(b) = seq.next_element::<u8>()? {
                out.push(b);
            }
            Ok(out)
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<[u8; 39], D::Error> {
        let v: Vec<u8> = de.deserialize_bytes(BytesVisitor)?;
        if v.len() != 39 {
            return Err(D::Error::custom(format!(
                "expected 39-byte Hash, got {}",
                v.len()
            )));
        }
        let mut buf = [0u8; 39];
        buf.copy_from_slice(&v);
        Ok(buf)
    }
}

/// Reference into the DAG: a diff (or pointer to one stored as
/// chunks) plus its parent hashes. Mirrors the integrity-zome
/// `PerspectiveDiffEntryReference`.
#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PerspectiveDiffEntryReference {
    pub diff: PerspectiveDiff,
    pub parents: Option<Vec<Hash>>,
    pub diffs_since_snapshot: usize,
    #[serde(default)]
    pub diff_chunks: Option<Vec<Hash>>,
}

impl PerspectiveDiffEntryReference {
    pub fn new(diff: PerspectiveDiff, parents: Option<Vec<Hash>>) -> Self {
        Self {
            diff,
            parents,
            diffs_since_snapshot: 0,
            diff_chunks: None,
        }
    }

    pub fn is_chunked(&self) -> bool {
        self.diff_chunks
            .as_ref()
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    }
}

/// Storage record that lets us skip-ahead a long DAG branch by checkpointing
/// every N diffs. Mirrors the integrity-zome `Snapshot`.
#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Snapshot {
    pub diff_chunks: Vec<Hash>,
    pub included_diffs: Vec<Hash>,
}

/// Implementation of the algorithm-crate-side `HasDiffParents<Hash>` for
/// the new mirror entry-reference type. This is what lets
/// `topo_sort_diff_references` chew on
/// `Vec<(Hash, PerspectiveDiffEntryReference)>` directly.
impl crate::HasDiffParents<Hash> for PerspectiveDiffEntryReference {
    fn parents(&self) -> Option<&[Hash]> {
        self.parents.as_deref()
    }
}
