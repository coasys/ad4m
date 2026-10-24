//! `ad4m-hash-chain`: append-only, linear hash chains of signed JSON entries.
//!
//! The module knows bytes, hashes, signatures, order and forks. It does not know what an
//! entry means: a chain type gives the meaning through [`ChainPolicy`]. The core does no
//! I/O, reads no clock and draws no randomness; storage adapters sit behind the `store` and
//! `sqlite` features. The key log built on it: `docs-src/identity/did-scid-coasys-1.md`.

use serde::de::DeserializeOwned;
use serde::ser::SerializeMap;
use serde::{Deserialize, Serialize, Serializer};
use serde_json::{json, Value};
use std::fmt;

mod codec;
mod jcs;
mod proof;
#[cfg(feature = "store")]
pub mod store;
#[cfg(feature = "wasm")]
pub mod wasm;

pub use codec::{
    decode_ed25519_multikey, decode_hash, decode_proof_value, encode_ed25519_multikey, encode_hash,
    encode_proof_value, hash_bytes, sha256, Hash, HashError, Multikey, ED25519_PUB, SHA2_256,
};
pub use jcs::{canonical_json, CanonError, MAX_SAFE_INTEGER};
pub use proof::{
    signing_input, verify_proof, verify_proof_json, Proof, ProofError, CRYPTOSUITE, PROOF_TYPE,
};

/// The id of one chain, given by the chain type: the KEL uses the SCID.
pub type ChainId = String;
/// The name of a chain type (`"coasys:kel"`); lets one database hold several chain types.
pub type ChainType = &'static str;

/// An entry is a JSON object with at least `seq` (integer), `prev` (hash or null) and
/// `proof` (array; a type may leave it out when empty). Every other field belongs to the
/// chain type and enters the hash.
pub trait Entry: Serialize + DeserializeOwned + Clone {
    fn seq(&self) -> u64;
    fn prev(&self) -> Option<&Hash>;
    fn proofs(&self) -> &[Proof];
}

/// `multihash_b58(sha256(JCS(entry without "proof")))`. Fails when the entry has no
/// canonical form (a float, an integer outside ±(2^53 − 1), or not an object).
pub fn entry_hash<E: Entry>(entry: &E) -> Result<Hash, CanonError> {
    let value = serde_json::to_value(entry).map_err(|e| CanonError::Serde(e.to_string()))?;
    entry_hash_json(&value)
}

/// [`entry_hash`] over an entry given as JSON (any object: the module reads no field).
pub fn entry_hash_json(entry: &Value) -> Result<Hash, CanonError> {
    let map = entry.as_object().ok_or(CanonError::NotObject)?;
    Ok(hash_bytes(
        jcs::canonical_object_without(map, "proof")?.as_bytes(),
    ))
}

/// The JCS text of a whole entry, proofs included: the form on the wire and on disk.
pub fn entry_json<E: Entry>(entry: &E) -> Result<String, CanonError> {
    let value = serde_json::to_value(entry).map_err(|e| CanonError::Serde(e.to_string()))?;
    if !value.is_object() {
        return Err(CanonError::NotObject);
    }
    canonical_json(&value)
}

/// The rules of one chain type. The module calls it after the order and link checks pass.
pub trait ChainPolicy {
    type Entry: Entry;
    type State: Clone;
    type Error: std::error::Error;
    const TYPE: ChainType;
    const DOMAIN: &'static str;
    /// Checks entry 0 (its id rule, its proofs, who signed it) and gives the chain id and the first state.
    fn genesis(&self, entry: &Self::Entry) -> Result<(ChainId, Self::State), Self::Error>;
    /// The chain id that a later entry names (the KEL: `scid`).
    fn chain_of(&self, entry: &Self::Entry) -> ChainId;
    /// Checks one later entry against the state before it: who may sign it and what it may do.
    fn apply(&self, state: &mut Self::State, entry: &Self::Entry) -> Result<(), Self::Error>;
}

/// The last entry of a chain.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Head {
    pub seq: u64,
    pub hash: Hash,
}

/// Why an entry or a list of entries does not continue a chain.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ChainError<E> {
    Seq { want: u64, got: u64 },
    Prev { want: Hash, got: Option<Hash> },
    WrongChain { want: ChainId, got: ChainId },
    Empty,
    Malformed(String),
    Policy(E),
}

impl<E: fmt::Display> fmt::Display for ChainError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChainError::Seq { want, got } => write!(f, "seq {got}, expected {want}"),
            ChainError::Prev { want, got } => write!(f, "prev {got:?}, expected {want}"),
            ChainError::WrongChain { want, got } => {
                write!(f, "entry of chain {got}, expected {want}")
            }
            ChainError::Empty => write!(f, "no entries"),
            ChainError::Malformed(reason) => write!(f, "malformed entry: {reason}"),
            ChainError::Policy(e) => write!(f, "{e}"),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for ChainError<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ChainError::Policy(e) => Some(e),
            _ => None,
        }
    }
}

impl<E: fmt::Display> ChainError<E> {
    /// The TypeScript `ChainErrorCode` of this error (the JSON the wasm exports throw).
    pub fn code(&self) -> Value {
        match self {
            ChainError::Seq { want, got } => json!({"code": "seq", "want": want, "got": got}),
            ChainError::Prev { want, got } => json!({"code": "prev", "want": want, "got": got}),
            ChainError::WrongChain { want, got } => {
                json!({"code": "wrong-chain", "want": want, "got": got})
            }
            ChainError::Empty => json!({"code": "empty"}),
            ChainError::Malformed(reason) => json!({"code": "malformed", "reason": reason}),
            ChainError::Policy(e) => json!({"code": "policy", "reason": e.to_string()}),
        }
    }
}

impl<E> From<CanonError> for ChainError<E> {
    fn from(e: CanonError) -> Self {
        ChainError::Malformed(e.to_string())
    }
}

/// One linear chain: its id, the hash at each seq, and the chain type's state after the head.
pub struct Chain<P: ChainPolicy> {
    id: ChainId,
    hashes: Vec<Hash>,
    state: P::State,
}

impl<P: ChainPolicy> Clone for Chain<P> {
    fn clone(&self) -> Self {
        Chain {
            id: self.id.clone(),
            hashes: self.hashes.clone(),
            state: self.state.clone(),
        }
    }
}

impl<P: ChainPolicy> fmt::Debug for Chain<P>
where
    P::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chain")
            .field("type", &P::TYPE)
            .field("id", &self.id)
            .field("hashes", &self.hashes)
            .field("state", &self.state)
            .finish()
    }
}

impl<P: ChainPolicy> Chain<P> {
    /// Starts a chain at entry 0: `seq` 0, `prev` null, then `policy.genesis`.
    pub fn genesis(policy: &P, entry: &P::Entry) -> Result<Self, ChainError<P::Error>> {
        let hash = checked_hash(entry)?;
        if entry.seq() != 0 {
            return Err(ChainError::Seq {
                want: 0,
                got: entry.seq(),
            });
        }
        if let Some(prev) = entry.prev() {
            return Err(ChainError::Malformed(format!(
                "entry 0 names prev {prev}; it must be null"
            )));
        }
        let (id, state) = policy.genesis(entry).map_err(ChainError::Policy)?;
        let named = policy.chain_of(entry);
        if named != id {
            return Err(ChainError::WrongChain {
                want: id,
                got: named,
            });
        }
        Ok(Chain {
            id,
            hashes: vec![hash],
            state,
        })
    }

    /// Appends one entry: seq = head + 1, prev = head hash, same chain id, then `policy.apply`
    /// on a copy of the state. Nothing changes when a check fails.
    pub fn append(&mut self, policy: &P, entry: &P::Entry) -> Result<Head, ChainError<P::Error>> {
        let mut state = self.state.clone();
        let head = self.head();
        let hash = step(policy, &self.id, &head, &mut state, entry)?;
        self.state = state;
        self.hashes.push(hash.clone());
        Ok(Head {
            seq: head.seq + 1,
            hash,
        })
    }

    pub fn id(&self) -> &ChainId {
        &self.id
    }

    pub fn head(&self) -> Head {
        Head {
            seq: (self.hashes.len() - 1) as u64,
            hash: self.hashes[self.hashes.len() - 1].clone(),
        }
    }

    pub fn hash_at(&self, seq: u64) -> Option<&Hash> {
        usize::try_from(seq).ok().and_then(|i| self.hashes.get(i))
    }

    /// The hash at every seq from 0 to the head, for [`compare`].
    pub fn hashes(&self) -> &[Hash] {
        &self.hashes
    }

    pub fn state(&self) -> &P::State {
        &self.state
    }
}

/// The entry's hash, after checking that its JSON is an object whose `seq` and `prev`
/// members agree with the `Entry` accessors and whose `prev` is a sha2-256 multihash.
fn checked_hash<E: Entry, X>(entry: &E) -> Result<Hash, ChainError<X>> {
    let value = serde_json::to_value(entry).map_err(|e| ChainError::Malformed(e.to_string()))?;
    let map = value
        .as_object()
        .ok_or_else(|| ChainError::Malformed("an entry must be a JSON object".into()))?;
    if map.get("seq").and_then(Value::as_u64) != Some(entry.seq()) {
        return Err(ChainError::Malformed(
            "the entry's `seq` member is missing or differs from Entry::seq".into(),
        ));
    }
    match (map.get("prev"), entry.prev()) {
        (Some(Value::Null), None) => {}
        (Some(Value::String(s)), Some(p)) if s == p => {
            decode_hash(p).map_err(|e| ChainError::Malformed(format!("prev {p}: {e}")))?;
        }
        _ => {
            return Err(ChainError::Malformed(
                "the entry's `prev` member is missing or differs from Entry::prev".into(),
            ))
        }
    }
    if !matches!(map.get("proof"), None | Some(Value::Array(_))) {
        return Err(ChainError::Malformed(
            "the entry's `proof` member must be an array".into(),
        ));
    }
    Ok(entry_hash_json(&value)?)
}

/// Checks one entry after `head` (order, link, chain id, then the policy) on `state`.
fn step<P: ChainPolicy>(
    policy: &P,
    id: &ChainId,
    head: &Head,
    state: &mut P::State,
    entry: &P::Entry,
) -> Result<Hash, ChainError<P::Error>> {
    let hash = checked_hash(entry)?;
    let want = head.seq + 1;
    if entry.seq() != want {
        return Err(ChainError::Seq {
            want,
            got: entry.seq(),
        });
    }
    if entry.prev() != Some(&head.hash) {
        return Err(ChainError::Prev {
            want: head.hash.clone(),
            got: entry.prev().cloned(),
        });
    }
    let named = policy.chain_of(entry);
    if &named != id {
        return Err(ChainError::WrongChain {
            want: id.clone(),
            got: named,
        });
    }
    policy.apply(state, entry).map_err(ChainError::Policy)?;
    Ok(hash)
}

/// Replays `entries` in the order given: `genesis` on the first, `append` on each other.
pub fn fold<P: ChainPolicy>(
    policy: &P,
    entries: &[P::Entry],
) -> Result<Chain<P>, ChainError<P::Error>> {
    let (first, rest) = entries.split_first().ok_or(ChainError::Empty)?;
    let mut chain = Chain::genesis(policy, first)?;
    for entry in rest {
        chain.append(policy, entry)?;
    }
    Ok(chain)
}

/// Two different entries at one seq of one chain. The module keeps both and picks neither.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ForkEvidence {
    pub chain: ChainId,
    pub seq: u64,
    pub ours: Hash,
    pub theirs: Hash,
}

/// How a peer's hashes relate to ours.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compare {
    /// The same hashes.
    Same,
    /// They hold every hash we hold, and more from seq `from` on.
    TheyExtend { from: u64 },
    /// We hold every hash they hold, and more from seq `from` on. Never a reason to truncate.
    WeExtend { from: u64 },
    /// The first seq where the hashes differ.
    Fork(ForkEvidence),
}

/// Compares our hashes with a peer's hashes from position `from` on: `ours[i]` and
/// `theirs[i]` are both the hash at seq `from + i`. Never truncates.
pub fn compare(ours: &[Hash], theirs: &[Hash], from: u64, chain: &ChainId) -> Compare {
    for (i, (a, b)) in ours.iter().zip(theirs).enumerate() {
        if a != b {
            return Compare::Fork(ForkEvidence {
                chain: chain.clone(),
                seq: from + i as u64,
                ours: a.clone(),
                theirs: b.clone(),
            });
        }
    }
    match ours.len().cmp(&theirs.len()) {
        std::cmp::Ordering::Equal => Compare::Same,
        std::cmp::Ordering::Less => Compare::TheyExtend {
            from: from + ours.len() as u64,
        },
        std::cmp::Ordering::Greater => Compare::WeExtend {
            from: from + theirs.len() as u64,
        },
    }
}

/// What [`import`] did with a batch.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportOutcome {
    /// New entries continued our head.
    Appended { head: Head, count: u32 },
    /// Every entry matches what we hold.
    AlreadyHave,
    /// The first new entry does not continue our head: fetch from `have + 1`.
    Gap { have: u64, first: u64 },
    /// An entry differs at a position we hold; nothing appended.
    Fork(ForkEvidence),
}

/// Imports a batch of entries of `chain`. `entries` may arrive in any order within one
/// call; the module sorts them by `seq`. Nothing changes unless the outcome is `Appended`.
///
/// Checks, in order: every entry has a canonical form and names this chain; an entry at a
/// seq we hold has the hash we hold (else `Fork`); the new entries start at head + 1 (else
/// `Gap`); then each new entry passes the order, link and policy checks of `append`. Two
/// different entries at one new seq within the batch are `Malformed`.
pub fn import<P: ChainPolicy>(
    chain: &mut Chain<P>,
    policy: &P,
    entries: &[P::Entry],
) -> Result<ImportOutcome, ChainError<P::Error>> {
    let mut items: Vec<(u64, Hash, &P::Entry)> = Vec::with_capacity(entries.len());
    for entry in entries {
        let hash = checked_hash(entry)?;
        let named = policy.chain_of(entry);
        if named != chain.id {
            return Err(ChainError::WrongChain {
                want: chain.id.clone(),
                got: named,
            });
        }
        items.push((entry.seq(), hash, entry));
    }
    items.sort_by_key(|(seq, _, _)| *seq);

    let head = chain.head();
    let mut new: Vec<(u64, Hash, &P::Entry)> = Vec::new();
    for (seq, hash, entry) in items {
        if seq <= head.seq {
            let ours = &chain.hashes[seq as usize];
            if *ours != hash {
                return Ok(ImportOutcome::Fork(ForkEvidence {
                    chain: chain.id.clone(),
                    seq,
                    ours: ours.clone(),
                    theirs: hash,
                }));
            }
            continue;
        }
        match new.last() {
            Some((last_seq, last_hash, _)) if *last_seq == seq => {
                if *last_hash != hash {
                    return Err(ChainError::Malformed(format!(
                        "two different entries at seq {seq} in one batch"
                    )));
                }
            }
            _ => new.push((seq, hash, entry)),
        }
    }

    let Some((first, _, _)) = new.first() else {
        return Ok(ImportOutcome::AlreadyHave);
    };
    if *first != head.seq + 1 {
        return Ok(ImportOutcome::Gap {
            have: head.seq,
            first: *first,
        });
    }

    let mut state = chain.state.clone();
    let mut at = head;
    let mut added = Vec::with_capacity(new.len());
    for (_, _, entry) in &new {
        let hash = step(policy, &chain.id, &at, &mut state, entry)?;
        at = Head {
            seq: at.seq + 1,
            hash: hash.clone(),
        };
        added.push(hash);
    }
    let count = u32::try_from(added.len()).unwrap_or(u32::MAX);
    chain.state = state;
    chain.hashes.extend(added);
    Ok(ImportOutcome::Appended { head: at, count })
}

// The TypeScript shapes the wasm exports return: `{ kind: "same" }`, `{ kind: "fork", fork }`, …

impl Serialize for Compare {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        match self {
            Compare::Same => map.serialize_entry("kind", "same")?,
            Compare::TheyExtend { from } => {
                map.serialize_entry("kind", "they-extend")?;
                map.serialize_entry("from", from)?;
            }
            Compare::WeExtend { from } => {
                map.serialize_entry("kind", "we-extend")?;
                map.serialize_entry("from", from)?;
            }
            Compare::Fork(fork) => {
                map.serialize_entry("kind", "fork")?;
                map.serialize_entry("fork", fork)?;
            }
        }
        map.end()
    }
}

impl Serialize for ImportOutcome {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        match self {
            ImportOutcome::Appended { head, count } => {
                map.serialize_entry("kind", "appended")?;
                map.serialize_entry("head", head)?;
                map.serialize_entry("count", count)?;
            }
            ImportOutcome::AlreadyHave => map.serialize_entry("kind", "already-have")?,
            ImportOutcome::Gap { have, first } => {
                map.serialize_entry("kind", "gap")?;
                map.serialize_entry("have", have)?;
                map.serialize_entry("first", first)?;
            }
            ImportOutcome::Fork(fork) => {
                map.serialize_entry("kind", "fork")?;
                map.serialize_entry("fork", fork)?;
            }
        }
        map.end()
    }
}
