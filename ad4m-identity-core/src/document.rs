//! The DID document of a log (section 9 of `docs-src/identity/did-scid-coasys-1.md`): a pure
//! function of the folded state at one seq. It reads no clock and does no I/O, so every
//! resolver (executor, host, browser) derives the same document from the same log.
//!
//! The document is time-free. An `Until` key's `expires` and a bot scope's `expires` are
//! properties of their verification methods (`coasys:expires`, `coasys:bot`), never a filter:
//! the document lists every key that [`KeyState::key_valid_at`] gives at the seq, and a
//! verifier compares an expiry with its own trusted time.
//!
//! The mapping:
//!
//! - every device key valid at the seq is a `Multikey` method `#<keyId>` (its signing key);
//!   a key that is not a bot's also has a `Multikey` method `#<keyId>-enc` (its X25519 key)
//!   listed in `keyAgreement`, where messages to the person are sealed;
//! - a standing or `Until` key is listed in `authentication` and `assertionMethod`;
//! - a bot key is listed only in `capabilityInvocation`, with the scope in force at the seq,
//!   so a generic verifier never takes a bot's signature as the person's;
//! - master keys are absent: they control the log, not the subject's statements;
//! - a revoked, replaced or ended key is absent from the seq that ended it;
//! - `controller` is the DID itself.

use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};

use crate::event::BotScope;
use crate::state::KeyRecord;
use crate::{KelChain, KeyState};

/// The DID Core v1 context.
pub const DID_CONTEXT: &str = "https://www.w3.org/ns/did/v1";
/// The Multikey context.
pub const MULTIKEY_CONTEXT: &str = "https://w3id.org/security/multikey/v1";
/// The `coasys/1` context: defines `coasys:bot` and `coasys:expires`.
pub const COASYS_CONTEXT: &str = "https://coasys.org/ns/did-scid-coasys/v1";
/// The `@context` of every document, in order.
pub const CONTEXTS: [&str; 3] = [DID_CONTEXT, MULTIKEY_CONTEXT, COASYS_CONTEXT];
/// The type of every verification method.
pub const MULTIKEY: &str = "Multikey";
/// The suffix of the id of a key's X25519 method: `#d1-enc`.
pub const ENC_SUFFIX: &str = "-enc";

/// A DID Core document. Serialises to JSON-LD with `@context` first and the members in the
/// order of the fields; `alsoKnownAs` is left out when empty, every relationship is always
/// written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DidDocument {
    pub id: String,
    pub controller: String,
    pub verification_method: Vec<VerificationMethod>,
    /// Relative method ids (`#d1`).
    pub authentication: Vec<String>,
    pub assertion_method: Vec<String>,
    pub key_agreement: Vec<String>,
    pub capability_invocation: Vec<String>,
    /// Other DID URLs of this DID (`<did>?src=<host>`), as the caller gives them.
    pub also_known_as: Vec<String>,
}

/// One verification method of a [`DidDocument`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VerificationMethod {
    /// `#d1` for a signing key, `#d1-enc` for its X25519 key.
    pub id: String,
    /// Always [`MULTIKEY`].
    pub type_: &'static str,
    pub controller: String,
    pub public_key_multibase: String,
    /// `coasys:expires`: the `expires` of a `KeyLife::Until` key.
    pub expires: Option<u64>,
    /// `coasys:bot`: the bot scope in force at the document's seq.
    pub bot: Option<BotScope>,
}

/// DID Core document metadata. `versionId` and `nextVersionId` are seqs, written as decimal
/// strings (DID Core makes both strings).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentMetadata {
    /// The seq whose state the document shows.
    pub version_id: u64,
    /// The seq after it, when the log holds one.
    pub next_version_id: Option<u64>,
    /// The log has ended (a `rotate` with `next: null`).
    pub deactivated: bool,
}

/// Why no document can be derived.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum DocumentError {
    #[error("VersionPastHead: version {version} is past the head {head}")]
    VersionPastHead { version: u64, head: u64 },
}

impl DocumentError {
    /// The stable name of the error.
    pub fn name(&self) -> &'static str {
        match self {
            DocumentError::VersionPastHead { .. } => "VersionPastHead",
        }
    }
}

/// The DID document of `chain` at `version` (the head when `None`), with `also_known_as` as
/// its `alsoKnownAs`. `deactivated` says whether the log has ended, whatever the version: it
/// is a property of the DID, while the methods are those of the chosen version.
pub fn did_document(
    chain: &KelChain,
    version: Option<u64>,
    also_known_as: &[String],
) -> Result<(DidDocument, DocumentMetadata), DocumentError> {
    let head = chain.head().seq;
    let seq = version.unwrap_or(head);
    if seq > head {
        return Err(DocumentError::VersionPastHead { version: seq, head });
    }
    let state = chain.state();
    let document = document_at(state, seq, also_known_as);
    let metadata = DocumentMetadata {
        version_id: seq,
        next_version_id: (seq < head).then_some(seq + 1),
        deactivated: state.ended,
    };
    Ok((document, metadata))
}

fn document_at(state: &KeyState, seq: u64, also_known_as: &[String]) -> DidDocument {
    let did = &state.did;
    let mut doc = DidDocument {
        id: did.clone(),
        controller: did.clone(),
        verification_method: Vec::new(),
        authentication: Vec::new(),
        assertion_method: Vec::new(),
        key_agreement: Vec::new(),
        capability_invocation: Vec::new(),
        also_known_as: also_known_as.to_vec(),
    };
    let mut keys: Vec<&KeyRecord> = state
        .keys
        .values()
        .filter(|r| state.key_valid_at(&r.id, seq))
        .collect();
    // Key ids are `d<n>` in order of addition (rule K7); a map of strings orders d10 before d2.
    keys.sort_by_key(|r| (device_number(&r.id), r.id.clone()));
    for record in keys {
        let id = format!("#{}", record.id);
        let bot = state.bot_scope_at(&record.id, seq).cloned();
        let is_bot = !record.scopes.is_empty();
        doc.verification_method.push(VerificationMethod {
            id: id.clone(),
            type_: MULTIKEY,
            controller: did.clone(),
            public_key_multibase: record.sign.clone(),
            expires: record.expires,
            bot,
        });
        if is_bot {
            doc.capability_invocation.push(id);
        } else {
            doc.authentication.push(id.clone());
            doc.assertion_method.push(id);
        }
        // A bot has no inbox: `keyAgreement` names where to seal messages to the person.
        if let Some(enc) = record.enc.as_ref().filter(|_| !is_bot) {
            let enc_id = format!("#{}{ENC_SUFFIX}", record.id);
            doc.verification_method.push(VerificationMethod {
                id: enc_id.clone(),
                type_: MULTIKEY,
                controller: did.clone(),
                public_key_multibase: enc.clone(),
                expires: None,
                bot: None,
            });
            doc.key_agreement.push(enc_id);
        }
    }
    doc
}

fn device_number(id: &str) -> u64 {
    id.strip_prefix('d')
        .and_then(|n| n.parse().ok())
        .unwrap_or(u64::MAX)
}

impl Serialize for DidDocument {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("@context", &CONTEXTS)?;
        map.serialize_entry("id", &self.id)?;
        map.serialize_entry("controller", &self.controller)?;
        map.serialize_entry("verificationMethod", &self.verification_method)?;
        map.serialize_entry("authentication", &self.authentication)?;
        map.serialize_entry("assertionMethod", &self.assertion_method)?;
        map.serialize_entry("keyAgreement", &self.key_agreement)?;
        map.serialize_entry("capabilityInvocation", &self.capability_invocation)?;
        if !self.also_known_as.is_empty() {
            map.serialize_entry("alsoKnownAs", &self.also_known_as)?;
        }
        map.end()
    }
}

impl Serialize for VerificationMethod {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("id", &self.id)?;
        map.serialize_entry("type", self.type_)?;
        map.serialize_entry("controller", &self.controller)?;
        map.serialize_entry("publicKeyMultibase", &self.public_key_multibase)?;
        if let Some(expires) = self.expires {
            map.serialize_entry("coasys:expires", &expires)?;
        }
        if let Some(bot) = &self.bot {
            map.serialize_entry("coasys:bot", bot)?;
        }
        map.end()
    }
}

impl Serialize for DocumentMetadata {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("versionId", &self.version_id.to_string())?;
        if let Some(next) = self.next_version_id {
            map.serialize_entry("nextVersionId", &next.to_string())?;
        }
        map.serialize_entry("deactivated", &self.deactivated)?;
        map.end()
    }
}
