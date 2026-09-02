use crate::types::Expression;
use chrono::SecondsFormat;
use chrono::{DateTime, Utc};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, PatchedKeyPair};
use log::error;
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::str::FromStr;
use std::sync::{Arc, RwLock};

// ─── DID method dispatch (P1.2) ──────────────────────────────────────────────

/// The DID methods the verifier distinguishes. `did:key` (and any other legacy
/// identifier) keeps the original, self-contained verification path; `did:scid`
/// resolves its key state from a key-event log through a [`KeyStateResolver`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DidMethod {
    Key,
    Scid,
    /// Anything else — verified on the legacy key-method path for backward compatibility.
    Other,
}

/// Classify a DID by its method prefix.
pub fn did_method(did: &str) -> DidMethod {
    if did.starts_with("did:scid:") {
        DidMethod::Scid
    } else if did.starts_with("did:key:") {
        DidMethod::Key
    } else {
        DidMethod::Other
    }
}

// ─── Key-state resolution seam (P1.2) ────────────────────────────────────────

/// A verification method usable to check a signature: an id plus the public key
/// encoded in `did:key` form, so the proven key-method crypto verifies it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerificationMethod {
    pub id: String,
    /// The verification key encoded as a `did:key` string.
    pub key: String,
}

/// The set of keys authoritative for a DID at a point in its key-event log.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct KeyState {
    pub keys: Vec<VerificationMethod>,
}

/// Why a `did:scid` resolution failed. Every variant verifies as "not valid"
/// (fail closed) — a resolution failure never counts as a passing signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    NotFound,
    Deactivated,
    Backend(String),
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::NotFound => write!(f, "identifier not found"),
            ResolveError::Deactivated => write!(f, "identifier deactivated"),
            ResolveError::Backend(e) => write!(f, "resolver backend error: {}", e),
        }
    }
}

/// Resolves `did:scid` identifiers to their key state. PR 3 installs the real,
/// agent-language-backed implementation via [`set_key_state_resolver`]; until
/// then the holder stays empty and `did:scid` verification fails closed.
pub trait KeyStateResolver: Send + Sync {
    /// Key state authoritative at a specific key-event-log sequence.
    fn key_state_at(&self, did: &str, kel_seq: u64) -> Result<KeyState, ResolveError>;
    /// Current key state (head of the key-event log).
    fn current_key_state(&self, did: &str) -> Result<KeyState, ResolveError>;
}

static RESOLVER: RwLock<Option<Arc<dyn KeyStateResolver>>> = RwLock::new(None);

/// Install the process-wide `did:scid` key-state resolver. Called once at boot
/// (PR 3). Idempotent; a later call replaces the resolver.
pub fn set_key_state_resolver(resolver: Arc<dyn KeyStateResolver>) {
    if let Ok(mut guard) = RESOLVER.write() {
        *guard = Some(resolver);
    }
}

fn key_state_resolver() -> Option<Arc<dyn KeyStateResolver>> {
    RESOLVER.read().ok().and_then(|g| g.clone())
}

// ─── Public verify API ───────────────────────────────────────────────────────

pub fn verify_string_signed_by_did(
    did: &str,
    data: &str,
    signed_data: &str,
) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(signed_data)?;
    let message = hash_message(&data.to_string());
    // A raw signed string carries no proof envelope, so there are no key_id /
    // kel_seq hints; a did:scid signer resolves against current key state.
    Ok(inner_verify(did, &message, &sig_bytes, None, None))
}

pub fn verify<T: Serialize>(expr: &Expression<T>) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(&expr.proof.signature)?;
    let timestamp = DateTime::<Utc>::from_str(&expr.timestamp).map_err(|e| {
        anyhow!(
            "Failed to parse timestamp when trying to verify signature: {}",
            e
        )
    })?;
    let message = hash_data_and_timestamp(&expr.data, &timestamp);
    let result = inner_verify(
        &expr.author,
        &message,
        &sig_bytes,
        expr.proof.key_id.as_deref(),
        expr.proof.kel_seq,
    );
    Ok(result)
}

pub(super) fn hash_data_and_timestamp<T: Serialize>(
    data: &T,
    timestamp: &DateTime<Utc>,
) -> Vec<u8> {
    let mut hasher = Sha256::new();

    // Serialize and hash the data directly.
    let serialized_data = serde_json::to_vec(data).expect("Failed to serialize data");
    hasher.update(&serialized_data);

    // Serialize and hash the timestamp.
    let timestamp_str = timestamp.to_rfc3339_opts(SecondsFormat::Millis, true);
    hasher.update(timestamp_str.as_bytes());

    // Finalize the hash and return the result.
    hasher.finalize().as_slice().into()
}

pub(super) fn hash_message(message: &String) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(message.as_bytes());
    hasher.finalize().as_slice().into()
}

// ─── inner_verify: dual-path dispatch (P1.3) ─────────────────────────────────

fn inner_verify(
    did: &str,
    message: &[u8],
    signature: &[u8],
    key_id: Option<&str>,
    kel_seq: Option<u64>,
) -> bool {
    match did_method(did) {
        DidMethod::Scid => {
            let resolver = key_state_resolver();
            verify_scid_with(
                resolver.as_deref(),
                did,
                message,
                signature,
                key_id,
                kel_seq,
            )
        }
        // did:key and any other legacy identifier keep the exact original behaviour.
        DidMethod::Key | DidMethod::Other => verify_key_method(did, message, signature),
    }
}

/// The unchanged legacy path: decode the verification key straight from the DID
/// string and check the signature. Byte-identical to the pre-refactor code.
fn verify_key_method(did: &str, message: &[u8], signature: &[u8]) -> bool {
    if let Ok(key_pair) = PatchedKeyPair::try_from(did) {
        match key_pair.verify(message, signature) {
            Ok(_) => true,
            Err(_) => {
                //debug!("Signature verification failed: {:?}", e);
                false
            }
        }
    } else {
        error!("Failed to parse DID as key method: {}", did);
        false
    }
}

/// The `did:scid` path: resolve the key state (at the anchored sequence, or the
/// current head) and verify against the authorised key. Fails closed when no
/// resolver is installed or resolution fails — a resolution failure never
/// counts as a valid signature. The resolver is trait-abstracted, so this
/// function performs no network I/O of its own.
fn verify_scid_with(
    resolver: Option<&dyn KeyStateResolver>,
    did: &str,
    message: &[u8],
    signature: &[u8],
    key_id: Option<&str>,
    kel_seq: Option<u64>,
) -> bool {
    let resolver = match resolver {
        Some(r) => r,
        None => {
            error!(
                "No did:scid key-state resolver installed; cannot verify {}",
                did
            );
            return false;
        }
    };
    let key_state = match kel_seq {
        Some(seq) => resolver.key_state_at(did, seq),
        None => resolver.current_key_state(did),
    };
    let key_state = match key_state {
        Ok(ks) => ks,
        Err(e) => {
            error!("Failed to resolve key state for {}: {}", did, e);
            return false;
        }
    };
    // Prefer the named verification method; if none is named, try each
    // authorised key in the state.
    key_state
        .keys
        .iter()
        .filter(|vm| key_id.map_or(true, |kid| vm.id == kid))
        .any(|vm| verify_key_method(&vm.key, message, signature))
}

// ─── tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::core::{Expression, ExpressionProof};
    use did_key::{generate, DIDCore, Ed25519KeyPair};

    /// A fixture resolver that answers for one SCID and errors for everything
    /// else — enough to exercise the did:scid verification path in isolation.
    struct FixtureResolver {
        did: String,
        state: KeyState,
    }
    impl KeyStateResolver for FixtureResolver {
        fn key_state_at(&self, did: &str, _seq: u64) -> Result<KeyState, ResolveError> {
            if did == self.did {
                Ok(self.state.clone())
            } else {
                Err(ResolveError::NotFound)
            }
        }
        fn current_key_state(&self, did: &str) -> Result<KeyState, ResolveError> {
            if did == self.did {
                Ok(self.state.clone())
            } else {
                Err(ResolveError::NotFound)
            }
        }
    }

    /// A fresh Ed25519 keypair plus its `did:key` string.
    fn keypair() -> (PatchedKeyPair, String) {
        let kp = generate::<Ed25519KeyPair>(None);
        let did = kp.get_did_document(did_key::Config::default()).id;
        (kp, did)
    }

    #[test]
    fn did_method_classifies() {
        assert_eq!(did_method("did:key:z6MkExample"), DidMethod::Key);
        assert_eq!(did_method("did:scid:ke:1:EExample"), DidMethod::Scid);
        assert_eq!(did_method("did:web:example.com"), DidMethod::Other);
    }

    // MUST verify every legacy did:key proof identically to the pre-refactor
    // path (valid accepts, tampered rejects); a did:key proof carries no
    // key_id / kel_seq and falls back to the legacy path.
    #[test]
    fn legacy_did_key_verifies_and_rejects() {
        let (kp, did) = keypair();
        let sig = kp.sign(&hash_message(&"hello world".to_string()));
        assert!(verify_string_signed_by_did(&did, "hello world", &hex::encode(&sig)).unwrap());
        assert!(!verify_string_signed_by_did(&did, "tampered", &hex::encode(&sig)).unwrap());
    }

    // The same via a full Expression proof (author = did:key, no key_id).
    #[test]
    fn legacy_did_key_expression_verifies() {
        let (kp, did) = keypair();
        let timestamp = "2026-09-02T00:00:00.000Z";
        let data = "payload".to_string();
        let ts = DateTime::<Utc>::from_str(timestamp).unwrap();
        let sig = kp.sign(&hash_data_and_timestamp(&data, &ts));
        let expr = Expression {
            author: did,
            timestamp: timestamp.to_string(),
            data,
            proof: ExpressionProof {
                key: String::new(),
                signature: hex::encode(&sig),
                key_id: None,
                kel_seq: None,
            },
        };
        assert!(verify(&expr).unwrap());
    }

    // MUST return false when no resolver is installed (fail closed).
    #[test]
    fn scid_without_resolver_fails_closed() {
        let msg = hash_message(&"x".to_string());
        assert!(!verify_scid_with(
            None,
            "did:scid:ke:1:EAbc",
            &msg,
            &[0u8; 64],
            None,
            Some(0)
        ));
    }

    // MUST accept a did:scid proof whose key_id held authority at kel_seq.
    #[test]
    fn scid_accepts_valid_from_fixture() {
        let (kp, key_did) = keypair();
        let scid = "did:scid:ke:1:EAgent".to_string();
        let vm_id = format!("{}#key-0", scid);
        let resolver = FixtureResolver {
            did: scid.clone(),
            state: KeyState {
                keys: vec![VerificationMethod {
                    id: vm_id.clone(),
                    key: key_did,
                }],
            },
        };
        let msg = hash_message(&"scid-signed".to_string());
        let sig = kp.sign(&msg);
        assert!(verify_scid_with(
            Some(&resolver),
            &scid,
            &msg,
            &sig,
            Some(&vm_id),
            Some(0)
        ));
    }

    // MUST reject a did:scid proof whose key_id names no key in the state.
    #[test]
    fn scid_rejects_unknown_key_id() {
        let (kp, key_did) = keypair();
        let scid = "did:scid:ke:1:EAgent".to_string();
        let resolver = FixtureResolver {
            did: scid.clone(),
            state: KeyState {
                keys: vec![VerificationMethod {
                    id: format!("{}#key-0", scid),
                    key: key_did,
                }],
            },
        };
        let msg = hash_message(&"scid-signed".to_string());
        let sig = kp.sign(&msg);
        assert!(!verify_scid_with(
            Some(&resolver),
            &scid,
            &msg,
            &sig,
            Some(&format!("{}#key-99", scid)),
            Some(0)
        ));
    }

    // MUST fail closed when resolution errors (unknown SCID).
    #[test]
    fn scid_resolve_error_fails_closed() {
        let (kp, key_did) = keypair();
        let resolver = FixtureResolver {
            did: "did:scid:ke:1:EKnown".to_string(),
            state: KeyState {
                keys: vec![VerificationMethod {
                    id: "id".to_string(),
                    key: key_did,
                }],
            },
        };
        let msg = hash_message(&"x".to_string());
        let sig = kp.sign(&msg);
        assert!(!verify_scid_with(
            Some(&resolver),
            "did:scid:ke:1:EUnknown",
            &msg,
            &sig,
            None,
            Some(0)
        ));
    }

    // A tampered message MUST fail even against the correct key.
    #[test]
    fn scid_rejects_tampered_message() {
        let (kp, key_did) = keypair();
        let scid = "did:scid:ke:1:EAgent".to_string();
        let vm_id = format!("{}#key-0", scid);
        let resolver = FixtureResolver {
            did: scid.clone(),
            state: KeyState {
                keys: vec![VerificationMethod {
                    id: vm_id.clone(),
                    key: key_did,
                }],
            },
        };
        let sig = kp.sign(&hash_message(&"original".to_string()));
        let tampered = hash_message(&"tampered".to_string());
        assert!(!verify_scid_with(
            Some(&resolver),
            &scid,
            &tampered,
            &sig,
            Some(&vm_id),
            Some(0)
        ));
    }
}
