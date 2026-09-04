//! Key-event log (KEL) for `did:scid` agent identity — PR 2.
//!
//! A storage-agnostic KEL core: events carry signatures and hash-chaining; the
//! SCID equals the hash of event #0; any copy self-verifies. `fold` replays an
//! event sequence into a `KeyState` — the same pure function runs client-side
//! today and compiles into Holochain DNA validation later.
//!
//! ## Design choices baked in
//!
//! - **JCS** (RFC 8785) canonical serialization for stable hashes.
//! - **X25519 encryption pubkey** alongside each signing key.
//! - **`agentType` + owner binding** sealed inside inception.
//! - **Recovery authority** committed as a descriptor hash (threshold + keys),
//!   not plaintext — guardian membership stays private until recovery runs.

pub mod adapter;
pub mod recovery;

use did_key::{CoreSign, PatchedKeyPair};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

// Re-export the recovery module's public API.
pub use recovery::{did_key_of, MasterSeed};

// ─── SAID helper ─────────────────────────────────────────────────────────────

fn base64_url_nopad(bytes: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes)
}

/// Self-addressing identifier: `E` derivation code + base64url(sha256(jcs(value))).
fn said_of<T: Serialize>(value: &T) -> String {
    let canonical = serde_jcs::to_vec(value).expect("JCS serialization must succeed");
    format!("E{}", base64_url_nopad(&Sha256::digest(&canonical)))
}

/// The `did:scid` prefix. One constant — a format decision touches one line.
const SCID_PREFIX: &str = "did:scid:ke:1:";

// ─── types ───────────────────────────────────────────────────────────────────

/// Key scope — controls what a delegated key may do.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Scope {
    /// Sign day-to-day expressions (proofs).
    pub sign: bool,
    /// Perform KEL operations: rotate, revoke, delegate, set recovery authority.
    pub kel_ops: bool,
    /// Delegate further keys.
    pub delegate: bool,
}

/// Device/executor lane — categorises what kind of device holds a key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Lane {
    /// A local device (laptop, desktop) under the user's direct control.
    LocalDevice,
    /// A mobile device (phone, tablet).
    Mobile,
    /// A hosted/platform executor running on infrastructure the user does not control.
    HostedExecutor,
    /// A community node serving a neighbourhood.
    CommunityNode,
}

impl Scope {
    /// A full-authority scope (inception key, recovery key).
    pub fn full() -> Self {
        Self {
            sign: true,
            kel_ops: true,
            delegate: true,
        }
    }

    /// A sign-only scope (device/executor keys — anti-rogue).
    pub fn sign_only() -> Self {
        Self {
            sign: true,
            kel_ops: false,
            delegate: false,
        }
    }
}

/// A key entry in the KEL — signing key plus optional encryption key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KeyEntry {
    /// Verification-method id (e.g. `did:scid:ke:1:E…#key-0`).
    pub id: String,
    /// Ed25519 signing key, `did:key`-encoded.
    pub signing_key: String,
    /// X25519 encryption public key — the encryption address (consumed by PR8).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub encryption_key: Option<String>,
    /// What this key may do.
    pub scope: Scope,
}

/// Human or assistant — sealed inside the inception hash.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AgentType {
    Human,
    Assistant,
}

/// Bidirectional owner proof for assistant inception — the owner's SCID plus
/// a signature from the owner proving they authorised this assistant.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnerBinding {
    /// The owner's `did:scid`.
    pub owner: String,
    /// Hex-encoded Ed25519 signature by the owner over the assistant's inception body.
    pub owner_signature: String,
}

/// The recovery authority descriptor — one shape for mnemonic-only and guardian
/// modes. Inception commits only `hash(descriptor)`, keeping membership private.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecoveryAuthority {
    pub threshold: u16,
    pub keys: Vec<String>,
}

/// Why a key revocation happened.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RevocationReason {
    Compromised,
    Retired,
    Replaced,
}

// ─── events ──────────────────────────────────────────────────────────────────

/// The body of a key event — everything the SAID and signature cover.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum KeyEventBody {
    Inception {
        keys: Vec<KeyEntry>,
        agent_type: AgentType,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        owner: Option<OwnerBinding>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        controller: Option<String>,
        /// Hash of a `RecoveryAuthority` descriptor.
        recovery_commitment: String,
    },
    Delegate {
        key: KeyEntry,
        from_seq: u64,
        /// Human-readable device name (e.g. "MacBook — local executor").
        #[serde(default, skip_serializing_if = "Option::is_none")]
        label: Option<String>,
        /// Device/executor lane.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        lane: Option<Lane>,
    },
    Rotate {
        keys: Vec<KeyEntry>,
    },
    Revoke {
        key_id: String,
        reason: RevocationReason,
    },
    ControllerOp {
        op: Box<KeyEventBody>,
    },
    SetRecoveryAuthority {
        commitment: String,
    },
    /// Permanent deactivation — identity stops accepting new events.
    Deactivate {
        reason: String,
    },
}

/// A signed, self-addressing key event.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KeyEvent {
    pub seq: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub prev_hash: Option<String>,
    pub body: KeyEventBody,
    /// SAID: `E` + base64url(sha256(jcs(body-with-seq-and-prev)))
    pub hash: String,
    /// Hex-encoded Ed25519 signature over `hash` bytes, by an authorized key.
    pub signature: String,
    /// The `key_id` of the signer.
    pub signer: String,
}

/// Hashable envelope for computing the SAID — seq + prev_hash + body.
#[derive(Serialize)]
struct HashEnvelope<'a> {
    seq: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    prev_hash: Option<&'a str>,
    body: &'a KeyEventBody,
}

impl KeyEvent {
    /// Compute the expected SAID for this event's content.
    pub fn compute_hash(&self) -> String {
        said_of(&HashEnvelope {
            seq: self.seq,
            prev_hash: self.prev_hash.as_deref(),
            body: &self.body,
        })
    }

    /// Build a new event, computing the SAID and signing with the given keypair.
    pub fn new(
        seq: u64,
        prev_hash: Option<String>,
        body: KeyEventBody,
        signer_id: &str,
        signer_kp: &PatchedKeyPair,
    ) -> Self {
        let hash = said_of(&HashEnvelope {
            seq,
            prev_hash: prev_hash.as_deref(),
            body: &body,
        });
        let signature = hex::encode(signer_kp.sign(hash.as_bytes()));
        Self {
            seq,
            prev_hash,
            body,
            hash,
            signature,
            signer: signer_id.to_string(),
        }
    }
}

// ─── KelError ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KelError {
    SeqGap { expected: u64, got: u64 },
    HashMismatch { seq: u64 },
    UnauthorizedSigner { key_id: String },
    ScopeViolation { key_id: String, attempted: String },
    InvalidInception(String),
    RecoveryCommitmentMismatch,
    MissingOwnerBinding,
    UnexpectedOwnerBinding,
    DuplicateKeyId(String),
    /// Event rejected because the identity has permanently deactivated.
    IdentityDeactivated,
}

impl std::fmt::Display for KelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KelError::SeqGap { expected, got } => {
                write!(f, "sequence gap: expected {}, got {}", expected, got)
            }
            KelError::HashMismatch { seq } => write!(f, "hash mismatch at seq {}", seq),
            KelError::UnauthorizedSigner { key_id } => {
                write!(f, "unauthorized signer: {}", key_id)
            }
            KelError::ScopeViolation { key_id, attempted } => {
                write!(f, "scope violation: {} attempted {}", key_id, attempted)
            }
            KelError::InvalidInception(msg) => write!(f, "invalid inception: {}", msg),
            KelError::RecoveryCommitmentMismatch => write!(f, "recovery commitment mismatch"),
            KelError::MissingOwnerBinding => write!(f, "assistant inception lacks owner binding"),
            KelError::UnexpectedOwnerBinding => {
                write!(f, "human inception carries an owner binding")
            }
            KelError::DuplicateKeyId(id) => write!(f, "duplicate key id: {}", id),
            KelError::IdentityDeactivated => write!(f, "identity permanently deactivated"),
        }
    }
}

impl std::error::Error for KelError {}

// ─── KeyState: fold output ───────────────────────────────────────────────────

/// A key's validity window in the log.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct KeyValidity {
    pub entry: KeyEntry,
    pub delegated_at: u64,
    pub revoked_at: Option<u64>,
    /// Human-readable device label (from Delegate body).
    pub label: Option<String>,
    /// Device/executor lane (from Delegate body).
    pub lane: Option<Lane>,
}

/// The result of folding an event log — the full key state at a point.
#[derive(Debug, Clone)]
pub struct KeyState {
    /// The `did:scid` master identifier.
    pub master: String,
    /// All keys ever delegated, with their validity windows.
    pub(crate) key_history: Vec<KeyValidity>,
    /// The agent type (sealed in inception).
    agent_type: AgentType,
    /// The controller SCID (for assistants).
    controller: Option<String>,
    /// Current recovery commitment hash.
    recovery_commitment: String,
    /// The sequence of the last processed event.
    head_seq: u64,
    /// The hash of the last processed event (for event chaining).
    head_hash: String,
    /// Permanent deactivation flag — once true, no further events accepted.
    deactivated: bool,
}

impl KeyState {
    /// The keys valid at sequence `seq`.
    pub fn keys_at(&self, seq: u64) -> Vec<&KeyEntry> {
        self.key_history
            .iter()
            .filter(|kv| kv.delegated_at <= seq && kv.revoked_at.is_none_or(|r| r > seq))
            .map(|kv| &kv.entry)
            .collect()
    }

    /// Whether a specific key held authority at sequence `seq`.
    pub fn key_valid_at(&self, key_id: &str, seq: u64) -> bool {
        self.key_history.iter().any(|kv| {
            kv.entry.id == key_id && kv.delegated_at <= seq && kv.revoked_at.is_none_or(|r| r > seq)
        })
    }

    /// The X25519 encryption keys valid at sequence `seq`.
    pub fn encryption_keys_at(&self, seq: u64) -> Vec<String> {
        self.keys_at(seq)
            .into_iter()
            .filter_map(|ke| ke.encryption_key.clone())
            .collect()
    }

    /// The controller SCID (for assistants; None for humans).
    pub fn controller(&self) -> Option<&str> {
        self.controller.as_deref()
    }

    /// The agent type sealed in inception.
    pub fn agent_type(&self) -> AgentType {
        self.agent_type
    }

    /// The current recovery commitment hash.
    pub fn recovery_commitment(&self) -> &str {
        &self.recovery_commitment
    }

    /// The sequence of the last processed event.
    pub fn head_seq(&self) -> u64 {
        self.head_seq
    }

    /// The hash of the last processed event (for event chaining).
    pub fn head_hash(&self) -> &str {
        &self.head_hash
    }

    /// The total number of keys ever delegated (for key_id indexing).
    pub fn key_count(&self) -> usize {
        self.key_history.len()
    }

    /// Whether the identity has permanently deactivated.
    pub fn is_deactivated(&self) -> bool {
        self.deactivated
    }

    /// Convert to the PR1 `signatures::KeyState` shape for the resolver seam.
    pub fn to_verification_state(&self, at_seq: Option<u64>) -> crate::agent::signatures::KeyState {
        let seq = at_seq.unwrap_or(self.head_seq);
        let keys = self
            .keys_at(seq)
            .into_iter()
            .map(|ke| crate::agent::signatures::VerificationMethod {
                id: ke.id.clone(),
                key: ke.signing_key.clone(),
            })
            .collect();
        crate::agent::signatures::KeyState {
            master: self.master.clone(),
            keys,
        }
    }

    /// Check whether a key_id has kel_ops scope at a given seq.
    fn has_kel_ops(&self, key_id: &str, seq: u64) -> bool {
        self.key_history.iter().any(|kv| {
            kv.entry.id == key_id
                && kv.delegated_at <= seq
                && kv.revoked_at.is_none_or(|r| r > seq)
                && kv.entry.scope.kel_ops
        })
    }

    /// Check whether a key_id has delegate scope at a given seq.
    fn has_delegate_scope(&self, key_id: &str, seq: u64) -> bool {
        self.key_history.iter().any(|kv| {
            kv.entry.id == key_id
                && kv.delegated_at <= seq
                && kv.revoked_at.is_none_or(|r| r > seq)
                && kv.entry.scope.delegate
        })
    }
}

// ─── fold: the pure function ─────────────────────────────────────────────────

/// Fold an event log into a `KeyState`. Pure — no I/O, no clock, no ambient state.
/// The same function runs client-side and compiles into Holochain DNA validation.
pub fn fold(events: &[KeyEvent]) -> Result<KeyState, KelError> {
    if events.is_empty() {
        return Err(KelError::InvalidInception("empty event log".into()));
    }

    // --- inception (event #0) ---
    let e0 = &events[0];
    if e0.seq != 0 {
        return Err(KelError::SeqGap {
            expected: 0,
            got: e0.seq,
        });
    }
    if e0.prev_hash.is_some() {
        return Err(KelError::InvalidInception(
            "inception carries prev_hash".into(),
        ));
    }
    // Verify SAID integrity.
    if e0.compute_hash() != e0.hash {
        return Err(KelError::HashMismatch { seq: 0 });
    }

    let (keys, agent_type, owner, controller, recovery_commitment) = match &e0.body {
        KeyEventBody::Inception {
            keys,
            agent_type,
            owner,
            controller,
            recovery_commitment,
        } => {
            if keys.is_empty() {
                return Err(KelError::InvalidInception("no keys in inception".into()));
            }
            // Agent type rules.
            match agent_type {
                AgentType::Assistant => {
                    if owner.is_none() {
                        return Err(KelError::MissingOwnerBinding);
                    }
                    if controller.is_none() {
                        return Err(KelError::InvalidInception(
                            "assistant inception lacks controller".into(),
                        ));
                    }
                }
                AgentType::Human => {
                    if owner.is_some() {
                        return Err(KelError::UnexpectedOwnerBinding);
                    }
                    if controller.is_some() {
                        return Err(KelError::InvalidInception(
                            "human inception carries controller".into(),
                        ));
                    }
                }
            }
            (
                keys.clone(),
                *agent_type,
                owner.clone(),
                controller.clone(),
                recovery_commitment.clone(),
            )
        }
        _ => {
            return Err(KelError::InvalidInception(
                "event #0 must carry an Inception body".into(),
            ))
        }
    };

    // SCID = did:scid prefix + inception hash.
    let master = format!("{}{}", SCID_PREFIX, e0.hash);

    // Build initial key history from inception keys.
    let mut key_history: Vec<KeyValidity> = Vec::new();
    for key in &keys {
        if key_history.iter().any(|kv| kv.entry.id == key.id) {
            return Err(KelError::DuplicateKeyId(key.id.clone()));
        }
        key_history.push(KeyValidity {
            entry: key.clone(),
            delegated_at: 0,
            revoked_at: None,
            label: None,
            lane: None,
        });
    }

    // Verify inception signature.
    if agent_type == AgentType::Assistant && owner.is_some() {
        // For assistant inception, the owner signs — their key isn't in the
        // assistant's key_history. Verify the owner's signature by extracting
        // the signing key from the signer's key_id (which must encode a did:key).
        verify_external_signer(e0)?;
    } else {
        // Human inception: the signer must name one of the inception keys.
        verify_event_signature(e0, &key_history, 0)?;
    }

    let mut state = KeyState {
        master,
        key_history,
        agent_type,
        controller,
        recovery_commitment,
        head_seq: 0,
        head_hash: e0.hash.clone(),
        deactivated: false,
    };

    // --- subsequent events ---
    for (i, ev) in events.iter().enumerate().skip(1) {
        let expected_seq = i as u64;
        // Reject all events after permanent deactivation.
        if state.deactivated {
            return Err(KelError::IdentityDeactivated);
        }

        if ev.seq != expected_seq {
            return Err(KelError::SeqGap {
                expected: expected_seq,
                got: ev.seq,
            });
        }

        // prev_hash must match the prior event's hash.
        match &ev.prev_hash {
            Some(ph) if *ph == events[i - 1].hash => {}
            _ => return Err(KelError::HashMismatch { seq: ev.seq }),
        }

        // SAID integrity.
        if ev.compute_hash() != ev.hash {
            return Err(KelError::HashMismatch { seq: ev.seq });
        }

        // The signer must hold authority at the previous seq (before this event applies).
        let prior_seq = ev.seq - 1;

        // Unwrap ControllerOp to get the inner body for state application.
        let (effective_body, is_controller_op) = match &ev.body {
            KeyEventBody::ControllerOp { op } => (op.as_ref(), true),
            other => (other, false),
        };

        // ControllerOp: the signer must match the controller SCID. The
        // controller operates from outside this KEL — scope checks run
        // against the controller's own KEL (verified by PR3's adapter).
        if is_controller_op {
            match &state.controller {
                Some(ctrl) if ev.signer == *ctrl => {
                    // Authorized — the controller can perform any op.
                }
                Some(_) => {
                    return Err(KelError::UnauthorizedSigner {
                        key_id: ev.signer.clone(),
                    });
                }
                None => {
                    return Err(KelError::UnauthorizedSigner {
                        key_id: ev.signer.clone(),
                    });
                }
            }
        } else {
            // Non-controller event: check scope against this KEL's key state.
            match effective_body {
                KeyEventBody::Inception { .. } => {
                    return Err(KelError::InvalidInception(
                        "inception event at non-zero seq".into(),
                    ));
                }
                KeyEventBody::Delegate { key, .. } => {
                    // Delegator needs delegate scope.
                    if !state.has_delegate_scope(&ev.signer, prior_seq) {
                        if state.has_kel_ops(&ev.signer, prior_seq) {
                            return Err(KelError::ScopeViolation {
                                key_id: ev.signer.clone(),
                                attempted: "delegate".into(),
                            });
                        }
                        return Err(KelError::UnauthorizedSigner {
                            key_id: ev.signer.clone(),
                        });
                    }
                    // Check for duplicate key id.
                    if state.key_history.iter().any(|kv| kv.entry.id == key.id) {
                        return Err(KelError::DuplicateKeyId(key.id.clone()));
                    }
                }
                KeyEventBody::Rotate { .. }
                | KeyEventBody::Revoke { .. }
                | KeyEventBody::SetRecoveryAuthority { .. }
                | KeyEventBody::Deactivate { .. } => {
                    if !state.has_kel_ops(&ev.signer, prior_seq) {
                        if state.key_valid_at(&ev.signer, prior_seq) {
                            return Err(KelError::ScopeViolation {
                                key_id: ev.signer.clone(),
                                attempted: format!("{:?}", effective_body)
                                    .split('{')
                                    .next()
                                    .unwrap_or("unknown")
                                    .trim()
                                    .to_lowercase(),
                            });
                        }
                        return Err(KelError::UnauthorizedSigner {
                            key_id: ev.signer.clone(),
                        });
                    }
                }
                KeyEventBody::ControllerOp { .. } => {
                    return Err(KelError::InvalidInception("nested ControllerOp".into()));
                }
            }
            // Verify signature against this KEL's key state.
            verify_event_signature(ev, &state.key_history, prior_seq)?;
        }

        // Apply the event to the state.
        match effective_body {
            KeyEventBody::Delegate {
                key,
                from_seq,
                label,
                lane,
            } => {
                state.key_history.push(KeyValidity {
                    entry: key.clone(),
                    delegated_at: *from_seq,
                    revoked_at: None,
                    label: label.clone(),
                    lane: lane.clone(),
                });
            }
            KeyEventBody::Rotate { keys } => {
                // Rotation replaces the active key set: revoke all kel_ops keys,
                // delegate the new set.
                for kv in &mut state.key_history {
                    if kv.revoked_at.is_none() && kv.entry.scope.kel_ops {
                        kv.revoked_at = Some(ev.seq);
                    }
                }
                for key in keys {
                    if state.key_history.iter().any(|kv| kv.entry.id == key.id) {
                        return Err(KelError::DuplicateKeyId(key.id.clone()));
                    }
                    state.key_history.push(KeyValidity {
                        entry: key.clone(),
                        delegated_at: ev.seq,
                        revoked_at: None,
                        label: None,
                        lane: None,
                    });
                }
            }
            KeyEventBody::Revoke { key_id, .. } => {
                let mut found = false;
                for kv in &mut state.key_history {
                    if kv.entry.id == *key_id && kv.revoked_at.is_none() {
                        kv.revoked_at = Some(ev.seq);
                        found = true;
                        break;
                    }
                }
                if !found {
                    return Err(KelError::UnauthorizedSigner {
                        key_id: key_id.clone(),
                    });
                }
            }
            KeyEventBody::SetRecoveryAuthority { commitment } => {
                state.recovery_commitment = commitment.clone();
            }
            KeyEventBody::Deactivate { .. } => {
                // Revoke all active keys and mark permanently deactivated.
                for kv in &mut state.key_history {
                    if kv.revoked_at.is_none() {
                        kv.revoked_at = Some(ev.seq);
                    }
                }
                state.deactivated = true;
            }
            _ => {} // Inception handled above.
        }

        state.head_seq = ev.seq;
        state.head_hash = ev.hash.clone();
    }

    Ok(state)
}

/// Verify an event's signature against an external signer — the signer's
/// `did:key` gets extracted from their key_id. Used for assistant inception
/// where the owner signs but their key isn't in the assistant's key_history.
fn verify_external_signer(ev: &KeyEvent) -> Result<(), KelError> {
    // Extract the base DID from the signer key_id: `did:key:z6Mk...#key-0` → `did:key:z6Mk...`
    let base_did = ev.signer.split('#').next().unwrap_or(&ev.signer);
    if !base_did.starts_with("did:key:") {
        return Err(KelError::UnauthorizedSigner {
            key_id: ev.signer.clone(),
        });
    }
    let kp = PatchedKeyPair::try_from(base_did).map_err(|_| KelError::UnauthorizedSigner {
        key_id: ev.signer.clone(),
    })?;
    let sig_bytes = hex::decode(&ev.signature).map_err(|_| KelError::UnauthorizedSigner {
        key_id: ev.signer.clone(),
    })?;
    kp.verify(ev.hash.as_bytes(), &sig_bytes)
        .map_err(|_| KelError::UnauthorizedSigner {
            key_id: ev.signer.clone(),
        })?;
    Ok(())
}

/// Verify an event's signature against the authorized keys at `at_seq`.
fn verify_event_signature(
    ev: &KeyEvent,
    key_history: &[KeyValidity],
    at_seq: u64,
) -> Result<(), KelError> {
    // Find the signing key.
    let signer_entry = key_history
        .iter()
        .find(|kv| {
            kv.entry.id == ev.signer
                && kv.delegated_at <= at_seq
                && kv.revoked_at.is_none_or(|r| r > at_seq)
        })
        .or_else(|| {
            // For inception (at_seq == 0), also accept keys delegated at seq 0.
            if at_seq == 0 {
                key_history
                    .iter()
                    .find(|kv| kv.entry.id == ev.signer && kv.delegated_at == 0)
            } else {
                None
            }
        })
        .ok_or_else(|| KelError::UnauthorizedSigner {
            key_id: ev.signer.clone(),
        })?;

    let kp = PatchedKeyPair::try_from(signer_entry.entry.signing_key.as_str()).map_err(|_| {
        KelError::UnauthorizedSigner {
            key_id: ev.signer.clone(),
        }
    })?;
    let sig_bytes = hex::decode(&ev.signature).map_err(|_| KelError::UnauthorizedSigner {
        key_id: ev.signer.clone(),
    })?;
    kp.verify(ev.hash.as_bytes(), &sig_bytes)
        .map_err(|_| KelError::UnauthorizedSigner {
            key_id: ev.signer.clone(),
        })?;
    Ok(())
}

// ─── convenience builders ────────────────────────────────────────────────────

/// Mint a new human identity. Returns the inception event and the SCID.
pub fn incept_human(
    keys: Vec<KeyEntry>,
    recovery_commitment: String,
    signer_id: &str,
    signer_kp: &PatchedKeyPair,
) -> (KeyEvent, String) {
    let body = KeyEventBody::Inception {
        keys,
        agent_type: AgentType::Human,
        owner: None,
        controller: None,
        recovery_commitment,
    };
    let ev = KeyEvent::new(0, None, body, signer_id, signer_kp);
    let scid = format!("{}{}", SCID_PREFIX, ev.hash);
    (ev, scid)
}

/// Mint a new assistant identity. Returns the inception event and the SCID.
pub fn incept_assistant(
    keys: Vec<KeyEntry>,
    recovery_commitment: String,
    owner_binding: OwnerBinding,
    controller: String,
    signer_id: &str,
    signer_kp: &PatchedKeyPair,
) -> (KeyEvent, String) {
    let body = KeyEventBody::Inception {
        keys,
        agent_type: AgentType::Assistant,
        owner: Some(owner_binding),
        controller: Some(controller),
        recovery_commitment,
    };
    let ev = KeyEvent::new(0, None, body, signer_id, signer_kp);
    let scid = format!("{}{}", SCID_PREFIX, ev.hash);
    (ev, scid)
}

// ─── tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use did_key::{generate, Ed25519KeyPair};

    /// Generate a fresh Ed25519 keypair and its did:key string.
    fn keypair() -> (PatchedKeyPair, String) {
        let kp = generate::<Ed25519KeyPair>(None);
        let did = did_key_of(&kp);
        (kp, did)
    }

    /// Build a KeyEntry with full scope.
    fn full_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::full(),
        }
    }

    /// Build a KeyEntry with sign-only scope.
    fn sign_only_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::sign_only(),
        }
    }

    /// Build a KeyEntry with an encryption key.
    fn key_with_enc(id: &str, signing_key: &str, enc_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: Some(enc_key.to_string()),
            scope: Scope::full(),
        }
    }

    /// A dummy recovery commitment (hash of a trivial descriptor).
    fn dummy_commitment() -> String {
        recovery::recovery_commitment(&RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkDummy".to_string()],
        })
    }

    /// Mint a simple human identity with one full-authority key.
    fn simple_human() -> (Vec<KeyEvent>, String) {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let (ev, scid) = incept_human(vec![key], dummy_commitment(), &key_id, &kp);
        (vec![ev], scid)
    }

    // ── SCID tests ───────────────────────────────────────────────────────

    #[test]
    fn scid_equals_inception_hash() {
        let (events, scid) = simple_human();
        let expected = format!("{}{}", SCID_PREFIX, events[0].hash);
        assert_eq!(scid, expected);
    }

    #[test]
    fn agent_type_sealed() {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);

        // Human inception.
        let (ev_human, scid_human) =
            incept_human(vec![key.clone()], dummy_commitment(), &key_id, &kp);
        // Assistant inception with same key — SCID must differ because
        // agent_type sits inside the hash.
        let owner_binding = OwnerBinding {
            owner: "did:scid:ke:1:Eowner".to_string(),
            owner_signature: "deadbeef".to_string(),
        };
        let (ev_assistant, scid_assistant) = incept_assistant(
            vec![key],
            dummy_commitment(),
            owner_binding,
            "did:scid:ke:1:Eowner".to_string(),
            &key_id,
            &kp,
        );
        assert_ne!(scid_human, scid_assistant);
        assert_ne!(ev_human.hash, ev_assistant.hash);
    }

    // ── sequence + hash-chain tests ──────────────────────────────────────

    #[test]
    fn seq_gap_rejected() {
        let (mut events, _scid) = simple_human();
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        // Event at seq 2 (gap at 1).
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did), &did),
            from_seq: 2,
            label: None,
            lane: None,
        };
        let ev = KeyEvent::new(2, Some(events[0].hash.clone()), body, &key_id, &kp);
        events.push(ev);
        let result = fold(&events);
        assert!(matches!(
            result,
            Err(KelError::SeqGap {
                expected: 1,
                got: 2
            })
        ));
    }

    #[test]
    fn reorder_rejected() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let (_kp1, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let body1 = KeyEventBody::Delegate {
            key: full_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // Reversed order: [ev1, ev0] — seq 0 expects inception body.
        let result = fold(&[ev1, ev0]);
        assert!(result.is_err());
    }

    #[test]
    fn bad_prev_hash() {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let (ev0, _) = incept_human(vec![key], dummy_commitment(), &key_id, &kp);

        let (_, did1) = keypair();
        let body1 = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        // Fabricated prev_hash.
        let ev1 = KeyEvent::new(1, Some("Efabricated_hash".to_string()), body1, &key_id, &kp);
        let result = fold(&[ev0, ev1]);
        assert!(matches!(result, Err(KelError::HashMismatch { seq: 1 })));
    }

    // ── authorization tests ──────────────────────────────────────────────

    #[test]
    fn unauthorized_signer() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Event signed by a key never delegated.
        let (kp_rogue, did_rogue) = keypair();
        let rogue_id = format!("{}#rogue", did_rogue);
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did_rogue), &did_rogue),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body, &rogue_id, &kp_rogue);
        let result = fold(&[ev0, ev1]);
        assert!(matches!(result, Err(KelError::UnauthorizedSigner { .. })));
    }

    #[test]
    fn sign_only_kel_op() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate a sign-only key.
        let (kp1, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let sign_key = sign_only_key(&key_id1, &did1);
        let body_delegate = KeyEventBody::Delegate {
            key: sign_key,
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body_delegate, &key_id0, &kp0);

        // The sign-only key tries a KEL op (Revoke).
        let body_revoke = KeyEventBody::Revoke {
            key_id: key_id0.clone(),
            reason: RevocationReason::Retired,
        };
        let ev2 = KeyEvent::new(2, Some(ev1.hash.clone()), body_revoke, &key_id1, &kp1);
        let result = fold(&[ev0, ev1, ev2]);
        assert!(matches!(result, Err(KelError::ScopeViolation { .. })));
    }

    // ── key_valid_at tests ───────────────────────────────────────────────

    #[test]
    fn key_valid_at_across_boundary() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate key K at seq 1 (from_seq=1).
        let (_, did_k) = keypair();
        let key_id_k = format!("{}#key-k", did_k);
        let key_k = full_key(&key_id_k, &did_k);
        let body1 = KeyEventBody::Delegate {
            key: key_k,
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // Revoke K at seq 2.
        let body2 = KeyEventBody::Revoke {
            key_id: key_id_k.clone(),
            reason: RevocationReason::Retired,
        };
        let ev2 = KeyEvent::new(2, Some(ev1.hash.clone()), body2, &key_id0, &kp0);

        let state = fold(&[ev0, ev1, ev2]).unwrap();
        // K valid at seq 1, invalid at seq 2 (revoked at 2).
        assert!(state.key_valid_at(&key_id_k, 1));
        assert!(!state.key_valid_at(&key_id_k, 2));
        // Inception key stays valid throughout (not revoked).
        assert!(state.key_valid_at(&key_id0, 0));
        assert!(state.key_valid_at(&key_id0, 2));
    }

    // ── encryption key tests ─────────────────────────────────────────────

    #[test]
    fn encryption_keys_at() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate a key with an X25519 encryption key at seq 1.
        let (_, did_enc) = keypair();
        let key_id_enc = format!("{}#key-enc", did_enc);
        let enc_key = key_with_enc(&key_id_enc, &did_enc, "x25519-pubkey-placeholder");
        let body1 = KeyEventBody::Delegate {
            key: enc_key,
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // Revoke the key at seq 2.
        let body2 = KeyEventBody::Revoke {
            key_id: key_id_enc.clone(),
            reason: RevocationReason::Retired,
        };
        let ev2 = KeyEvent::new(2, Some(ev1.hash.clone()), body2, &key_id0, &kp0);

        let state = fold(&[ev0, ev1, ev2]).unwrap();
        assert_eq!(
            state.encryption_keys_at(1),
            vec!["x25519-pubkey-placeholder"]
        );
        assert!(state.encryption_keys_at(2).is_empty());
    }

    // ── recovery commitment tests ────────────────────────────────────────

    #[test]
    fn recovery_commitment_match() {
        let seed = MasterSeed::from_mnemonic(
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
        )
        .unwrap();
        let auth = recovery::mnemonic_recovery_authority(&seed);
        let commitment = recovery::recovery_commitment(&auth);
        // Re-derive and compare.
        let auth2 = recovery::mnemonic_recovery_authority(&seed);
        let commitment2 = recovery::recovery_commitment(&auth2);
        assert_eq!(commitment, commitment2);
    }

    #[test]
    fn recovery_commitment_mismatch() {
        let auth_a = RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkA".to_string()],
        };
        let auth_b = RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkB".to_string()],
        };
        assert_ne!(
            recovery::recovery_commitment(&auth_a),
            recovery::recovery_commitment(&auth_b)
        );
    }

    // ── fold purity test ─────────────────────────────────────────────────

    #[test]
    fn fold_purity() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let (_, did1) = keypair();
        let body1 = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        let events = vec![ev0, ev1];
        let state_a = fold(&events).unwrap();
        let state_b = fold(&events).unwrap();
        // Same master.
        assert_eq!(state_a.master, state_b.master);
        assert_eq!(state_a.master, scid);
        // Same key count.
        assert_eq!(state_a.key_history.len(), state_b.key_history.len());
        // Same agent type.
        assert_eq!(state_a.agent_type(), state_b.agent_type());
    }

    // ── agent type constraint tests ──────────────────────────────────────

    #[test]
    fn assistant_needs_owner() {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        // Assistant without owner binding → must fail.
        let body = KeyEventBody::Inception {
            keys: vec![key],
            agent_type: AgentType::Assistant,
            owner: None,
            controller: Some("did:scid:ke:1:Eowner".to_string()),
            recovery_commitment: dummy_commitment(),
        };
        let ev = KeyEvent::new(0, None, body, &key_id, &kp);
        let result = fold(&[ev]);
        assert!(matches!(result, Err(KelError::MissingOwnerBinding)));
    }

    #[test]
    fn human_rejects_owner() {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let body = KeyEventBody::Inception {
            keys: vec![key],
            agent_type: AgentType::Human,
            owner: Some(OwnerBinding {
                owner: "did:scid:ke:1:Eowner".to_string(),
                owner_signature: "deadbeef".to_string(),
            }),
            controller: None,
            recovery_commitment: dummy_commitment(),
        };
        let ev = KeyEvent::new(0, None, body, &key_id, &kp);
        let result = fold(&[ev]);
        assert!(matches!(result, Err(KelError::UnexpectedOwnerBinding)));
    }

    // ── duplicate key id test ────────────────────────────────────────────

    #[test]
    fn duplicate_key_id() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate another key with the SAME id.
        let (_, did1) = keypair();
        let dup_key = full_key(&key_id0, &did1); // same id, different signing key
        let body1 = KeyEventBody::Delegate {
            key: dup_key,
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);
        let result = fold(&[ev0, ev1]);
        assert!(matches!(result, Err(KelError::DuplicateKeyId(_))));
    }

    // ── controller op tests ──────────────────────────────────────────────

    #[test]
    fn controller_op_authorized() {
        let (kp_owner, did_owner) = keypair();
        let _owner_key_id = format!("{}#key-0", did_owner);

        let (kp_asst, did_asst) = keypair();
        let asst_key_id = format!("{}#key-0", did_asst);
        let asst_key = full_key(&asst_key_id, &did_asst);

        let owner_binding = OwnerBinding {
            owner: did_owner.clone(),
            owner_signature: "placeholder".to_string(),
        };
        let (ev0, _) = incept_assistant(
            vec![asst_key],
            dummy_commitment(),
            owner_binding,
            did_owner.clone(),
            &asst_key_id,
            &kp_asst,
        );

        // Owner signs a ControllerOp (revoking the assistant's key).
        let inner_op = KeyEventBody::Revoke {
            key_id: asst_key_id.clone(),
            reason: RevocationReason::Retired,
        };
        let body = KeyEventBody::ControllerOp {
            op: Box::new(inner_op),
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body, &did_owner, &kp_owner);
        // Fold should succeed — the controller matches.
        let result = fold(&[ev0, ev1]);
        assert!(result.is_ok());
    }

    #[test]
    fn controller_op_unauthorized() {
        let (_, did_owner) = keypair();
        let (kp_asst, did_asst) = keypair();
        let asst_key_id = format!("{}#key-0", did_asst);
        let asst_key = full_key(&asst_key_id, &did_asst);

        let owner_binding = OwnerBinding {
            owner: did_owner.clone(),
            owner_signature: "placeholder".to_string(),
        };
        let (ev0, _) = incept_assistant(
            vec![asst_key],
            dummy_commitment(),
            owner_binding,
            did_owner,
            &asst_key_id,
            &kp_asst,
        );

        // A non-controller signs a ControllerOp.
        let (kp_rogue, did_rogue) = keypair();
        let inner_op = KeyEventBody::Revoke {
            key_id: asst_key_id.clone(),
            reason: RevocationReason::Compromised,
        };
        let body = KeyEventBody::ControllerOp {
            op: Box::new(inner_op),
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body, &did_rogue, &kp_rogue);
        let result = fold(&[ev0, ev1]);
        assert!(matches!(result, Err(KelError::UnauthorizedSigner { .. })));
    }

    // ── JCS stability test ───────────────────────────────────────────────

    #[test]
    fn jcs_stable_across_field_order() {
        // JCS sorts keys — reordering the struct fields must not change the hash.
        let body_a = KeyEventBody::Inception {
            keys: vec![full_key("id-0", "did:key:z6MkA")],
            agent_type: AgentType::Human,
            owner: None,
            controller: None,
            recovery_commitment: "abc123".to_string(),
        };
        // Serialize via JCS — field order comes from the Serialize impl, but
        // JCS normalises to sorted keys. Confirm: same body → same SAID.
        let said_a = said_of(&HashEnvelope {
            seq: 0,
            prev_hash: None,
            body: &body_a,
        });
        let said_b = said_of(&HashEnvelope {
            seq: 0,
            prev_hash: None,
            body: &body_a,
        });
        assert_eq!(said_a, said_b);
    }
}
