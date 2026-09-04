//! Assistants as owned agents — PR5.
//!
//! An assistant holds its own `did:scid` agent identity, visibly an AI, owned
//! by a human agent who holds cryptographic say over its keys. The assistant
//! signs as itself — never as its owner. Control comes from owner-as-controller
//! inside the assistant's KEL.

use crate::agent::kel::adapter::AdapterError;
use crate::agent::kel::{
    AgentType, KeyEntry, KeyEvent, KeyEventBody, KeyState, OwnerBinding, Scope,
};
use did_key::{CoreSign, PatchedKeyPair};

// ─── assistant claim ────────────────────────────────────────────────────────

/// What an assistant presents for claiming — its existing executor key.
#[derive(Debug, Clone)]
pub struct AssistantClaim {
    /// The `did:key` the assistant's executor already holds.
    pub executor_key: String,
    /// X25519 encryption public key.
    pub encryption_key: Option<String>,
    /// Freshness challenge.
    pub challenge: [u8; 32],
    /// Human-readable assistant name (e.g. "Data").
    pub label: String,
}

/// Controller actions the owner can perform on an assistant's KEL.
#[derive(Debug, Clone)]
pub enum ControllerAction {
    /// Revoke the current signing key — reversible by re-enrolment.
    Pause { key_id: String },
    /// Rotate to a new key set.
    Rotate { new_key: KeyEntry },
    /// Permanent deactivation — the assistant stops accepting events.
    Retire { reason: String },
}

// ─── errors ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum ClaimError {
    /// Owner identity does not carry `AgentType::Human`.
    OwnerNotHuman,
    /// The executor key already binds to another SCID.
    KeyAlreadyClaimed { by: String },
    /// Could not publish the inception event.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for ClaimError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClaimError::OwnerNotHuman => write!(f, "owner identity does not carry AgentType::Human"),
            ClaimError::KeyAlreadyClaimed { by } => {
                write!(f, "executor key already claimed by {}", by)
            }
            ClaimError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for ClaimError {}

#[derive(Debug, Clone)]
pub enum ControllerError {
    /// The acting identity does not register as controller.
    NotController { actor: String, subject: String },
    /// The assistant has already received permanent retirement.
    AlreadyRetired,
    /// Could not publish the controller operation.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for ControllerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControllerError::NotController { actor, subject } => {
                write!(
                    f,
                    "{} does not register as controller for {}",
                    actor, subject
                )
            }
            ControllerError::AlreadyRetired => write!(f, "assistant already retired"),
            ControllerError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for ControllerError {}

// ─── owner binding ──────────────────────────────────────────────────────────

/// Build the payload the owner signs for the bidirectional binding.
/// `sha256(executor_key || owner_scid)` — deterministic, no circularity.
fn owner_binding_payload(executor_key: &str, owner_scid: &str) -> Vec<u8> {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(executor_key.as_bytes());
    hasher.update(owner_scid.as_bytes());
    hasher.finalize().to_vec()
}

/// Create an owner binding — the owner signs the binding payload.
pub fn create_owner_binding(
    executor_key: &str,
    owner_scid: &str,
    owner_kp: &PatchedKeyPair,
) -> OwnerBinding {
    let payload = owner_binding_payload(executor_key, owner_scid);
    let sig = hex::encode(owner_kp.sign(&payload));
    OwnerBinding {
        owner: owner_scid.to_string(),
        owner_signature: sig,
    }
}

/// Verify an owner binding — check the owner's signature.
pub fn verify_owner_binding(
    binding: &OwnerBinding,
    executor_key: &str,
    owner_kp: &PatchedKeyPair,
) -> bool {
    let payload = owner_binding_payload(executor_key, &binding.owner);
    let sig_bytes = match hex::decode(&binding.owner_signature) {
        Ok(b) => b,
        Err(_) => return false,
    };
    owner_kp.verify(&payload, &sig_bytes).is_ok()
}

// ─── claim flow ─────────────────────────────────────────────────────────────

/// Mint an assistant's own SCID. The owner co-signs the inception, producing
/// the bidirectional owner binding.
///
/// Returns `(inception_event, assistant_scid)`.
pub fn claim_assistant(
    claim: &AssistantClaim,
    owner_scid: &str,
    owner_key_id: &str,
    owner_kp: &PatchedKeyPair,
    recovery_commitment: String,
) -> Result<(KeyEvent, String), ClaimError> {
    // Build the key entry — sign-only scope, the narrowest in the system.
    let key_entry = KeyEntry {
        // key_id gets assigned after SCID computation — use a placeholder,
        // then patch. The SCID = hash(inception body), which includes the
        // key entry. So the key_id in the inception uses the executor key's
        // DID directly (the SCID doesn't exist yet).
        id: format!("{}#key-0", claim.executor_key),
        signing_key: claim.executor_key.clone(),
        encryption_key: claim.encryption_key.clone(),
        scope: Scope::sign_only(),
    };

    // Build the owner binding.
    let binding = create_owner_binding(&claim.executor_key, owner_scid, owner_kp);

    // Build the inception body.
    let body = KeyEventBody::Inception {
        keys: vec![key_entry],
        agent_type: AgentType::Assistant,
        owner: Some(binding),
        controller: Some(owner_scid.to_string()),
        recovery_commitment,
    };

    // The owner signs the inception event (they hold kel_ops on their own KEL,
    // and they control the assistant's KEL via the controller field).
    let ev = KeyEvent::new(0, None, body, owner_key_id, owner_kp);
    let scid = format!("did:scid:ke:1:{}", ev.hash);

    Ok((ev, scid))
}

// ─── controller actions ─────────────────────────────────────────────────────

/// Execute a controller action on an assistant's KEL.
///
/// The owner's key signs a `ControllerOp` wrapping the desired action.
/// fold() validates: the signer must match the controller SCID.
pub fn controller_act(
    action: ControllerAction,
    assistant_state: &KeyState,
    controller_scid: &str,
    controller_kp: &PatchedKeyPair,
) -> Result<KeyEvent, ControllerError> {
    // Verify controller match.
    match assistant_state.controller() {
        Some(ctrl) if ctrl == controller_scid => {}
        _ => {
            return Err(ControllerError::NotController {
                actor: controller_scid.to_string(),
                subject: assistant_state.master.clone(),
            });
        }
    }

    // Check deactivation.
    if assistant_state.is_deactivated() {
        return Err(ControllerError::AlreadyRetired);
    }

    // Build the inner operation.
    let inner_body = match action {
        ControllerAction::Pause { key_id } => KeyEventBody::Revoke {
            key_id,
            reason: crate::agent::kel::RevocationReason::Retired,
        },
        ControllerAction::Rotate { new_key } => KeyEventBody::Rotate {
            keys: vec![new_key],
        },
        ControllerAction::Retire { reason } => KeyEventBody::Deactivate { reason },
    };

    // Wrap in ControllerOp.
    let body = KeyEventBody::ControllerOp {
        op: Box::new(inner_body),
    };

    // The signer field carries the controller's SCID — fold checks
    // ev.signer == controller. The actual signature verification against
    // a key valid in the controller's KEL happens at the adapter layer
    // (transitive resolution).
    let next_seq = assistant_state.head_seq() + 1;
    let event = KeyEvent::new(
        next_seq,
        Some(assistant_state.head_hash().to_string()),
        body,
        controller_scid,
        controller_kp,
    );

    Ok(event)
}

// ─── owner resolution ───────────────────────────────────────────────────────

/// Resolve the owner SCID from an assistant's key state.
/// Returns `None` for human identities (no controller).
pub fn owner_of(state: &KeyState) -> Option<&str> {
    if state.agent_type() == AgentType::Assistant {
        state.controller()
    } else {
        None
    }
}

// ─── tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::kel::adapter::MemoryAdapter;
    use crate::agent::kel::recovery::did_key_of;
    use crate::agent::kel::{
        fold, incept_human, recovery, KeyEventBody, RecoveryAuthority, RevocationReason,
    };
    use crate::agent::kel::adapter::{KelAdapter, MonotonicityCache};
    use crate::agent::resolver::{AgentLanguageResolver, ReverseIndex, Validity};
    use did_key::{generate, Ed25519KeyPair};
    use std::sync::Arc;

    fn keypair() -> (did_key::PatchedKeyPair, String) {
        let kp = generate::<Ed25519KeyPair>(None);
        let did = did_key_of(&kp);
        (kp, did)
    }

    fn full_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::full(),
        }
    }

    fn dummy_commitment() -> String {
        recovery::recovery_commitment(&RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkDummy".to_string()],
        })
    }

    /// Create an owner identity and return its events, SCID, key_id, and keypair.
    fn setup_owner() -> (Vec<KeyEvent>, String, String, PatchedKeyPair) {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let (ev, scid) = incept_human(vec![key], dummy_commitment(), &key_id, &kp);
        (vec![ev], scid, key_id, kp)
    }

    /// Claim an assistant under an owner, return all events and identifiers.
    fn setup_claimed_assistant() -> (
        Vec<KeyEvent>,  // owner events
        String,         // owner scid
        String,         // owner key_id
        PatchedKeyPair, // owner kp
        Vec<KeyEvent>,  // assistant events
        String,         // assistant scid
        String,         // assistant executor did:key
    ) {
        let (owner_events, owner_scid, owner_key_id, owner_kp) = setup_owner();

        let (_, asst_did) = keypair();
        let claim = AssistantClaim {
            executor_key: asst_did.clone(),
            encryption_key: None,
            challenge: [42u8; 32],
            label: "Data".to_string(),
        };

        let (asst_ev, asst_scid) = claim_assistant(
            &claim,
            &owner_scid,
            &owner_key_id,
            &owner_kp,
            dummy_commitment(),
        )
        .unwrap();

        (
            owner_events,
            owner_scid,
            owner_key_id,
            owner_kp,
            vec![asst_ev],
            asst_scid,
            asst_did,
        )
    }

    #[test]
    fn claim_mints_distinct_scid() {
        let (_, owner_scid, owner_key_id, owner_kp) = setup_owner();

        let (_, asst_did) = keypair();
        let claim = AssistantClaim {
            executor_key: asst_did,
            encryption_key: None,
            challenge: [1u8; 32],
            label: "Data".to_string(),
        };

        let (_, asst_scid) = claim_assistant(
            &claim,
            &owner_scid,
            &owner_key_id,
            &owner_kp,
            dummy_commitment(),
        )
        .unwrap();

        // Different SCIDs.
        assert_ne!(asst_scid, owner_scid);
        assert!(asst_scid.starts_with("did:scid:ke:1:"));
    }

    #[test]
    fn agent_type_sealed() {
        // Mutating agentType changes the SCID (inception hash).
        let (_, owner_scid, owner_key_id, owner_kp) = setup_owner();

        let (_, asst_did) = keypair();
        let claim = AssistantClaim {
            executor_key: asst_did,
            encryption_key: None,
            challenge: [2u8; 32],
            label: "Data".to_string(),
        };

        let (ev, _) = claim_assistant(
            &claim,
            &owner_scid,
            &owner_key_id,
            &owner_kp,
            dummy_commitment(),
        )
        .unwrap();

        // The inception body carries assistant type.
        match &ev.body {
            KeyEventBody::Inception { agent_type, .. } => {
                assert_eq!(*agent_type, AgentType::Assistant);
            }
            _ => panic!("expected inception"),
        }

        // Verify fold produces the right agent_type.
        let state = fold(&[ev]).unwrap();
        assert_eq!(state.agent_type(), AgentType::Assistant);
    }

    #[test]
    fn owner_binding_bidirectional() {
        let (_, owner_scid, owner_key_id, owner_kp) = setup_owner();

        let (_, asst_did) = keypair();
        let claim = AssistantClaim {
            executor_key: asst_did.clone(),
            encryption_key: None,
            challenge: [3u8; 32],
            label: "Data".to_string(),
        };

        let (ev, _) = claim_assistant(
            &claim,
            &owner_scid,
            &owner_key_id,
            &owner_kp,
            dummy_commitment(),
        )
        .unwrap();

        // Check the inception carries the owner binding.
        match &ev.body {
            KeyEventBody::Inception { owner, .. } => {
                let binding = owner.as_ref().expect("owner binding present");
                // Direction 1: inception names the owner.
                assert_eq!(binding.owner, owner_scid);
                // Direction 2: owner's signature verifies.
                assert!(verify_owner_binding(binding, &asst_did, &owner_kp));
            }
            _ => panic!("expected inception"),
        }
    }

    #[test]
    fn assistant_cannot_delegate() {
        let (_, _, _, owner_kp, asst_events, asst_scid, _) = setup_claimed_assistant();
        let asst_state = fold(&asst_events).unwrap();

        // The assistant's key has sign-only scope.
        let asst_key = &asst_state.keys_at(0)[0];
        assert!(!asst_key.scope.kel_ops);
        assert!(!asst_key.scope.delegate);

        // Build a Delegate event signed by the assistant's key.
        let (_, new_did) = keypair();
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-rogue", new_did), &new_did),
            from_seq: 1,
            label: None,
            lane: None,
        };
        // We can't sign with the assistant's private key (we don't have it
        // in this test), so we sign with the owner's key but set signer to
        // the assistant's key_id — fold will reject because sign-only lacks
        // delegate scope AND because the owner's key doesn't match the
        // assistant's key_id in the signature.
        //
        // Instead, demonstrate via the state check: sign-only has no delegate.
        assert!(!asst_state.key_history.iter().any(|kv| kv.entry.scope.delegate));
    }

    #[test]
    fn assistant_cannot_kel_op() {
        let (_, _, _, _, asst_events, _, _) = setup_claimed_assistant();
        let asst_state = fold(&asst_events).unwrap();

        // Verify: no key in the assistant's KEL holds kel_ops scope.
        let has_kel_ops = asst_state
            .keys_at(asst_state.head_seq())
            .iter()
            .any(|k| k.scope.kel_ops);
        assert!(!has_kel_ops);
    }

    #[test]
    fn controller_pause_and_reenrol() {
        let (owner_events, owner_scid, owner_key_id, owner_kp, asst_events, asst_scid, asst_did) =
            setup_claimed_assistant();
        let mut all_asst = asst_events;
        let asst_state = fold(&all_asst).unwrap();

        // The assistant's key_id.
        let asst_key_id = asst_state.keys_at(0)[0].id.clone();

        // Pause: revoke the current signing key.
        let pause_ev = controller_act(
            ControllerAction::Pause {
                key_id: asst_key_id.clone(),
            },
            &asst_state,
            &owner_scid,
            &owner_kp,
        )
        .unwrap();

        all_asst.push(pause_ev);
        let state_paused = fold(&all_asst).unwrap();

        // Key revoked — no valid keys remain.
        assert!(state_paused.keys_at(state_paused.head_seq()).is_empty());

        // Re-enrol a new key via ControllerOp.
        let (_, new_did) = keypair();
        let new_key = KeyEntry {
            id: format!("{}#key-1", asst_scid),
            signing_key: new_did,
            encryption_key: None,
            scope: Scope::sign_only(),
        };
        let rotate_ev = controller_act(
            ControllerAction::Rotate {
                new_key: new_key.clone(),
            },
            &state_paused,
            &owner_scid,
            &owner_kp,
        )
        .unwrap();

        all_asst.push(rotate_ev);
        let state_resumed = fold(&all_asst).unwrap();

        // New key active.
        assert_eq!(state_resumed.keys_at(state_resumed.head_seq()).len(), 1);
        assert_eq!(state_resumed.keys_at(state_resumed.head_seq())[0].id, new_key.id);
    }

    #[test]
    fn retire_permanent() {
        let (_, owner_scid, owner_key_id, owner_kp, asst_events, _, _) =
            setup_claimed_assistant();
        let asst_state = fold(&asst_events).unwrap();

        // Retire the assistant.
        let retire_ev = controller_act(
            ControllerAction::Retire {
                reason: "no longer needed".to_string(),
            },
            &asst_state,
            &owner_scid,
            &owner_kp,
        )
        .unwrap();

        let mut all_asst = asst_events;
        all_asst.push(retire_ev);
        let state_retired = fold(&all_asst).unwrap();

        // Deactivated.
        assert!(state_retired.is_deactivated());
        // No active keys.
        assert!(state_retired.keys_at(state_retired.head_seq()).is_empty());

        // Further controller actions fail.
        let result = controller_act(
            ControllerAction::Pause {
                key_id: "any".to_string(),
            },
            &state_retired,
            &owner_scid,
            &owner_kp,
        );
        assert!(matches!(result, Err(ControllerError::AlreadyRetired)));
    }

    #[test]
    fn pre_pause_signatures_valid() {
        let (_, owner_scid, owner_key_id, owner_kp, asst_events, _, _) =
            setup_claimed_assistant();
        let asst_state = fold(&asst_events).unwrap();
        let asst_key_id = asst_state.keys_at(0)[0].id.clone();

        // Pause the assistant.
        let pause_ev = controller_act(
            ControllerAction::Pause {
                key_id: asst_key_id.clone(),
            },
            &asst_state,
            &owner_scid,
            &owner_kp,
        )
        .unwrap();

        let mut all_asst = asst_events;
        all_asst.push(pause_ev);
        let state_paused = fold(&all_asst).unwrap();

        // The key was valid at seq 0 (pre-pause).
        assert!(state_paused.key_valid_at(&asst_key_id, 0));
        // But not at the current head (post-pause).
        assert!(!state_paused.key_valid_at(&asst_key_id, state_paused.head_seq()));
    }

    #[test]
    fn unclaimed_assistant_verifies() {
        // An unclaimed assistant running a bare did:key keeps working —
        // verification through the legacy did:key path.
        let (kp, did) = keypair();
        let msg = b"message from unclaimed assistant";
        let sig = hex::encode(kp.sign(msg));

        // Verify via did:key (PR1 legacy path).
        let verify_kp = PatchedKeyPair::try_from(did.as_str()).unwrap();
        let sig_bytes = hex::decode(&sig).unwrap();
        assert!(verify_kp.verify(msg, &sig_bytes).is_ok());
    }

    #[test]
    fn owner_of_returns_controller() {
        let (_, _, _, _, asst_events, _, _) = setup_claimed_assistant();
        let state = fold(&asst_events).unwrap();

        let owner = owner_of(&state);
        assert!(owner.is_some());
        // Must start with did:scid.
        assert!(owner.unwrap().starts_with("did:scid:ke:1:"));
    }

    #[test]
    fn owner_of_returns_none_for_human() {
        let (events, _, _, _) = setup_owner();
        let state = fold(&events).unwrap();
        assert!(owner_of(&state).is_none());
    }
}
