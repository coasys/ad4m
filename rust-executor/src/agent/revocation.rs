//! Revocation, rotation, and mnemonic recovery — PR6.
//!
//! Closes the key lifecycle: revoke, rotate, and recover control from the
//! mnemonic alone. Gates refuse revoked keys at neighbourhood join and
//! session start.
//!
//! ## User-facing guarantee
//!
//! "Everything this key signed until now stays valid. It can't sign anything
//! new as you. This can't be undone — enrol it again as a new key if needed."

use crate::agent::kel::adapter::AdapterError;
use crate::agent::kel::recovery::{did_key_of, MasterSeed};
#[cfg(test)]
use crate::agent::kel::{fold, RecoveryAuthority, Scope};
use crate::agent::kel::{KeyEntry, KeyEvent, KeyEventBody, KeyState, Lane, RevocationReason};
use crate::agent::resolver::{AgentLanguageResolver, Validity};
use crate::agent::signatures::ResolveError;
use did_key::PatchedKeyPair;

// ─── errors ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum RevokeError {
    /// No key with this id exists in the KEL.
    KeyNotFound(String),
    /// Key already revoked at an earlier seq.
    AlreadyRevoked(String),
    /// Signer lacks `kel_ops` scope or recovery authority.
    ScopeViolation,
    /// Could not publish the revocation event.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for RevokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevokeError::KeyNotFound(id) => write!(f, "key not found: {}", id),
            RevokeError::AlreadyRevoked(id) => write!(f, "key already revoked: {}", id),
            RevokeError::ScopeViolation => write!(f, "signer lacks kel_ops scope"),
            RevokeError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for RevokeError {}

#[derive(Debug, Clone)]
pub enum RotateError {
    /// Must rotate to at least one key.
    EmptyKeySet,
    /// Signer lacks `kel_ops` scope or recovery authority.
    ScopeViolation,
    /// Could not publish the rotation event.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for RotateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RotateError::EmptyKeySet => write!(f, "rotation requires at least one key"),
            RotateError::ScopeViolation => write!(f, "signer lacks kel_ops scope"),
            RotateError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for RotateError {}

#[derive(Debug, Clone)]
pub enum RecoverError {
    /// Phrase does not derive to the committed recovery authority.
    InvalidMnemonic(String),
    /// Derived authority hash does not match the inception commitment.
    CommitmentMismatch,
    /// Identity has no recovery commitment in its inception.
    NoRecoveryAuthority,
    /// Could not publish the recovery events.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for RecoverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecoverError::InvalidMnemonic(msg) => write!(f, "invalid mnemonic: {}", msg),
            RecoverError::CommitmentMismatch => {
                write!(f, "derived authority does not match inception commitment")
            }
            RecoverError::NoRecoveryAuthority => write!(f, "no recovery commitment"),
            RecoverError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for RecoverError {}

#[derive(Debug, Clone)]
pub enum GateError {
    /// The key has revoked.
    Revoked { key_id: String, at_seq: u64 },
    /// The identity has permanently deactivated.
    Deactivated { scid: String },
    /// Resolution failed.
    ResolveFailed(ResolveError),
}

impl std::fmt::Display for GateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GateError::Revoked { key_id, at_seq } => {
                write!(f, "key {} revoked at seq {}", key_id, at_seq)
            }
            GateError::Deactivated { scid } => write!(f, "identity {} deactivated", scid),
            GateError::ResolveFailed(e) => write!(f, "resolution failed: {:?}", e),
        }
    }
}

impl std::error::Error for GateError {}

// ─── revocation ─────────────────────────────────────────────────────────────

/// Build a revocation event. Pre-checks that the key exists and hasn't already
/// revoked. fold() validates scope on append.
pub fn revoke(
    key_id: &str,
    reason: RevocationReason,
    state: &KeyState,
    signer_key_id: &str,
    signer: &PatchedKeyPair,
) -> Result<KeyEvent, RevokeError> {
    // Check the target key exists.
    let target = state.key_history.iter().find(|kv| kv.entry.id == key_id);
    match target {
        None => return Err(RevokeError::KeyNotFound(key_id.to_string())),
        Some(kv) if kv.revoked_at.is_some() => {
            return Err(RevokeError::AlreadyRevoked(key_id.to_string()))
        }
        _ => {}
    }

    // Early scope check — signer needs kel_ops.
    let has_authority = state.key_history.iter().any(|kv| {
        kv.entry.id == signer_key_id && kv.revoked_at.is_none() && kv.entry.scope.kel_ops
    });
    if !has_authority {
        return Err(RevokeError::ScopeViolation);
    }

    let body = KeyEventBody::Revoke {
        key_id: key_id.to_string(),
        reason,
    };

    let next_seq = state.head_seq() + 1;
    let event = KeyEvent::new(
        next_seq,
        Some(state.head_hash().to_string()),
        body,
        signer_key_id,
        signer,
    );

    Ok(event)
}

// ─── rotation ───────────────────────────────────────────────────────────────

/// Build a rotation event — replace the authoritative key set. The author
/// SCID stays unchanged.
pub fn rotate(
    new_keys: Vec<KeyEntry>,
    state: &KeyState,
    signer_key_id: &str,
    signer: &PatchedKeyPair,
) -> Result<KeyEvent, RotateError> {
    if new_keys.is_empty() {
        return Err(RotateError::EmptyKeySet);
    }

    // Early scope check.
    let has_authority = state.key_history.iter().any(|kv| {
        kv.entry.id == signer_key_id && kv.revoked_at.is_none() && kv.entry.scope.kel_ops
    });
    if !has_authority {
        return Err(RotateError::ScopeViolation);
    }

    let body = KeyEventBody::Rotate { keys: new_keys };

    let next_seq = state.head_seq() + 1;
    let event = KeyEvent::new(
        next_seq,
        Some(state.head_hash().to_string()),
        body,
        signer_key_id,
        signer,
    );

    Ok(event)
}

// ─── mnemonic recovery ──────────────────────────────────────────────────────

/// Derive the recovery authority from the phrase, verify it matches the
/// inception commitment, and sign a delegation for a new device — recovering
/// control with no prior device present.
pub fn recover_from_mnemonic(
    phrase: &str,
    state: &KeyState,
    new_device: KeyEntry,
) -> Result<KeyEvent, RecoverError> {
    let seed = MasterSeed::from_mnemonic(phrase)
        .map_err(|e| RecoverError::InvalidMnemonic(e.to_string()))?;

    let authority = crate::agent::kel::recovery::mnemonic_recovery_authority(&seed);
    let commitment = crate::agent::kel::recovery::recovery_commitment(&authority);

    // Verify the commitment matches.
    if commitment != state.recovery_commitment() {
        return Err(RecoverError::CommitmentMismatch);
    }

    // Derive the recovery keypair and sign a delegation.
    let recovery_kp = crate::agent::kel::recovery::recovery_keypair(&seed);
    let recovery_did = did_key_of(&recovery_kp);
    let recovery_key_id = format!("{}#recovery-0", recovery_did);

    let inner = KeyEventBody::Delegate {
        key: new_device,
        from_seq: state.head_seq() + 1,
        label: Some("recovered device".to_string()),
        lane: Some(Lane::LocalDevice),
    };
    let body = KeyEventBody::RecoveryOp {
        op: Box::new(inner),
        proof: authority,
    };

    let next_seq = state.head_seq() + 1;
    let event = KeyEvent::new(
        next_seq,
        Some(state.head_hash().to_string()),
        body,
        &recovery_key_id,
        &recovery_kp,
    );

    Ok(event)
}

/// Paranoia default after total loss: revoke **all** existing device keys,
/// then delegate a fresh one. Returns the sequence of events to append.
pub fn recover_and_reset(
    phrase: &str,
    state: &KeyState,
    new_device: KeyEntry,
) -> Result<Vec<KeyEvent>, RecoverError> {
    let seed = MasterSeed::from_mnemonic(phrase)
        .map_err(|e| RecoverError::InvalidMnemonic(e.to_string()))?;

    let authority = crate::agent::kel::recovery::mnemonic_recovery_authority(&seed);
    let commitment = crate::agent::kel::recovery::recovery_commitment(&authority);

    if commitment != state.recovery_commitment() {
        return Err(RecoverError::CommitmentMismatch);
    }

    let recovery_kp = crate::agent::kel::recovery::recovery_keypair(&seed);
    let recovery_did = did_key_of(&recovery_kp);
    let recovery_key_id = format!("{}#recovery-0", recovery_did);

    let mut events = Vec::new();
    let mut current_seq = state.head_seq();
    let mut current_hash = state.head_hash().to_string();

    // Revoke all active keys.
    let active_keys: Vec<String> = state
        .keys_at(current_seq)
        .iter()
        .map(|k| k.id.clone())
        .collect();

    for key_id in active_keys {
        current_seq += 1;
        let inner = KeyEventBody::Revoke {
            key_id,
            reason: RevocationReason::Compromised,
        };
        let body = KeyEventBody::RecoveryOp {
            op: Box::new(inner),
            proof: authority.clone(),
        };
        let ev = KeyEvent::new(
            current_seq,
            Some(current_hash.clone()),
            body,
            &recovery_key_id,
            &recovery_kp,
        );
        current_hash = ev.hash.clone();
        events.push(ev);
    }

    // Delegate the new device.
    current_seq += 1;
    let inner = KeyEventBody::Delegate {
        key: new_device,
        from_seq: current_seq,
        label: Some("recovered device".to_string()),
        lane: Some(Lane::LocalDevice),
    };
    let body = KeyEventBody::RecoveryOp {
        op: Box::new(inner),
        proof: authority,
    };
    let ev = KeyEvent::new(
        current_seq,
        Some(current_hash),
        body,
        &recovery_key_id,
        &recovery_kp,
    );
    events.push(ev);

    Ok(events)
}

// ─── R2 gates ───────────────────────────────────────────────────────────────

/// R2 gate: admit a DID to a neighbourhood. Resolves validity first;
/// revoked keys get refused.
pub fn admit_to_neighbourhood(
    resolver: &AgentLanguageResolver,
    did: &str,
    at_seq: Option<u64>,
) -> Result<crate::agent::resolver::Agent, GateError> {
    let agent = resolver
        .resolve_agent(did, at_seq)
        .map_err(GateError::ResolveFailed)?;

    match &agent.validity {
        Validity::Revoked { at_seq } => {
            return Err(GateError::Revoked {
                key_id: did.to_string(),
                at_seq: *at_seq,
            });
        }
        Validity::Superseded { at_seq } => {
            return Err(GateError::Revoked {
                key_id: did.to_string(),
                at_seq: *at_seq,
            });
        }
        Validity::Valid => {}
    }

    Ok(agent)
}

/// R2 gate: admit a session. Resolves validity at the current head;
/// revoked or deactivated identities get refused.
pub fn admit_session(
    resolver: &AgentLanguageResolver,
    did: &str,
) -> Result<crate::agent::resolver::Agent, GateError> {
    admit_to_neighbourhood(resolver, did, None)
}

// ─── tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::kel::adapter::{KelAdapter, MemoryAdapter, MonotonicityCache};
    use crate::agent::kel::recovery;
    use crate::agent::kel::recovery::did_key_of;
    use crate::agent::kel::{incept_human, KeyEventBody, RecoveryAuthority};
    use crate::agent::resolver::ReverseIndex;
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

    fn sign_only_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::sign_only(),
        }
    }

    const MNEMONIC: &str =
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";

    fn mnemonic_commitment() -> String {
        let seed = MasterSeed::from_mnemonic(MNEMONIC).unwrap();
        let auth = recovery::mnemonic_recovery_authority(&seed);
        recovery::recovery_commitment(&auth)
    }

    fn setup_with_mnemonic() -> (Vec<KeyEvent>, String, String, PatchedKeyPair) {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let (ev, scid) = incept_human(vec![key], mnemonic_commitment(), &key_id, &kp);
        (vec![ev], scid, key_id, kp)
    }

    // ── revocation ──────────────────────────────────────────────────────────

    #[test]
    fn post_revocation_fails() {
        let (events, scid, key_id0, kp0) = setup_with_mnemonic();

        // Delegate a second key.
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let state = fold(&events).unwrap();
        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);

        // Revoke key1 at seq 2.
        let state = fold(&all).unwrap();
        let rev_ev = revoke(
            &key_id1,
            RevocationReason::Compromised,
            &state,
            &key_id0,
            &kp0,
        )
        .unwrap();
        all.push(rev_ev);

        let state = fold(&all).unwrap();
        // Post-revocation: key1 not valid at current seq.
        assert!(!state.key_valid_at(&key_id1, state.head_seq()));
    }

    #[test]
    fn pre_revocation_verifies() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let state = fold(&events).unwrap();

        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);

        let state = fold(&all).unwrap();
        let rev_ev = revoke(&key_id1, RevocationReason::Retired, &state, &key_id0, &kp0).unwrap();
        all.push(rev_ev);

        let state = fold(&all).unwrap();
        // Pre-revocation: key1 was valid at seq 1.
        assert!(state.key_valid_at(&key_id1, 1));
    }

    #[test]
    fn sign_only_cannot_revoke() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let (kp1, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let state = fold(&events).unwrap();

        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);

        let state = fold(&all).unwrap();
        // Sign-only key attempts revocation — pre-check fails.
        let result = revoke(
            &key_id0,
            RevocationReason::Compromised,
            &state,
            &key_id1,
            &kp1,
        );
        assert!(matches!(result, Err(RevokeError::ScopeViolation)));
    }

    #[test]
    fn key_not_found() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let state = fold(&events).unwrap();
        let result = revoke(
            "nonexistent#key-99",
            RevocationReason::Retired,
            &state,
            &key_id0,
            &kp0,
        );
        assert!(matches!(result, Err(RevokeError::KeyNotFound(_))));
    }

    #[test]
    fn already_revoked() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let state = fold(&events).unwrap();

        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);

        let state = fold(&all).unwrap();
        let rev_ev = revoke(&key_id1, RevocationReason::Retired, &state, &key_id0, &kp0).unwrap();
        all.push(rev_ev);

        let state = fold(&all).unwrap();
        // Second revocation fails.
        let result = revoke(&key_id1, RevocationReason::Retired, &state, &key_id0, &kp0);
        assert!(matches!(result, Err(RevokeError::AlreadyRevoked(_))));
    }

    // ── rotation ────────────────────────────────────────────────────────────

    #[test]
    fn rotation_preserves_scid() {
        let (events, scid, key_id0, kp0) = setup_with_mnemonic();
        let state = fold(&events).unwrap();

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-rot-0", scid), &new_did);
        let rot_ev = rotate(vec![new_key], &state, &key_id0, &kp0).unwrap();

        let mut all = events;
        all.push(rot_ev);
        let state2 = fold(&all).unwrap();

        // SCID unchanged.
        assert_eq!(state2.master, scid);
    }

    #[test]
    fn empty_rotation_fails() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let state = fold(&events).unwrap();
        let result = rotate(vec![], &state, &key_id0, &kp0);
        assert!(matches!(result, Err(RotateError::EmptyKeySet)));
    }

    // ── mnemonic recovery ───────────────────────────────────────────────────

    #[test]
    fn mnemonic_recovery_no_device() {
        let (events, scid, _, _) = setup_with_mnemonic();
        let state = fold(&events).unwrap();

        // Fresh device generates a key.
        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);

        // Recover from mnemonic.
        let ev = recover_from_mnemonic(MNEMONIC, &state, new_key.clone()).unwrap();

        let mut all = events;
        all.push(ev);

        // Fold accepts the recovery event: RecoveryOp wraps the Delegate,
        // carrying the full descriptor. fold verifies hash(proof) ==
        // recovery_commitment and the signer's did:key ∈ proof.keys.
        let new_state = fold(&all).unwrap();

        // The recovered key appears in the new state.
        let valid_keys = new_state.keys_at(new_state.head_seq());
        let recovered = valid_keys.iter().find(|k| k.id == new_key.id);
        assert!(recovered.is_some(), "recovered key must appear in state");
    }

    #[test]
    fn wrong_mnemonic_fails() {
        let (events, _, _, _) = setup_with_mnemonic();
        let state = fold(&events).unwrap();

        let (_, new_did) = keypair();
        let new_key = full_key("test#key-0", &new_did);

        // Wrong mnemonic.
        let result = recover_from_mnemonic(
            "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong",
            &state,
            new_key,
        );
        assert!(matches!(result, Err(RecoverError::CommitmentMismatch)));
    }

    #[test]
    fn lost_everything_revokes_all() {
        let (events, scid, key_id0, kp0) = setup_with_mnemonic();
        let state = fold(&events).unwrap();

        // Delegate another key before losing everything.
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);
        let state = fold(&all).unwrap();

        // Now recover and reset.
        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let recovery_events = recover_and_reset(MNEMONIC, &state, new_key.clone()).unwrap();

        // Should produce: 2 revocations (key0 + key1) + 1 delegation.
        assert_eq!(recovery_events.len(), 3);

        // All wrapped in RecoveryOp. First two wrap Revoke, last wraps Delegate.
        assert!(matches!(
            &recovery_events[0].body,
            KeyEventBody::RecoveryOp { op, .. } if matches!(op.as_ref(), KeyEventBody::Revoke { .. })
        ));
        assert!(matches!(
            &recovery_events[1].body,
            KeyEventBody::RecoveryOp { op, .. } if matches!(op.as_ref(), KeyEventBody::Revoke { .. })
        ));
        match &recovery_events[2].body {
            KeyEventBody::RecoveryOp { op, .. } => match op.as_ref() {
                KeyEventBody::Delegate { key, .. } => {
                    assert_eq!(key.id, new_key.id);
                }
                _ => panic!("expected RecoveryOp(Delegate)"),
            },
            _ => panic!("expected RecoveryOp"),
        }

        // Fold accepts the full recovery sequence.
        all.extend(recovery_events);
        let final_state = fold(&all).unwrap();

        // Original keys revoked, only the recovered key valid.
        let valid = final_state.keys_at(final_state.head_seq());
        assert_eq!(valid.len(), 1);
        assert_eq!(valid[0].id, new_key.id);
    }

    // ── compromise ──────────────────────────────────────────────────────────

    #[test]
    fn compromise_thief_blocked() {
        let (events, _, key_id0, kp0) = setup_with_mnemonic();
        let (kp_thief, did_thief) = keypair();
        let thief_key_id = format!("{}#key-thief", did_thief);
        let state = fold(&events).unwrap();

        // Delegate a sign-only key (the "stolen" one).
        let body = KeyEventBody::Delegate {
            key: sign_only_key(&thief_key_id, &did_thief),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);
        let state = fold(&all).unwrap();

        // Thief tries to rotate — blocked by scope.
        let (_, new_did) = keypair();
        let result = rotate(
            vec![full_key("rogue#key", &new_did)],
            &state,
            &thief_key_id,
            &kp_thief,
        );
        assert!(matches!(result, Err(RotateError::ScopeViolation)));

        // Thief tries to revoke — blocked by scope.
        let result = revoke(
            &key_id0,
            RevocationReason::Compromised,
            &state,
            &thief_key_id,
            &kp_thief,
        );
        assert!(matches!(result, Err(RevokeError::ScopeViolation)));
    }

    // ── R2 gates ────────────────────────────────────────────────────────────

    #[test]
    fn gate_refuses_revoked() {
        let (events, scid, key_id0, kp0) = setup_with_mnemonic();
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let state = fold(&events).unwrap();

        let body = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(state.head_hash().to_string()), body, &key_id0, &kp0);
        let mut all = events;
        all.push(ev1);

        let state = fold(&all).unwrap();
        let rev_ev = revoke(
            &key_id1,
            RevocationReason::Compromised,
            &state,
            &key_id0,
            &kp0,
        )
        .unwrap();
        all.push(rev_ev);

        // Set up resolver.
        let adapter = Arc::new(MemoryAdapter::new());
        adapter.seed(&scid, all);
        let cache = Arc::new(MonotonicityCache::new());
        let reverse_index = Arc::new(ReverseIndex::new());
        let resolver = AgentLanguageResolver::new(adapter, cache, reverse_index);

        // Resolve via master first to populate reverse index.
        resolver.resolve_agent(&scid, None).unwrap();

        // Gate: the revoked key at current seq should fail.
        let result = admit_to_neighbourhood(&resolver, &key_id1, None);
        assert!(matches!(result, Err(GateError::Revoked { .. })));

        // Session gate also fails.
        let result = admit_session(&resolver, &key_id1);
        assert!(matches!(result, Err(GateError::Revoked { .. })));

        // Master SCID still valid.
        let agent = admit_to_neighbourhood(&resolver, &scid, None).unwrap();
        assert_eq!(agent.master, scid);
    }
}
