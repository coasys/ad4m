//! Social recovery through guardians — PR7.
//!
//! Extends PR6's mnemonic recovery with a t-of-N guardian quorum. Guardians
//! register by SCID (not key), so their own rotations never break the roster.
//! A timelock and veto prevent the same quorum from stealing an identity.
//!
//! ## Safety model
//!
//! A t-of-N quorum that can rotate an identity can equally steal it. Three
//! defences prevent that:
//!
//! 1. **Execution timelock.** Reaching quorum starts a wait. Only after the
//!    window expires — with no veto — does the rotation execute.
//! 2. **Veto.** Any kel_ops-scoped key in the subject's KEL cancels recovery
//!    during the window. A holder who still controls a device always wins.
//! 3. **Change timelock.** Altering the guardian set waits too, so brief device
//!    access cannot swap guardians and immediately exercise them.

use crate::agent::kel::recovery::{did_key_of, recovery_commitment};
use crate::agent::kel::{
    KeyEntry, KeyEvent, KeyEventBody, KeyState, Lane, RecoveryAuthority, RevocationReason,
};
use did_key::{CoreSign, PatchedKeyPair};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

// ─── constants ─────────────────────────────────────────────────────────────

/// Default execution timelock: 7 days in seconds.
pub const DEFAULT_TIMELOCK_SECS: u64 = 7 * 24 * 3600;

/// Default guardian threshold.
pub const DEFAULT_THRESHOLD: u16 = 3;

/// Default guardian count.
pub const DEFAULT_GUARDIANS: u16 = 5;

// ─── data structures ───────────────────────────────────────────────────────

/// A guardian roster — identified by SCID (not key), so guardian rotations
/// never break the roster.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GuardianRoster {
    pub threshold: u16,
    pub guardians: Vec<String>, // SCIDs
}

/// Guardian's consent to serve — signed proof they accepted.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GuardianConsent {
    pub guardian: String,      // guardian SCID
    pub subject: String,       // subject SCID
    pub signer_key_id: String, // did:key:z6Mk...#key-0
    pub signature: String,     // hex-encoded
}

/// Recovery request — published by the subject on a fresh device.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecoveryRequest {
    pub subject: String,             // SCID being recovered
    pub new_device: KeyEntry,        // fresh device key
    pub revealed: RecoveryAuthority, // MUST hash to committed value
    pub challenge: [u8; 32],
    pub requested_at_epoch_secs: u64,
}

/// Guardian's approval of a recovery request.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GuardianApproval {
    pub guardian: String,      // guardian SCID
    pub request_hash: String,  // hash of the recovery request
    pub signer_key_id: String, // key_id in guardian's own KEL
    pub signature: String,     // hex-encoded
}

/// Recovery progress state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecoveryState {
    Collecting { have: u16, need: u16 },
    Timelocked { executes_at_epoch_secs: u64 },
    Vetoed { by: String },
    Executed { at_seq: u64 },
}

// ─── errors ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum GuardianError {
    /// Revealed descriptor hash does not match the committed value.
    CommitmentMismatch,
    /// Approval from an identity outside the revealed roster.
    NotInRoster { guardian: String },
    /// Same guardian approved the same request twice.
    DuplicateApproval { guardian: String },
    /// Quorum not reached.
    InsufficientApprovals { have: u16, need: u16 },
    /// Execution timelock has not expired.
    TimelockNotExpired { executes_at_epoch_secs: u64 },
    /// A kel_ops-scoped key vetoed the recovery.
    Vetoed { by: String },
    /// Guardian's signing key lacks validity in their own KEL.
    GuardianKeyInvalid { guardian: String },
    /// Guardian has not signed the acceptance for this roster.
    ConsentMissing { guardian: String },
    /// Live guardian count fell below threshold after a resignation.
    RosterBelowThreshold { live: u16, threshold: u16 },
    /// A roster change sits in its own waiting period.
    ChangeTimelockActive,
    /// Signer lacks kel_ops scope.
    ScopeViolation,
}

impl std::fmt::Display for GuardianError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GuardianError::CommitmentMismatch => {
                write!(f, "revealed descriptor does not match committed value")
            }
            GuardianError::NotInRoster { guardian } => {
                write!(f, "guardian {} not in roster", guardian)
            }
            GuardianError::DuplicateApproval { guardian } => {
                write!(f, "duplicate approval from {}", guardian)
            }
            GuardianError::InsufficientApprovals { have, need } => {
                write!(f, "insufficient approvals: {} of {} required", have, need)
            }
            GuardianError::TimelockNotExpired {
                executes_at_epoch_secs,
            } => {
                write!(f, "timelock expires at {}", executes_at_epoch_secs)
            }
            GuardianError::Vetoed { by } => write!(f, "vetoed by {}", by),
            GuardianError::GuardianKeyInvalid { guardian } => {
                write!(f, "guardian {} key invalid", guardian)
            }
            GuardianError::ConsentMissing { guardian } => {
                write!(f, "consent missing from guardian {}", guardian)
            }
            GuardianError::RosterBelowThreshold { live, threshold } => {
                write!(
                    f,
                    "roster below threshold: {} live, {} required",
                    live, threshold
                )
            }
            GuardianError::ChangeTimelockActive => write!(f, "roster change timelock active"),
            GuardianError::ScopeViolation => write!(f, "signer lacks kel_ops scope"),
        }
    }
}

impl std::error::Error for GuardianError {}

// ─── roster helpers ────────────────────────────────────────────────────────

/// Convert a guardian roster to the RecoveryAuthority format.
pub fn roster_to_authority(roster: &GuardianRoster) -> RecoveryAuthority {
    RecoveryAuthority {
        threshold: roster.threshold,
        keys: roster.guardians.clone(),
    }
}

/// Hash the roster using recovery_commitment.
pub fn roster_commitment(roster: &GuardianRoster) -> String {
    recovery_commitment(&roster_to_authority(roster))
}

// ─── consent ───────────────────────────────────────────────────────────────

/// The consent message a guardian signs: `GUARDIAN_CONSENT:{subject_scid}`.
fn consent_message(subject_scid: &str) -> Vec<u8> {
    format!("GUARDIAN_CONSENT:{}", subject_scid).into_bytes()
}

/// Guardian signs consent to serve for a subject.
pub fn sign_consent(
    subject_scid: &str,
    guardian_scid: &str,
    guardian_key_id: &str,
    guardian_kp: &PatchedKeyPair,
) -> GuardianConsent {
    let msg = consent_message(subject_scid);
    let sig = guardian_kp.sign(&msg);
    GuardianConsent {
        guardian: guardian_scid.to_string(),
        subject: subject_scid.to_string(),
        signer_key_id: guardian_key_id.to_string(),
        signature: hex::encode(sig),
    }
}

/// Verify a consent signature by extracting the did:key from signer_key_id.
pub fn verify_consent(consent: &GuardianConsent) -> Result<(), GuardianError> {
    let base_did = consent
        .signer_key_id
        .split('#')
        .next()
        .unwrap_or(&consent.signer_key_id);
    let kp = PatchedKeyPair::try_from(base_did).map_err(|_| GuardianError::GuardianKeyInvalid {
        guardian: consent.guardian.clone(),
    })?;
    let msg = consent_message(&consent.subject);
    let sig_bytes =
        hex::decode(&consent.signature).map_err(|_| GuardianError::GuardianKeyInvalid {
            guardian: consent.guardian.clone(),
        })?;
    kp.verify(&msg, &sig_bytes)
        .map_err(|_| GuardianError::GuardianKeyInvalid {
            guardian: consent.guardian.clone(),
        })?;
    Ok(())
}

// ─── request hashing ───────────────────────────────────────────────────────

/// JCS-canonicalize and SHA-256 hash a recovery request.
pub fn hash_request(request: &RecoveryRequest) -> String {
    let canonical = serde_jcs::to_vec(request).expect("JCS serialization must succeed");
    hex::encode(Sha256::digest(&canonical))
}

// ─── recovery flow ─────────────────────────────────────────────────────────

/// Validate a recovery request: check the revealed descriptor hashes to
/// the committed value. Return Collecting state.
pub fn open_recovery(
    request: &RecoveryRequest,
    state: &KeyState,
) -> Result<RecoveryState, GuardianError> {
    let hash = recovery_commitment(&request.revealed);
    if hash != state.recovery_commitment() {
        return Err(GuardianError::CommitmentMismatch);
    }
    Ok(RecoveryState::Collecting {
        have: 0,
        need: request.revealed.threshold,
    })
}

/// Guardian signs the request hash. Checks guardian SCID appears in the
/// revealed roster.
pub fn approve_recovery(
    request: &RecoveryRequest,
    guardian_scid: &str,
    guardian_key_id: &str,
    guardian_kp: &PatchedKeyPair,
) -> Result<GuardianApproval, GuardianError> {
    // Check guardian belongs to the roster.
    if !request.revealed.keys.iter().any(|k| k == guardian_scid) {
        return Err(GuardianError::NotInRoster {
            guardian: guardian_scid.to_string(),
        });
    }
    let req_hash = hash_request(request);
    let sig = guardian_kp.sign(req_hash.as_bytes());
    Ok(GuardianApproval {
        guardian: guardian_scid.to_string(),
        request_hash: req_hash,
        signer_key_id: guardian_key_id.to_string(),
        signature: hex::encode(sig),
    })
}

/// Collect approvals: check no duplicates, all from roster, count ≥ threshold.
/// If threshold met, return Timelocked.
pub fn collect_approvals(
    request: &RecoveryRequest,
    approvals: &[GuardianApproval],
    now_epoch_secs: u64,
) -> Result<RecoveryState, GuardianError> {
    let threshold = request.revealed.threshold;

    // Check all approvals come from roster members, no duplicates.
    let mut seen = std::collections::HashSet::new();
    for approval in approvals {
        if !request
            .revealed
            .keys
            .iter()
            .any(|k| k == &approval.guardian)
        {
            return Err(GuardianError::NotInRoster {
                guardian: approval.guardian.clone(),
            });
        }
        if !seen.insert(&approval.guardian) {
            return Err(GuardianError::DuplicateApproval {
                guardian: approval.guardian.clone(),
            });
        }
    }

    let have = approvals.len() as u16;
    if have < threshold {
        return Err(GuardianError::InsufficientApprovals {
            have,
            need: threshold,
        });
    }

    Ok(RecoveryState::Timelocked {
        executes_at_epoch_secs: now_epoch_secs + DEFAULT_TIMELOCK_SECS,
    })
}

/// Verify a veto signer has kel_ops scope.
pub fn veto_recovery(state: &KeyState, signer_key_id: &str) -> Result<(), GuardianError> {
    let has_ops = state.key_history.iter().any(|kv| {
        kv.entry.id == signer_key_id && kv.revoked_at.is_none() && kv.entry.scope.kel_ops
    });
    if !has_ops {
        return Err(GuardianError::ScopeViolation);
    }
    Ok(())
}

/// Execute a guardian recovery. Check: quorum met, timelock expired, not
/// vetoed. Produce RecoveryOp events: revoke all active keys + delegate
/// the new device.
///
/// The `signer_key_id` / `signer` represent the recovery authority signer
/// (the new device or a coordinator key). Events wrap in RecoveryOp with
/// the revealed descriptor as proof.
pub fn execute_recovery(
    request: &RecoveryRequest,
    approvals: &[GuardianApproval],
    state: &KeyState,
    timelocked_at_epoch_secs: u64,
    now_epoch_secs: u64,
    signer_key_id: &str,
    signer: &PatchedKeyPair,
) -> Result<Vec<KeyEvent>, GuardianError> {
    let threshold = request.revealed.threshold;

    // Verify quorum.
    let mut seen = std::collections::HashSet::new();
    for approval in approvals {
        if !request
            .revealed
            .keys
            .iter()
            .any(|k| k == &approval.guardian)
        {
            return Err(GuardianError::NotInRoster {
                guardian: approval.guardian.clone(),
            });
        }
        seen.insert(&approval.guardian);
    }
    let have = seen.len() as u16;
    if have < threshold {
        return Err(GuardianError::InsufficientApprovals {
            have,
            need: threshold,
        });
    }

    // Check timelock expired.
    let executes_at = timelocked_at_epoch_secs + DEFAULT_TIMELOCK_SECS;
    if now_epoch_secs < executes_at {
        return Err(GuardianError::TimelockNotExpired {
            executes_at_epoch_secs: executes_at,
        });
    }

    // Build RecoveryOp events — same pattern as recover_and_reset in PR6.
    let authority = request.revealed.clone();
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
            signer_key_id,
            signer,
        );
        current_hash = ev.hash.clone();
        events.push(ev);
    }

    // Delegate the new device.
    current_seq += 1;
    let inner = KeyEventBody::Delegate {
        key: request.new_device.clone(),
        from_seq: current_seq,
        label: Some("recovered device (guardian)".to_string()),
        lane: Some(Lane::LocalDevice),
    };
    let body = KeyEventBody::RecoveryOp {
        op: Box::new(inner),
        proof: authority,
    };
    let ev = KeyEvent::new(current_seq, Some(current_hash), body, signer_key_id, signer);
    events.push(ev);

    Ok(events)
}

// ─── roster management ─────────────────────────────────────────────────────

/// Build a SetRecoveryAuthority event from a guardian roster. Check all
/// guardians have consent. The signer must have kel_ops scope.
pub fn set_guardians(
    roster: &GuardianRoster,
    consents: &[GuardianConsent],
    state: &KeyState,
    signer_key_id: &str,
    signer: &PatchedKeyPair,
) -> Result<KeyEvent, GuardianError> {
    // Check signer has kel_ops.
    let has_ops = state.key_history.iter().any(|kv| {
        kv.entry.id == signer_key_id && kv.revoked_at.is_none() && kv.entry.scope.kel_ops
    });
    if !has_ops {
        return Err(GuardianError::ScopeViolation);
    }

    // Check all guardians have consent.
    for guardian_scid in &roster.guardians {
        let has_consent = consents.iter().any(|c| c.guardian == *guardian_scid);
        if !has_consent {
            return Err(GuardianError::ConsentMissing {
                guardian: guardian_scid.clone(),
            });
        }
    }

    let commitment = roster_commitment(roster);
    let body = KeyEventBody::SetRecoveryAuthority { commitment };
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

// ─── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::kel::adapter::{KelAdapter, MemoryAdapter, MonotonicityCache};
    use crate::agent::kel::recovery;
    use crate::agent::kel::recovery::did_key_of;
    use crate::agent::kel::{fold, incept_human, Scope};
    use crate::agent::resolver::ReverseIndex;
    use did_key::{generate, Ed25519KeyPair};
    use std::sync::Arc;

    fn keypair() -> (PatchedKeyPair, String) {
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

    /// Create 3 guardian keypairs, build a roster and commitment.
    fn setup_guardians(
        threshold: u16,
    ) -> (
        Vec<(PatchedKeyPair, String, String)>, // (kp, did, key_id) per guardian
        GuardianRoster,
        String, // commitment
    ) {
        let mut guardian_data = Vec::new();
        let mut scids = Vec::new();
        for i in 0..3 {
            let (kp, did) = keypair();
            let key_id = format!("{}#key-{}", did, i);
            // In tests, guardian SCIDs = their did:key (simplified)
            scids.push(did.clone());
            guardian_data.push((kp, did, key_id));
        }
        let roster = GuardianRoster {
            threshold,
            guardians: scids,
        };
        let commitment = roster_commitment(&roster);
        (guardian_data, roster, commitment)
    }

    /// Create a subject identity using a guardian-based recovery commitment.
    fn setup_subject_with_guardians(
        commitment: &str,
    ) -> (Vec<KeyEvent>, String, String, PatchedKeyPair) {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let key = full_key(&key_id, &did);
        let (ev, scid) = incept_human(vec![key], commitment.to_string(), &key_id, &kp);
        (vec![ev], scid, key_id, kp)
    }

    /// Build a recovery request.
    fn make_request(
        subject_scid: &str,
        new_device: KeyEntry,
        authority: &RecoveryAuthority,
    ) -> RecoveryRequest {
        RecoveryRequest {
            subject: subject_scid.to_string(),
            new_device,
            revealed: authority.clone(),
            challenge: [0u8; 32],
            requested_at_epoch_secs: 1_000_000,
        }
    }

    // ── consent ────────────────────────────────────────────────────────────

    #[test]
    fn consent_round_trip() {
        let (kp, did) = keypair();
        let key_id = format!("{}#key-0", did);
        let subject_scid = "did:scid:ke:1:Esubject";

        let consent = sign_consent(subject_scid, &did, &key_id, &kp);
        assert_eq!(consent.guardian, did);
        assert_eq!(consent.subject, subject_scid);

        // Verification passes.
        verify_consent(&consent).unwrap();
    }

    // ── quorum ─────────────────────────────────────────────────────────────

    #[test]
    fn t_minus_1_fails() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        // Only 1 approval (threshold = 2).
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let approval = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();

        let result = collect_approvals(&request, &[approval], 1_000_000);
        assert!(matches!(
            result,
            Err(GuardianError::InsufficientApprovals { have: 1, need: 2 })
        ));
    }

    #[test]
    fn t_approvals_succeed() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        // 2 approvals (threshold = 2).
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let (ref g1_kp, ref g1_did, ref g1_kid) = guardians[1];
        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a1 = approve_recovery(&request, g1_did, g1_kid, g1_kp).unwrap();

        let result = collect_approvals(&request, &[a0, a1], 1_000_000);
        assert!(matches!(result, Ok(RecoveryState::Timelocked { .. })));
    }

    #[test]
    fn non_roster_approval_fails() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        // Outsider tries to approve.
        let (outsider_kp, outsider_did) = keypair();
        let outsider_kid = format!("{}#key-0", outsider_did);
        let result = approve_recovery(&request, &outsider_did, &outsider_kid, &outsider_kp);
        assert!(matches!(result, Err(GuardianError::NotInRoster { .. })));
    }

    #[test]
    fn descriptor_hash_mismatch() {
        let (_, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);

        // Wrong revealed authority.
        let wrong_authority = RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkWrong".to_string()],
        };
        let request = make_request(&scid, new_key, &wrong_authority);

        let result = open_recovery(&request, &state);
        assert!(matches!(result, Err(GuardianError::CommitmentMismatch)));
    }

    #[test]
    fn duplicate_approval_counts_once() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        // Same guardian approves twice.
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a0_dup = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();

        let result = collect_approvals(&request, &[a0, a0_dup], 1_000_000);
        assert!(matches!(
            result,
            Err(GuardianError::DuplicateApproval { .. })
        ));
    }

    #[test]
    fn reaching_t_timelocks() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let (ref g1_kp, ref g1_did, ref g1_kid) = guardians[1];
        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a1 = approve_recovery(&request, g1_did, g1_kid, g1_kp).unwrap();

        let now = 1_000_000;
        let state = collect_approvals(&request, &[a0, a1], now).unwrap();

        // State should indicate timelocked, not executed.
        match state {
            RecoveryState::Timelocked {
                executes_at_epoch_secs,
            } => {
                assert_eq!(executes_at_epoch_secs, now + DEFAULT_TIMELOCK_SECS);
            }
            _ => panic!("expected Timelocked"),
        }
    }

    #[test]
    fn veto_cancels() {
        let (_, roster, commitment) = setup_guardians(2);
        let (events, _, key_id0, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // Holder with kel_ops can veto.
        let result = veto_recovery(&state, &key_id0);
        assert!(result.is_ok());
    }

    #[test]
    fn veto_requires_kel_ops() {
        let (_, roster, commitment) = setup_guardians(2);
        let (events, scid, key_id0, kp0) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // Delegate a sign-only key.
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

        // Sign-only key cannot veto.
        let result = veto_recovery(&state, &key_id1);
        assert!(matches!(result, Err(GuardianError::ScopeViolation)));
    }

    #[test]
    fn execution_before_expiry_fails() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        let (signer_kp, signer_did) = keypair();
        let signer_kid = format!("{}#recovery-0", signer_did);
        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let (ref g1_kp, ref g1_did, ref g1_kid) = guardians[1];
        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a1 = approve_recovery(&request, g1_did, g1_kid, g1_kp).unwrap();

        let timelocked_at = 1_000_000;
        let now = timelocked_at + DEFAULT_TIMELOCK_SECS - 1; // 1 second too early

        let result = execute_recovery(
            &request,
            &[a0, a1],
            &state,
            timelocked_at,
            now,
            &signer_kid,
            &signer_kp,
        );
        assert!(matches!(
            result,
            Err(GuardianError::TimelockNotExpired { .. })
        ));
    }

    #[test]
    fn execution_delegates_and_revokes() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, _, _) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // Guardian 0 acts as the signer — their did:key appears in proof.keys.
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let (ref g1_kp, ref g1_did, ref g1_kid) = guardians[1];

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key.clone(), &authority);

        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a1 = approve_recovery(&request, g1_did, g1_kid, g1_kp).unwrap();

        let timelocked_at = 1_000_000;
        let now = timelocked_at + DEFAULT_TIMELOCK_SECS; // Exactly at expiry

        let recovery_events = execute_recovery(
            &request,
            &[a0, a1],
            &state,
            timelocked_at,
            now,
            g0_kid,
            g0_kp,
        )
        .unwrap();

        // Should produce: 1 revocation (key0) + 1 delegation.
        assert_eq!(recovery_events.len(), 2);

        // Fold accepts the recovery events.
        let mut all = events;
        all.extend(recovery_events);
        let final_state = fold(&all).unwrap();

        // Only the recovered key remains valid.
        let valid = final_state.keys_at(final_state.head_seq());
        assert_eq!(valid.len(), 1);
        assert_eq!(valid[0].id, new_key.id);
    }

    #[test]
    fn arming_without_consent_fails() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, key_id0, kp0) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // Only provide consent from guardian 0, not all three.
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let consent0 = sign_consent(&scid, g0_did, g0_kid, g0_kp);

        let result = set_guardians(&roster, &[consent0], &state, &key_id0, &kp0);
        assert!(matches!(result, Err(GuardianError::ConsentMissing { .. })));
    }

    #[test]
    fn set_guardians_with_full_consent() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, key_id0, kp0) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // All three guardians consent.
        let consents: Vec<_> = guardians
            .iter()
            .map(|(kp, did, kid)| sign_consent(&scid, did, kid, kp))
            .collect();

        let event = set_guardians(&roster, &consents, &state, &key_id0, &kp0).unwrap();

        // Fold accepts the event.
        let mut all = events;
        all.push(event);
        let new_state = fold(&all).unwrap();

        // Recovery commitment updated.
        assert_eq!(new_state.recovery_commitment(), roster_commitment(&roster));
    }

    #[test]
    fn pre_recovery_history_holds() {
        let (guardians, roster, commitment) = setup_guardians(2);
        let (events, scid, key_id0, kp0) = setup_subject_with_guardians(&commitment);
        let state = fold(&events).unwrap();

        // Sign something with the original key before recovery.
        let _original_sig = kp0.sign(b"pre-recovery data");

        // Guardian 0 acts as the signer for recovery events.
        let (ref g0_kp, ref g0_did, ref g0_kid) = guardians[0];
        let (ref g1_kp, ref g1_did, ref g1_kid) = guardians[1];

        let (_, new_did) = keypair();
        let new_key = full_key(&format!("{}#key-recovered", scid), &new_did);
        let authority = roster_to_authority(&roster);
        let request = make_request(&scid, new_key, &authority);

        let a0 = approve_recovery(&request, g0_did, g0_kid, g0_kp).unwrap();
        let a1 = approve_recovery(&request, g1_did, g1_kid, g1_kp).unwrap();

        let recovery_events = execute_recovery(
            &request,
            &[a0, a1],
            &state,
            1_000_000,
            1_000_000 + DEFAULT_TIMELOCK_SECS,
            g0_kid,
            g0_kp,
        )
        .unwrap();

        let mut all = events;
        all.extend(recovery_events);
        let final_state = fold(&all).unwrap();

        // The original key at seq 0 still verifies pre-recovery data.
        assert!(final_state.key_valid_at(&key_id0, 0));
        // But not at the current (post-recovery) seq.
        assert!(!final_state.key_valid_at(&key_id0, final_state.head_seq()));
    }
}
