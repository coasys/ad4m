//! Device and executor enrolment — PR4.
//!
//! One identity, many executors. Keys generate locally on each device and bind
//! by a signed delegation event carrying an explicit scope. Nothing in executor
//! bootstrap changes: executors keep minting `did:key` locally exactly as today,
//! and the identity module acts as an **overlay** that claims existing keys.
//!
//! All enrolment paths (QR, mnemonic, hosted request) converge on one approval
//! surface and one event type: `KeyEventBody::Delegate`.

use crate::agent::kel::adapter::AdapterError;
use crate::agent::kel::{KeyEntry, KeyEvent, KeyEventBody, KeyState, Lane, Scope};
use did_key::PatchedKeyPair;
use std::collections::HashSet;
use std::sync::RwLock;

// ─── enrolment offer ────────────────────────────────────────────────────────

/// What a new device presents for enrolment. Public keys only — private keys
/// never leave the device.
#[derive(Debug, Clone)]
pub struct EnrolOffer {
    /// Ed25519 signing key, `did:key`-encoded, freshly generated on the device.
    pub signing_key: String,
    /// X25519 encryption public key.
    pub encryption_key: String,
    /// Freshness challenge — prevents replay.
    pub challenge: [u8; 32],
    /// Human-readable device name (e.g. "MacBook — local executor").
    pub label: String,
    /// Device/executor category.
    pub lane: Lane,
}

/// Where the enrolment request originated.
#[derive(Debug, Clone)]
pub enum RequestOrigin {
    /// Scanned from a QR code shown by the new device.
    Qr,
    /// A hosted platform executor filed a request.
    HostedExecutor { provider: String },
    /// The user entered the mnemonic on the new device directly.
    Mnemonic,
}

/// An enrolment request — an offer plus its origin.
#[derive(Debug, Clone)]
pub struct EnrolRequest {
    pub offer: EnrolOffer,
    pub origin: RequestOrigin,
}

// ─── consumed challenges ────────────────────────────────────────────────────

/// Tracks consumed challenges. Prevents replay — a re-presented offer fails.
#[derive(Default)]
pub struct ConsumedChallenges {
    consumed: RwLock<HashSet<[u8; 32]>>,
}

impl ConsumedChallenges {
    pub fn new() -> Self {
        Self::default()
    }

    /// Attempt to consume a challenge. Returns `true` if fresh (first use),
    /// `false` if already consumed (replay).
    pub fn try_consume(&self, challenge: &[u8; 32]) -> bool {
        if let Ok(mut set) = self.consumed.write() {
            set.insert(*challenge)
        } else {
            false
        }
    }

    /// Check whether a challenge has already been consumed.
    pub fn already_consumed(&self, challenge: &[u8; 32]) -> bool {
        self.consumed
            .read()
            .ok()
            .is_some_and(|set| set.contains(challenge))
    }
}

// ─── errors ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum EnrolError {
    /// Challenge already consumed — replay attempt.
    ReplayedChallenge,
    /// Approver does not hold `kel_ops` scope or recovery authority.
    SignerLacksAuthority,
    /// Scope combination not permitted.
    InvalidScope(String),
    /// Could not publish the delegation event to the adapter.
    AdapterFailure(AdapterError),
}

impl std::fmt::Display for EnrolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnrolError::ReplayedChallenge => write!(f, "replayed challenge"),
            EnrolError::SignerLacksAuthority => {
                write!(f, "approver lacks kel_ops or recovery authority")
            }
            EnrolError::InvalidScope(msg) => write!(f, "invalid scope: {}", msg),
            EnrolError::AdapterFailure(e) => write!(f, "adapter failure: {}", e),
        }
    }
}

impl std::error::Error for EnrolError {}

// ─── approval ───────────────────────────────────────────────────────────────

/// Approve an enrolment offer — build a signed `Delegate` event.
///
/// The approver (signer) must hold `kel_ops` scope in the current key state,
/// or the request must come from the recovery authority (not checked here —
/// fold validates scope).
///
/// Returns the `KeyEvent` ready for publication via an adapter. The caller
/// must append it to the log.
pub fn approve_enrolment(
    offer: &EnrolOffer,
    scope: Scope,
    state: &KeyState,
    signer_key_id: &str,
    signer: &PatchedKeyPair,
    consumed: &ConsumedChallenges,
) -> Result<KeyEvent, EnrolError> {
    // 1. Check challenge freshness.
    if !consumed.try_consume(&offer.challenge) {
        return Err(EnrolError::ReplayedChallenge);
    }

    // 2. Validate scope — hosted/platform keys MUST receive sign-only.
    if matches!(offer.lane, Lane::HostedExecutor | Lane::CommunityNode)
        && (scope.kel_ops || scope.delegate)
    {
        return Err(EnrolError::InvalidScope(
            "hosted/platform keys must receive sign-only scope".into(),
        ));
    }

    // 3. Early authority check — the signer needs kel_ops + delegate scope.
    //    fold() enforces this too, but catching it here gives a clear error.
    let signer_has_authority = state
        .key_history
        .iter()
        .any(|kv| {
            kv.entry.id == signer_key_id
                && kv.revoked_at.is_none()
                && kv.entry.scope.kel_ops
                && kv.entry.scope.delegate
        });
    if !signer_has_authority {
        return Err(EnrolError::SignerLacksAuthority);
    }

    // 4. Build the key entry and delegate event.
    let next_seq = state.head_seq() + 1;
    let key_id = format!("{}#key-{}", state.master, state.key_count());

    let key_entry = KeyEntry {
        id: key_id,
        signing_key: offer.signing_key.clone(),
        encryption_key: Some(offer.encryption_key.clone()),
        scope,
    };

    let body = KeyEventBody::Delegate {
        key: key_entry,
        from_seq: next_seq,
        label: Some(offer.label.clone()),
        lane: Some(offer.lane.clone()),
    };

    let event = KeyEvent::new(
        next_seq,
        Some(state.head_hash().to_string()),
        body,
        signer_key_id,
        signer,
    );

    Ok(event)
}

// ─── roster ─────────────────────────────────────────────────────────────────

/// What the roster shows for each device/executor — derived purely from the KEL.
#[derive(Debug, Clone)]
pub struct RosterEntry {
    /// The key entry.
    pub key: KeyEntry,
    /// Human-readable device name.
    pub label: Option<String>,
    /// Device/executor lane.
    pub lane: Option<Lane>,
    /// The sequence at which this key joined.
    pub enrolled_at_seq: u64,
    /// Whether the key remains active.
    pub active: bool,
    /// The sequence at which revocation happened (if revoked).
    pub revoked_at_seq: Option<u64>,
}

/// Derive the device/executor roster from a folded `KeyState`.
/// Pure function — no I/O, no separate source of truth.
pub fn roster(state: &KeyState) -> Vec<RosterEntry> {
    state
        .key_history
        .iter()
        .map(|kv| RosterEntry {
            key: kv.entry.clone(),
            label: kv.label.clone(),
            lane: kv.lane.clone(),
            enrolled_at_seq: kv.delegated_at,
            active: kv.revoked_at.is_none(),
            revoked_at_seq: kv.revoked_at,
        })
        .collect()
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
    use crate::agent::resolver::{AgentLanguageResolver, ReverseIndex};
    use crate::agent::kel::adapter::{KelAdapter, MonotonicityCache};
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

    fn sign_only_key(id: &str, signing_key: &str, enc_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: Some(enc_key.to_string()),
            scope: Scope::sign_only(),
        }
    }

    fn dummy_commitment() -> String {
        recovery::recovery_commitment(&RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkDummy".to_string()],
        })
    }

    fn make_offer(signing_key: &str) -> EnrolOffer {
        let mut challenge = [0u8; 32];
        rand::RngCore::fill_bytes(&mut rand::thread_rng(), &mut challenge);
        EnrolOffer {
            signing_key: signing_key.to_string(),
            encryption_key: "x25519-placeholder".to_string(),
            challenge,
            label: "Test Device".to_string(),
            lane: Lane::LocalDevice,
        }
    }

    fn setup_identity() -> (
        Vec<KeyEvent>,
        String,   // scid
        String,   // key_id0
        did_key::PatchedKeyPair,
    ) {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);
        (vec![ev0], scid, key_id0, kp0)
    }

    // ── spec tests ──────────────────────────────────────────────────────────

    #[test]
    fn two_executors_one_master() {
        let (events, scid, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        // Enrol device A.
        let (_, did_a) = keypair();
        let offer_a = make_offer(&did_a);
        let ev_a = approve_enrolment(
            &offer_a,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        // Fold with the new event.
        let mut all_events = events.clone();
        all_events.push(ev_a);
        let state2 = fold(&all_events).unwrap();

        // Enrol device B.
        let (_, did_b) = keypair();
        let offer_b = make_offer(&did_b);
        let ev_b = approve_enrolment(
            &offer_b,
            Scope::sign_only(),
            &state2,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        all_events.push(ev_b);
        let state3 = fold(&all_events).unwrap();

        // Both devices resolve to the same master.
        assert_eq!(state3.keys_at(state3.head_seq()).len(), 3); // inception + A + B
        assert_eq!(state3.master, scid);

        // Verify via the resolver — both delegated keys map to master.
        let adapter = Arc::new(MemoryAdapter::new());
        adapter.seed(&scid, all_events);
        let cache = Arc::new(MonotonicityCache::new());
        let reverse_index = Arc::new(ReverseIndex::new());
        let resolver = AgentLanguageResolver::new(adapter, cache, reverse_index);

        // Resolve via master → populates reverse index.
        let agent = resolver.resolve_agent(&scid, None).unwrap();
        assert_eq!(agent.keys.len(), 3);

        // Resolve via each device key → same master.
        let key_a = &state3.keys_at(state3.head_seq())[1];
        let agent_a = resolver.resolve_agent(&key_a.id, None).unwrap();
        assert_eq!(agent_a.master, scid);

        let key_b = &state3.keys_at(state3.head_seq())[2];
        let agent_b = resolver.resolve_agent(&key_b.id, None).unwrap();
        assert_eq!(agent_b.master, scid);
    }

    #[test]
    fn enrolment_no_private_key() {
        // An EnrolOffer contains only public keys — no private material.
        // This test inspects the offer and the resulting event.
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        let (_, did_dev) = keypair();
        let offer = make_offer(&did_dev);

        // The offer carries only public data.
        assert!(offer.signing_key.starts_with("did:key:"));
        // No secret material in the struct (challenge = random nonce, not a key).

        let ev = approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        // The resulting event carries only the public key.
        match &ev.body {
            KeyEventBody::Delegate { key, .. } => {
                assert!(key.signing_key.starts_with("did:key:"));
                assert!(key.encryption_key.is_some());
            }
            _ => panic!("expected Delegate event"),
        }
    }

    #[test]
    fn replayed_challenge_fails() {
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        let (_, did_dev) = keypair();
        let offer = make_offer(&did_dev);

        // First approval succeeds.
        approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        // Second approval with the same challenge fails.
        let result = approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        );
        assert!(matches!(result, Err(EnrolError::ReplayedChallenge)));
    }

    #[test]
    fn sign_only_cannot_delegate() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate a sign-only key.
        let (kp1, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let body1 = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1, "enc-placeholder"),
            from_seq: 1,
            label: Some("device".to_string()),
            lane: Some(Lane::LocalDevice),
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // Now try to use that sign-only key to delegate another key.
        let (_, did2) = keypair();
        let body2 = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-2", did2), &did2),
            from_seq: 2,
            label: None,
            lane: None,
        };
        let ev2 = KeyEvent::new(2, Some(ev1.hash.clone()), body2, &key_id1, &kp1);

        // fold rejects — sign-only lacks delegate scope.
        let result = fold(&[ev0, ev1, ev2]);
        assert!(matches!(
            result,
            Err(crate::agent::kel::KelError::UnauthorizedSigner { .. })
        ));
    }

    #[test]
    fn sign_only_cannot_revoke() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, _scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate a sign-only key.
        let (kp1, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let body1 = KeyEventBody::Delegate {
            key: sign_only_key(&key_id1, &did1, "enc-placeholder"),
            from_seq: 1,
            label: None,
            lane: None,
        };
        let ev1 = KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // The sign-only key tries to revoke the inception key.
        let body2 = KeyEventBody::Revoke {
            key_id: key_id0.clone(),
            reason: RevocationReason::Compromised,
        };
        let ev2 = KeyEvent::new(2, Some(ev1.hash.clone()), body2, &key_id1, &kp1);

        // fold rejects — sign-only lacks kel_ops scope.
        let result = fold(&[ev0, ev1, ev2]);
        assert!(matches!(
            result,
            Err(crate::agent::kel::KelError::ScopeViolation { .. })
        ));
    }

    #[test]
    fn hosted_same_event() {
        // Hosted executor enrolment produces the exact same Delegate event type
        // as QR enrolment — one approval surface for everything.
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        // QR enrolment.
        let (_, did_qr) = keypair();
        let offer_qr = EnrolOffer {
            signing_key: did_qr,
            encryption_key: "x25519-qr".to_string(),
            challenge: [1u8; 32],
            label: "QR device".to_string(),
            lane: Lane::LocalDevice,
        };
        let ev_qr = approve_enrolment(
            &offer_qr,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        // Hosted executor enrolment.
        let (_, did_hosted) = keypair();
        let offer_hosted = EnrolOffer {
            signing_key: did_hosted,
            encryption_key: "x25519-hosted".to_string(),
            challenge: [2u8; 32],
            label: "Coasys platform executor".to_string(),
            lane: Lane::HostedExecutor,
        };
        // Hosted must receive sign-only.
        let ev_hosted = approve_enrolment(
            &offer_hosted,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        // Both produce Delegate events.
        assert!(matches!(ev_qr.body, KeyEventBody::Delegate { .. }));
        assert!(matches!(ev_hosted.body, KeyEventBody::Delegate { .. }));
    }

    #[test]
    fn hosted_cannot_receive_kel_ops() {
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        let (_, did_hosted) = keypair();
        let offer = EnrolOffer {
            signing_key: did_hosted,
            encryption_key: "x25519-hosted".to_string(),
            challenge: [3u8; 32],
            label: "Hosted executor".to_string(),
            lane: Lane::HostedExecutor,
        };

        // Attempt to give kel_ops to a hosted executor — must fail.
        let result = approve_enrolment(
            &offer,
            Scope::full(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        );
        assert!(matches!(result, Err(EnrolError::InvalidScope(_))));
    }

    #[test]
    fn delegation_carries_x25519() {
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        let (_, did_dev) = keypair();
        let offer = EnrolOffer {
            signing_key: did_dev,
            encryption_key: "x25519-device-enc-key".to_string(),
            challenge: [4u8; 32],
            label: "Device".to_string(),
            lane: Lane::LocalDevice,
        };

        let ev = approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        match &ev.body {
            KeyEventBody::Delegate { key, .. } => {
                assert_eq!(key.encryption_key.as_deref(), Some("x25519-device-enc-key"));
            }
            _ => panic!("expected Delegate"),
        }
    }

    #[test]
    fn roster_from_fold() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);
        let mut events = vec![ev0.clone()];

        // Delegate 3 devices.
        for i in 1..=3u8 {
            let (_, did_dev) = keypair();
            let state = fold(&events).unwrap();
            let body = KeyEventBody::Delegate {
                key: sign_only_key(
                    &format!("{}#key-{}", scid, i),
                    &did_dev,
                    &format!("enc-{}", i),
                ),
                from_seq: i as u64,
                label: Some(format!("Device {}", i)),
                lane: Some(Lane::LocalDevice),
            };
            let ev = KeyEvent::new(
                i as u64,
                Some(state.head_hash().to_string()),
                body,
                &key_id0,
                &kp0,
            );
            events.push(ev);
        }

        // Revoke device 2.
        let state = fold(&events).unwrap();
        let body_revoke = KeyEventBody::Revoke {
            key_id: format!("{}#key-2", scid),
            reason: RevocationReason::Retired,
        };
        let ev_revoke = KeyEvent::new(
            4,
            Some(state.head_hash().to_string()),
            body_revoke,
            &key_id0,
            &kp0,
        );
        events.push(ev_revoke);

        let state = fold(&events).unwrap();
        let entries = roster(&state);

        // 4 entries total: inception key + 3 delegations.
        assert_eq!(entries.len(), 4);

        // Inception key — active, no label/lane.
        assert!(entries[0].active);
        assert!(entries[0].label.is_none());

        // Device 1 — active.
        assert!(entries[1].active);
        assert_eq!(entries[1].label.as_deref(), Some("Device 1"));
        assert_eq!(entries[1].lane, Some(Lane::LocalDevice));

        // Device 2 — revoked.
        assert!(!entries[2].active);
        assert_eq!(entries[2].revoked_at_seq, Some(4));

        // Device 3 — active.
        assert!(entries[3].active);
    }

    #[test]
    fn pre_enrolment_link_verifies() {
        // A link signed before key K gets delegated should still verify
        // after delegation — delegation doesn't affect pre-existing history.
        let (events, scid, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();

        // The inception key signs a message at seq 0.
        let msg = b"data signed before any delegation";
        use did_key::CoreSign;
        let sig = hex::encode(kp0.sign(msg));

        // Delegate a new key.
        let consumed = ConsumedChallenges::new();
        let (_, did_dev) = keypair();
        let offer = make_offer(&did_dev);
        let ev = approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &key_id0,
            &kp0,
            &consumed,
        )
        .unwrap();

        let mut all_events = events;
        all_events.push(ev);
        let state2 = fold(&all_events).unwrap();

        // The inception key still verifies at seq 0.
        assert!(state2.key_valid_at(&key_id0, 0));
        // And at the current head.
        assert!(state2.key_valid_at(&key_id0, state2.head_seq()));

        // Verify the signature.
        let kp_verify =
            PatchedKeyPair::try_from(state.keys_at(0)[0].signing_key.as_str()).unwrap();
        let sig_bytes = hex::decode(&sig).unwrap();
        assert!(kp_verify.verify(msg, &sig_bytes).is_ok());
    }

    #[test]
    fn signer_without_authority_fails() {
        let (events, _, key_id0, kp0) = setup_identity();
        let state = fold(&events).unwrap();
        let consumed = ConsumedChallenges::new();

        // Generate a device key (not in the KEL).
        let (kp_rogue, did_rogue) = keypair();
        let rogue_key_id = format!("{}#rogue", did_rogue);

        let (_, did_dev) = keypair();
        let offer = make_offer(&did_dev);

        // Rogue key attempts to approve — should fail.
        let result = approve_enrolment(
            &offer,
            Scope::sign_only(),
            &state,
            &rogue_key_id,
            &kp_rogue,
            &consumed,
        );
        assert!(matches!(result, Err(EnrolError::SignerLacksAuthority)));
    }
}
