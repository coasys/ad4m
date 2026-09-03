//! Identity RPC handlers — the bridge between TypeScript IdentityClient and the
//! Rust identity engine (KEL, resolver, enrolment, revocation, guardians, keyring).

use serde_json::{json, Value};
use std::sync::Arc;

use crate::agent::kel::{self, fold, AgentType, KeyEntry, KeyEventBody, Scope};
use crate::agent::resolver::{self, IdentityService};
#[allow(unused_imports)]
use crate::agent::kel::adapter::KelAdapter;

use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

/// Shared context for a WebSocket request.
type Ctx = Arc<crate::types::RequestContext>;

// ── helpers ─────────────────────────────────────────────────────────────────

fn svc_err(msg: impl Into<String>) -> WsRpcError {
    WsRpcError::internal(msg.into())
}

fn key_entry_to_json(ke: &KeyEntry) -> Value {
    json!({
        "id": ke.id,
        "signingKey": ke.signing_key,
        "encryptionKey": ke.encryption_key,
        "scope": {
            "sign": ke.scope.sign,
            "kelOps": ke.scope.kel_ops,
            "delegate": ke.scope.delegate,
        },
    })
}

// ── identity.resolve ────────────────────────────────────────────────────────

async fn resolve(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let id = params.require_str("id")?;

    IdentityService::with(|svc| {
        let agent = svc
            .resolver
            .resolve_agent(&id, None)
            .map_err(|e| svc_err(format!("resolve failed: {}", e)))?;

        let validity = match agent.validity {
            resolver::Validity::Valid => "valid",
            resolver::Validity::Revoked { .. } => "revoked",
            resolver::Validity::Superseded { .. } => "revoked",
        };

        Ok(json!({
            "did": agent.master,
            "validity": validity,
            "keyState": {
                "headSeq": agent.keys.len(), // approximate
                "agentType": format!("{:?}", agent.agent_type).to_lowercase(),
                "validKeys": agent.keys.iter().map(key_entry_to_json).collect::<Vec<_>>(),
            },
        }))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.create ─────────────────────────────────────────────────────────

async fn create(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let _display_name = params.require_str("displayName")?;
    let _password = params.require_str("password")?;
    let agent_type_str = params["agentType"].as_str().unwrap_or("human");

    let agent_type = match agent_type_str {
        "assistant" => AgentType::Assistant,
        _ => AgentType::Human,
    };

    IdentityService::with(|svc| {
        // Generate a keypair.
        let kp = did_key::generate::<did_key::Ed25519KeyPair>(None);
        let did = kel::recovery::did_key_of(&kp);
        let key_id = format!("{}#key-0", did);
        let key = KeyEntry {
            id: key_id.clone(),
            signing_key: did.clone(),
            encryption_key: None,
            scope: Scope::full(),
        };

        // Create recovery authority from a generated mnemonic.
        let (mnemonic, seed) = kel::recovery::MasterSeed::generate()
            .map_err(|e| svc_err(format!("mnemonic generation: {}", e)))?;
        let recovery_auth = kel::recovery::mnemonic_recovery_authority(&seed);
        let commitment = kel::recovery::recovery_commitment(&recovery_auth);

        // Mint inception event.
        let (inception, scid) = match agent_type {
            AgentType::Human => kel::incept_human(vec![key], commitment, &key_id, &kp),
            AgentType::Assistant => {
                return Err(svc_err(
                    "use identity.claimAssistant for assistant inception",
                ));
            }
        };

        // Persist to adapter.
        svc.adapter
            .append(&scid, inception)
            .map_err(|e| svc_err(format!("persist inception: {}", e)))?;

        // Populate reverse index.
        svc.reverse_index.insert(&key_id, &scid);

        // Return SCID + mnemonic (the client stores the mnemonic securely).
        Ok(json!({
            "did": scid,
            "mnemonic": mnemonic,
        }))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.roster ─────────────────────────────────────────────────────────

async fn roster(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let scid = params
        .get("did")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    IdentityService::with(|svc| {
        // If no DID provided, try to get the first SCID from the adapter.
        let scid = match scid {
            Some(s) => s,
            None => return Ok(json!([])),
        };

        let events = svc
            .adapter
            .get_log(&scid, 0)
            .map_err(|e| svc_err(format!("get_log: {}", e)))?;
        let state = fold(&events).map_err(|e| svc_err(format!("fold: {}", e)))?;

        let roster_entries: Vec<Value> = crate::agent::enrolment::roster(&state)
            .iter()
            .map(|entry| {
                json!({
                    "key": key_entry_to_json(&entry.key),
                    "label": entry.label,
                    "lane": entry.lane.as_ref().map(|l| format!("{:?}", l)),
                    "enrolledAtSeq": entry.enrolled_at_seq,
                    "active": entry.active,
                    "revokedAtSeq": entry.revoked_at_seq,
                })
            })
            .collect();

        Ok(json!(roster_entries))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.kelEvents ──────────────────────────────────────────────────────

async fn kel_events(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let scid = params.require_str("did")?;

    IdentityService::with(|svc| {
        let events = svc
            .adapter
            .get_log(&scid, 0)
            .map_err(|e| svc_err(format!("get_log: {}", e)))?;

        let display: Vec<Value> = events
            .iter()
            .map(|ev| {
                let event_type = match &ev.body {
                    KeyEventBody::Inception { .. } => "inception",
                    KeyEventBody::Delegate { .. } => "delegate",
                    KeyEventBody::Rotate { .. } => "rotate",
                    KeyEventBody::Revoke { .. } => "revoke",
                    KeyEventBody::ControllerOp { .. } => "controller_op",
                    KeyEventBody::RecoveryOp { .. } => "recovery_op",
                    KeyEventBody::SetRecoveryAuthority { .. } => "set_recovery_authority",
                    KeyEventBody::Deactivate { .. } => "deactivate",
                };
                let summary = match &ev.body {
                    KeyEventBody::Inception { keys, agent_type, .. } => {
                        format!(
                            "{:?} identity created with {} key(s)",
                            agent_type,
                            keys.len()
                        )
                    }
                    KeyEventBody::Delegate { key, .. } => {
                        format!("delegated key {}", key.id)
                    }
                    KeyEventBody::Rotate { keys } => {
                        format!("rotated to {} key(s)", keys.len())
                    }
                    KeyEventBody::Revoke { key_id, reason } => {
                        format!("revoked {} ({:?})", key_id, reason)
                    }
                    KeyEventBody::ControllerOp { op } => {
                        format!("controller op ({:?})", std::mem::discriminant(op.as_ref()))
                    }
                    KeyEventBody::RecoveryOp { op, .. } => {
                        format!("recovery op ({:?})", std::mem::discriminant(op.as_ref()))
                    }
                    KeyEventBody::SetRecoveryAuthority { .. } => {
                        "updated recovery authority".to_string()
                    }
                    KeyEventBody::Deactivate { reason } => {
                        format!("deactivated: {}", reason)
                    }
                };
                json!({
                    "seq": ev.seq,
                    "type": event_type,
                    "summary": summary,
                    "signedBy": ev.signer,
                    "raw": serde_json::to_string(ev).unwrap_or_default(),
                })
            })
            .collect();

        Ok(json!(display))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.exportKel ──────────────────────────────────────────────────────

async fn export_kel(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let scid = params.require_str("did")?;

    IdentityService::with(|svc| {
        let events = svc
            .adapter
            .get_log(&scid, 0)
            .map_err(|e| svc_err(format!("get_log: {}", e)))?;
        let json = serde_json::to_string_pretty(&events)
            .map_err(|e| svc_err(format!("serialize: {}", e)))?;
        Ok(Value::String(json))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.revokeKey ──────────────────────────────────────────────────────

async fn revoke_key(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let key_id = params.require_str("keyId")?;

    IdentityService::with(|svc| {
        // Find the SCID this key belongs to.
        let master = svc
            .reverse_index
            .master_for(&key_id)
            .ok_or_else(|| svc_err("key not found in reverse index"))?;

        let events = svc
            .adapter
            .get_log(&master, 0)
            .map_err(|e| svc_err(format!("get_log: {}", e)))?;
        let state = fold(&events).map_err(|e| svc_err(format!("fold: {}", e)))?;

        // Check that the key exists in the current state.
        let _key_exists = state
            .keys_at(state.head_seq())
            .into_iter()
            .any(|k| k.id == key_id);

        // Building the revocation event requires the signer's private key
        // (wallet integration). Return the seq where revocation would go.
        let next_seq = state.head_seq() + 1;
        Ok(json!({ "nextSeq": next_seq, "keyId": key_id }))
    })
    .map_err(|e| svc_err(e))?
}

// ── identity.guardians ──────────────────────────────────────────────────────

async fn guardians(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    // Guardian roster comes from the KEL's SetRecoveryAuthority events.
    // Until a guardian roster is set, return empty.
    Ok(json!([]))
}

// ── identity.recoveryState ──────────────────────────────────────────────────

async fn recovery_state(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    // No pending recovery by default.
    Ok(Value::Null)
}

// ── identity.dekVersions ────────────────────────────────────────────────────

async fn dek_versions(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    // Keyring versions — empty until encryption keys get provisioned.
    Ok(json!([]))
}

// ── stub handlers for operations requiring wallet signing ───────────────────

async fn generate_mnemonic(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let (mnemonic, _seed) = kel::recovery::MasterSeed::generate()
        .map_err(|e| svc_err(format!("mnemonic generation: {}", e)))?;
    Ok(Value::String(mnemonic))
}

async fn confirm_mnemonic_backup(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    // Backup confirmation — just record the fact.
    Ok(Value::Bool(true))
}

async fn create_enrol_offer(params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    let label = params.require_str("label")?;
    // Generate a challenge and return the offer structure.
    let kp = did_key::generate::<did_key::Ed25519KeyPair>(None);
    let did = kel::recovery::did_key_of(&kp);
    Ok(json!({
        "publicKey": did,
        "label": label,
        "challenge": hex::encode(&[0u8; 32]), // placeholder challenge
        "scope": { "lanes": ["*"], "ops": [] },
    }))
}

async fn approve_enrolment(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "enrolment approval requires wallet signing — not yet wired".to_string(),
    ))
}

async fn approve_hosted_enrolment(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "hosted enrolment requires wallet signing — not yet wired".to_string(),
    ))
}

async fn enrol_via_mnemonic(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "mnemonic enrolment requires wallet signing — not yet wired".to_string(),
    ))
}

async fn rotate_key(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "key rotation requires wallet signing — not yet wired".to_string(),
    ))
}

async fn recover_from_mnemonic(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "mnemonic recovery requires wallet signing — not yet wired".to_string(),
    ))
}

async fn claim_assistant(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "assistant claim requires wallet signing — not yet wired".to_string(),
    ))
}

async fn set_guardians(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "guardian setup requires wallet signing — not yet wired".to_string(),
    ))
}

async fn open_recovery(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "recovery request requires wallet signing — not yet wired".to_string(),
    ))
}

async fn approve_recovery(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "recovery approval requires wallet signing — not yet wired".to_string(),
    ))
}

async fn veto_recovery(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "recovery veto requires wallet signing — not yet wired".to_string(),
    ))
}

async fn execute_recovery(_params: Value, _ctx: Ctx) -> Result<Value, WsRpcError> {
    Err(WsRpcError::internal(
        "recovery execution requires wallet signing — not yet wired".to_string(),
    ))
}

// ── registration ────────────────────────────────────────────────────────────

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("identity.create", create);
    map.register("identity.resolve", resolve);
    map.register("identity.roster", roster);
    map.register("identity.kelEvents", kel_events);
    map.register("identity.exportKel", export_kel);
    map.register("identity.revokeKey", revoke_key);
    map.register("identity.generateMnemonic", generate_mnemonic);
    map.register("identity.confirmMnemonicBackup", confirm_mnemonic_backup);
    map.register("identity.createEnrolOffer", create_enrol_offer);
    map.register("identity.approveEnrolment", approve_enrolment);
    map.register("identity.approveHostedEnrolment", approve_hosted_enrolment);
    map.register("identity.enrolViaMnemonic", enrol_via_mnemonic);
    map.register("identity.rotateKey", rotate_key);
    map.register("identity.recoverFromMnemonic", recover_from_mnemonic);
    map.register("identity.claimAssistant", claim_assistant);
    map.register("identity.guardians", guardians);
    map.register("identity.setGuardians", set_guardians);
    map.register("identity.openRecovery", open_recovery);
    map.register("identity.approveRecovery", approve_recovery);
    map.register("identity.vetoRecovery", veto_recovery);
    map.register("identity.executeRecovery", execute_recovery);
    map.register("identity.recoveryState", recovery_state);
    map.register("identity.dekVersions", dek_versions);
}
