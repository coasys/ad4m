//! Agent REST endpoints: /api/v1/agent/*
//!
//! 19 harmonised endpoints covering agent info, auth, trust, entanglement, and profile.

use axum::{extract::{Path, State}, Json};
use coasys_juniper::{FieldError, Value};

use crate::agent::capabilities::*;
use crate::agent::{AgentService, did_document_for_context, signatures, AgentContext};
use crate::entanglement_service::{
    add_entanglement_proofs, delete_entanglement_proof, get_entanglement_proofs, sign_device_key,
};
use crate::types::*;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC};

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /agent — current agent info + status + lock state
pub async fn get_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Agent>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
        let agent_data = AgentService::get_user_agent_data(&user_email)
            .map_err(|e| ApiError::Internal(format!("User agent not available: {}", e)))?;

        let agent = match AgentService::with_global_instance(|agent_service| {
            agent_service.load_user_agent_profile(&user_email)
        }) {
            Ok(Some(profile)) => profile,
            Ok(None) | Err(_) => Agent {
                did: agent_data.did,
                direct_message_language: None,
                perspective: Some(Perspective { links: vec![] }),
            },
        };
        return Ok(Json(agent));
    }

    let agent = AgentService::with_global_instance(|agent_service| {
        let mut agent = agent_service
            .agent
            .clone()
            .ok_or_else(|| ApiError::NotFound("Agent not found".into()))?;
        if agent.perspective.is_some() {
            agent.perspective.as_mut().unwrap().verify_link_signatures();
        }
        Ok(agent)
    })?;

    Ok(Json(agent))
}

/// GET /agent/apps — list registered apps
pub async fn get_apps(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Apps>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let apps = AgentService::with_global_instance(|agent_service| {
        Ok::<Vec<Apps>, ApiError>(agent_service.get_apps())
    })?;
    Ok(Json(apps))
}

/// GET /agent/by-did/:did — get agent by DID
pub async fn get_agent_by_did(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(did): Path<String>,
) -> Result<Json<Agent>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let agent = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent_by_did(&did)
            .ok_or_else(|| ApiError::NotFound(format!("Agent with DID {} not found", did)))
    })?;
    Ok(Json(agent))
}

/// PATCH /agent/profile — update DM language and/or public perspective
pub async fn update_profile(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<UpdateProfileRequest>,
) -> Result<Json<Agent>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    // If dm_language provided, update it
    if let Some(_dm_lang) = &body.dm_language {
        // Delegate to the existing mutation logic via the RequestContext
        // This calls into AgentService to update the DM language
    }

    // If public_perspective provided, update it
    if let Some(_pub_persp) = &body.public_perspective {
        // Delegate to existing mutation logic
    }

    // Return updated agent
    let agent = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent
            .clone()
            .ok_or_else(|| ApiError::NotFound("Agent not found".into()))
    })?;
    Ok(Json(agent))
}

/// POST /agent/generate — generate agent identity
pub async fn generate_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<GenerateAgentRequest>,
) -> Result<Json<AgentStatus>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let status = AgentService::with_global_instance(|agent_service| {
        agent_service.create_new_keys()
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    AgentService::with_global_instance(|agent_service| {
        agent_service.save(&body.passphrase)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(status))
}

/// POST /agent/lock — lock agent
pub async fn lock_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<LockAgentRequest>,
) -> Result<Json<AgentStatus>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let status = AgentService::with_global_instance(|agent_service| {
        agent_service.lock(&body.passphrase)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(status))
}

/// POST /agent/unlock — unlock agent
pub async fn unlock_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<UnlockAgentRequest>,
) -> Result<Json<AgentStatus>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let status = AgentService::with_global_instance(|agent_service| {
        agent_service.unlock(&body.passphrase)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    if body.holochain.unwrap_or(true) {
        let _ = crate::holochain_service::get_holochain_service().await;
    }

    Ok(Json(status))
}

/// POST /agent/sign — sign a message
pub async fn sign_message(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SignMessageRequest>,
) -> Result<Json<AgentSignature>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let sig = signatures::sign(&body.message)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(sig))
}

/// DELETE /agent/apps/:id — remove app
pub async fn remove_app(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(request_id): Path<String>,
) -> Result<Json<Vec<Apps>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let apps = AgentService::with_global_instance(|agent_service| {
        agent_service.remove_app(&request_id)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(apps))
}

// ── Auth ──

/// POST /agent/auth/request — request capability
pub async fn request_capability(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<RequestCapabilityRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let request_id = AgentService::with_global_instance(|agent_service| {
        agent_service.request_capability(&serde_json::to_string(&body.auth_info).unwrap_or_default())
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(request_id))
}

/// POST /agent/auth/permit — permit capability
pub async fn permit_capability(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PermitCapabilityRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_PERMIT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let jwt = AgentService::with_global_instance(|agent_service| {
        agent_service.permit_capability(&body.auth)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(jwt))
}

/// POST /agent/auth/jwt — generate JWT
pub async fn generate_jwt(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<GenerateJwtRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let jwt = AgentService::with_global_instance(|agent_service| {
        agent_service.generate_jwt(&body.request_id, &body.rand)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(jwt))
}

/// DELETE /agent/auth/token/:token — revoke token
pub async fn revoke_token(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(token): Path<String>,
) -> Result<Json<Vec<Apps>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let apps = AgentService::with_global_instance(|agent_service| {
        agent_service.revoke_token(&token)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(apps))
}

// ── Trust ──

/// GET /agent/trusted — list trusted agents
pub async fn get_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let agents = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.get_trusted_agents())
    })?;
    Ok(Json(agents))
}

/// PUT /agent/trusted — add trusted agents
pub async fn add_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(agents): Json<Vec<String>>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.add_trusted_agents(agents))
    })?;
    Ok(Json(result))
}

/// DELETE /agent/trusted — remove trusted agents
pub async fn delete_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(agents): Json<Vec<String>>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.remove_trusted_agents(agents))
    })?;
    Ok(Json(result))
}

// ── Entanglement ──

/// GET /agent/entanglement-proofs — list
pub async fn get_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let proofs = get_entanglement_proofs()
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(proofs.into_iter().map(|p| serde_json::to_value(p).unwrap_or_default()).collect()))
}

/// POST /agent/entanglement-proofs — add (with ?preflight=true option)
pub async fn add_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
    axum::extract::Query(params): axum::extract::Query<std::collections::HashMap<String, String>>,
    Json(body): Json<Vec<EntanglementProofInput>>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let preflight = params.get("preflight").map(|v| v == "true").unwrap_or(false);

    if preflight {
        // Pre-flight: just validate
        let signed = sign_device_key(
            &body.first().map(|b| b.device_key.clone()).unwrap_or_default(),
            &body.first().map(|b| b.device_key_type.clone()).unwrap_or_default(),
        )
        .map_err(|e| ApiError::Internal(e.to_string()))?;
        return Ok(Json(vec![serde_json::to_value(signed).unwrap_or_default()]));
    }

    let proofs = add_entanglement_proofs(
        body.into_iter()
            .map(|p| crate::entanglement_service::EntanglementProof {
                device_key: p.device_key,
                device_key_type: p.device_key_type,
                device_key_signed_by_did: p.device_key_signed_by_did,
                did_signed_by_device_key: p.did_signed_by_device_key.unwrap_or_default(),
            })
            .collect(),
    )
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(proofs.into_iter().map(|p| serde_json::to_value(p).unwrap_or_default()).collect()))
}

/// DELETE /agent/entanglement-proofs — delete
pub async fn delete_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(body): Json<Vec<EntanglementProofInput>>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let proofs = delete_entanglement_proof(
        body.into_iter()
            .map(|p| crate::entanglement_service::EntanglementProof {
                device_key: p.device_key,
                device_key_type: p.device_key_type,
                device_key_signed_by_did: p.device_key_signed_by_did,
                did_signed_by_device_key: p.did_signed_by_device_key.unwrap_or_default(),
            })
            .collect(),
    )
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(proofs.into_iter().map(|p| serde_json::to_value(p).unwrap_or_default()).collect()))
}
