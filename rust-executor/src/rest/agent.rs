//! Agent REST endpoints: /api/v1/agent/*
//!
//! 19 harmonised endpoints covering agent info, auth, trust, entanglement, and profile.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::agent::{
    did_document_for_context, AgentContext, AgentService, AgentSignature as InternalAgentSignature,
};
use crate::entanglement_service::{
    add_entanglement_proofs, delete_entanglement_proof, get_entanglement_proofs, sign_device_key,
};
use crate::languages::LanguageController;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC};
use crate::types::domain::Perspective as DomainPerspective;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

fn link_input_to_decorated(link_input: &LinkInput) -> DecoratedLinkExpression {
    DecoratedLinkExpression {
        author: String::new(),
        timestamp: String::new(),
        data: Link {
            source: link_input.source.clone(),
            target: link_input.target.clone(),
            predicate: link_input.predicate.clone(),
        },
        proof: DecoratedExpressionProof {
            key: String::new(),
            signature: String::new(),
            valid: None,
            invalid: None,
        },
        status: None,
    }
}

/// GET /agent — current agent info + status + lock state
pub async fn get_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Agent>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Multi-user mode: extract user DID from JWT token if present
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
                perspective: Some(DomainPerspective { links: vec![] }),
            },
        };
        return Ok(Json(agent));
    }

    // Fallback to main agent for admin/legacy mode
    let agent = AgentService::with_global_instance(|agent_service| {
        let mut agent = agent_service
            .agent
            .clone()
            .ok_or_else(|| ApiError::NotFound("Agent not found".into()))?;
        if agent.perspective.is_some() {
            agent.perspective.as_mut().unwrap().verify_link_signatures();
        }
        Ok::<Agent, ApiError>(agent)
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
        .map_err(|e| ApiError::Forbidden(e))?;

    Ok(Json(apps_map::get_apps()))
}

/// GET /agent/by-did/:did — get agent by DID
pub async fn get_agent_by_did(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(did): Path<String>,
) -> Result<Json<Option<Agent>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Check if DID matches main agent
    let did_match = {
        let agent_instance = AgentService::global_instance();
        let agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref = match agent_service.as_ref() {
            Some(a) => a,
            None => return Err(ApiError::NotFound("Agent not initialized".into())),
        };
        match &agent_ref.did {
            Some(existing) => &did == existing,
            None => false,
        }
    };

    if !did_match {
        // Look up the agent expression via the agent language
        let controller = LanguageController::global_instance();
        let agent_lang = controller.get_agent_language().await;
        if let Ok(lang) = agent_lang {
            let lang_address = lang.address().to_string();
            match controller.get_expression(&lang_address, &did).await {
                Ok(Some(expr_json)) => {
                    let agent: Option<Agent> = serde_json::from_value(
                        expr_json
                            .get("data")
                            .cloned()
                            .unwrap_or(serde_json::Value::Null),
                    )
                    .ok();
                    let agent = agent.map(|mut a| {
                        if a.perspective.is_some() {
                            a.perspective.as_mut().unwrap().verify_link_signatures();
                        }
                        a
                    });
                    Ok(Json(agent))
                }
                Ok(None) => Ok(Json(None)),
                Err(e) => {
                    log::warn!("agentByDID: failed to get expression for {}: {}", did, e);
                    Err(ApiError::Internal(format!(
                        "agentByDID: failed to get expression for {}: {}",
                        did, e
                    )))
                }
            }
        } else {
            Ok(Json(None))
        }
    } else {
        let agent = AgentService::with_global_instance(|agent_service| agent_service.agent.clone());
        Ok(Json(agent))
    }
}

/// PATCH /agent/profile — update DM language and/or public perspective
pub async fn update_profile(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<UpdateProfileRequest>,
) -> Result<Json<Agent>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // If dm_language provided, update it
    if let Some(dm_lang) = body.dm_language {
        AgentService::with_mutable_global_instance(|agent_service| {
            if let Some(ref mut agent) = agent_service.agent {
                agent.direct_message_language = Some(dm_lang.clone());
                if let Some(ref passphrase) = agent_service.passphrase {
                    agent_service.save(passphrase.clone());
                }
            }
        });

        // Publish updated agent to agent language
        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!(
                "Failed to publish agent expression after DM language update: {}",
                e
            );
        }
    }

    // If public_perspective provided, update it
    if let Some(pub_persp) = body.public_perspective {
        // For multi-user mode
        if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
            let agent_data = AgentService::get_user_agent_data(&user_email)
                .map_err(|e| ApiError::Internal(format!("User agent not available: {}", e)))?;

            let decorated_links: Vec<DecoratedLinkExpression> = pub_persp
                .links
                .iter()
                .map(|link_input| link_input_to_decorated(link_input))
                .collect();

            let agent = Agent {
                did: agent_data.did,
                direct_message_language: None,
                perspective: Some(DomainPerspective {
                    links: decorated_links,
                }),
            };

            AgentService::with_global_instance(|agent_service| {
                agent_service.store_user_agent_profile(&user_email, &agent)
            })
            .map_err(|e| ApiError::Internal(format!("Failed to store user profile: {}", e)))?;

            if let Err(e) =
                AgentService::publish_agent_to_language(&AgentContext::for_user_email(user_email))
                    .await
            {
                log::warn!(
                    "Failed to publish updated user profile to agent language: {}",
                    e
                );
            }

            return Ok(Json(agent));
        } else {
            // Main agent path
            let decorated_links: Vec<DecoratedLinkExpression> = pub_persp
                .links
                .iter()
                .map(|link_input| link_input_to_decorated(link_input))
                .collect();

            AgentService::with_mutable_global_instance(|agent_service| {
                if let Some(ref mut agent) = agent_service.agent {
                    agent.perspective = Some(DomainPerspective {
                        links: decorated_links,
                    });
                    if let Some(ref passphrase) = agent_service.passphrase {
                        agent_service.save(passphrase.clone());
                    }
                }
            });

            if let Err(e) =
                AgentService::publish_agent_to_language(&AgentContext::main_agent()).await
            {
                log::warn!(
                    "Failed to publish agent expression after profile update: {}",
                    e
                );
            }
        }
    }

    // Return updated agent
    let agent = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent
            .clone()
            .ok_or_else(|| ApiError::NotFound("Agent not found".into()))
    })?;

    // Notify subscribers
    get_global_pubsub()
        .await
        .publish(
            &AGENT_UPDATED_TOPIC,
            &serde_json::to_string(&agent).unwrap(),
        )
        .await;

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
        .map_err(|e| ApiError::Forbidden(e))?;

    let mut agent = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.create_new_keys();

        // Set the direct message language from bootstrap seed
        let dm_language = crate::runtime_service::RuntimeService::with_global_instance(|rt| {
            rt.get_direct_message_language()
        });
        if let Some(ref mut agent) = agent_service.agent {
            agent.direct_message_language = Some(dm_language);
        }

        agent_service.save(body.passphrase.clone());
        agent_service.passphrase = Some(body.passphrase.clone());

        agent_service.dump().clone()
    });

    // Start Holochain conductor
    let config = crate::config::get_global_config();
    let hc_config = crate::holochain_service::LocalConductorConfig::from_ad4m_config(
        &config,
        body.passphrase.clone(),
    );

    let mut init_errors: Vec<String> = Vec::new();

    if let Err(e) = crate::holochain_service::HolochainService::init(hc_config).await {
        log::error!("Error initializing Holochain: {:?}", e);
        init_errors.push(format!("Holochain init failed: {}", e));
    } else {
        log::info!("Holochain init complete");
    }

    // Load system languages
    let language_language_only = config.language_language_only.unwrap_or(false);
    let controller = LanguageController::global_instance();
    if let Err(e) = controller
        .load_system_languages(language_language_only)
        .await
    {
        log::error!("Error loading system languages: {:?}", e);
        init_errors.push(format!("Failed to load system languages: {}", e));
    } else {
        log::info!("System languages loaded");
    }

    // Publish agent expression
    if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
        log::warn!("Error publishing agent expression: {}", e);
    }

    if !init_errors.is_empty() {
        agent.error = Some(init_errors.join("; "));
    }

    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&agent).unwrap(),
        )
        .await;

    log::info!("AD4M init complete");
    Ok(Json(agent))
}

/// POST /agent/lock — lock agent
pub async fn lock_agent(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(body): Json<LockAgentRequest>,
) -> Result<Json<AgentStatus>, ApiError> {
    // No capability check for lock
    let agent = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.lock(body.passphrase.clone());
        agent_service.dump().clone()
    });

    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&agent).unwrap(),
        )
        .await;

    Ok(Json(agent))
}

/// POST /agent/unlock — unlock agent
pub async fn unlock_agent(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<UnlockAgentRequest>,
) -> Result<Json<AgentStatus>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let agent_instance = AgentService::global_instance();
    {
        let mut agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref = agent_service.as_mut().expect("agent instance");
        agent_ref
            .unlock(body.passphrase.clone())
            .map_err(|e| ApiError::Internal(e.to_string()))?;
    }

    let mut init_errors: Vec<String> = Vec::new();

    let is_unlocked = agent_instance
        .lock()
        .expect("agent lock")
        .as_ref()
        .expect("agent instance")
        .is_unlocked();

    if is_unlocked {
        // Start Holochain conductor if not already running
        if crate::holochain_service::maybe_get_holochain_service()
            .await
            .is_none()
        {
            log::info!("Holochain service not initialized. Initializing...");
            let config = crate::config::get_global_config();
            let hc_config = crate::holochain_service::LocalConductorConfig::from_ad4m_config(
                &config,
                body.passphrase.clone(),
            );

            if let Err(e) = crate::holochain_service::HolochainService::init(hc_config).await {
                log::error!("Error initializing Holochain: {:?}", e);
                init_errors.push(format!("Holochain init failed: {}", e));
            } else {
                log::info!("Holochain init complete");
            }
        }

        // Load system languages
        let config = crate::config::get_global_config();
        let language_language_only = config.language_language_only.unwrap_or(false);
        let controller = LanguageController::global_instance();
        if let Err(e) = controller
            .load_system_languages(language_language_only)
            .await
        {
            log::error!("Error loading system languages: {:?}", e);
            init_errors.push(format!("Failed to load system languages: {}", e));
        } else {
            log::info!("System languages loaded");
        }

        log::info!("AD4M init complete");

        // Publish agent expression
        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!("Error publishing agent expression: {}", e);
        }
    }

    let mut agent = {
        let agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref = agent_service.as_ref().expect("agent instance");
        agent_ref.dump().clone()
    };

    if !is_unlocked {
        agent.error = Some("Failed to unlock agent".to_string());
    } else if !init_errors.is_empty() {
        agent.error = Some(init_errors.join("; "));
    }

    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&agent).unwrap(),
        )
        .await;

    Ok(Json(agent))
}

/// POST /agent/sign — sign a message
pub async fn sign_message(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SignMessageRequest>,
) -> Result<Json<AgentSignature>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let sig = InternalAgentSignature::from_message(body.message)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(sig.into()))
}

/// DELETE /agent/apps/:id — remove app
pub async fn remove_app(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(request_id): Path<String>,
) -> Result<Json<Vec<Apps>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    apps_map::remove_app(&request_id).map_err(|e| ApiError::Internal(e))?;
    Ok(Json(apps_map::get_apps()))
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
        .map_err(|e| ApiError::Forbidden(e))?;

    let auth_info: AuthInfo = body
        .auth_info
        .try_into()
        .map_err(|e: String| ApiError::BadRequest(e))?;
    let request_id = crate::agent::capabilities::request_capability(auth_info.clone()).await;

    if context.auto_permit_cap_requests {
        println!("======================================");
        println!("Got capability request: \n{:?}", auth_info);
        let random_number_challenge =
            crate::agent::capabilities::permit_capability(AuthInfoExtended {
                request_id: request_id.clone(),
                auth: auth_info,
            })
            .map_err(|e| ApiError::Internal(e))?;
        println!("--------------------------------------");
        println!("Random number challenge: {}", random_number_challenge);
        println!("======================================");
    }

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
        .map_err(|e| ApiError::Forbidden(e))?;

    let auth: AuthInfoExtended = serde_json::from_str(&body.auth)
        .map_err(|e| ApiError::BadRequest(format!("Invalid auth info: {}", e)))?;
    let random_number_challenge =
        crate::agent::capabilities::permit_capability(auth).map_err(|e| ApiError::Internal(e))?;
    Ok(Json(random_number_challenge))
}

/// POST /agent/auth/jwt — generate JWT
pub async fn generate_jwt(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<GenerateJwtRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let cap_token = generate_capability_token(body.request_id, body.rand)
        .await
        .map_err(|e| ApiError::Internal(e))?;
    Ok(Json(cap_token))
}

/// DELETE /agent/auth/token/:token — revoke token
pub async fn revoke_token(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(request_id): Path<String>,
) -> Result<Json<Vec<Apps>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    apps_map::revoke_app(&request_id).map_err(|e| ApiError::Internal(e))?;
    Ok(Json(apps_map::get_apps()))
}

// ── Status ──

/// GET /agent/status — agent status
pub async fn get_agent_status(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<AgentStatus>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Multi-user mode
    if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
        let agent_data = AgentService::get_user_agent_data(&user_email)
            .map_err(|e| ApiError::Internal(format!("User agent not available: {}", e)))?;

        let agent_context = AgentContext::for_user_email(user_email);
        let did_document = did_document_for_context(&agent_context).map_err(|e| {
            ApiError::Internal(format!("Failed to get DID document for user: {}", e))
        })?;

        return Ok(Json(AgentStatus {
            did: Some(agent_data.did),
            did_document: Some(serde_json::to_string(&did_document).map_err(|e| {
                ApiError::Internal(format!("Failed to serialize DID document: {}", e))
            })?),
            error: None,
            is_initialized: true,
            is_unlocked: true,
        }));
    }

    // Fallback to main agent status
    let status = AgentService::with_global_instance(|agent_service| agent_service.dump());
    Ok(Json(status))
}

/// GET /agent/is-locked — check if agent is locked
pub async fn is_locked(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<bool>, ApiError> {
    let locked = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent
            .clone()
            .ok_or_else(|| ApiError::NotFound("Agent not found".into()))?;
        Ok::<bool, ApiError>(!agent_service.is_unlocked())
    })?;
    Ok(Json(locked))
}

// ── Trust ──

/// GET /agent/trusted — list trusted agents
pub async fn get_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let agents = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(Json(agents))
}

/// PUT /agent/trusted — add trusted agents
pub async fn add_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(agents): Json<Vec<String>>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.add_trusted_agent(agents);
    });

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(Json(result))
}

/// DELETE /agent/trusted — remove trusted agents
pub async fn delete_trusted_agents(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(agents): Json<Vec<String>>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.remove_trusted_agent(agents);
    });

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(Json(result))
}

// ── Entanglement ──

/// GET /agent/entanglement-proofs — list
pub async fn get_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let proofs = get_entanglement_proofs();
    Ok(Json(
        proofs
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect(),
    ))
}

/// POST /agent/entanglement-proofs — add (with ?preflight=true option)
pub async fn add_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
    axum::extract::Query(params): axum::extract::Query<std::collections::HashMap<String, String>>,
    Json(body): Json<Vec<EntanglementProofInput>>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let preflight = params
        .get("preflight")
        .map(|v| v == "true")
        .unwrap_or(false);

    if preflight {
        // Pre-flight: just validate
        let signed = sign_device_key(
            body.first()
                .map(|b| b.device_key.clone())
                .unwrap_or_default(),
            body.first()
                .map(|b| b.device_key_type.clone())
                .unwrap_or_default(),
        );
        return Ok(Json(vec![serde_json::to_value(signed).unwrap_or_default()]));
    }

    // add_entanglement_proofs returns () not Result, and takes domain EntanglementProof
    let agent_did = AgentService::with_global_instance(|a| a.did.clone().unwrap_or_default());
    let agent_key_id =
        AgentService::with_global_instance(|a| a.signing_key_id.clone().unwrap_or_default());
    let domain_proofs: Vec<EntanglementProof> = body
        .into_iter()
        .map(|p| EntanglementProof {
            device_key: p.device_key,
            device_key_type: p.device_key_type,
            device_key_signed_by_did: p.device_key_signed_by_did,
            did_signed_by_device_key: p.did_signed_by_device_key,
            did: agent_did.clone(),
            did_signing_key_id: agent_key_id.clone(),
        })
        .collect();
    add_entanglement_proofs(domain_proofs.clone());

    Ok(Json(
        domain_proofs
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect(),
    ))
}

/// DELETE /agent/entanglement-proofs — delete
pub async fn delete_entanglement(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(body): Json<Vec<EntanglementProofInput>>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let agent_did = AgentService::with_global_instance(|a| a.did.clone().unwrap_or_default());
    let agent_key_id =
        AgentService::with_global_instance(|a| a.signing_key_id.clone().unwrap_or_default());
    let domain_proofs: Vec<EntanglementProof> = body
        .into_iter()
        .map(|p| EntanglementProof {
            device_key: p.device_key,
            device_key_type: p.device_key_type,
            device_key_signed_by_did: p.device_key_signed_by_did,
            did_signed_by_device_key: p.did_signed_by_device_key,
            did: agent_did.clone(),
            did_signing_key_id: agent_key_id.clone(),
        })
        .collect();
    delete_entanglement_proof(domain_proofs.clone());

    Ok(Json(
        domain_proofs
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect(),
    ))
}
