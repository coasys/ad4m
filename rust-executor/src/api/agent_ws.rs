//! Agent WS-native handlers.
//!
//! 19 handlers covering agent info, auth, trust, entanglement, and profile.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::agent::{
    did_document_for_context, AgentContext, AgentService, AgentSignature as InternalAgentSignature,
};
use crate::entanglement_service::{
    add_entanglement_proofs, delete_entanglement_proof, get_entanglement_proofs, sign_device_key,
};
use crate::holochain_service::conductor_startup::spawn_conductor_startup;
use crate::languages::LanguageController;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC};
use crate::types::domain::Perspective as DomainPerspective;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

/// Convert a client-supplied profile link into the decorated form, **deriving
/// the validity verdict on this replica** rather than believing the caller.
///
/// The wire type [`ExpressionProof`] carries only `key` and `signature`: it has
/// no validity field, precisely so that a peer cannot assert one.
/// `LinkExpressionInput` does carry `valid`/`invalid`, and copying them across
/// re-introduces by hand the field the wire format deliberately omits — a
/// caller could then hand us a garbage signature with `valid: true` and we
/// would store that verdict. Routing through
/// `DecoratedLinkExpression::from((LinkExpression, LinkStatus))` runs
/// `verify()` here and fails closed when verification errors, which is what
/// every other ingest path already does.
///
/// A missing key or signature is still accepted, as before — it simply cannot
/// verify, so it lands as `valid: false`. This is deliberately not an error:
/// rejecting the write would change which profile updates succeed, and the
/// defect being fixed is the trustworthiness of the verdict, not the
/// tolerance of the endpoint.
fn link_expression_input_to_decorated(lei: &LinkExpressionInput) -> DecoratedLinkExpression {
    let unverified = LinkExpression {
        author: lei.author.clone(),
        timestamp: lei.timestamp.clone(),
        data: Link {
            source: lei.data.source.clone(),
            target: lei.data.target.clone(),
            predicate: lei.data.predicate.clone(),
        },
        proof: ExpressionProof {
            key: lei.proof.key.clone().unwrap_or_default(),
            signature: lei.proof.signature.clone().unwrap_or_default(),
        },
        status: lei.status.clone(),
    };
    // `LinkStatus::Shared` is the enum's own `#[default]`, and the conversion
    // stores `Some(status)`. Profile links from a client that sent no status
    // therefore become `SHARED` instead of staying `None`, which matches what
    // every other link in the store looks like.
    let status = lei.status.clone().unwrap_or_default();
    DecoratedLinkExpression::from((unverified, status))
}

// ── Handlers ────────────────────────────────────────────────────────────────

/// agent.get — current agent info + status + lock state
async fn get_agent(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    // Multi-user mode: extract user DID from JWT token if present
    if let Some(user_email) = ctx.user_email.clone() {
        let agent_data = AgentService::get_user_agent_data(&user_email)
            .map_err(|e| WsRpcError::internal(format!("User agent not available: {}", e)))?;

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
        return Ok(serde_json::to_value(agent)?);
    }

    // Fallback to main agent for admin/legacy mode.
    let agent = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.ensure_main_agent_loaded();
        let mut agent = agent_service
            .agent
            .clone()
            .ok_or_else(|| WsRpcError::not_found("Agent not found"))?;
        if agent.perspective.is_some() {
            agent
                .perspective
                .as_mut()
                .map(|p| p.verify_link_signatures());
        }
        Ok::<Agent, WsRpcError>(agent)
    })?;

    Ok(serde_json::to_value(agent)?)
}

/// agent.apps — list registered apps
async fn get_apps(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    Ok(serde_json::to_value(apps_map::get_apps())?)
}

/// agent.byDid — get agent by DID
async fn get_agent_by_did(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let did = params.require_str("did")?;

    // Check if DID matches main agent
    let did_match = {
        let agent_instance = AgentService::global_instance();
        let agent_service = agent_instance
            .lock()
            .map_err(|_| WsRpcError::internal("Agent service unavailable"))?;
        let agent_ref = agent_service
            .as_ref()
            .ok_or_else(|| WsRpcError::internal("Agent not initialized"))?;
        match &agent_ref.did {
            Some(existing) => did == *existing,
            None => false,
        }
    };

    if !did_match {
        let controller = LanguageController::global_instance();
        let agent_lang = controller.get_agent_language().await;
        if let Ok(lang) = agent_lang {
            let lang_address = lang.address().to_string();
            match controller.get_expression(&lang_address, &did).await {
                Ok(Some(expr_json)) => {
                    let agent: Option<Agent> = serde_json::from_value(
                        expr_json.get("data").cloned().unwrap_or(Value::Null),
                    )
                    .ok();
                    let agent = agent.map(|mut a| {
                        if a.perspective.is_some() {
                            a.perspective.as_mut().map(|p| p.verify_link_signatures());
                        }
                        a
                    });
                    Ok(serde_json::to_value(agent)?)
                }
                Ok(None) => Ok(Value::Null),
                Err(e) => {
                    log::warn!("agentByDID: failed to get expression for {}: {}", did, e);
                    Err(WsRpcError::internal(format!(
                        "agentByDID: failed to get expression for {}: {}",
                        did, e
                    )))
                }
            }
        } else {
            Ok(Value::Null)
        }
    } else {
        let agent = AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.ensure_main_agent_loaded();
            agent_service.agent.clone()
        });
        Ok(serde_json::to_value(agent)?)
    }
}

/// agent.updateProfile — update DM language and/or public perspective
async fn update_profile(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UpdateProfileRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

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
        if let Some(user_email) = ctx.user_email.clone() {
            let agent_data = AgentService::get_user_agent_data(&user_email)
                .map_err(|e| WsRpcError::internal(format!("User agent not available: {}", e)))?;

            let decorated_links: Vec<DecoratedLinkExpression> = pub_persp
                .links
                .iter()
                .map(|lei| link_expression_input_to_decorated(lei))
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
            .map_err(|e| WsRpcError::internal(format!("Failed to store user profile: {}", e)))?;

            if let Err(e) =
                AgentService::publish_agent_to_language(&AgentContext::for_user_email(user_email))
                    .await
            {
                log::warn!(
                    "Failed to publish updated user profile to agent language: {}",
                    e
                );
            }

            return Ok(serde_json::to_value(agent)?);
        } else {
            // Main agent path
            let decorated_links: Vec<DecoratedLinkExpression> = pub_persp
                .links
                .iter()
                .map(|lei| link_expression_input_to_decorated(lei))
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
            .ok_or_else(|| WsRpcError::not_found("Agent not found"))
    })?;

    get_global_pubsub()
        .await
        .publish(
            &AGENT_UPDATED_TOPIC,
            &serde_json::to_string(&agent).unwrap_or_else(|e| {
                log::error!("Failed to serialize agent for pubsub: {e}");
                String::new()
            }),
        )
        .await;

    Ok(serde_json::to_value(agent)?)
}

/// Publishes the main agent to the agent language without holding up the caller.
///
/// The publish is best-effort (a failure is only logged) and reaches a remote
/// server, so awaiting it made generate/unlock take as long as that server took
/// to answer or time out. `publish_agent_to_language` serializes publishes, so a
/// profile update made right after this still wins.
fn spawn_main_agent_publish() {
    tokio::spawn(async {
        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!("Error publishing agent expression: {}", e);
        }
    });
}

/// agent.generate — generate agent identity
async fn generate_agent(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: GenerateAgentRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let mut agent = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.create_new_keys();

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

    let mut init_errors: Vec<String> = Vec::new();

    // Start the conductor before loading languages: a seed whose system languages run on
    // Holochain waits for it in their constructors. See `agent::conductor_startup`.
    let startup = spawn_conductor_startup(body.passphrase.clone());
    let config = crate::config::get_global_config();
    let language_language_only = config.language_language_only.unwrap_or(false);
    if let Err(e) = startup.load_core_languages(language_language_only).await {
        log::error!("Error loading system languages: {:?}", e);
        init_errors.push(format!("Failed to load system languages: {}", e));
    } else {
        log::info!("System languages loaded");
    }

    spawn_main_agent_publish();

    if !init_errors.is_empty() {
        agent.error = Some(init_errors.join("; "));
    }

    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&agent).unwrap_or_else(|e| {
                log::error!("Failed to serialize agent for pubsub: {e}");
                String::new()
            }),
        )
        .await;

    log::info!("AD4M init complete");
    Ok(serde_json::to_value(agent)?)
}

/// agent.lock — lock agent
async fn lock_agent(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: LockAgentRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.lock(body.passphrase.clone());
        agent_service.dump().clone()
    });

    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&agent).unwrap_or_else(|e| {
                log::error!("Failed to serialize agent for pubsub: {e}");
                String::new()
            }),
        )
        .await;

    Ok(serde_json::to_value(agent)?)
}

/// agent.unlock — unlock agent
async fn unlock_agent(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UnlockAgentRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_instance = AgentService::global_instance();
    {
        let mut agent_service = agent_instance
            .lock()
            .map_err(|_| WsRpcError::internal("Agent service unavailable"))?;
        let agent_ref = agent_service
            .as_mut()
            .ok_or_else(|| WsRpcError::internal("Agent not initialized"))?;
        agent_ref
            .unlock(body.passphrase.clone())
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
    }

    let mut init_errors: Vec<String> = Vec::new();

    let is_unlocked = agent_instance
        .lock()
        .map_err(|_| WsRpcError::internal("Agent service unavailable"))?
        .as_ref()
        .ok_or_else(|| WsRpcError::internal("Agent not initialized"))?
        .is_unlocked();

    if is_unlocked {
        // Start the conductor before loading languages: a seed whose system languages run on
        // Holochain waits for it in their constructors. See `agent::conductor_startup`.
        let startup = spawn_conductor_startup(body.passphrase.clone());
        let config = crate::config::get_global_config();
        let language_language_only = config.language_language_only.unwrap_or(false);
        if let Err(e) = startup.load_core_languages(language_language_only).await {
            log::error!("Error loading system languages: {:?}", e);
            init_errors.push(format!("Failed to load system languages: {}", e));
        } else {
            log::info!("System languages loaded");
        }

        log::info!("AD4M init complete");

        spawn_main_agent_publish();
    }

    let mut agent = {
        let agent_service = agent_instance
            .lock()
            .map_err(|_| WsRpcError::internal("Agent service unavailable"))?;
        let agent_ref = agent_service
            .as_ref()
            .ok_or_else(|| WsRpcError::internal("Agent not initialized"))?;
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
            &serde_json::to_string(&agent).unwrap_or_else(|e| {
                log::error!("Failed to serialize agent for pubsub: {e}");
                String::new()
            }),
        )
        .await;

    Ok(serde_json::to_value(agent)?)
}

/// agent.sign — sign a message
async fn sign_message(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_SIGN_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: SignMessageRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let sig = InternalAgentSignature::from_message(body.message)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let out: AgentSignature = sig.into();
    Ok(serde_json::to_value(out)?)
}

/// agent.removeApp — remove app
async fn remove_app(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let request_id = params.require_str("id")?;
    apps_map::remove_app(&request_id).map_err(|e| WsRpcError::internal(e))?;
    Ok(serde_json::to_value(apps_map::get_apps())?)
}

// ── Auth ──

/// agent.requestCapability — request capability
async fn request_capability(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_AUTH_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: RequestCapabilityRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let auth_info: AuthInfo = body
        .auth_info
        .try_into()
        .map_err(|e: String| WsRpcError::bad_request(e))?;
    let request_id = crate::agent::capabilities::request_capability(auth_info.clone()).await;

    if ctx.auto_permit_cap_requests {
        log::debug!(
            "🔐 auto-permitting capability request (request_id={}, app_name={:?})",
            request_id,
            auth_info.app_name
        );
        let random_number_challenge =
            crate::agent::capabilities::permit_capability(AuthInfoExtended {
                request_id: request_id.clone(),
                auth: auth_info,
            })
            .map_err(|e| WsRpcError::internal(e))?;

        // Dev-mode auto-permit needs `rand` to call agent.generateJwt.
        // Print it to stdout ONLY when AD4M_LOG_SECRETS=1 (dev opt-in),
        // matching the gate used by email_service/mcp-auth. Wire response
        // stays Value::String(request_id) so existing clients and the
        // integration tests are unaffected. Non-opt-in operators still see
        // a diagnostic log line telling them how to obtain the value.
        if std::env::var("AD4M_LOG_SECRETS")
            .map(|v| v == "1")
            .unwrap_or(false)
        {
            println!(
                "AD4M_LOG_SECRETS=1: auto-permitted request_id={} rand={}",
                request_id, random_number_challenge
            );
        } else {
            log::debug!(
                "🔐 capability request auto-permitted (request_id={}, rand=<redacted; set AD4M_LOG_SECRETS=1 to print>)",
                request_id
            );
        }
        // Fall through to Value::String(request_id) below.
    }

    Ok(Value::String(request_id))
}

/// agent.permitCapability — permit capability
async fn permit_capability_handler(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_PERMIT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: PermitCapabilityRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let auth: AuthInfoExtended = serde_json::from_str(&body.auth)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid auth info: {}", e)))?;
    let random_number_challenge =
        crate::agent::capabilities::permit_capability(auth).map_err(|e| WsRpcError::internal(e))?;
    Ok(Value::String(random_number_challenge))
}

/// agent.generateJwt — generate JWT
async fn generate_jwt(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_AUTH_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: GenerateJwtRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let cap_token = generate_capability_token(body.request_id, body.rand)
        .await
        .map_err(|e| WsRpcError::internal(e))?;
    Ok(Value::String(cap_token))
}

/// agent.revokeToken — revoke token
async fn revoke_token(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let token = params.require_str("token")?;
    apps_map::revoke_app(&token).map_err(|e| WsRpcError::internal(e))?;
    Ok(serde_json::to_value(apps_map::get_apps())?)
}

// ── Status ──

/// agent.status — agent status
async fn get_agent_status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    // Multi-user mode
    if let Some(user_email) = ctx.user_email.clone() {
        let agent_data = AgentService::get_user_agent_data(&user_email)
            .map_err(|e| WsRpcError::internal(format!("User agent not available: {}", e)))?;

        let agent_context = AgentContext::for_user_email(user_email);
        let did_document = did_document_for_context(&agent_context).map_err(|e| {
            WsRpcError::internal(format!("Failed to get DID document for user: {}", e))
        })?;

        return Ok(serde_json::to_value(AgentStatus {
            did: Some(agent_data.did),
            did_document: Some(serde_json::Value::String(
                serde_json::to_string(&did_document).map_err(|e| {
                    WsRpcError::internal(format!("Failed to serialize DID document: {}", e))
                })?,
            )),
            error: None,
            is_initialized: true,
            is_unlocked: true,
        })?);
    }

    // Fallback to main agent status
    let status = AgentService::with_global_instance(|agent_service| agent_service.dump());
    Ok(serde_json::to_value(status)?)
}

/// agent.isLocked — check if agent is locked
async fn is_locked(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let locked = AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.ensure_main_agent_loaded();
        agent_service
            .agent
            .clone()
            .ok_or_else(|| WsRpcError::not_found("Agent not found"))?;
        Ok::<bool, WsRpcError>(!agent_service.is_unlocked())
    })?;
    Ok(Value::Bool(locked))
}

// ── Trust ──

/// agent.getTrustedAgents — list trusted agents
async fn get_trusted_agents(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let agents = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(serde_json::to_value(agents)?)
}

/// agent.addTrustedAgents — add trusted agents
async fn add_trusted_agents(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: TrustedAgentsWrapper = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.add_trusted_agent(body.agents);
    });

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(serde_json::to_value(result)?)
}

/// agent.deleteTrustedAgents — remove trusted agents
async fn delete_trusted_agents(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: TrustedAgentsWrapper = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.remove_trusted_agent(body.agents);
    });

    let result = crate::runtime_service::RuntimeService::with_global_instance(|runtime| {
        runtime.get_trusted_agents()
    });
    Ok(serde_json::to_value(result)?)
}

// ── Entanglement ──

/// agent.getEntanglementProofs — list
async fn get_entanglement(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let proofs = get_entanglement_proofs();
    Ok(serde_json::to_value(
        proofs
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect::<Vec<_>>(),
    )?)
}

/// agent.addEntanglementProofs — add
async fn add_entanglement(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: EntanglementProofsWrapper = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    // Check for preflight mode
    let preflight = params
        .get("preflight")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    if preflight {
        let signed = sign_device_key(
            body.proofs
                .first()
                .map(|b| b.device_key.clone())
                .unwrap_or_default(),
            body.proofs
                .first()
                .map(|b| b.device_key_type.clone())
                .unwrap_or_default(),
        );
        return Ok(serde_json::to_value(vec![
            serde_json::to_value(signed).unwrap_or_default()
        ])?);
    }

    let agent_did = AgentService::with_global_instance(|a| a.did.clone().unwrap_or_default());
    let agent_key_id =
        AgentService::with_global_instance(|a| a.signing_key_id.clone().unwrap_or_default());
    let domain_proofs: Vec<EntanglementProof> = body
        .proofs
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

    Ok(serde_json::to_value(
        domain_proofs
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect::<Vec<_>>(),
    )?)
}

/// agent.deleteEntanglementProofs — delete
async fn delete_entanglement(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: EntanglementProofsWrapper = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_did = AgentService::with_global_instance(|a| a.did.clone().unwrap_or_default());
    let agent_key_id =
        AgentService::with_global_instance(|a| a.signing_key_id.clone().unwrap_or_default());
    let domain_proofs: Vec<EntanglementProof> = body
        .proofs
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
    delete_entanglement_proof(domain_proofs);

    let remaining = get_entanglement_proofs();
    Ok(serde_json::to_value(
        remaining
            .into_iter()
            .map(|p| serde_json::to_value(p).unwrap_or_default())
            .collect::<Vec<_>>(),
    )?)
}

/// agent.import — import agent from keystore
async fn import_agent(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    Err(WsRpcError::not_implemented(
        "Agent import not yet implemented",
    ))
}

/// agent.entanglementProofPreflight — pre-flight check
async fn entanglement_proof_preflight(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: EntanglementProofPreflightRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let signed = sign_device_key(body.device_key, body.device_key_type);
    Ok(serde_json::to_value(signed).unwrap_or_default())
}

// ── Registration ────────────────────────────────────────────────────────────

/// Register all agent WS handlers.
///
/// Message types match the client SDK's `apiClient.call()` type strings.
pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("agent.get", get_agent);
    map.register("agent.getApps", get_apps);
    map.register("agent.byDid", get_agent_by_did);
    map.register("agent.updateProfile", update_profile);
    map.register("agent.generate", generate_agent);
    map.register("agent.import", import_agent);
    map.register("agent.lock", lock_agent);
    map.register("agent.unlock", unlock_agent);
    map.register("agent.sign", sign_message);
    map.register("agent.removeApp", remove_app);
    map.register("agent.requestCapability", request_capability);
    map.register("agent.permitCapability", permit_capability_handler);
    map.register("agent.generateJwt", generate_jwt);
    map.register("agent.revokeToken", revoke_token);
    map.register("agent.status", get_agent_status);
    map.register("agent.isLocked", is_locked);
    map.register("agent.getTrustedAgents", get_trusted_agents);
    map.register("agent.addTrustedAgents", add_trusted_agents);
    map.register("agent.deleteTrustedAgents", delete_trusted_agents);
    map.register("agent.getEntanglementProofs", get_entanglement);
    map.register("agent.addEntanglementProofs", add_entanglement);
    map.register("agent.deleteEntanglementProofs", delete_entanglement);
    map.register(
        "agent.entanglementProofPreflight",
        entanglement_proof_preflight,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::signatures::TestSigner;
    use crate::types::domain::{ExpressionProofInput, LinkInput};

    /// Build the wire input a client sends, with whatever verdict it cares to claim.
    fn input_claiming(
        author: &str,
        timestamp: &str,
        signature: &str,
        claimed_valid: Option<bool>,
    ) -> LinkExpressionInput {
        LinkExpressionInput {
            author: author.to_string(),
            timestamp: timestamp.to_string(),
            data: LinkInput {
                source: "did:key:alice".into(),
                target: "literal://string:hello".into(),
                predicate: Some("ad4m://has_name".into()),
            },
            proof: ExpressionProofInput {
                key: Some("#z6Mk-key".into()),
                signature: Some(signature.to_string()),
                valid: claimed_valid,
                invalid: claimed_valid.map(|v| !v),
            },
            status: None,
        }
    }

    /// The defect: a caller could assert `valid: true` over a signature that
    /// does not verify, and the executor stored that verdict verbatim.
    #[test]
    fn a_forged_valid_verdict_is_overruled_by_local_verification() {
        let signer = TestSigner::generate();
        let input = input_claiming(
            &signer.did,
            "2026-09-17T06:00:00.000Z",
            // Well-formed hex so `verify` gets past `hex::decode` and actually
            // checks the signature — a non-hex string would fail earlier and
            // pass this test for the wrong reason.
            &"ab".repeat(64),
            Some(true),
        );

        let decorated = link_expression_input_to_decorated(&input);

        assert_eq!(
            decorated.proof.valid,
            Some(false),
            "a signature that does not verify must be recorded invalid no matter what the caller claimed"
        );
        assert_eq!(decorated.proof.invalid, Some(true));
    }

    /// The other direction, and the reason this pair is a contract rather than
    /// a mirror: a fix that simply hardcoded `valid: false` would satisfy the
    /// test above. A genuinely signed link must come out valid even when the
    /// caller claims the opposite, which pins that the verdict is *computed*
    /// and that the caller's field is ignored in both directions.
    #[test]
    fn a_genuine_signature_is_honoured_even_when_the_caller_claims_invalid() {
        let signer = TestSigner::generate();
        let link = Link {
            source: "did:key:alice".into(),
            target: "literal://string:hello".into(),
            predicate: Some("ad4m://has_name".into()),
        }
        .normalize();
        let signed = signer.sign(link.clone());

        let mut input = input_claiming(
            &signed.author,
            &signed.timestamp,
            &signed.proof.signature,
            Some(false),
        );
        input.data = LinkInput {
            source: link.source.clone(),
            target: link.target.clone(),
            predicate: link.predicate.clone(),
        };

        let decorated = link_expression_input_to_decorated(&input);

        assert_eq!(
            decorated.proof.valid,
            Some(true),
            "a signature that verifies must be recorded valid even though the caller said invalid"
        );
        assert_eq!(decorated.proof.invalid, Some(false));
    }

    /// A client that sends no status gets the store's default rather than a
    /// hole, matching every other link in the system.
    #[test]
    fn a_missing_status_becomes_shared() {
        let signer = TestSigner::generate();
        let input = input_claiming(
            &signer.did,
            "2026-09-17T06:00:00.000Z",
            &"ab".repeat(64),
            None,
        );
        assert_eq!(
            link_expression_input_to_decorated(&input).status,
            Some(LinkStatus::Shared)
        );
    }
}
