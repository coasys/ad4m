#![allow(non_snake_case)]

use crate::{
    agent::{capabilities::*, create_signed_expression, AgentContext, AgentService},
    ai_service::AIService,
    neighbourhoods::{self, install_neighbourhood_with_context},
    perspectives::{
        self, add_perspective, export_perspective, get_perspective, import_perspective,
        perspective_instance::{PerspectiveInstance, SdnaType},
        remove_perspective, update_perspective, SerializedPerspective,
    },
    types::{AITask, DecoratedLinkExpression, Link, LinkExpression, ModelType},
};
use crate::{
    db::Ad4mDb,
    perspectives::perspective_instance::{Command, Parameter, SubjectClassOption},
    runtime_service::RuntimeService,
    types::Notification,
};
use coasys_juniper::{graphql_object, graphql_value, FieldError, FieldResult, Value};

use super::graphql_types::*;
use crate::{
    entanglement_service::{
        add_entanglement_proofs, delete_entanglement_proof, get_entanglement_proofs,
        sign_device_key,
    },
    holochain_service::get_holochain_service,
    languages::LanguageController,
    pubsub::{
        get_global_pubsub, mark_credits_dirty, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC,
    },
};
use base64::prelude::*;

// Use the shared can_access_perspective function from query_resolvers
use super::query_resolvers::can_access_perspective;

/// Deduct compute credits for a user after an operation completes.
/// Only looks up the rate and bills when billing is actually active.
/// No-ops in single-user mode, free hosting, or free-access users.
fn deduct_compute_credits(
    auth_token: &str,
    rate_key: &str,
    quantity: f64,
    operation: &str,
    summary: Option<&str>,
) -> FieldResult<()> {
    if !is_billing_active(auth_token)? {
        return Ok(());
    }
    let rate = match Ad4mDb::with_global_instance(|db| db.get_host_rate(rate_key)) {
        Ok(Some(rate)) => rate,
        Ok(None) => {
            return Err(FieldError::new(
                format!("No host rate configured for '{}'", rate_key),
                graphql_value!(null),
            ))
        }
        Err(e) => {
            return Err(FieldError::new(
                format!("Failed to read host rate: {}", e),
                graphql_value!(null),
            ))
        }
    };
    let amount = quantity * rate;
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        crate::billing::bill_compute(email, amount, operation, summary)
            .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
    }
    Ok(())
}

/// Read-only credit check. Returns Ok(()) if the user can afford compute.
/// Used as a fast pre-check before expensive operations; the actual deduction
/// happens after the operation via deduct_compute_credits with the exact cost.
fn check_compute_credits(auth_token: &str) -> FieldResult<()> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
        if global_free {
            return Ok(());
        }
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
        if !free {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
                .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
            if credits <= 0.0 {
                return Err(FieldError::new(
                    "Insufficient compute credits",
                    graphql_value!(null),
                ));
            }
        }
    }
    Ok(())
}

/// Returns true if billing is active for this user (not free hosting, not free access).
/// Returns false if there's no user email (single-user / local mode) or if hosting/user is free.
fn is_billing_active(auth_token: &str) -> FieldResult<bool> {
    if let Some(ref email) = user_email_from_token(auth_token.to_string()) {
        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
        if global_free {
            return Ok(false);
        }
        let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
        Ok(!free)
    } else {
        Ok(false)
    }
}

// Helper function to get perspective with access control
async fn get_perspective_with_access_control(
    uuid: &str,
    context: &RequestContext,
) -> FieldResult<PerspectiveInstance> {
    let perspective = get_perspective_with_uuid_field_error(uuid)?;
    let user_email = user_email_from_token(context.auth_token.clone());

    // Check access to the perspective
    let handle = perspective.persisted.lock().await.clone();
    if !can_access_perspective(&user_email, &handle) {
        return Err(FieldError::new(
            "Access denied: You don't have permission to access this perspective",
            graphql_value!(null),
        ));
    }

    Ok(perspective)
}

pub struct Mutation;

fn get_perspective_with_uuid_field_error(uuid: &str) -> FieldResult<PerspectiveInstance> {
    get_perspective(uuid).ok_or_else(|| {
        FieldError::new(
            "Perspective not found",
            graphql_value!({ "uuid": uuid.to_owned() }),
        )
    })
}

fn link_status_from_input(status: Option<String>) -> Result<LinkStatus, FieldError> {
    match status.as_deref() {
        Some("shared") => Ok(LinkStatus::Shared),
        Some("local") => Ok(LinkStatus::Local),
        None => Ok(LinkStatus::Shared),
        _ => Err(FieldError::new(
            "Invalid status, must be either 'shared' or 'local'",
            graphql_value!({ "invalid_status": status }),
        )),
    }
}

#[graphql_object(context = RequestContext)]
impl Mutation {
    async fn add_trusted_agents(
        &self,
        context: &RequestContext,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.add_trusted_agent(agents);

            Ok(runtime_service.get_trusted_agents())
        })
    }

    async fn agent_add_entanglement_proofs(
        &self,
        _context: &RequestContext,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        //TODO: capability missing for this function
        let converted_proofs: Vec<EntanglementProof> = proofs
            .into_iter()
            .map(|input| EntanglementProof {
                did: input.did,
                did_signing_key_id: input.did_signing_key_id,
                device_key_type: input.device_key_type,
                device_key: input.device_key,
                device_key_signed_by_did: input.device_key_signed_by_did,
                did_signed_by_device_key: Some(input.did_signed_by_device_key),
            })
            .collect();

        add_entanglement_proofs(converted_proofs);

        let proofs = get_entanglement_proofs();

        Ok(proofs)
    }

    async fn agent_delete_entanglement_proofs(
        &self,
        _context: &RequestContext,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        //TODO: capability missing for this function
        let converted_proofs: Vec<EntanglementProof> = proofs
            .into_iter()
            .map(|input| EntanglementProof {
                did: input.did,
                did_signing_key_id: input.did_signing_key_id,
                device_key_type: input.device_key_type,
                device_key: input.device_key,
                device_key_signed_by_did: input.device_key_signed_by_did,
                did_signed_by_device_key: Some(input.did_signed_by_device_key),
            })
            .collect();

        delete_entanglement_proof(converted_proofs);

        let proofs = get_entanglement_proofs();

        Ok(proofs)
    }

    async fn agent_entanglement_proof_pre_flight(
        &self,
        _context: &RequestContext,
        device_key: String,
        device_key_type: String,
    ) -> FieldResult<EntanglementProof> {
        //TODO: capability missing for this function
        let proof = sign_device_key(device_key, device_key_type);

        Ok(proof)
    }

    async fn agent_generate(
        &self,
        context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_CREATE_CAPABILITY)?;
        let mut agent = AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.create_new_keys();

            // Set the direct message language from bootstrap seed
            let dm_language =
                RuntimeService::with_global_instance(|rt| rt.get_direct_message_language());
            if let Some(ref mut agent) = agent_service.agent {
                agent.direct_message_language = Some(dm_language);
            }

            agent_service.save(passphrase.clone());

            // Store passphrase so future wallet modifications (e.g., adding user keys) can be saved
            agent_service.passphrase = Some(passphrase.clone());

            agent_service.dump().clone()
        });

        // Start Holochain conductor (previously done via JS core.initHolochain)
        let config = crate::config::get_global_config();
        let hc_config = crate::holochain_service::LocalConductorConfig::from_ad4m_config(
            &config,
            passphrase.clone(),
        );

        let mut init_errors: Vec<String> = Vec::new();

        if let Err(e) = crate::holochain_service::HolochainService::init(hc_config).await {
            log::error!("Error initializing Holochain: {:?}", e);
            init_errors.push(format!("Holochain init failed: {}", e));
        } else {
            log::info!("Holochain init complete");
        }

        // Load system languages (previously done via JS core.initLanguages)
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

        // Publish agent expression to the agent language
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

        Ok(agent)
    }

    async fn agent_lock(
        &self,
        _context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let agent = AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.lock(passphrase.clone());
            agent_service.dump().clone()
        });

        get_global_pubsub()
            .await
            .publish(
                &AGENT_STATUS_CHANGED_TOPIC,
                &serde_json::to_string(&agent).unwrap(),
            )
            .await;

        Ok(agent)
    }

    async fn agent_remove_app(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        apps_map::remove_app(&request_id)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_request_capability(
        &self,
        context: &RequestContext,
        auth_info: AuthInfoInput,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)?;
        let auth_info: AuthInfo = auth_info.try_into().map_err(|e: String| {
            coasys_juniper::FieldError::new(e, coasys_juniper::Value::null())
        })?;
        let request_id = request_capability(auth_info.clone()).await;
        if context.auto_permit_cap_requests {
            println!("======================================");
            println!("Got capability request: \n{:?}", auth_info);
            let random_number_challenge = permit_capability(AuthInfoExtended {
                request_id: request_id.clone(),
                auth: auth_info,
            })?;
            println!("--------------------------------------");
            println!("Random number challenge: {}", random_number_challenge);
            println!("======================================");
        }

        Ok(request_id)
    }

    //NOTE: all the functions from here on out have not been tested by calling the cli <-> rust graphql server
    async fn agent_permit_capability(
        &self,
        context: &RequestContext,
        auth: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_PERMIT_CAPABILITY)?;
        let auth: AuthInfoExtended = serde_json::from_str(&auth)?;
        let random_number_challenge = permit_capability(auth)?;
        Ok(random_number_challenge)
    }

    async fn agent_generate_jwt(
        &self,
        context: &RequestContext,
        rand: String,
        request_id: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)?;
        let cap_token = generate_capability_token(request_id, rand).await?;
        Ok(cap_token)
    }

    async fn agent_revoke_token(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        apps_map::revoke_app(&request_id)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_sign_message(
        &self,
        context: &RequestContext,
        message: String,
    ) -> FieldResult<AgentSignature> {
        check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)?;
        Ok(crate::agent::AgentSignature::from_message(message)?.into())
    }

    async fn agent_unlock(
        &self,
        context: &RequestContext,
        passphrase: String,
        _holochain: bool,
    ) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)?;

        let agent_instance = AgentService::global_instance();
        {
            let mut agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &mut AgentService = agent_service.as_mut().expect("agent instance");

            agent_ref.unlock(passphrase.clone())?
        }

        let mut init_errors: Vec<String> = Vec::new();

        if agent_instance
            .lock()
            .expect("agent lock")
            .as_ref()
            .expect("agent instance")
            .is_unlocked()
        {
            // Start Holochain conductor if not already running (previously done via JS core.callResolver)
            if crate::holochain_service::maybe_get_holochain_service()
                .await
                .is_none()
            {
                log::info!("Holochain service not initialized. Initializing...");
                let config = crate::config::get_global_config();
                let hc_config = crate::holochain_service::LocalConductorConfig::from_ad4m_config(
                    &config,
                    passphrase.clone(),
                );

                if let Err(e) = crate::holochain_service::HolochainService::init(hc_config).await {
                    log::error!("Error initializing Holochain: {:?}", e);
                    init_errors.push(format!("Holochain init failed: {}", e));
                } else {
                    log::info!("Holochain init complete");
                }
            } else {
                log::info!("Holochain service already initialized");
            }

            // Load system languages (previously done via JS core.initLanguages)
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

            // Publish agent expression to the agent language
            if let Err(e) =
                AgentService::publish_agent_to_language(&AgentContext::main_agent()).await
            {
                log::warn!("Error publishing agent expression: {}", e);
            }
        }

        let mut agent = {
            let agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
            agent_ref.dump().clone()
        };

        if !agent_instance
            .lock()
            .expect("agent lock")
            .as_ref()
            .expect("agent instance")
            .is_unlocked()
        {
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

        Ok(agent)
    }

    async fn agent_update_direct_message_language(
        &self,
        context: &RequestContext,
        direct_message_language: String,
    ) -> FieldResult<Agent> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        let agent = AgentService::with_mutable_global_instance(|agent_service| {
            if let Some(ref mut agent) = agent_service.agent {
                agent.direct_message_language = Some(direct_message_language.clone());
                let updated_agent = agent.clone();
                if let Some(ref passphrase) = agent_service.passphrase {
                    agent_service.save(passphrase.clone());
                }
                Ok(updated_agent)
            } else {
                Err(FieldError::new("Agent not initialized", Value::null()))
            }
        })?;

        // Publish updated agent to agent language
        if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::main_agent()).await {
            log::warn!(
                "Failed to publish agent expression after DM language update: {}",
                e
            );
        }

        // Notify subscribers
        get_global_pubsub()
            .await
            .publish(
                &AGENT_UPDATED_TOPIC,
                &serde_json::to_string(&agent).unwrap(),
            )
            .await;

        Ok(agent)
    }

    async fn agent_update_public_perspective(
        &self,
        context: &RequestContext,
        perspective: PerspectiveInput,
    ) -> FieldResult<Agent> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // For multi-user mode: extract user email from JWT token if present
        if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
            // Get user agent data
            let agent_data = AgentService::get_user_agent_data(&user_email).map_err(|e| {
                FieldError::new(format!("User agent not available: {}", e), Value::null())
            })?;

            // Convert LinkExpressionInput to DecoratedLinkExpression
            let decorated_links: Vec<DecoratedLinkExpression> = perspective
                .links
                .iter()
                .map(|link_input| DecoratedLinkExpression::try_from(link_input.clone()))
                .collect::<Result<Vec<_>, _>>()?;

            // Create agent with updated perspective
            let agent = Agent {
                did: agent_data.did,
                direct_message_language: None,
                perspective: Some(Perspective {
                    links: decorated_links,
                }),
            };

            // Store the updated profile for the user
            AgentService::with_global_instance(|agent_service| {
                agent_service.store_user_agent_profile(&user_email, &agent)
            })
            .map_err(|e| {
                FieldError::new(
                    format!("Failed to store user profile: {}", e),
                    Value::null(),
                )
            })?;

            // Publish the updated agent to the agent language
            if let Err(e) = AgentService::publish_agent_to_language(&AgentContext::for_user_email(
                user_email.clone(),
            ))
            .await
            {
                log::warn!(
                    "Failed to publish updated user {} profile to agent language: {}",
                    agent.did,
                    e
                );
                // Don't fail the profile update, just log the warning
            }

            Ok(agent)
        } else {
            // Main agent path: update perspective and publish to agent language
            let decorated_links: Vec<DecoratedLinkExpression> = perspective
                .links
                .iter()
                .map(|link_input| DecoratedLinkExpression::try_from(link_input.clone()))
                .collect::<Result<Vec<_>, _>>()?;

            let agent = AgentService::with_mutable_global_instance(|agent_service| {
                if let Some(ref mut agent) = agent_service.agent {
                    agent.perspective = Some(Perspective {
                        links: decorated_links,
                    });
                    let updated = agent.clone();
                    if let Some(ref passphrase) = agent_service.passphrase {
                        agent_service.save(passphrase.clone());
                    }
                    Ok(updated)
                } else {
                    Err(FieldError::new("Agent not initialized", Value::null()))
                }
            })?;

            // Publish updated agent to agent language
            AgentService::publish_agent_to_language(&AgentContext::main_agent())
                .await
                .map_err(|e| {
                    log::warn!(
                        "Failed to publish agent expression after profile update: {}",
                        e
                    );
                    FieldError::new(
                        format!("Profile updated but failed to publish: {}", e),
                        Value::null(),
                    )
                })?;

            // Notify subscribers
            get_global_pubsub()
                .await
                .publish(
                    &AGENT_UPDATED_TOPIC,
                    &serde_json::to_string(&agent).unwrap(),
                )
                .await;

            Ok(agent)
        }
    }

    async fn delete_trusted_agents(
        &self,
        context: &RequestContext,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.remove_trusted_agent(agents);

            Ok(runtime_service.get_trusted_agents())
        })
    }

    // Simple user management mutations for multi-user mode
    async fn runtime_create_user(
        &self,
        context: &RequestContext,
        email: String,
        password: String,
        app_info: Option<AuthInfoInput>,
    ) -> FieldResult<UserCreationResult> {
        // Normalize email: trim whitespace and convert to lowercase
        let email = email.trim().to_lowercase();

        // Check capability (empty tokens get user management caps in multi-user mode)
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
        )?;

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return Ok(UserCreationResult {
                did: String::new(),
                success: false,
                error: Some("Multi-user mode is not enabled".to_string()),
            });
        }

        // Generate DID by creating a keypair in the wallet using email as key name
        use crate::agent::AgentService;
        AgentService::ensure_user_key_exists(&email).map_err(|e| {
            FieldError::new(
                format!("Failed to create user key: {}", e),
                graphql_value!(null),
            )
        })?;

        let did = AgentService::get_user_did_by_email(&email).map_err(|e| {
            FieldError::new(
                format!("Failed to retrieve user DID: {}", e),
                graphql_value!(null),
            )
        })?;

        // Save the wallet to persist the new user key
        AgentService::with_global_instance(|agent_service| {
            if let Some(passphrase) = &agent_service.passphrase {
                agent_service.save(passphrase.clone());
                log::info!("Saved wallet after creating key for user: {}", email);
            } else {
                log::warn!(
                    "Cannot save wallet - no passphrase stored. User DID may change on restart!"
                );
            }
        });

        // Check if user already exists
        let db = Ad4mDb::global_instance();
        let existing_user = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref.get_user(&email).ok()
        };

        if existing_user.is_some() {
            return Ok(UserCreationResult {
                did: String::new(),
                success: false,
                error: Some("User already exists".to_string()),
            });
        }

        // Add user to database with hashed password
        {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref.add_user(&email, &did, &password).map_err(|e| {
                FieldError::new(format!("Failed to add user: {}", e), graphql_value!(null))
            })?;
        }

        // Create initial agent profile for the user
        let initial_agent = Agent {
            did: did.clone(),
            direct_message_language: None,
            perspective: Some(Perspective { links: vec![] }),
        };

        // Store the profile locally
        AgentService::with_global_instance(|agent_service| {
            agent_service.store_user_agent_profile(&email, &initial_agent)
        })
        .map_err(|e| {
            FieldError::new(
                format!("Failed to store user profile: {}", e),
                Value::null(),
            )
        })?;

        // Publish the agent to the agent language
        if let Err(e) =
            AgentService::publish_agent_to_language(&AgentContext::for_user_email(email.clone()))
                .await
        {
            log::warn!("Failed to publish user {} to agent language: {}", did, e);
            // Don't fail the user creation, just log the warning
        }

        // Check if test mode is enabled (we check this early to use in rate limiting)
        let test_mode = crate::email_service::EMAIL_TEST_MODE
            .lock()
            .ok()
            .map(|mode| *mode)
            .unwrap_or(false);

        // Apply rate limiting before generating verification code (but skip in test mode)
        // This prevents abuse by repeatedly creating accounts to spam the email server
        // Using atomic check-and-update to prevent TOCTOU race conditions.
        if !test_mode {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");

            if let Err(e) = db_ref.check_and_update_rate_limit(&email) {
                log::warn!("Rate limit exceeded for signup verification: {}", e);
                return Ok(UserCreationResult {
                    did,
                    success: true,
                    error: Some(format!(
                        "User created successfully but verification email was not sent due to rate limiting: {}",
                        e
                    )),
                });
            }
        }

        // Generate verification code and send email
        let code = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref
                .create_verification_code(&email, "signup")
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to create verification code: {}", e),
                        graphql_value!(null),
                    )
                })?
        };

        // Get app name and icon from provided app_info for email context
        let app_name = app_info.as_ref().map(|info| info.app_name.clone());
        let app_icon = app_info
            .as_ref()
            .and_then(|info| info.app_icon_path.clone());

        // Get SMTP config if available OR if test mode is enabled
        let smtp_config_opt = crate::config::SMTP_CONFIG
            .lock()
            .ok()
            .and_then(|cfg| cfg.clone())
            .filter(|config| config.enabled);

        if test_mode || smtp_config_opt.is_some() {
            // In test mode, use dummy config since send_verification_email will capture codes instead
            let smtp_config = if test_mode && smtp_config_opt.is_none() {
                crate::config::SmtpConfig {
                    enabled: true,
                    host: "test.localhost".to_string(),
                    port: 587,
                    username: "test".to_string(),
                    password: "test".to_string(),
                    from_address: "test@localhost".to_string(),
                }
            } else {
                smtp_config_opt.unwrap()
            };

            let email_service = crate::email_service::EmailService::new(smtp_config);
            if let Err(e) = email_service
                .send_verification_email(
                    &email,
                    &code,
                    "signup",
                    app_name.as_deref(),
                    app_icon.as_deref(),
                )
                .await
            {
                log::warn!("Failed to send verification email to {}: {}", email, e);

                // Clean up the verification code since email delivery failed
                // (but not in test mode, where codes need to be preserved for testing)
                if !test_mode {
                    let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
                    let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
                    if let Err(cleanup_err) = db_ref.delete_verification_code(&email, "signup") {
                        log::error!(
                            "Failed to cleanup verification code for {} after email failure: {}",
                            email,
                            cleanup_err
                        );
                    }
                }

                // Don't fail user creation if email sending fails
                return Ok(UserCreationResult {
                    did,
                    success: true,
                    error: Some(format!(
                        "User created but failed to send verification email: {}",
                        e
                    )),
                });
            }

            // Note: Rate limiting for signup is now applied earlier (before verification code creation)
            // to prevent abuse of the signup endpoint
        } else {
            log::warn!(
                "SMTP not configured - skipping verification email for {}",
                email
            );

            // Clean up the verification code since SMTP is not configured
            {
                let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
                let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
                if let Err(cleanup_err) = db_ref.delete_verification_code(&email, "signup") {
                    log::error!(
                        "Failed to cleanup verification code for {} when SMTP unconfigured: {}",
                        email,
                        cleanup_err
                    );
                }
            }

            // Return error message when SMTP is not configured (similar to email failure case)
            // Note: User can still login with password - email verification is optional
            return Ok(UserCreationResult {
                did,
                success: true,
                error: Some(
                    "User created successfully. You can login with your password. Email verification was not sent because SMTP is not configured. To enable email verification, please configure email settings in the launcher.".to_string(),
                ),
            });
        }

        Ok(UserCreationResult {
            did,
            success: true,
            error: None,
        })
    }

    async fn runtime_login_user(
        &self,
        context: &RequestContext,
        email: String,
        password: String,
    ) -> FieldResult<String> {
        // Normalize email: trim whitespace and convert to lowercase
        let email = email.trim().to_lowercase();

        // Check capability (empty tokens get login capability in multi-user mode)
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY,
        )?;

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return Err(FieldError::new(
                "Multi-user mode is not enabled",
                graphql_value!(null),
            ));
        }

        // Verify user credentials
        let db = Ad4mDb::global_instance();
        let password_valid = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref
                .verify_user_password(&email, &password)
                .unwrap_or(false)
        };

        if !password_valid {
            return Err(FieldError::new("Invalid credentials", graphql_value!(null)));
        }

        if !AgentService::user_exists(&email) {
            return Err(FieldError::new(
                "User key not found on executor",
                graphql_value!(null),
            ));
        }

        // Extract app info from the current capability token if available
        let auth_info = if context.auth_token.is_empty() {
            // Default app info - use user-scoped capabilities instead of admin ALL_CAPABILITY
            AuthInfo {
                app_name: "multi-user-app".to_string(),
                app_desc: "Multi-user application".to_string(),
                app_domain: Some("multi-user".to_string()),
                app_url: Some("https://multi-user.app".to_string()),
                app_icon_path: None,
                capabilities: Some(get_user_default_capabilities()),
                user_email: Some(email.clone()),
            }
        } else {
            // App context - preserve the original app info and add user supposed to be the same as the caller
            match decode_jwt(context.auth_token.clone()) {
                Ok(current_claims) => {
                    let mut auth_info = current_claims.capabilities;
                    auth_info.user_email = Some(email.clone());
                    auth_info
                }
                Err(_) => {
                    // Invalid token - reject the request
                    return Err(FieldError::new("Invalid auth token", graphql_value!(null)));
                }
            }
        };

        let cap_token = token::generate_jwt(
            auth_info.app_name.clone(),
            DEFAULT_TOKEN_VALID_PERIOD,
            auth_info,
        )
        .map_err(|e| {
            FieldError::new(
                format!("Failed to generate token: {}", e),
                graphql_value!(null),
            )
        })?;

        Ok(cap_token)
    }

    async fn runtime_request_login_verification(
        &self,
        context: &RequestContext,
        email: String,
        app_info: Option<AuthInfoInput>,
    ) -> FieldResult<VerificationRequestResult> {
        use crate::graphql::graphql_types::VerificationRequestResult;

        // Normalize email: trim whitespace and convert to lowercase
        let email = email.trim().to_lowercase();

        // Check capability
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY,
        )?;

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return Ok(VerificationRequestResult {
                success: false,
                message: "Multi-user mode is not enabled".to_string(),
                requires_password: false,
                is_existing_user: false,
            });
        }

        // Get DB handle for subsequent operations
        let db = Ad4mDb::global_instance();

        // Check if user exists first (we need to know this to decide how to handle rate limiting)
        let user_exists = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref.get_user(&email).is_ok()
        };

        if !user_exists {
            // New user - check rate limit but don't update it since no email is sent yet.
            // The rate limit will be updated in runtime_create_user when the email is actually sent.
            // This prevents user enumeration while avoiding the rate limit conflict in the signup flow.
            {
                let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
                let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
                if let Err(e) = db_ref.check_rate_limit(&email) {
                    return Ok(VerificationRequestResult {
                        success: false,
                        message: e.to_string(),
                        requires_password: false,
                        is_existing_user: false,
                    });
                }
            }
            // New user - they need to sign up with password
            return Ok(VerificationRequestResult {
                success: true,
                message: "New user - please provide a password to sign up".to_string(),
                requires_password: true,
                is_existing_user: false,
            });
        }

        // Existing user - check and update rate limit since we will send an email
        // Using atomic check-and-update to prevent TOCTOU race conditions.
        {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            if let Err(e) = db_ref.check_and_update_rate_limit(&email) {
                return Ok(VerificationRequestResult {
                    success: false,
                    message: e.to_string(),
                    requires_password: false,
                    is_existing_user: true,
                });
            }
        }

        // Generate verification code
        let code = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref
                .create_verification_code(&email, "login")
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to create verification code: {}", e),
                        graphql_value!(null),
                    )
                })?
        };

        // Get app name and icon from provided app_info for email context
        let app_name = app_info.as_ref().map(|info| info.app_name.clone());
        let app_icon = app_info
            .as_ref()
            .and_then(|info| info.app_icon_path.clone());

        // Check if test mode is enabled
        let test_mode = crate::email_service::EMAIL_TEST_MODE
            .lock()
            .ok()
            .map(|mode| *mode)
            .unwrap_or(false);

        // Get SMTP config from global instance OR use dummy config in test mode
        let smtp_config_opt = crate::config::SMTP_CONFIG
            .lock()
            .ok()
            .and_then(|cfg| cfg.clone())
            .filter(|config| config.enabled);

        let smtp_config = if test_mode && smtp_config_opt.is_none() {
            // In test mode without SMTP config, use dummy config
            crate::config::SmtpConfig {
                enabled: true,
                host: "test.localhost".to_string(),
                port: 587,
                username: "test".to_string(),
                password: "test".to_string(),
                from_address: "test@localhost".to_string(),
            }
        } else if let Some(config) = smtp_config_opt {
            config
        } else {
            // SMTP not configured - return requires_password so UI can show password field for login
            log::warn!(
                "SMTP not configured - requiring password login for {}",
                email
            );

            // Clean up the verification code since SMTP is not configured
            {
                let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
                let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
                if let Err(cleanup_err) = db_ref.delete_verification_code(&email, "login") {
                    log::error!(
                        "Failed to cleanup verification code for {} when SMTP unconfigured: {}",
                        email,
                        cleanup_err
                    );
                }
            }

            return Ok(VerificationRequestResult {
                success: true,
                message: "Email verification is not available. Please login with your password."
                    .to_string(),
                requires_password: true,
                is_existing_user: true,
            });
        };

        // Send verification email
        let email_service = crate::email_service::EmailService::new(smtp_config);
        if let Err(e) = email_service
            .send_verification_email(
                &email,
                &code,
                "login",
                app_name.as_deref(),
                app_icon.as_deref(),
            )
            .await
        {
            log::warn!("Failed to send verification email to {}: {}", email, e);

            // Clean up the verification code since email delivery failed
            // (but not in test mode, where codes need to be preserved for testing)
            if !test_mode {
                let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
                let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
                if let Err(cleanup_err) = db_ref.delete_verification_code(&email, "login") {
                    log::error!(
                        "Failed to cleanup verification code for {} after email failure: {}",
                        email,
                        cleanup_err
                    );
                }
            }

            return Err(FieldError::new(
                format!("Failed to send verification email: {}", e),
                graphql_value!(null),
            ));
        }

        Ok(VerificationRequestResult {
            success: true,
            message: "Verification email sent".to_string(),
            requires_password: false,
            is_existing_user: true,
        })
    }

    async fn runtime_verify_email_code(
        &self,
        context: &RequestContext,
        email: String,
        code: String,
        verification_type: String,
    ) -> FieldResult<String> {
        use crate::agent::capabilities::{
            get_user_default_capabilities, token, AuthInfo, DEFAULT_TOKEN_VALID_PERIOD,
        };

        // Normalize email: trim whitespace and convert to lowercase
        let email = email.trim().to_lowercase();

        // Check capability
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
        )?;

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return Err(FieldError::new(
                "Multi-user mode is not enabled",
                graphql_value!(null),
            ));
        }

        // Verify the code
        let db = Ad4mDb::global_instance();
        let code_valid = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            match db_ref.verify_code(&email, &code, &verification_type) {
                Ok(valid) => valid,
                Err(e) => {
                    // Code was invalidated due to too many failed attempts
                    return Err(FieldError::new(e.to_string(), graphql_value!(null)));
                }
            }
        };

        if !code_valid {
            return Err(FieldError::new(
                "Invalid or expired verification code",
                graphql_value!(null),
            ));
        }

        // Verify user exists
        if !AgentService::user_exists(&email) {
            return Err(FieldError::new(
                "User key not found on executor",
                graphql_value!(null),
            ));
        }

        // Generate JWT token for the user
        let auth_info = if context.auth_token.is_empty() {
            // Default app info - use user-scoped capabilities instead of admin ALL_CAPABILITY
            AuthInfo {
                app_name: "multi-user-app".to_string(),
                app_desc: "Multi-user application".to_string(),
                app_domain: Some("multi-user".to_string()),
                app_url: Some("https://multi-user.app".to_string()),
                app_icon_path: None,
                capabilities: Some(get_user_default_capabilities()),
                user_email: Some(email.clone()),
            }
        } else {
            // Preserve app context
            match token::decode_jwt(context.auth_token.clone()) {
                Ok(current_claims) => {
                    let mut auth_info = current_claims.capabilities;
                    auth_info.user_email = Some(email.clone());
                    auth_info
                }
                Err(_) => {
                    return Err(FieldError::new("Invalid auth token", graphql_value!(null)));
                }
            }
        };

        let cap_token = token::generate_jwt(
            auth_info.app_name.clone(),
            DEFAULT_TOKEN_VALID_PERIOD,
            auth_info,
        )
        .map_err(|e| {
            FieldError::new(
                format!("Failed to generate token: {}", e),
                graphql_value!(null),
            )
        })?;

        Ok(cap_token)
    }

    async fn runtime_test_email(&self, context: &RequestContext, to: String) -> FieldResult<bool> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        // Get SMTP config from global instance
        let smtp_config = crate::config::SMTP_CONFIG
            .lock()
            .ok()
            .and_then(|cfg| cfg.clone())
            .filter(|config| config.enabled)
            .ok_or_else(|| {
                FieldError::new(
                    "SMTP is not configured or is disabled. Please enable email settings in the launcher.",
                    graphql_value!(null),
                )
            })?;

        // Send test email
        let email_service = crate::email_service::EmailService::new(smtp_config);
        email_service.send_test_email(&to).await.map_err(|e| {
            FieldError::new(
                format!("Failed to send test email: {}", e),
                graphql_value!(null),
            )
        })?;

        Ok(true)
    }

    /// Enable email test mode (for testing only - captures codes instead of sending)
    async fn runtime_email_test_mode_enable(&self, context: &RequestContext) -> FieldResult<bool> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        crate::email_service::enable_test_mode();
        Ok(true)
    }

    /// Disable email test mode
    async fn runtime_email_test_mode_disable(&self, context: &RequestContext) -> FieldResult<bool> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        crate::email_service::disable_test_mode();
        Ok(true)
    }

    /// Get captured verification code from test mode
    async fn runtime_email_test_get_code(
        &self,
        context: &RequestContext,
        email: String,
    ) -> FieldResult<Option<String>> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        // Normalize email: trim whitespace and convert to lowercase
        // This ensures consistency with how emails are stored by runtime_create_user
        // and runtime_request_login_verification
        let email = email.trim().to_lowercase();

        Ok(crate::email_service::get_test_code(&email))
    }

    /// Clear all captured test codes
    async fn runtime_email_test_clear_codes(&self, context: &RequestContext) -> FieldResult<bool> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        crate::email_service::clear_test_codes();
        Ok(true)
    }

    /// Test helper: Set expiry time for a verification code to simulate expiration
    async fn runtime_email_test_set_expiry(
        &self,
        context: &RequestContext,
        email: String,
        verification_type: String,
        expires_at: i32,
    ) -> FieldResult<bool> {
        use crate::agent::capabilities::ALL_CAPABILITY;

        // Check capability - admin only
        check_capability(&context.capabilities, &ALL_CAPABILITY)?;

        // Normalize email: trim whitespace and convert to lowercase
        // This ensures consistency with how emails are stored by runtime_create_user
        // and runtime_request_login_verification
        let email = email.trim().to_lowercase();

        let db = Ad4mDb::global_instance();
        let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
        let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
        // Convert i32 to i64 for database storage
        db_ref
            .set_verification_code_expiry(&email, &verification_type, expires_at as i64)
            .map_err(|e| {
                FieldError::new(format!("Failed to set expiry: {}", e), graphql_value!(null))
            })?;

        Ok(true)
    }

    async fn expression_create(
        &self,
        context: &RequestContext,
        content: String,
        language_address: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &EXPRESSION_CREATE_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let content_json: serde_json::Value =
            serde_json::from_str(&content).unwrap_or(serde_json::Value::String(content.clone()));
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

        controller
            .expression_create(&language_address, content_json, &agent_context)
            .await
            .map_err(|e| {
                FieldError::new(
                    format!("Failed to create expression on {}: {}", language_address, e),
                    Value::null(),
                )
            })
    }

    async fn expression_interact(
        &self,
        context: &RequestContext,
        interaction_call: InteractionCall,
        url: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &EXPRESSION_UPDATE_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&url) {
            if controller.is_language_loaded(&lang_address).await {
                match controller
                    .expression_interact(&url, &interaction_call)
                    .await
                {
                    Ok(Some(result)) => return Ok(result),
                    Ok(None) => return Ok("null".to_string()),
                    Err(e) => {
                        return Err(FieldError::new(
                            format!("expression_interact failed for {}: {}", url, e),
                            Value::null(),
                        ));
                    }
                }
            }
        }

        Err(FieldError::new(
            format!("Language not loaded for expression URL: {}", url),
            Value::null(),
        ))
    }

    async fn language_apply_template_and_publish(
        &self,
        context: &RequestContext,
        source_language_hash: String,
        template_data: String,
    ) -> FieldResult<LanguageRef> {
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;

        // Check if the language language is loaded on the Rust side
        let controller = LanguageController::global_instance();
        let language_language_loaded = {
            let sys = controller.system_addresses.lock().await;
            sys.language_language.is_some()
        };

        if language_language_loaded {
            // Rust-side implementation
            let template_map: serde_json::Map<String, serde_json::Value> =
                serde_json::from_str(&template_data).map_err(|e| {
                    FieldError::new(
                        format!("Invalid template_data JSON: {}", e),
                        graphql_value!(null),
                    )
                })?;

            let input = controller
                .language_apply_template_on_source(&source_language_hash, template_map)
                .await
                .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;

            let input_name = input.meta.name.clone();

            // Save the templated bundle locally so it can be loaded into a runtime
            if let Err(e) = controller.save_language_bundle(&input.bundle, None) {
                log::warn!("Failed to save templated language bundle locally: {}", e);
            }

            let language_language_address = {
                let sys = controller.system_addresses.lock().await;
                sys.language_language.clone().unwrap()
            };

            let input_json = serde_json::to_string(&input).map_err(|e| {
                FieldError::new(
                    format!("Failed to serialize language input: {}", e),
                    graphql_value!(null),
                )
            })?;

            let publish_script = format!(
                r#"await globalThis.__ad4m_language_instance__.expressionAdapter.putAdapter.createPublic({})"#,
                input_json
            );

            let address_raw = controller
                .execute_on_language(&language_language_address, &publish_script)
                .await
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to publish language: {}", e),
                        graphql_value!(null),
                    )
                })?;

            // Strip surrounding quotes from the address
            let address = address_raw.trim().trim_matches('"').to_string();

            // Load the templated language into a per-language runtime
            let bundle_on_disk = crate::utils::languages_directory()
                .join(&address)
                .join("bundle.js");
            if bundle_on_disk.exists() {
                if let Err(e) = controller.load_language(bundle_on_disk, false).await {
                    log::warn!("Failed to load templated language into runtime: {}", e);
                }
            }

            Ok(LanguageRef {
                address,
                name: input_name,
            })
        } else {
            Err(FieldError::new(
                "Language language not loaded - cannot apply template and publish".to_string(),
                graphql_value!(null),
            ))
        }
    }

    async fn language_publish(
        &self,
        context: &RequestContext,
        language_meta: LanguageMetaInput,
        language_path: String,
    ) -> FieldResult<LanguageMeta> {
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;

        let controller = LanguageController::global_instance();

        // Read the language bundle from disk
        let bundle = std::fs::read_to_string(&language_path).map_err(|e| {
            FieldError::new(
                format!("Failed to read language bundle: {}", e),
                graphql_value!(null),
            )
        })?;

        // Save the bundle locally
        let (hash, _bundle_path) = controller
            .save_language_bundle(&bundle, None)
            .map_err(|e| {
                FieldError::new(
                    format!("Failed to save language bundle: {}", e),
                    graphql_value!(null),
                )
            })?;

        // Build meta with the computed address.
        // Note: `author` and `templated` are NOT included in the data –
        // they come from the Expression envelope (author) or are derived (templated).
        // This matches the old JS LanguageMetaInternal behavior.
        let meta = LanguageMeta {
            name: language_meta.name.clone(),
            address: hash.clone(),
            description: Some(language_meta.description.clone()),
            possible_template_params: language_meta.possible_template_params.clone(),
            source_code_link: language_meta.source_code_link.clone(),
            ..LanguageMeta::default()
        };

        // Publish to the language language
        let language_language_address = {
            let sys = controller.system_addresses.lock().await;
            sys.language_language.clone().ok_or_else(|| {
                FieldError::new("Language language not loaded", graphql_value!(null))
            })?
        };

        // Create the language language input
        let language_input = LanguageLanguageInput {
            bundle: bundle.clone(),
            meta: meta.clone(),
        };

        let content = serde_json::to_value(&language_input).map_err(|e| {
            FieldError::new(
                format!("Failed to serialize language input: {}", e),
                graphql_value!(null),
            )
        })?;

        let agent_context = crate::agent::AgentContext::main_agent();
        controller
            .expression_create(&language_language_address, content, &agent_context)
            .await
            .map_err(|e| {
                FieldError::new(
                    format!("Failed to publish language: {}", e),
                    graphql_value!(null),
                )
            })?;

        // Load the language into a per-language runtime
        let bundle_on_disk = crate::utils::languages_directory()
            .join(&hash)
            .join("bundle.js");
        if bundle_on_disk.exists() {
            if let Err(e) = controller.load_language(bundle_on_disk, false).await {
                log::warn!("Failed to load published language into runtime: {}", e);
            }
        }

        // Build the response meta with all fields for GraphQL
        let response_meta = LanguageMeta {
            author: crate::agent::did(),
            templated: Some(false),
            ..meta
        };
        Ok(response_meta)
    }

    async fn language_remove(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &LANGUAGE_DELETE_CAPABILITY)?;
        let mut controller = LanguageController::global_instance();
        controller
            .language_remove(&address)
            .await
            .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
        Ok(true)
    }

    async fn language_write_settings(
        &self,
        context: &RequestContext,
        language_address: String,
        settings: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &LANGUAGE_UPDATE_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        if controller.is_language_loaded(&language_address).await {
            let settings_json: serde_json::Value = serde_json::from_str(&settings)
                .unwrap_or(serde_json::Value::String(settings.clone()));

            controller
                .write_settings(&language_address, settings_json)
                .await
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to write settings: {}", e),
                        graphql_value!(null),
                    )
                })?;

            controller
                .reload_language(&language_address)
                .await
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to reload language after settings change: {}", e),
                        graphql_value!(null),
                    )
                })?;

            return Ok(true);
        }

        Err(FieldError::new(
            format!("Language not loaded: {}", language_address),
            graphql_value!(null),
        ))
    }

    async fn neighbourhood_join_from_url(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<PerspectiveHandle> {
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(install_neighbourhood_with_context(url, &agent_context).await?)
    }

    async fn neighbourhood_publish_from_perspective(
        &self,
        context: &RequestContext,
        link_language: String,
        meta: PerspectiveInput,
        #[allow(non_snake_case)] perspectiveUUID: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let url = neighbourhoods::neighbourhood_publish_from_perspective_with_context(
            &perspectiveUUID,
            link_language,
            meta.into(),
            &agent_context,
        )
        .await?;

        Ok(url)
    }

    async fn neighbourhood_send_broadcast(
        &self,
        context: &RequestContext,
        payload: PerspectiveInput,
        #[allow(non_snake_case)] perspectiveUUID: String,
        loopback: Option<bool>,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let perspective = Perspective::from(payload);
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .send_broadcast(perspective.into(), loopback.unwrap_or(false))
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn neighbourhood_send_broadcast_u(
        &self,
        context: &RequestContext,
        payload: PerspectiveUnsignedInput,
        #[allow(non_snake_case)] perspectiveUUID: String,
        loopback: Option<bool>,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = Perspective {
            links: payload
                .links
                .into_iter()
                .map(Link::from)
                .map(|l| create_signed_expression(l.normalize(), &agent_context))
                .filter_map(Result::ok)
                .map(LinkExpression::from)
                .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
                .collect::<Vec<DecoratedLinkExpression>>(),
        };
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .send_broadcast(perspective.into(), loopback.unwrap_or(false))
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn neighbourhood_send_signal(
        &self,
        context: &RequestContext,
        payload: PerspectiveInput,
        #[allow(non_snake_case)] perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let perspective = Perspective::from(payload);
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .send_signal(remote_agent_did, perspective.into())
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn neighbourhood_send_signal_u(
        &self,
        context: &RequestContext,
        payload: PerspectiveUnsignedInput,
        #[allow(non_snake_case)] perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = Perspective {
            links: payload
                .links
                .into_iter()
                .map(Link::from)
                .map(|l| create_signed_expression(l.normalize(), &agent_context))
                .filter_map(Result::ok)
                .map(LinkExpression::from)
                .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
                .collect::<Vec<DecoratedLinkExpression>>(),
        };
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .send_signal(remote_agent_did, perspective.into())
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn neighbourhood_set_online_status(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] perspectiveUUID: String,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let perspective = Perspective::from(status);
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .set_online_status(perspective.into())
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn neighbourhood_set_online_status_u(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] perspectiveUUID: String,
        status: PerspectiveUnsignedInput,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let perspective = Perspective {
            links: status
                .links
                .into_iter()
                .map(|l| Link::from(l).normalize())
                .map(|l| create_signed_expression(l, &agent_context))
                .filter_map(Result::ok)
                .map(LinkExpression::from)
                .map(|l| DecoratedLinkExpression::from((l, LinkStatus::Shared)))
                .collect::<Vec<DecoratedLinkExpression>>(),
        };
        let perspective = create_signed_expression(perspective, &agent_context)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .set_online_status(perspective.into())
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;
        Ok(true)
    }

    async fn perspective_add(
        &self,
        context: &RequestContext,
        name: String,
    ) -> FieldResult<PerspectiveHandle> {
        check_capability(&context.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)?;

        // Determine owner DID based on user context
        let user_email_opt = user_email_from_token(context.auth_token.clone());

        let owner_did = if let Some(user_email) = user_email_opt {
            // Multi-user mode: set owner to the authenticated user's DID
            Some(
                AgentService::get_user_did_by_email(&user_email).map_err(|e| {
                    FieldError::new(
                        format!("Failed to get user DID: {}", e),
                        graphql_value!(null),
                    )
                })?,
            )
        } else {
            // Main agent mode: don't set owner for regular perspectives
            // Owner will be set when/if the perspective becomes a neighbourhood
            None
        };

        let handle = if let Some(owner) = &owner_did {
            PerspectiveHandle::new_with_owner(name.clone(), owner.clone())
        } else {
            // Fallback: create without owner (shouldn't happen now)
            PerspectiveHandle::new_from_name(name.clone())
        };

        add_perspective(handle.clone(), None).await?;
        Ok(handle)
    }

    async fn perspective_add_link(
        &self,
        context: &RequestContext,
        link: LinkInput,
        uuid: String,
        status: Option<String>,
        batch_id: Option<String>,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let result = perspective
            .add_link(
                link.into(),
                link_status_from_input(status)?,
                batch_id,
                &agent_context,
            )
            .await?;

        Ok(result)
    }

    async fn perspective_add_link_expression(
        &self,
        context: &RequestContext,
        link: LinkExpressionInput,
        uuid: String,
        status: Option<String>,
        batch_id: Option<String>,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        check_compute_credits(&context.auth_token)?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let link = crate::types::LinkExpression::try_from(link)?;
        let result = perspective
            .add_link_expression(link, link_status_from_input(status)?, batch_id)
            .await?;

        if let Err(e) = deduct_compute_credits(
            &context.auth_token,
            "link write",
            1.0,
            "link_write",
            Some(&format!("1 link in perspective {}", uuid)),
        ) {
            log::warn!("Call exceeded compute credits (add_link_expression): result returned but future calls will fail. Details: {:?}", e);
        }
        Ok(result)
    }

    async fn perspective_add_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkInput>,
        uuid: String,
        status: Option<String>,
        batch_id: Option<String>,
    ) -> FieldResult<Vec<DecoratedLinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let result = perspective
            .add_links(
                links.into_iter().map(|l| l.into()).collect(),
                link_status_from_input(status)?,
                batch_id,
                &agent_context,
            )
            .await?;

        Ok(result)
    }

    async fn perspective_link_mutations(
        &self,
        context: &RequestContext,
        mutations: LinkMutations,
        uuid: String,
        status: Option<String>,
    ) -> FieldResult<DecoratedPerspectiveDiff> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let result = perspective
            .link_mutations(mutations, link_status_from_input(status)?, &agent_context)
            .await?;

        Ok(result)
    }

    async fn perspective_publish_snapshot(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        unimplemented!()
    }

    async fn perspective_remove(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_delete_capability(vec![uuid.clone()]),
        )?;
        Ok(remove_perspective(&uuid).await.is_some())
    }

    async fn perspective_remove_link(
        &self,
        context: &RequestContext,
        link: LinkExpressionInput,
        uuid: String,
        batch_id: Option<String>,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let link = crate::types::LinkExpression::try_from(link)?;
        perspective.remove_link(link, batch_id).await?;
        Ok(true)
    }

    async fn perspective_remove_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkExpressionInput>,
        uuid: String,
        batch_id: Option<String>,
    ) -> FieldResult<Vec<DecoratedLinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let links = links
            .into_iter()
            .map(LinkExpression::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        let removed_links = perspective.remove_links(links, batch_id).await?;
        Ok(removed_links)
    }

    async fn perspective_update(
        &self,
        context: &RequestContext,
        name: String,
        uuid: String,
    ) -> FieldResult<PerspectiveHandle> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let perspective = get_perspective_with_access_control(&uuid, context).await?;
        let mut handle = perspective.persisted.lock().await.clone();
        handle.name = Some(name);
        update_perspective(&handle).await?;
        Ok(handle)
    }

    async fn perspective_update_link(
        &self,
        context: &RequestContext,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
        batch_id: Option<String>,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(perspective
            .update_link(
                LinkExpression::from_input_without_proof(old_link),
                new_link.into(),
                batch_id,
                &agent_context,
            )
            .await?)
    }

    async fn perspective_add_sdna(
        &self,
        context: &RequestContext,
        uuid: String,
        name: String,
        sdna_code: Option<String>,
        sdna_type: String,
        shacl_json: Option<String>,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let sdna_type = SdnaType::from_string(&sdna_type)
            .map_err(|e| FieldError::new(e, graphql_value!({ "invalid_sdna_type": sdna_type })))?;
        perspective
            .add_sdna(
                name,
                sdna_code.unwrap_or_default(),
                sdna_type,
                shacl_json,
                &agent_context,
            )
            .await?;
        Ok(true)
    }

    async fn perspective_execute_commands(
        &self,
        context: &RequestContext,
        uuid: String,
        commands: String,
        expression: String,
        parameters: Option<String>,
        batch_id: Option<String>,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

        let commands: Vec<Command> = serde_json::from_str(&commands)?;
        let parameters: Vec<Parameter> = if let Some(parameters) = parameters {
            serde_json::from_str(&parameters)?
        } else {
            vec![]
        };

        perspective
            .execute_commands(commands, expression, parameters, batch_id, &agent_context)
            .await?;

        Ok(true)
    }

    async fn perspective_create_subject(
        &self,
        context: &RequestContext,
        uuid: String,
        subject_class: String,
        expression_address: String,
        initial_values: Option<String>,
        batch_id: Option<String>,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

        let subject_class: SubjectClassOption = serde_json::from_str(&subject_class)?;
        let initial_values = if let Some(initial_values) = initial_values {
            Some(serde_json::from_str(&initial_values)?)
        } else {
            None
        };

        perspective
            .create_subject(
                subject_class,
                expression_address,
                initial_values,
                batch_id,
                &agent_context,
            )
            .await?;

        Ok(true)
    }

    async fn perspective_get_subject_data(
        &self,
        context: &RequestContext,
        uuid: String,
        subject_class: String,
        expression_address: String,
    ) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let subject_class: SubjectClassOption =
            serde_json::from_str(&subject_class).map_err(|e| {
                FieldError::new(
                    e,
                    graphql_value!({ "invalid_subject_class": subject_class }),
                )
            })?;

        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());

        let result = perspective
            .get_subject_data(subject_class, expression_address, &agent_context)
            .await?;
        Ok(result)
    }

    async fn perspective_subscribe_query(
        &self,
        context: &RequestContext,
        uuid: String,
        query: String,
    ) -> FieldResult<QuerySubscription> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        // Extract user context from auth token
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        let user_email = agent_context.user_email;

        let perspective = get_perspective_with_access_control(&uuid, context).await?;
        let (subscription_id, result_string) =
            perspective.subscribe_and_query(query, user_email).await?;

        Ok(QuerySubscription {
            subscription_id,
            result: result_string,
        })
    }

    async fn perspective_keep_alive_query(
        &self,
        context: &RequestContext,
        uuid: String,
        subscription_id: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let perspective = get_perspective_with_access_control(&uuid, context).await?;
        perspective.keepalive_query(subscription_id).await?;
        Ok(true)
    }

    async fn perspective_dispose_query_subscription(
        &self,
        context: &RequestContext,
        uuid: String,
        subscription_id: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let perspective = get_perspective_with_access_control(&uuid, context).await?;
        Ok(perspective
            .dispose_query_subscription(subscription_id)
            .await?)
    }

    async fn runtime_add_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)?;
        let friends = RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.add_friend(dids);
            runtime_service.get_friends()
        });

        Ok(friends)
    }

    async fn runtime_add_known_link_language_templates(
        &self,
        context: &RequestContext,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.add_know_link_language(addresses.clone());

            Ok(runtime_service.get_know_link_languages())
        })
    }

    async fn runtime_friend_send_message(
        &self,
        context: &RequestContext,
        did: String,
        message: PerspectiveInput,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_CREATE_CAPABILITY)?;

        let friends =
            RuntimeService::with_global_instance(|runtime_service| runtime_service.get_friends());

        if !friends.contains(&did.clone()) {
            log::error!("Friend not found: {}", did);

            return Ok(false);
        }

        // Direct message sending requires DM language - not yet ported to Rust
        log::warn!("runtime_friend_send_message: DM language interaction not yet ported to Rust");
        let _ = message;
        Err(FieldError::new(
            "DM send not implemented in Rust",
            Value::Null,
        ))
    }

    async fn runtime_hc_add_agent_infos(
        &self,
        context: &RequestContext,
        agent_infos: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY,
        )?;

        let agent_infos: Vec<String> = serde_json::from_str(&agent_infos)?;

        for agent_info in agent_infos.iter() {
            match serde_json::from_str::<serde_json::Value>(agent_info) {
                Ok(json) => {
                    log::info!(
                        "Adding Agent info: {}",
                        serde_json::to_string_pretty(&json).unwrap()
                    );
                }
                Err(e) => {
                    log::error!("Failed to parse agent info as JSON: {}", e);
                }
            }
        }

        get_holochain_service()
            .await
            .add_agent_infos(agent_infos)
            .await
            .map_err(|e| {
                log::error!("Failed to add agent infos: {:?}", e);
                e
            })?;

        Ok(true)
    }

    async fn runtime_open_link(&self, _context: &RequestContext, url: String) -> FieldResult<bool> {
        if webbrowser::open(&url).is_ok() {
            log::info!("Browser opened successfully");
            Ok(true)
        } else {
            log::info!("Failed to open browser");
            Ok(false)
        }
    }

    async fn runtime_quit(&self, context: &RequestContext) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_QUIT_CAPABILITY)?;
        // Trigger graceful shutdown via the global shutdown channel.
        // The main loop will shut down Holochain conductor, flush state, and exit cleanly.
        // If the shutdown channel is already consumed, assume shutdown is in progress and return success.
        if let Some(tx) = crate::globals::SHUTDOWN_TX.lock().unwrap().take() {
            log::info!("runtime_quit: sending graceful shutdown signal");
            let _ = tx.send(());
            Ok(true)
        } else {
            log::warn!("runtime_quit: shutdown channel already consumed, shutdown is in progress");
            Ok(true)
        }
    }

    async fn runtime_remove_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_DELETE_CAPABILITY)?;

        RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.remove_friend(dids.clone());

            Ok(runtime_service.get_friends())
        })
    }

    async fn runtime_remove_known_link_language_templates(
        &self,
        context: &RequestContext,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.remove_know_link_language(addresses.clone());

            Ok(runtime_service.get_know_link_languages())
        })
    }

    async fn runtime_set_status(
        &self,
        context: &RequestContext,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_MY_STATUS_UPDATE_CAPABILITY)?;
        // Runtime status setting requires DM language - not yet ported to Rust
        log::warn!("runtime_set_status: not yet ported to Rust");
        let _ = status;
        Ok(true)
    }

    async fn runtime_set_multi_user_enabled(
        &self,
        context: &RequestContext,
        enabled: bool,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        Ad4mDb::with_global_instance(|db| {
            db.set_multi_user_enabled(enabled)
                .map_err(|e| FieldError::new(e.to_string(), Value::null()))?;
            Ok(enabled)
        })
    }

    async fn runtime_set_free_hosting_enabled(
        &self,
        context: &RequestContext,
        enabled: bool,
    ) -> FieldResult<bool> {
        if !context.is_admin_credential {
            return Err(FieldError::new("Admin credentials required", Value::null()));
        }
        Ad4mDb::with_global_instance(|db| {
            db.set_free_hosting_enabled(enabled)
                .map_err(|e| FieldError::new(e.to_string(), Value::null()))?;
            // Mark all users dirty so the credit flush loop pushes updated
            // HostingUserInfo (with the new freeAccess value) to clients.
            if let Ok(users) = db.list_users() {
                for u in users {
                    mark_credits_dirty(&u.username);
                }
            }
            Ok(enabled)
        })
    }

    async fn runtime_request_install_notification(
        &self,
        context: &RequestContext,
        notification: NotificationInput,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        // Extract user context from auth token
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        let user_email = agent_context.user_email;
        Ok(RuntimeService::request_install_notification(notification, user_email).await?)
    }

    async fn runtime_update_notification(
        &self,
        context: &RequestContext,
        id: String,
        notification: NotificationInput,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Extract user context from auth token
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        let user_email = agent_context.user_email;

        // Fetch existing notification to verify ownership
        let existing_notification =
            Ad4mDb::with_global_instance(|db| db.get_notification(id.clone()))
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to fetch notification: {}", e),
                        Value::null(),
                    )
                })?
                .ok_or_else(|| FieldError::new("Notification not found", Value::null()))?;

        // Verify ownership: user_email must match
        if existing_notification.user_email != user_email {
            return Err(FieldError::new(
                "Permission denied: You do not own this notification",
                Value::null(),
            ));
        }

        // Build updated notification after ownership check
        // if managed user, preserve the granted status
        let mut notification =
            Notification::from_input_and_id(id.clone(), notification, user_email.clone());
        if user_email.is_some() {
            notification.granted = existing_notification.granted;
        }

        Ad4mDb::with_global_instance(|db| db.update_notification(id, &notification))?;

        Ok(true)
    }

    async fn runtime_remove_notification(
        &self,
        context: &RequestContext,
        id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Extract user context from auth token
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        let user_email = agent_context.user_email;

        // Fetch existing notification to verify ownership
        let existing_notification =
            Ad4mDb::with_global_instance(|db| db.get_notification(id.clone()))
                .map_err(|e| {
                    FieldError::new(
                        format!("Failed to fetch notification: {}", e),
                        Value::null(),
                    )
                })?
                .ok_or_else(|| FieldError::new("Notification not found", Value::null()))?;

        // Verify ownership: user_email must match
        if existing_notification.user_email != user_email {
            return Err(FieldError::new(
                "Permission denied: You do not own this notification",
                Value::null(),
            ));
        }

        // Proceed with removal after ownership check
        Ad4mDb::with_global_instance(|db| db.remove_notification(id))?;
        Ok(true)
    }

    async fn runtime_grant_notification(
        &self,
        context: &RequestContext,
        id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Only the main agent can grant notifications
        // Managed users have their notifications auto-granted on creation
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        if !agent_context.is_main_agent {
            return Err(FieldError::new(
                "Permission denied: Only the main agent can grant notifications",
                Value::null(),
            ));
        }

        let mut notification = Ad4mDb::with_global_instance(|db| db.get_notification(id.clone()))
            .map_err(|e| e.to_string())?
            .ok_or("Notification with given id not found")?;

        notification.granted = true;

        Ad4mDb::with_global_instance(|db| db.update_notification(id, &notification))
            .map_err(|e| e.to_string())?;

        Ok(true)
    }

    async fn runtime_export_db(
        &self,
        context: &RequestContext,
        file_path: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        let json_data =
            Ad4mDb::with_global_instance(|db| db.export_all_to_json()).map_err(|e| {
                FieldError::new(
                    "Failed to export database",
                    graphql_value!({ "error": e.to_string() }),
                )
            })?;

        // Write to file
        std::fs::write(&file_path, serde_json::to_string_pretty(&json_data)?).map_err(|e| {
            FieldError::new(
                "Failed to write export file",
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        Ok(true)
    }

    async fn runtime_import_db(
        &self,
        context: &RequestContext,
        file_path: String,
    ) -> FieldResult<ImportResult> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Read from file
        let json_str = std::fs::read_to_string(&file_path).map_err(|e| {
            FieldError::new(
                format!("Failed to read import file '{}': {}", file_path, e),
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        let json_data: serde_json::Value = serde_json::from_str(&json_str).map_err(|e| {
            FieldError::new(
                format!("Failed to parse JSON data: {}", e),
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        let result =
            Ad4mDb::with_global_instance(|db| db.import_from_json(json_data)).map_err(|e| {
                log::error!("Failed to import database: {}", e);
                FieldError::new(
                    format!("Failed to import database: {}", e),
                    graphql_value!({ "error": e.to_string() }),
                )
            })?;

        perspectives::initialize_from_db();

        Ok(result)
    }

    async fn ai_add_model(
        &self,
        context: &RequestContext,
        model: ModelInput,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        let id = AIService::global_instance().await?.add_model(model).await?;
        Ok(id)
    }

    async fn ai_update_model(
        &self,
        context: &RequestContext,
        model_id: String,
        model: ModelInput,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Update the model using AIService
        AIService::global_instance()
            .await?
            .update_model(model_id, model)
            .await
            .map_err(|e| {
                FieldError::new(
                    "Failed to update model",
                    graphql_value!({ "error": e.to_string() }),
                )
            })?;

        Ok(true)
    }

    async fn ai_remove_model(
        &self,
        context: &RequestContext,
        model_id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Remove the model using AIService
        AIService::global_instance()
            .await?
            .remove_model(model_id)
            .await
            .map_err(|e| {
                FieldError::new(
                    "Failed to remove model",
                    graphql_value!({ "error": e.to_string() }),
                )
            })?;

        Ok(true)
    }

    async fn ai_set_default_model(
        &self,
        context: &RequestContext,
        model_type: ModelType,
        model_id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        let maybe_model = Ad4mDb::with_global_instance(|db| db.get_model(model_id.clone()))
            .map_err(|e| e.to_string())?;
        if maybe_model.is_none() {
            return Err(FieldError::new(
                "Model not found",
                graphql_value!({ "model_id": model_id }),
            ));
        };

        AIService::global_instance()
            .await?
            .set_default_model(model_type, model_id)
            .await?;

        Ok(true)
    }

    async fn ai_add_task(
        &self,
        context: &RequestContext,
        task: AITaskInput,
    ) -> FieldResult<AITask> {
        check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)?;
        Ok(AIService::global_instance()
            .await?
            .add_task(task.clone())
            .await?)
    }

    async fn ai_remove_task(
        &self,
        context: &RequestContext,
        task_id: String,
    ) -> FieldResult<AITask> {
        check_capability(&context.capabilities, &AI_DELETE_CAPABILITY)?;
        if let Some(task) = AIService::get_tasks()?
            .into_iter()
            .find(|t| t.task_id == task_id)
        {
            AIService::global_instance()
                .await?
                .delete_task(task_id.clone())
                .await?;
            Ok(task)
        } else {
            Err(FieldError::new(
                "Task not found",
                graphql_value!({ "task_id": task_id }),
            ))
        }
    }

    async fn ai_update_task(
        &self,
        context: &RequestContext,
        task_id: String,
        task: AITaskInput,
    ) -> FieldResult<AITask> {
        check_capability(&context.capabilities, &AI_UPDATE_CAPABILITY)?;
        let mut task: AITask = task.into();
        task.task_id = task_id;
        Ok(AIService::global_instance()
            .await?
            .update_task(task.clone())
            .await?)
    }

    async fn ai_prompt(
        &self,
        context: &RequestContext,
        task_id: String,
        prompt: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)?;
        check_compute_credits(&context.auth_token)?;

        let result = AIService::global_instance()
            .await?
            .prompt(task_id, prompt)
            .await?;

        let total_tokens = result.prompt_tokens + result.completion_tokens;
        // Look up rate by model name (the host_rates key is just the model name)
        let model_name =
            match Ad4mDb::with_global_instance(|db| db.get_model(result.model_id.clone())) {
                Ok(Some(m)) => m.name,
                Ok(None) => {
                    log::warn!(
                        "Model not found in DB for model_id={}, using default rate",
                        result.model_id
                    );
                    String::new()
                }
                Err(e) => {
                    log::error!("DB error looking up model_id={}: {}", result.model_id, e);
                    String::new()
                }
            };
        if let Err(e) = deduct_compute_credits(
            &context.auth_token,
            &model_name,
            total_tokens as f64,
            "ai_prompt",
            Some(&format!(
                "{}: {} prompt + {} completion tokens",
                model_name, result.prompt_tokens, result.completion_tokens
            )),
        ) {
            log::warn!("Call exceeded compute credits (ai_prompt, model={}, tokens={}): result returned but future calls will fail. Details: {:?}", model_name, total_tokens, e);
        }

        Ok(result.text)
    }

    async fn ai_embed(
        &self,
        context: &RequestContext,
        model_id: String,
        text: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)?;
        check_compute_credits(&context.auth_token)?;

        let result = AIService::global_instance()
            .await?
            .embed(model_id, text)
            .await?;

        if let Err(e) = deduct_compute_credits(
            &context.auth_token,
            "embedding per token",
            result.token_count as f64,
            "ai_embed",
            Some(&format!("{} tokens", result.token_count)),
        ) {
            log::warn!("Call exceeded compute credits (ai_embed, tokens={}): result returned but future calls will fail. Details: {:?}", result.token_count, e);
        }

        let json_string = serde_json::to_string(&result.embeddings)
            .map_err(|e| FieldError::from(format!("Failed to serialize vector: {}", e)))?;

        // Compress the JSON string using zlib compression
        let compressed_bytes = deflate::deflate_bytes_zlib(json_string.as_bytes());

        // Encode the compressed bytes to base64
        let base64_encoded = BASE64_STANDARD.encode(compressed_bytes);

        Ok(base64_encoded)
    }

    async fn ai_open_transcription_stream(
        &self,
        context: &RequestContext,
        model_id: String,
        params: Option<VoiceActivityParamsInput>,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)?;
        check_compute_credits(&context.auth_token)?;

        // When billing is active, verify a rate is configured for this model
        // before spinning up the stream (and loading the Whisper model).
        if is_billing_active(&context.auth_token)? {
            let rate_key = Ad4mDb::with_global_instance(|db| db.get_model(model_id.clone()))
                .ok()
                .flatten()
                .map(|m| m.name)
                .unwrap_or_else(|| model_id.clone());
            let has_rate = Ad4mDb::with_global_instance(|db| db.get_host_rate(&rate_key))
                .map_err(|e| FieldError::new(e.to_string(), graphql_value!(null)))?;
            if has_rate.is_none() {
                return Err(FieldError::new(
                    format!(
                        "No host rate configured for '{}' — cannot open transcription stream",
                        rate_key
                    ),
                    graphql_value!(null),
                ));
            }
        }

        Ok(AIService::global_instance()
            .await?
            .open_transcription_stream(
                model_id,
                params.map(|p| p.into()),
                context.auth_token.clone(),
            )
            .await?)
    }

    // note: f32 does not implement IsInputType, so I'm taking f64 here
    async fn ai_feed_transcription_stream(
        &self,
        context: &RequestContext,
        stream_ids: Vec<String>,
        audio: Vec<f64>,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)?;
        check_compute_credits(&context.auth_token)?;
        let audio_f32: Vec<f32> = audio.into_iter().map(|x| x as f32).collect();
        let service = AIService::global_instance().await?;

        // Feed each stream individually
        for stream_id in &stream_ids {
            if let Err(e) = service
                .feed_transcription_stream(stream_id, audio_f32.clone(), &context.auth_token)
                .await
            {
                log::warn!("Error feeding stream {}: {}", stream_id, e);
            }
        }

        Ok(String::from("true"))
    }

    async fn ai_close_transcription_stream(
        &self,
        context: &RequestContext,
        stream_id: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY)?;
        AIService::global_instance()
            .await?
            .close_transcription_stream(&stream_id, &context.auth_token)
            .await?;
        Ok(String::from("true"))
    }

    async fn runtime_export_perspective(
        &self,
        context: &RequestContext,
        perspective_uuid: String,
        file_path: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Export the perspective
        let serialized = export_perspective(&perspective_uuid).await.map_err(|e| {
            FieldError::new(
                "Failed to export perspective",
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        // Write to file
        std::fs::write(&file_path, serde_json::to_string_pretty(&serialized)?).map_err(|e| {
            FieldError::new(
                "Failed to write export file",
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        Ok(true)
    }

    async fn runtime_import_perspective(
        &self,
        context: &RequestContext,
        file_path: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        // Read from file
        let json_str = std::fs::read_to_string(&file_path).map_err(|e| {
            FieldError::new(
                format!("Failed to read import file '{}': {}", file_path, e),
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        let serialized: SerializedPerspective = serde_json::from_str(&json_str).map_err(|e| {
            FieldError::new(
                format!("Failed to parse perspective data: {}", e),
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        // Import the perspective
        import_perspective(serialized).await.map_err(|e| {
            FieldError::new(
                format!("Failed to import perspective: {}", e),
                graphql_value!({ "error": e.to_string() }),
            )
        })?;

        Ok(true)
    }

    async fn perspective_create_batch(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let perspective = get_perspective_with_access_control(&uuid, context).await?;
        Ok(perspective.create_batch().await)
    }

    async fn perspective_commit_batch(
        &self,
        context: &RequestContext,
        uuid: String,
        batch_id: String,
    ) -> FieldResult<DecoratedPerspectiveDiff> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context).await?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(perspective.commit_batch(batch_id, &agent_context).await?)
    }

    async fn runtime_restart_holochain(&self, context: &RequestContext) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_QUIT_CAPABILITY)?;

        log::info!("Restarting Holochain service...");

        let interface = get_holochain_service().await;

        // This will shut down the conductor and exit the service thread
        interface.shutdown().await?;

        // Wait a moment for the service to shut down completely
        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

        // Restart the service with the stored config
        crate::holochain_service::HolochainService::restart_service().await?;

        log::info!("Holochain service has been restarted successfully.");

        Ok(true)
    }

    async fn runtime_set_hot_wallet_address(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_UPDATE_CAPABILITY)?;

        let user_email = user_email_from_token(context.auth_token.clone()).ok_or_else(|| {
            FieldError::new(
                "Setting hot wallet address requires multi-user authentication",
                Value::null(),
            )
        })?;

        Ad4mDb::with_global_instance(|db| {
            db.set_user_hot_wallet(&user_email, &address).map_err(|e| {
                FieldError::new(
                    format!("Failed to set hot wallet address: {}", e),
                    Value::null(),
                )
            })
        })?;

        Ok(true)
    }

    async fn runtime_request_payment(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] amountHOT: String,
    ) -> FieldResult<PaymentRequestResult> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_UPDATE_CAPABILITY)?;

        // When free hosting is enabled, payments are not applicable
        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
        if global_free {
            return Ok(PaymentRequestResult {
                success: false,
                message: "Payments are disabled — this host is configured for free access."
                    .to_string(),
            });
        }

        let user_email = user_email_from_token(context.auth_token.clone()).ok_or_else(|| {
            FieldError::new(
                "Payment requests require multi-user authentication",
                Value::null(),
            )
        })?;

        // Validate amountHOT is a positive number
        let parsed_amount: f64 = amountHOT.trim().parse().map_err(|_| {
            FieldError::new(
                format!("Invalid amountHOT '{}': must be a valid number", amountHOT),
                Value::null(),
            )
        })?;
        if parsed_amount <= 0.0 {
            return Err(FieldError::new(
                format!("amountHOT must be positive, got {}", parsed_amount),
                Value::null(),
            ));
        }

        // Look up user's wHOT wallet address (= Holochain AgentPubKey)
        let wallet_address = Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user_email))
            .map_err(|e| FieldError::new(format!("DB error: {}", e), Value::null()))?;

        let wallet_address = match wallet_address {
            Some(addr) if !addr.is_empty() => addr,
            _ => {
                return Ok(PaymentRequestResult {
                    success: false,
                    message: "User has not set their wHOT wallet address. Call setHotWalletAddress first.".to_string(),
                });
            }
        };

        // Create payment proposal via Unyt alliance DNA
        let note = format!("AD4M hosting top-up: {} wHOT for {}", amountHOT, user_email);
        match crate::unyt_service::create_proposal(&amountHOT, &wallet_address, Some(&note)).await {
            Ok(proposal_hash) => {
                // Record the payment request in the DB — fail the whole operation if this doesn't persist
                if let Err(e) = Ad4mDb::with_global_instance(|db| {
                    db.create_payment_request(&user_email, &amountHOT, &proposal_hash)
                }) {
                    log::error!("Failed to record payment request in DB: {}", e);
                    return Err(FieldError::new(
                        format!("Payment proposal created but failed to persist: {}", e),
                        Value::null(),
                    ));
                }

                log::info!(
                    "Created wHOT payment proposal {} for user={} amount={}",
                    proposal_hash,
                    user_email,
                    amountHOT
                );

                Ok(PaymentRequestResult {
                    success: true,
                    message: format!(
                        "Payment proposal created (hash: {}). Awaiting approval in user's wHOT wallet.",
                        proposal_hash
                    ),
                })
            }
            Err(e) => {
                let err_str = e.to_string();
                log::error!(
                    "Failed to create wHOT payment proposal for user={}: {}",
                    user_email,
                    err_str
                );
                let message = if err_str.contains("No Global Definition found") {
                    "The host is still syncing the Unyt currency network. Please try again in a few minutes."
                        .to_string()
                } else if err_str.contains("CellDisabled") {
                    "The host's Unyt service is temporarily unavailable. Please try again later."
                        .to_string()
                } else {
                    format!("Failed to create payment proposal: {}", err_str)
                };
                Ok(PaymentRequestResult {
                    success: false,
                    message,
                })
            }
        }
    }

    async fn runtime_set_user_credits(
        &self,
        context: &RequestContext,
        email: String,
        amount: f64,
    ) -> FieldResult<bool> {
        // Admin-only: only the launcher's admin credential can set user credits
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can set user credits",
                Value::null(),
            ));
        }

        if amount < 0.0 || amount.is_nan() || amount.is_infinite() {
            return Err(FieldError::new(
                "Invalid credit amount: must be a finite, non-negative number",
                Value::null(),
            ));
        }

        Ad4mDb::with_global_instance(|db| {
            db.set_user_credits(&email, amount).map_err(|e| {
                FieldError::new(format!("Failed to set user credits: {}", e), Value::null())
            })
        })?;

        mark_credits_dirty(&email);
        Ok(true)
    }

    async fn runtime_set_user_free_access(
        &self,
        context: &RequestContext,
        email: String,
        enabled: bool,
    ) -> FieldResult<bool> {
        // Admin-only: only the launcher's admin credential can grant/revoke free access
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can set user free access",
                Value::null(),
            ));
        }

        Ad4mDb::with_global_instance(|db| {
            db.set_user_free_access(&email, enabled).map_err(|e| {
                FieldError::new(
                    format!("Failed to set user free access: {}", e),
                    Value::null(),
                )
            })
        })?;

        mark_credits_dirty(&email);
        Ok(true)
    }

    /// Reinstall the Unyt alliance DNA (e.g. after version update).
    async fn runtime_reinstall_unyt_dna(
        &self,
        context: &RequestContext,
    ) -> FieldResult<PaymentRequestResult> {
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can reinstall the Unyt DNA",
                Value::null(),
            ));
        }

        match crate::unyt_service::reinstall().await {
            Ok(()) => Ok(PaymentRequestResult {
                success: true,
                message: "Unyt alliance DNA reinstalled successfully".to_string(),
            }),
            Err(e) => Ok(PaymentRequestResult {
                success: false,
                message: format!("Failed to reinstall: {}", e),
            }),
        }
    }

    /// Send wHOT from the host's wallet to an external address.
    async fn runtime_send_hot(
        &self,
        context: &RequestContext,
        recipient: String,
        amount: String,
    ) -> FieldResult<PaymentRequestResult> {
        // Admin-only: only the host operator can send from the wallet
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can send wHOT",
                Value::null(),
            ));
        }

        // Validate inputs
        let recipient = recipient.trim().to_string();
        if recipient.is_empty() {
            return Err(FieldError::new(
                "recipient must not be empty",
                Value::null(),
            ));
        }
        let parsed_amount: f64 = amount.parse().map_err(|_| {
            FieldError::new(
                format!("amount '{}' is not a valid number", amount),
                Value::null(),
            )
        })?;
        if parsed_amount <= 0.0 {
            return Err(FieldError::new(
                "amount must be greater than 0",
                Value::null(),
            ));
        }

        match crate::unyt_service::send_hot(&recipient, &amount, Some("AD4M host withdrawal")).await
        {
            Ok(commitment_hash) => Ok(PaymentRequestResult {
                success: true,
                message: format!(
                    "Sent {} wHOT to {} (commitment: {})",
                    amount, recipient, commitment_hash
                ),
            }),
            Err(e) => Ok(PaymentRequestResult {
                success: false,
                message: format!("Failed to send wHOT: {}", e),
            }),
        }
    }

    /// Set host rates used for credit deduction.
    /// Expects a JSON string: [{"description": "...", "priceInHOT": 0.01}, ...]
    async fn runtime_set_host_rates(
        &self,
        context: &RequestContext,
        rates_json: String,
    ) -> FieldResult<bool> {
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can set host rates",
                Value::null(),
            ));
        }

        let parsed: Vec<serde_json::Value> = serde_json::from_str(&rates_json)
            .map_err(|e| FieldError::new(format!("Invalid rates JSON: {}", e), Value::null()))?;

        let rates: Vec<(String, f64)> = parsed
            .iter()
            .enumerate()
            .map(|(i, item)| {
                let desc = item
                    .get("description")
                    .and_then(|v| v.as_str())
                    .filter(|s| !s.is_empty())
                    .ok_or_else(|| {
                        FieldError::new(
                            format!("Rate entry {} missing or empty 'description'", i),
                            Value::null(),
                        )
                    })?;
                let price = item
                    .get("priceInHOT")
                    .and_then(|v| v.as_f64())
                    .ok_or_else(|| {
                        FieldError::new(
                            format!("Rate entry {} missing 'priceInHOT'", i),
                            Value::null(),
                        )
                    })?;
                if price < 0.0 {
                    return Err(FieldError::new(
                        format!("Rate entry {} has negative priceInHOT", i),
                        Value::null(),
                    ));
                }
                Ok((desc.to_string(), price))
            })
            .collect::<FieldResult<Vec<_>>>()?;

        Ad4mDb::with_global_instance(|db| {
            db.set_host_rates(&rates).map_err(|e| {
                FieldError::new(format!("Failed to set host rates: {}", e), Value::null())
            })
        })?;

        Ok(true)
    }

    /// Store a membrane proof for Unyt alliance DNA installation.
    /// The proof should be base64-encoded bytes from the joining server.
    async fn runtime_set_unyt_membrane_proof(
        &self,
        context: &RequestContext,
        proof: String,
    ) -> FieldResult<PaymentRequestResult> {
        if !context.is_admin_credential {
            return Err(FieldError::new(
                "Only the admin (launcher) can set the membrane proof",
                Value::null(),
            ));
        }

        match crate::unyt_service::set_membrane_proof(&proof) {
            Ok(()) => {
                // Trigger DNA installation now that we have the proof
                match crate::unyt_service::ensure_installed().await {
                    Ok(()) => {
                        log::info!("Unyt alliance DNA installed after membrane proof was set");
                        Ok(PaymentRequestResult {
                            success: true,
                            message: "Membrane proof stored and DNA installed.".to_string(),
                        })
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to install Unyt alliance DNA after membrane proof: {}",
                            e
                        );
                        Ok(PaymentRequestResult {
                            success: false,
                            message: format!(
                                "Membrane proof stored but DNA installation failed: {}",
                                e
                            ),
                        })
                    }
                }
            }
            Err(e) => Ok(PaymentRequestResult {
                success: false,
                message: format!("Failed to store membrane proof: {}", e),
            }),
        }
    }
}
