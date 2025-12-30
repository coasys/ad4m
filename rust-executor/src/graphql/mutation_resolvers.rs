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
    pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC},
};
use base64::prelude::*;

// Use the shared can_access_perspective function from query_resolvers
use super::query_resolvers::can_access_perspective;

// Helper function to get perspective with access control
fn get_perspective_with_access_control(
    uuid: &str,
    context: &RequestContext,
) -> FieldResult<PerspectiveInstance> {
    let perspective = get_perspective_with_uuid_field_error(uuid)?;
    let user_email = user_email_from_token(context.auth_token.clone());

    // Check access to the perspective
    let handle = futures::executor::block_on(perspective.persisted.lock()).clone();
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
        let agent = AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.create_new_keys();
            agent_service.save(passphrase.clone());

            // Store passphrase so future wallet modifications (e.g., adding user keys) can be saved
            agent_service.passphrase = Some(passphrase.clone());

            agent_service.dump().clone()
        });

        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerate", {{ passphrase: "{}" }})
            )"#,
            passphrase
        );
        js.execute(script).await?;

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
        let auth_info: AuthInfo = auth_info.into();
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
        holochain: bool,
    ) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)?;

        let agent_instance = AgentService::global_instance();
        {
            let mut agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &mut AgentService = agent_service.as_mut().expect("agent instance");

            agent_ref.unlock(passphrase.clone())?
        }

        if agent_instance
            .lock()
            .expect("agent lock")
            .as_ref()
            .expect("agent instance")
            .is_unlocked()
        {
            let mut js = context.js_handle.clone();
            let script = format!(
                r#"JSON.stringify(
                    await core.callResolver("Mutation", "agentUnlock", {{ passphrase: "{}", holochain: "{}" }})
                )"#,
                passphrase, holochain
            );
            js.execute(script).await?;
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
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdateDirectMessageLanguage", {{ directMessageLanguage: "{}" }})
            )"#,
            direct_message_language
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
            let mut js_handle = context.js_handle.clone();
            if let Err(e) =
                AgentService::publish_user_agent_to_language(&user_email, &agent, &mut js_handle)
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
            // Fallback to JS implementation for main agent
            let mut js = context.js_handle.clone();
            let perspective_json = serde_json::to_string(&perspective)?;
            let script = format!(
                r#"JSON.stringify(
                    await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }})
                )"#,
                perspective_json
            );
            let result = js.execute(script).await?;
            let result: JsResultType<Agent> = serde_json::from_str(&result)?;
            result.get_graphql_result()
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
    ) -> FieldResult<UserCreationResult> {
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

        // Create new user
        let user = User {
            username: email.clone(),
            did: did.clone(),
            password: password, // Store password for now (will improve with CAL-compliant key derivation later)
            last_seen: None,
        };

        // Add user to database
        {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref.add_user(&user).map_err(|e| {
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
        let mut js_handle = context.js_handle.clone();
        if let Err(e) =
            AgentService::publish_user_agent_to_language(&email, &initial_agent, &mut js_handle)
                .await
        {
            log::warn!("Failed to publish user {} to agent language: {}", did, e);
            // Don't fail the user creation, just log the warning
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
        // Check capability (empty tokens get user management caps in multi-user mode)
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
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

        // Get user from database
        let db = Ad4mDb::global_instance();
        let user = {
            let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
            let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
            db_ref.get_user(&email).map_err(|e| {
                FieldError::new(format!("User not found: {}", e), graphql_value!(null))
            })?
        };

        // Verify password (simple comparison for now)
        if user.password != password {
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
            // Admin context - use default app info
            AuthInfo {
                app_name: "multi-user-app".to_string(),
                app_desc: "Multi-user application".to_string(),
                app_domain: Some("multi-user".to_string()),
                app_url: Some("https://multi-user.app".to_string()),
                app_icon_path: None,
                capabilities: Some(vec![ALL_CAPABILITY.clone()]),
                user_email: Some(user.username.clone()),
            }
        } else {
            // App context - preserve the original app info and add user supposed to be the same as the caller
            match decode_jwt(context.auth_token.clone()) {
                Ok(current_claims) => {
                    let mut auth_info = current_claims.capabilities;
                    auth_info.user_email = Some(user.username.clone());
                    auth_info
                }
                Err(_) => {
                    // Fallback if token decode fails
                    AuthInfo {
                        app_name: "multi-user-app".to_string(),
                        app_desc: "Multi-user application".to_string(),
                        app_domain: Some("multi-user".to_string()),
                        app_url: Some("https://multi-user.app".to_string()),
                        app_icon_path: None,
                        capabilities: Some(vec![ALL_CAPABILITY.clone()]),
                        user_email: Some(user.username.clone()),
                    }
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

    async fn expression_create(
        &self,
        context: &RequestContext,
        content: String,
        language_address: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &EXPRESSION_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();

        // Get user context from JWT token
        let user_email =
            crate::agent::capabilities::user_email_from_token(context.auth_token.clone());

        let script = if let Some(user_email) = user_email {
            // User context: set agent service context temporarily
            format!(
                r#"JSON.stringify(
                    await (async () => {{
                        const originalContext = core.agentService.getUserContext();
                        core.agentService.setUserContext("{}");
                        try {{
                            return await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: "{}" }});
                        }} finally {{
                            core.agentService.setUserContext(originalContext);
                        }}
                    }})()
                )"#,
                user_email, content, language_address
            )
        } else {
            // Main agent context: call directly
            format!(
                r#"JSON.stringify(
                    await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: "{}" }})
                )"#,
                content, language_address
            )
        };

        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_interact(
        &self,
        context: &RequestContext,
        interaction_call: InteractionCall,
        url: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &EXPRESSION_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();

        // Get user context from JWT token
        let user_email =
            crate::agent::capabilities::user_email_from_token(context.auth_token.clone());

        let interaction_call_json = serde_json::to_string(&interaction_call)?;
        let script = if let Some(user_email) = user_email {
            // User context: set agent service context temporarily
            format!(
                r#"JSON.stringify(
                    await (async () => {{
                        const originalContext = core.agentService.getUserContext();
                        core.agentService.setUserContext("{}");
                        try {{
                            return await core.callResolver(
                                "Mutation",
                                "expressionInteract",
                                {{ interactionCall: {}, url: "{}" }},
                            );
                        }} finally {{
                            core.agentService.setUserContext(originalContext);
                        }}
                    }})()
                )"#,
                user_email, interaction_call_json, url
            )
        } else {
            // Main agent context: call directly
            format!(
                r#"JSON.stringify(
                await core.callResolver(
                    "Mutation",
                    "expressionInteract",
                    {{ interactionCall: {}, url: "{}" }},
                ))"#,
                interaction_call_json, url
            )
        };

        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_apply_template_and_publish(
        &self,
        context: &RequestContext,
        source_language_hash: String,
        template_data: String,
    ) -> FieldResult<LanguageRef> {
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageApplyTemplateAndPublish",
                {{ sourceLanguageHash: "{}", templateData: JSON.stringify({}) }},
            ))"#,
            source_language_hash, template_data
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LanguageRef> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_publish(
        &self,
        context: &RequestContext,
        language_meta: LanguageMetaInput,
        language_path: String,
    ) -> FieldResult<LanguageMeta> {
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let language_meta_json = serde_json::to_string(&language_meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languagePublish",
                {{ languageMeta: {}, languagePath: "{}" }},
            ))"#,
            language_meta_json, language_path
        );

        let result = js.execute(script).await?;
        println!("language_publish result: {:?}", result);
        let result: JsResultType<LanguageMeta> = serde_json::from_str(&result)?;
        println!("language_publish result 1: {:?}", result);
        result.get_graphql_result()
    }

    async fn language_remove(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &LANGUAGE_DELETE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageRemove",
                {{ address: "{}" }},
            ))"#,
            address
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_write_settings(
        &self,
        context: &RequestContext,
        language_address: String,
        settings: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &LANGUAGE_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageWriteSettings",
                {{ languageAddress: "{}", settings: "{}" }},
            ))"#,
            language_address, settings
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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

        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(perspective
            .add_link(
                link.into(),
                link_status_from_input(status)?,
                batch_id,
                &agent_context,
            )
            .await?)
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
        let link = crate::types::LinkExpression::try_from(link)?;
        Ok(perspective
            .add_link_expression(link, link_status_from_input(status)?, batch_id)
            .await?)
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(perspective
            .add_links(
                links.into_iter().map(|l| l.into()).collect(),
                link_status_from_input(status)?,
                batch_id,
                &agent_context,
            )
            .await?)
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(perspective
            .link_mutations(mutations, link_status_from_input(status)?, &agent_context)
            .await?)
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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
        let perspective = get_perspective_with_access_control(&uuid, context)?;
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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
        sdna_code: String,
        sdna_type: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let sdna_type = SdnaType::from_string(&sdna_type)
            .map_err(|e| FieldError::new(e, graphql_value!({ "invalid_sdna_type": sdna_type })))?;
        perspective
            .add_sdna(name, sdna_code, sdna_type, &agent_context)
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

        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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

        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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

        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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

        let perspective = get_perspective_with_access_control(&uuid, context)?;
        let (subscription_id, result_string) = perspective.subscribe_and_query(query).await?;

        Ok(QuerySubscription {
            subscription_id,
            result: result_string,
        })
    }

    async fn perspective_subscribe_surreal_query(
        &self,
        context: &RequestContext,
        uuid: String,
        query: String,
    ) -> FieldResult<QuerySubscription> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let perspective = get_perspective_with_uuid_field_error(&uuid)?;
        let (subscription_id, result_string) =
            perspective.subscribe_and_query_surreal(query).await?;

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

        let perspective = get_perspective_with_access_control(&uuid, context)?;
        perspective.keepalive_query(subscription_id).await?;
        Ok(true)
    }

    async fn perspective_keep_alive_surreal_query(
        &self,
        context: &RequestContext,
        uuid: String,
        subscription_id: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let perspective = get_perspective_with_uuid_field_error(&uuid)?;
        perspective.keepalive_surreal_query(subscription_id).await?;
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

        let perspective = get_perspective_with_access_control(&uuid, context)?;
        Ok(perspective
            .dispose_query_subscription(subscription_id)
            .await?)
    }

    async fn perspective_dispose_surreal_query_subscription(
        &self,
        context: &RequestContext,
        uuid: String,
        subscription_id: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let perspective = get_perspective_with_uuid_field_error(&uuid)?;
        Ok(perspective
            .dispose_surreal_query_subscription(subscription_id)
            .await?)
    }

    async fn runtime_add_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)?;
        let cloned_did = dids.clone();
        let friends = RuntimeService::with_global_instance(|runtime_service| {
            runtime_service.add_friend(dids);
            runtime_service.get_friends()
        });

        // TODO: remove this when language controller is moved.
        let mut js = context.js_handle.clone();
        let dids_json = serde_json::to_string(&cloned_did)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddFriends",
                {{ dids: {} }},
            ))"#,
            dids_json,
        );
        let result = js.execute(script).await?;
        // TODO: what is this for? result is not used.. should this error if it can't be parsed?
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()?;

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

        let mut js = context.js_handle.clone();
        let message_json = serde_json::to_string(&message)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeFriendSendMessage",
                {{ did: "{}", message: {} }},
            ))"#,
            did, message_json,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        let get_graphql_result = result.get_graphql_result()?;
        Ok(get_graphql_result)
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
        std::process::exit(0);
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
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeSetStatus",
                {{ status: {} }},
            ))"#,
            status_json,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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

    async fn runtime_request_install_notification(
        &self,
        context: &RequestContext,
        notification: NotificationInput,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        Ok(RuntimeService::request_install_notification(notification).await?)
    }

    async fn runtime_update_notification(
        &self,
        context: &RequestContext,
        id: String,
        notification: NotificationInput,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;

        let notification = Notification::from_input_and_id(id.clone(), notification);

        Ad4mDb::with_global_instance(|db| db.update_notification(id, &notification))?;

        Ok(true)
    }

    async fn runtime_remove_notification(
        &self,
        context: &RequestContext,
        id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        Ad4mDb::with_global_instance(|db| db.remove_notification(id))?;
        Ok(true)
    }

    async fn runtime_grant_notification(
        &self,
        context: &RequestContext,
        id: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
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
        check_capability(&context.capabilities, &AI_CREATE_CAPABILITY)?;
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
        Ok(AIService::global_instance()
            .await?
            .prompt(task_id, prompt)
            .await?)
    }

    async fn ai_embed(
        &self,
        context: &RequestContext,
        model_id: String,
        text: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AI_PROMPT_CAPABILITY)?;
        let vector = AIService::global_instance()
            .await?
            .embed(model_id, text)
            .await?;
        let json_string = serde_json::to_string(&vector)
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
        Ok(AIService::global_instance()
            .await?
            .open_transcription_stream(model_id, params.map(|p| p.into()))
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
        let audio_f32: Vec<f32> = audio.into_iter().map(|x| x as f32).collect();
        let service = AIService::global_instance().await?;

        // Feed each stream individually
        for stream_id in &stream_ids {
            if let Err(e) = service
                .feed_transcription_stream(stream_id, audio_f32.clone())
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
            .close_transcription_stream(&stream_id)
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
        let perspective = get_perspective_with_access_control(&uuid, context)?;
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
        let mut perspective = get_perspective_with_access_control(&uuid, context)?;
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
}
