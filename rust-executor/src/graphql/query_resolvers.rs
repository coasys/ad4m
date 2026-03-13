#![allow(non_snake_case)]
use super::graphql_types::*;
use crate::agent::{capabilities::*, did_document_for_context, signatures, AgentContext};
use crate::ai_service::AIService;
use crate::languages::LanguageController;
use crate::types::{AITask, DecoratedExpressionProof, ModelType};
use crate::{agent::AgentService, entanglement_service::get_entanglement_proofs};
use crate::{
    db::Ad4mDb,
    globals::AD4M_VERSION,
    holochain_service::get_holochain_service,
    perspectives::{all_perspectives, get_perspective, utils::prolog_resolution_to_string},
    runtime_service::RuntimeService,
    types::{DecoratedLinkExpression, Model, Notification},
};
use coasys_juniper::{graphql_object, FieldError, FieldResult, Value};

pub struct Query;

// Helper function to check if a user can access a perspective
pub fn can_access_perspective(
    user_email: &Option<String>,
    perspective: &PerspectiveHandle,
) -> bool {
    match user_email {
        Some(email) => {
            // User context: check if user is in owners list
            if let Ok(user_did) = AgentService::get_user_did_by_email(email) {
                log::debug!(
                    "📋 can_access_perspective(): user {} perspective {} user_did {}",
                    email,
                    perspective.uuid,
                    user_did
                );
                log::debug!(
                    "📋 can_access_perspective(): perspective.owners {:?}",
                    perspective.owners
                );
                perspective.is_owned_by(&user_did)
            } else {
                log::debug!("📋 can_access_perspective(): No DID for user {}", email);
                false
            }
        }
        None => {
            // Main agent context: access unowned perspectives OR perspectives owned by main agent
            if perspective.is_unowned() {
                true
            } else {
                // Check if the main agent owns this perspective
                AgentService::with_global_instance(|agent_service| {
                    if let Some(main_agent_did) = &agent_service.did {
                        perspective.is_owned_by(main_agent_did)
                    } else {
                        false
                    }
                })
            }
        }
    }
}

#[graphql_object(context = RequestContext)]
impl Query {
    async fn agent(&self, context: &RequestContext) -> FieldResult<Agent> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;

        // For multi-user mode: extract user DID from JWT token if present
        if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
            let agent_data = AgentService::get_user_agent_data(&user_email).map_err(|e| {
                FieldError::new(format!("User agent not available: {}", e), Value::null())
            })?;

            // Try to load user-specific profile, fallback to empty profile
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

            return Ok(agent);
        }

        // Fallback to main agent for admin/legacy mode
        AgentService::with_global_instance(|agent_service| {
            let mut agent = agent_service
                .agent
                .clone()
                .ok_or(FieldError::new("Agent not found", Value::null()))?;

            if agent.perspective.is_some() {
                agent.perspective.as_mut().unwrap().verify_link_signatures();
            }

            Ok(agent)
        })
    }

    #[graphql(name = "agentByDID")]
    async fn agent_by_did(
        &self,
        context: &RequestContext,
        did: String,
    ) -> FieldResult<Option<Agent>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let agent_instance = AgentService::global_instance();
        let did_match = {
            let agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
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
                        // Verify link signatures in the agent's perspective,
                        // same as agent_me() does
                        let agent = agent.map(|mut a| {
                            if a.perspective.is_some() {
                                a.perspective.as_mut().unwrap().verify_link_signatures();
                            }
                            a
                        });
                        Ok(agent)
                    }
                    Ok(None) => Ok(None),
                    Err(e) => {
                        log::warn!("agentByDID: failed to get expression for {}: {}", did, e);
                        Err(FieldError::new(
                            format!("agentByDID: failed to get expression for {}: {}", did, e),
                            Value::null(),
                        ))
                    }
                }
            } else {
                Ok(None)
            }
        } else {
            let agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
            Ok(agent_ref.agent.clone())
        }
    }

    async fn agent_get_apps(&self, context: &RequestContext) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_get_entanglement_proofs(
        &self,
        _context: &RequestContext,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let proofs = get_entanglement_proofs();
        Ok(proofs)
    }

    async fn agent_is_locked(&self, _context: &RequestContext) -> FieldResult<bool> {
        AgentService::with_global_instance(|agent_service| {
            let _agent = agent_service
                .agent
                .clone()
                .ok_or(FieldError::new("Agent not found", Value::null()))?;

            Ok(!agent_service.is_unlocked())
        })
    }

    async fn agent_status(&self, context: &RequestContext) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;

        // For multi-user mode: extract user DID from JWT token if present
        if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
            let agent_data = AgentService::get_user_agent_data(&user_email).map_err(|e| {
                FieldError::new(format!("User agent not available: {}", e), Value::null())
            })?;

            // Generate DID document for user
            let agent_context = AgentContext::for_user_email(user_email);
            let did_document = did_document_for_context(&agent_context).map_err(|e| {
                FieldError::new(
                    format!("Failed to get DID document for user: {}", e),
                    Value::null(),
                )
            })?;

            return Ok(AgentStatus {
                did: Some(agent_data.did),
                did_document: Some(serde_json::to_string(&did_document).map_err(|e| {
                    FieldError::new(
                        format!("Failed to serialize DID document: {}", e),
                        Value::null(),
                    )
                })?),
                error: None,
                is_initialized: true,
                is_unlocked: true,
            });
        }

        // Fallback to main agent status for admin/legacy mode
        AgentService::with_global_instance(|agent_service| Ok(agent_service.dump()))
    }

    async fn expression(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<Option<ExpressionRendered>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let parsed = LanguageController::parse_expr_url(&url);

        if let Ok((lang_address, expression_address)) = parsed {
            let is_literal = lang_address == "literal";
            let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

            if is_loaded {
                match controller
                    .get_expression(&lang_address, &expression_address)
                    .await
                {
                    Ok(Some(expr_json)) => {
                        return Ok(Some(build_expression_rendered(&expr_json, &lang_address)));
                    }
                    Ok(None) => {
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(FieldError::new(
                            format!("Failed to get expression {}: {}", url, e),
                            Value::null(),
                        ));
                    }
                }
            }
        }

        // Language not loaded
        Ok(None)
    }

    async fn expression_interactions(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<Vec<InteractionMeta>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&url) {
            if controller.is_language_loaded(&lang_address).await {
                return controller.expression_interactions(&url).await.map_err(|e| {
                    FieldError::new(
                        format!("Failed to get expression interactions for {}: {}", url, e),
                        Value::null(),
                    )
                });
            }
        }

        Ok(vec![])
    }

    async fn expression_many(
        &self,
        context: &RequestContext,
        urls: Vec<String>,
    ) -> FieldResult<Vec<Option<ExpressionRendered>>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let mut results = Vec::new();

        for url in urls.iter() {
            if let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(url)
            {
                let is_literal = lang_address == "literal";
                let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

                if is_loaded {
                    match controller
                        .get_expression(&lang_address, &expression_address)
                        .await
                    {
                        Ok(Some(expr_json)) => {
                            results
                                .push(Some(build_expression_rendered(&expr_json, &lang_address)));
                            continue;
                        }
                        Ok(None) => {}
                        Err(e) => {
                            log::warn!("get_expression failed for {}: {}", url, e);
                        }
                    }
                }
            }
            results.push(None);
        }

        Ok(results)
    }

    async fn expression_raw(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<Option<String>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        if let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(&url) {
            let is_literal = lang_address == "literal";
            let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

            if is_loaded {
                match controller
                    .get_expression(&lang_address, &expression_address)
                    .await
                {
                    Ok(Some(expr_json)) => {
                        return Ok(Some(serde_json::to_string(&expr_json)?));
                    }
                    Ok(None) => {
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(FieldError::new(
                            format!("Failed to get expression {}: {}", url, e),
                            Value::null(),
                        ));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn get_trusted_agents(&self, context: &RequestContext) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            let agents = runtime_service.get_trusted_agents();
            Ok(agents)
        })
    }

    async fn language(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<LanguageHandle> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();

        // If not already loaded, try to install/load it (includes trust verification)
        if !controller.is_language_loaded(&address).await {
            controller.language_by_ref(&address).await.map_err(|e| {
                // Extract the inner message for LoadError to match expected API format
                let msg = match &e {
                    crate::languages::error::LanguageError::LoadError { message, .. } => {
                        message.clone()
                    }
                    other => other.to_string(),
                };
                FieldError::new(msg, Value::null())
            })?;
        }

        if controller.is_language_loaded(&address).await {
            let name = controller.get_language_name(&address).await;
            let settings = controller.get_settings_public(&address);
            let settings_str = if settings.is_null() {
                None
            } else {
                Some(serde_json::to_string(&settings).unwrap_or_default())
            };

            let (constructor_icon_json, icon_json, settings_icon_json) =
                controller.get_language_icons(&address).await;

            let constructor_icon =
                constructor_icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());
            let icon = icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());
            let settings_icon =
                settings_icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());

            return Ok(LanguageHandle {
                address,
                name,
                settings: settings_str,
                constructor_icon,
                icon,
                settings_icon,
            });
        }

        Err(FieldError::new(
            format!("Language not loaded: {}", address),
            Value::null(),
        ))
    }

    async fn language_meta(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<LanguageMeta> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        controller
            .get_language_expression(&address)
            .await
            .map_err(|e| {
                FieldError::new(
                    format!("Failed to get language meta for {}: {}", address, e),
                    Value::null(),
                )
            })
    }

    async fn language_source(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        controller.get_language_source(&address).await.map_err(|e| {
            FieldError::new(
                format!("Failed to get language source for {}: {}", address, e),
                Value::null(),
            )
        })
    }

    async fn languages(
        &self,
        context: &RequestContext,
        filter: Option<String>,
    ) -> FieldResult<Vec<LanguageHandle>> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let refs = controller.get_installed_languages(filter.as_deref()).await;

        let mut handles = Vec::new();
        for lang_ref in refs {
            let settings = controller.get_settings_public(&lang_ref.address);
            let settings_str = if settings.is_null() {
                None
            } else {
                Some(serde_json::to_string(&settings).unwrap_or_default())
            };

            handles.push(LanguageHandle {
                address: lang_ref.address,
                name: lang_ref.name,
                settings: settings_str,
                constructor_icon: None,
                icon: None,
                settings_icon: None,
            });
        }
        Ok(handles)
    }

    async fn neighbourhood_has_telepresence_adapter(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        Ok(get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .has_telepresence_adapter()
            .await)
    }

    async fn neighbourhood_online_agents(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] perspectiveUUID: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .online_agents()
            .await
            .map_err(|e| FieldError::from(e.to_string()))
    }

    async fn neighbourhood_other_agents(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)] perspectiveUUID: String,
    ) -> FieldResult<Vec<String>> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;

        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        let current_user_did = crate::agent::did_for_context(&agent_context)
            .map_err(|e| FieldError::from(e.to_string()))?;

        log::debug!("others() for current_user_did: {}", current_user_did);
        log::debug!("main agent did: {}", crate::agent::did());

        // Check if the current user is an owner of the perspective
        let perspective = get_perspective(&uuid).ok_or(FieldError::from(format!(
            "No perspective found with uuid {}",
            uuid
        )))?;

        let handle = perspective.persisted.lock().await.clone();

        // Check ownership - either the perspective has no owners (legacy/unowned)
        // or the current user is in the owners list
        if let Some(owners) = &handle.owners {
            if !owners.contains(&current_user_did) {
                return Err(FieldError::from(format!(
                    "Access denied: You are not an owner of this neighbourhood perspective"
                )));
            }
        }
        // If owners is None, allow access for backward compatibility with legacy perspectives

        // Get all DIDs from the link language
        let all_dids = perspective
            .others()
            .await
            .map_err(|e| FieldError::from(e.to_string()))?;

        log::debug!("all_dids: {:?}", all_dids);
        log::debug!("current_user_did: {}", current_user_did);
        let others: Vec<String> = all_dids
            .into_iter()
            .filter(|did| did != &current_user_did)
            .collect();

        log::debug!("others: {:?}", others);

        Ok(others)
    }

    async fn perspective(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<Option<PerspectiveHandle>> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        if let Some(p) = get_perspective(&uuid) {
            let handle = p.persisted.lock().await.clone();

            // Check if user has access to this perspective
            let user_email = user_email_from_token(context.auth_token.clone());

            if can_access_perspective(&user_email, &handle) {
                Ok(Some(handle))
            } else {
                Ok(None) // No access to this perspective
            }
        } else {
            Ok(None)
        }
    }

    async fn perspective_query_links(
        &self,
        context: &RequestContext,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<DecoratedLinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        Ok(get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .get_links(&query)
            .await?)
    }

    async fn perspective_query_prolog(
        &self,
        context: &RequestContext,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let agent_context = AgentContext::from_auth_token(context.auth_token.clone());
        Ok(prolog_resolution_to_string(
            get_perspective(&uuid)
                .ok_or(FieldError::from(format!(
                    "No perspective found with uuid {}",
                    uuid
                )))?
                .prolog_query_with_context(query, &agent_context)
                .await?,
        ))
    }

    /// Get all subject class names from SHACL links (Prolog-free implementation)
    async fn perspective_query_surreal_db(
        &self,
        context: &RequestContext,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let result = get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .surreal_query(query)
            .await?;

        Ok(serde_json::to_string(&result)?)
    }

    async fn perspective_snapshot(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<Perspective> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec![uuid.clone()]),
        )?;

        let all_links = get_perspective(&uuid)
            .ok_or(FieldError::from(format!(
                "No perspective found with uuid {}",
                uuid
            )))?
            .get_links(&LinkQuery::default())
            .await?;

        Ok(Perspective { links: all_links })
    }

    async fn perspectives(&self, context: &RequestContext) -> FieldResult<Vec<PerspectiveHandle>> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec!["*".into()]),
        )?;

        let mut result = Vec::new();

        // Extract user email from token for multi-user ownership filtering
        let user_email = user_email_from_token(context.auth_token.clone());

        // Only the launcher (authenticated via admin_credential) gets the full overview.
        // Regular app tokens (JWT) — even those granted ALL_CAPABILITY — are not considered admin
        // here and will only see perspectives they own or have joined.
        let is_admin = context.is_admin_credential;

        for p in all_perspectives().iter() {
            let mut handle = p.persisted.lock().await.clone();

            log::debug!("📋 perspectives(): perspective {} has owners: {:?}, is_admin: {}, user_email: {:?}",
                handle.uuid, handle.owners, is_admin, user_email);

            // Admin (launcher) sees all perspectives for the overview; others filter by ownership
            if is_admin {
                log::debug!(
                    "📋 perspectives(): is_admin: true, Including perspective {}",
                    handle.uuid
                );
                result.push(handle);
            } else if can_access_perspective(&user_email, &handle) {
                handle.owners = None;
                result.push(handle);
            } else {
                log::debug!(
                    "📋 perspectives(): Excluding perspective {} (no access)",
                    handle.uuid
                );
            }
        }

        Ok(result)
    }

    async fn runtime_friend_status(
        &self,
        context: &RequestContext,
        did: String,
    ) -> FieldResult<PerspectiveExpression> {
        check_capability(
            &context.capabilities,
            &RUNTIME_FRIEND_STATUS_READ_CAPABILITY,
        )?;

        let friends =
            RuntimeService::with_global_instance(|runtime_service| runtime_service.get_friends());

        if !friends.contains(&did.clone()) {
            log::error!("Friend not found: {}", did);

            return Ok(PerspectiveExpression::default());
        }

        // Direct message status requires DM language - return default for now
        log::warn!("runtime_friend_status: DM language interaction not yet ported to Rust");
        Ok(PerspectiveExpression::default())
    }

    async fn runtime_friends(&self, context: &RequestContext) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_READ_CAPABILITY)?;

        RuntimeService::with_global_instance(|runtime_service| {
            let friends = runtime_service.get_friends();
            Ok(friends)
        })
    }

    async fn runtime_hc_agent_infos(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY,
        )?;

        let interface = get_holochain_service().await;
        let infos = interface.agent_infos().await?;

        Ok(serde_json::to_string(&infos)?)
    }

    async fn runtime_get_network_metrics(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY,
        )?;

        let interface = get_holochain_service().await;
        let metrics = interface.get_network_metrics().await?;

        Ok(metrics)
    }

    async fn runtime_info(&self, _context: &RequestContext) -> FieldResult<RuntimeInfo> {
        AgentService::with_global_instance(|agent_service| {
            agent_service
                .agent
                .clone()
                .ok_or(FieldError::new("Agent not found", Value::null()))?;

            Ok(RuntimeInfo {
                is_initialized: agent_service.is_initialized(),
                is_unlocked: agent_service.is_unlocked(),
                ad4m_executor_version: AD4M_VERSION.clone(),
            })
        })
    }

    async fn runtime_known_link_language_templates(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY,
        )?;

        RuntimeService::with_global_instance(|runtime_service| {
            let languages = runtime_service.get_know_link_languages();
            Ok(languages)
        })
    }

    async fn runtime_message_inbox(
        &self,
        context: &RequestContext,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)?;
        let _ = filter;
        // Direct message inbox requires DM language - return empty for now
        log::warn!("runtime_message_inbox: DM language interaction not yet ported to Rust");
        Ok(vec![])
    }

    async fn runtime_message_outbox(
        &self,
        context: &RequestContext,
        _filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)?;

        RuntimeService::with_global_instance(|runtime_service| {
            let outbox = runtime_service.get_outbox();
            Ok(outbox)
        })
    }

    async fn runtime_verify_string_signed_by_did(
        &self,
        context: &RequestContext,
        data: String,
        did: String,
        _did_signing_key_id: String,
        signed_data: String,
    ) -> FieldResult<bool> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        signatures::verify_string_signed_by_did(&did, &data, &signed_data)
            .map_err(|e| e.to_string())
            .map_err(|e| coasys_juniper::FieldError::new(e, coasys_juniper::Value::Null))
    }

    async fn runtime_notifications(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<Notification>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        // Extract user context from auth token to filter notifications per user
        let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
        let user_email = agent_context.user_email;
        let notifications_result =
            Ad4mDb::with_global_instance(|db| db.get_notifications_for_user(user_email));
        if let Err(e) = notifications_result {
            return Err(FieldError::new(e.to_string(), Value::null()));
        }
        Ok(notifications_result.unwrap())
    }

    async fn runtime_multi_user_enabled(&self, context: &RequestContext) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY,
        )?;
        Ad4mDb::with_global_instance(|db| {
            db.get_multi_user_enabled()
                .map_err(|e| FieldError::new(e.to_string(), Value::null()))
        })
    }

    async fn runtime_list_users(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<UserStatistics>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
        )?;

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return Ok(vec![]);
        }

        // Get all users from database
        let users = Ad4mDb::with_global_instance(|db| db.list_users())
            .map_err(|e| FieldError::new(format!("Failed to list users: {}", e), Value::null()))?;

        // For each user, count their perspectives
        let mut user_stats = vec![];
        let all_perspectives = all_perspectives();

        for user in users {
            // Count perspectives owned by this user
            let mut perspective_count = 0;
            for perspective in &all_perspectives {
                let handle = perspective.persisted.lock().await.clone();
                if let Some(owners) = &handle.owners {
                    if owners.contains(&user.did) {
                        perspective_count += 1;
                    }
                }
            }

            user_stats.push(UserStatistics {
                email: user.username,
                did: user.did,
                last_seen: user.last_seen.map(|ts| {
                    DateTime::from(
                        chrono::DateTime::from_timestamp(ts as i64, 0)
                            .unwrap_or_else(chrono::Utc::now),
                    )
                }),
                perspective_count,
            });
        }

        Ok(user_stats)
    }

    async fn runtime_hosting_user_info(
        &self,
        context: &RequestContext,
    ) -> FieldResult<HostingUserInfo> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;

        let user_email = user_email_from_token(context.auth_token.clone()).ok_or_else(|| {
            FieldError::new(
                "Hosting user info requires multi-user authentication",
                Value::null(),
            )
        })?;

        let free_access = Ad4mDb::with_global_instance(|db| db.get_user_free_access(&user_email))
            .map_err(|e| FieldError::new(format!("Failed to get free access status: {}", e), Value::null()))?;

        let remaining_credits = if free_access {
            "unlimited".to_string()
        } else {
            let credits =
                Ad4mDb::with_global_instance(|db| db.get_user_credits(&user_email))
                    .map_err(|e| FieldError::new(format!("Failed to get user credits: {}", e), Value::null()))?;
            credits.to_string()
        };

        let hot_wallet_address =
            Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user_email))
                .map_err(|e| FieldError::new(format!("Failed to get hot wallet address: {}", e), Value::null()))?;

        Ok(HostingUserInfo {
            email: user_email,
            remaining_credits,
            hot_wallet_address,
            free_access,
        })
    }

    async fn ai_get_models(&self, context: &RequestContext) -> FieldResult<Vec<Model>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let models_result = Ad4mDb::with_global_instance(|db| db.get_models());
        match models_result {
            Ok(models) => Ok(models),
            Err(e) => Err(FieldError::new(e.to_string(), Value::null())),
        }
    }

    async fn ai_get_default_model(
        &self,
        context: &RequestContext,
        model_type: ModelType,
    ) -> FieldResult<Option<Model>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;

        let default_id = Ad4mDb::with_global_instance(|db| db.get_default_model(model_type))
            .map_err(|e| FieldError::new(e.to_string(), Value::null()))?;

        Ok(if let Some(id) = default_id {
            Ad4mDb::with_global_instance(|db| db.get_model(id))
                .map_err(|e| FieldError::new(e.to_string(), Value::null()))?
        } else {
            None
        })
    }

    async fn ai_tasks(&self, context: &RequestContext) -> FieldResult<Vec<AITask>> {
        check_capability(&context.capabilities, &AI_READ_CAPABILITY)?;

        match AIService::get_tasks() {
            Ok(tasks) => Ok(tasks),
            Err(e) => Err(FieldError::new(e.to_string(), Value::null())),
        }
    }

    async fn ai_model_loading_status(
        &self,
        context: &RequestContext,
        model: String,
    ) -> FieldResult<AIModelLoadingStatus> {
        check_capability(&context.capabilities, &AI_READ_CAPABILITY)?;

        match AIService::model_status(model).await {
            Ok(status) => Ok(status),
            Err(e) => Err(FieldError::new(e.to_string(), Value::null())),
        }
    }
}

/// Build an ExpressionRendered from a raw JsonValue expression and language address.
pub fn build_expression_rendered(
    expr_json: &serde_json::Value,
    lang_address: &str,
) -> ExpressionRendered {
    let author = expr_json
        .get("author")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let timestamp = expr_json
        .get("timestamp")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let data = match expr_json.get("data") {
        Some(d) => serde_json::to_string(d).unwrap_or_default(),
        None => String::new(),
    };

    let proof = if let Some(p) = expr_json.get("proof") {
        DecoratedExpressionProof {
            key: p
                .get("key")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            signature: p
                .get("signature")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            valid: p.get("valid").and_then(|v| v.as_bool()),
            invalid: p.get("invalid").and_then(|v| v.as_bool()),
        }
    } else {
        DecoratedExpressionProof::default()
    };

    ExpressionRendered {
        author,
        timestamp,
        data,
        proof,
        language: LanguageRef {
            address: lang_address.to_string(),
            name: String::new(),
        },
        icon: Icon { code: None },
    }
}
