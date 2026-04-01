#![allow(non_snake_case)]
use super::graphql_types::*;
use crate::agent::{capabilities::*, did_document_for_context, signatures, AgentContext};
use crate::ai_service::AIService;
use crate::config::get_global_config;
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

    /// Returns the domain name(s) from the TLS certificate's Subject Alternative Names,
    /// or None if TLS is not configured.
    async fn runtime_tls_domain(&self, context: &RequestContext) -> FieldResult<Option<String>> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        let config = get_global_config();
        let tls = match config.tls {
            Some(tls) => tls,
            None => return Ok(None),
        };

        let cert_pem = std::fs::read(&tls.cert_file_path).map_err(|e| {
            FieldError::new(
                format!("Failed to read TLS certificate: {}", e),
                Value::null(),
            )
        })?;

        use x509_parser::prelude::*;
        // Parse PEM to get the first certificate
        let (_, pem) = parse_x509_pem(&cert_pem)
            .map_err(|e| FieldError::new(format!("Failed to parse PEM: {}", e), Value::null()))?;
        let (_, cert) = X509Certificate::from_der(&pem.contents).map_err(|e| {
            FieldError::new(format!("Failed to parse certificate: {}", e), Value::null())
        })?;

        // Try SAN extension first, skipping wildcard entries
        if let Ok(Some(san)) = cert.subject_alternative_name() {
            for name in &san.value.general_names {
                if let GeneralName::DNSName(dns) = name {
                    if !dns.contains('*') {
                        return Ok(Some(dns.to_string()));
                    }
                }
            }
        }

        // Fall back to CN, skipping wildcard entries
        for rdn in cert.subject().iter() {
            for attr in rdn.iter() {
                if attr.attr_type() == &oid_registry::OID_X509_COMMON_NAME {
                    if let Ok(cn) = attr.as_str() {
                        if !cn.contains('*') {
                            return Ok(Some(cn.to_string()));
                        }
                    }
                }
            }
        }

        Ok(None)
    }

    /// Returns the readiness status of executor subsystems.
    /// Test harnesses should poll this query instead of using `sleep()`.
    /// No capability check — readiness is safe to expose publicly.
    async fn runtime_readiness(&self, _context: &RequestContext) -> FieldResult<ReadinessStatus> {
        // TODO: holochain_ready only checks if the service handle exists, not actual conductor readiness.
        // A proper fix would require an API to query conductor state, which doesn't exist yet.
        let holochain_ready = crate::holochain_service::maybe_get_holochain_service()
            .await
            .is_some();

        // TODO: languages_loaded currently maps to wallet unlock state, not language-controller state.
        // The language loading happens during unlock, but there's no separate API to check if all
        // languages have finished loading. This is a reasonable approximation for now.
        let (agent_initialized, languages_loaded) =
            AgentService::with_global_instance(|agent_service| {
                (agent_service.is_initialized(), agent_service.is_unlocked())
            });

        Ok(ReadinessStatus {
            gql_ready: true, // If this query returns, GQL is ready
            holochain_ready,
            agent_initialized,
            languages_loaded,
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

    async fn runtime_free_hosting_enabled(&self, context: &RequestContext) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY,
        )?;
        Ad4mDb::with_global_instance(|db| {
            db.get_free_hosting_enabled()
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
        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(false);

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

            let free_access: bool = global_free
                || Ad4mDb::with_global_instance(|db| db.get_user_free_access(&user.username))
                    .map_err(|e| {
                        FieldError::new(
                            format!("Failed to get user free access: {}", e),
                            Value::null(),
                        )
                    })?;

            let remaining_credits = if free_access {
                "unlimited".to_string()
            } else {
                let credits =
                    Ad4mDb::with_global_instance(|db| db.get_user_credits(&user.username))
                        .map_err(|e| {
                            FieldError::new(
                                format!("Failed to get user credits: {}", e),
                                Value::null(),
                            )
                        })?;
                format!("{}", credits)
            };

            let hot_wallet_address =
                Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user.username)).map_err(
                    |e| {
                        FieldError::new(
                            format!("Failed to get hot wallet for user {}: {}", user.username, e),
                            Value::null(),
                        )
                    },
                )?;

            user_stats.push(UserStatistics {
                email: user.username.clone(),
                did: user.did,
                last_seen: user.last_seen.map(|ts| {
                    DateTime::from(
                        chrono::DateTime::from_timestamp(ts as i64, 0)
                            .unwrap_or_else(chrono::Utc::now),
                    )
                }),
                perspective_count,
                remaining_credits,
                free_access,
                hot_wallet_address,
            });
        }

        Ok(user_stats)
    }

    async fn runtime_user_wallet_address(
        &self,
        context: &RequestContext,
        email: String,
    ) -> FieldResult<Option<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
        )?;
        let email = email.trim().to_lowercase();
        let addr =
            Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&email)).map_err(|e| {
                FieldError::new(
                    format!("Failed to get wallet address: {}", e),
                    Value::null(),
                )
            })?;
        Ok(addr)
    }

    async fn runtime_hosting_user_info(
        &self,
        context: &RequestContext,
    ) -> FieldResult<HostingUserInfo> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        let user_email = user_email_from_token(context.auth_token.clone()).ok_or_else(|| {
            FieldError::new(
                "Hosting user info requires multi-user authentication",
                Value::null(),
            )
        })?;

        let global_free =
            Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(false);
        let free_access = global_free
            || Ad4mDb::with_global_instance(|db| db.get_user_free_access(&user_email)).map_err(
                |e| {
                    FieldError::new(
                        format!("Failed to get free access status: {}", e),
                        Value::null(),
                    )
                },
            )?;

        let remaining_credits = if free_access {
            "unlimited".to_string()
        } else {
            let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(&user_email))
                .map_err(|e| {
                    FieldError::new(format!("Failed to get user credits: {}", e), Value::null())
                })?;
            credits.to_string()
        };

        let hot_wallet_address =
            Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user_email)).map_err(
                |e| {
                    FieldError::new(
                        format!("Failed to get hot wallet address: {}", e),
                        Value::null(),
                    )
                },
            )?;

        Ok(HostingUserInfo {
            email: user_email,
            remaining_credits,
            hot_wallet_address,
            free_access,
        })
    }

    /// Get the host's configured rates for credit deduction.
    async fn runtime_host_rates(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        let rates = Ad4mDb::with_global_instance(|db| {
            db.get_host_rates().map_err(|e| {
                FieldError::new(format!("Failed to get host rates: {}", e), Value::null())
            })
        })?;

        let json: Vec<serde_json::Value> = rates
            .into_iter()
            .map(|(desc, price)| serde_json::json!({ "description": desc, "priceInHOT": price }))
            .collect();

        Ok(serde_json::to_string(&json).unwrap_or_else(|_| "[]".to_string()))
    }

    /// Get the host's wHOT wallet balance from the alliance DNA ledger.
    async fn runtime_hot_wallet_balance(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        match crate::unyt_service::get_ledger().await {
            Ok(ledger) => {
                // Extract balance from ledger JSON
                let balance = ledger
                    .get("balance")
                    .cloned()
                    .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));
                Ok(serde_json::to_string(&balance).unwrap_or_else(|_| "{}".to_string()))
            }
            Err(e) => Err(FieldError::new(
                format!("Failed to get wHOT wallet balance: {}", e),
                Value::null(),
            )),
        }
    }

    /// Get the host's wHOT transaction history (outgoing + incoming).
    async fn runtime_hot_wallet_history(
        &self,
        context: &RequestContext,
        page: Option<i32>,
        per_page: Option<i32>,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        // Validate pagination parameters
        if let Some(p) = page {
            if p < 0 {
                return Err(FieldError::new("page must be non-negative", Value::Null));
            }
        }
        const MAX_PER_PAGE: i32 = 1000;
        if let Some(pp) = per_page {
            if pp < 1 || pp > MAX_PER_PAGE {
                return Err(FieldError::new(
                    format!("per_page must be between 1 and {}", MAX_PER_PAGE),
                    Value::Null,
                ));
            }
        }

        // Fetch outgoing history
        let mut all_txs: Vec<serde_json::Value> = Vec::new();

        match crate::unyt_service::get_history(
            page.map(|p| p as u64),
            per_page.unwrap_or(50) as u64,
        )
        .await
        {
            Ok(history) => {
                log::debug!("get_history raw result: {}", history);
                // get_history returns {"items": [...], "low_boundary": ..., "end_of_chain": ...}
                let items = history
                    .get("items")
                    .and_then(|v| v.as_array())
                    .or_else(|| history.as_array());
                if let Some(arr) = items {
                    log::info!("get_history parsed {} items", arr.len());
                    for tx in arr {
                        all_txs.push(tx.clone());
                    }
                } else {
                    log::warn!("get_history: could not extract items array from response");
                }
            }
            Err(e) => {
                log::warn!("Failed to get outgoing history: {}", e);
            }
        }

        // Fetch incoming transactions via notification links
        match crate::unyt_service::get_all_notification_links().await {
            Ok(links) => {
                log::debug!("get_all_notification_links raw result: {}", links);
                match crate::unyt_service::get_actionable_transactions(links).await {
                    Ok(incoming) => {
                        log::debug!("get_actionable_transactions raw result: {}", incoming);
                        // Result is {"proposal_actionable":[], "commitment_actionable":[], "accept_actionable":[], "reject_actionable":[]}
                        if let Some(obj) = incoming.as_object() {
                            for (category, txs) in obj {
                                if let Some(arr) = txs.as_array() {
                                    for tx in arr {
                                        let mut tx = tx.clone();
                                        if let Some(tx_obj) = tx.as_object_mut() {
                                            let direction = if category == "reject_actionable" {
                                                "rejected"
                                            } else {
                                                "incoming"
                                            };
                                            tx_obj.insert(
                                                "direction".to_string(),
                                                serde_json::Value::String(direction.to_string()),
                                            );
                                        }
                                        all_txs.push(tx);
                                    }
                                }
                            }
                        } else if let Some(arr) = incoming.as_array() {
                            for tx in arr {
                                let mut tx = tx.clone();
                                if let Some(obj) = tx.as_object_mut() {
                                    obj.insert(
                                        "direction".to_string(),
                                        serde_json::Value::String("incoming".to_string()),
                                    );
                                }
                                all_txs.push(tx);
                            }
                        }
                    }
                    Err(e) => {
                        log::warn!("Failed to get actionable transactions: {}", e);
                    }
                }
            }
            Err(e) => {
                log::warn!("Failed to get notification links: {}", e);
            }
        }

        // Add pending sends from DB so they show immediately in the UI
        if let Ok(pending) = Ad4mDb::with_global_instance(|db| db.get_pending_sends()) {
            log::info!("DB pending sends: {} items", pending.len());
            for (recipient, amount, proposal_hash) in &pending {
                // Check if this proposal is already in all_txs (from zome history)
                let already_present = all_txs.iter().any(|tx| {
                    tx.get("id").and_then(|v| v.as_str()) == Some(proposal_hash.as_str())
                        || tx
                            .get("history")
                            .and_then(|h| h.as_array())
                            .map_or(false, |arr| {
                                arr.iter().any(|h| {
                                    h.get("id").and_then(|v| v.as_str())
                                        == Some(proposal_hash.as_str())
                                })
                            })
                });
                if !already_present {
                    let mut tx = serde_json::json!({
                        "id": proposal_hash,
                        "tx_type": "Proposal",
                        "amount": { "0": format!("-{}", amount.trim_start_matches('-')) },
                        "counterparty": [recipient],
                        "status": "pending",
                        "direction": "outgoing",
                    });
                    // Enrich with email
                    if let Ok(Some(email)) = Ad4mDb::with_global_instance(|db| {
                        db.get_user_by_hot_wallet_address(recipient)
                    }) {
                        tx.as_object_mut().unwrap().insert(
                            "counterparty_email".to_string(),
                            serde_json::Value::String(email),
                        );
                    }
                    all_txs.push(tx);
                }
            }
        }

        // NOTE: Rejection reconciliation (reject_pending_send, complete_payment_request)
        // is handled by check_pending_sends() and check_pending_payments() in unyt_service.rs,
        // which run periodically. This query resolver is intentionally read-only.

        // Enrich counterparty agent pubkeys with user emails from DB
        for tx in &mut all_txs {
            if let Some(counterparty_arr) = tx
                .get("counterparty")
                .and_then(|c| c.as_array())
                .map(|a| a.to_vec())
            {
                if let Some(pubkey) = counterparty_arr.first().and_then(|v| v.as_str()) {
                    if let Ok(Some(email)) =
                        Ad4mDb::with_global_instance(|db| db.get_user_by_hot_wallet_address(pubkey))
                    {
                        if let Some(obj) = tx.as_object_mut() {
                            obj.insert(
                                "counterparty_email".to_string(),
                                serde_json::Value::String(email),
                            );
                        }
                    }
                }
            }
        }

        log::info!(
            "Returning {} total transaction history items",
            all_txs.len()
        );
        Ok(serde_json::to_string(&all_txs).unwrap_or_else(|_| "[]".to_string()))
    }

    /// Get the host's wHOT agent public key (their identity on the wHOT DHT).
    async fn runtime_hot_agent_pubkey(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        match crate::unyt_service::whoami().await {
            Ok(pubkey) => Ok(pubkey),
            Err(e) => Err(FieldError::new(
                format!("Failed to get wHOT agent pubkey: {}", e),
                Value::null(),
            )),
        }
    }

    /// Get or create the Holochain agent public key for the Unyt DNA (base64).
    /// Available even before Unyt DNA is installed — needed to request membrane proof.
    async fn runtime_unyt_agent_key(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        match crate::unyt_service::get_or_create_agent_key().await {
            Ok(key) => Ok(key),
            Err(e) => Err(FieldError::new(
                format!("Failed to get/create Unyt agent key: {}", e),
                Value::null(),
            )),
        }
    }

    /// Get Unyt DNA version info (installed vs bundled).
    async fn runtime_unyt_version_info(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;
        let (installed, bundled) = crate::unyt_service::version_info();
        Ok(serde_json::json!({
            "installed": installed,
            "bundled": bundled,
            "needsUpdate": installed.as_deref() != Some(bundled.as_str()),
        })
        .to_string())
    }

    /// Get compute activity log entries.
    /// Regular users see only their own entries; admin sees all users.
    async fn runtime_compute_log(
        &self,
        context: &RequestContext,
        since: Option<String>,
        limit: Option<i32>,
        user_email: Option<String>,
    ) -> FieldResult<Vec<ComputeLogEntry>> {
        check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)?;

        let raw_limit = limit.unwrap_or(100);
        if raw_limit < 0 {
            return Err(FieldError::new("limit must be non-negative", Value::Null));
        }
        let max = (raw_limit as i64).min(1000);

        // If admin and user_email is provided, query that user's log
        if context.is_admin_credential {
            if let Some(ref email) = user_email {
                let entries = Ad4mDb::with_global_instance(|db| {
                    db.get_compute_log(email, since.as_deref(), max)
                })
                .map_err(|e| {
                    FieldError::new(format!("Failed to get compute log: {}", e), Value::null())
                })?;
                return Ok(entries
                    .into_iter()
                    .map(|e| ComputeLogEntry {
                        id: e.id as i32,
                        user_email: e.user_email,
                        timestamp: e.timestamp,
                        operation: e.operation,
                        summary: e.summary,
                        cost: e.cost,
                        credits_after: e.credits_after,
                    })
                    .collect());
            }
            // Admin with no user_email — return all users
            let entries =
                Ad4mDb::with_global_instance(|db| db.get_compute_log_all(since.as_deref(), max))
                    .map_err(|e| {
                        FieldError::new(format!("Failed to get compute log: {}", e), Value::null())
                    })?;
            return Ok(entries
                .into_iter()
                .map(|e| ComputeLogEntry {
                    id: e.id as i32,
                    user_email: e.user_email,
                    timestamp: e.timestamp,
                    operation: e.operation,
                    summary: e.summary,
                    cost: e.cost,
                    credits_after: e.credits_after,
                })
                .collect());
        }

        // Regular user — return only their own entries
        let email = user_email_from_token(context.auth_token.clone());
        match email {
            Some(email) => {
                let entries = Ad4mDb::with_global_instance(|db| {
                    db.get_compute_log(&email, since.as_deref(), max)
                })
                .map_err(|e| {
                    FieldError::new(format!("Failed to get compute log: {}", e), Value::null())
                })?;
                Ok(entries
                    .into_iter()
                    .map(|e| ComputeLogEntry {
                        id: e.id as i32,
                        user_email: e.user_email,
                        timestamp: e.timestamp,
                        operation: e.operation,
                        summary: e.summary,
                        cost: e.cost,
                        credits_after: e.credits_after,
                    })
                    .collect())
            }
            None => Ok(vec![]),
        }
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
