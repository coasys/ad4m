#![allow(non_snake_case)]
use super::graphql_types::*;
use crate::agent::{capabilities::*, did_document_for_context, signatures, AgentContext};
use crate::ai_service::AIService;
use crate::languages::LanguageController;
use crate::types::{AITask, DecoratedExpressionProof, ModelType};
use crate::{agent::AgentService, entanglement_service::get_entanglement_proofs};
use crate::{
    db::Ad4mDb,
    holochain_service::get_holochain_service,
    perspectives::{all_perspectives, get_perspective, utils::prolog_resolution_to_string},
    runtime_service::RuntimeService,
    types::{DecoratedLinkExpression, Model, Notification},
};
use coasys_juniper::{graphql_object, FieldError, FieldResult, Value};
use std::env;

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
            let mut js = context.js_handle.clone();
            let result = js
                .execute(format!(
                    r#"JSON.stringify(
                        await core.callResolver("Query", "agentByDID",
                            {{ did: "{}" }},
                        )
                    )"#,
                    did,
                ))
                .await?;
            let result: JsResultType<Option<Agent>> = serde_json::from_str(&result)?;
            result.get_graphql_result()
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
                        // Fall through to JS — the JS runtime may have different state
                        log::debug!(
                            "Rust-side get_expression returned None for {}, trying JS fallback",
                            url
                        );
                    }
                    Err(e) => {
                        log::warn!(
                            "Rust-side get_expression failed for {}: {}, falling back to JS",
                            url,
                            e
                        );
                    }
                }
            }
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expression", {{ url: "{}" }}))"#,
                url
            ))
            .await?;
        let result: JsResultType<Option<ExpressionRendered>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
                match controller.expression_interactions(&url).await {
                    Ok(interactions) => return Ok(interactions),
                    Err(e) => {
                        log::warn!("Rust-side expression_interactions failed for {}: {}, falling back to JS", url, e);
                    }
                }
            }
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionInteractions", {{ url: "{}" }}))"#,
                url,
            ))
            .await?;
        let result: JsResultType<Vec<InteractionMeta>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_many(
        &self,
        context: &RequestContext,
        urls: Vec<String>,
    ) -> FieldResult<Vec<Option<ExpressionRendered>>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let mut results = Vec::new();
        let mut js_fallback_urls = Vec::new();
        let mut js_fallback_indices = Vec::new();

        for (i, url) in urls.iter().enumerate() {
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
                        Ok(None) => {
                            // Fall through to JS fallback — the JS runtime may have different state
                            log::debug!(
                                "Rust-side get_expression returned None for {}, trying JS fallback",
                                url
                            );
                        }
                        Err(e) => {
                            log::warn!(
                                "Rust-side get_expression failed for {}: {}, falling back to JS",
                                url,
                                e
                            );
                        }
                    }
                }
            }
            // Need JS fallback for this URL
            results.push(None); // placeholder
            js_fallback_urls.push(url.clone());
            js_fallback_indices.push(i);
        }

        // Fall back to JS for URLs that couldn't be handled
        if !js_fallback_urls.is_empty() {
            let urls_string = js_fallback_urls
                .iter()
                .map(|url| format!("\"{}\"", url))
                .collect::<Vec<String>>()
                .join(",");
            let mut js = context.js_handle.clone();
            let js_result = js
                .execute(format!(
                    r#"JSON.stringify(await core.callResolver("Query", "expressionMany", {{ urls: [{}] }}))"#,
                    urls_string,
                ))
                .await?;
            let js_result: JsResultType<Vec<Option<ExpressionRendered>>> =
                serde_json::from_str(&js_result)?;
            if let Ok(js_expressions) = js_result.get_graphql_result() {
                for (j, idx) in js_fallback_indices.iter().enumerate() {
                    if let Some(expr) = js_expressions.get(j) {
                        results[*idx] = expr.clone();
                    }
                }
            }
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
                        log::debug!(
                            "Rust-side expression_raw returned None for {}, trying JS fallback",
                            url
                        );
                    }
                    Err(e) => {
                        log::warn!(
                            "Rust-side expression_raw failed for {}: {}, falling back to JS",
                            url,
                            e
                        );
                    }
                }
            }
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionRaw", {{ url: "{}" }}))"#,
                url,
            ))
            .await?;
        let result: JsResultType<Option<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
        if controller.is_language_loaded(&address).await {
            let name = controller.get_language_name(&address).await;
            let settings = controller.get_settings_public(&address);
            let settings_str = if settings.is_null() {
                None
            } else {
                Some(serde_json::to_string(&settings).unwrap_or_default())
            };

            return Ok(LanguageHandle {
                address,
                name,
                settings: settings_str,
                constructor_icon: None,
                icon: None,
                settings_icon: None,
            });
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "language", {{ address: "{}" }}))"#,
                address,
            ))
            .await?;
        let result: JsResultType<LanguageHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_meta(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<LanguageMeta> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let ll_loaded = {
            let sys = controller.system_addresses.lock().await;
            if let Some(ll_addr) = &sys.language_language {
                controller.is_language_loaded(ll_addr).await
            } else {
                false
            }
        };

        if ll_loaded {
            match controller.get_language_expression(&address).await {
                Ok(meta) => return Ok(meta),
                Err(e) => {
                    log::warn!(
                        "Rust-side language_meta failed for {}: {}, falling back to JS",
                        address,
                        e
                    );
                }
            }
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageMeta", {{ address: "{}" }}))"#,
                address,
            ))
            .await?;
        let result: JsResultType<LanguageMeta> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_source(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let ll_loaded = {
            let sys = controller.system_addresses.lock().await;
            if let Some(ll_addr) = &sys.language_language {
                controller.is_language_loaded(ll_addr).await
            } else {
                false
            }
        };

        if ll_loaded {
            match controller.get_language_source(&address).await {
                Ok(source) => return Ok(source),
                Err(e) => {
                    log::warn!(
                        "Rust-side language_source failed for {}: {}, falling back to JS",
                        address,
                        e
                    );
                }
            }
        }

        // Fall back to JS
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageSource", {{ address: "{}" }}))"#,
                address,
            ))
            .await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn languages(
        &self,
        context: &RequestContext,
        filter: Option<String>,
    ) -> FieldResult<Vec<LanguageHandle>> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;

        let controller = LanguageController::global_instance();
        let refs = controller.get_installed_languages(filter.as_deref()).await;

        if !refs.is_empty() {
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
            return Ok(handles);
        }

        // Fall back to JS if no languages are loaded in Rust runtimes
        let filter_string = filter.map_or("null".to_string(), |f| f.to_string());
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languages", {{ filter: "{}" }}))"#,
                filter_string,
            ))
            .await?;
        let result: JsResultType<Vec<LanguageHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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

        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.friendsDirectMessageLanguage("{}") ? await (await core.friendsDirectMessageLanguage("{}")).directMessageAdapter.status()  : null)"#,
                did,
                did
            ))
            .await?;
        let result: PerspectiveExpression = serde_json::from_str(&result)?;
        Ok(result)
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
                ad4m_executor_version: env!("CARGO_PKG_VERSION").to_string(),
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
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await (await core.myDirectMessageLanguage()).directMessageAdapter.inbox("{}"))"#,
            filter_str,
        );
        let mut js = context.js_handle.clone();
        let result = js.execute(script).await?;
        let result: Vec<PerspectiveExpression> = serde_json::from_str(&result)?;
        println!("llllll inbox result: {:?}", result);
        Ok(result)
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
fn build_expression_rendered(
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
        Some(d) if d.is_string() => d.as_str().unwrap_or("").to_string(),
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
