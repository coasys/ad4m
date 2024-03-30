#![allow(non_snake_case)]
use coasys_juniper::{graphql_object, FieldError, FieldResult, Value};
use crate::{holochain_service::get_holochain_service, perspectives::{all_perspectives, get_perspective}, types::{DecoratedLinkExpression }};
use crate::{agent::AgentService, entanglement_service::get_entanglement_proofs};
use std::{env, path};
use super::graphql_types::*;
use base64::{encode};
use crate::{agent::{capabilities::*, signatures}, holochain_service, runtime_service, wallet::{self, Wallet}};

pub struct Query;

#[graphql_object(context = RequestContext)]
impl Query {
    async fn agent(&self, context: &RequestContext) -> FieldResult<Agent> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let agent_instance = AgentService::instance();
        let agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
        let mut agent = agent_ref.agent.clone().ok_or(FieldError::new(
            "Agent not found",
            Value::null(),
        ))?;

        if agent.perspective.is_some() {
            agent.perspective.as_mut().unwrap().verify_link_signatures();
        }

        Ok(agent)
    }

    #[graphql(name = "agentByDID")]
    async fn agent_by_did(
        &self,
        context: &RequestContext,
        did: String,
    ) -> FieldResult<Option<Agent>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let agent_instance = AgentService::instance();
        let did_match = {
            let agent_service = agent_instance.lock().expect("agent lock");
            let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
            did == agent_ref.did.clone().unwrap()
        };

        if !did_match {
            let mut js = context.js_handle.clone();
            let result = js
                .execute(
                    format!(
                        r#"JSON.stringify(
                        await core.callResolver("Query", "agentByDID",
                            {{ did: "{}" }},
                        )
                    )"#,
                        did,
                    )
                    .into(),
                )
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
        context: &RequestContext,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let proofs = get_entanglement_proofs();
        Ok(proofs)
    }

    async fn agent_is_locked(&self, context: &RequestContext) -> FieldResult<bool> {
        let agent_instance = AgentService::instance();
        let agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");
        let is_unlocked = agent_ref.is_unlocked();

        Ok(is_unlocked)
    }

    async fn agent_status(&self, context: &RequestContext) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let agent_instance = AgentService::instance();
        let agent_service = agent_instance.lock().expect("agent lock");
        let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

        let status = agent_ref.dump();

        Ok(status)
    }

    async fn expression(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<Option<ExpressionRendered>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;
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
        let urls_string = urls
            .into_iter()
            .map(|url| format!("\"{}\"", url))
            .collect::<Vec<String>>()
            .join(",");
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionMany", {{ urls: [{}] }}))"#,
                urls_string,
            ))
            .await?;
        let result: JsResultType<Vec<Option<ExpressionRendered>>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_raw(
        &self,
        context: &RequestContext,
        url: String,
    ) -> FieldResult<Option<String>> {
        check_capability(&context.capabilities, &EXPRESSION_READ_CAPABILITY)?;
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
        let agents = runtime_service::get_trusted_agents();

        Ok(agents)
    }

    async fn language(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<LanguageHandle> {
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;
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
        let filter_string = filter.map_or("null".to_string(), |f| format!("{}", f));
        check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)?;
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
        #[allow(non_snake_case)]
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        Ok(get_perspective(&uuid)
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
            .has_telepresence_adapter()
            .await)
    }

    async fn neighbourhood_online_agents(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)]
        perspectiveUUID: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
            .online_agents()
            .await
            .map_err(|e| FieldError::from(e.to_string()))
    }

    async fn neighbourhood_other_agents(
        &self,
        context: &RequestContext,
        #[allow(non_snake_case)]
        perspectiveUUID: String,
    ) -> FieldResult<Vec<String>> {
        let uuid = perspectiveUUID;
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        get_perspective(&uuid)
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
            .others()
            .await
            .map_err(|e| FieldError::from(e.to_string()))
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
            Ok(Some(p.persisted.lock().await.clone()))
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
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
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

        Ok(get_perspective(&uuid)
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
            .prolog_query(query)
            .await?)
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
            .ok_or(FieldError::from(format!("No perspective found with uuid {}", uuid)))?
            .get_links(&LinkQuery::default())
            .await?;

        Ok(Perspective {
            links: all_links,
        })
    }

    async fn perspectives(&self, context: &RequestContext) -> FieldResult<Vec<PerspectiveHandle>> {
        check_capability(
            &context.capabilities,
            &perspective_query_capability(vec!["*".into()]),
        )?;

        let mut result = Vec::new();
        for p in all_perspectives().iter() {
            let handle = p.persisted.lock().await.clone();
            result.push(handle);
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
        let friends = runtime_service::get_friends();

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

        let friends = runtime_service::get_friends();
        Ok(friends)
    }

    async fn runtime_hc_agent_infos(&self, context: &RequestContext) -> FieldResult<String> {
        check_capability(
            &context.capabilities,
            &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY,
        )?;

        let interface = get_holochain_service().await;
        let infos = interface.agent_infos().await?;

        let encoded_infos: Vec<String> = infos
            .iter()
            .map(|info| base64::encode(info.encode().expect("Failed to encode AgentInfoSigned")))
            .collect();

        Ok(serde_json::to_string(&encoded_infos)?)
    }

    async fn runtime_info(&self, context: &RequestContext) -> FieldResult<RuntimeInfo> {
        let mut js = context.js_handle.clone();

        let agent_path = format!("{}/ad4m/agent.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| "".to_string()));

        let wallet_instance = Wallet::instance();
        let wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_ref().expect("wallet instance");
        let is_unlocked = wallet_ref.is_unlocked();
        let is_initialized: bool = path::Path::new(&agent_path).exists();

        Ok(RuntimeInfo {
            is_initialized,
            is_unlocked,
            ad4m_executor_version: env!("CARGO_PKG_VERSION").to_string(),
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
        let runtime = runtime_service::get_know_link_languages();
        Ok(runtime)
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
        filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)?;
        let outbox = runtime_service::get_outbox();
        Ok(outbox)
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
}
