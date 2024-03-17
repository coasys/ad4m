#![allow(non_snake_case)]
use deno_core::anyhow;
use coasys_juniper::{graphql_object, FieldError, FieldResult};
use futures::stream;
use futures::stream::StreamExt;

use crate::{holochain_service::get_holochain_service, perspectives::{all_perspectives, get_perspective}, types::{DecoratedLinkExpression }};

use super::graphql_types::*;
use crate::agent::{capabilities::*, signatures};

pub struct Query;

#[graphql_object(context = RequestContext)]
impl Query {
    async fn agent(&self, context: &RequestContext) -> FieldResult<Agent> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agent", null))"#,
            ))
            .await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    #[graphql(name = "agentByDID")]
    async fn agent_by_did(
        &self,
        context: &RequestContext,
        did: String,
    ) -> FieldResult<Option<Agent>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
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
    }

    async fn agent_get_apps(&self, context: &RequestContext) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_get_entanglement_proofs(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentGetEntanglementProofs", null, null))"#
            ))
            .await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_is_locked(&self, context: &RequestContext) -> FieldResult<bool> {
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentIsLocked", null, null))"#
            ))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_status(&self, context: &RequestContext) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentStatus"))"#,
            ))
            .await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "getTrustedAgents"))"#,
            ))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriendStatus", {{ did: "{}" }}))"#,
                did
            ))
            .await?;
        let result: JsResultType<PerspectiveExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friends(&self, context: &RequestContext) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_READ_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriends"))"#,
            ))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeInfo", null))"#,
            ))
            .await?;
        let result: JsResultType<RuntimeInfo> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_known_link_language_templates(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY,
        )?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeKnownLinkLanguageTemplates"))"#))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageInbox", {}))"#,
            filter_str,
        );
        let mut js = context.js_handle.clone();
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<PerspectiveExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_message_outbox(
        &self,
        context: &RequestContext,
        filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)?;
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageOutbox", {}))"#,
            filter_str,
        );
        let mut js = context.js_handle.clone();
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<SentMessage>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
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
