#![allow(non_snake_case)]
use juniper::{graphql_object, FieldResult};

use super::graphql_types::*;
use super::utils::get_capabilies;
use super::RequestContext;

pub struct Query;

#[graphql_object(context = RequestContext)]
impl Query {
    async fn agent(&self, context: &RequestContext) -> FieldResult<Agent> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agent", null, {{ capabilities: {} }}))"#,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(
                format!(
                    r#"JSON.stringify(
                    await core.callResolver("Query", "agentByDID", 
                        {{ did: "{}" }}, 
                        {{ capabilities: {} }}
                    )
                )"#,
                    did, capabilities
                )
                .into(),
            )
            .await?;
        let result: JsResultType<Option<Agent>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_get_apps(&self, context: &RequestContext) -> FieldResult<Vec<Apps>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentGetApps", {{ capabilities: {} }}))"#,
                capabilities
            ))
            .await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_get_entanglement_proofs(
        &self,
        context: &RequestContext,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let _capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
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
        let _capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentStatus", {{ capabilities: {} }}))"#,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expression", {{ url: "{}" }}, {{ capabilities: {} }}))"#,
                url,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionInteractions", {{ url: "{}" }}, {{ capabilities: {} }}))"#,
                url,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionMany", {{ urls: [{}] }}, {{ capabilities: {} }}))"#,
                urls_string,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionRaw", {{ url: "{}" }}, {{ capabilities: {} }}))"#,
                url,
                capabilities
            ))
            .await?;
        let result: JsResultType<Option<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn get_trusted_agents(&self, context: &RequestContext) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "getTrustedAgents", {{ capabilities: {} }}))"#,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "language", {{ address: "{}" }}, {{ capabilities: {} }}))"#,
                address,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageMeta", {{ address: "{}" }}, {{ capabilities: {} }}))"#,
                address,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageSource", {{ address: "{}" }}, {{ capabilities: {} }}))"#,
                address,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languages", {{ filter: "{}" }}, {{ capabilities: {} }}))"#,
                filter_string,
                capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodHasTelepresenceAdapter", {{ perspectiveUUID: "{}" }}, {{ capabilities: {} }}))"#, perspectiveUUID, capabilities))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_online_agents(
        &self,
        context: &RequestContext,
        perspectiveUUID: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodOnlineAgents", {{ perspectiveUUID: "{}" }}, {{ capabilities: {} }}))"#, perspectiveUUID, capabilities))
            .await?;
        let result: JsResultType<Vec<OnlineAgent>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_other_agents(
        &self,
        context: &RequestContext,
        perspectiveUUID: String,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodOtherAgents", {{ perspectiveUUID: "{}" }}, {{ capabilities: {} }}))"#, perspectiveUUID, capabilities))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<Option<PerspectiveHandle>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspective", {{ uuid: "{}" }}, {{ capabilities: {} }}))"#,
                uuid,
                capabilities
            ))
            .await?;
        let result: JsResultType<Option<PerspectiveHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_query_links(
        &self,
        context: &RequestContext,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let query_string = serde_json::to_string(&query)?;
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "perspectiveQueryLinks", {{ query: {}, uuid: "{}" }}, {{ capabilities: {} }}))"#,
            query_string, uuid, capabilities
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_query_prolog(
        &self,
        context: &RequestContext,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "perspectiveQueryProlog", {{ query: '{}', uuid: "{}" }}, {{ capabilities: {} }}))"#,
            query, uuid, capabilities
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_snapshot(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<Perspective> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspectiveSnapshot", {{ uuid: "{}" }}, {{ capabilities: {} }}))"#,
                uuid,
                capabilities
            ))
            .await?;
        let result: JsResultType<Perspective> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspectives(&self, context: &RequestContext) -> FieldResult<Vec<PerspectiveHandle>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspectives", {{ capabilities: {} }}))"#,
                capabilities
            ))
            .await?;
        let result: JsResultType<Vec<PerspectiveHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friend_status(
        &self,
        context: &RequestContext,
        did: String,
    ) -> FieldResult<PerspectiveExpression> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriendStatus", {{ did: "{}" }}, {{ capabilities: {} }}))"#,
                did,
                capabilities
            ))
            .await?;
        let result: JsResultType<PerspectiveExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friends(&self, context: &RequestContext) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriends", {{ capabilities: {} }}))"#,
                capabilities
            ))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_hc_agent_infos(&self, context: &RequestContext) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeHcAgentInfos", {{ capabilities: {} }}))"#, capabilities))
            .await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_info(&self, context: &RequestContext) -> FieldResult<RuntimeInfo> {
        let _capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeKnownLinkLanguageTemplates", {{ capabilities: {} }}))"#, capabilities))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_message_inbox(
        &self,
        context: &RequestContext,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageInbox", {}, {{ capabilities: {} }}))"#,
            filter_str, capabilities
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageOutbox", {}, {{ capabilities: {} }}))"#,
            filter_str, capabilities
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
        did_signing_key_id: String,
        signed_data: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(
                await core.callResolver("Query", "runtimeVerifyStringSignedByDid",
                    {{ data: "{}", did: "{}", didSigningKeyId: "{}", signedData: "{}" }},
                    {{ capabilities: {} }}
                )
            )"#,
                data, did, did_signing_key_id, signed_data, capabilities
            ))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }
}
