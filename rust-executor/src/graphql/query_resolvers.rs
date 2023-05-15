#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, FieldResult};
use log::debug;

use crate::js_core::JsCoreHandle;

use super::graphql_types::*;

pub struct Query;

const ALL_CAPABILITY: &str = r#"{with: {domain: "*", pointers: ["*"]},can: ["*"]}"#;

#[graphql_object(context = JsCoreHandle)]
impl Query {
    async fn agent(&self, context: &JsCoreHandle) -> FieldResult<Agent> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agent", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    #[graphql(name = "agentByDID")]
    async fn agent_by_did(
        &self,
        context: &JsCoreHandle,
        did: String,
    ) -> FieldResult<Option<Agent>> {
        let mut js = context.clone();
        let result = js
            .execute(
                format!(
                    r#"JSON.stringify(
                    await core.callResolver("Query", "agentByDID", 
                        {{ did: "{}" }}, 
                        {{ capabilities: [{}] }}
                    )
                )"#,
                    did, ALL_CAPABILITY
                )
                .into(),
            )
            .await?;
        let result: JsResultType<Option<Agent>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_get_apps(&self, context: &JsCoreHandle) -> FieldResult<Vec<Apps>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentGetApps", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_get_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentGetEntanglementProofs", null, null))"#
            ))
            .await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_is_locked(&self, context: &JsCoreHandle) -> FieldResult<bool> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentIsLocked", null, null))"#
            ))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_status(&self, context: &JsCoreHandle) -> FieldResult<AgentStatus> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "agentStatus", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<Option<ExpressionRendered>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expression", {{ url: "{}" }}, {{ capabilities: [{}] }}))"#,
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Option<ExpressionRendered>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_interactions(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<Vec<InteractionMeta>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionInteractions", {{ url: "{}" }}, {{ capabilities: [{}] }}))"#,
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<InteractionMeta>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_many(
        &self,
        context: &JsCoreHandle,
        urls: Vec<String>,
    ) -> FieldResult<Vec<Option<ExpressionRendered>>> {
        let urls_string = urls
            .into_iter()
            .map(|url| format!("\"{}\"", url))
            .collect::<Vec<String>>()
            .join(",");
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionMany", {{ urls: [{}] }}, {{ capabilities: [{}] }}))"#,
                urls_string,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<Option<ExpressionRendered>>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_raw(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<Option<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "expressionRaw", {{ url: "{}" }}, {{ capabilities: [{}] }}))"#,
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Option<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn get_trusted_agents(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "getTrustedAgents", {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<LanguageHandle> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "language", {{ address: "{}" }}, {{ capabilities: [{}] }}))"#,
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<LanguageHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_meta(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<LanguageMeta> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageMeta", {{ address: "{}" }}, {{ capabilities: [{}] }}))"#,
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<LanguageMeta> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_source(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languageSource", {{ address: "{}" }}, {{ capabilities: [{}] }}))"#,
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn languages(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<LanguageHandle>> {
        let filter_string = filter.map_or("null".to_string(), |f| format!("\"{}\"", f));
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "languages", {{ filter: "{}" }}, {{ capabilities: [{}] }}))"#,
                filter_string,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<LanguageHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_has_telepresence_adapter(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodHasTelepresenceAdapter", {{ perspectiveUUID: "{}" }}, {{ capabilities: [{}] }}))"#, perspective_uuid, ALL_CAPABILITY))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_online_agents(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodOnlineAgents", {{ perspectiveUUID: "{}" }}, {{ capabilities: [{}] }}))"#, perspective_uuid, ALL_CAPABILITY))
            .await?;
        let result: JsResultType<Vec<OnlineAgent>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_other_agents(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "neighbourhoodOtherAgents", {{ perspectiveUUID: "{}" }}, {{ capabilities: [{}] }}))"#, perspective_uuid, ALL_CAPABILITY))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<Option<PerspectiveHandle>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspective", {{ uuid: "{}" }}, {{ capabilities: [{}] }}))"#,
                uuid,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Option<PerspectiveHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_query_links(
        &self,
        context: &JsCoreHandle,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let query_string = serde_json::to_string(&query)?;
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "perspectiveQueryLinks", {{ query: {}, uuid: "{}" }}, {{ capabilities: [{}] }}))"#,
            query_string, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_query_prolog(
        &self,
        context: &JsCoreHandle,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let script = format!(
            r#"await core.callResolver("Query", "perspectiveQueryProlog", {{ query: '{}', uuid: "{}" }}, {{ capabilities: [{}] }})"#,
            query, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_snapshot(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<Perspective> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspectiveSnapshot", {{ uuid: "{}" }}, {{ capabilities: [{}] }}))"#,
                uuid,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Perspective> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspectives(&self, context: &JsCoreHandle) -> FieldResult<Vec<PerspectiveHandle>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "perspectives", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<PerspectiveHandle>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friend_status(
        &self,
        context: &JsCoreHandle,
        did: String,
    ) -> FieldResult<PerspectiveExpression> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriendStatus", {{ did: "{}" }}, {{ capabilities: [{}] }}))"#,
                did,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<PerspectiveExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friends(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(await core.callResolver("Query", "runtimeFriends", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_hc_agent_infos(&self, context: &JsCoreHandle) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeHcAgentInfos", null, {{ capabilities: [{}] }}))"#, ALL_CAPABILITY))
            .await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_info(&self, context: &JsCoreHandle) -> FieldResult<RuntimeInfo> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeInfo", null, {{ capabilities: [{}] }}))"#,
                ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<RuntimeInfo> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(r#"JSON.stringify(await core.callResolver("Query", "runtimeKnownLinkLanguageTemplates", {{ capabilities: [{}] }}))"#, ALL_CAPABILITY))
            .await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_message_inbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageInbox", {}, {{ capabilities: [{}] }}))"#,
            filter_str, ALL_CAPABILITY
        );
        let mut js = context.clone();
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<PerspectiveExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_message_outbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        let filter_str = filter
            .map(|val| format!(r#"{{ filter: "{}" }}"#, val))
            .unwrap_or_else(|| String::from("{ filter: null }"));
        let script = format!(
            r#"JSON.stringify(await core.callResolver("Query", "runtimeMessageOutbox", {}, {{ capabilities: [{}] }}))"#,
            filter_str, ALL_CAPABILITY
        );
        let mut js = context.clone();
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<SentMessage>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_verify_string_signed_by_did(
        &self,
        context: &JsCoreHandle,
        data: String,
        did: String,
        did_signing_key_id: String,
        signed_data: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                r#"JSON.stringify(
                await core.callResolver("Query", "runtimeVerifyStringSignedByDid",
                    {{ data: "{}", did: "{}", didSigningKeyId: "{}", signedData: "{}" }},
                    {{ capabilities: [{}] }}
                )
            )"#,
                data, did, did_signing_key_id, signed_data, ALL_CAPABILITY
            ))
            .await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }
}
