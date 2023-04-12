#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, FieldResult};

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
                "JSON.stringify(core.resolvers.Query.agent(null, {{ capabilities: [{}] }}))",
                ALL_CAPABILITY
            ))
            .await?;
        let a: Agent = serde_json::from_str(&result)?;
        return Ok(a);
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
                    core.resolvers.Query.agentByDID(
                        null, 
                        {{ did: {} }}, 
                        {{ capabilities: [{}] }}
                    )
                )"#,
                    did, ALL_CAPABILITY
                )
                .into(),
            )
            .await?;
        let a: Option<Agent> = serde_json::from_str(&result)?;
        return Ok(a);
    }

    async fn agent_get_apps(&self, context: &JsCoreHandle) -> FieldResult<Vec<Apps>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.agentGetApps(null, {{ capabilities: [{}] }}))",
                ALL_CAPABILITY
            ))
            .await?;
        let apps: Vec<Apps> = serde_json::from_str(&result)?;
        return Ok(apps);
    }

    async fn agent_get_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.agentGetEntanglementProofs(null, {{ capabilities: [{}] }}))", ALL_CAPABILITY))
            .await?;
        let proofs: Vec<EntanglementProof> = serde_json::from_str(&result)?;
        return Ok(proofs);
    }

    async fn agent_is_locked(&self, context: &JsCoreHandle) -> FieldResult<bool> {
        let mut js = context.clone();
        let result = js
            .execute("JSON.stringify(core.resolvers.Query.agentIsLocked(null))".to_string())
            .await?;
        let is_locked: bool = serde_json::from_str(&result)?;
        return Ok(is_locked);
    }

    async fn agent_status(&self, context: &JsCoreHandle) -> FieldResult<AgentStatus> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.agentService.dump(null, {{ capabilities: [{}] }}))",
                ALL_CAPABILITY
            ))
            .await?;
        let s: AgentStatus = serde_json::from_str(&result)?;
        return Ok(s);
    }

    async fn expression(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<ExpressionRendered> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.expression(null, {{ url: {} }}, {{ capabilities: [{}] }}))",
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let expression: ExpressionRendered = serde_json::from_str(&result)?;
        return Ok(expression);
    }

    async fn expression_interactions(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<Vec<InteractionMeta>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.expressionInteractions(null, {{ url: {} }}, {{ capabilities: [{}] }}))",
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let interactions: Vec<InteractionMeta> = serde_json::from_str(&result)?;
        return Ok(interactions);
    }

    async fn expression_many(
        &self,
        context: &JsCoreHandle,
        urls: Vec<String>,
    ) -> FieldResult<Vec<ExpressionRendered>> {
        let urls_string = urls
            .into_iter()
            .map(|url| format!("\"{}\"", url))
            .collect::<Vec<String>>()
            .join(",");
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.expressionMany(null, {{ urls: [{}] }}, {{ capabilities: [{}] }}))",
                urls_string,
                ALL_CAPABILITY
            ))
            .await?;
        let expressions: Vec<ExpressionRendered> = serde_json::from_str(&result)?;
        return Ok(expressions);
    }

    async fn expression_raw(&self, context: &JsCoreHandle, url: String) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.expressionRaw(null, {{ url: {} }}, {{ capabilities: [{}] }}))",
                url,
                ALL_CAPABILITY
            ))
            .await?;
        let expression_raw: String = serde_json::from_str(&result)?;
        return Ok(expression_raw);
    }

    async fn get_trusted_agents(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.getTrustedAgents(null, {{ capabilities: [{}] }}))", ALL_CAPABILITY))
            .await?;
        let trusted_agents: Vec<String> = serde_json::from_str(&result)?;
        return Ok(trusted_agents);
    }

    async fn language(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<LanguageHandle> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.language(null, {{ address: {}, {{ capabilities: [{}] }} }}))",
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let language_handle: LanguageHandle = serde_json::from_str(&result)?;
        return Ok(language_handle);
    }

    async fn language_meta(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<LanguageMeta> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.languageMeta(null, {{ address: {}, {{ capabilities: [{}] }} }}))",
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let language_meta: LanguageMeta = serde_json::from_str(&result)?;
        return Ok(language_meta);
    }

    async fn language_source(
        &self,
        context: &JsCoreHandle,
        address: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.languageSource(null, {{ address: {}, {{ capabilities: [{}] }} }}))",
                address,
                ALL_CAPABILITY
            ))
            .await?;
        let language_source: String = serde_json::from_str(&result)?;
        return Ok(language_source);
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
                "JSON.stringify(core.resolvers.Query.languages(null, {{ filter: {}, {{ capabilities: [{}] }} }}))",
                filter_string,
                ALL_CAPABILITY
            ))
            .await?;
        let languages: Vec<LanguageHandle> = serde_json::from_str(&result)?;
        return Ok(languages);
    }

    async fn neighbourhood_has_telepresence_adapter(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.neighbourhoodHasTelepresenceAdapter(null, {{ perspectiveUUID: {}, {{ capabilities: [{}] }} }}))", perspective_uuid, ALL_CAPABILITY))
            .await?;
        let has_adapter: bool = serde_json::from_str(&result)?;
        return Ok(has_adapter);
    }

    async fn neighbourhood_online_agents(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.neighbourhoodOnlineAgents(null, {{ perspectiveUUID: {}, {{ capabilities: [{}] }} }}))", perspective_uuid, ALL_CAPABILITY))
            .await?;
        let online_agents: Vec<OnlineAgent> = serde_json::from_str(&result)?;
        return Ok(online_agents);
    }

    async fn neighbourhood_other_agents(
        &self,
        context: &JsCoreHandle,
        perspective_uuid: String,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.neighbourhoodOtherAgents(null, {{ perspectiveUUID: {}, {{ capabilities: [{}] }} }}))", perspective_uuid, ALL_CAPABILITY))
            .await?;
        let other_agents: Vec<String> = serde_json::from_str(&result)?;
        return Ok(other_agents);
    }

    async fn perspective(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<PerspectiveHandle> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.perspective(null, {{ uuid: {}, {{ capabilities: [{}] }} }}))",
                uuid,
                ALL_CAPABILITY
            ))
            .await?;
        let perspective_handle: PerspectiveHandle = serde_json::from_str(&result)?;
        return Ok(perspective_handle);
    }

    async fn perspective_query_links(
        &self,
        context: &JsCoreHandle,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let query_string = serde_json::to_string(&query)?;
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.perspectiveQueryLinks(null, {{ query: {}, uuid: {}, {{ capabilities: [{}] }} }}))", query_string, uuid, ALL_CAPABILITY))
            .await?;
        let link_expressions: Vec<LinkExpression> = serde_json::from_str(&result)?;
        return Ok(link_expressions);
    }

    async fn perspective_query_prolog(
        &self,
        context: &JsCoreHandle,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.perspectiveQueryProlog(null, {{ query: {}, uuid: {}, {{ capabilities: [{}] }} }}))", query, uuid, ALL_CAPABILITY))
            .await?;
        let prolog_result: String = serde_json::from_str(&result)?;
        return Ok(prolog_result);
    }

    async fn perspective_snapshot(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<Perspective> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.perspectiveSnapshot(null, {{ uuid: {}, {{ capabilities: [{}] }} }}))",
                uuid,
                ALL_CAPABILITY
            ))
            .await?;
        let perspective_snapshot: Perspective = serde_json::from_str(&result)?;
        return Ok(perspective_snapshot);
    }

    async fn perspectives(&self, context: &JsCoreHandle) -> FieldResult<Vec<PerspectiveHandle>> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.perspectives(null, {{ capabilities: [{}] }}))",
                ALL_CAPABILITY
            ))
            .await?;
        let perspectives: Vec<PerspectiveHandle> = serde_json::from_str(&result)?;
        return Ok(perspectives);
    }

    async fn runtime_friend_status(
        &self,
        context: &JsCoreHandle,
        did: String,
    ) -> FieldResult<PerspectiveExpression> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.runtimeFriendStatus(null, {{ did: {} }}, {{ capabilities: [{}] }}))",
                did,
                ALL_CAPABILITY
            ))
            .await?;
        let friend_status: PerspectiveExpression = serde_json::from_str(&result)?;
        return Ok(friend_status);
    }

    async fn runtime_friends(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.runtimeFriends(null, {{ capabilities: [{}] }}))", ALL_CAPABILITY))
            .await?;
        let friends: Vec<String> = serde_json::from_str(&result)?;
        return Ok(friends);
    }

    async fn runtime_hc_agent_infos(&self, context: &JsCoreHandle) -> FieldResult<String> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.runtimeHcAgentInfos(null, {{ capabilities: [{}] }}))", ALL_CAPABILITY))
            .await?;
        let hc_agent_infos: String = serde_json::from_str(&result)?;
        return Ok(hc_agent_infos);
    }

    async fn runtime_info(&self, context: &JsCoreHandle) -> FieldResult<RuntimeInfo> {
        let mut js = context.clone();
        let result = js
            .execute(format!(
                "JSON.stringify(core.resolvers.Query.runtimeInfo(null, {{ capabilities: [{}] }}))",
                ALL_CAPABILITY
            ))
            .await?;
        let runtime_info: RuntimeInfo = serde_json::from_str(&result)?;
        return Ok(runtime_info);
    }

    async fn runtime_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.runtimeKnownLinkLanguageTemplates(null, {{ capabilities: [{}] }}))", ALL_CAPABILITY))
            .await?;
        let templates: Vec<String> = serde_json::from_str(&result)?;
        return Ok(templates);
    }

    async fn runtime_message_inbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        let filter_str = filter.unwrap_or_else(|| String::from("{}"));
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.runtimeMessageInbox(null, {{ filter: {}, capabilities: [{}] }}))", filter_str, ALL_CAPABILITY))
            .await?;
        let inbox_messages: Vec<PerspectiveExpression> = serde_json::from_str(&result)?;
        return Ok(inbox_messages);
    }

    async fn runtime_message_outbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        let filter_str = filter.unwrap_or_else(|| String::from("{}"));
        let mut js = context.clone();
        let result = js
            .execute(format!("JSON.stringify(core.resolvers.Query.runtimeMessageOutbox(null, {{ filter: {}, capabilities: [{}] }}))", filter_str, ALL_CAPABILITY))
            .await?;
        let outbox_messages: Vec<SentMessage> = serde_json::from_str(&result)?;
        return Ok(outbox_messages);
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
            .execute(
                format!(
                    r#"JSON.stringify(core.resolvers.Query.runtimeVerifyStringSignedByDID(
                        null,
                        {{ data: {}, did: {}, didSigningKeyId: {}, signedData: {}, capabilities: [{}] }}
                    ))"#,
                    data, did, did_signing_key_id, signed_data, ALL_CAPABILITY
                ),
            )
            .await?;
        let verified: bool = serde_json::from_str(&result)?;
        return Ok(verified);
    }
}