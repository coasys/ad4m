#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, FieldResult};
use log::debug;

use crate::js_core::JsCoreHandle;

use super::graphql_types::*;

pub struct Mutation;

const ALL_CAPABILITY: &str = r#"{with: {domain: "*", pointers: ["*"]},can: ["*"]}"#;

#[graphql_object(context = JsCoreHandle)]
impl Mutation {
    async fn add_trusted_agents(
        &self,
        context: &JsCoreHandle,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "addTrustedAgents", {{ agents: {:?} }}, {{ capabilities: [{}] }})
            )"#,
            agents, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_add_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentAddEntanglementProofs", {{ proofs: {} }}, {{ capabilities: [{}] }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
            ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_delete_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentDeleteEntanglementProofs", {{ proofs: {} }}, {{ capabilities: [{}] }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
            ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_entanglement_proof_pre_flight(
        &self,
        context: &JsCoreHandle,
        device_key: String,
        device_key_type: String,
    ) -> FieldResult<EntanglementProof> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentEntanglementProofPreFlight", {{ deviceKey: "{}", deviceKeyType: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            device_key, device_key_type, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<EntanglementProof> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_generate(
        &self,
        context: &JsCoreHandle,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerate", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_generate_jwt(
        &self,
        context: &JsCoreHandle,
        rand: String,
        request_id: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerateJwt", {{ rand: "{}", requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            rand, request_id, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_lock(
        &self,
        context: &JsCoreHandle,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentLock", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    //NOTE: all the functions from here on out have not been tested by calling the cli <-> rust graphql server
    async fn agent_permit_capability(
        &self,
        context: &JsCoreHandle,
        auth: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentPermitCapability", {{ auth: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            auth, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_remove_app(
        &self,
        context: &JsCoreHandle,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRemoveApp", {{ requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            request_id, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_request_capability(
        &self,
        context: &JsCoreHandle,
        auth_info: AuthInfoInput,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let auth_info_json = serde_json::to_string(&auth_info)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRequestCapability", {{ authInfo: {} }}, {{ capabilities: [{}] }})
            )"#,
            auth_info_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_revoke_token(
        &self,
        context: &JsCoreHandle,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRevokeToken", {{ requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            request_id, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_sign_message(
        &self,
        context: &JsCoreHandle,
        message: String,
    ) -> FieldResult<AgentSignature> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentSignMessage", {{ message: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            message, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentSignature> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_unlock(
        &self,
        context: &JsCoreHandle,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUnlock", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_update_direct_message_language(
        &self,
        context: &JsCoreHandle,
        direct_message_language: String,
    ) -> FieldResult<Agent> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdateDirectMessageLanguage", {{ directMessageLanguage: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            direct_message_language, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_update_public_perspective(
        &self,
        context: &JsCoreHandle,
        perspective: PerspectiveInput,
    ) -> FieldResult<Agent> {
        let mut js = context.clone();
        let perspective_json = serde_json::to_string(&perspective)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}, {{ capabilities: [{}] }})
            )"#,
            perspective_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn delete_trusted_agents(
        &self,
        context: &JsCoreHandle,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "deleteTrustedAgents", {{ agents: {} }}, {{ capabilities: [{}] }})
            )"#,
            agents_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_create(
        &self,
        context: &JsCoreHandle,
        content: String,
        language_address: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            content, language_address, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_interact(
        &self,
        context: &JsCoreHandle,
        interaction_call: InteractionCall,
        url: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let interaction_call_json = serde_json::to_string(&interaction_call)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "expressionInteract",
                {{ interactionCall: {}, url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            interaction_call_json, url, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_apply_template_and_publish(
        &self,
        context: &JsCoreHandle,
        source_language_hash: String,
        template_data: String,
    ) -> FieldResult<LanguageRef> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageApplyTemplateAndPublish",
                {{ sourceLanguageHash: "{}", templateData: JSON.stringify({}) }},
                {{ capabilities: [{}] }}
            ))"#,
            source_language_hash, template_data, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LanguageRef> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_publish(
        &self,
        context: &JsCoreHandle,
        language_meta: LanguageMetaInput,
        language_path: String,
    ) -> FieldResult<LanguageMeta> {
        let mut js = context.clone();
        let language_meta_json = serde_json::to_string(&language_meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languagePublish",
                {{ languageMeta: {}, languagePath: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            language_meta_json, language_path, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LanguageMeta> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_remove(&self, context: &JsCoreHandle, address: String) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageRemove",
                {{ address: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            address, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_write_settings(
        &self,
        context: &JsCoreHandle,
        language_address: String,
        settings: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageWriteSettings",
                {{ languageAddress: "{}", settings: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            language_address, settings, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_join_from_url(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<PerspectiveHandle> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodJoinFromUrl",
                {{ url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            url, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_publish_from_perspective(
        &self,
        context: &JsCoreHandle,
        link_language: String,
        meta: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let meta_json = serde_json::to_string(&meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodPublishFromPerspective",
                {{ linkLanguage: "{}", meta: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_language, meta_json, perspectiveUUID, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_broadcast(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcast",
                {{ payload: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_broadcast_u(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcastU",
                {{ payload: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_signal(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignal",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_signal_u(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignalU",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_set_online_status(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatus",
                {{ perspectiveUUID: "{}", status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            perspectiveUUID, status_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_set_online_status_u(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
        status: PerspectiveUnsignedInput,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatusU",
                {{ perspectiveUUID: "{}", status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            perspectiveUUID, status_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add(
        &self,
        context: &JsCoreHandle,
        name: String,
    ) -> FieldResult<PerspectiveHandle> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAdd",
                {{ name: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            name, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_link(
        &self,
        context: &JsCoreHandle,
        link: LinkInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let mut js = context.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLink",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_link_expression(
        &self,
        context: &JsCoreHandle,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let mut js = context.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinkExpression",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_links(
        &self,
        context: &JsCoreHandle,
        links: Vec<LinkInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let mut js = context.clone();
        let links_json = serde_json::to_string(&links)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinks",
                {{ links: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            links_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_link_mutations(
        &self,
        context: &JsCoreHandle,
        mutations: LinkMutations,
        uuid: String,
    ) -> FieldResult<LinkExpressionMutations> {
        let mut js = context.clone();
        let mutations_json = serde_json::to_string(&mutations)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveLinkMutations",
                {{ mutations: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            mutations_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpressionMutations> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_publish_snapshot(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<String> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectivePublishSnapshot",
                {{ uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove(&self, context: &JsCoreHandle, uuid: String) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemove",
                {{ uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove_link(
        &self,
        context: &JsCoreHandle,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLink",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove_links(
        &self,
        context: &JsCoreHandle,
        links: Vec<LinkExpressionInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let mut js = context.clone();
        let links_json = serde_json::to_string(&links)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLinks",
                {{ links: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            links_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_update(
        &self,
        context: &JsCoreHandle,
        name: String,
        uuid: String,
    ) -> FieldResult<PerspectiveHandle> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveUpdate",
                {{ name: "{}", uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            name, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_update_link(
        &self,
        context: &JsCoreHandle,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let mut js = context.clone();
        let new_link_json = serde_json::to_string(&new_link)?;
        let old_link_json = serde_json::to_string(&old_link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveUpdateLink",
                {{ newLink: {}, oldLink: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            new_link_json, old_link_json, uuid, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_add_friends(
        &self,
        context: &JsCoreHandle,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddFriends",
                {{ dids: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            dids_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_add_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddKnownLinkLanguageTemplates",
                {{ addresses: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            addresses_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friend_send_message(
        &self,
        context: &JsCoreHandle,
        did: String,
        message: PerspectiveInput,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let message_json = serde_json::to_string(&message)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeFriendSendMessage",
                {{ did: "{}", message: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            did, message_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_hc_add_agent_infos(
        &self,
        context: &JsCoreHandle,
        agent_infos: String,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeHcAddAgentInfos",
                {{ agentInfos: JSON.stringify({}) }},
                {{ capabilities: [{}] }}
            ))"#,
            agent_infos, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_open_link(&self, context: &JsCoreHandle, url: String) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeOpenLink",
                {{ url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            url, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_quit(&self, context: &JsCoreHandle) -> FieldResult<bool> {
        let mut js = context.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeQuit",
                {{ capabilities: [{}] }}
            ))"#,
            ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_remove_friends(
        &self,
        context: &JsCoreHandle,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveFriends",
                {{ dids: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            dids_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_remove_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let mut js = context.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveKnownLinkLanguageTemplates",
                {{ addresses: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            addresses_json, ALL_CAPABILITY
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_set_status(
        &self,
        context: &JsCoreHandle,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        let mut js = context.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeSetStatus",
                {{ status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            status_json, ALL_CAPABILITY
        );
        debug!("runtime_set_status script: {}", script);
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }
}
