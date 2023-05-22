#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, FieldResult};
use log::debug;

use super::graphql_types::*;
use super::utils::get_capabilies;
use super::RequestContext;

pub struct Mutation;

#[graphql_object(context = RequestContext)]
impl Mutation {
    async fn add_trusted_agents(
        &self,
        context: &RequestContext,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "addTrustedAgents", {{ agents: {:?} }}, {{ capabilities: [{}] }})
            )"#,
            agents, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_add_entanglement_proofs(
        &self,
        context: &RequestContext,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentAddEntanglementProofs", {{ proofs: {} }}, {{ capabilities: [{}] }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
            context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_delete_entanglement_proofs(
        &self,
        context: &RequestContext,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentDeleteEntanglementProofs", {{ proofs: {} }}, {{ capabilities: [{}] }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
            context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<EntanglementProof>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_entanglement_proof_pre_flight(
        &self,
        context: &RequestContext,
        device_key: String,
        device_key_type: String,
    ) -> FieldResult<EntanglementProof> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentEntanglementProofPreFlight", {{ deviceKey: "{}", deviceKeyType: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            device_key, device_key_type, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<EntanglementProof> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_generate(
        &self,
        context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerate", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_generate_jwt(
        &self,
        context: &RequestContext,
        rand: String,
        request_id: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerateJwt", {{ rand: "{}", requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            rand, request_id, context.capability
        );
        debug!("agent_generate_jwt script: {}", script);
        let result = js.execute(script).await?;
        debug!("agent_generate_jwt result: {}", result);
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_lock(
        &self,
        context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentLock", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    //NOTE: all the functions from here on out have not been tested by calling the cli <-> rust graphql server
    async fn agent_permit_capability(
        &self,
        context: &RequestContext,
        auth: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentPermitCapability", {{ auth: JSON.stringify({}) }}, {{ capabilities: [{}] }})
            )"#,
            auth, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_remove_app(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRemoveApp", {{ requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            request_id, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_request_capability(
        &self,
        context: &RequestContext,
        auth_info: AuthInfoInput,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let auth_info_json = serde_json::to_string(&auth_info)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRequestCapability", {{ authInfo: {} }}, {{ capabilities: [{}] }})
            )"#,
            auth_info_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_revoke_token(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentRevokeToken", {{ requestId: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            request_id, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<Apps>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_sign_message(
        &self,
        context: &RequestContext,
        message: String,
    ) -> FieldResult<AgentSignature> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentSignMessage", {{ message: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            message, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentSignature> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_unlock(
        &self,
        context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUnlock", {{ passphrase: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            passphrase, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_update_direct_message_language(
        &self,
        context: &RequestContext,
        direct_message_language: String,
    ) -> FieldResult<Agent> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdateDirectMessageLanguage", {{ directMessageLanguage: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            direct_message_language, context.capability
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let perspective_json = serde_json::to_string(&perspective)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}, {{ capabilities: [{}] }})
            )"#,
            perspective_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Agent> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn delete_trusted_agents(
        &self,
        context: &RequestContext,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "deleteTrustedAgents", {{ agents: {} }}, {{ capabilities: [{}] }})
            )"#,
            agents_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn expression_create(
        &self,
        context: &RequestContext,
        content: String,
        language_address: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: "{}" }}, {{ capabilities: [{}] }})
            )"#,
            content, language_address, context.capability
        );
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let interaction_call_json = serde_json::to_string(&interaction_call)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "expressionInteract",
                {{ interactionCall: {}, url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            interaction_call_json, url, context.capability
        );
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageApplyTemplateAndPublish",
                {{ sourceLanguageHash: "{}", templateData: JSON.stringify({}) }},
                {{ capabilities: [{}] }}
            ))"#,
            source_language_hash, template_data, context.capability
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let language_meta_json = serde_json::to_string(&language_meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languagePublish",
                {{ languageMeta: {}, languagePath: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            language_meta_json, language_path, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LanguageMeta> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn language_remove(
        &self,
        context: &RequestContext,
        address: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageRemove",
                {{ address: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            address, context.capability
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageWriteSettings",
                {{ languageAddress: "{}", settings: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            language_address, settings, context.capability
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
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodJoinFromUrl",
                {{ url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            url, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_publish_from_perspective(
        &self,
        context: &RequestContext,
        link_language: String,
        meta: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let meta_json = serde_json::to_string(&meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodPublishFromPerspective",
                {{ linkLanguage: "{}", meta: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_language, meta_json, perspectiveUUID, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_broadcast(
        &self,
        context: &RequestContext,
        payload: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcast",
                {{ payload: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_broadcast_u(
        &self,
        context: &RequestContext,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcastU",
                {{ payload: {}, perspectiveUUID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_signal(
        &self,
        context: &RequestContext,
        payload: PerspectiveInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignal",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_send_signal_u(
        &self,
        context: &RequestContext,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignalU",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDID: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_set_online_status(
        &self,
        context: &RequestContext,
        perspectiveUUID: String,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatus",
                {{ perspectiveUUID: "{}", status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            perspectiveUUID, status_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn neighbourhood_set_online_status_u(
        &self,
        context: &RequestContext,
        perspectiveUUID: String,
        status: PerspectiveUnsignedInput,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatusU",
                {{ perspectiveUUID: "{}", status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            perspectiveUUID, status_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add(
        &self,
        context: &RequestContext,
        name: String,
    ) -> FieldResult<PerspectiveHandle> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAdd",
                {{ name: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            name, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_link(
        &self,
        context: &RequestContext,
        link: LinkInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLink",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_link_expression(
        &self,
        context: &RequestContext,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinkExpression",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let links_json = serde_json::to_string(&links)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinks",
                {{ links: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            links_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_link_mutations(
        &self,
        context: &RequestContext,
        mutations: LinkMutations,
        uuid: String,
    ) -> FieldResult<LinkExpressionMutations> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let mutations_json = serde_json::to_string(&mutations)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveLinkMutations",
                {{ mutations: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            mutations_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpressionMutations> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_publish_snapshot(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<String> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectivePublishSnapshot",
                {{ uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<String> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemove",
                {{ uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove_link(
        &self,
        context: &RequestContext,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLink",
                {{ link: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            link_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_remove_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkExpressionInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let links_json = serde_json::to_string(&links)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLinks",
                {{ links: {}, uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            links_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<LinkExpression>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_update(
        &self,
        context: &RequestContext,
        name: String,
        uuid: String,
    ) -> FieldResult<PerspectiveHandle> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveUpdate",
                {{ name: "{}", uuid: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            name, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<PerspectiveHandle> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_update_link(
        &self,
        context: &RequestContext,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
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
            new_link_json, old_link_json, uuid, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_add_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddFriends",
                {{ dids: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            dids_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_add_known_link_language_templates(
        &self,
        context: &RequestContext,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddKnownLinkLanguageTemplates",
                {{ addresses: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            addresses_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_friend_send_message(
        &self,
        context: &RequestContext,
        did: String,
        message: PerspectiveInput,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let message_json = serde_json::to_string(&message)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeFriendSendMessage",
                {{ did: "{}", message: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            did, message_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_hc_add_agent_infos(
        &self,
        context: &RequestContext,
        agent_infos: String,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeHcAddAgentInfos",
                {{ agentInfos: JSON.stringify({}) }},
                {{ capabilities: [{}] }}
            ))"#,
            agent_infos, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_open_link(&self, context: &RequestContext, url: String) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeOpenLink",
                {{ url: "{}" }},
                {{ capabilities: [{}] }}
            ))"#,
            url, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_quit(&self, context: &RequestContext) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeQuit",
                {{ capabilities: [{}] }}
            ))"#,
            context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_remove_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveFriends",
                {{ dids: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            dids_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_remove_known_link_language_templates(
        &self,
        context: &RequestContext,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveKnownLinkLanguageTemplates",
                {{ addresses: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            addresses_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_set_status(
        &self,
        context: &RequestContext,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        let capabilities =
            get_capabilies(context.js_handle.clone(), context.capability.clone()).await?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeSetStatus",
                {{ status: {} }},
                {{ capabilities: [{}] }}
            ))"#,
            status_json, context.capability
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }
}
