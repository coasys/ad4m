#![allow(non_snake_case)]
use juniper::{graphql_object, graphql_value, FieldResult};
use kitsune_p2p_types::agent_info::AgentInfoSigned;
use log::debug;

use super::graphql_types::*;
use crate::{agent::{self, capabilities::*}, holochain_service::{agent_infos_from_str, get_holochain_service}};
use ad4m_client::literal::Literal;
pub struct Mutation;

#[graphql_object(context = RequestContext)]
impl Mutation {
    async fn add_trusted_agents(
        &self,
        context: &RequestContext,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(
            &context.capabilities,
            &RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY,
        )?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "addTrustedAgents", {{ agents: {:?} }})
            )"#,
            agents
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
        //TODO: capability missing for this function
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentAddEntanglementProofs", {{ proofs: {} }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
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
        //TODO: capability missing for this function
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentDeleteEntanglementProofs", {{ proofs: {} }})
            )"#,
            serde_json::to_string(&proofs).unwrap(),
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
        //TODO: capability missing for this function
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentEntanglementProofPreFlight", {{ deviceKey: "{}", deviceKeyType: "{}" }})
            )"#,
            device_key, device_key_type
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
        check_capability(&context.capabilities, &AGENT_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentGenerate", {{ passphrase: "{}" }})
            )"#,
            passphrase
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_lock(
        &self,
        context: &RequestContext,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_LOCK_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentLock", {{ passphrase: "{}" }})
            )"#,
            passphrase
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentStatus> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_remove_app(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        apps_map::remove_app(&request_id)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_request_capability(
        &self,
        context: &RequestContext,
        auth_info: AuthInfoInput,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)?;
        Ok(agent::capabilities::request_capability(auth_info.into()).await)
    }

    //NOTE: all the functions from here on out have not been tested by calling the cli <-> rust graphql server
    async fn agent_permit_capability(
        &self,
        context: &RequestContext,
        auth: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_PERMIT_CAPABILITY)?;
        let auth: AuthInfoExtended = serde_json::from_str(&auth)?;
        let random_number_challenge = agent::capabilities::permit_capability(auth)?;
        Ok(random_number_challenge)
    }

    async fn agent_generate_jwt(
        &self,
        context: &RequestContext,
        rand: String,
        request_id: String,
    ) -> FieldResult<String> {
        check_capability(&context.capabilities, &AGENT_AUTH_CAPABILITY)?;
        let cap_token = agent::capabilities::generate_capability_token(request_id, rand).await?;
        Ok(cap_token)
    }

    async fn agent_revoke_token(
        &self,
        context: &RequestContext,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        apps_map::revoke_app(&request_id)?;
        Ok(apps_map::get_apps())
    }

    async fn agent_sign_message(
        &self,
        context: &RequestContext,
        message: String,
    ) -> FieldResult<AgentSignature> {
        check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentSignMessage", {{ message: "{}" }})
            )"#,
            message
        );
        let result = js.execute(script).await?;
        let result: JsResultType<AgentSignature> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn agent_unlock(
        &self,
        context: &RequestContext,
        passphrase: String,
        holochain: bool,
    ) -> FieldResult<AgentStatus> {
        check_capability(&context.capabilities, &AGENT_SIGN_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUnlock", {{ passphrase: "{}", holochain: "{}" }})
            )"#,
            passphrase, holochain
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
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdateDirectMessageLanguage", {{ directMessageLanguage: "{}" }})
            )"#,
            direct_message_language
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
        check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let perspective_json = serde_json::to_string(&perspective)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }})
            )"#,
            perspective_json
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
        check_capability(
            &context.capabilities,
            &RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY,
        )?;
        let mut js = context.js_handle.clone();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "deleteTrustedAgents", {{ agents: {} }})
            )"#,
            agents_json
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
        check_capability(&context.capabilities, &EXPRESSION_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: "{}" }})
            )"#,
            content, language_address
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
        check_capability(&context.capabilities, &EXPRESSION_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let interaction_call_json = serde_json::to_string(&interaction_call)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "expressionInteract",
                {{ interactionCall: {}, url: "{}" }},
            ))"#,
            interaction_call_json, url
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
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageApplyTemplateAndPublish",
                {{ sourceLanguageHash: "{}", templateData: JSON.stringify({}) }},
            ))"#,
            source_language_hash, template_data
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
        check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let language_meta_json = serde_json::to_string(&language_meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languagePublish",
                {{ languageMeta: {}, languagePath: "{}" }},
            ))"#,
            language_meta_json, language_path
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
        check_capability(&context.capabilities, &LANGUAGE_DELETE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageRemove",
                {{ address: "{}" }},
            ))"#,
            address
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
        check_capability(&context.capabilities, &LANGUAGE_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "languageWriteSettings",
                {{ languageAddress: "{}", settings: "{}" }},
            ))"#,
            language_address, settings
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodJoinFromUrl",
                {{ url: "{}" }},
            ))"#,
            url
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let meta_json = serde_json::to_string(&meta)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodPublishFromPerspective",
                {{ linkLanguage: "{}", meta: {}, perspectiveUUID: "{}" }},
            ))"#,
            link_language, meta_json, perspectiveUUID
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcast",
                {{ payload: {}, perspectiveUUID: "{}" }},
            ))"#,
            payload_json, perspectiveUUID
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendBroadcastU",
                {{ payload: {}, perspectiveUUID: "{}" }},
            ))"#,
            payload_json, perspectiveUUID
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignal",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDid: "{}" }},
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSendSignalU",
                {{ payload: {}, perspectiveUUID: "{}", remoteAgentDID: "{}" }},
            ))"#,
            payload_json, perspectiveUUID, remote_agent_did
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatus",
                {{ perspectiveUUID: "{}", status: {} }},
            ))"#,
            perspectiveUUID, status_json
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
        check_capability(&context.capabilities, &NEIGHBOURHOOD_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "neighbourhoodSetOnlineStatusU",
                {{ perspectiveUUID: "{}", status: {} }},
            ))"#,
            perspectiveUUID, status_json
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
        check_capability(&context.capabilities, &PERSPECTIVE_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAdd",
                {{ name: "{}" }},
            ))"#,
            name,
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
        status: Option<String>,
    ) -> FieldResult<LinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();

        let link_json = serde_json::to_string(&link)?;
        let status = match status {
            Some(status) => {
                if status != String::from("shared") && status != String::from("local") {
                    return Err(juniper::FieldError::new(
                        "Invalid status, must be either 'shared' or 'local'",
                        graphql_value!({ "invalid_status": status }),
                    ));
                }
                format!(r#""{}""#, status)
            }
            None => String::from("null"),
        };
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLink",
                {{ link: {}, uuid: "{}", status: {} }},
            ))"#,
            link_json, uuid, status,
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
        status: Option<String>,
    ) -> FieldResult<LinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let link_json = serde_json::to_string(&link)?;
        let status = match status {
            Some(status) => {
                if status != String::from("shared") && status != String::from("local") {
                    return Err(juniper::FieldError::new(
                        "Invalid status, must be either 'shared' or 'local'",
                        graphql_value!({ "invalid_status": status }),
                    ));
                }
                format!(r#""{}""#, status)
            }
            None => String::from("null"),
        };
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinkExpression",
                {{ link: {}, uuid: "{}", status: {} }},
            ))"#,
            link_json, uuid, status,
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
        status: Option<String>,
    ) -> FieldResult<Vec<LinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let links_json = serde_json::to_string(&links)?;
        let status = match status {
            Some(status) => {
                if status != String::from("shared") && status != String::from("local") {
                    return Err(juniper::FieldError::new(
                        "Invalid status, must be either 'shared' or 'local'",
                        graphql_value!({ "invalid_status": status }),
                    ));
                }
                format!(r#""{}""#, status)
            }
            None => String::from("null"),
        };
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveAddLinks",
                {{ links: {}, uuid: "{}", status: {} }},
            ))"#,
            links_json, uuid, status,
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
        status: Option<String>,
    ) -> FieldResult<LinkExpressionMutations> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let mutations_json = serde_json::to_string(&mutations)?;
        let status = match status {
            Some(status) => {
                if status != String::from("shared") && status != String::from("local") {
                    return Err(juniper::FieldError::new(
                        "Invalid status, must be either 'shared' or 'local'",
                        graphql_value!({ "invalid_status": status }),
                    ));
                }
                format!(r#""{}""#, status)
            }
            None => String::from("shared"),
        };
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveLinkMutations",
                {{ mutations: {}, uuid: "{}", status: {} }},
            ))"#,
            mutations_json, uuid, status,
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
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectivePublishSnapshot",
                {{ uuid: "{}" }},
            ))"#,
            uuid,
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
        check_capability(
            &context.capabilities,
            &perspective_delete_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemove",
                {{ uuid: "{}" }},
            ))"#,
            uuid,
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
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let link_json = serde_json::to_string(&link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLink",
                {{ link: {}, uuid: "{}" }},
            ))"#,
            link_json, uuid,
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
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let links_json = serde_json::to_string(&links)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveRemoveLinks",
                {{ links: {}, uuid: "{}" }},
            ))"#,
            links_json, uuid,
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
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveUpdate",
                {{ name: "{}", uuid: "{}" }},
            ))"#,
            name, uuid,
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
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let new_link_json = serde_json::to_string(&new_link)?;
        let old_link_json = serde_json::to_string(&old_link)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "perspectiveUpdateLink",
                {{ newLink: {}, oldLink: {}, uuid: "{}" }},
            ))"#,
            new_link_json, old_link_json, uuid,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<LinkExpression> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn perspective_add_sdna(
        &self,
        context: &RequestContext,
        uuid: String,
        name: String,
        sdna_code: String,
        sdna_type: String,
    ) -> FieldResult<bool> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut js = context.js_handle.clone();
        let sdna_literal = Literal::from_string(sdna_code).to_url()?;
        let script = format!(
            r#"JSON.stringify(
                await core.callResolver(
                    "Mutation",
                    "perspectiveAddSdna",
                    {{ uuid: "{}", name: "{}", sdnaCode: "{}", sdnaType: "{}" }},
                )
            )"#,
            uuid, name, sdna_literal, sdna_type,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_add_friends(
        &self,
        context: &RequestContext,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddFriends",
                {{ dids: {} }},
            ))"#,
            dids_json,
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
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY,
        )?;
        let mut js = context.js_handle.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddKnownLinkLanguageTemplates",
                {{ addresses: {} }},
            ))"#,
            addresses_json,
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
        check_capability(&context.capabilities, &RUNTIME_MESSAGES_CREATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let message_json = serde_json::to_string(&message)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeFriendSendMessage",
                {{ did: "{}", message: {} }},
            ))"#,
            did, message_json,
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
        check_capability(
            &context.capabilities,
            &RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY,
        )?;

        let agent_infos = agent_infos_from_str(agent_infos.as_str())?;
        log::info!("Adding HC agent infos: {:?}", agent_infos);

        get_holochain_service()
            .await
            .add_agent_infos(agent_infos)
            .await
            .map_err(|e| {
                log::error!("Failed to add agent infos: {:?}", e);
                e
            })?;

        Ok(true)
    }

    async fn runtime_open_link(&self, context: &RequestContext, url: String) -> FieldResult<bool> {
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeOpenLink",
                {{ url: "{}" }},
            ))"#,
            url,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }

    async fn runtime_quit(&self, context: &RequestContext) -> FieldResult<bool> {
        check_capability(&context.capabilities, &RUNTIME_QUIT_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeQuit",
            ))"#,
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
        check_capability(&context.capabilities, &RUNTIME_FRIENDS_DELETE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let dids_json = serde_json::to_string(&dids)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveFriends",
                {{ dids: {} }},
            ))"#,
            dids_json,
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
        check_capability(
            &context.capabilities,
            &RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY,
        )?;
        let mut js = context.js_handle.clone();
        let addresses_json = serde_json::to_string(&addresses)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeRemoveKnownLinkLanguageTemplates",
                {{ addresses: {} }},
            ))"#,
            addresses_json,
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
        check_capability(&context.capabilities, &RUNTIME_MY_STATUS_UPDATE_CAPABILITY)?;
        let mut js = context.js_handle.clone();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeSetStatus",
                {{ status: {} }},
            ))"#,
            status_json,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<bool> = serde_json::from_str(&result)?;
        result.get_graphql_result()
    }
}
