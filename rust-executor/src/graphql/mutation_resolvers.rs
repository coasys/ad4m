#![allow(non_snake_case)]
use juniper::{graphql_object, graphql_value, FieldResult};

use crate::{perspectives::{add_perspective, get_perspective, perspective_instance::{PerspectiveInstance, SdnaType}, remove_perspective}, types::{DecoratedLinkExpression, LinkExpression, PerspectiveDiff}};

use super::graphql_types::*;
use crate::{agent::{self, capabilities::*}, holochain_service::{agent_infos_from_str, get_holochain_service}};
use ad4m_client::literal::Literal;
pub struct Mutation;

fn get_perspective_with_uuid_field_error(uuid: &String) -> FieldResult<PerspectiveInstance> {
    get_perspective(uuid).ok_or_else(|| juniper::FieldError::new(
        "Perspective not found",
        graphql_value!({ "uuid": uuid.clone() }),
    ))
}

fn link_status_from_input(status: Option<String>) -> Result<crate::types::LinkStatus, juniper::FieldError> {
    match status.as_ref().map(|s| s.as_str()) {
        Some("shared") => Ok(crate::types::LinkStatus::Shared),
        Some("local") => Ok(crate::types::LinkStatus::Local),
        None => Ok(crate::types::LinkStatus::Shared),
        _ => Err(juniper::FieldError::new(
            "Invalid status, must be either 'shared' or 'local'",
            graphql_value!({ "invalid_status": status }),
        )),
    }
}

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
        Ok(agent::AgentSignature::from_message(message)?.into())
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
        let handle = PerspectiveHandle::new_from_name(name.clone());
        add_perspective(handle.clone())?;
        Ok(handle)
    }

    async fn perspective_add_link(
        &self,
        context: &RequestContext,
        link: LinkInput,
        uuid: String,
        status: Option<String>,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;

        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        Ok(perspective.add_link(link.into(), link_status_from_input(status)?).await?)

    }

    async fn perspective_add_link_expression(
        &self,
        context: &RequestContext,
        link: LinkExpressionInput,
        uuid: String,
        status: Option<String>,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        let link = crate::types::LinkExpression::try_from(link)?;
        Ok(perspective.add_link_expression(link, link_status_from_input(status)?).await?)
    }

    async fn perspective_add_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkInput>,
        uuid: String,
        status: Option<String>,
    ) -> FieldResult<Vec<DecoratedLinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        Ok(perspective.add_links(
            links
                .into_iter()
                .map(|l| l.into())
                .collect(), 
            link_status_from_input(status)?
        ).await?)
    }

    async fn perspective_link_mutations(
        &self,
        context: &RequestContext,
        mutations: LinkMutations,
        uuid: String,
        status: Option<String>,
    ) -> FieldResult<DecoratedPerspectiveDiff> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        Ok(perspective.link_mutations(mutations, link_status_from_input(status)?).await?)
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
        unimplemented!()
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
        Ok(remove_perspective(&uuid).is_some())
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
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        let link = crate::types::LinkExpression::try_from(link)?;
        perspective.remove_link(link.into()).await?;
        Ok(true)
    }

    async fn perspective_remove_links(
        &self,
        context: &RequestContext,
        links: Vec<LinkExpressionInput>,
        uuid: String,
    ) -> FieldResult<Vec<DecoratedLinkExpression>> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        for link in links.into_iter() {
            let link = crate::types::LinkExpression::try_from(link)?;
            perspective.remove_link(link.into()).await?;
        }
        
        // TODO: change this return type. why should we return all the deleted links again?
        Ok(Vec::new())
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
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        let mut handle = perspective.persisted.as_ref().clone();
        handle.name = Some(name);
        perspective.update_from_handle(handle.clone());
        Ok(handle)
    }

    async fn perspective_update_link(
        &self,
        context: &RequestContext,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<DecoratedLinkExpression> {
        check_capability(
            &context.capabilities,
            &perspective_update_capability(vec![uuid.clone()]),
        )?;
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        Ok(perspective.update_link(LinkExpression::from_input_without_proof(old_link), new_link.into()).await?)
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
        let mut perspective = get_perspective_with_uuid_field_error(&uuid)?;
        let sdna_type = SdnaType::from_string(&sdna_type)
            .map_err(|e| juniper::FieldError::new(
                e,
                graphql_value!({ "invalid_sdna_type": sdna_type })
            ))?;
        perspective.add_sdna(name, sdna_code, sdna_type).await?;
        Ok(true)
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
