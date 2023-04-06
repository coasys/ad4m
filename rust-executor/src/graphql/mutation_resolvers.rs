#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, graphql_value, FieldError, FieldResult};

use crate::js_core::JsCoreHandle;

use super::graphql_types::*;

pub struct Mutation;

#[graphql_object(context = JsCoreHandle)]
impl Mutation {
    fn add_trusted_agents(
        &self,
        context: &JsCoreHandle,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_add_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_delete_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_entanglement_proof_pre_flight(
        &self,
        context: &JsCoreHandle,
        device_key: String,
        device_key_type: String,
    ) -> FieldResult<EntanglementProof> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_generate(
        &self,
        context: &JsCoreHandle,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_generate_jwt(
        &self,
        context: &JsCoreHandle,
        rand: String,
        request_id: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_import(
        &self,
        context: &JsCoreHandle,
        did: String,
        did_document: String,
        keystore: String,
        passphrase: String,
    ) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_lock(&self, context: &JsCoreHandle, passphrase: String) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_permit_capability(&self, context: &JsCoreHandle, auth: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_remove_app(
        &self,
        context: &JsCoreHandle,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_request_capability(
        &self,
        context: &JsCoreHandle,
        auth_info: AuthInfoInput,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_revoke_token(
        &self,
        context: &JsCoreHandle,
        request_id: String,
    ) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_sign_message(
        &self,
        context: &JsCoreHandle,
        message: String,
    ) -> FieldResult<AgentSignature> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_unlock(&self, context: &JsCoreHandle, passphrase: String) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_update_direct_message_language(
        &self,
        context: &JsCoreHandle,
        direct_message_language: String,
    ) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_update_public_perspective(
        &self,
        context: &JsCoreHandle,
        perspective: PerspectiveInput,
    ) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn delete_trusted_agents(
        &self,
        context: &JsCoreHandle,
        agents: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_create(
        &self,
        context: &JsCoreHandle,
        content: String,
        language_address: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_interact(
        &self,
        context: &JsCoreHandle,
        interaction_call: InteractionCall,
        url: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_apply_template_and_publish(
        &self,
        context: &JsCoreHandle,
        source_language_hash: String,
        template_data: String,
    ) -> FieldResult<LanguageRef> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_publish(
        &self,
        context: &JsCoreHandle,
        language_meta: LanguageMetaInput,
        language_path: String,
    ) -> FieldResult<LanguageMeta> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_remove(&self, context: &JsCoreHandle, address: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_write_settings(
        &self,
        context: &JsCoreHandle,
        language_address: String,
        settings: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_join_from_url(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_publish_from_perspective(
        &self,
        context: &JsCoreHandle,
        link_language: String,
        meta: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_broadcast(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_broadcast_u(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_signal(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_signal_u(
        &self,
        context: &JsCoreHandle,
        payload: PerspectiveUnsignedInput,
        perspectiveUUID: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_set_online_status(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_set_online_status_u(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
        status: PerspectiveUnsignedInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add(
        &self,
        context: &JsCoreHandle,
        name: String,
    ) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add_link(
        &self,
        context: &JsCoreHandle,
        link: LinkInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add_link_expression(
        &self,
        context: &JsCoreHandle,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add_links(
        &self,
        context: &JsCoreHandle,
        links: Vec<LinkInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_link_mutations(
        &self,
        context: &JsCoreHandle,
        mutations: LinkMutations,
        uuid: String,
    ) -> FieldResult<LinkExpressionMutations> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_publish_snapshot(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_remove(&self, context: &JsCoreHandle, uuid: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_remove_link(
        &self,
        context: &JsCoreHandle,
        link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_remove_links(
        &self,
        context: &JsCoreHandle,
        links: Vec<LinkExpressionInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_update(
        &self,
        context: &JsCoreHandle,
        name: String,
        uuid: String,
    ) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_update_link(
        &self,
        context: &JsCoreHandle,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_add_friends(
        &self,
        context: &JsCoreHandle,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_add_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friend_send_message(
        &self,
        context: &JsCoreHandle,
        did: String,
        message: PerspectiveInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_hc_add_agent_infos(
        &self,
        context: &JsCoreHandle,
        agent_infos: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_open_link(&self, context: &JsCoreHandle, url: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_quit(&self) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_remove_friends(
        &self,
        context: &JsCoreHandle,
        dids: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_remove_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_set_status(
        &self,
        context: &JsCoreHandle,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }
}
