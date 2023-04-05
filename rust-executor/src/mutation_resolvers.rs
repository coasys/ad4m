use juniper::{graphql_object, graphql_value, FieldError, FieldResult};

use crate::graphql_types::*;

pub struct Mutation;

#[graphql_object]
impl Mutation {
    fn add_trusted_agents(&self, agents: Vec<String>) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_add_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_delete_entanglement_proofs(
        &self,
        proofs: Vec<EntanglementProofInput>,
    ) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_entanglement_proof_pre_flight(
        &self,
        device_key: String,
        device_key_type: String,
    ) -> FieldResult<EntanglementProof> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_generate(&self, passphrase: String) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_generate_jwt(&self, rand: String, request_id: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_import(
        &self,
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

    fn agent_lock(&self, passphrase: String) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_permit_capability(&self, auth: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_remove_app(&self, request_id: String) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_request_capability(&self, auth_info: AuthInfoInput) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_revoke_token(&self, request_id: String) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_sign_message(&self, message: String) -> FieldResult<AgentSignature> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_unlock(&self, passphrase: String) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_update_direct_message_language(
        &self,
        direct_message_language: String,
    ) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_update_public_perspective(&self, perspective: PerspectiveInput) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn delete_trusted_agents(&self, agents: Vec<String>) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_create(&self, content: String, language_address: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_interact(
        &self,
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
        language_meta: LanguageMetaInput,
        language_path: String,
    ) -> FieldResult<LanguageMeta> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_remove(&self, address: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_write_settings(
        &self,
        language_address: String,
        settings: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_join_from_url(&self, url: String) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_publish_from_perspective(
        &self,
        link_language: String,
        meta: PerspectiveInput,
        perspective_uuid: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_broadcast(
        &self,
        payload: PerspectiveInput,
        perspective_uuid: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_broadcast_u(
        &self,
        payload: PerspectiveUnsignedInput,
        perspective_uuid: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_signal(
        &self,
        payload: PerspectiveInput,
        perspective_uuid: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_send_signal_u(
        &self,
        payload: PerspectiveUnsignedInput,
        perspective_uuid: String,
        remote_agent_did: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_set_online_status(
        &self,
        perspective_uuid: String,
        status: PerspectiveInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_set_online_status_u(
        &self,
        perspective_uuid: String,
        status: PerspectiveUnsignedInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add(&self, name: String) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add_link(&self, link: LinkInput, uuid: String) -> FieldResult<LinkExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_add_link_expression(
        &self,
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
        mutations: LinkMutations,
        uuid: String,
    ) -> FieldResult<LinkExpressionMutations> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_publish_snapshot(&self, uuid: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_remove(&self, uuid: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_remove_link(
        &self,
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
        links: Vec<LinkExpressionInput>,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_update(&self, name: String, uuid: String) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_update_link(
        &self,
        new_link: LinkInput,
        old_link: LinkExpressionInput,
        uuid: String,
    ) -> FieldResult<LinkExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_add_friends(&self, dids: Vec<String>) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_add_known_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friend_send_message(
        &self,
        did: String,
        message: PerspectiveInput,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_hc_add_agent_infos(&self, agent_infos: String) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_open_link(&self, url: String) -> FieldResult<bool> {
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

    fn runtime_remove_friends(&self, dids: Vec<String>) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_remove_known_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_set_status(&self, status: PerspectiveInput) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }
}
