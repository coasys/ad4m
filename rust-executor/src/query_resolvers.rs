use juniper::{graphql_object, graphql_value, FieldError, FieldResult};

use crate::graphql_types::*;

pub struct Query;

#[graphql_object]
impl Query {
    fn agent(&self) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_by_did(&self, did: String) -> FieldResult<Option<Agent>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_get_apps(&self) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_get_entanglement_proofs(&self) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_is_locked(&self) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_status(&self) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression(&self, url: String) -> FieldResult<ExpressionRendered> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_interactions(&self, url: String) -> FieldResult<Vec<InteractionMeta>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_many(&self, urls: Vec<String>) -> FieldResult<Vec<ExpressionRendered>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_raw(&self, url: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn get_trusted_agents(&self) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language(&self, address: String) -> FieldResult<LanguageHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_meta(&self, address: String) -> FieldResult<LanguageMeta> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_source(&self, address: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn languages(&self, filter: Option<String>) -> FieldResult<Vec<LanguageHandle>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_has_telepresence_adapter(
        &self,
        perspective_uuid: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_online_agents(
        &self,
        perspective_uuid: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_other_agents(&self, perspective_uuid: String) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective(&self, uuid: String) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_query_links(
        &self,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_query_prolog(&self, query: String, uuid: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_snapshot(&self, uuid: String) -> FieldResult<Perspective> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspectives(&self) -> FieldResult<Vec<PerspectiveHandle>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friend_status(&self, did: String) -> FieldResult<PerspectiveExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friends(&self) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_hc_agent_infos(&self) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_info(&self) -> FieldResult<RuntimeInfo> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_known_link_language_templates(&self) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_message_inbox(
        &self,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_message_outbox(&self, filter: Option<String>) -> FieldResult<Vec<SentMessage>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_verify_string_signed_by_did(
        &self,
        data: String,
        did: String,
        did_signing_key_id: String,
        signed_data: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }
}
