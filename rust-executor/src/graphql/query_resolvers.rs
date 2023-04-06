#![allow(non_snake_case)]
#![allow(unused_variables)]
use juniper::{graphql_object, graphql_value, FieldError, FieldResult};

use crate::js_core::JsCoreHandle;

use super::graphql_types::*;

pub struct Query;

#[graphql_object(context = JsCoreHandle)]
impl Query {
    fn agent(&self, context: &JsCoreHandle) -> FieldResult<Agent> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    #[graphql(name = "agentByDID")]
    fn agent_by_did(&self, context: &JsCoreHandle, did: String) -> FieldResult<Option<Agent>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_get_apps(&self, context: &JsCoreHandle) -> FieldResult<Vec<Apps>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_get_entanglement_proofs(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<EntanglementProof>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_is_locked(&self, context: &JsCoreHandle) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn agent_status(&self, context: &JsCoreHandle) -> FieldResult<AgentStatus> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression(&self, context: &JsCoreHandle, url: String) -> FieldResult<ExpressionRendered> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_interactions(
        &self,
        context: &JsCoreHandle,
        url: String,
    ) -> FieldResult<Vec<InteractionMeta>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_many(
        &self,
        context: &JsCoreHandle,
        urls: Vec<String>,
    ) -> FieldResult<Vec<ExpressionRendered>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn expression_raw(&self, context: &JsCoreHandle, url: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn get_trusted_agents(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language(&self, context: &JsCoreHandle, address: String) -> FieldResult<LanguageHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_meta(&self, context: &JsCoreHandle, address: String) -> FieldResult<LanguageMeta> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn language_source(&self, context: &JsCoreHandle, address: String) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn languages(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<LanguageHandle>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_has_telepresence_adapter(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
    ) -> FieldResult<bool> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_online_agents(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
    ) -> FieldResult<Vec<OnlineAgent>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn neighbourhood_other_agents(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective(&self, context: &JsCoreHandle, uuid: String) -> FieldResult<PerspectiveHandle> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_query_links(
        &self,
        context: &JsCoreHandle,
        query: LinkQuery,
        uuid: String,
    ) -> FieldResult<Vec<LinkExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_query_prolog(
        &self,
        context: &JsCoreHandle,
        query: String,
        uuid: String,
    ) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspective_snapshot(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> FieldResult<Perspective> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn perspectives(&self, context: &JsCoreHandle) -> FieldResult<Vec<PerspectiveHandle>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friend_status(
        &self,
        context: &JsCoreHandle,
        did: String,
    ) -> FieldResult<PerspectiveExpression> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_friends(&self, context: &JsCoreHandle) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_hc_agent_infos(&self, context: &JsCoreHandle) -> FieldResult<String> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_info(&self, context: &JsCoreHandle) -> FieldResult<RuntimeInfo> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_known_link_language_templates(
        &self,
        context: &JsCoreHandle,
    ) -> FieldResult<Vec<String>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_message_inbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<PerspectiveExpression>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_message_outbox(
        &self,
        context: &JsCoreHandle,
        filter: Option<String>,
    ) -> FieldResult<Vec<SentMessage>> {
        Err(FieldError::new(
            "Not implemented",
            graphql_value!({ "Not implemented": true }),
        ))
    }

    fn runtime_verify_string_signed_by_did(
        &self,
        context: &JsCoreHandle,
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
