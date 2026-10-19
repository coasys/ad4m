//! `instance_update` + `instance_remove` — mutate or delete an existing
//! instance of any subject class.

use super::{
    check_setters, error_json, fetch_instance, instance_uri, not_found, pretty, resolve_class,
    subject_class, validate_properties, validation_failure, Ad4mMcpHandler, WriteMode,
};
use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

/// Parameters for updating scalar properties of an instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceUpdateParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// URI of the instance to update
    pub base_uri: String,
    /// Property values to set, keyed by property name. Only the given
    /// properties change; each must be a single-valued property of the class.
    pub properties: Map<String, Value>,
}

/// Parameters for removing an instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceRemoveParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the instance
    pub class_name: String,
    /// URI of the instance to remove
    pub base_uri: String,
}

impl Ad4mMcpHandler {
    /// Update single-valued properties of an instance.
    #[tool(
        description = "Set one or more single-valued properties on an existing instance. class_name is one of the class names from describe_perspective; properties is a JSON object of the values to change (unlisted properties are untouched). Values are validated against the class schema — the error names the property, expected type and cardinality. Collections cannot be set here: use instance_add_to_collection. Example: instance_update(perspective_id, class_name='Task', base_uri='<id>', properties={\"status\": \"done\"})."
    )]
    pub async fn instance_update(&self, params: Parameters<InstanceUpdateParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        if p.properties.is_empty() {
            return error_json("properties is empty — pass at least one property to set.");
        }

        let validated = match validate_properties(&shape, &p.properties, WriteMode::Update) {
            Ok(v) => v,
            Err(errors) => return validation_failure(&class_name, &errors),
        };
        let setter_errors = check_setters(&perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return validation_failure(&class_name, &setter_errors);
        }

        let base_uri = match instance_uri("base_uri", &p.base_uri) {
            Ok(uri) => uri,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        if let Err(e) = update_in_batch(
            &mut perspective,
            &class_name,
            &base_uri,
            validated.scalars.clone(),
            &agent_context,
        )
        .await
        {
            return error_json(e);
        }

        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "base_uri": base_uri,
            "updated_properties": validated.scalars.keys().cloned().collect::<Vec<_>>(),
        }))
    }

    /// Remove an instance and every link touching it.
    #[tool(
        description = "Delete an instance of a subject class: removes all its property links, its type markers, and every inbound link from other instances (e.g. collection membership). Refuses if no instance of class_name exists at base_uri, so a wrong class or URI cannot delete something else. Through an abstract class, runs the destructor of each of the record's classes it reads first; when the record has other classes it stays as them: only its links at the reached classes' predicates go, and its other links and the inbound links stay. ⚠️ Irreversible."
    )]
    pub async fn instance_remove(&self, params: Parameters<InstanceRemoveParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, _shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        let base_uri = match instance_uri("base_uri", &p.base_uri) {
            Ok(uri) => uri,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        // Success only when the delete and its whole cascade succeeded. The
        // cascade is not batched, so on failure the links already removed
        // stay removed: say so, with the count, instead of reporting a
        // deletion that did not fully happen.
        let removed = match perspective
            .remove_subject_cascading(&class_name, &base_uri, &agent_context)
            .await
        {
            Ok(removed) => removed,
            Err(failure) => {
                return pretty(&json!({
                    "error": format!(
                        "Error removing {} instance at '{}': {}. {} link(s) had already been \
                         removed, so the instance may be partially deleted — inspect what is \
                         left with query_links (source or target = '{}') and retry.",
                        class_name, base_uri, failure.error, failure.removed, base_uri
                    ),
                    "class_name": class_name,
                    "base_uri": base_uri,
                    "links_removed": failure.removed,
                }))
            }
        };
        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "deleted": base_uri,
            "links_removed": removed,
        }))
    }
}

/// Set `values` (validated, by their class's names) on the instance at
/// `base_uri` through `update_subject`, in one batch, so a peer never sees
/// a property's old value removed before its new one lands. The error is
/// the message to return.
pub(crate) async fn update_in_batch(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
    values: Map<String, Value>,
    agent_context: &AgentContext,
) -> Result<(), String> {
    let batch_id = perspective.create_batch().await;
    if let Err(e) = perspective
        .update_subject(
            subject_class(class_name),
            base_uri.to_string(),
            Value::Object(values),
            Some(batch_id.clone()),
            agent_context,
        )
        .await
    {
        perspective.discard_batch(&batch_id).await;
        return Err(format!(
            "Error updating {class_name} instance (batch abandoned): {e:#}"
        ));
    }
    perspective
        .commit_batch(batch_id, agent_context)
        .await
        .map(|_| ())
        .map_err(|e| format!("Error committing update: {e:#}"))
}
