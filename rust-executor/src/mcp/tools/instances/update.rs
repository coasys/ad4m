//! `instance_update` + `instance_remove` — mutate or delete an existing
//! instance of any subject class.

use super::{
    check_setters, error_json, fetch_instance, not_found, pretty, remove_all_links_of,
    resolve_class, subject_class, validate_properties, validation_failure, Ad4mMcpHandler,
    WriteMode,
};
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

        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        // One batch for the whole update so remove-old + add-new per property
        // lands atomically (same reasoning as the per-class setters: without
        // it a peer can observe the property as unset in between).
        let batch_id = perspective.create_batch().await;
        if let Err(e) = perspective
            .update_subject(
                subject_class(&class_name),
                p.base_uri.clone(),
                Value::Object(validated.scalars.clone()),
                Some(batch_id.clone()),
                &agent_context,
            )
            .await
        {
            return error_json(format!(
                "Error updating {class_name} instance (batch abandoned): {e:#}"
            ));
        }
        if let Err(e) = perspective.commit_batch(batch_id, &agent_context).await {
            return error_json(format!("Error committing update: {e:#}"));
        }

        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "base_uri": p.base_uri,
            "updated_properties": validated.scalars.keys().cloned().collect::<Vec<_>>(),
        }))
    }

    /// Remove an instance and every link touching it.
    #[tool(
        description = "Delete an instance of a subject class: removes all its property links, its type markers, and every inbound link from other instances (e.g. collection membership). Refuses if no instance of class_name exists at base_uri, so a wrong class or URI cannot delete something else. ⚠️ Irreversible."
    )]
    pub async fn instance_remove(&self, params: Parameters<InstanceRemoveParams>) -> String {
        let p = &params.0;
        let (mut perspective, _agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let (class_name, _shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        let removed = remove_all_links_of(&mut perspective, &p.base_uri).await;
        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "deleted": p.base_uri,
            "links_removed": removed,
        }))
    }
}
