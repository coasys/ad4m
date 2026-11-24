//! `instance_update` + `instance_remove` — mutate or delete an existing
//! instance of any subject class.

use super::{common_error, error_json, pretty, Ad4mMcpHandler};
use crate::perspectives::instances::{self as shared, CascadeStep, InstanceError, WriteStep};
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
        match shared::update(
            &mut perspective,
            &agent_context,
            shared::InstanceUpdate {
                class_name: p.class_name.clone(),
                base_uri: p.base_uri.clone(),
                properties: p.properties.clone(),
                batch_id: None,
            },
        )
        .await
        {
            Ok(updated) => pretty(&json!({
                "success": true,
                "class_name": updated.class_name,
                "base_uri": updated.base_uri,
                "updated_properties": updated.updated_properties,
            })),
            Err(e) => update_error(&e),
        }
    }

    /// Remove an instance and every link touching it.
    #[tool(
        description = "Delete an instance of a subject class: removes all its property links, its type markers, and every inbound link from other instances (e.g. collection membership). Refuses if no instance of class_name exists at base_uri, so a wrong class or URI cannot delete something else. Through an abstract class, runs the destructor of each of the record's classes it reads instead. ⚠️ Irreversible."
    )]
    pub async fn instance_remove(&self, params: Parameters<InstanceRemoveParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        match shared::remove(
            &mut perspective,
            &agent_context,
            shared::InstanceRemove {
                class_name: p.class_name.clone(),
                base_uri: p.base_uri.clone(),
                batch_id: None,
            },
        )
        .await
        {
            Ok(removed) => pretty(&json!({
                "success": true,
                "class_name": removed.class_name,
                "deleted": removed.base_uri,
                "links_removed": removed.links_removed,
            })),
            Err(e) => remove_error(&e),
        }
    }
}

/// The class's text when its shape or the instance could not be read; `None` for other steps.
fn read_failure(
    class_name: &str,
    step: &WriteStep,
    error: &deno_core::anyhow::Error,
) -> Option<String> {
    match step {
        WriteStep::ReadShape => Some(error_json(format!(
            "Error reading {class_name}'s shape: {error:#}"
        ))),
        WriteStep::ReadInstance => Some(error_json(format!(
            "Error reading {class_name} instance: {error}"
        ))),
        _ => None,
    }
}

/// `instance_update`'s text for a refused or failed update.
fn update_error(error: &InstanceError) -> String {
    if let Some(text) = common_error(error) {
        return text;
    }
    match error {
        InstanceError::EmptyProperties => {
            error_json("properties is empty — pass at least one property to set.")
        }
        InstanceError::Failed {
            class_name,
            step,
            error,
        } => read_failure(class_name, step, error).unwrap_or_else(|| {
            error_json(match step {
                WriteStep::Commit => format!("Error committing update: {error:#}"),
                _ => format!("Error updating {class_name} instance (batch abandoned): {error:#}"),
            })
        }),
        other => error_json(format!("{other:?}")),
    }
}

/// `instance_remove`'s text for a refused or failed remove.
fn remove_error(error: &InstanceError) -> String {
    if let Some(text) = common_error(error) {
        return text;
    }
    match error {
        InstanceError::Failed {
            class_name,
            step,
            error,
        } => read_failure(class_name, step, error).unwrap_or_else(|| {
            error_json(format!("Error removing {class_name} instance: {error:#}"))
        }),
        // The cascade is not batched, so on failure the links already removed stay removed:
        // say so, with the count, instead of reporting a deletion that did not fully happen.
        InstanceError::PartiallyRemoved {
            class_name,
            base_uri,
            removed,
            step,
            error,
        } => {
            let failure = match step {
                CascadeStep::List { uri } => {
                    format!("listing the links of '{uri}' failed: {error:#}")
                }
                CascadeStep::Remove {
                    source,
                    predicate,
                    target,
                } => format!(
                    "removing link {} -[{}]-> {} failed: {:#}",
                    source,
                    predicate.as_deref().unwrap_or(""),
                    target,
                    error
                ),
            };
            pretty(&json!({
                "error": format!(
                    "Error removing {} instance at '{}': {}. {} link(s) had already been \
                     removed, so the instance may be partially deleted — inspect what is \
                     left with query_links (source or target = '{}') and retry.",
                    class_name, base_uri, failure, removed, base_uri
                ),
                "class_name": class_name,
                "base_uri": base_uri,
                "links_removed": removed,
            }))
        }
        other => error_json(format!("{other:?}")),
    }
}
