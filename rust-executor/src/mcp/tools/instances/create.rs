//! `instance_create` — create an instance of any subject class.

use super::{common_error, error_json, pretty, Ad4mMcpHandler};
use crate::perspectives::instances::{self as shared, InstanceError, WriteStep};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

/// Parameters for creating a subject instance of any class
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceCreateParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name exactly as listed by describe_perspective (e.g. "Message")
    pub class_name: String,
    /// Property values keyed by property name. Scalar properties take one JSON
    /// value (string / number / boolean, matching the property's type);
    /// collection properties take an array of item URIs. Required properties
    /// must be present.
    pub properties: Option<Map<String, Value>>,
    /// Optional URI for the new instance. A random one is generated when omitted.
    pub base_uri: Option<String>,
    /// Optional parent URI. The new instance is also put under this node (e.g.
    /// a Message inside a Channel), the way the node holds children: its
    /// class's own `children` relation when it has one (a block's
    /// `we://child`), else `ad4m://has_child`.
    pub parent: Option<String>,
}

impl Ad4mMcpHandler {
    /// Create an instance of any subject class.
    #[tool(
        description = "Create a new instance of a subject class. class_name is one of the class names from describe_perspective; properties is a JSON object of property values (single JSON value per scalar property, array of item URIs per collection). Required properties must be present; every value is validated against the class schema (property, expected type, cardinality are named on rejection). Optionally pass parent to also put the instance under another node (e.g. a Message into a Channel): through the parent class's own children relation when it has one (a block's we://child), else with an ad4m://has_child link. Returns the new instance's base_uri (its id). Example: instance_create(perspective_id, class_name='Message', properties={\"body\": \"Hello\"}, parent='<channel uri>')."
    )]
    pub async fn instance_create(&self, params: Parameters<InstanceCreateParams>) -> String {
        let p = &params.0;
        let (mut perspective, agent_context) =
            match self.get_writable_perspective(&p.perspective_id).await {
                Ok(v) => v,
                Err(e) => return e,
            };
        let created = match shared::create(
            &mut perspective,
            &agent_context,
            shared::InstanceCreate {
                class_name: p.class_name.clone(),
                properties: p.properties.clone(),
                base_uri: p.base_uri.clone(),
                parent: p.parent.clone(),
                batch_id: None,
            },
        )
        .await
        {
            Ok(created) => created,
            Err(e) => return create_error(&e),
        };

        let mut result = json!({
            "created": true,
            "perspective_id": p.perspective_id,
            "class_name": created.class_name,
            "base_uri": created.base_uri,
            "properties_set": created.properties_set,
        });
        if !created.collections_set.is_empty() {
            let collections_set: Map<String, Value> = created
                .collections_set
                .into_iter()
                .map(|c| (c.collection, json!(c.items)))
                .collect();
            result["collections_set"] = Value::Object(collections_set);
        }
        if let Some(parent) = created.parent {
            result["parent"] = json!(parent);
            result["added_to_parent"] = json!(true);
        }

        pretty(&result)
    }
}

/// `instance_create`'s text for a refused or failed create.
fn create_error(error: &InstanceError) -> String {
    if let Some(text) = common_error(error) {
        return text;
    }
    match error {
        InstanceError::AlreadyExists {
            class_name,
            base_uri,
        } => error_json(format!(
            "A {class_name} instance already exists at '{base_uri}'. Use instance_update \
             to change it, or omit base_uri to create a new instance with a generated id."
        )),
        InstanceError::Failed {
            class_name,
            step,
            error,
        } => error_json(match step {
            WriteStep::CheckExisting { base_uri } => format!(
                "Could not check whether a {class_name} instance already exists at \
                 '{base_uri}': {error} — nothing was created."
            ),
            WriteStep::ResolveCollection { collection } => format!(
                "Could not resolve collection '{collection}' on {class_name}: {error} — \
                 nothing was created"
            ),
            WriteStep::CreateSubject => {
                format!("Error creating {class_name} instance (nothing was created): {error:#}")
            }
            WriteStep::AddCollectionItem { collection, item } => format!(
                "Failed to add '{item}' to collection '{collection}' — nothing was \
                 created (batch abandoned): {error:#}"
            ),
            WriteStep::AddToParent { parent } => format!(
                "Failed to link to parent '{parent}' — nothing was created (batch \
                 abandoned): {error:#}"
            ),
            WriteStep::Commit => format!("Error committing create: {error:#}"),
            other => format!("Error creating {class_name} instance ({other:?}): {error:#}"),
        }),
        other => error_json(format!("{other:?}")),
    }
}
