//! `instance_create` — create an instance of any subject class.

use super::{
    check_setters, error_json, fetch_instance, generate_instance_uri, link_target, pretty,
    resolve_class, subject_class, validate_properties, validation_failure, Ad4mMcpHandler,
    WriteMode, HAS_CHILD,
};
use crate::mcp::shacl;
use crate::types::{Link, LinkStatus};
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
    /// Optional parent URI. The new instance is additionally linked as an
    /// `ad4m://has_child` child of this node (e.g. a Message inside a Channel).
    pub parent: Option<String>,
}

impl Ad4mMcpHandler {
    /// Create an instance of any subject class.
    #[tool(
        description = "Create a new instance of a subject class. class_name is one of the class names from describe_perspective; properties is a JSON object of property values (single JSON value per scalar property, array of item URIs per collection). Required properties must be present; every value is validated against the class schema (property, expected type, cardinality are named on rejection). Optionally pass parent to also link the instance as an ad4m://has_child child of another instance (e.g. a Message into a Channel). Returns the new instance's base_uri (its id). Example: instance_create(perspective_id, class_name='Message', properties={\"body\": \"Hello\"}, parent='<channel uri>')."
    )]
    pub async fn instance_create(&self, params: Parameters<InstanceCreateParams>) -> String {
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

        let empty = Map::new();
        let props = p.properties.as_ref().unwrap_or(&empty);
        let validated = match validate_properties(&shape, props, WriteMode::Create) {
            Ok(v) => v,
            Err(errors) => return validation_failure(&class_name, &errors),
        };
        let setter_errors = check_setters(&perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return validation_failure(&class_name, &setter_errors);
        }

        // Normalize once: a bare caller-supplied id ("my-channel") becomes a
        // literal URI here, and this single value is used for the subject,
        // the collection links AND the parent link — otherwise they would
        // target different nodes.
        let base_uri = match p.base_uri.as_deref() {
            Some(uri) if !uri.trim().is_empty() => link_target(uri.trim()),
            _ => generate_instance_uri(),
        };
        if let Ok(Some(_)) = fetch_instance(&perspective, &class_name, &base_uri).await {
            return error_json(format!(
                "A {class_name} instance already exists at '{base_uri}'. Use instance_update to \
                 change it, or omit base_uri to create a new instance with a generated id."
            ));
        }

        // Resolve every collection predicate BEFORE creating the subject, so
        // a bad collection name fails the whole call cleanly instead of
        // leaving a live instance with partial collection membership.
        let mut collection_predicates: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        for (collection, _) in &validated.collections {
            match shacl::resolve_property_predicate(&perspective, &class_name, collection).await {
                Ok(pred) => {
                    collection_predicates.insert(collection.clone(), pred);
                }
                Err(e) => {
                    return error_json(format!(
                        "Could not resolve collection '{collection}' on {class_name}: {e} — \
                         nothing was created"
                    ))
                }
            }
        }

        let initial_values = if validated.scalars.is_empty() {
            None
        } else {
            Some(Value::Object(validated.scalars.clone()))
        };

        // One batch for the whole create — subject, collection links, and the
        // parent link land atomically or not at all (same reasoning as
        // instance_update: no partially-constructed instance is ever
        // observable, by peers or by a failed call's aftermath).
        let batch_id = perspective.create_batch().await;
        if let Err(e) = perspective
            .create_subject(
                subject_class(&class_name),
                base_uri.clone(),
                initial_values,
                Some(batch_id.clone()),
                &agent_context,
            )
            .await
        {
            return error_json(format!(
                "Error creating {class_name} instance (nothing was created): {e:#}"
            ));
        }

        // Collections: one link per item, same path as instance_add_to_collection.
        let mut collections_set: Map<String, Value> = Map::new();
        for (collection, items) in &validated.collections {
            let predicate = collection_predicates[collection].clone();
            let mut added = Vec::with_capacity(items.len());
            for item in items {
                let item_str = match item {
                    Value::String(s) => s.clone(),
                    other => other.to_string(),
                };
                let target = Self::create_property_expression(
                    &perspective,
                    &class_name,
                    collection,
                    &item_str,
                    &agent_context,
                )
                .await;
                let link = Link {
                    source: base_uri.clone(),
                    predicate: Some(predicate.clone()),
                    target,
                };
                if let Err(e) = perspective
                    .add_link(
                        link,
                        LinkStatus::Shared,
                        Some(batch_id.clone()),
                        &agent_context,
                    )
                    .await
                {
                    return error_json(format!(
                        "Failed to add '{item_str}' to collection '{collection}' — nothing was \
                         created (batch abandoned): {e:#}"
                    ));
                }
                added.push(Value::String(item_str));
            }
            collections_set.insert(collection.clone(), Value::Array(added));
        }

        let parent = p.parent.as_deref().filter(|s| !s.trim().is_empty());
        if let Some(parent) = parent {
            let link = Link {
                source: link_target(parent.trim()),
                predicate: Some(HAS_CHILD.to_string()),
                target: base_uri.clone(),
            };
            if let Err(e) = perspective
                .add_link(
                    link,
                    LinkStatus::Shared,
                    Some(batch_id.clone()),
                    &agent_context,
                )
                .await
            {
                return error_json(format!(
                    "Failed to link to parent '{parent}' — nothing was created (batch \
                     abandoned): {e:#}"
                ));
            }
        }

        if let Err(e) = perspective.commit_batch(batch_id, &agent_context).await {
            return error_json(format!("Error committing create: {e:#}"));
        }

        let mut result = json!({
            "created": true,
            "perspective_id": p.perspective_id,
            "class_name": class_name,
            "base_uri": base_uri,
            "properties_set": validated.scalars.keys().cloned().collect::<Vec<_>>(),
        });
        if !collections_set.is_empty() {
            result["collections_set"] = Value::Object(collections_set);
        }
        if let Some(parent) = parent {
            result["parent"] = json!(parent);
            result["added_to_parent"] = json!(true);
        }

        pretty(&result)
    }
}
