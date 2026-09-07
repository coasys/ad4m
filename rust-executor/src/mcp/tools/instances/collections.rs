//! `instance_add_to_collection` — collection membership on any subject class.

use super::{
    class_properties, coerce_scalar, describe_value, error_json, fetch_instance, find_property,
    not_found, pretty, resolve_class, validation_failure, Ad4mMcpHandler, ValidationError,
};
use crate::mcp::shacl;
use crate::types::{Link, LinkStatus};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

/// Parameters for adding an item to a collection property
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceAddToCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the instance that owns the collection
    pub class_name: String,
    /// URI of the instance that owns the collection
    pub base_uri: String,
    /// Collection property name as listed under `collections` by describe_perspective
    pub collection: String,
    /// URI of the item to add (typically another instance's `id`)
    pub item_uri: String,
}

impl Ad4mMcpHandler {
    /// Add an item to a collection property of an instance.
    #[tool(
        description = "Add an item to a collection property of an instance (e.g. add a Message to a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the URI of the item (usually another instance's id). Adding the same item twice is a no-op."
    )]
    pub async fn instance_add_to_collection(
        &self,
        params: Parameters<InstanceAddToCollectionParams>,
    ) -> String {
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

        let infos = class_properties(&shape);
        let Some(info) = find_property(&infos, &p.collection) else {
            let names: Vec<&str> = infos
                .iter()
                .filter(|i| i.collection() && i.writable())
                .map(|i| i.name())
                .collect();
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: p.collection.clone(),
                    problem: format!(
                        "unknown collection on class '{}'. Collections: {}",
                        class_name,
                        if names.is_empty() {
                            "(none)".to_string()
                        } else {
                            names.join(", ")
                        }
                    ),
                    expected_type: None,
                    cardinality: None,
                    received: None,
                }],
            );
        };
        if !info.collection() {
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: info.name().to_string(),
                    problem: "is a single-valued property, not a collection — use \
                              instance_update to set it"
                        .to_string(),
                    expected_type: Some(info.type_name()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: Some(describe_value(&Value::String(p.item_uri.clone()))),
                }],
            );
        }
        if let Some(reason) = info.read_only_reason() {
            return validation_failure(
                &class_name,
                &[ValidationError {
                    property: info.name().to_string(),
                    problem: format!("read-only: {reason}"),
                    expected_type: Some(info.type_name()),
                    cardinality: Some(info.cardinality_text().to_string()),
                    received: None,
                }],
            );
        }
        if let Err(e) = coerce_scalar(info, &Value::String(p.item_uri.clone())) {
            return validation_failure(&class_name, &[e]);
        }

        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(_)) => {}
            Ok(None) => return not_found(&class_name, &p.base_uri),
            Err(e) => return error_json(format!("Error reading {class_name} instance: {e}")),
        }

        let predicate = match shacl::resolve_property_predicate(
            &perspective,
            &class_name,
            &info.name(),
        )
        .await
        {
            Ok(pred) => pred,
            Err(e) => {
                return error_json(format!(
                    "Error resolving collection '{}': {}",
                    info.name(),
                    e
                ))
            }
        };
        let target = Self::create_property_expression(
            &perspective,
            &class_name,
            &info.name(),
            &p.item_uri,
            &agent_context,
        )
        .await;
        let link = Link {
            source: p.base_uri.clone(),
            predicate: Some(predicate),
            target,
        };
        match perspective
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => pretty(&json!({
                "success": true,
                "class_name": class_name,
                "base_uri": p.base_uri,
                "collection": info.name(),
                "item_uri": p.item_uri,
            })),
            Err(e) => error_json(format!(
                "Error adding to collection '{}': {:#}",
                info.name(),
                e
            )),
        }
    }
}
