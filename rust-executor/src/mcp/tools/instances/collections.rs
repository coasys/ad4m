//! `instance_add_to_collection` + `instance_remove_from_collection` —
//! collection membership on any subject class.

use super::{
    class_properties, coerce_scalar, decoded_literal, describe_value, error_json, fetch_instance,
    find_property, instance_uri, link_target, not_found, pretty, resolve_class, subject_class,
    typed_value, validation_failure, Ad4mMcpHandler, PropView, ValidationError,
};
use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
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

/// Parameters for removing an item from a collection property
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceRemoveFromCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the instance that owns the collection
    pub class_name: String,
    /// URI of the instance that owns the collection
    pub base_uri: String,
    /// Collection property name as listed under `collections` by describe_perspective
    pub collection: String,
    /// URI of the item to remove, exactly as instance_get lists it in the collection
    pub item_uri: String,
}

/// Resolve `collection` to a writable collection property of the class, or
/// return the validation-failure payload that explains why it is not one
/// (unknown name, single-valued property, read-only relation).
fn resolve_collection<'a, 'p>(
    class_name: &str,
    infos: &'a [PropView<'p>],
    collection: &str,
    item_uri: &str,
) -> Result<&'a PropView<'p>, String> {
    let Some(info) = find_property(infos, collection) else {
        let names: Vec<&str> = infos
            .iter()
            .filter(|i| i.collection() && i.writable())
            .map(|i| i.name())
            .collect();
        return Err(validation_failure(
            class_name,
            &[ValidationError {
                property: collection.to_string(),
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
        ));
    };
    if !info.collection() {
        return Err(validation_failure(
            class_name,
            &[ValidationError {
                property: info.name().to_string(),
                problem: "is a single-valued property, not a collection — use \
                          instance_update to set it"
                    .to_string(),
                expected_type: Some(info.type_name()),
                cardinality: Some(info.cardinality_text().to_string()),
                received: Some(describe_value(&Value::String(item_uri.to_string()))),
            }],
        ));
    }
    if let Some(reason) = info.read_only_reason() {
        return Err(validation_failure(
            class_name,
            &[ValidationError {
                property: info.name().to_string(),
                problem: format!("read-only: {reason}"),
                expected_type: Some(info.type_name()),
                cardinality: Some(info.cardinality_text().to_string()),
                received: None,
            }],
        ));
    }
    Ok(info)
}

/// Whether a stored collection link target denotes `wanted`. The target may
/// be the item URI itself (references), the literal-encoded form of a bare
/// value, or a signed literal envelope whose payload is the value — accept
/// all three spellings so the caller can pass back whatever instance_get
/// showed them.
fn target_matches(target: &str, wanted: &str, encoded: &str) -> bool {
    target == wanted || target == encoded || decoded_literal(target) == wanted
}

impl Ad4mMcpHandler {
    /// Add an item to a collection property of an instance.
    #[tool(
        description = "Add an item to a collection property of an instance (e.g. add a Message to a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the URI of the item (usually another instance's id). Adding an item that is already in the collection is a no-op (links_added: 0). Undo with instance_remove_from_collection."
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
        let info = match resolve_collection(&class_name, &infos, &p.collection, &p.item_uri) {
            Ok(info) => info,
            Err(e) => return e,
        };
        if let Err(e) = coerce_scalar(info, &Value::String(p.item_uri.clone())) {
            return validation_failure(&class_name, &[e]);
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

        match add_member(
            &mut perspective,
            &class_name,
            &base_uri,
            info.name(),
            &p.item_uri,
            &agent_context,
        )
        .await
        {
            Ok(added) => {
                let mut result = json!({
                    "success": true,
                    "class_name": class_name,
                    "base_uri": base_uri,
                    "collection": info.name(),
                    "item_uri": p.item_uri,
                    "links_added": if added { 1 } else { 0 },
                });
                if !added {
                    result["already_member"] = json!(true);
                }
                pretty(&result)
            }
            Err(e) => error_json(e),
        }
    }

    /// Remove an item from a collection property of an instance.
    #[tool(
        description = "Remove an item from a collection property of an instance (e.g. take a Message out of a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the item to remove as listed by instance_get. Only the membership link is removed — the item itself is untouched (use instance_remove to delete it). Removing an item that is not in the collection is a no-op (links_removed: 0)."
    )]
    pub async fn instance_remove_from_collection(
        &self,
        params: Parameters<InstanceRemoveFromCollectionParams>,
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
        let info = match resolve_collection(&class_name, &infos, &p.collection, &p.item_uri) {
            Ok(info) => info,
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

        let wanted = p.item_uri.trim();
        let (removed, stored_targets) = match remove_member(
            &mut perspective,
            &class_name,
            &base_uri,
            info.name(),
            wanted,
            &agent_context,
        )
        .await
        {
            Ok(v) => v,
            Err(e) => return error_json(e),
        };

        // A collection whose property declares a custom `resolveLanguage`
        // stores each member as an `expression_create` address, not as the
        // value the caller passed to instance_add_to_collection — so nothing
        // here can match by value and "0 links removed, success" would be a
        // silent lie. Say what the stored addresses are instead.
        if removed == 0 && !stored_targets.is_empty() {
            if let Ok(Some(lang)) = perspective
                .get_resolve_language_from_shacl(&class_name, &info.name())
                .await
            {
                if lang != "literal" {
                    return error_json(format!(
                        "'{}' is not how collection '{}' stores its members: the property \
                         declares resolveLanguage '{}', so each member is an expression \
                         address created at write time, not the value passed to \
                         instance_add_to_collection. Pass item_uri exactly as instance_get \
                         lists it. Current members: {}.",
                        wanted,
                        info.name(),
                        lang,
                        stored_targets.join(", ")
                    ));
                }
            }
        }

        pretty(&json!({
            "success": true,
            "class_name": class_name,
            "base_uri": base_uri,
            "collection": info.name(),
            "item_uri": p.item_uri,
            "links_removed": removed,
        }))
    }
}

/// Add `item` to `collection` of the instance at `base_uri` through the
/// class's adder (`update_subject`), unless it already is a member: the
/// store dedups the raw triple, but every signed link expression gets its
/// own reifier, so a second add would show up as a duplicate member.
/// `Ok(false)`: it already was. The error is the message to return.
pub(crate) async fn add_member(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
    collection: &str,
    item: &str,
    agent_context: &AgentContext,
) -> Result<bool, String> {
    let wanted = item.trim();
    let encoded = link_target(wanted);
    let links = perspective
        .member_links(class_name, base_uri, collection)
        .await
        .map_err(|e| format!("Error reading collection '{collection}': {e:#}"))?;
    if links
        .iter()
        .any(|link| target_matches(&link.data.target, wanted, &encoded))
    {
        return Ok(false);
    }
    perspective
        .update_subject(
            subject_class(class_name),
            base_uri.to_string(),
            json!({ collection: { "action": "adder", "value": [typed_value(item)] } }),
            None,
            agent_context,
        )
        .await
        .map_err(|e| format!("Error adding to collection '{collection}': {e:#}"))?;
    Ok(true)
}

/// Remove `item` from `collection` of the instance at `base_uri` through the
/// class's remover (`update_subject`), in whichever spelling it is stored
/// (see [`target_matches`]). Returns how many membership links held it, and
/// every stored member, for the caller to explain a miss. The error is the
/// message to return.
pub(crate) async fn remove_member(
    perspective: &mut PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
    collection: &str,
    item: &str,
    agent_context: &AgentContext,
) -> Result<(usize, Vec<String>), String> {
    let encoded = link_target(item);
    let links = perspective
        .member_links(class_name, base_uri, collection)
        .await
        .map_err(|e| format!("Error reading collection '{collection}': {e:#}"))?;
    let stored: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
    let matching: Vec<&String> = stored
        .iter()
        .filter(|target| target_matches(target, item, &encoded))
        .collect();
    if matching.is_empty() {
        return Ok((0, stored));
    }
    let mut members = matching.clone();
    members.sort();
    members.dedup();
    perspective
        .update_subject(
            subject_class(class_name),
            base_uri.to_string(),
            json!({ collection: { "action": "remover", "value": members } }),
            None,
            agent_context,
        )
        .await
        .map_err(|e| format!("Error removing '{item}' from collection '{collection}': {e:#}"))?;
    Ok((matching.len(), stored))
}
