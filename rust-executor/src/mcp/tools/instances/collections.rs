//! `instance_add_to_collection` + `instance_remove_from_collection` —
//! collection membership on any subject class.

use super::{common_error, error_json, pretty, Ad4mMcpHandler};
use crate::perspectives::instances::{self as shared, InstanceError, WriteStep};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

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

/// The collection tools' text for a refused or failed write.
fn collection_error(error: &InstanceError) -> String {
    if let Some(text) = common_error(error) {
        return text;
    }
    match error {
        InstanceError::Failed {
            class_name,
            step,
            error,
        } => error_json(match step {
            WriteStep::ReadShape => format!("{error:#}"),
            WriteStep::ReadInstance => format!("Error reading {class_name} instance: {error}"),
            WriteStep::ResolveCollection { collection } => {
                format!("Error resolving collection '{collection}': {error}")
            }
            WriteStep::ReadCollection { collection } => {
                format!("Error reading collection '{collection}': {error:#}")
            }
            WriteStep::AddCollectionItem { collection, .. } => {
                format!("Error adding to collection '{collection}': {error:#}")
            }
            WriteStep::RemoveCollectionItem {
                collection,
                item,
                removed,
            } => format!(
                "Error removing '{item}' from collection '{collection}' ({removed} link(s) \
                 already removed): {error:#}"
            ),
            other => format!("Error writing collection ({other:?}): {error:#}"),
        }),
        InstanceError::StoredAsExpressions {
            collection,
            item_uri,
            language,
            members,
            ..
        } => error_json(format!(
            "'{}' is not how collection '{}' stores its members: the property declares \
             resolveLanguage '{}', so each member is an expression address created at write \
             time, not the value passed to instance_add_to_collection. Pass item_uri exactly \
             as instance_get lists it. Current members: {}.",
            item_uri,
            collection,
            language,
            members.join(", ")
        )),
        other => error_json(format!("{other:?}")),
    }
}

impl Ad4mMcpHandler {
    /// Add an item to a collection property of an instance.
    #[tool(
        description = "Add an item to a collection property of an instance (e.g. add a Message to a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the URI of the item (usually another instance's id). Adding an item that is already in the collection is a no-op (links_added: 0, already_member: true). A collection whose class declares a language call as its adder (a mounted class) is added to by that language: the reply says language_call: true, with links_added: 0. Undo with instance_remove_from_collection."
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
        match shared::add_to_collection(
            &mut perspective,
            &agent_context,
            shared::InstanceAddToCollection {
                class_name: p.class_name.clone(),
                base_uri: p.base_uri.clone(),
                collection: p.collection.clone(),
                item_uri: p.item_uri.clone(),
                batch_id: None,
            },
        )
        .await
        {
            Ok(added) => {
                let mut result = json!({
                    "success": true,
                    "class_name": added.class_name,
                    "base_uri": added.base_uri,
                    "collection": added.collection,
                    "item_uri": added.item_uri,
                    "links_added": added.links_added,
                });
                if added.already_member {
                    result["already_member"] = json!(true);
                }
                if added.language_call {
                    result["language_call"] = json!(true);
                }
                pretty(&result)
            }
            Err(e) => collection_error(&e),
        }
    }

    /// Remove an item from a collection property of an instance.
    #[tool(
        description = "Remove an item from a collection property of an instance (e.g. take a Message out of a Channel's messages). class_name is the owning instance's class, base_uri its id, collection one of the names listed under collections by describe_perspective, item_uri the item to remove as listed by instance_get. Only the membership link is removed — the item itself is untouched (use instance_remove to delete it). Removing an item that is not in the collection is a no-op (links_removed: 0). A collection whose class declares a language call as its remover (a mounted class) is removed from by that language: the reply says language_call: true."
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
        match shared::remove_from_collection(
            &mut perspective,
            &agent_context,
            shared::InstanceRemoveFromCollection {
                class_name: p.class_name.clone(),
                base_uri: p.base_uri.clone(),
                collection: p.collection.clone(),
                item_uri: p.item_uri.clone(),
                batch_id: None,
            },
        )
        .await
        {
            Ok(removed) => {
                let mut result = json!({
                    "success": true,
                    "class_name": removed.class_name,
                    "base_uri": removed.base_uri,
                    "collection": removed.collection,
                    "item_uri": removed.item_uri,
                    "links_removed": removed.links_removed,
                });
                if removed.language_call {
                    result["language_call"] = json!(true);
                }
                pretty(&result)
            }
            Err(e) => collection_error(&e),
        }
    }
}
