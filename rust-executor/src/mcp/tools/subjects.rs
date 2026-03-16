//! Subject CRUD, property, collection, and deletion tools
//!
//! Tools for working with typed subject instances (model objects) in perspectives.

use super::Ad4mMcpHandler;
use crate::graphql::graphql_types::{LinkQuery, LinkStatus};
use crate::perspectives::perspective_instance::{Command, Parameter, SubjectClassOption};
use crate::types::Link;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for querying subject instances
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QuerySubjectsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Message", "Channel", "Todo")
    pub class_name: String,
    /// Optional Prolog query for filtering
    pub query: Option<String>,
}

/// Parameters for getting subject data
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectDataParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address (subject instance ID)
    pub expression_address: String,
}

/// Parameters for creating a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct CreateSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address for the new subject
    pub expression_address: String,
    /// Initial property values as JSON string
    pub initial_values: Option<String>,
}

/// Parameters for executing commands on a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ExecuteCommandsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Commands to execute as JSON string (array of Command objects)
    pub commands: String,
    /// Expression address (subject instance)
    pub expression_address: String,
    /// Optional parameters as JSON string
    pub parameters: Option<String>,
}

/// Parameters for setting a property on a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetSubjectPropertyParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Channel", "Message")
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Property name to set (e.g., "name", "body")
    pub property_name: String,
    /// Value to set (will be wrapped as literal if needed)
    pub value: String,
}

/// Parameters for getting a collection from a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Collection name (e.g., "messages", "members")
    pub collection_name: String,
}

/// Parameters for adding an item to a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddToCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to add
    pub item_address: String,
}

/// Parameters for removing an item from a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RemoveFromCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to remove
    pub item_address: String,
}

/// Parameters for getting children of a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectChildrenParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the parent (optional)
    #[serde(default)]
    pub class_name: Option<String>,
    /// Expression address of the parent subject
    pub expression_address: String,
    /// Optional: filter children to only this class name
    pub child_class_name: Option<String>,
}

/// Parameters for deleting a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct DeleteSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject to delete
    pub expression_address: String,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Query instances of a subject class
    #[tool(
        description = "Query all instances of a subject class (model). Returns expression addresses of all instances. Use get_subject_data to read each instance's properties."
    )]
    pub async fn query_subjects(&self, params: Parameters<QuerySubjectsParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                // Strategy 1: Use SHACL constructor to find instance type marker
                let shape_links = Self::get_shacl_shape_links(&perspective, &p.class_name).await;

                if let Some(shape_link) = shape_links.first() {
                    let shape_uri = &shape_link.data.target;
                    let constructor_links = perspective
                        .get_links(&LinkQuery {
                            source: Some(shape_uri.clone()),
                            predicate: Some("ad4m://constructor".to_string()),
                            ..Default::default()
                        })
                        .await
                        .unwrap_or_default();

                    if let Some(constructor_link) = constructor_links.first() {
                        let actions_str =
                            Self::resolve_literal_value(&constructor_link.data.target);
                        if let Ok(actions) =
                            serde_json::from_str::<Vec<serde_json::Value>>(&actions_str)
                        {
                            if let Some(type_action) = actions.iter().find(|a| {
                                a.get("action").and_then(|v| v.as_str()) == Some("addLink")
                                    && a.get("source").and_then(|v| v.as_str()) == Some("this")
                            }) {
                                let pred = type_action
                                    .get("predicate")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("");
                                let tgt = type_action
                                    .get("target")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("");
                                if !pred.is_empty() && !tgt.is_empty() {
                                    let links = perspective
                                        .get_links(&LinkQuery {
                                            predicate: Some(pred.to_string()),
                                            target: Some(tgt.to_string()),
                                            ..Default::default()
                                        })
                                        .await
                                        .unwrap_or_default();
                                    let instances: Vec<String> =
                                        links.iter().map(|l| l.data.source.clone()).collect();
                                    return serde_json::to_string_pretty(&instances)
                                        .unwrap_or_else(|e| format!("Error: {}", e));
                                }
                            }
                        }
                    }
                }

                // Strategy 2: Fallback to rdf://type matching
                let class_links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some("ad4m://SubjectClass".to_string()),
                        ..Default::default()
                    })
                    .await;

                let target_class = match class_links {
                    Ok(links) => links.iter().find_map(|l| {
                        let name = l.data.source.split("://").last().unwrap_or("");
                        if name == p.class_name {
                            Some(l.data.source.clone())
                        } else {
                            None
                        }
                    }),
                    Err(e) => return format!("Error finding class: {}", e),
                };

                let target_class = match target_class {
                    Some(tc) => tc,
                    None => return format!("Subject class '{}' not found", p.class_name),
                };

                let instance_links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some(target_class),
                        ..Default::default()
                    })
                    .await;

                match instance_links {
                    Ok(links) => {
                        let instances: Vec<String> =
                            links.iter().map(|l| l.data.source.clone()).collect();
                        serde_json::to_string_pretty(&instances)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying instances: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get all data (properties) for a specific subject instance
    #[tool(
        description = "Get all data (properties and values) for a specific subject instance. Returns the complete state of the model instance as a JSON object with property names and values. Example: get_subject_data(perspective_id='abc-123', class_name='Message', expression_address='literal://string:xyz') returns {body: 'Hello', author: 'did:key:...', timestamp: '2026-01-01'}."
    )]
    pub async fn get_subject_data(&self, params: Parameters<GetSubjectDataParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                let name_literal = format!("literal://string:shacl://{}", p.class_name);
                let shape_links = match perspective
                    .get_links(&LinkQuery {
                        source: Some(name_literal),
                        predicate: Some("ad4m://shacl_shape_uri".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => links,
                    Err(e) => return format!("Error querying SHACL shape: {}", e),
                };

                if shape_links.is_empty() {
                    return format!("No SHACL shape found for class '{}'", p.class_name);
                }

                let shape_uri = &shape_links[0].data.target;

                let prop_links = match perspective
                    .get_links(&LinkQuery {
                        source: Some(shape_uri.clone()),
                        predicate: Some("sh://property".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => links,
                    Err(e) => return format!("Error querying properties: {}", e),
                };

                let mut data = serde_json::Map::new();

                for prop_link in &prop_links {
                    let prop_uri = &prop_link.data.target;

                    let prop_name = prop_uri
                        .rsplit_once('.')
                        .map(|(_, name)| name.to_string())
                        .unwrap_or_else(|| prop_uri.clone());

                    let path_links = match perspective
                        .get_links(&LinkQuery {
                            source: Some(prop_uri.clone()),
                            predicate: Some("sh://path".to_string()),
                            ..Default::default()
                        })
                        .await
                    {
                        Ok(links) => links,
                        Err(_) => continue,
                    };

                    if let Some(path_link) = path_links.first() {
                        let predicate = &path_link.data.target;

                        let is_collection = match perspective
                            .get_links(&LinkQuery {
                                source: Some(prop_uri.clone()),
                                predicate: Some("rdf://type".to_string()),
                                target: Some("ad4m://CollectionShape".to_string()),
                                ..Default::default()
                            })
                            .await
                        {
                            Ok(links) => !links.is_empty(),
                            Err(_) => false,
                        };

                        let value_links = match perspective
                            .get_links(&LinkQuery {
                                source: Some(p.expression_address.clone()),
                                predicate: Some(predicate.clone()),
                                ..Default::default()
                            })
                            .await
                        {
                            Ok(links) => links,
                            Err(_) => continue,
                        };

                        if is_collection {
                            let items: Vec<String> =
                                value_links.iter().map(|l| l.data.target.clone()).collect();
                            data.insert(
                                prop_name,
                                serde_json::Value::Array(
                                    items.into_iter().map(serde_json::Value::String).collect(),
                                ),
                            );
                        } else if let Some(link) = value_links.first() {
                            let value = Self::resolve_literal_value(&link.data.target);
                            data.insert(prop_name, serde_json::Value::String(value));
                        }
                    }
                }

                serde_json::to_string_pretty(&serde_json::Value::Object(data))
                    .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => e,
        }
    }

    /// Create a new subject instance
    #[tool(
        description = "Create a new subject instance (model object) with optional initial property values. Example: create_subject(perspective_id='abc-123', class_name='Channel', initial_values='{\"name\": \"general\"}') creates a Channel with the given name. Returns the new instance's expression address."
    )]
    pub async fn create_subject(&self, params: Parameters<CreateSubjectParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let subject_class: SubjectClassOption = match serde_json::from_value(json!({
                    "className": p.class_name
                })) {
                    Ok(sc) => sc,
                    Err(e) => return format!("Error creating subject class: {}", e),
                };

                let initial_values: Option<serde_json::Value> = match &p.initial_values {
                    Some(v) => match serde_json::from_str(v) {
                        Ok(parsed) => Some(parsed),
                        Err(e) => return format!("Error parsing initial_values JSON: {}", e),
                    },
                    None => None,
                };

                match perspective
                    .create_subject(
                        subject_class,
                        p.expression_address.clone(),
                        initial_values,
                        None,
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "created": true,
                            "perspective_id": p.perspective_id,
                            "class_name": p.class_name,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error creating subject: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Execute commands (actions) on a subject instance
    #[tool(
        description = "Execute commands (actions) on a subject instance. Commands are JSON arrays of {source, predicate, target, action} objects."
    )]
    pub async fn execute_commands(&self, params: Parameters<ExecuteCommandsParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let commands: Vec<Command> = match serde_json::from_str(&p.commands) {
                    Ok(cmds) => cmds,
                    Err(e) => return format!("Error parsing commands JSON: {}", e),
                };

                let parameters: Vec<Parameter> = match &p.parameters {
                    Some(params_str) => match serde_json::from_str(params_str) {
                        Ok(parsed) => parsed,
                        Err(e) => return format!("Error parsing parameters JSON: {}", e),
                    },
                    None => Vec::new(),
                };

                match perspective
                    .execute_commands(
                        commands,
                        p.expression_address.clone(),
                        parameters,
                        None,
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "executed": true,
                            "perspective_id": p.perspective_id,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error executing commands: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Set a property value on a subject instance
    #[tool(
        description = "Set a property on a subject instance. Works at the model level — you provide the property name (e.g. 'name', 'body') and the tool handles the underlying link operations. No need to know predicates or link structure. Example: set_subject_property(perspective_id='abc-123', class_name='Channel', expression_address='literal://string:xyz', property_name='name', value='general')."
    )]
    pub async fn set_subject_property(
        &self,
        params: Parameters<SetSubjectPropertyParams>,
    ) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.property_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving property '{}': {}", p.property_name, e)
                    }
                };

                let existing = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate.clone()),
                        ..Default::default()
                    })
                    .await;

                if let Ok(links) = existing {
                    for link in links {
                        let _ = perspective.remove_link(link.into(), None).await;
                    }
                }

                let target = Self::create_property_expression(
                    &perspective,
                    &p.class_name,
                    &p.property_name,
                    &p.value,
                    &agent_context,
                )
                .await;

                let link = Link {
                    source: p.expression_address.clone(),
                    predicate: Some(predicate),
                    target,
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(_) => serde_json::to_string_pretty(&json!({
                        "success": true,
                        "property": p.property_name,
                        "value": p.value,
                    }))
                    .unwrap_or_else(|e| format!("Error: {}", e)),
                    Err(e) => format!("Error setting property: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get all items in a named collection on a subject instance
    #[tool(
        description = "Get all items in a collection property of a subject instance (e.g., all messages in a channel). Returns a list of expression addresses. Works at the model level — just provide the collection name."
    )]
    pub async fn get_subject_collection(
        &self,
        params: Parameters<GetSubjectCollectionParams>,
    ) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(items) => {
                        let targets: Vec<String> =
                            items.iter().map(|l| l.data.target.clone()).collect();
                        serde_json::to_string_pretty(&json!({
                            "collection": p.collection_name,
                            "items": targets,
                            "count": targets.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying collection: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Add an item to a collection on a subject instance
    #[tool(
        description = "Add an item to a collection on a subject instance (e.g., add a message to a channel). Creates the link between parent and child using the correct predicate for the collection."
    )]
    pub async fn add_to_collection(&self, params: Parameters<AddToCollectionParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                let link = Link {
                    source: p.expression_address.clone(),
                    predicate: Some(predicate),
                    target: p.item_address.clone(),
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(_) => serde_json::to_string_pretty(&json!({
                        "success": true,
                        "collection": p.collection_name,
                        "item": p.item_address,
                    }))
                    .unwrap_or_else(|e| format!("Error: {}", e)),
                    Err(e) => format!("Error adding to collection: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Remove an item from a collection on a subject instance
    #[tool(
        description = "Remove an item from a collection on a subject instance. Removes the link between parent and child."
    )]
    pub async fn remove_from_collection(
        &self,
        params: Parameters<RemoveFromCollectionParams>,
    ) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, _agent_context)) => {
                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate),
                        target: Some(p.item_address.clone()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(found) => {
                        let mut removed = 0;
                        for link in found {
                            if let Ok(_) = perspective.remove_link(link.into(), None).await {
                                removed += 1;
                            }
                        }
                        serde_json::to_string_pretty(&json!({
                            "success": true,
                            "removed": removed,
                            "collection": p.collection_name,
                            "item": p.item_address,
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error finding link to remove: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get all children of a subject instance
    #[tool(
        description = "Get all subjects that are children of a given subject (linked via ad4m://has_child). Optionally filter by child class name. Returns a list of child addresses."
    )]
    pub async fn get_subject_children(
        &self,
        params: Parameters<GetSubjectChildrenParams>,
    ) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some("ad4m://has_child".to_string()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(child_links) => {
                        let mut children: Vec<serde_json::Value> = Vec::new();

                        for link in &child_links {
                            let child_addr = &link.data.target;

                            if let Some(ref filter_class) = p.child_class_name {
                                let type_links = perspective
                                    .get_links(&LinkQuery {
                                        source: Some(child_addr.clone()),
                                        predicate: Some("rdf://type".to_string()),
                                        ..Default::default()
                                    })
                                    .await;

                                let matches = match type_links {
                                    Ok(tl) => tl.iter().any(|l| {
                                        l.data.target.split("://").last().unwrap_or("")
                                            == filter_class.as_str()
                                    }),
                                    Err(_) => false,
                                };

                                if !matches {
                                    continue;
                                }
                            }

                            children.push(json!({
                                "address": child_addr,
                            }));
                        }

                        serde_json::to_string_pretty(&json!({
                            "parent": p.expression_address,
                            "children": children,
                            "count": children.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying children: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Delete a subject instance
    #[tool(
        description = "Delete a subject instance. Removes all links where the subject is the source (its properties, type markers, and collection links) AND all inbound links from other subjects pointing to it (e.g., collection membership). ⚠️ This cascade-deletes references from other subjects — use with caution."
    )]
    pub async fn delete_subject(&self, params: Parameters<DeleteSubjectParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, _agent_context)) => {
                let source_links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        ..Default::default()
                    })
                    .await;

                let mut removed = 0;
                if let Ok(links) = source_links {
                    for link in links {
                        if perspective.remove_link(link.into(), None).await.is_ok() {
                            removed += 1;
                        }
                    }
                }

                let target_links = perspective
                    .get_links(&LinkQuery {
                        target: Some(p.expression_address.clone()),
                        ..Default::default()
                    })
                    .await;

                if let Ok(links) = target_links {
                    for link in links {
                        if perspective.remove_link(link.into(), None).await.is_ok() {
                            removed += 1;
                        }
                    }
                }

                serde_json::to_string_pretty(&json!({
                    "success": true,
                    "deleted": p.expression_address,
                    "links_removed": removed,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => e,
        }
    }
}
