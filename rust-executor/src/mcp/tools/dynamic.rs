//! Dynamic SHACL-based tool generation
//!
//! Generates MCP tools dynamically from Subject Class (SHACL SDNA) definitions.
//! This enables AI agents to work with any data model defined in a perspective,
//! not just hardcoded tool sets.

use super::Ad4mMcpHandler;
use crate::graphql::graphql_types::{LinkQuery, LinkStatus};
use crate::mcp::shacl::{self, ShaclProperty};
use crate::perspectives::perspective_instance::SubjectClassOption;
use crate::perspectives::{all_perspectives, get_perspective};
use crate::types::Link;
use rmcp::model::{CallToolResult, Content, Tool};
use rmcp::ErrorData;
use serde_json::json;
use std::sync::Arc;

impl Ad4mMcpHandler {
    /// Generate dynamic MCP tools from SHACL subject classes across all perspectives
    pub(crate) async fn generate_dynamic_tools(&self) -> Vec<Tool> {
        let perspectives = all_perspectives();
        let mut tools = Vec::new();
        let mut seen_classes = std::collections::HashSet::new();

        for p in perspectives.iter() {
            let handle = p.persisted.lock().await.clone();

            // Multi-user isolation: skip perspectives the user can't access
            if !self.can_access_perspective(&handle).await {
                continue;
            }

            let uuid = handle.uuid.clone();

            let perspective = match get_perspective(&uuid) {
                Some(p) => p,
                None => continue,
            };

            let classes = shacl::load_classes(&perspective).await;

            for class in &classes {
                if !seen_classes.insert(class.name_lower.clone()) {
                    continue;
                }

                tools.push(Self::make_create_tool(&class.name, &class.properties));
                tools.push(Self::make_query_tool(&class.name));
                tools.push(Self::make_get_tool(&class.name));
                tools.push(Self::make_delete_tool(&class.name));
                // Per-property set tools and collection tools
                for prop in &class.properties {
                    if prop.is_collection {
                        tools.push(Self::make_collection_get_tool(&class.name, &prop.name));
                        tools.push(Self::make_collection_add_tool(&class.name, &prop.name));
                        tools.push(Self::make_collection_remove_tool(&class.name, &prop.name));
                    } else {
                        tools.push(Self::make_set_property_tool(&class.name, &prop.name));
                    }
                }
            }
        }

        tools
    }

    /// Extract property information from a SHACL shape
    fn make_tool_schema(
        properties: Vec<(&str, &str)>,
        required: Vec<&str>,
    ) -> Arc<serde_json::Map<String, serde_json::Value>> {
        let mut props = serde_json::Map::new();
        for (name, desc) in properties {
            props.insert(
                name.to_string(),
                json!({ "type": "string", "description": desc }),
            );
        }
        let mut schema = serde_json::Map::new();
        schema.insert("type".to_string(), json!("object"));
        schema.insert("properties".to_string(), serde_json::Value::Object(props));
        schema.insert("required".to_string(), json!(required));
        Arc::new(schema)
    }

    fn make_create_tool(class_name: &str, properties: &[ShaclProperty]) -> Tool {
        let name_lower = class_name.to_lowercase();
        let mut prop_entries: Vec<(String, String)> = vec![
            ("perspective_id".to_string(), "Perspective UUID".to_string()),
            (
                "expression_address".to_string(),
                format!("Address for the new {} instance", class_name),
            ),
        ];
        for p in properties {
            if !p.is_collection {
                let required_marker = if p.is_required() { " (required)" } else { "" };
                prop_entries.push((
                    p.name.clone(),
                    format!("{}{} ({})", p.name, required_marker, p.type_description()),
                ));
            }
        }
        let props: Vec<(&str, &str)> = prop_entries
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();
        let prop_descs: Vec<String> = properties
            .iter()
            .filter(|p| !p.is_collection)
            .map(|p| {
                if p.is_required() {
                    format!("{}* ({})", p.name, p.type_description())
                } else {
                    format!("{} ({})", p.name, p.type_description())
                }
            })
            .collect();

        Tool::new(
            format!("{}_create", name_lower),
            format!(
                "Create a new {} instance. Properties: {} (* = required)",
                class_name,
                prop_descs.join(", ")
            ),
            Self::make_tool_schema(props, vec!["perspective_id", "expression_address"]),
        )
    }

    fn make_query_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_query", name_lower),
            format!(
                "Query all {} instances in a perspective. Returns expression addresses.",
                class_name
            ),
            Self::make_tool_schema(
                vec![("perspective_id", "Perspective UUID")],
                vec!["perspective_id"],
            ),
        )
    }

    fn make_get_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_get", name_lower),
            format!(
                "Get all properties and values of a {} instance. Returns a JSON object with property names as keys. Scalar properties return single values; collections return arrays.",
                class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    fn make_set_property_tool(class_name: &str, property_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let prop_lower = property_name.to_lowercase();
        let value_desc = format!("New value for {}", property_name);
        Tool::new(
            format!("{}_set_{}", name_lower, prop_lower),
            format!(
                "Set the '{}' property on a {} instance.",
                property_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", &value_desc),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_collection_get_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_get_{}", name_lower, coll_lower),
            format!(
                "Get all items in the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    fn make_collection_add_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_add_{}", name_lower, coll_lower),
            format!(
                "Add an item to the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", "Value to add to the collection"),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_collection_remove_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_remove_{}", name_lower, coll_lower),
            format!(
                "Remove an item from the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", "Value to remove from the collection"),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_delete_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_delete", name_lower),
            format!(
                "Delete a {} instance and all its associated links.",
                class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    (
                        "expression_address",
                        "Expression address of the instance to delete",
                    ),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    /// Handle a dynamic SHACL tool call
    pub(crate) async fn handle_dynamic_tool(
        &self,
        tool_name: &str,
        arguments: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<CallToolResult, ErrorData> {
        let args = arguments.unwrap_or_default();

        // Parse tool name: {class_name}_{operation} or {class_name}_{operation}_{property}
        let (class_name_lower, rest) = match tool_name.split_once('_') {
            Some((cls, rest)) => (cls, rest),
            None => {
                return Ok(CallToolResult::error(vec![Content::text(format!(
                    "Unknown tool: {}",
                    tool_name
                ))]));
            }
        };

        // rest could be "create", "query", "get", "delete", "set_propname", "add_propname", "remove_propname", "get_propname"
        let (operation, property_name) = if let Some((op, prop)) = rest.split_once('_') {
            (op, Some(prop.to_string()))
        } else {
            (rest, None)
        };

        if !matches!(
            operation,
            "create" | "query" | "get" | "update" | "delete" | "set" | "add" | "remove"
        ) {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Unknown tool: {}",
                tool_name
            ))]));
        }

        let perspective_id = match args.get("perspective_id").and_then(|v| v.as_str()) {
            Some(id) => id.to_string(),
            None => {
                return Ok(CallToolResult::error(vec![Content::text(
                    "Missing required parameter: perspective_id",
                )]));
            }
        };

        // Find actual class name (preserving original case)
        let class_name = {
            let perspective = match self.get_readable_perspective(&perspective_id).await {
                Ok(p) => p,
                Err(_) => {
                    return Ok(CallToolResult::error(vec![Content::text(format!(
                        "Perspective not found: {}",
                        perspective_id
                    ))]));
                }
            };
            match shacl::find_class_name(&perspective, class_name_lower).await {
                Some(name) => name,
                None => {
                    return Ok(CallToolResult::error(vec![Content::text(format!(
                        "Subject class '{}' not found in perspective {}",
                        class_name_lower, perspective_id
                    ))]));
                }
            }
        };

        let result = match operation {
            "create" => {
                self.handle_dynamic_create(&perspective_id, &class_name, &args)
                    .await
            }
            "query" => {
                self.handle_dynamic_query(&perspective_id, &class_name)
                    .await
            }
            "get" => {
                if let Some(ref prop) = property_name {
                    // {class}_get_{collection} — get collection items
                    self.handle_dynamic_get_collection(&perspective_id, &class_name, prop, &args)
                        .await
                } else {
                    self.handle_dynamic_get(&perspective_id, &class_name, &args)
                        .await
                }
            }
            "set" => {
                // {class}_set_{property}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_set_property(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "add" => {
                // {class}_add_{collection}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_add_collection(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "remove" => {
                // {class}_remove_{collection}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_remove_collection(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "update" => {
                self.handle_dynamic_update(&perspective_id, &class_name, &args)
                    .await
            }
            "delete" => {
                self.handle_dynamic_delete(&perspective_id, &class_name, &args)
                    .await
            }
            _ => unreachable!(),
        };

        Ok(CallToolResult::success(vec![Content::text(result)]))
    }

    async fn handle_dynamic_create(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        // Build initial_values from non-system property args
        let initial_values: Option<serde_json::Value> = {
            let props: serde_json::Map<String, serde_json::Value> = args
                .iter()
                .filter(|(k, _)| {
                    k.as_str() != "perspective_id" && k.as_str() != "expression_address"
                })
                .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), json!(s))))
                .collect();
            if props.is_empty() {
                None
            } else {
                Some(serde_json::Value::Object(props))
            }
        };

        let subject_class: SubjectClassOption = match serde_json::from_value(json!({
            "className": class_name
        })) {
            Ok(sc) => sc,
            Err(e) => return format!("Error: {}", e),
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        match perspective
            .create_subject(
                subject_class,
                expression_address.clone(),
                initial_values,
                None,
                &agent_context,
            )
            .await
        {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "created": true,
                "perspective_id": perspective_id,
                "class_name": class_name,
                "expression_address": expression_address
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error creating subject: {}", e),
        }
    }

    async fn handle_dynamic_query(&self, perspective_id: &str, class_name: &str) -> String {
        let perspective = match self.get_readable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Strategy 1: Find instances via SHACL constructor "type marker" link pattern.
        // The constructor's first addLink action typically defines the type marker
        // (e.g., flux://entry_type → flux://has_message for Message class).
        let shape_links = Self::get_shacl_shape_links(&perspective, class_name).await;

        if let Some(shape_link) = shape_links.first() {
            let shape_uri = &shape_link.data.target;

            // Get constructor actions
            let constructor_links = perspective
                .get_links(&LinkQuery {
                    source: Some(shape_uri.clone()),
                    predicate: Some("ad4m://constructor".to_string()),
                    ..Default::default()
                })
                .await
                .unwrap_or_default();

            if let Some(constructor_link) = constructor_links.first() {
                let actions_str = Self::resolve_literal_value(&constructor_link.data.target);
                if let Ok(actions) = serde_json::from_str::<Vec<serde_json::Value>>(&actions_str) {
                    // Find the type marker action (first addLink with source="this")
                    if let Some(type_action) = actions.iter().find(|a| {
                        a.get("action").and_then(|v| v.as_str()) == Some("addLink")
                            && a.get("source").and_then(|v| v.as_str()) == Some("this")
                    }) {
                        let predicate = type_action
                            .get("predicate")
                            .and_then(|v| v.as_str())
                            .unwrap_or("");
                        let target = type_action
                            .get("target")
                            .and_then(|v| v.as_str())
                            .unwrap_or("");

                        if !predicate.is_empty() && !target.is_empty() {
                            // Query for all links matching this type marker pattern
                            let instance_links = match perspective
                                .get_links(&LinkQuery {
                                    predicate: Some(predicate.to_string()),
                                    target: Some(target.to_string()),
                                    ..Default::default()
                                })
                                .await
                            {
                                Ok(links) => links,
                                Err(e) => return format!("Error: {}", e),
                            };

                            let instances: Vec<String> = instance_links
                                .iter()
                                .map(|l| l.data.source.clone())
                                .collect();
                            return serde_json::to_string_pretty(&instances)
                                .unwrap_or_else(|e| format!("Error: {}", e));
                        }
                    }
                }
            }
        }

        // Strategy 2: Fallback to rdf://type matching (for classes that use standard RDF typing)
        let class_links = match perspective
            .get_links(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        let target_class = match class_links.iter().find_map(|l| {
            let name = l.data.source.split("://").last().unwrap_or("");
            if name == class_name {
                Some(l.data.source.clone())
            } else {
                None
            }
        }) {
            Some(tc) => tc,
            None => return format!("Subject class '{}' not found", class_name),
        };

        let instance_links = match perspective
            .get_links(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some(target_class),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        let instances: Vec<String> = instance_links
            .iter()
            .map(|l| l.data.source.clone())
            .collect();
        serde_json::to_string_pretty(&instances).unwrap_or_else(|e| format!("Error: {}", e))
    }

    async fn handle_dynamic_get(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let perspective = match self.get_readable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Reuse get_subject_data logic — try both encoded and raw SHACL name
        let shape_links = Self::get_shacl_shape_links(&perspective, class_name).await;

        if shape_links.is_empty() {
            return format!("No SHACL shape found for class '{}'", class_name);
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
            Err(e) => return format!("Error: {}", e),
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
                        source: Some(expression_address.clone()),
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

    async fn handle_dynamic_update(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        let mut updated = Vec::new();
        for (key, value) in args {
            if key == "perspective_id" || key == "expression_address" {
                continue;
            }
            let value_str = match value.as_str() {
                Some(s) => s.to_string(),
                None => value.to_string(),
            };

            let predicate = match self
                .resolve_property_predicate(&perspective, class_name, key)
                .await
            {
                Ok(pred) => pred,
                Err(e) => return format!("Error resolving property '{}': {}", key, e),
            };

            // Remove old values
            if let Ok(links) = perspective
                .get_links(&LinkQuery {
                    source: Some(expression_address.clone()),
                    predicate: Some(predicate.clone()),
                    ..Default::default()
                })
                .await
            {
                for link in links {
                    let _ = perspective.remove_link(link.into(), None).await;
                }
            }

            // Add new value
            let target = if value_str.starts_with("literal://") || value_str.contains("://") {
                value_str.clone()
            } else {
                Self::encode_literal(&value_str)
            };

            let link = Link {
                source: expression_address.clone(),
                predicate: Some(predicate),
                target,
            };

            match perspective
                .add_link(link, LinkStatus::Shared, None, &agent_context)
                .await
            {
                Ok(_) => updated.push(key.clone()),
                Err(e) => return format!("Error setting property '{}': {}", key, e),
            }
        }

        serde_json::to_string_pretty(&json!({
            "success": true,
            "updated_properties": updated,
        }))
        .unwrap_or_else(|e| format!("Error: {}", e))
    }

    async fn handle_dynamic_delete(
        &self,
        perspective_id: &str,
        _class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        let mut removed = 0;
        if let Ok(links) = perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                ..Default::default()
            })
            .await
        {
            for link in links {
                if perspective.remove_link(link.into(), None).await.is_ok() {
                    removed += 1;
                }
            }
        }

        if let Ok(links) = perspective
            .get_links(&LinkQuery {
                target: Some(expression_address.clone()),
                ..Default::default()
            })
            .await
        {
            for link in links {
                if perspective.remove_link(link.into(), None).await.is_ok() {
                    removed += 1;
                }
            }
        }

        serde_json::to_string_pretty(&json!({
            "success": true,
            "deleted": expression_address,
            "links_removed": removed,
        }))
        .unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// Handle {class}_set_{property} — set a single property on a subject instance
    async fn handle_dynamic_set_property(
        &self,
        perspective_id: &str,
        class_name: &str,
        property_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        // Resolve property predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, property_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving property '{}': {}", property_name, e),
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            Self::encode_literal(&value)
        };

        // Use batch to atomically remove old + add new (setSingleTarget pattern).
        // Without batching, the remove can propagate to other nodes before the add,
        // causing the property to appear as "uninitialized" temporarily.
        let batch_id = perspective.create_batch().await;

        // Remove existing links with this predicate
        let existing = perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate.clone()),
                ..Default::default()
            })
            .await;

        if let Ok(links) = existing {
            for link in links {
                let _ = perspective
                    .remove_link(link.into(), Some(batch_id.clone()))
                    .await;
            }
        }

        let link = Link {
            source: expression_address.clone(),
            predicate: Some(predicate),
            target,
        };

        let _ = perspective
            .add_link(link, LinkStatus::Shared, Some(batch_id.clone()), &agent_context)
            .await;

        match perspective.commit_batch(batch_id, &agent_context).await {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "success": true,
                "expression_address": expression_address,
                "property": property_name,
                "value": value,
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error setting property '{}': {}", property_name, e),
        }
    }

    /// Handle {class}_get_{collection} — get items in a collection
    async fn handle_dynamic_get_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let (perspective, _agent_ctx) = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        // Query all links with this predicate from the expression
        match perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate),
                ..Default::default()
            })
            .await
        {
            Ok(links) => {
                let items: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
                serde_json::to_string_pretty(&json!({
                    "expression_address": expression_address,
                    "collection": collection_name,
                    "items": items,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!("Error getting collection '{}': {}", collection_name, e),
        }
    }

    /// Handle {class}_add_{collection} — add item to a collection
    async fn handle_dynamic_add_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        // Resolve collection predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            Self::encode_literal(&value)
        };

        let link = Link {
            source: expression_address.clone(),
            predicate: Some(predicate),
            target,
        };

        match perspective
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "success": true,
                "expression_address": expression_address,
                "collection": collection_name,
                "added": value,
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error adding to collection '{}': {}", collection_name, e),
        }
    }

    /// Handle {class}_remove_{collection} — remove item from a collection
    async fn handle_dynamic_remove_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let (mut perspective, _agent_ctx) =
            match self.get_writable_perspective(perspective_id).await {
                Ok(p) => p,
                Err(e) => return e,
            };

        // Resolve collection predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        // Find and remove the link with matching target
        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            Self::encode_literal(&value)
        };

        match perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate),
                target: Some(target),
                ..Default::default()
            })
            .await
        {
            Ok(links) => {
                let mut removed = 0;
                for link in links {
                    if perspective.remove_link(link.into(), None).await.is_ok() {
                        removed += 1;
                    }
                }
                serde_json::to_string_pretty(&json!({
                    "success": true,
                    "expression_address": expression_address,
                    "collection": collection_name,
                    "removed": value,
                    "links_removed": removed,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!(
                "Error removing from collection '{}': {}",
                collection_name, e
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    // Test-only struct that mirrors McpContext for test isolation
    struct TestAuthContext {
        admin_credential: Option<String>,
        auth_token: Arc<RwLock<Option<String>>>,
    }

    impl TestAuthContext {
        fn new(admin_credential: Option<String>) -> Self {
            Self {
                admin_credential,
                auth_token: Arc::new(RwLock::new(None)),
            }
        }

        async fn get_auth_token(&self) -> Option<String> {
            self.auth_token.read().await.clone()
        }
    }

    // Test the auth_status logic directly without needing full MCP handler
    #[tokio::test]
    async fn test_auth_status_unauthenticated() {
        let ctx = TestAuthContext::new(None);
        let token = ctx.get_auth_token().await;

        // Simulate auth_status logic
        let result = match token {
            Some(t) if !t.is_empty() => "authenticated",
            _ => "not_authenticated",
        };

        assert_eq!(result, "not_authenticated");
    }

    #[tokio::test]
    async fn test_auth_token_stores_value() {
        let ctx = TestAuthContext::new(None);

        // Simulate token storage logic
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("test-token".to_string());
        }

        let token = ctx.get_auth_token().await;
        assert_eq!(token, Some("test-token".to_string()));
    }

    #[tokio::test]
    async fn test_admin_credential_check() {
        let ctx = TestAuthContext::new(Some("my-admin-secret".to_string()));

        // Set admin credential as token
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("my-admin-secret".to_string());
        }

        let token = ctx.get_auth_token().await;
        let is_admin = token.as_ref() == ctx.admin_credential.as_ref();

        assert!(is_admin);
    }

    #[tokio::test]
    async fn test_invalid_admin_credential() {
        let ctx = TestAuthContext::new(Some("my-admin-secret".to_string()));

        // Set wrong credential
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("wrong-secret".to_string());
        }

        let token = ctx.get_auth_token().await;
        let is_admin = token.as_ref() == ctx.admin_credential.as_ref();

        assert!(!is_admin);
    }

    #[test]
    fn test_escape_prolog_string() {
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string(r#"test"value"#),
            r#"test\"value"#
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string(r"test\path"),
            r"test\\path"
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("test'quote"),
            r"test\'quote"
        );
        // Test newline and carriage return escaping
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("line1\nline2"),
            r"line1\nline2"
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("text\r\nmore"),
            r"text\r\nmore"
        );
    }

    // Integration tests for full login flow would need the database initialized
    // See tests/js/tests/mcp-auth.test.ts for full integration tests
}
