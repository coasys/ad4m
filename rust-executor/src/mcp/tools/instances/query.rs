//! `instance_query` + `instance_get` — read instances of any subject class.

use super::{
    error_json, fetch_instance, link_target, normalize_filter, not_found, pretty, resolve_class,
    run_model_query, Ad4mMcpHandler, DEFAULT_QUERY_LIMIT, HAS_CHILD, MAX_QUERY_LIMIT,
};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

/// Parameters for querying instances of a class
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceQueryParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// Optional filter on property values (a model-query `where` clause).
    /// Keys are property names. Values: an exact match (`{"status": "open"}`),
    /// an array for IN (`{"status": ["open", "doing"]}`), or an operator object
    /// (`{"count": {"gt": 5}}`, `{"title": {"contains": "mcp"}}`,
    /// `{"owner": {"not": "did:key:…"}}`). Combine with `"OR": [..]` /
    /// `"AND": [..]` / `"NOT": {..}`. `id` filters on the instance URI.
    pub filter: Option<Map<String, Value>>,
    /// Optional parent URI: only return instances that are `ad4m://has_child`
    /// children of this node (e.g. the messages of one channel).
    pub parent: Option<String>,
    /// Maximum number of instances to return (default 100).
    pub limit: Option<usize>,
    /// Number of instances to skip, for pagination.
    pub offset: Option<usize>,
}

/// Parameters for reading one instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InstanceGetParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name as listed by describe_perspective
    pub class_name: String,
    /// URI of the instance (the `id` returned by instance_create / instance_query)
    pub base_uri: String,
}

impl Ad4mMcpHandler {
    /// Query instances of any subject class, with typed property values.
    #[tool(
        description = "List instances of a subject class with their property values. class_name is one of the class names from describe_perspective. Optional filter is a where clause on property values: exact match {\"status\": \"open\"}, IN {\"status\": [\"open\", \"doing\"]}, operators {\"count\": {\"gt\": 5}} / {\"title\": {\"contains\": \"mcp\"}} / {\"owner\": {\"not\": \"…\"}}, combinators \"OR\" / \"AND\" / \"NOT\"; \"id\" filters on the instance URI. Optional parent restricts to ad4m://has_child children of one instance (e.g. messages of a channel). Paginate with limit (default 100) and offset; total_count reports the full match count. Each instance has id (its base_uri), author, timestamp, and one key per property."
    )]
    pub async fn instance_query(&self, params: Parameters<InstanceQueryParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let (class_name, shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };

        let mut query = json!({
            "limit": p.limit.unwrap_or(DEFAULT_QUERY_LIMIT).min(MAX_QUERY_LIMIT)
        });
        if let Some(offset) = p.offset {
            query["offset"] = json!(offset);
        }
        if let Some(filter) = &p.filter {
            if !filter.is_empty() {
                match normalize_filter(&shape, filter) {
                    Ok(f) => query["where"] = Value::Object(f),
                    Err(e) => return error_json(e),
                }
            }
        }
        if let Some(parent) = p.parent.as_deref().filter(|s| !s.trim().is_empty()) {
            query["parent"] = json!({ "id": link_target(parent.trim()), "predicate": HAS_CHILD });
        }

        match run_model_query(&perspective, &class_name, &query).await {
            Ok((instances, total)) => pretty(&json!({
                "class_name": class_name,
                "count": instances.len(),
                "total_count": total,
                "instances": instances,
            })),
            Err(e) => error_json(format!("Error querying {class_name} instances: {e}")),
        }
    }

    /// Read one instance of any subject class.
    #[tool(
        description = "Get one instance of a subject class by its base_uri, with all property values and collections resolved. class_name is one of the class names from describe_perspective. Returns an error if no instance of that class exists at the URI."
    )]
    pub async fn instance_get(&self, params: Parameters<InstanceGetParams>) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let (class_name, _shape) = match resolve_class(&perspective, &p.class_name).await {
            Ok(v) => v,
            Err(e) => return e,
        };
        match fetch_instance(&perspective, &class_name, &p.base_uri).await {
            Ok(Some(instance)) => pretty(&instance),
            Ok(None) => not_found(&class_name, &p.base_uri),
            Err(e) => error_json(format!("Error reading {class_name} instance: {e}")),
        }
    }
}
