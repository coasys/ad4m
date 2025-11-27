use crate::types::DecoratedLinkExpression;
use deno_core::anyhow::Error;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use surrealdb::{
    engine::local::{Db, Mem},
    opt::{capabilities::Capabilities, Config},
    Surreal, Value as SurrealValue,
};
use tokio::sync::RwLock;

/// Helper function to unwrap SurrealDB's enum-wrapped JSON structure
/// SurrealDB values serialize with variant names as keys (e.g., {"Strand": "value"})
/// This function recursively unwraps these to get clean JSON
fn unwrap_surreal_json(value: Value) -> Value {
    match value {
        Value::Object(mut obj) => {
            // Check if this is a single-key enum wrapper
            if obj.len() == 1 {
                // Get the single key to check if it's a known enum variant
                let key = obj.keys().next().unwrap().clone();

                match key.as_str() {
                    // Array wrapper - recursively unwrap contents
                    "Array" => {
                        if let Some(Value::Array(arr)) = obj.remove(&key) {
                            return Value::Array(
                                arr.into_iter().map(unwrap_surreal_json).collect(),
                            );
                        }
                    }
                    // Object wrapper - recursively unwrap all fields
                    "Object" => {
                        if let Some(Value::Object(inner)) = obj.remove(&key) {
                            let unwrapped: serde_json::Map<_, _> = inner
                                .into_iter()
                                .map(|(k, v)| (k, unwrap_surreal_json(v)))
                                .collect();
                            return Value::Object(unwrapped);
                        }
                    }
                    // Simple value wrappers - extract the inner value
                    "Strand" | "String" | "Number" | "Bool" => {
                        if let Some(val) = obj.remove(&key) {
                            return val;
                        }
                    }
                    // Thing (record ID) - convert to string representation
                    "Thing" => {
                        if let Some(Value::Object(thing_obj)) = obj.remove(&key) {
                            // Format as "table:id"
                            let tb = thing_obj.get("tb").and_then(|v| v.as_str()).unwrap_or("");
                            let id = thing_obj
                                .get("id")
                                .and_then(|v| {
                                    unwrap_surreal_json(v.clone()).as_str().map(String::from)
                                })
                                .unwrap_or_default();
                            return Value::String(format!("{}:{}", tb, id));
                        }
                    }
                    // Unknown single-key object - recursively unwrap the value
                    _ => {
                        if let Some(val) = obj.remove(&key) {
                            return Value::Object(
                                [(key, unwrap_surreal_json(val))].into_iter().collect(),
                            );
                        }
                    }
                }
            }

            // Multi-key object - recursively unwrap all values
            Value::Object(
                obj.into_iter()
                    .map(|(k, v)| (k, unwrap_surreal_json(v)))
                    .collect(),
            )
        }
        Value::Array(arr) => Value::Array(arr.into_iter().map(unwrap_surreal_json).collect()),
        // Primitive values - return as is
        other => other,
    }
}

/// Node record representing a URI in the graph
#[derive(Debug, Serialize, Deserialize, Clone)]
struct NodeRecord {
    #[serde(skip)]
    #[allow(dead_code)]
    id: Option<serde_json::Value>,
    uri: String,
}

/// Link edge record connecting two nodes in the graph
/// This is used with RELATE statements: node->link->node
#[derive(Debug, Serialize, Deserialize)]
struct LinkEdge {
    #[serde(skip)]
    #[allow(dead_code)]
    id: Option<serde_json::Value>, // SurrealDB Thing ID
    #[serde(skip_serializing_if = "Option::is_none")]
    #[allow(dead_code)]
    r#in: Option<serde_json::Value>, // Source node (auto-created by RELATE)
    #[serde(skip_serializing_if = "Option::is_none")]
    #[allow(dead_code)]
    out: Option<serde_json::Value>, // Target node (auto-created by RELATE)
    perspective: String,
    predicate: String,
    author: String,
    timestamp: String,
}

#[derive(Clone)]
pub struct SurrealDBService {
    db: Arc<Surreal<Db>>,
}

impl SurrealDBService {
    /// Helper to get or create a node record for a given URI
    /// Returns the node ID (node:hash)
    async fn ensure_node(&self, uri: &str) -> Result<String, Error> {
        use sha2::{Sha256, Digest};

        // Create a stable ID from the URI hash
        let mut hasher = Sha256::new();
        hasher.update(uri.as_bytes());
        let hash = format!("{:x}", hasher.finalize());
        let hash_prefix = &hash[..16];
        let node_id = format!("node:{}", hash_prefix);

        // Use the SDK's create method with a specific record ID
        // This will create the node or return error if exists (which we ignore)
        let create_result: Result<Option<NodeRecord>, _> = self.db
            .create(("node", hash_prefix))
            .content(NodeRecord {
                id: None,
                uri: uri.to_string(),
            })
            .await;

        // Ignore duplicate errors - node already exists is fine
        let _ = create_result;

        Ok(node_id)
    }

    pub async fn new() -> Result<Self, Error> {
        // Enable scripting (and any other capabilities you want)
        let config = Config::default().capabilities(Capabilities::default().with_scripting(true));

        // Initialize in-memory SurrealDB instance
        let db = Surreal::new::<Mem>(config).await?;

        // Set namespace and database
        db.use_ns("ad4m").use_db("perspectives").await?;

        // Define schema with graph structure for optimal traversal
        db.query(
            "
            -- Node table stores all URIs (sources and targets)
            DEFINE TABLE IF NOT EXISTS node SCHEMAFULL;
            DEFINE FIELD IF NOT EXISTS uri ON node TYPE string;
            DEFINE INDEX IF NOT EXISTS uri_idx ON node FIELDS uri UNIQUE;

            -- Link table is a graph edge connecting nodes
            -- RELATE automatically creates 'in' (source node) and 'out' (target node) fields pointing to nodes
            -- We ALSO store source/target as explicit string fields for simple WHERE clause filtering
            DEFINE TABLE IF NOT EXISTS link SCHEMAFULL TYPE RELATION IN node OUT node;
            DEFINE FIELD IF NOT EXISTS source ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS target ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS perspective ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS predicate ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS author ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS timestamp ON link TYPE string;

            -- Indexes for fast queries (both graph traversal and string-based)
            DEFINE INDEX IF NOT EXISTS perspective_idx ON link FIELDS perspective;
            DEFINE INDEX IF NOT EXISTS predicate_idx ON link FIELDS predicate;
            DEFINE INDEX IF NOT EXISTS source_idx ON link FIELDS source;
            DEFINE INDEX IF NOT EXISTS target_idx ON link FIELDS target;
            DEFINE INDEX IF NOT EXISTS in_predicate_idx ON link FIELDS in, predicate;
            DEFINE INDEX IF NOT EXISTS out_predicate_idx ON link FIELDS out, predicate;
            DEFINE INDEX IF NOT EXISTS perspective_predicate_idx ON link FIELDS perspective, predicate;
            DEFINE INDEX IF NOT EXISTS source_predicate_idx ON link FIELDS source, predicate;
            DEFINE INDEX IF NOT EXISTS target_predicate_idx ON link FIELDS target, predicate;

            DEFINE FUNCTION IF NOT EXISTS fn::parse_literal($url: option<string>) {
                RETURN function($url) {
                    const [url] = arguments;
                    
                    if (!url || typeof url !== 'string') {
                        return url;
                    }
                    
                    if (!url.startsWith('literal://')) {
                        return url;
                    }
                    
                    const body = url.substring(10);
                    
                    if (body.startsWith('string:')) {
                        return decodeURIComponent(body.substring(7));
                    }
                    
                    if (body.startsWith('number:')) {
                        return parseFloat(body.substring(7));
                    }
                    
                    if (body.startsWith('boolean:')) {
                        return body.substring(8) === 'true';
                    }
                    
                    if (body.startsWith('json:')) {
                        try {
                            const json = decodeURIComponent(body.substring(5));
                            const parsed = JSON.parse(json);
                            if (parsed.data !== undefined) {
                                return parsed.data;
                            }
                            return parsed;
                        } catch (e) {
                            return url;
                        }
                    }
                    
                    return url;
                };
            };
            ",
        )
        .await?;

        Ok(SurrealDBService { db: Arc::new(db) })
    }

    pub async fn add_link(
        &self,
        perspective_uuid: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<(), Error> {

        // Ensure source and target nodes exist
        let source_id = self.ensure_node(&link.data.source).await?;
        let target_id = self.ensure_node(&link.data.target).await?;

        // Create the edge using RELATE
        // RELATE creates graph 'in'/'out' fields, we also set explicit source/target strings
        let predicate = link.data.predicate.clone().unwrap_or_default();

        let relate_query = format!(
            "RELATE {}->link->{} SET source = $source, target = $target, perspective = $perspective, predicate = $predicate, author = $author, timestamp = $timestamp",
            source_id, target_id
        );

        self.db
            .query(&relate_query)
            .bind(("source", link.data.source.clone()))
            .bind(("target", link.data.target.clone()))
            .bind(("perspective", perspective_uuid.to_string()))
            .bind(("predicate", predicate))
            .bind(("author", link.author.clone()))
            .bind(("timestamp", link.timestamp.clone()))
            .await?;

        Ok(())
    }

    pub async fn remove_link(
        &self,
        perspective_uuid: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<(), Error> {
        let perspective_uuid = perspective_uuid.to_string();

        // Get node IDs for source and target
        let source_id = self.ensure_node(&link.data.source).await?;
        let target_id = self.ensure_node(&link.data.target).await?;

        let predicate = link.data.predicate.clone().unwrap_or_default();

        // Delete the graph edge matching in, out, and other criteria
        // In graph edges, 'in' is source and 'out' is target
        self.db
            .query(
                "DELETE FROM link WHERE
                in = type::thing($source_id) AND
                out = type::thing($target_id) AND
                perspective = $perspective AND
                predicate = $predicate",
            )
            .bind(("source_id", source_id))
            .bind(("target_id", target_id))
            .bind(("perspective", perspective_uuid.to_string()))
            .bind(("predicate", predicate))
            .await?;

        Ok(())
    }

    pub async fn query_links(
        &self,
        perspective_uuid: &str,
        query: &str,
    ) -> Result<Vec<Value>, Error> {
        let total_start = std::time::Instant::now();
        let perspective_uuid = perspective_uuid.to_string();
        let query = query.trim().to_string();

        // Automatically inject perspective filter into the query
        // NOTE: Cannot alias grouped fields in SELECT when using GROUP BY - it breaks SurrealDB grouping!
        let prep_start = std::time::Instant::now();
        let query_upper = query.to_uppercase();

        let filtered_query = if query_upper.contains("FROM LINK") {
            // Find all occurrences of "FROM link" (case insensitive)
            let mut result = query.clone();
            let mut search_start = 0;

            loop {
                let remaining = &result[search_start..];
                let remaining_upper = remaining.to_uppercase();

                if let Some(pos) = remaining_upper.find("FROM LINK") {
                    let absolute_pos = search_start + pos;

                    // Check if this FROM LINK is already in a subquery
                    // Count opening and closing parentheses before this position
                    let _before = &result[..absolute_pos];
                    let _open_parens = _before.matches('(').count();
                    let _close_parens = _before.matches(')').count();

                    // If we're not inside a subquery (or we are but this is the innermost link table)
                    // we should wrap it
                    let end_pos = absolute_pos + 9; // "FROM link".len() = 9

                    // Check what comes after "link" - if there's already AS or a subquery, skip it
                    let after = &result[end_pos..].trim_start();
                    if after.starts_with("AS") || after.starts_with("as") {
                        // Already aliased, skip this one
                        search_start = end_pos;
                        continue;
                    }

                    // Replace "FROM link" with "FROM link WHERE perspective = $perspective"
                    // But need to check if there's already a WHERE clause after this FROM
                    let after_from = &result[end_pos..];
                    let after_from_upper = after_from.trim_start().to_uppercase();

                    if after_from_upper.starts_with("WHERE") {
                        // Already has WHERE, inject AND condition
                        let where_start =
                            end_pos + after_from.len() - after_from.trim_start().len();
                        let where_end = where_start + 5; // "WHERE".len()
                        result = format!(
                            "{} perspective = $perspective AND{}",
                            &result[..where_end],
                            &result[where_end..]
                        );
                        search_start = where_end + 35;
                    } else {
                        // No WHERE yet, add one
                        // Trim the after part to avoid double spaces
                        let after_trimmed = &result[end_pos..].trim_start();
                        result = format!(
                            "{}FROM link WHERE perspective = $perspective {}",
                            &result[..absolute_pos],
                            after_trimmed
                        );
                        search_start =
                            absolute_pos + "FROM link WHERE perspective = $perspective".len();
                    }
                } else {
                    break;
                }
            }

            result
        } else {
            // No "FROM link" found, return query as-is
            // This might be a complex query or error, but we'll let SurrealDB handle it
            query.clone()
        };

        log::info!("🦦🦦 SurrealDB query preparation took {:?}", prep_start.elapsed());
        log::info!("🦦🦦 SurrealDB filtered query:\n{}", filtered_query);

        let execute_start = std::time::Instant::now();
        let mut query_obj = self.db.query(filtered_query.clone());
        query_obj = query_obj.bind(("perspective", perspective_uuid.clone()));

        // Execute query with periodic logging instead of timeout
        log::info!("🦦⏳ Starting query execution...");

        // Spawn a task that logs every 10 seconds while the query is running
        let query_for_logging = filtered_query.clone();
        let execute_start_for_logging = execute_start.clone();
        let logging_handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(10));
            loop {
                interval.tick().await;
                let elapsed = execute_start_for_logging.elapsed();
                log::warn!("🦦⏰ Query still running after {:?}\nQuery: {:?}", elapsed, query_for_logging);
            }
        });

        let response_result = query_obj.await;

        // Cancel the logging task since query completed
        logging_handle.abort();

        let mut response = match response_result {
            Ok(r) => {
                log::info!("🦦✅ Query execution succeeded in {:?}", execute_start.elapsed());
                r
            }
            Err(e) => {
                log::error!("🦦💥 SurrealDB query failed after {:?}: {:?}\nQuery was: {}", execute_start.elapsed(), e, filtered_query);
                return Err(e.into());
            }
        };

        // Take the results as a single SurrealDB Value
        // This will contain the query results in SurrealDB's native format
        let take_start = std::time::Instant::now();
        let result: SurrealValue = response.take(0)?;
        log::info!("🦦📦 Result extraction took {:?}", take_start.elapsed());

        // Convert the SurrealDB value to JSON using round-trip serialization
        // This serializes with enum variant names as keys (e.g., {"Array": [...]})
        let serialize_start = std::time::Instant::now();
        let json_string = serde_json::to_string(&result)?;
        let json_value: Value = serde_json::from_str(&json_string)?;

        // Unwrap the SurrealDB enum structure to get clean JSON
        let unwrapped = unwrap_surreal_json(json_value);
        log::info!("🦦🔄 Serialization/unwrapping took {:?}", serialize_start.elapsed());

        log::info!("🦦🦦🦦 SurrealDB query result count: {}",
            if let Value::Array(ref arr) = unwrapped { arr.len() } else { 0 });
        log::info!("🦦⏱️ TOTAL query time: {:?}", total_start.elapsed());
        // Extract array from the unwrapped result, propagating error if not an array
        match unwrapped {
            Value::Array(arr) => Ok(arr),
            Value::Null => Ok(vec![]),
            // For non-array results (e.g., RETURN function() or single values), wrap in array
            other => Ok(vec![other]),
        }
    }

    #[allow(dead_code)]
    pub async fn clear_perspective(&self, perspective_uuid: &str) -> Result<(), Error> {
        let perspective_uuid = perspective_uuid.to_string();

        self.db
            .query("DELETE FROM link WHERE perspective = $perspective")
            .bind(("perspective", perspective_uuid))
            .await?;

        Ok(())
    }

    #[allow(dead_code)]
    pub async fn reload_perspective(
        &self,
        perspective_uuid: &str,
        links: Vec<DecoratedLinkExpression>,
    ) -> Result<(), Error> {
        use sha2::{Sha256, Digest};
        use std::collections::{HashMap, HashSet};

        let perspective_uuid_str = perspective_uuid.to_string();

        // Clear existing links
        self.db
            .query("DELETE FROM link WHERE perspective = $perspective")
            .bind(("perspective", perspective_uuid_str.clone()))
            .await?;

        if links.is_empty() {
            return Ok(());
        }

        // Step 1: Collect all unique URIs and create nodes
        let mut unique_uris = HashSet::new();
        let mut uri_to_node_id = HashMap::new();

        for link in &links {
            unique_uris.insert(link.data.source.clone());
            unique_uris.insert(link.data.target.clone());
        }

        // Batch create all nodes
        let mut query_builder = self.db.query("");
        for uri in &unique_uris {
            let mut hasher = Sha256::new();
            hasher.update(uri.as_bytes());
            let hash = format!("{:x}", hasher.finalize());
            let node_id = format!("node:{}", &hash[..16]);

            uri_to_node_id.insert(uri.clone(), node_id.clone());

            query_builder = query_builder
                .query(format!("UPDATE {} SET uri = $uri_{}", node_id, hash[..8].to_string()))
                .bind((format!("uri_{}", hash[..8].to_string()), uri.clone()));
        }

        // Execute node creation batch
        query_builder.await?;

        // Step 2: Batch create all edges using RELATE
        let mut relate_builder = self.db.query("");
        for (idx, link) in links.iter().enumerate() {
            let source_id = uri_to_node_id.get(&link.data.source).unwrap();
            let target_id = uri_to_node_id.get(&link.data.target).unwrap();
            let predicate = link.data.predicate.clone().unwrap_or_default();

            let relate_query = format!(
                "RELATE {}->link->{} SET source = $s{}, target = $tgt{}, perspective = $p{}, predicate = $pred{}, author = $a{}, timestamp = $t{}",
                source_id, target_id, idx, idx, idx, idx, idx, idx
            );

            relate_builder = relate_builder
                .query(&relate_query)
                .bind((format!("s{}", idx), link.data.source.clone()))
                .bind((format!("tgt{}", idx), link.data.target.clone()))
                .bind((format!("p{}", idx), perspective_uuid.to_string()))
                .bind((format!("pred{}", idx), predicate))
                .bind((format!("a{}", idx), link.author.clone()))
                .bind((format!("t{}", idx), link.timestamp.clone()));
        }

        // Execute edge creation batch
        relate_builder.await?;

        Ok(())
    }
}

lazy_static! {
    static ref SURREAL_SERVICE: Arc<RwLock<Option<SurrealDBService>>> = Arc::new(RwLock::new(None));
}

pub async fn init_surreal_service() -> Result<(), Error> {
    let service = SurrealDBService::new().await?;
    let mut lock = SURREAL_SERVICE.write().await;
    *lock = Some(service);
    Ok(())
}

pub async fn get_surreal_service() -> SurrealDBService {
    let lock = SURREAL_SERVICE.read().await;
    lock.clone()
        .expect("SurrealDBService not initialized. Call init_surreal_service() first.")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    fn create_test_link(
        source: &str,
        predicate: Option<&str>,
        target: &str,
        author: &str,
        timestamp: &str,
    ) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: author.to_string(),
            timestamp: timestamp.to_string(),
            data: Link {
                source: source.to_string(),
                predicate: predicate.map(|s| s.to_string()),
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "test_key".to_string(),
                signature: "test_signature".to_string(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: None,
        }
    }

    #[tokio::test]
    async fn test_new_service_initializes_successfully() {
        let service = SurrealDBService::new().await;
        assert!(service.is_ok(), "Service should initialize successfully");
    }

    #[tokio::test]
    async fn test_add_single_link() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_1";
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        let result = service.add_link(perspective_uuid, &link).await;
        assert!(result.is_ok(), "Adding link should succeed");

        // Query to verify the link was added (perspective filter is automatic)
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 1, "Should have exactly one link");
    }

    #[tokio::test]
    async fn test_add_link_with_none_predicate() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_2";
        let link = create_test_link(
            "source1",
            None,
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        let result = service.add_link(perspective_uuid, &link).await;
        assert!(
            result.is_ok(),
            "Adding link with None predicate should succeed"
        );

        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 1, "Should have exactly one link");
    }

    #[tokio::test]
    async fn test_add_multiple_links() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_3";

        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("predicate2"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        let link3 = create_test_link(
            "source3",
            Some("predicate3"),
            "target3",
            "author3",
            "2024-01-01T00:00:02Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();
        service.add_link(perspective_uuid, &link3).await.unwrap();

        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 3, "Should have exactly three links");
    }

    #[tokio::test]
    async fn test_remove_link() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_4";
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        // Add the link
        service.add_link(perspective_uuid, &link).await.unwrap();

        // Verify it was added
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 1, "Should have one link after adding");

        // Remove the link
        let result = service.remove_link(perspective_uuid, &link).await;
        assert!(result.is_ok(), "Removing link should succeed");

        // Verify it was removed
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 0, "Should have no links after removal");
    }

    #[tokio::test]
    async fn test_remove_nonexistent_link() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_5";
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        // Try to remove a link that was never added
        let result = service.remove_link(perspective_uuid, &link).await;
        assert!(result.is_ok(), "Removing nonexistent link should not error");
    }

    #[tokio::test]
    async fn test_ensure_node_creates_node() {
        let service = SurrealDBService::new().await.unwrap();
        let test_uri = "testnode://example";

        // Use ensure_node to create the node
        let _node_id = service.ensure_node(test_uri).await.unwrap();

        let mut res = service.db.query("SELECT * FROM node")
            .await
            .unwrap();

        // Use SurrealDB Value type and convert to JSON
        use surrealdb::Value as SurrealValue;
        let result: SurrealValue = res.take(0).unwrap();
        
        // Convert to JSON and unwrap SurrealDB enum structure
        let json_string = serde_json::to_string(&result).unwrap();
        let json_value: serde_json::Value = serde_json::from_str(&json_string).unwrap();
        let unwrapped = unwrap_surreal_json(json_value);
        
        let nodes = unwrapped.as_array().unwrap();
        
        assert_eq!(nodes.len(), 1, "Node should be created and retrievable");

        // Check node id format and URI match
        let found_id = nodes[0].get("id").unwrap();
        assert!(
            found_id.to_string().contains("node:"),
            "Node id should contain 'node:'"
        );
        let found_uri = nodes[0].get("uri").unwrap().as_str().unwrap();
        assert_eq!(found_uri, test_uri, "Node uri should match");
    }

    
    #[tokio::test]
    async fn test_nodes_are_created_for_links() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_nodes";

        // Create and add a link (which should ensure two nodes exist)
        let link = create_test_link(
            "source_node_uri",
            Some("predicate1"),
            "target_node_uri",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective_uuid, &link).await.unwrap();

        // Query for all nodes (should have two: source and target)
        let mut res = service.db.query("SELECT * FROM node").await.unwrap();

        // Use SurrealDB Value type and convert to JSON
        use surrealdb::Value as SurrealValue;
        let result: SurrealValue = res.take(0).unwrap();
        
        // Convert to JSON and unwrap SurrealDB enum structure
        let json_string = serde_json::to_string(&result).unwrap();
        let json_value: serde_json::Value = serde_json::from_str(&json_string).unwrap();
        let unwrapped = unwrap_surreal_json(json_value);
        
        let nodes = unwrapped.as_array().unwrap();
        
        // Filter to exclude test pollution if any, or just check for our URIs
        let found_source = nodes.iter().any(|n| n.get("uri").and_then(|u| u.as_str()).map(|u| u == "source_node_uri").unwrap_or(false));
        let found_target = nodes.iter().any(|n| n.get("uri").and_then(|u| u.as_str()).map(|u| u == "target_node_uri").unwrap_or(false));

        assert!(
            found_source && found_target,
            "Both source and target nodes should exist"
        );
        assert_eq!(
            nodes.len(),
            2,
            "There should be exactly two nodes after adding one link"
        );
    }

    #[tokio::test]
    async fn test_query_links_by_source() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_6";

        let link1 = create_test_link(
            "common_source",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "common_source",
            Some("predicate2"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        let link3 = create_test_link(
            "different_source",
            Some("predicate3"),
            "target3",
            "author3",
            "2024-01-01T00:00:02Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();
        service.add_link(perspective_uuid, &link3).await.unwrap();

        // Query using graph traversal: in.uri refers to the source node's URI
        let query = "SELECT * FROM link WHERE in.uri = 'common_source'";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            2,
            "Should have exactly two links with common_source"
        );
    }

    #[tokio::test]
    async fn test_query_links_by_predicate() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_7";

        let link1 = create_test_link(
            "source1",
            Some("common_predicate"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("common_predicate"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        let link3 = create_test_link(
            "source3",
            Some("different_predicate"),
            "target3",
            "author3",
            "2024-01-01T00:00:02Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();
        service.add_link(perspective_uuid, &link3).await.unwrap();

        let query = "SELECT * FROM link WHERE predicate = 'common_predicate'";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            2,
            "Should have exactly two links with common_predicate"
        );
    }

    #[tokio::test]
    async fn test_query_links_by_target() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_8";

        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "common_target",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("predicate2"),
            "common_target",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        let link3 = create_test_link(
            "source3",
            Some("predicate3"),
            "different_target",
            "author3",
            "2024-01-01T00:00:02Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();
        service.add_link(perspective_uuid, &link3).await.unwrap();

        // Query using graph traversal: out.uri refers to the target node's URI
        let query = "SELECT * FROM link WHERE out.uri = 'common_target'";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            2,
            "Should have exactly two links with common_target"
        );
    }

    #[tokio::test]
    async fn test_query_links_composite_source_and_predicate() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_9";

        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source1",
            Some("predicate1"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        let link3 = create_test_link(
            "source1",
            Some("predicate2"),
            "target3",
            "author3",
            "2024-01-01T00:00:02Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();
        service.add_link(perspective_uuid, &link3).await.unwrap();

        // Query using graph traversal: combine in.uri (source) with predicate
        let query = "SELECT * FROM link WHERE in.uri = 'source1' AND predicate = 'predicate1'";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            2,
            "Should have exactly two links matching source1 and predicate1"
        );
    }

    #[tokio::test]
    async fn test_clear_perspective() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_10";

        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("predicate2"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();

        // Verify links were added
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 2, "Should have two links before clear");

        // Clear the perspective
        let result = service.clear_perspective(perspective_uuid).await;
        assert!(result.is_ok(), "Clearing perspective should succeed");

        // Verify all links were removed
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 0, "Should have no links after clear");
    }

    #[tokio::test]
    async fn test_perspective_isolation() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective1 = "test_perspective_11";
        let perspective2 = "test_perspective_12";

        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("predicate2"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );

        service.add_link(perspective1, &link1).await.unwrap();
        service.add_link(perspective2, &link2).await.unwrap();

        // Query perspective1
        let query = "SELECT * FROM link";
        let results1 = service.query_links(perspective1, query).await.unwrap();
        assert_eq!(
            results1.len(),
            1,
            "Perspective1 should have exactly one link"
        );

        // Query perspective2
        let results2 = service.query_links(perspective2, query).await.unwrap();
        assert_eq!(
            results2.len(),
            1,
            "Perspective2 should have exactly one link"
        );

        // Clear perspective1 should not affect perspective2
        service.clear_perspective(perspective1).await.unwrap();
        let results1_after = service.query_links(perspective1, query).await.unwrap();
        let results2_after = service.query_links(perspective2, query).await.unwrap();

        assert_eq!(
            results1_after.len(),
            0,
            "Perspective1 should have no links after clear"
        );
        assert_eq!(
            results2_after.len(),
            1,
            "Perspective2 should still have one link"
        );
    }

    #[tokio::test]
    async fn test_reload_perspective() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_13";

        // Add initial links
        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        let link2 = create_test_link(
            "source2",
            Some("predicate2"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );

        service.add_link(perspective_uuid, &link1).await.unwrap();
        service.add_link(perspective_uuid, &link2).await.unwrap();

        // Verify initial links
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 2, "Should have two links initially");

        // Reload with new set of links
        let link3 = create_test_link(
            "source3",
            Some("predicate3"),
            "target3",
            "author3",
            "2024-01-01T00:00:02Z",
        );
        let link4 = create_test_link(
            "source4",
            Some("predicate4"),
            "target4",
            "author4",
            "2024-01-01T00:00:03Z",
        );
        let link5 = create_test_link(
            "source5",
            Some("predicate5"),
            "target5",
            "author5",
            "2024-01-01T00:00:04Z",
        );

        let new_links = vec![link3, link4, link5];
        let result = service
            .reload_perspective(perspective_uuid, new_links)
            .await;
        assert!(result.is_ok(), "Reloading perspective should succeed");

        // Verify new links replaced old ones
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            3,
            "Should have exactly three links after reload"
        );
    }

    #[tokio::test]
    async fn test_reload_perspective_with_empty_list() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_14";

        // Add initial links
        let link1 = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective_uuid, &link1).await.unwrap();

        // Verify initial link
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 1, "Should have one link initially");

        // Reload with empty list
        let result = service.reload_perspective(perspective_uuid, vec![]).await;
        assert!(
            result.is_ok(),
            "Reloading perspective with empty list should succeed"
        );

        // Verify all links were cleared
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 0, "Should have no links after reload");
    }

    #[tokio::test]
    async fn test_global_service_initialization() {
        // Initialize the global service
        let init_result = init_surreal_service().await;
        assert!(
            init_result.is_ok(),
            "Global service initialization should succeed"
        );

        // Get the global service
        let service = get_surreal_service().await;

        // Test that the service works
        let perspective_uuid = "test_perspective_15";
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        let result = service.add_link(perspective_uuid, &link).await;
        assert!(result.is_ok(), "Global service should be able to add links");
    }

    #[tokio::test]
    async fn test_query_without_perspective_binding() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_16";

        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective_uuid, &link).await.unwrap();

        // Query with WHERE clause using graph traversal (perspective filter is automatically added)
        let query = "SELECT * FROM link WHERE in.uri = 'source1'";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            1,
            "Should find exactly one link in this perspective"
        );
    }

    #[tokio::test]
    async fn test_automatic_perspective_filtering() {
        // This test verifies that perspective filtering is enforced automatically
        // even when the query doesn't explicitly mention perspective
        let service = SurrealDBService::new().await.unwrap();
        let perspective1 = "test_perspective_auto_1";
        let perspective2 = "test_perspective_auto_2";

        // Add links to perspective1
        let link1 = create_test_link(
            "shared_source",
            Some("shared_predicate"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective1, &link1).await.unwrap();

        // Add links to perspective2 with same source/predicate
        let link2 = create_test_link(
            "shared_source",
            Some("shared_predicate"),
            "target2",
            "author2",
            "2024-01-01T00:00:01Z",
        );
        service.add_link(perspective2, &link2).await.unwrap();

        // Query without mentioning perspective - should only return perspective1's data
        let query = "SELECT * FROM link WHERE in.uri = 'shared_source'";
        let results1 = service.query_links(perspective1, query).await.unwrap();
        assert_eq!(
            results1.len(),
            1,
            "Should only see perspective1's link despite matching source"
        );

        // Same query on perspective2 - should only return perspective2's data
        let results2 = service.query_links(perspective2, query).await.unwrap();
        assert_eq!(
            results2.len(),
            1,
            "Should only see perspective2's link despite matching source"
        );

        // Verify the results are actually different
        // In graph model, 'out' is a Thing ID string like "node:hash"
        let out1 = results1[0]["out"].as_str().unwrap();
        let out2 = results2[0]["out"].as_str().unwrap();
        assert_ne!(
            out1, out2,
            "Results should be from different perspectives (different target nodes)"
        );
    }

    #[tokio::test]
    async fn test_concurrent_operations() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_17";

        // Create multiple links
        let links: Vec<_> = (0..10)
            .map(|i| {
                create_test_link(
                    &format!("source{}", i),
                    Some(&format!("predicate{}", i)),
                    &format!("target{}", i),
                    &format!("author{}", i),
                    "2024-01-01T00:00:00Z",
                )
            })
            .collect();

        // Add links concurrently
        let futures: Vec<_> = links
            .iter()
            .map(|link| service.add_link(perspective_uuid, link))
            .collect();

        let results = futures::future::join_all(futures).await;
        assert!(
            results.iter().all(|r| r.is_ok()),
            "All concurrent additions should succeed"
        );

        // Verify all links were added
        let query = "SELECT * FROM link";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(results.len(), 10, "Should have all 10 links");
    }
}
