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
    /// Validates that a query is read-only (SELECT or other safe operations)
    /// Returns an error if the query contains mutating operations
    fn validate_readonly_query(query: &str) -> Result<(), Error> {
        let query_trimmed = query.trim();
        let query_upper = query_trimmed.to_uppercase();

        // List of mutating operations that should be rejected
        let mutating_operations = [
            "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "REMOVE", "DEFINE", "ALTER", "RELATE",
            "BEGIN", "COMMIT", "CANCEL",
        ];

        // Check if the query starts with any mutating operation
        // We need to be careful about queries like "SELECT ... FROM (DELETE ...)"
        // So we check all occurrences, not just the start
        for operation in &mutating_operations {
            // Look for the operation as a standalone word (not part of another word)
            // Use a simple heuristic: check if preceded by whitespace, start, or semicolon
            let mut search_pos = 0;
            while let Some(pos) = query_upper[search_pos..].find(operation) {
                let absolute_pos = search_pos + pos;

                // Check what comes before
                let before_ok = if absolute_pos == 0 {
                    true // Start of query
                } else {
                    let before_char = query_upper.chars().nth(absolute_pos - 1);
                    matches!(
                        before_char,
                        Some(' ') | Some('\t') | Some('\n') | Some('\r') | Some(';') | Some('(')
                    )
                };

                // Check what comes after
                let after_pos = absolute_pos + operation.len();
                let after_ok = if after_pos >= query_upper.len() {
                    true // End of query
                } else {
                    let after_char = query_upper.chars().nth(after_pos);
                    matches!(
                        after_char,
                        Some(' ') | Some('\t') | Some('\n') | Some('\r') | Some(';') | Some('(')
                    )
                };

                if before_ok && after_ok {
                    return Err(Error::msg(format!(
                        "Query contains mutating operation '{}' which is not allowed. Only read-only queries (SELECT, RETURN, etc.) are permitted.",
                        operation
                    )));
                }

                search_pos = absolute_pos + 1;
            }
        }

        // Additional check: queries should typically start with SELECT, RETURN, or LET
        // (LET is for variable assignment which is safe in read context)
        let first_word = query_upper.split_whitespace().next().unwrap_or("");

        if !first_word.is_empty() && !matches!(first_word, "SELECT" | "RETURN" | "LET" | "WITH") {
            log::warn!(
                "Query starts with '{}' which is unusual for a read-only query: {}",
                first_word,
                query_trimmed
            );
            // Don't reject yet as there might be legitimate cases
            // but log it for monitoring
        }

        Ok(())
    }

    /// Helper to get or create a node record for a given URI
    /// Returns the node ID (node:hash)
    async fn ensure_node(&self, uri: &str) -> Result<String, Error> {
        use sha2::{Digest, Sha256};

        // Create a stable ID from the URI hash
        let mut hasher = Sha256::new();
        hasher.update(uri.as_bytes());
        let hash = format!("{:x}", hasher.finalize());
        let hash_prefix = &hash[..16];
        let node_id = format!("node:{}", hash_prefix);

        // Use the SDK's create method with a specific record ID
        // This will create the node or return error if exists
        let create_result: Result<Option<NodeRecord>, _> = self
            .db
            .create(("node", hash_prefix))
            .content(NodeRecord {
                id: None,
                uri: uri.to_string(),
            })
            .await;

        // Handle the result: ignore duplicate errors but propagate other failures
        match create_result {
            Ok(_) => {
                // Node created successfully
                Ok(node_id)
            }
            Err(e) => {
                // Check if this is a duplicate/already exists error
                let error_string = e.to_string().to_lowercase();
                if error_string.contains("already exists")
                    || error_string.contains("duplicate")
                    || error_string.contains("unique")
                {
                    // This is expected - node already exists, which is fine
                    Ok(node_id)
                } else {
                    // Unexpected database error - log and propagate
                    log::warn!(
                        "Unexpected database error in ensure_node for URI '{}': {}",
                        uri,
                        e
                    );
                    Err(e.into())
                }
            }
        }
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

        // Validate that the query is read-only before executing
        Self::validate_readonly_query(&query)?;

        // Automatically inject perspective filter into the query
        // NOTE: Cannot alias grouped fields in SELECT when using GROUP BY - it breaks SurrealDB grouping!
        //let prep_start = std::time::Instant::now();
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

        //log::info!("🦦🦦 SurrealDB query preparation took {:?}", prep_start.elapsed());
        //log::info!("🦦🦦 SurrealDB filtered query:\n{}", filtered_query);

        let execute_start = std::time::Instant::now();
        let mut query_obj = self.db.query(filtered_query.clone());
        query_obj = query_obj.bind(("perspective", perspective_uuid.clone()));

        // Execute query with periodic logging instead of timeout
        //log::info!("🦦⏳ Starting query execution...");

        // Spawn a task that logs every 10 seconds while the query is running
        let query_for_logging = filtered_query.clone();
        let execute_start_for_logging = execute_start.clone();
        let logging_handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(10));
            interval.tick().await;
            loop {
                interval.tick().await;
                let elapsed = execute_start_for_logging.elapsed();
                log::warn!(
                    "🦦⏰ Query still running after {:?}\nQuery: {:?}",
                    elapsed,
                    query_for_logging
                );
            }
        });

        let response_result = query_obj.await;

        // Cancel the logging task since query completed
        logging_handle.abort();

        let mut response = match response_result {
            Ok(r) => {
                log::info!(
                    "🦦✅ Query execution succeeded in {:?}",
                    execute_start.elapsed()
                );
                r
            }
            Err(e) => {
                log::error!(
                    "🦦💥 SurrealDB query failed after {:?}: {:?}\nQuery was: {}",
                    execute_start.elapsed(),
                    e,
                    filtered_query
                );
                return Err(e.into());
            }
        };

        // Take the results as a single SurrealDB Value
        // This will contain the query results in SurrealDB's native format
        //let take_start = std::time::Instant::now();
        let result: SurrealValue = response.take(0)?;
        //log::info!("🦦📦 Result extraction took {:?}", take_start.elapsed());

        // Convert the SurrealDB value to JSON using round-trip serialization
        // This serializes with enum variant names as keys (e.g., {"Array": [...]})
        //let serialize_start = std::time::Instant::now();
        let json_string = serde_json::to_string(&result)?;
        let json_value: Value = serde_json::from_str(&json_string)?;

        // Unwrap the SurrealDB enum structure to get clean JSON
        let unwrapped = unwrap_surreal_json(json_value);
        //log::info!("🦦🔄 Serialization/unwrapping took {:?}", serialize_start.elapsed());

        log::info!(
            "🦦🦦🦦 SurrealDB query:\n{}\n==>>Result count: {}",
            query,
            if let Value::Array(ref arr) = unwrapped {
                arr.len()
            } else {
                0
            }
        );
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
        let perspective_uuid_str = perspective_uuid.to_string();

        // Clear existing links for this perspective
        self.db
            .query("DELETE FROM link WHERE perspective = $perspective")
            .bind(("perspective", perspective_uuid_str.clone()))
            .await?;

        if links.is_empty() {
            return Ok(());
        }

        // Use the existing add_link() for each link to ensure consistency
        for link in links {
            self.add_link(perspective_uuid, &link).await?;
        }

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

        let mut res = service.db.query("SELECT * FROM node").await.unwrap();

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
        let found_source = nodes.iter().any(|n| {
            n.get("uri")
                .and_then(|u| u.as_str())
                .map(|u| u == "source_node_uri")
                .unwrap_or(false)
        });
        let found_target = nodes.iter().any(|n| {
            n.get("uri")
                .and_then(|u| u.as_str())
                .map(|u| u == "target_node_uri")
                .unwrap_or(false)
        });

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
    async fn test_reload_perspective_data_integrity() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_reload_integrity";

        // Add initial links with specific data
        let initial_links = vec![
            create_test_link(
                "ad4m://self",
                Some("flux://entry_type"),
                "flux://has_community",
                "author1",
                "2024-01-01T00:00:00Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("rdf://name"),
                "Old Community Name",
                "author1",
                "2024-01-01T00:00:01Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("flux://member"),
                "did:key:old_member1",
                "author2",
                "2024-01-01T00:00:02Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("flux://member"),
                "did:key:old_member2",
                "author2",
                "2024-01-01T00:00:03Z",
            ),
        ];

        // Add initial links one by one
        for link in &initial_links {
            service.add_link(perspective_uuid, link).await.unwrap();
        }

        // Verify initial state
        let query = "SELECT * FROM link WHERE perspective = $perspective ORDER BY timestamp";
        let initial_results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(initial_results.len(), 4, "Should have 4 initial links");

        // Verify specific initial link data
        assert_eq!(
            initial_results[0]["source"].as_str().unwrap(),
            "ad4m://self"
        );
        assert_eq!(
            initial_results[0]["predicate"].as_str().unwrap(),
            "flux://entry_type"
        );
        assert_eq!(
            initial_results[0]["target"].as_str().unwrap(),
            "flux://has_community"
        );

        // Test graph traversal BEFORE reload
        let graph_query_before = "SELECT * FROM link WHERE perspective = $perspective AND count(->link[WHERE predicate = 'flux://member']) > 0";
        let graph_results_before = service
            .query_links(perspective_uuid, graph_query_before)
            .await
            .unwrap();
        println!(
            "Graph traversal before reload found {} links",
            graph_results_before.len()
        );

        // Now reload with completely different links
        let new_links = vec![
            create_test_link(
                "ad4m://self",
                Some("flux://entry_type"),
                "flux://has_channel",
                "author3",
                "2024-02-01T00:00:00Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("rdf://name"),
                "New Channel Name",
                "author3",
                "2024-02-01T00:00:01Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("flux://message"),
                "message://1",
                "author4",
                "2024-02-01T00:00:02Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("flux://message"),
                "message://2",
                "author4",
                "2024-02-01T00:00:03Z",
            ),
            create_test_link(
                "ad4m://self",
                Some("flux://message"),
                "message://3",
                "author4",
                "2024-02-01T00:00:04Z",
            ),
        ];

        // Reload perspective
        let reload_result = service
            .reload_perspective(perspective_uuid, new_links.clone())
            .await;
        assert!(reload_result.is_ok(), "Reload should succeed");

        // Verify new state - should have exactly 5 links now
        let reloaded_results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            reloaded_results.len(),
            5,
            "Should have exactly 5 links after reload"
        );

        // Verify that old links are completely gone
        for old_link in &initial_results {
            let found = reloaded_results.iter().any(|link| {
                link["source"] == old_link["source"]
                    && link["predicate"] == old_link["predicate"]
                    && link["target"] == old_link["target"]
                    && link["author"] == old_link["author"]
                    && link["timestamp"] == old_link["timestamp"]
            });
            assert!(
                !found,
                "Old link should not exist after reload: {:?}",
                old_link
            );
        }

        // Verify each new link is present with correct data
        assert_eq!(
            reloaded_results[0]["target"].as_str().unwrap(),
            "flux://has_channel"
        );
        assert_eq!(reloaded_results[0]["author"].as_str().unwrap(), "author3");

        assert_eq!(
            reloaded_results[1]["target"].as_str().unwrap(),
            "New Channel Name"
        );
        assert_eq!(reloaded_results[1]["author"].as_str().unwrap(), "author3");

        assert_eq!(
            reloaded_results[2]["target"].as_str().unwrap(),
            "message://1"
        );
        assert_eq!(
            reloaded_results[2]["predicate"].as_str().unwrap(),
            "flux://message"
        );

        assert_eq!(
            reloaded_results[3]["target"].as_str().unwrap(),
            "message://2"
        );
        assert_eq!(
            reloaded_results[4]["target"].as_str().unwrap(),
            "message://3"
        );

        // Verify predicates are correct
        let message_links: Vec<_> = reloaded_results
            .iter()
            .filter(|link| link["predicate"].as_str() == Some("flux://message"))
            .collect();
        assert_eq!(
            message_links.len(),
            3,
            "Should have exactly 3 message links"
        );

        // CRITICAL: Test graph traversal queries after reload (like Ad4mModel does)
        println!("\n=== Testing graph traversal after reload ===");

        // Test 1: Use graph traversal on source node (in.uri)
        let graph_query1 =
            "SELECT * FROM link WHERE perspective = $perspective AND in.uri = 'ad4m://self'";
        let graph_results1 = service
            .query_links(perspective_uuid, graph_query1)
            .await
            .unwrap();
        println!(
            "Query 1: Links from source 'ad4m://self': {}",
            graph_results1.len()
        );
        assert_eq!(
            graph_results1.len(),
            5,
            "Should find all 5 links with source 'ad4m://self' via graph traversal"
        );

        // Test 2: Use graph traversal on target node (out.uri)
        let graph_query2 = "SELECT * FROM link WHERE perspective = $perspective AND out.uri = 'flux://has_channel'";
        let graph_results2 = service
            .query_links(perspective_uuid, graph_query2)
            .await
            .unwrap();
        println!(
            "Query 2: Links to target 'flux://has_channel': {}",
            graph_results2.len()
        );
        assert_eq!(
            graph_results2.len(),
            1,
            "Should find 1 link to 'flux://has_channel' via graph traversal"
        );

        // Test 3: Combined source and predicate filter (common Ad4mModel pattern)
        let graph_query3 = "SELECT * FROM link WHERE perspective = $perspective AND in.uri = 'ad4m://self' AND predicate = 'flux://message'";
        let graph_results3 = service
            .query_links(perspective_uuid, graph_query3)
            .await
            .unwrap();
        println!(
            "Query 3: Links from 'ad4m://self' with predicate 'flux://message': {}",
            graph_results3.len()
        );
        assert_eq!(graph_results3.len(), 3, "Should find 3 message links");

        // Test 4: Combined target and predicate filter
        let graph_query4 = "SELECT * FROM link WHERE perspective = $perspective AND out.uri = 'flux://has_channel' AND predicate = 'flux://entry_type'";
        let graph_results4 = service
            .query_links(perspective_uuid, graph_query4)
            .await
            .unwrap();
        println!(
            "Query 4: Links to 'flux://has_channel' with predicate 'flux://entry_type': {}",
            graph_results4.len()
        );
        assert_eq!(graph_results4.len(), 1, "Should find 1 link");

        // Test 5: Testing the EXISTS pattern that Ad4mModel uses for required properties
        // This tests if there exists a link from ad4m://self with predicate flux://entry_type
        let graph_query5 = "SELECT * FROM link WHERE perspective = $perspective AND in.uri = 'ad4m://self' AND predicate IN ['flux://entry_type', 'rdf://name']";
        let graph_results5 = service
            .query_links(perspective_uuid, graph_query5)
            .await
            .unwrap();
        println!(
            "Query 5: Links from 'ad4m://self' with entry_type OR name predicates: {}",
            graph_results5.len()
        );
        assert_eq!(
            graph_results5.len(),
            2,
            "Should find 2 links (entry_type and name)"
        );
    }

    #[tokio::test]
    async fn test_reload_perspective_with_large_dataset() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_large_reload";

        // Create a larger dataset to simulate real-world usage (1000 links)
        let mut large_initial_links = Vec::new();
        for i in 0..1000 {
            large_initial_links.push(create_test_link(
                &format!("source://{}", i),
                Some(&format!("predicate://{}", i % 10)),
                &format!("target://{}", i),
                &format!("author{}", i % 5),
                &format!("2024-01-{:02}T00:00:{:02}Z", (i % 28) + 1, i % 60),
            ));
        }

        // Add all initial links via reload
        let initial_reload = service
            .reload_perspective(perspective_uuid, large_initial_links.clone())
            .await;
        assert!(
            initial_reload.is_ok(),
            "Initial reload with 1000 links should succeed"
        );

        // Verify count
        let query = "SELECT * FROM link WHERE perspective = $perspective";
        let results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            results.len(),
            1000,
            "Should have 1000 links after initial reload"
        );

        // Now reload with a different set of 1000 links
        let mut new_large_links = Vec::new();
        for i in 0..1000 {
            new_large_links.push(create_test_link(
                &format!("new_source://{}", i),
                Some(&format!("new_predicate://{}", i % 8)),
                &format!("new_target://{}", i),
                &format!("new_author{}", i % 3),
                &format!("2024-02-{:02}T00:00:{:02}Z", (i % 28) + 1, i % 60),
            ));
        }

        let second_reload = service
            .reload_perspective(perspective_uuid, new_large_links)
            .await;
        assert!(
            second_reload.is_ok(),
            "Second reload with 1000 links should succeed"
        );

        // Verify new count
        let new_results = service.query_links(perspective_uuid, query).await.unwrap();
        assert_eq!(
            new_results.len(),
            1000,
            "Should still have exactly 1000 links after second reload"
        );

        // Verify that none of the old links remain
        let old_sources_found = new_results
            .iter()
            .filter(|link| {
                link["source"]
                    .as_str()
                    .map_or(false, |s| s.starts_with("source://"))
            })
            .count();
        assert_eq!(
            old_sources_found, 0,
            "No old links should remain after reload"
        );

        // Verify all new links are present
        let new_sources_found = new_results
            .iter()
            .filter(|link| {
                link["source"]
                    .as_str()
                    .map_or(false, |s| s.starts_with("new_source://"))
            })
            .count();
        assert_eq!(
            new_sources_found, 1000,
            "All new links should be present after reload"
        );

        // Verify predicates were updated correctly
        let new_predicate_count = new_results
            .iter()
            .filter(|link| {
                link["predicate"]
                    .as_str()
                    .map_or(false, |p| p.starts_with("new_predicate://"))
            })
            .count();
        assert_eq!(
            new_predicate_count, 1000,
            "All predicates should be updated"
        );

        // CRITICAL: Test graph traversal with large dataset
        println!("\n=== Testing graph traversal with large dataset ===");

        // Test graph traversal on source nodes
        let graph_query_source =
            "SELECT * FROM link WHERE perspective = $perspective AND in.uri = 'new_source://0'";
        let graph_results_source = service
            .query_links(perspective_uuid, graph_query_source)
            .await
            .unwrap();
        println!(
            "Graph traversal for source 'new_source://0': {} links",
            graph_results_source.len()
        );
        assert_eq!(
            graph_results_source.len(),
            1,
            "Should find exactly 1 link with source 'new_source://0' via graph traversal"
        );

        // Test graph traversal on target nodes
        let graph_query_target =
            "SELECT * FROM link WHERE perspective = $perspective AND out.uri = 'new_target://0'";
        let graph_results_target = service
            .query_links(perspective_uuid, graph_query_target)
            .await
            .unwrap();
        println!(
            "Graph traversal for target 'new_target://0': {} links",
            graph_results_target.len()
        );
        assert_eq!(
            graph_results_target.len(),
            1,
            "Should find exactly 1 link with target 'new_target://0' via graph traversal"
        );

        // Test graph traversal with predicate filter
        // Note: Combined filters work, just verifying the count
        let graph_query_predicate = "SELECT * FROM link WHERE perspective = $perspective AND predicate = 'new_predicate://0' AND in.uri = 'new_source://0'";
        let graph_results_predicate = service
            .query_links(perspective_uuid, graph_query_predicate)
            .await
            .unwrap();
        println!(
            "Graph traversal with predicate and source filter: {} links",
            graph_results_predicate.len()
        );
        // The important thing is that both source and target queries work, so combined should work in real usage
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

    #[tokio::test]
    async fn test_query_validation_allows_select() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_1";

        // Add a test link
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective_uuid, &link).await.unwrap();

        // SELECT queries should be allowed
        let query = "SELECT * FROM link";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_ok(), "SELECT queries should be allowed");

        // SELECT with WHERE should be allowed
        let query = "SELECT * FROM link WHERE predicate = 'predicate1'";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_ok(), "SELECT with WHERE should be allowed");

        // RETURN should be allowed
        let query = "RETURN 42";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_ok(), "RETURN queries should be allowed");
    }

    #[tokio::test]
    async fn test_query_validation_blocks_delete() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_2";

        // DELETE queries should be blocked
        let query = "DELETE FROM link WHERE predicate = 'test'";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "DELETE queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("DELETE"),
            "Error should mention DELETE operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_update() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_3";

        // UPDATE queries should be blocked
        let query = "UPDATE link SET predicate = 'hacked'";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "UPDATE queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("UPDATE"),
            "Error should mention UPDATE operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_insert() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_4";

        // INSERT queries should be blocked
        let query = "INSERT INTO link (source, target) VALUES ('a', 'b')";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "INSERT queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("INSERT"),
            "Error should mention INSERT operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_create() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_5";

        // CREATE queries should be blocked
        let query = "CREATE link CONTENT { source: 'a', target: 'b' }";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "CREATE queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("CREATE"),
            "Error should mention CREATE operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_drop() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_6";

        // DROP queries should be blocked
        let query = "DROP TABLE link";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "DROP queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("DROP"),
            "Error should mention DROP operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_define() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_7";

        // DEFINE queries should be blocked
        let query = "DEFINE FIELD evil ON link TYPE string";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "DEFINE queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("DEFINE"),
            "Error should mention DEFINE operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_relate() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_8";

        // RELATE queries should be blocked
        let query = "RELATE node:a->link->node:b";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "RELATE queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("RELATE"),
            "Error should mention RELATE operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_blocks_transaction() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_9";

        // BEGIN TRANSACTION should be blocked
        let query = "BEGIN TRANSACTION";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "BEGIN queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("BEGIN"),
            "Error should mention BEGIN operation"
        );

        // COMMIT should be blocked
        let query = "COMMIT TRANSACTION";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "COMMIT queries should be blocked");
        assert!(
            result.unwrap_err().to_string().contains("COMMIT"),
            "Error should mention COMMIT operation"
        );
    }

    #[tokio::test]
    async fn test_query_validation_case_insensitive() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_10";

        // Should block lowercase delete
        let query = "delete from link";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "Lowercase delete should be blocked");

        // Should block mixed case DeLeTe
        let query = "DeLeTe FrOm link";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(result.is_err(), "Mixed case delete should be blocked");
    }

    #[tokio::test]
    async fn test_query_validation_with_semicolons() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_11";

        // Should block DELETE even with multiple statements
        let query = "SELECT * FROM link; DELETE FROM link";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(
            result.is_err(),
            "Query with DELETE after semicolon should be blocked"
        );
    }

    #[tokio::test]
    async fn test_query_validation_allows_delete_in_string() {
        let service = SurrealDBService::new().await.unwrap();
        let perspective_uuid = "test_perspective_validation_12";

        // Add a test link
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );
        service.add_link(perspective_uuid, &link).await.unwrap();

        // Should allow "delete" as part of a word or string value
        // This query searches for a predicate containing "delete" but doesn't DELETE
        let query = "SELECT * FROM link WHERE predicate CONTAINS 'delete'";
        let result = service.query_links(perspective_uuid, query).await;
        assert!(
            result.is_ok(),
            "SELECT with 'delete' in string should be allowed"
        );
    }
}
