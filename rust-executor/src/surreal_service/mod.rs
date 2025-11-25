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

#[derive(Debug, Serialize, Deserialize)]
struct LinkRecord {
    #[serde(skip)]
    #[allow(dead_code)]
    id: Option<serde_json::Value>, // SurrealDB Thing ID - skip to avoid deserialization issues with Thing type
    perspective: String,
    source: String,
    predicate: String,
    target: String,
    author: String,
    timestamp: String,
}

#[derive(Clone)]
pub struct SurrealDBService {
    db: Arc<Surreal<Db>>,
}

impl SurrealDBService {
    pub async fn new() -> Result<Self, Error> {
        // Enable scripting (and any other capabilities you want)
        let config = Config::default().capabilities(Capabilities::default().with_scripting(true));

        // Initialize in-memory SurrealDB instance
        let db = Surreal::new::<Mem>(config).await?;

        // Set namespace and database
        db.use_ns("ad4m").use_db("perspectives").await?;

        // Define schema with explicit fields and indexes for fast queries
        db.query(
            "
            DEFINE TABLE IF NOT EXISTS link SCHEMAFULL;
            DEFINE FIELD IF NOT EXISTS perspective ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS source ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS predicate ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS target ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS author ON link TYPE string;
            DEFINE FIELD IF NOT EXISTS timestamp ON link TYPE string;
            DEFINE INDEX IF NOT EXISTS perspective_idx ON link FIELDS perspective;
            DEFINE INDEX IF NOT EXISTS source_idx ON link FIELDS source;
            DEFINE INDEX IF NOT EXISTS target_idx ON link FIELDS target;
            DEFINE INDEX IF NOT EXISTS predicate_idx ON link FIELDS predicate;
            DEFINE INDEX IF NOT EXISTS source_predicate_idx ON link FIELDS source, predicate;
            
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
        let record = LinkRecord {
            id: None, // Will be assigned by SurrealDB
            perspective: perspective_uuid.to_string(),
            source: link.data.source.clone(),
            predicate: link.data.predicate.clone().unwrap_or_default(),
            target: link.data.target.clone(),
            author: link.author.clone(),
            timestamp: link.timestamp.clone(),
        };

        let _: Option<LinkRecord> = self.db.create("link").content(record).await?;

        Ok(())
    }

    pub async fn remove_link(
        &self,
        perspective_uuid: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<(), Error> {
        let perspective_uuid = perspective_uuid.to_string();
        let predicate = link.data.predicate.clone().unwrap_or_default();
        let source = link.data.source.clone();
        let target = link.data.target.clone();

        self.db
            .query(
                "DELETE FROM link WHERE 
                perspective = $perspective AND
                source = $source AND
                predicate = $predicate AND
                target = $target",
            )
            .bind(("perspective", perspective_uuid))
            .bind(("source", source))
            .bind(("predicate", predicate))
            .bind(("target", target))
            .await?;

        Ok(())
    }

    pub async fn query_links(
        &self,
        perspective_uuid: &str,
        query: &str,
    ) -> Result<Vec<Value>, Error> {
        let perspective_uuid = perspective_uuid.to_string();
        let query = query.trim().to_string();

        // Automatically inject perspective filter into the query
        // NOTE: Cannot alias grouped fields in SELECT when using GROUP BY - it breaks SurrealDB grouping!
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

        let mut query_obj = self.db.query(filtered_query);
        query_obj = query_obj.bind(("perspective", perspective_uuid));

        let mut response = query_obj.await?;

        // Take the results as a single SurrealDB Value
        // This will contain the query results in SurrealDB's native format
        let result: SurrealValue = response.take(0)?;

        // Convert the SurrealDB value to JSON using round-trip serialization
        // This serializes with enum variant names as keys (e.g., {"Array": [...]})
        let json_string = serde_json::to_string(&result)?;
        let json_value: Value = serde_json::from_str(&json_string)?;

        // Unwrap the SurrealDB enum structure to get clean JSON
        let unwrapped = unwrap_surreal_json(json_value);

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
        // Clear existing links
        self.clear_perspective(perspective_uuid).await?;

        // Batch insert all links in a transaction
        if !links.is_empty() {
            // Build a batch query with all CREATE statements
            // SurrealDB executes all statements in a single query atomically
            let mut query_builder = self.db.query("");

            // Add each link as a CREATE statement in the batch
            for (idx, link) in links.iter().enumerate() {
                let record = LinkRecord {
                    id: None, // Will be assigned by SurrealDB
                    perspective: perspective_uuid.to_string(),
                    source: link.data.source.clone(),
                    predicate: link.data.predicate.clone().unwrap_or_default(),
                    target: link.data.target.clone(),
                    author: link.author.clone(),
                    timestamp: link.timestamp.clone(),
                };

                // Add CREATE statement with bound parameters for this link
                // Use unique parameter name for each record to avoid conflicts
                let param_name = format!("record{}", idx);
                query_builder = query_builder
                    .query(format!("CREATE link CONTENT ${};", param_name))
                    .bind((param_name, record));
            }

            // Execute the batch query
            let mut response = query_builder.await?;

            // Check results - verify each CREATE operation succeeded
            for idx in 0..links.len() {
                let _: Option<LinkRecord> = response
                    .take(idx)
                    .map_err(|e| Error::msg(format!("Failed to insert link {}: {}", idx, e)))?;
            }
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

        let query = "SELECT * FROM link WHERE source = 'common_source'";
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

        let query = "SELECT * FROM link WHERE target = 'common_target'";
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

        let query = "SELECT * FROM link WHERE source = 'source1' AND predicate = 'predicate1'";
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

        // Query with WHERE clause (perspective filter is automatically added)
        let query = "SELECT * FROM link WHERE source = 'source1'";
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
        let query = "SELECT * FROM link WHERE source = 'shared_source'";
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
        let target1 = results1[0]["target"].as_str().unwrap();
        let target2 = results2[0]["target"].as_str().unwrap();
        assert_ne!(
            target1, target2,
            "Results should be from different perspectives"
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
