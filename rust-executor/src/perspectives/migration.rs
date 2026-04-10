/// Migration module for perspective link data.
///
/// Provides migration paths:
/// - Rusqlite → SPARQL (Oxigraph) — for users upgrading from pre-SurrealDB versions
///
/// During migration, `literal://` URIs are converted to the canonical `literal:` format.
///
/// This module can be safely removed once all users have migrated.
/// To remove:
/// 1. Delete this file
/// 2. Remove `pub mod migration;` from mod.rs
/// 3. Remove migration calls from perspective initialization
/// 4. Optionally remove migration-tracking methods from db.rs
use crate::db::Ad4mDb;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression};

/// Result of a migration operation.
#[derive(Debug, Clone)]
pub struct MigrationResult {
    /// Number of links successfully migrated.
    pub migrated: usize,
    /// Number of links that failed to migrate.
    pub errors: usize,
    /// Number of individual URI fields converted from `literal://` to `literal:`.
    pub literal_conversions: usize,
}

/// Convert a single URI from legacy `literal://` format to canonical `literal:` format.
///
/// Returns the (possibly converted) URI and whether a conversion occurred.
///
/// # Examples
/// - `literal://string:hello` → `literal:string:hello` (converted)
/// - `ad4m://self` → `ad4m://self` (unchanged)
/// - `did:key:z6Mk...` → `did:key:z6Mk...` (unchanged)
pub fn convert_literal_uri(uri: &str) -> (String, bool) {
    if uri.starts_with("literal://") {
        // "literal://" is 10 chars; replace with "literal:" (8 chars)
        (format!("literal:{}", &uri[10..]), true)
    } else {
        (uri.to_string(), false)
    }
}

/// Convert all `literal://` URIs in a link's source, predicate, and target fields.
///
/// Modifies the link in place and returns the number of fields converted.
pub fn convert_link_literal_uris(link: &mut DecoratedLinkExpression) -> usize {
    let mut conversions = 0;

    let (new_source, changed) = convert_literal_uri(&link.data.source);
    if changed {
        link.data.source = new_source;
        conversions += 1;
    }

    if let Some(ref pred) = link.data.predicate {
        let (new_pred, changed) = convert_literal_uri(pred);
        if changed {
            link.data.predicate = Some(new_pred);
            conversions += 1;
        }
    }

    let (new_target, changed) = convert_literal_uri(&link.data.target);
    if changed {
        link.data.target = new_target;
        conversions += 1;
    }

    conversions
}

/// Migrate all links for a perspective from Rusqlite to the SPARQL (Oxigraph) service.
///
/// This handles users upgrading from any previous version (pre-SurrealDB or post-SurrealDB).
/// During migration, all `literal://` URIs are converted to the canonical `literal:` format.
///
/// The function is idempotent — calling it multiple times on the same perspective is safe.
pub fn migrate_links_from_rusqlite_to_sparql(
    perspective_uuid: &str,
    sparql_store: &crate::perspectives::sparql_store::SparqlStore,
) -> Result<MigrationResult, String> {
    // Check if already migrated
    let already_migrated = Ad4mDb::with_global_instance(|db| {
        db.is_perspective_migrated(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    if already_migrated {
        log::debug!(
            "Perspective {} already migrated to SPARQL, skipping",
            perspective_uuid
        );
        return Ok(MigrationResult {
            migrated: 0,
            errors: 0,
            literal_conversions: 0,
        });
    }

    log::debug!(
        "Starting link migration from Rusqlite to SPARQL for perspective {}",
        perspective_uuid
    );

    // Get all links from Rusqlite
    let links = Ad4mDb::with_global_instance(|db| {
        db.get_all_links(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    if links.is_empty() {
        log::debug!("No links to migrate for perspective {}", perspective_uuid);
        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(perspective_uuid)
                .map_err(|e| e.to_string())
        })?;
        return Ok(MigrationResult {
            migrated: 0,
            errors: 0,
            literal_conversions: 0,
        });
    }

    log::info!(
        "Migrating {} links for perspective {} from Rusqlite to SPARQL",
        links.len(),
        perspective_uuid
    );

    let mut migrated_count = 0;
    let mut error_count = 0;
    let mut total_literal_conversions = 0;

    for (link_expr, status) in &links {
        let mut decorated_link = DecoratedLinkExpression {
            author: link_expr.author.clone(),
            timestamp: link_expr.timestamp.clone(),
            data: link_expr.data.clone(),
            proof: DecoratedExpressionProof {
                key: link_expr.proof.key.clone(),
                signature: link_expr.proof.signature.clone(),
                valid: None,
                invalid: None,
            },
            status: Some(status.clone()),
        };

        // Convert literal:// → literal: in all URI fields
        total_literal_conversions += convert_link_literal_uris(&mut decorated_link);

        match sparql_store.add_link(&decorated_link) {
            Ok(_) => {
                migrated_count += 1;
            }
            Err(e) => {
                log::error!(
                    "Failed to migrate link to SPARQL for perspective {}: {}",
                    perspective_uuid,
                    e
                );
                error_count += 1;
            }
        }
    }

    log::info!(
        "Migration completed for perspective {}: {} links migrated, {} errors, {} literal URI conversions",
        perspective_uuid,
        migrated_count,
        error_count,
        total_literal_conversions
    );

    if error_count > 0 {
        return Err(format!(
            "Migration failed for perspective {}: {} out of {} links failed",
            perspective_uuid,
            error_count,
            links.len()
        ));
    }

    // Mark perspective as migrated
    Ad4mDb::with_global_instance(|db| {
        db.mark_perspective_as_migrated(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    // Delete links from Rusqlite
    let deleted_count = Ad4mDb::with_global_instance(|db| {
        db.delete_all_links_for_perspective(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    log::info!(
        "Deleted {} links from Rusqlite for perspective {}",
        deleted_count,
        perspective_uuid
    );

    Ok(MigrationResult {
        migrated: migrated_count,
        errors: error_count,
        literal_conversions: total_literal_conversions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::sparql_store::SparqlStore;
    use crate::types::LinkStatus;
    use crate::types::{ExpressionProof, Link, LinkExpression};
    use chrono::Utc;

    use std::sync::Once;

    static INIT_DB: Once = Once::new();

    fn ensure_db() {
        INIT_DB.call_once(|| {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        });
    }

    // ── URI conversion tests ──────────────────────────────────────────

    #[test]
    fn test_convert_literal_uri_string() {
        let (result, changed) = convert_literal_uri("literal://string:hello");
        assert_eq!(result, "literal:string:hello");
        assert!(changed);
    }

    #[test]
    fn test_convert_literal_uri_json() {
        let (result, changed) = convert_literal_uri("literal://json:%7B%22key%22%3A%22value%22%7D");
        assert_eq!(result, "literal:json:%7B%22key%22%3A%22value%22%7D");
        assert!(changed);
    }

    #[test]
    fn test_convert_literal_uri_number() {
        let (result, changed) = convert_literal_uri("literal://number:42");
        assert_eq!(result, "literal:number:42");
        assert!(changed);
    }

    #[test]
    fn test_convert_literal_uri_boolean() {
        let (result, changed) = convert_literal_uri("literal://boolean:true");
        assert_eq!(result, "literal:boolean:true");
        assert!(changed);
    }

    #[test]
    fn test_convert_literal_uri_already_canonical() {
        let (result, changed) = convert_literal_uri("literal:string:hello");
        assert_eq!(result, "literal:string:hello");
        assert!(!changed);
    }

    #[test]
    fn test_convert_literal_uri_ad4m_unchanged() {
        let (result, changed) = convert_literal_uri("ad4m://self");
        assert_eq!(result, "ad4m://self");
        assert!(!changed);
    }

    #[test]
    fn test_convert_literal_uri_flux_unchanged() {
        let (result, changed) = convert_literal_uri("flux://has_channel");
        assert_eq!(result, "flux://has_channel");
        assert!(!changed);
    }

    #[test]
    fn test_convert_literal_uri_did_unchanged() {
        let (result, changed) = convert_literal_uri("did:key:z6MkTest123");
        assert_eq!(result, "did:key:z6MkTest123");
        assert!(!changed);
    }

    #[test]
    fn test_convert_literal_uri_neighbourhood_unchanged() {
        let (result, changed) = convert_literal_uri("neighbourhood://QmHash123");
        assert_eq!(result, "neighbourhood://QmHash123");
        assert!(!changed);
    }

    #[test]
    fn test_convert_literal_uri_typed_literal() {
        let (result, changed) = convert_literal_uri("literal://5^^xsd:integer");
        assert_eq!(result, "literal:5^^xsd:integer");
        assert!(changed);
    }

    // ── Link conversion tests ─────────────────────────────────────────

    #[test]
    fn test_convert_link_literal_uris_source_and_target() {
        let mut link = DecoratedLinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "literal://string:source_val".to_string(),
                predicate: Some("ad4m://has_child".to_string()),
                target: "literal://string:target_val".to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "key".to_string(),
                signature: "sig".to_string(),
                valid: None,
                invalid: None,
            },
            status: None,
        };

        let conversions = convert_link_literal_uris(&mut link);
        assert_eq!(conversions, 2);
        assert_eq!(link.data.source, "literal:string:source_val");
        assert_eq!(link.data.predicate, Some("ad4m://has_child".to_string()));
        assert_eq!(link.data.target, "literal:string:target_val");
    }

    #[test]
    fn test_convert_link_literal_uris_all_three() {
        let mut link = DecoratedLinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "literal://string:s".to_string(),
                predicate: Some("literal://string:p".to_string()),
                target: "literal://string:t".to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "k".to_string(),
                signature: "s".to_string(),
                valid: None,
                invalid: None,
            },
            status: None,
        };

        let conversions = convert_link_literal_uris(&mut link);
        assert_eq!(conversions, 3);
        assert_eq!(link.data.source, "literal:string:s");
        assert_eq!(link.data.predicate, Some("literal:string:p".to_string()));
        assert_eq!(link.data.target, "literal:string:t");
    }

    #[test]
    fn test_convert_link_literal_uris_none_needed() {
        let mut link = DecoratedLinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "ad4m://self".to_string(),
                predicate: Some("flux://has_channel".to_string()),
                target: "literal:string:already_canonical".to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "k".to_string(),
                signature: "s".to_string(),
                valid: None,
                invalid: None,
            },
            status: None,
        };

        let conversions = convert_link_literal_uris(&mut link);
        assert_eq!(conversions, 0);
        assert_eq!(link.data.source, "ad4m://self");
        assert_eq!(link.data.predicate, Some("flux://has_channel".to_string()));
        assert_eq!(link.data.target, "literal:string:already_canonical");
    }

    #[test]
    fn test_convert_link_literal_uris_none_predicate() {
        let mut link = DecoratedLinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "literal://string:s".to_string(),
                predicate: None,
                target: "literal://json:%7B%7D".to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "k".to_string(),
                signature: "s".to_string(),
                valid: None,
                invalid: None,
            },
            status: None,
        };

        let conversions = convert_link_literal_uris(&mut link);
        assert_eq!(conversions, 2);
        assert_eq!(link.data.source, "literal:string:s");
        assert_eq!(link.data.predicate, None);
        assert_eq!(link.data.target, "literal:json:%7B%7D");
    }

    // ── Migration tracking tests ──────────────────────────────────────

    #[test]
    fn test_migration_tracking() {
        ensure_db();

        let uuid = "test-migration-tracking";

        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(uuid)
                .expect("Failed to check migration status")
        });
        assert!(!is_migrated);

        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(uuid)
                .expect("Failed to mark as migrated")
        });

        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(uuid)
                .expect("Failed to check migration status")
        });
        assert!(is_migrated);

        // Idempotent
        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(uuid)
                .expect("Idempotent mark should not fail")
        });
    }

    #[test]
    fn test_delete_all_links_for_perspective() {
        ensure_db();

        let handle =
            crate::types::PerspectiveHandle::new_from_name("Test Delete Links SPARQL".to_string());

        let link1 = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://source1".to_string(),
                predicate: Some("test://pred".to_string()),
                target: "test://target1".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig1".to_string(),
                key: "key1".to_string(),
            },
            status: Some(LinkStatus::Local),
        };

        let link2 = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://source2".to_string(),
                predicate: Some("test://pred".to_string()),
                target: "test://target2".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig2".to_string(),
                key: "key2".to_string(),
            },
            status: Some(LinkStatus::Local),
        };

        Ad4mDb::with_global_instance(|db| {
            db.add_link(&handle.uuid, &link1, &LinkStatus::Local)
                .unwrap();
            db.add_link(&handle.uuid, &link2, &LinkStatus::Local)
                .unwrap();
        });

        let before = Ad4mDb::with_global_instance(|db| db.get_all_links(&handle.uuid).unwrap());
        assert_eq!(before.len(), 2);

        let deleted = Ad4mDb::with_global_instance(|db| {
            db.delete_all_links_for_perspective(&handle.uuid).unwrap()
        });
        assert_eq!(deleted, 2);

        let after = Ad4mDb::with_global_instance(|db| db.get_all_links(&handle.uuid).unwrap());
        assert_eq!(after.len(), 0);
    }

    // ── Full migration flow tests ─────────────────────────────────────

    #[test]
    fn test_migrate_empty_perspective_sparql() {
        ensure_db();

        let handle = crate::types::PerspectiveHandle::new_from_name(
            "Test Empty Migration SPARQL".to_string(),
        );

        let sparql = SparqlStore::new(None).expect("Failed to create SparqlStore");

        let result =
            migrate_links_from_rusqlite_to_sparql(&handle.uuid, &sparql).expect("Migration failed");

        assert_eq!(result.migrated, 0);
        assert_eq!(result.errors, 0);
        assert_eq!(result.literal_conversions, 0);

        // Should be marked as migrated
        let is_migrated =
            Ad4mDb::with_global_instance(|db| db.is_perspective_migrated(&handle.uuid).unwrap());
        assert!(is_migrated);
    }

    #[test]
    fn test_migrate_with_literal_conversion() {
        ensure_db();

        let handle = crate::types::PerspectiveHandle::new_from_name(
            "Test Literal Conversion Migration".to_string(),
        );

        // Add links with legacy literal:// URIs to Rusqlite
        let link_with_old_literals = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "literal://string:hello".to_string(),
                predicate: Some("flux://body".to_string()),
                target: "literal://json:%7B%22msg%22%3A%22hi%22%7D".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig".to_string(),
                key: "key".to_string(),
            },
            status: Some(LinkStatus::Shared),
        };

        let link_already_canonical = LinkExpression {
            author: "did:test:bob".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "ad4m://self".to_string(),
                predicate: Some("flux://has_channel".to_string()),
                target: "literal:string:already_good".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig2".to_string(),
                key: "key2".to_string(),
            },
            status: Some(LinkStatus::Shared),
        };

        Ad4mDb::with_global_instance(|db| {
            db.add_link(&handle.uuid, &link_with_old_literals, &LinkStatus::Shared)
                .unwrap();
            db.add_link(&handle.uuid, &link_already_canonical, &LinkStatus::Shared)
                .unwrap();
        });

        let sparql = SparqlStore::new(None).expect("Failed to create SparqlStore");

        let result =
            migrate_links_from_rusqlite_to_sparql(&handle.uuid, &sparql).expect("Migration failed");

        assert_eq!(result.migrated, 2);
        assert_eq!(result.errors, 0);
        // 2 conversions: source + target of first link
        assert_eq!(result.literal_conversions, 2);

        // Verify links in SPARQL store have converted URIs
        let sparql_links = sparql.get_all_links().expect("Failed to get SPARQL links");
        assert_eq!(sparql_links.len(), 2);

        // Find the converted link
        let converted = sparql_links
            .iter()
            .find(|l| l.data.source == "literal:string:hello")
            .expect("Should find link with converted source URI");
        assert_eq!(
            converted.data.target,
            "literal:json:%7B%22msg%22%3A%22hi%22%7D"
        );

        // Find the already-canonical link
        let canonical = sparql_links
            .iter()
            .find(|l| l.data.source == "ad4m://self")
            .expect("Should find link with canonical source URI");
        assert_eq!(canonical.data.target, "literal:string:already_good");

        // Rusqlite should be empty
        let remaining = Ad4mDb::with_global_instance(|db| db.get_all_links(&handle.uuid).unwrap());
        assert_eq!(remaining.len(), 0);

        // Should be marked as migrated
        let is_migrated =
            Ad4mDb::with_global_instance(|db| db.is_perspective_migrated(&handle.uuid).unwrap());
        assert!(is_migrated);
    }

    #[test]
    fn test_migrate_idempotent() {
        ensure_db();

        let handle =
            crate::types::PerspectiveHandle::new_from_name("Test Empty Migration".to_string());

        let link = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "literal://string:test".to_string(),
                predicate: Some("test://pred".to_string()),
                target: "test://target".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig".to_string(),
                key: "key".to_string(),
            },
            status: Some(LinkStatus::Local),
        };

        Ad4mDb::with_global_instance(|db| {
            db.add_link(&handle.uuid, &link, &LinkStatus::Local)
                .unwrap();
        });

        let sparql = SparqlStore::new(None).expect("Failed to create SparqlStore");

        // First migration
        let result1 = migrate_links_from_rusqlite_to_sparql(&handle.uuid, &sparql)
            .expect("First migration failed");
        assert_eq!(result1.migrated, 1);
        assert_eq!(result1.literal_conversions, 1);

        // Second migration — should be a no-op
        let result2 = migrate_links_from_rusqlite_to_sparql(&handle.uuid, &sparql)
            .expect("Second migration failed");
        assert_eq!(result2.migrated, 0);
        assert_eq!(result2.errors, 0);
        assert_eq!(result2.literal_conversions, 0);

        // SPARQL store should still have the link with converted URI
        let sparql_links = sparql.get_all_links().expect("Failed to get SPARQL links");
        assert!(
            sparql_links.len() >= 1,
            "Should have at least 1 link in SPARQL"
        );
        assert!(
            sparql_links
                .iter()
                .any(|l| l.data.source == "literal:string:test"),
            "Should find the converted link"
        );
    }
}
