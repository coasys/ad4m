/// One-time migration module to move links from Rusqlite to SurrealDB
///
/// This module can be safely removed in future versions after all users have migrated.
///
/// To remove this module:
/// 1. Delete this file (src/perspectives/migration.rs)
/// 2. Remove the `pub mod migration;` line from src/perspectives/mod.rs
/// 3. Remove migration calls from initialize_from_db() and add_perspective()
/// 4. Optionally remove migration-related methods from src/db.rs:
///    - is_perspective_migrated()
///    - mark_perspective_as_migrated()
///    - delete_all_links_for_perspective()
///    - perspective_link_migration table
use crate::db::Ad4mDb;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression};

/// Migrate all links for a perspective from Rusqlite to SurrealDB
///
/// This is a one-time migration that:
/// 1. Checks if the perspective has already been migrated
/// 2. Loads all links from Rusqlite
/// 3. Adds them to SurrealDB
/// 4. Marks the perspective as migrated
/// 5. Deletes the links from Rusqlite
///
/// This function is idempotent - it can be safely called multiple times.
pub async fn migrate_links_from_rusqlite_to_surrealdb(
    perspective_uuid: &str,
    surreal_service: &crate::surreal_service::SurrealDBService,
) -> Result<(), String> {
    // Check if already migrated
    let already_migrated = Ad4mDb::with_global_instance(|db| {
        db.is_perspective_migrated(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    if already_migrated {
        log::debug!(
            "Perspective {} already migrated, skipping migration",
            perspective_uuid
        );
        return Ok(());
    }

    log::info!(
        "Starting link migration from Rusqlite to SurrealDB for perspective {}",
        perspective_uuid
    );

    // Get all links from Rusqlite
    let links = Ad4mDb::with_global_instance(|db| {
        db.get_all_links(perspective_uuid)
            .map_err(|e| e.to_string())
    })?;

    if links.is_empty() {
        log::debug!("No links to migrate for perspective {}", perspective_uuid);
        // Mark as migrated even if no links
        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(perspective_uuid)
                .map_err(|e| e.to_string())
        })?;
        return Ok(());
    }

    log::info!(
        "Migrating {} links for perspective {}",
        links.len(),
        perspective_uuid
    );

    // Convert LinkExpression to DecoratedLinkExpression and add to SurrealDB
    let mut migrated_count = 0;
    let mut error_count = 0;

    for (link_expr, status) in &links {
        let decorated_link = DecoratedLinkExpression {
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

        match surreal_service
            .add_link(perspective_uuid, &decorated_link)
            .await
        {
            Ok(_) => {
                migrated_count += 1;
            }
            Err(e) => {
                log::error!(
                    "Failed to migrate link for perspective {}: {}",
                    perspective_uuid,
                    e
                );
                error_count += 1;
            }
        }
    }

    log::info!(
        "Migration completed for perspective {}: {} links migrated, {} errors",
        perspective_uuid,
        migrated_count,
        error_count
    );

    // Only mark as migrated and delete if no errors occurred
    if error_count > 0 {
        return Err(format!(
            "Migration failed for perspective {}: {} out of {} links failed to migrate",
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

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphql::graphql_types::LinkStatus;
    use crate::types::{ExpressionProof, Link, LinkExpression};
    use chrono::Utc;

    fn setup() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
    }

    #[tokio::test]
    async fn test_migration_tracking() {
        setup();

        let perspective_uuid = "test-migration-uuid";

        // Test that perspective is not migrated initially
        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(perspective_uuid)
                .expect("Failed to check migration status")
        });
        assert!(!is_migrated, "Perspective should not be migrated initially");

        // Mark perspective as migrated
        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(perspective_uuid)
                .expect("Failed to mark perspective as migrated")
        });

        // Test that perspective is now migrated
        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(perspective_uuid)
                .expect("Failed to check migration status")
        });
        assert!(is_migrated, "Perspective should be migrated");

        // Test idempotency - marking again should not fail
        Ad4mDb::with_global_instance(|db| {
            db.mark_perspective_as_migrated(perspective_uuid)
                .expect("Failed to mark perspective as migrated (idempotent)")
        });

        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(perspective_uuid)
                .expect("Failed to check migration status")
        });
        assert!(is_migrated, "Perspective should still be migrated");
    }

    #[tokio::test]
    async fn test_delete_all_links_for_perspective() {
        setup();

        let handle = crate::graphql::graphql_types::PerspectiveHandle::new_from_name(
            "Test Delete Links".to_string(),
        );

        // Add some links directly to Rusqlite (simulating old data)
        let test_link_1 = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://source1".to_string(),
                predicate: Some("test://predicate".to_string()),
                target: "test://target1".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig1".to_string(),
                key: "key1".to_string(),
            },
            status: Some(LinkStatus::Local),
        };

        let test_link_2 = LinkExpression {
            author: "did:test:alice".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://source2".to_string(),
                predicate: Some("test://predicate".to_string()),
                target: "test://target2".to_string(),
            },
            proof: ExpressionProof {
                signature: "sig2".to_string(),
                key: "key2".to_string(),
            },
            status: Some(LinkStatus::Local),
        };

        Ad4mDb::with_global_instance(|db| {
            db.add_link(&handle.uuid, &test_link_1, &LinkStatus::Local)
                .expect("Failed to add test link 1");
            db.add_link(&handle.uuid, &test_link_2, &LinkStatus::Local)
                .expect("Failed to add test link 2");
        });

        // Verify links were added
        let links_before = Ad4mDb::with_global_instance(|db| {
            db.get_all_links(&handle.uuid).expect("Failed to get links")
        });
        assert_eq!(links_before.len(), 2, "Should have 2 links before deletion");

        // Delete all links
        let deleted_count = Ad4mDb::with_global_instance(|db| {
            db.delete_all_links_for_perspective(&handle.uuid)
                .expect("Failed to delete links")
        });
        assert_eq!(deleted_count, 2, "Should have deleted 2 links");

        // Verify links were deleted
        let links_after = Ad4mDb::with_global_instance(|db| {
            db.get_all_links(&handle.uuid).expect("Failed to get links")
        });
        assert_eq!(links_after.len(), 0, "Should have 0 links after deletion");
    }

    #[tokio::test]
    async fn test_full_migration_flow() {
        setup();

        // Use a unique UUID to avoid conflicts with other tests
        let unique_name = format!("Test Full Migration {}", uuid::Uuid::new_v4());
        let handle = crate::graphql::graphql_types::PerspectiveHandle::new_from_name(unique_name);

        // Add some links to Rusqlite (simulating old data)
        let test_link = LinkExpression {
            author: "did:test:bob".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://migration/source".to_string(),
                predicate: Some("test://migration/predicate".to_string()),
                target: "test://migration/target".to_string(),
            },
            proof: ExpressionProof {
                signature: "migration_sig".to_string(),
                key: "migration_key".to_string(),
            },
            status: Some(LinkStatus::Shared),
        };

        Ad4mDb::with_global_instance(|db| {
            db.add_link(&handle.uuid, &test_link, &LinkStatus::Shared)
                .expect("Failed to add test link");
        });

        // Verify link is in Rusqlite
        let rusqlite_links_before = Ad4mDb::with_global_instance(|db| {
            db.get_all_links(&handle.uuid)
                .expect("Failed to get links from Rusqlite")
        });
        assert_eq!(
            rusqlite_links_before.len(),
            1,
            "Should have 1 link in Rusqlite before migration"
        );

        // Verify perspective is not migrated
        let is_migrated_before = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(&handle.uuid)
                .expect("Failed to check migration status")
        });
        assert!(
            !is_migrated_before,
            "Perspective should not be migrated before migration"
        );

        // Create SurrealDB service with unique database name for isolation
        let db_name = format!(
            "test_migration_{}",
            uuid::Uuid::new_v4().to_string().replace("-", "")
        );
        let surreal_service = crate::surreal_service::SurrealDBService::new("ad4m", &db_name, None)
            .await
            .expect("Failed to create SurrealDB service");

        // Run migration
        migrate_links_from_rusqlite_to_surrealdb(&handle.uuid, &surreal_service)
            .await
            .expect("Migration failed");

        // Verify links are in SurrealDB
        let surreal_links = surreal_service
            .get_all_links(&handle.uuid)
            .await
            .expect("Failed to get links from SurrealDB");
        assert_eq!(
            surreal_links.len(),
            1,
            "Should have 1 link in SurrealDB after migration"
        );
        assert_eq!(
            surreal_links[0].author, test_link.author,
            "Author should match"
        );
        assert_eq!(
            surreal_links[0].data.source, test_link.data.source,
            "Source should match"
        );
        assert_eq!(
            surreal_links[0].data.target, test_link.data.target,
            "Target should match"
        );
        assert_eq!(
            surreal_links[0].proof.signature, test_link.proof.signature,
            "Signature should match"
        );
        assert_eq!(
            surreal_links[0].status,
            Some(LinkStatus::Shared),
            "Status should match"
        );

        // Verify links are deleted from Rusqlite
        let rusqlite_links_after = Ad4mDb::with_global_instance(|db| {
            db.get_all_links(&handle.uuid)
                .expect("Failed to get links from Rusqlite")
        });
        assert_eq!(
            rusqlite_links_after.len(),
            0,
            "Should have 0 links in Rusqlite after migration"
        );

        // Verify perspective is marked as migrated
        let is_migrated_after = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(&handle.uuid)
                .expect("Failed to check migration status")
        });
        assert!(
            is_migrated_after,
            "Perspective should be migrated after migration"
        );

        // Run migration again to test idempotency
        migrate_links_from_rusqlite_to_surrealdb(&handle.uuid, &surreal_service)
            .await
            .expect("Second migration failed");

        // Verify links are still in SurrealDB and count hasn't changed
        let surreal_links_after_second = surreal_service
            .get_all_links(&handle.uuid)
            .await
            .expect("Failed to get links from SurrealDB after second migration");
        assert_eq!(
            surreal_links_after_second.len(),
            1,
            "Should still have exactly 1 link in SurrealDB after second migration"
        );
    }

    #[tokio::test]
    async fn test_migration_with_no_links() {
        setup();

        let handle = crate::graphql::graphql_types::PerspectiveHandle::new_from_name(
            "Test Empty Migration".to_string(),
        );

        // Create SurrealDB service with unique database name for isolation
        let db_name = format!(
            "test_empty_migration_{}",
            uuid::Uuid::new_v4().to_string().replace("-", "")
        );
        let surreal_service = crate::surreal_service::SurrealDBService::new("ad4m", &db_name, None)
            .await
            .expect("Failed to create SurrealDB service");

        // Run migration on perspective with no links
        migrate_links_from_rusqlite_to_surrealdb(&handle.uuid, &surreal_service)
            .await
            .expect("Migration failed for empty perspective");

        // Verify perspective is marked as migrated even with no links
        let is_migrated = Ad4mDb::with_global_instance(|db| {
            db.is_perspective_migrated(&handle.uuid)
                .expect("Failed to check migration status")
        });
        assert!(
            is_migrated,
            "Perspective should be marked as migrated even with no links"
        );

        // Verify no links in SurrealDB
        let surreal_links = surreal_service
            .get_all_links(&handle.uuid)
            .await
            .expect("Failed to get links from SurrealDB");
        assert_eq!(surreal_links.len(), 0, "Should have 0 links in SurrealDB");
    }
}
