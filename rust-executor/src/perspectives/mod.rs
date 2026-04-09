pub mod migration;
pub mod perspective_instance;
pub mod sdna;
pub mod shacl_parser;
pub mod shacl_to_prolog;
<<<<<<< HEAD
pub mod sparql_store;
pub mod utils;
=======
pub mod utils; // TODO: Remove this module after all users have migrated to SurrealDB
>>>>>>> origin/feat/audio-transport-optimisation
use crate::types::{
    LinkQuery, LinkStatus, NeighbourhoodSignalFilter, PerspectiveExpression, PerspectiveHandle,
    PerspectiveRemovedWithOwner, PerspectiveState, PerspectiveWithOwner,
};
use lazy_static::lazy_static;
use perspective_instance::PerspectiveInstance;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::RwLock;

use crate::db::Ad4mDb;
use crate::pubsub::{
    get_global_pubsub, get_global_pubsub_sync, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_ADDED_TOPIC,
    PERSPECTIVE_REMOVED_TOPIC, PERSPECTIVE_UPDATED_TOPIC,
};
use crate::types::{LinkExpression, PerspectiveDiff};

lazy_static! {
    static ref PERSPECTIVES: RwLock<HashMap<String, RwLock<PerspectiveInstance>>> =
        RwLock::new(HashMap::new());
    static ref APP_DATA_PATH: RwLock<Option<String>> = RwLock::new(None);
    /// Cache mapping link_language address → PerspectiveHandle for fast signal routing
    static ref LINK_LANG_TO_PERSPECTIVE_HANDLE: RwLock<HashMap<String, PerspectiveHandle>> =
        RwLock::new(HashMap::new());
}

/// Set the application data path for perspective storage
///
/// This must be called before creating perspectives to enable file-based storage.
/// Each perspective will create its own SPARQL (Oxigraph) store in memory.
///
/// # Arguments
/// * `path` - The base application data directory path
pub fn set_app_data_path(path: String) {
    let mut data_path = APP_DATA_PATH.write().unwrap();
    *data_path = Some(path);
}

/// Get the configured application data path
///
/// # Returns
/// * `Some(String)` - The configured app data path if set
/// * `None` - No app data path configured (will use in-memory storage)
pub(crate) fn get_app_data_path() -> Option<String> {
    APP_DATA_PATH.read().unwrap().clone()
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SerializedPerspective {
    #[serde(flatten)]
    pub handle: PerspectiveHandle,
    pub links: Vec<LinkExpression>,
}

pub fn initialize_from_db() {
    let handles = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_perspectives()
        .expect("Couldn't get perspectives from db");

    for handle in handles {
        let handle_clone = handle.clone();

        // Check if perspective already exists (initial quick check)
        {
            let perspectives = PERSPECTIVES.read().unwrap();
            if perspectives.contains_key(&handle_clone.uuid) {
                log::debug!(
                    "Perspective {} already initialized, skipping",
                    handle_clone.uuid
                );
                continue;
            }
        }

        // Spawn async task to initialize perspective
        tokio::spawn(async move {
            let p = PerspectiveInstance::new(handle_clone.clone(), None);

            // Run literal:// → literal: URI migration (idempotent)
            match migration::migrate_links_from_rusqlite_to_sparql(
                &handle_clone.uuid,
                &p.sparql_store,
            ) {
                Ok(result) if result.migrated > 0 => {
                    log::info!(
                        "🔄 Migration for {}: {} migrated, {} literal conversions",
                        handle_clone.uuid,
                        result.migrated,
                        result.literal_conversions
                    );
                }
                Ok(_) => {} // Already migrated or nothing to migrate
                Err(e) => log::warn!("Migration check for {}: {}", handle_clone.uuid, e),
            }

            // Rebuild SPARQL index from existing links
            // Skip SPARQL rebuild if persistent store already has data
            if p.sparql_store.has_data() {
                log::info!(
                    "✅ SPARQL store for perspective {} already has data, skipping rebuild",
                    handle_clone.uuid
                );
            } else {
                match p.get_links(&LinkQuery::default()).await {
                    Ok(links) => {
                        if !links.is_empty() {
                            log::info!(
                                "🔄 SPARQL REBUILD: Syncing {} links for perspective {}",
                                links.len(),
                                handle_clone.uuid
                            );
                            if let Err(e) = p.sync_existing_links_to_sparql(&links) {
                                log::error!(
                                    "Failed to sync links to SPARQL for perspective {}: {}",
                                    handle_clone.uuid,
                                    e
                                );
                            }
                        }
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to get links for SPARQL sync in perspective {}: {}",
                            handle_clone.uuid,
                            e
                        );
                    }
                }
            }

            // Atomically check-and-insert to prevent race condition
            // (In case multiple initializations were spawned before any completed)
            let should_start_tasks = {
                let mut perspectives = PERSPECTIVES.write().unwrap();
                if perspectives.contains_key(&handle_clone.uuid) {
                    log::warn!(
                        "Perspective {} was initialized by another task, discarding duplicate",
                        handle_clone.uuid
                    );
                    false
                } else {
                    perspectives.insert(handle_clone.uuid.clone(), RwLock::new(p.clone()));
                    true
                }
            };

            if should_start_tasks {
                // Start background tasks
                tokio::spawn(p.start_background_tasks());
            }
        });
    }
}

pub async fn add_perspective(
    handle: PerspectiveHandle,
    created_from_join: Option<bool>,
) -> Result<(), String> {
    if PERSPECTIVES.read().unwrap().contains_key(&handle.uuid) {
        return Err(format!(
            "Perspective with uuid {} already exists",
            &handle.uuid
        ));
    }

    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_perspective(&handle)
        .map_err(|e| e.to_string())?;

    let p = PerspectiveInstance::new(handle.clone(), created_from_join);
    tokio::spawn(p.clone().start_background_tasks());

    {
        let mut perspectives = PERSPECTIVES.write().unwrap();
        perspectives.insert(handle.uuid.clone(), RwLock::new(p));
    }

    // Publish one event per owner so each user gets their own notification
    let pubsub = get_global_pubsub().await;
    let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

    if let Some(owners) = owners_list {
        for owner in owners {
            let perspective_with_owner = PerspectiveWithOwner {
                perspective: handle.clone(),
                owner: owner.clone(),
            };
            pubsub
                .publish(
                    &PERSPECTIVE_ADDED_TOPIC,
                    &serde_json::to_string(&perspective_with_owner).unwrap(),
                )
                .await;
        }
    } else {
        // For perspectives without explicit owners (main agent), publish with main agent DID
        let main_agent_did = crate::agent::did();
        let perspective_with_owner = PerspectiveWithOwner {
            perspective: handle.clone(),
            owner: main_agent_did,
        };
        pubsub
            .publish(
                &PERSPECTIVE_ADDED_TOPIC,
                &serde_json::to_string(&perspective_with_owner).unwrap(),
            )
            .await;
    }
    Ok(())
}

pub fn all_perspectives() -> Vec<PerspectiveInstance> {
    PERSPECTIVES
        .read()
        .expect("Couldn't get read lock on PERSPECTIVES")
        .values()
        .map(|lock| {
            lock.read()
                .expect("Couldn't get read lock on PerspectiveInstance")
                .clone()
        })
        .collect()
}

pub fn get_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    PERSPECTIVES
        .read()
        .expect("Couldn't get read lock on PERSPECTIVES")
        .get(uuid)
        .map(|lock| {
            lock.read()
                .expect("Couldn't get read lock on PerspectiveInstance")
                .clone()
        })
}

pub async fn update_perspective(handle: &PerspectiveHandle) -> Result<(), String> {
    {
        if PERSPECTIVES.read().unwrap().get(&handle.uuid).is_none() {
            return Err(format!("Perspective with uuid {} not found", &handle.uuid));
        }

        let instance = PERSPECTIVES
            .read()
            .unwrap()
            .get(&handle.uuid)
            .unwrap()
            .read()
            .unwrap()
            .clone();

        instance.update_from_handle(handle.clone()).await;

        Ad4mDb::with_global_instance(|db| {
            db.update_perspective(handle).map_err(|e| e.to_string())
        })?;

        // Update the signal routing cache if this perspective has a link language.
        // Without this, the cached PerspectiveHandle retains a stale owners list,
        // causing signals (especially broadcasts) to not be delivered to owners
        // added after the cache was first populated (e.g. managed users joining
        // an existing neighbourhood).
        if let Some(nh) = &handle.neighbourhood {
            register_link_language_perspective(nh.data.link_language.clone(), handle.clone());
        }
    }

    // Publish one event per owner so each user gets their own notification
    let pubsub = get_global_pubsub().await;
    let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

    if let Some(owners) = owners_list {
        for owner in owners {
            let perspective_with_owner = PerspectiveWithOwner {
                perspective: handle.clone(),
                owner: owner.clone(),
            };
            pubsub
                .publish(
                    &PERSPECTIVE_UPDATED_TOPIC,
                    &serde_json::to_string(&perspective_with_owner).unwrap(),
                )
                .await;
        }
    } else {
        // For perspectives without explicit owners (main agent), publish with main agent DID
        let main_agent_did = crate::agent::did();
        let perspective_with_owner = PerspectiveWithOwner {
            perspective: handle.clone(),
            owner: main_agent_did,
        };
        pubsub
            .publish(
                &PERSPECTIVE_UPDATED_TOPIC,
                &serde_json::to_string(&perspective_with_owner).unwrap(),
            )
            .await;
    }
    Ok(())
}

pub async fn remove_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    if let Err(e) = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_perspective(uuid)
    {
        log::error!("Error removing perspective from db: {}", e);
    }

    let removed_instance = {
        let mut perspectives = PERSPECTIVES.write().unwrap();
        perspectives
            .remove(uuid)
            .and_then(|instance_lock| instance_lock.into_inner().ok())
    };

    if let Some(ref instance) = removed_instance {
        instance.teardown_background_tasks().await;

        // Publish one removal event per owner so each user gets their own notification
        let handle = instance.persisted.lock().await.clone();
        let pubsub = get_global_pubsub().await;
        if let Some(owners) = &handle.owners {
            for owner in owners {
                let removed_with_owner = PerspectiveRemovedWithOwner {
                    uuid: uuid.to_string(),
                    owner: owner.clone(),
                };
                pubsub
                    .publish(
                        &PERSPECTIVE_REMOVED_TOPIC,
                        &serde_json::to_string(&removed_with_owner).unwrap(),
                    )
                    .await;
            }
        }
    }

    removed_instance
}

pub fn handle_perspective_diff_from_link_language(diff: PerspectiveDiff, language_address: String) {
    tokio::spawn(handle_perspective_diff_from_link_language_impl(
        diff,
        language_address,
    ));
}

/// Register a mapping from link_language address to PerspectiveHandle for fast signal routing
pub fn register_link_language_perspective(language_address: String, handle: PerspectiveHandle) {
    LINK_LANG_TO_PERSPECTIVE_HANDLE
        .write()
        .unwrap()
        .insert(language_address, handle);
}

/// Look up PerspectiveHandle by link language address from cache
fn cached_perspective_handle(language_address: &str) -> Option<PerspectiveHandle> {
    LINK_LANG_TO_PERSPECTIVE_HANDLE
        .read()
        .unwrap()
        .get(language_address)
        .cloned()
}

async fn perspective_by_link_language(language_address: String) -> Option<PerspectiveInstance> {
    // Fast path: check cache first
    if let Some(handle) = cached_perspective_handle(&language_address) {
        if let Some(perspective) = get_perspective(&handle.uuid) {
            return Some(perspective);
        }
    }

    // Slow path: iterate all perspectives (and populate cache)
    let perspectives = PERSPECTIVES
        .read()
        .unwrap()
        .values()
        .map(|lock| lock.read().unwrap().clone())
        .collect::<Vec<_>>();
    for perspective in perspectives.into_iter() {
        let handle = perspective.persisted.lock().await.clone();

        if let Some(nh) = &handle.neighbourhood {
            if nh.data.link_language == language_address {
                // Populate cache for future lookups
                register_link_language_perspective(language_address, handle);
                return Some(perspective);
            }
        }
    }
    None
}

pub async fn handle_perspective_diff_from_link_language_impl(
    diff: PerspectiveDiff,
    language_address: String,
) {
    if let Some(perspective) = perspective_by_link_language(language_address.clone()).await {
        if let Err(e) = perspective.diff_from_link_language(diff).await {
            log::error!(
                "Failed to persist diff from link language ({}): {:?}",
                language_address,
                e
            );
        }
    }
}

pub fn handle_sync_state_changed_from_link_language(
    state: PerspectiveState,
    language_address: String,
) {
    tokio::spawn(handle_sync_state_changed_from_link_language_impl(
        state,
        language_address,
    ));
}

pub async fn handle_sync_state_changed_from_link_language_impl(
    state: PerspectiveState,
    language_address: String,
) {
    if let Some(perspective) = perspective_by_link_language(language_address.clone()).await {
        match perspective.update_perspective_state(state).await {
            Ok(_) => (),
            Err(e) => log::error!("Error updating perspective state from link language: {}", e),
        }
    }
}

pub fn handle_telepresence_signal_from_link_language(
    mut signal: PerspectiveExpression,
    language_address: String,
    recipient_did: Option<String>,
) {
    // Fast path: if we have a cached PerspectiveHandle, publish directly to PubSub
    // without going through the expensive perspective_by_link_language async lookup.
    // Uses synchronous publish to avoid Tokio task scheduling contention that can
    // delay spawned tasks by seconds when the executor is busy with Deno operations.
    if let Some(handle) = cached_perspective_handle(&language_address) {
        signal.verify_signatures();
        publish_telepresence_signal_sync(handle, signal, recipient_did);
    } else {
        // Slow path: need to look up perspective by language address
        tokio::spawn(handle_telepresence_signal_from_link_language_impl(
            signal,
            language_address,
            recipient_did,
        ));
    }
}

/// Publish a telepresence signal to PubSub for delivery to REST subscribers
pub(crate) async fn publish_telepresence_signal(
    handle: PerspectiveHandle,
    signal: PerspectiveExpression,
    recipient_did: Option<String>,
) {
    if let Some(recipient) = recipient_did {
        get_global_pubsub()
            .await
            .publish(
                &NEIGHBOURHOOD_SIGNAL_TOPIC,
                &serde_json::to_string(&NeighbourhoodSignalFilter {
                    perspective: handle,
                    signal,
                    recipient: Some(recipient),
                })
                .unwrap(),
            )
            .await;
    } else if let Some(owners) = handle.owners.as_ref().filter(|o| !o.is_empty()) {
        for owner_did in owners {
            get_global_pubsub()
                .await
                .publish(
                    &NEIGHBOURHOOD_SIGNAL_TOPIC,
                    &serde_json::to_string(&NeighbourhoodSignalFilter {
                        perspective: handle.clone(),
                        signal: signal.clone(),
                        recipient: Some(owner_did.clone()),
                    })
                    .unwrap(),
                )
                .await;
        }
    } else {
        get_global_pubsub()
            .await
            .publish(
                &NEIGHBOURHOOD_SIGNAL_TOPIC,
                &serde_json::to_string(&NeighbourhoodSignalFilter {
                    perspective: handle,
                    signal,
                    recipient: None,
                })
                .unwrap(),
            )
            .await;
    }
}

/// Synchronous version of publish_telepresence_signal for use in Deno op context
/// where tokio::spawn tasks get delayed by Tokio scheduler contention.
fn publish_telepresence_signal_sync(
    handle: PerspectiveHandle,
    signal: PerspectiveExpression,
    recipient_did: Option<String>,
) {
    let pubsub = get_global_pubsub_sync();
    if let Some(recipient) = recipient_did {
        pubsub.publish_sync(
            &NEIGHBOURHOOD_SIGNAL_TOPIC,
            &serde_json::to_string(&NeighbourhoodSignalFilter {
                perspective: handle,
                signal,
                recipient: Some(recipient),
            })
            .unwrap(),
        );
    } else if let Some(owners) = &handle.owners {
        for owner_did in owners {
            pubsub.publish_sync(
                &NEIGHBOURHOOD_SIGNAL_TOPIC,
                &serde_json::to_string(&NeighbourhoodSignalFilter {
                    perspective: handle.clone(),
                    signal: signal.clone(),
                    recipient: Some(owner_did.clone()),
                })
                .unwrap(),
            );
        }
    } else {
        pubsub.publish_sync(
            &NEIGHBOURHOOD_SIGNAL_TOPIC,
            &serde_json::to_string(&NeighbourhoodSignalFilter {
                perspective: handle,
                signal,
                recipient: None,
            })
            .unwrap(),
        );
    }
}

pub async fn handle_telepresence_signal_from_link_language_impl(
    signal: PerspectiveExpression,
    language_address: String,
    recipient_did: Option<String>,
) {
    if let Some(perspective) = perspective_by_link_language(language_address.clone()).await {
        perspective
            .telepresence_signal_from_link_language(signal, recipient_did)
            .await;
    }
}

pub async fn export_perspective(uuid: &str) -> Result<SerializedPerspective, String> {
    let perspective =
        get_perspective(uuid).ok_or_else(|| format!("Perspective not found: {}", uuid))?;
    let handle = perspective.persisted.lock().await.clone();

    let decorated_links = perspective
        .get_links(&LinkQuery::default())
        .await
        .map_err(|e| e.to_string())?;
    let links = decorated_links.into_iter().map(|l| l.into()).collect();

    Ok(SerializedPerspective {
        handle: handle.clone(),
        links,
    })
}

pub async fn import_perspective(
    instance: SerializedPerspective,
) -> Result<PerspectiveHandle, String> {
    // First check if perspective exists
    let existing = get_perspective(&instance.handle.uuid);

    if existing.is_none() {
        // Create new perspective only if it doesn't exist
        add_perspective(instance.handle.clone(), None)
            .await
            .map_err(|e| format!("Failed to create perspective: {}", e))?;
    }

    // Add all links directly to SPARQL store to preserve original authorship
    let perspective = get_perspective(&instance.handle.uuid)
        .ok_or_else(|| "Perspective not found after creation".to_string())?;

    let decorated_links: Vec<crate::types::DecoratedLinkExpression> = instance
        .links
        .into_iter()
        .map(|link| {
            let status = link.status.clone().unwrap_or(LinkStatus::Local);
            crate::types::DecoratedLinkExpression::from((link, status))
        })
        .collect();

    let diff = crate::types::DecoratedPerspectiveDiff {
        additions: decorated_links,
        removals: vec![],
    };

    // Write to SPARQL store
    perspective
        .persist_link_diff(&diff)
        .await
        .map_err(|e| format!("Failed to persist link diff to SPARQL store: {}", e))?;

    Ok(instance.handle)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ExpressionProof, Link};
    use chrono::Utc;

    fn setup() {
        //setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
    }

    async fn find_perspective_by_uuid(
        all_perspectives: &Vec<PerspectiveInstance>,
        uuid: &String,
    ) -> Option<PerspectiveInstance> {
        for p in all_perspectives {
            if p.persisted.lock().await.uuid == *uuid {
                return Some(p.clone());
            }
        }

        None
    }

    #[tokio::test]
    async fn test_perspective_persistence_roundtrip() {
        setup();
        assert!(all_perspectives().is_empty());

        let handle1 = PerspectiveHandle::new_from_name("Test Perspective 1".to_string());
        let handle2 = PerspectiveHandle::new_from_name("Test Perspective 2".to_string());

        add_perspective(handle1.clone(), None)
            .await
            .expect("Failed to add perspective");
        add_perspective(handle2.clone(), None)
            .await
            .expect("Failed to add perspective");
        // Test the get_all_perspectives function
        let perspectives = all_perspectives();

        // Assert expected results
        assert_eq!(perspectives.len(), 2);

        assert!(find_perspective_by_uuid(&perspectives, &handle1.uuid)
            .await
            .is_some());
        assert!(find_perspective_by_uuid(&perspectives, &handle2.uuid)
            .await
            .is_some());

        let p1 = find_perspective_by_uuid(&perspectives, &handle1.uuid)
            .await
            .expect("Failed to find perspective by uuid");
        assert_eq!(
            p1.persisted.lock().await.name,
            Some("Test Perspective 1".to_string())
        );

        let mut handle_updated = handle1.clone();
        handle_updated.name = Some("Test Perspective 1 Updated".to_string());
        update_perspective(&handle_updated)
            .await
            .expect("Failed to update perspective");

        let p1_updated = get_perspective(&handle1.uuid).unwrap();
        assert_eq!(
            p1_updated.persisted.lock().await.name,
            Some("Test Perspective 1 Updated".to_string())
        );

        let perspectives = all_perspectives();
        assert_eq!(perspectives.len(), 2);
        let p1_updated_from_all = find_perspective_by_uuid(&perspectives, &handle1.uuid)
            .await
            .expect("Failed to find perspective by uuid");
        assert_eq!(
            p1_updated_from_all.persisted.lock().await.name,
            Some("Test Perspective 1 Updated".to_string())
        );

        // Clean up test perspectives
        remove_perspective(handle1.uuid.as_str()).await;
        let perspectives = all_perspectives();
        assert_eq!(perspectives.len(), 1);
        assert!(find_perspective_by_uuid(&perspectives, &handle2.uuid)
            .await
            .is_some());
    }

    #[tokio::test]
    async fn test_perspective_import_export() {
        setup();

        // Create a test perspective with some links
        let handle = PerspectiveHandle::new_from_name("Test Import/Export".to_string());
        add_perspective(handle.clone(), None)
            .await
            .expect("Failed to add perspective");
        let mut perspective = get_perspective(&handle.uuid).unwrap();

        // Add some test links to the perspective
        let test_link = LinkExpression {
            author: "did:test:author".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            data: Link {
                source: "test://source".to_string(),
                predicate: Some("test://predicate".to_string()),
                target: "test://target".to_string(),
            },
            proof: ExpressionProof {
                key: "test-key".to_string(),
                signature: "test-signature".to_string(),
            },
            status: Some(LinkStatus::Local),
        };
        println!("test_link: {:?}", test_link);

        perspective
            .add_link_expression(test_link.clone(), LinkStatus::Local, None)
            .await
            .expect("Failed to add link");

        println!("test_link added");

        // Export the perspective
        let exported = export_perspective(&handle.uuid)
            .await
            .expect("Failed to export perspective");

        println!("exported: {:?}", exported);
        // Verify exported data
        assert_eq!(exported.handle.uuid, handle.uuid);
        assert_eq!(exported.links.len(), 1);
        assert_eq!(exported.links[0].author, test_link.author);
        assert_eq!(exported.links[0].data.source, test_link.data.source);

        // Remove original perspective
        remove_perspective(&handle.uuid).await;
        assert!(get_perspective(&handle.uuid).is_none());

        // Import the perspective back
        let imported_handle = import_perspective(exported)
            .await
            .expect("Failed to import perspective");

        // Verify imported perspective
        assert_eq!(imported_handle.uuid, handle.uuid);

        // Verify imported links
        let imported_perspective =
            get_perspective(&handle.uuid).expect("Failed to get imported perspective");

        let imported_links = imported_perspective
            .get_links(&LinkQuery::default())
            .await
            .expect("Failed to get imported links");

        assert_eq!(imported_links.len(), 1);
        assert_eq!(imported_links[0].author, test_link.author);
        assert_eq!(imported_links[0].data.source, test_link.data.source);
        assert_eq!(imported_links[0].proof.signature, test_link.proof.signature);

        remove_perspective(&handle.uuid).await;
    }

    // Migration tests have been moved to src/perspectives/migration.rs

    // Additional tests for other functions can be added here
}
