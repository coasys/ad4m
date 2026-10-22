pub mod auto_processor;
pub(crate) mod content_address;
pub(crate) mod flow_classes;
pub(crate) mod flow_context;
pub(crate) mod flow_evaluator;
#[cfg(test)]
mod flow_evaluator_e2e;
pub(crate) mod flow_instance;
#[cfg(test)]
mod flow_instance_e2e;
pub(crate) mod flow_semantic_check;
pub(crate) mod flow_spawn;
pub(crate) mod hardwired_class;
pub(crate) mod hosted_status;
pub mod interpretation;
#[cfg(test)]
mod interpretation_e2e;
#[cfg(test)]
mod interpretation_harness_e2e;
// `pub(crate)` so test modules outside `perspectives` (e.g. the MCP flow
// tools, which read flow state through the same loaders) can seed a real
// `PerspectiveInstance` instead of duplicating the setup. Still `#[cfg(test)]`,
// so it never reaches a release build.
#[cfg(test)]
pub(crate) mod interpretation_test_support;
pub mod memory_diagnostics;
pub mod migration;
pub mod model_query;
pub mod ordering;
pub mod perspective_instance;
#[cfg(test)]
mod read_back_round_trip_tests;
pub mod sdna;
pub mod shacl_parser;
pub mod shacl_to_prolog;
pub mod sparql_store;
pub mod subject_classes_of;
pub mod utils;
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

#[cfg(test)]
type RoutingCacheWriteHook = std::sync::Arc<dyn Fn() + Send + Sync>;

#[cfg(test)]
tokio::task_local! {
    /// Test seam: `register_link_language_perspective` runs this right before
    /// it writes the routing cache, so a test can run a concurrent handle
    /// change at exactly that point. Set per task, so only the task under test
    /// runs it.
    static BEFORE_ROUTING_CACHE_WRITE: RoutingCacheWriteHook;
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

            // No named-graph → reifier migration. The named-graph storage model
            // never shipped: `git tag --contains` on the commit that replaced it
            // (`7aeeb8982`) is empty, and the last release tag carrying
            // `perspectives/` has no `sparql_store.rs` at all. Carrying a
            // migration for a format nobody holds meant carrying a third
            // `proofValid` read path, and with it the "never evaluated" verdict
            // it decoded (#1046).

            // No literal-encoding migration on boot. A scalar rides the API as a
            // `literal:*` wire target and is stored as a native typed RDF literal
            // by the write path (`target_to_storage_term`, applied uniformly to
            // every link — model properties, raw `addLink`, and SHACLFlow
            // bookkeeping alike); reads render it back to the same wire form
            // (`storage_term_to_target_string`). Fresh writes therefore land in
            // the indexed shape by construction, so there is nothing to migrate.
            // A store-sweep migration would sit below this wire-format boundary
            // and could not tell a model property value from a SHACLFlow
            // bookkeeping link — see the git history of this file for the removed
            // v3/v4 sweeps. If real pre-typed-literal stores ever need upgrading,
            // reintroduce a migration keyed on SHACL property shapes, never a
            // blanket rewrite.

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

    // Sync perspective metadata to shared DB for cross-executor rehydration.
    // Store the creating executor's DID so other executors can fetch links
    // from the correct path (links are keyed by DID + perspective UUID).
    let config = crate::config::get_global_config();
    if config.db_backend.as_deref() == Some("shared") {
        let backend = crate::db_backend::db_backend();
        let creator_did = crate::agent::did();
        let meta = serde_json::json!({
            "uuid": &handle.uuid,
            "name": &handle.name,
            "owners": &handle.owners,
            "state": format!("{:?}", handle.state),
            "creator_did": &creator_did,
        });
        if let Err(e) = backend.upsert("shared:platform", "perspectives", &handle.uuid, meta) {
            log::warn!("Failed to sync perspective metadata to shared DB: {}", e);
        }
    }

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
                perspective_uuid: handle.uuid.clone(),
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
            perspective_uuid: handle.uuid.clone(),
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

/// End every live query the RPC connection `connection_id` opened.
pub async fn dispose_connection_subscriptions(connection_id: &str) {
    for perspective in all_perspectives() {
        perspective
            .dispose_connection_subscriptions(connection_id)
            .await;
    }
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

/// Register a fully-constructed PerspectiveInstance in the global map.
/// Used by the rehydration path (shared backend → local) to avoid exposing the PERSPECTIVES static.
pub(crate) fn register_perspective(uuid: String, instance: PerspectiveInstance) {
    let mut perspectives = PERSPECTIVES.write().unwrap();
    perspectives.insert(uuid, RwLock::new(instance));
}

/// Remove an instance from the global map without touching the persistence
/// backend. Test-only: lets fixtures that used `register_perspective` leave
/// global state clean for tests that assert on it.
#[cfg(test)]
pub(crate) fn unregister_perspective(uuid: &str) {
    PERSPECTIVES.write().unwrap().remove(uuid);
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

/// Applies `change` to a perspective's handle under the instance's lock, then
/// persists and announces the result. It is the only way to change a handle: a
/// change that wrote back a whole copy overwrote a concurrent one (users joining
/// one neighbourhood at once lost owners; a join wrote back over the state the
/// link language's install had just set).
pub async fn update_perspective_with(
    uuid: &str,
    change: impl FnOnce(&mut PerspectiveHandle),
) -> Result<PerspectiveHandle, String> {
    let instance =
        get_perspective(uuid).ok_or_else(|| format!("Perspective with uuid {} not found", uuid))?;
    let handle = {
        let mut persisted = instance.persisted.lock().await;
        let link_language = |h: &PerspectiveHandle| {
            h.neighbourhood
                .as_ref()
                .map(|nh| nh.data.link_language.clone())
        };
        let before = link_language(&persisted);
        change(&mut persisted);
        // A publish sets the link language: wake the loop that attaches it.
        if link_language(&persisted) != before {
            instance.link_language_changed();
        }
        Ad4mDb::with_global_instance(|db| {
            db.update_perspective(&persisted).map_err(|e| e.to_string())
        })?;
        // Signals reach owners through this cached copy, so it must not lag the handle
        // (users joining later got no broadcasts). Written under the lock, so a
        // concurrent change cannot register an older copy after a newer one.
        if let Some(nh) = &persisted.neighbourhood {
            register_link_language_perspective(nh.data.link_language.clone(), persisted.clone());
        }
        persisted.clone()
    };

    // Publish one event per owner so each user gets their own notification
    let pubsub = get_global_pubsub().await;
    let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

    if let Some(owners) = owners_list {
        for owner in owners {
            let perspective_with_owner = PerspectiveWithOwner {
                perspective_uuid: handle.uuid.clone(),
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
            perspective_uuid: handle.uuid.clone(),
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
    Ok(handle)
}

pub async fn remove_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    let removed_instance = {
        let mut perspectives = PERSPECTIVES.write().unwrap();
        perspectives
            .remove(uuid)
            .and_then(|instance_lock| instance_lock.into_inner().ok())
    };

    // Torn down BEFORE the rows are deleted. A flow pass still running holds
    // a clone of this instance, and every flow-bookkeeping write checks the
    // flag under the DB lock this delete takes (`FlowReplicaState::record_pass`).
    // So such a write either lands before the delete and is removed with
    // the rest, or sees the flag and writes nothing. Deleting first let it
    // write after the delete, leaving rows for a perspective that is gone.
    if let Some(ref instance) = removed_instance {
        instance.teardown_background_tasks().await;
    }

    if let Err(e) = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_perspective(uuid)
    {
        log::error!("Error removing perspective from db: {}", e);
    }

    if let Some(ref instance) = removed_instance {
        // Drop any link-language -> perspective cache entries that
        // pointed at this uuid. Without this, the cache keeps a
        // stale PerspectiveHandle forever; the slow-path fallback
        // self-heals for the async lookup but `publish_telepresence_signal_sync`
        // in handle_telepresence_signal_from_link_language uses the
        // cached handle DIRECTLY without verifying get_perspective
        // still returns Some, so signals would keep flowing for a
        // removed perspective until the cache entry was overwritten.
        {
            let handle_snapshot = instance.persisted.lock().await.clone();
            if let Some(nh) = &handle_snapshot.neighbourhood {
                let mut cache = LINK_LANG_TO_PERSPECTIVE_HANDLE.write().unwrap();
                cache.remove(&nh.data.link_language);
            }
        }

        // Clean up the per-perspective SPARQL (Oxigraph/RocksDB) directory
        if let Some(data_path) = get_app_data_path() {
            let db_path = std::path::Path::new(&data_path)
                .join("perspectives")
                .join(uuid);
            if db_path.exists() {
                if let Err(e) = std::fs::remove_dir_all(&db_path) {
                    log::warn!(
                        "Failed to remove SPARQL store directory for perspective {}: {}",
                        uuid,
                        e
                    );
                } else {
                    log::debug!("Cleaned up SPARQL store directory for perspective {}", uuid);
                }
            }
        }

        // Publish one removal event per owner so each user gets their own notification
        let handle = instance.persisted.lock().await.clone();
        let pubsub = get_global_pubsub().await;
        if let Some(owners) = &handle.owners {
            for owner in owners {
                let removed_with_owner = PerspectiveRemovedWithOwner {
                    perspective_uuid: uuid.to_string(),
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
    #[cfg(test)]
    if let Ok(hook) = BEFORE_ROUTING_CACHE_WRITE.try_with(|hook| hook.clone()) {
        hook();
    }
    LINK_LANG_TO_PERSPECTIVE_HANDLE
        .write()
        .unwrap()
        .insert(language_address, handle);
}

/// Look up PerspectiveHandle by link language address from cache
pub(crate) fn cached_perspective_handle(language_address: &str) -> Option<PerspectiveHandle> {
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
        let handle = perspective.persisted.lock().await;
        if let Some(nh) = &handle.neighbourhood {
            if nh.data.link_language == language_address {
                // Populate the cache for future lookups, under the lock like every cache write.
                register_link_language_perspective(language_address, handle.clone());
                drop(handle);
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
    } else {
        log::warn!(
            "DIFF-FROM-LINK-LANG [{}]: No perspective found for this link language!",
            language_address
        );
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
                    perspective_uuid: handle.uuid.clone(),
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
                        perspective_uuid: handle.uuid.clone(),
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
                    perspective_uuid: handle.uuid.clone(),
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
                perspective_uuid: handle.uuid.clone(),
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
                    perspective_uuid: handle.uuid.clone(),
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
                perspective_uuid: handle.uuid.clone(),
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

    // `instance.links` is already `LinkExpression`. Decorating just to persist
    // would convert back at the store boundary. Missing status defaults to
    // Local, matching the previous decorate path (not Shared).
    let additions: Vec<crate::types::LinkExpression> = instance
        .links
        .into_iter()
        .map(|mut link| {
            if link.status.is_none() {
                link.status = Some(LinkStatus::Local);
            }
            link
        })
        .collect();

    perspective
        .persist_link_diff(&crate::types::PerspectiveDiff {
            additions,
            removals: vec![],
        })
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
        crate::test_utils::setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::agent::AgentService::init_global_test_instance();
    }

    async fn find_perspective_by_uuid(
        all_perspectives: &Vec<PerspectiveInstance>,
        uuid: &String,
    ) -> Option<PerspectiveInstance> {
        for p in all_perspectives {
            if p.uuid == *uuid {
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

        update_perspective_with(&handle1.uuid, |h| {
            h.name = Some("Test Perspective 1 Updated".to_string())
        })
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

    /// A flow pass that is still running when its perspective is removed
    /// must not leave bookkeeping rows behind (Marvin, #1425). The removal
    /// tears the perspective down before it can delete anything, so a pass
    /// that writes after the delete sees the flag and writes nothing.
    ///
    /// The DB lock is held on another thread so the removal stops at its
    /// delete; the flag must already be set by then. Before the fix the
    /// removal waited on that lock first, with the perspective still live.
    ///
    /// The removal runs on its own thread and runtime, and this thread
    /// polls with a blocking sleep, so the perspective's background tasks
    /// (spawned on this runtime) cannot interleave with the window.
    #[tokio::test]
    async fn removal_tears_down_before_it_deletes_flow_rows() {
        use crate::perspectives::flow_instance::replica_state::FlowReplicaState;
        use std::sync::atomic::Ordering;
        use std::sync::mpsc;
        use std::time::{Duration, Instant};

        setup();
        let handle = PerspectiveHandle::new_from_name("teardown order".to_string());
        add_perspective(handle.clone(), None)
            .await
            .expect("add perspective");
        // What a running pass holds: a clone sharing the teardown flag.
        let in_flight = get_perspective(&handle.uuid).expect("perspective");

        let (locked_tx, locked_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel::<()>();
        let holder = std::thread::spawn(move || {
            let db = Ad4mDb::global_instance();
            let _guard = db.lock().expect("db lock");
            locked_tx.send(()).expect("signal locked");
            // Returns on release or when the test drops the sender.
            let _ = release_rx.recv();
        });
        locked_rx.recv().expect("holder took the lock");

        let uuid = handle.uuid.clone();
        let removal = std::thread::spawn(move || {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("runtime")
                .block_on(remove_perspective(&uuid))
                .is_some()
        });
        let deadline = Instant::now() + Duration::from_secs(5);
        while !in_flight.is_teardown.load(Ordering::Acquire) && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(10));
        }
        let torn_down_before_delete = in_flight.is_teardown.load(Ordering::Acquire);
        drop(release_tx);
        holder.join().expect("holder thread");
        assert!(
            removal.join().expect("removal thread"),
            "the perspective was removed"
        );
        assert!(
            torn_down_before_delete,
            "the removal must tear the perspective down before it deletes its rows"
        );

        // The in-flight pass now records, after the delete.
        let recorded = FlowReplicaState::record_pass(
            &handle.uuid,
            "did:key:alice",
            "ad4m://flow/instance/1",
            Some("open"),
            &["ad4m://flow/proposal/1".to_string()],
            &in_flight.is_teardown,
        )
        .expect("record_pass");
        assert_eq!(recorded, None, "nothing is written");
        assert!(
            FlowReplicaState::states(&handle.uuid, "did:key:alice", None)
                .expect("states")
                .is_empty(),
            "no row outlives the perspective"
        );
        assert!(!FlowReplicaState::is_fired(
            &handle.uuid,
            "did:key:alice",
            "ad4m://flow/proposal/1"
        )
        .expect("is_fired"));
    }

    /// A neighbourhood perspective on `link_language`, owned by `owners`.
    /// Registered without background tasks: the link language's install would
    /// write the routing cache too.
    fn neighbourhood_perspective(
        link_language: &str,
        owners: Option<Vec<String>>,
    ) -> PerspectiveInstance {
        let mut handle = PerspectiveHandle::new_from_name(link_language.to_string());
        handle.owners = owners;
        handle.neighbourhood = Some(crate::types::DecoratedNeighbourhoodExpression {
            data: crate::types::domain::Neighbourhood {
                link_language: link_language.to_string(),
                ..Default::default()
            },
            ..Default::default()
        });
        Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
        register_perspective(
            handle.uuid.clone(),
            PerspectiveInstance::new(handle.clone(), None),
        );
        get_perspective(&handle.uuid).unwrap()
    }

    /// A hook for the task under test, plus a second task that adds the owner
    /// `did:test:b` through `update_perspective_with` once the hook starts it.
    /// The hook runs where the task under test writes the routing cache. If
    /// that task no longer holds the handle lock there, the hook waits until
    /// the second change has finished, cache write included, so the first
    /// task's cache write comes last. If it still holds the lock, the second
    /// change waits for that lock and the hook returns at once.
    fn add_b_at_the_cache_write(
        instance: &PerspectiveInstance,
    ) -> (RoutingCacheWriteHook, tokio::task::JoinHandle<()>) {
        use std::sync::Arc;
        use tokio::sync::Notify;

        let start_b = Arc::new(Notify::new());
        let b_done = Arc::new(Notify::new());
        let b = tokio::spawn({
            let (uuid, start_b, b_done) = (instance.uuid.clone(), start_b.clone(), b_done.clone());
            async move {
                start_b.notified().await;
                update_perspective_with(&uuid, |h| h.add_owner("did:test:b"))
                    .await
                    .unwrap();
                b_done.notify_one();
            }
        });
        let instance = instance.clone();
        let hook: RoutingCacheWriteHook = Arc::new(move || {
            let lock_released = instance.persisted.try_lock().is_ok();
            start_b.notify_one();
            if lock_released {
                let b_done = b_done.clone();
                tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current().block_on(b_done.notified())
                });
            }
        });
        (hook, b)
    }

    /// The owners in the routing cache for `link_language`; removes the cache
    /// entry and the perspective so later tests start clean.
    fn cached_owners_and_clean_up(link_language: &str, uuid: &str) -> Vec<String> {
        let cached = cached_perspective_handle(link_language);
        LINK_LANG_TO_PERSPECTIVE_HANDLE
            .write()
            .unwrap()
            .remove(link_language);
        unregister_perspective(uuid);
        cached.and_then(|h| h.owners).unwrap_or_default()
    }

    /// Two concurrent changes to a neighbourhood's handle: the routing cache
    /// must end with both. A's cache write coming after B's would put back A's
    /// older copy, and B would get no broadcast signals.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn concurrent_handle_changes_keep_every_owner_in_the_routing_cache() {
        setup();
        let link_language = "routing-cache-concurrent-changes";
        let instance = neighbourhood_perspective(link_language, None);
        let (hook, b) = add_b_at_the_cache_write(&instance);

        BEFORE_ROUTING_CACHE_WRITE
            .scope(
                hook,
                update_perspective_with(&instance.uuid, |h| h.add_owner("did:test:a")),
            )
            .await
            .unwrap();
        b.await.unwrap();

        let owners = cached_owners_and_clean_up(link_language, &instance.uuid);
        assert!(owners.contains(&"did:test:a".to_string()), "{owners:?}");
        assert!(
            owners.contains(&"did:test:b".to_string()),
            "the routing cache lost B: {owners:?}"
        );
    }

    /// A signal lookup that fills the routing cache on a miss, while a handle
    /// change adds an owner: the lookup's copy must not replace the newer one.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_routing_cache_miss_does_not_overwrite_a_concurrent_owner() {
        setup();
        let link_language = "routing-cache-miss-during-change";
        let instance =
            neighbourhood_perspective(link_language, Some(vec!["did:test:a".to_string()]));
        let (hook, b) = add_b_at_the_cache_write(&instance);

        let found = BEFORE_ROUTING_CACHE_WRITE
            .scope(
                hook,
                perspective_by_link_language(link_language.to_string()),
            )
            .await;
        b.await.unwrap();

        let owners = cached_owners_and_clean_up(link_language, &instance.uuid);
        assert_eq!(found.map(|p| p.uuid), Some(instance.uuid.clone()));
        assert!(
            owners.contains(&"did:test:b".to_string()),
            "the routing cache lost B: {owners:?}"
        );
    }

    // Migration tests have been moved to src/perspectives/migration.rs

    // Additional tests for other functions can be added here
}
