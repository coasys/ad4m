pub mod perspective_instance;
pub mod sdna;
pub mod utils;
use crate::graphql::graphql_types::{PerspectiveExpression, PerspectiveHandle, PerspectiveState};
use lazy_static::lazy_static;
use perspective_instance::PerspectiveInstance;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::db::Ad4mDb;
use crate::pubsub::{
    get_global_pubsub, PERSPECTIVE_ADDED_TOPIC, PERSPECTIVE_REMOVED_TOPIC,
    PERSPECTIVE_UPDATED_TOPIC,
};
use crate::types::PerspectiveDiff;

lazy_static! {
    static ref PERSPECTIVES: RwLock<HashMap<String, RwLock<PerspectiveInstance>>> =
        RwLock::new(HashMap::new());
}

pub fn initialize_from_db() {
    let handles = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .get_all_perspectives()
        .expect("Couldn't get perspectives from db");
    let mut perspectives = PERSPECTIVES.write().unwrap();
    for handle in handles {
        let p = PerspectiveInstance::new(handle.clone(), None);
        tokio::spawn(p.clone().start_background_tasks());
        perspectives.insert(handle.uuid.clone(), RwLock::new(p));
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

    get_global_pubsub()
        .await
        .publish(
            &PERSPECTIVE_ADDED_TOPIC,
            &serde_json::to_string(&handle).unwrap(),
        )
        .await;
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
    }

    get_global_pubsub()
        .await
        .publish(
            &PERSPECTIVE_UPDATED_TOPIC,
            &serde_json::to_string(&handle).unwrap(),
        )
        .await;
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
    }

    get_global_pubsub()
        .await
        .publish(&PERSPECTIVE_REMOVED_TOPIC, &String::from(uuid))
        .await;
    removed_instance
}

pub fn handle_perspective_diff_from_link_language(diff: PerspectiveDiff, language_address: String) {
    tokio::spawn(handle_perspective_diff_from_link_language_impl(
        diff,
        language_address,
    ));
}

async fn perspective_by_link_language(language_address: String) -> Option<PerspectiveInstance> {
    let perspectives = PERSPECTIVES
        .read()
        .unwrap()
        .values()
        .map(|lock| lock.read().unwrap().clone())
        .collect::<Vec<_>>();
    for perspective in perspectives.into_iter() {
        let handle = perspective.persisted.lock().await.clone();

        if let Some(nh) = handle.neighbourhood {
            if nh.data.link_language == language_address {
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
        perspective.diff_from_link_language(diff).await;
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
    signal: PerspectiveExpression,
    language_address: String,
) {
    tokio::spawn(handle_telepresence_signal_from_link_language_impl(
        signal,
        language_address,
    ));
}

pub async fn handle_telepresence_signal_from_link_language_impl(
    signal: PerspectiveExpression,
    language_address: String,
) {
    if let Some(perspective) = perspective_by_link_language(language_address.clone()).await {
        perspective
            .telepresence_signal_from_link_language(signal)
            .await;
    }
}

#[cfg(test)]
mod tests {

    use super::*;

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

    // Additional tests for other functions can be added here
}
