pub mod perspective_instance;
pub mod sdna;
pub mod utils;
use std::sync::{RwLock, RwLockWriteGuard};
use std::collections::HashMap;
use lazy_static::lazy_static;
use perspective_instance::PerspectiveInstance;
use crate::graphql::graphql_types::PerspectiveHandle;

use crate::db::Ad4mDb;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_ADDED_TOPIC, PERSPECTIVE_REMOVED_TOPIC, PERSPECTIVE_UPDATED_TOPIC};
use crate::types::PerspectiveDiff;

lazy_static! {
    static ref PERSPECTIVES: RwLock<HashMap<String, RwLock<PerspectiveInstance>>> = RwLock::new(HashMap::new());
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
        perspectives.insert(
            handle.uuid.clone(), 
            RwLock::new(PerspectiveInstance::new(handle, None))
        );
    }
}

pub async fn add_perspective(handle: PerspectiveHandle) -> Result<(), String> {
    if PERSPECTIVES.read().unwrap().contains_key(&handle.uuid) {
        return Err(format!("Perspective with uuid {} already exists", &handle.uuid));
    }

    Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .add_perspective(&handle)
        .map_err(|e| e.to_string())?;

    {
        let mut perspectives = PERSPECTIVES.write().unwrap();
        perspectives.insert(
            handle.uuid.clone(), 
            RwLock::new(PerspectiveInstance::new(handle.clone(), None))
        );
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
        .map(|lock| lock.read().expect("Couldn't get read lock on PerspectiveInstance").clone())
        .collect()
}

pub fn get_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    PERSPECTIVES
        .read()
        .expect("Couldn't get read lock on PERSPECTIVES")
        .get(uuid)
        .map(|lock| lock.read().expect("Couldn't get read lock on PerspectiveInstance").clone())
}

pub async fn update_perspective(handle: &PerspectiveHandle) -> Result<(), String> {
    {
        let perspectives = PERSPECTIVES.read().unwrap();
        if let Some(instance_lock) = perspectives.get(&handle.uuid) {
            let mut instance = instance_lock.write().unwrap();
            instance.update_from_handle(handle.clone());
            Ad4mDb::with_global_instance(|db| {
                db.update_perspective(&handle)
                    .map_err(|e| e.to_string())
            })?;
        } else {
            return Err(format!("Perspective with uuid {} not found", &handle.uuid))
        }
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
        .remove_perspective(uuid) {
            log::error!("Error removing perspective from db: {}", e);
        }
    
    let removed_instance = {
        let mut perspectives = PERSPECTIVES.write().unwrap();
        perspectives.remove(uuid).and_then(|instance_lock| instance_lock.into_inner().ok())
    };

    get_global_pubsub()
        .await
        .publish(
            &PERSPECTIVE_REMOVED_TOPIC,
            &String::from(uuid),
        )
        .await;
    removed_instance
}

pub fn handle_perspective_diff_from_link_language(diff: PerspectiveDiff, language_address: String) {
    let perspectives = PERSPECTIVES.read().unwrap();
    perspectives.iter().for_each(|(_, perspective_lock)| {
        let perspective = perspective_lock.read().unwrap();
        //if perspective.language_address == language_address {
            // Assuming there is a method to handle diff within a PerspectiveInstance that is now behind a lock
            // perspective.handle_diff(diff.clone());
            // Placeholder for actual handling logic
        //}
    });
}


#[cfg(test)]
mod tests {
    use super::*;

    fn setup() {
        //setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
    }

    #[tokio::test]
    async fn test_perspective_persistence_roundtrip() {
        setup();
        assert!(all_perspectives().is_empty());

        let handle1 = PerspectiveHandle::new_from_name("Test Perspective 1".to_string());
        let handle2 = PerspectiveHandle::new_from_name("Test Perspective 2".to_string());

        add_perspective(handle1.clone()).await.expect("Failed to add perspective");
        add_perspective(handle2.clone()).await.expect("Failed to add perspective");
        // Test the get_all_perspectives function
        let perspectives = all_perspectives();
        
        // Assert expected results
        assert_eq!(perspectives.len(), 2);
        assert!(perspectives.iter().any(|p| p.persisted.uuid == handle1.uuid));
        assert!(perspectives.iter().any(|p| p.persisted.uuid == handle2.uuid));
        
        let p1 = perspectives.iter().find(|p| p.persisted.uuid == handle1.uuid).unwrap().clone();
        assert_eq!(p1.persisted.name, Some("Test Perspective 1".to_string()));


        let mut handle_updated = handle1.clone();
        handle_updated.name = Some("Test Perspective 1 Updated".to_string());
        update_perspective(&handle_updated).await.expect("Failed to update perspective");

        let p1_updated = get_perspective(&handle1.uuid).unwrap();
        assert_eq!(p1_updated.persisted.name, Some("Test Perspective 1 Updated".to_string()));

        let perspectives = all_perspectives();
        assert_eq!(perspectives.len(), 2);
        let p1_updated_from_all = perspectives.iter().find(|p| p.persisted.uuid == handle1.uuid).unwrap().clone();
        assert_eq!(p1_updated_from_all.persisted.name, Some("Test Perspective 1 Updated".to_string()));


        // Clean up test perspectives
        remove_perspective(handle1.uuid.as_str()).await;
        let perspectives = all_perspectives();
        assert_eq!(perspectives.len(), 1);
        assert!(perspectives.iter().any(|p| p.persisted.uuid == handle2.uuid));
    }

    // Additional tests for other functions can be added here
}

