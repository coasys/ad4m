pub mod perspective_instance;


use std::sync::{RwLock, RwLockWriteGuard};
use std::collections::HashMap;
use lazy_static::lazy_static;
use perspective_instance::PerspectiveInstance;
use crate::graphql::graphql_types::PerspectiveHandle;

use crate::db::Ad4mDb;
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

pub fn add_perspective(handle: PerspectiveHandle) -> Result<(), String> {
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

    let mut perspectives = PERSPECTIVES.write().unwrap();
    perspectives.insert(
        handle.uuid.clone(), 
        RwLock::new(PerspectiveInstance::new(handle.clone(), None))
    );
    Ok(())
}

pub fn with_perspective<F, T>(uuid: &str, f: F) -> Option<T>
where
    F: FnOnce(RwLockWriteGuard<PerspectiveInstance>) -> T,
{
    let perspectives = PERSPECTIVES.read().unwrap();
    perspectives.get(uuid).and_then(|lock| lock.write().ok()).map(f)
}

pub fn update_perspective(handle: &PerspectiveHandle) -> Result<(), String> {
    let perspectives = PERSPECTIVES.read().unwrap();
    if let Some(instance_lock) = perspectives.get(&handle.uuid) {
        let mut instance = instance_lock.write().unwrap();
        instance.update_from_handle(handle.clone());
        Ok(())
    } else {
        Err(format!("Perspective with uuid {} not found", &handle.uuid))
    }
}

pub fn remove_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    if let Err(e) = Ad4mDb::global_instance()
        .lock()
        .expect("Couldn't get write lock on Ad4mDb")
        .as_ref()
        .expect("Ad4mDb not initialized")
        .remove_perspective(uuid) {
            log::error!("Error removing perspective from db: {}", e);
        }
    let mut perspectives = PERSPECTIVES.write().unwrap();
    perspectives.remove(uuid).and_then(|instance_lock| instance_lock.into_inner().ok())
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