use super::types::AuthInfoExtended;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::Path;
use std::sync::Mutex;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct App {
    auth_info_extended: AuthInfoExtended,
    revoked: bool,
    token: String,
}

impl App {
    pub fn new(auth_info_extended: AuthInfoExtended, revoked: bool, token: String) -> Self {
        App {
            auth_info_extended,
            revoked,
            token,
        }
    }
}

use std::env;

lazy_static! {
    static ref DATA_FILE_PATH: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "apps_data.json".to_string()));
}

fn get_data_file_path() -> String {
    DATA_FILE_PATH.lock().unwrap().clone()
}

pub fn set_data_file_path(file_path: String) {
    let mut data_file_path = DATA_FILE_PATH.lock().unwrap();
    *data_file_path = file_path;
}

fn persist_apps_to_file(apps: &HashMap<String, App>) -> io::Result<()> {
    let file_path = get_data_file_path();
    let serialized_apps = serde_json::to_string(apps)?;
    fs::write(file_path, serialized_apps)?;
    Ok(())
}

fn load_apps_from_file() -> io::Result<HashMap<String, App>> {
    let file_path = get_data_file_path();
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let apps: HashMap<String, App> = serde_json::from_str(&contents)?;
    Ok(apps)
}

lazy_static! {
    static ref APPS: Mutex<HashMap<String, App>> = {
        let apps = if Path::new(&get_data_file_path()).exists() {
            load_apps_from_file().unwrap_or_else(|_| HashMap::new())
        } else {
            HashMap::new()
        };
        Mutex::new(apps)
    };
}

pub fn insert_app(
    request_key: String,
    auth_info_extended: AuthInfoExtended,
    token: String,
) -> Result<(), String> {
    let mut apps = APPS.lock().map_err(|e| e.to_string())?;
    apps.insert(request_key, App::new(auth_info_extended, false, token));
    persist_apps_to_file(&apps).map_err(|e| e.to_string())?;
    Ok(())
}

/// Why `revoke_app` failed.
#[derive(Debug)]
pub enum RevokeError {
    /// No app has this request id or token.
    NotFound,
    /// The app stays revoked in memory, but the registry file did not take the change, so a
    /// restart would bring the app back.
    Store(String),
}

impl std::fmt::Display for RevokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevokeError::NotFound => write!(f, "No app matches this token or request id"),
            RevokeError::Store(e) => write!(f, "Revoked until restart, but not saved: {e}"),
        }
    }
}

/// Revokes the app that `token_or_request_id` names: its request id, which the SDK sends, or
/// its JWT, which the RPC's field name (`token`) promises. A value that names no app is an
/// error: a revoke that reports success and revokes nothing ends the search for a leak
/// (#1060).
pub fn revoke_app(token_or_request_id: &str) -> Result<(), RevokeError> {
    let mut apps = APPS.lock().map_err(|e| RevokeError::Store(e.to_string()))?;
    let key = if apps.contains_key(token_or_request_id) {
        Some(token_or_request_id.to_string())
    } else {
        apps.iter()
            .find(|(_, app)| crate::utils::constant_time_eq(&app.token, token_or_request_id))
            .map(|(key, _)| key.clone())
    };
    let Some(app) = key.and_then(|key| apps.get_mut(&key)) else {
        return Err(RevokeError::NotFound);
    };
    app.revoked = true;
    persist_apps_to_file(&apps).map_err(|e| RevokeError::Store(e.to_string()))
}

pub fn remove_app(request_key: &str) -> Result<(), String> {
    let mut apps = APPS.lock().map_err(|e| e.to_string())?;
    if apps.remove(request_key).is_some() {
        persist_apps_to_file(&apps).map_err(|e| e.to_string())?;
        Ok(())
    } else {
        Err(format!("App with request_key '{}' not found.", request_key))
    }
}

pub fn get_app(request_key: &str) -> Result<Option<App>, String> {
    let apps = APPS.lock().map_err(|e| e.to_string())?;
    Ok(apps.get(request_key).cloned())
}

/// Whether the operator revoked the app that holds this token. Scans in place, without
/// copying the app list, because connections call it on every request and event.
pub fn is_revoked(token: &str) -> bool {
    APPS.lock()
        .map(|apps| {
            apps.values()
                .any(|app| crate::utils::constant_time_eq(&app.token, token) && app.revoked)
        })
        .unwrap_or(true)
}

pub fn get_apps() -> Vec<crate::types::Apps> {
    let apps = APPS.lock().unwrap();
    apps.iter()
        .map(|(request_id, app)| crate::types::Apps {
            auth: app.auth_info_extended.auth.clone(),
            request_id: request_id.clone(),
            revoked: Some(app.revoked),
            token: app.token.clone(),
        })
        .collect()
}
