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

pub fn revoke_app(request_key: &str) -> Result<(), String> {
    let mut apps = APPS.lock().map_err(|e| e.to_string())?;
    if let Some(app) = apps.get_mut(request_key) {
        app.revoked = true;
        persist_apps_to_file(&apps).map_err(|e| e.to_string())?;
    }
    Ok(())
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

pub fn get_apps() -> Vec<crate::graphql::graphql_types::Apps> {
    let apps = APPS.lock().unwrap();
    apps.iter()
        .map(|(request_id, app)| crate::graphql::graphql_types::Apps {
            auth: app.auth_info_extended.auth.clone(),
            request_id: request_id.clone(),
            revoked: Some(app.revoked),
            token: app.token.clone(),
        })
        .collect()
}
