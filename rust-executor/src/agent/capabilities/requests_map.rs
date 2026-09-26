use super::types::AuthInfo;
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref REQUESTS: Mutex<HashMap<String, AuthInfo>> = Mutex::new(HashMap::new());
}

pub fn insert_request(request_key: String, auth_info: AuthInfo) -> Result<(), String> {
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.insert(request_key, auth_info);
    Ok(())
}

pub fn get_request(request_key: &str) -> Result<Option<AuthInfo>, String> {
    let requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    Ok(requests.get(request_key).cloned())
}

/// Drops every code issued for `request_id` (keys have the form `{request_id}-{code}`).
pub fn remove_requests_for(request_id: &str) -> Result<(), String> {
    let prefix = format!("{}-", request_id);
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.retain(|key, _| !key.starts_with(&prefix));
    Ok(())
}

pub fn remove_request(request_key: &str) -> Result<(), String> {
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.remove(request_key);
    Ok(())
}
