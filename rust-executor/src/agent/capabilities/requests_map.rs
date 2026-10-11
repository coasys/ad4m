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

/// The request ID a key was issued for. Keys have the form `{request_id}-{code}`; the code is
/// digits only, while request IDs are UUIDs that contain dashes themselves, so the ID is
/// everything before the *last* dash. A prefix test would let a caller who submits only the
/// start of someone else's ID match, and lock out, that other request.
fn request_id_of(key: &str) -> &str {
    key.rsplit_once('-').map_or(key, |(id, _)| id)
}

/// True while some code issued for `request_id` has not been redeemed.
pub fn has_requests_for(request_id: &str) -> Result<bool, String> {
    let requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    Ok(requests.keys().any(|key| request_id_of(key) == request_id))
}

/// Drops every code issued for exactly `request_id`.
pub fn remove_requests_for(request_id: &str) -> Result<(), String> {
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.retain(|key, _| request_id_of(key) != request_id);
    Ok(())
}

pub fn remove_request(request_key: &str) -> Result<(), String> {
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.remove(request_key);
    Ok(())
}
