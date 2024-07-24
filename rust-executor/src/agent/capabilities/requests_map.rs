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

pub fn remove_request(request_key: &str) -> Result<(), String> {
    let mut requests = REQUESTS.lock().map_err(|e| e.to_string())?;
    requests.remove(request_key);
    Ok(())
}
