//! Host function bindings for WASM language modules.
//!
//! These functions call back into the AD4M executor through the WASM import mechanism.
//! They are available to language implementations for operations like signing expressions,
//! getting the agent's DID, computing hashes, etc.

use crate::memory::{decode_fat_ptr, read_input, write_output};
use crate::types::Expression;
use serde::Serialize;

// Declare host function imports from the "ad4m" module.
// These are provided by the AD4M executor when instantiating the WASM module.
extern "C" {
    #[link_name = "agent_did"]
    fn _host_agent_did() -> u64;

    #[link_name = "agent_sign"]
    fn _host_agent_sign(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "agent_verify"]
    fn _host_agent_verify(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "agent_create_signed_expression"]
    fn _host_agent_create_signed_expression(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "log_message"]
    fn _host_log_message(ptr: u32, len: u32);

    #[link_name = "hash"]
    fn _host_hash(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "hc_call"]
    fn _host_hc_call(data_ptr: u32, data_len: u32) -> u64;
}

/// Read a fat-pointer result from the host into bytes.
fn read_host_result(fat_ptr: u64) -> Option<Vec<u8>> {
    if fat_ptr == 0 {
        return None;
    }
    let (ptr, len) = decode_fat_ptr(fat_ptr);
    if ptr == 0 || len == 0 {
        return None;
    }
    Some(read_input(ptr, len))
}

/// Get the current agent's DID.
pub fn agent_did() -> Option<String> {
    let fat = unsafe { _host_agent_did() };
    let bytes = read_host_result(fat)?;
    serde_json::from_slice(&bytes).ok()
}

/// Sign data with the agent's key.
pub fn agent_sign(data: &[u8]) -> Option<Vec<u8>> {
    let fat_input = write_output(data);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_agent_sign(ptr, len) };
    let bytes = read_host_result(fat)?;
    serde_json::from_slice(&bytes).ok()
}

/// Verify a signature.
pub fn agent_verify(did: &str, data: &str, signed_data: &str) -> bool {
    #[derive(Serialize)]
    struct VerifyRequest<'a> {
        did: &'a str,
        data: &'a str,
        signed_data: &'a str,
    }
    let req = VerifyRequest {
        did,
        data,
        signed_data,
    };
    let json = match serde_json::to_vec(&req) {
        Ok(j) => j,
        Err(_) => return false,
    };
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_agent_verify(ptr, len) };
    let bytes = match read_host_result(fat) {
        Some(b) => b,
        None => return false,
    };
    serde_json::from_slice::<bool>(&bytes).unwrap_or(false)
}

/// Create a signed expression from content.
pub fn create_signed_expression(content: &serde_json::Value) -> Option<Expression> {
    let json = serde_json::to_vec(content).ok()?;
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_agent_create_signed_expression(ptr, len) };
    let bytes = read_host_result(fat)?;
    serde_json::from_slice(&bytes).ok()
}

/// Log a message to the AD4M executor's log.
pub fn log(message: &str) {
    let bytes = message.as_bytes();
    let fat = write_output(bytes);
    let (ptr, len) = decode_fat_ptr(fat);
    unsafe {
        _host_log_message(ptr, len);
    }
}

/// Compute an IPFS-compatible content hash.
pub fn hash(data: &str) -> Option<String> {
    let bytes = data.as_bytes();
    let fat_input = write_output(bytes);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_hash(ptr, len) };
    let result_bytes = read_host_result(fat)?;
    serde_json::from_slice(&result_bytes).ok()
}

/// Call a Holochain zome function.
///
/// # Arguments
/// * `dna_nick` - The DNA role name / nickname
/// * `zome_name` - The zome to call
/// * `fn_name` - The function within the zome
/// * `payload` - Msgpack-encoded payload bytes
///
/// # Returns
/// The raw response bytes on success, or an error string.
pub fn holochain_call(dna_nick: &str, zome_name: &str, fn_name: &str, payload: &[u8]) -> Result<Vec<u8>, String> {
    #[derive(Serialize)]
    struct HcCallRequest<'a> {
        dna_nick: &'a str,
        zome_name: &'a str,
        fn_name: &'a str,
        payload: Vec<u8>,
    }
    let request = HcCallRequest {
        dna_nick,
        zome_name,
        fn_name,
        payload: payload.to_vec(),
    };
    let json = serde_json::to_vec(&request).map_err(|e| format!("serialize error: {}", e))?;
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_hc_call(ptr, len) };
    let bytes = read_host_result(fat).ok_or_else(|| "hc_call returned null".to_string())?;
    // Parse response - check for error field
    if let Ok(val) = serde_json::from_slice::<serde_json::Value>(&bytes) {
        if let Some(err) = val.get("error") {
            return Err(err.as_str().unwrap_or("unknown error").to_string());
        }
        if let Some(ok_data) = val.get("Ok") {
            if let Some(arr) = ok_data.as_array() {
                return Ok(arr.iter().filter_map(|v| v.as_u64().map(|n| n as u8)).collect());
            }
        }
    }
    Ok(bytes)
}

/// Legacy alias - calls holochain_call with the new API.
#[deprecated(note = "Use holochain_call() instead")]
pub fn hc_call(dna_nick: &str, zome_name: &str, fn_name: &str, payload: &[u8]) -> Option<Vec<u8>> {
    holochain_call(dna_nick, zome_name, fn_name, payload).ok()
}

// ============================================================================
// Holochain DNA Installation Host Functions
// ============================================================================

extern "C" {
    #[link_name = "hc_install_app"]
    fn _host_hc_install_app(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "hc_remove_app"]
    fn _host_hc_remove_app(data_ptr: u32, data_len: u32) -> u64;

    #[link_name = "hc_get_agent_key"]
    fn _host_hc_get_agent_key() -> u64;
}

/// Install a Holochain app from raw .happ bundle bytes.
///
/// The app will be installed with the language address as the installed_app_id,
/// using the agent's key and empty membrane proofs.
///
/// Returns the AppInfo as a JSON value on success.
pub fn holochain_install_app(happ_bytes: &[u8]) -> Result<serde_json::Value, String> {
    #[derive(Serialize)]
    struct HcInstallAppRequest {
        happ_bytes: Vec<u8>,
    }
    let request = HcInstallAppRequest {
        happ_bytes: happ_bytes.to_vec(),
    };
    let json = serde_json::to_vec(&request).map_err(|e| format!("serialize error: {}", e))?;
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_hc_install_app(ptr, len) };
    let bytes = read_host_result(fat).ok_or_else(|| "hc_install_app returned null".to_string())?;
    let val: serde_json::Value = serde_json::from_slice(&bytes).map_err(|e| format!("parse error: {}", e))?;
    if let Some(err) = val.get("error") {
        return Err(err.as_str().unwrap_or("unknown error").to_string());
    }
    Ok(val)
}

/// Remove a Holochain app by its installed app ID.
pub fn holochain_remove_app(app_id: &str) -> Result<(), String> {
    #[derive(Serialize)]
    struct HcRemoveAppRequest<'a> {
        app_id: &'a str,
    }
    let request = HcRemoveAppRequest { app_id };
    let json = serde_json::to_vec(&request).map_err(|e| format!("serialize error: {}", e))?;
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_hc_remove_app(ptr, len) };
    let bytes = read_host_result(fat).ok_or_else(|| "hc_remove_app returned null".to_string())?;
    let val: serde_json::Value = serde_json::from_slice(&bytes).map_err(|e| format!("parse error: {}", e))?;
    if let Some(err) = val.get("error") {
        return Err(err.as_str().unwrap_or("unknown error").to_string());
    }
    Ok(())
}

/// Get the agent's Holochain public key bytes.
pub fn holochain_get_agent_key() -> Result<Vec<u8>, String> {
    let fat = unsafe { _host_hc_get_agent_key() };
    let bytes = read_host_result(fat).ok_or_else(|| "hc_get_agent_key returned null".to_string())?;
    let val: serde_json::Value = serde_json::from_slice(&bytes).map_err(|e| format!("parse error: {}", e))?;
    if let Some(err) = val.get("error") {
        return Err(err.as_str().unwrap_or("unknown error").to_string());
    }
    if let Some(ok_data) = val.get("Ok") {
        if let Some(arr) = ok_data.as_array() {
            return Ok(arr.iter().filter_map(|v| v.as_u64().map(|n| n as u8)).collect());
        }
    }
    Err("unexpected response format".to_string())
}
