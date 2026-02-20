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
#[derive(Serialize)]
pub struct HcCallRequest {
    pub dna_hash: Vec<u8>,
    pub agent_pubkey: Vec<u8>,
    pub zome_name: String,
    pub fn_name: String,
    pub payload: Vec<u8>,
}

/// Call a Holochain zome function.
pub fn hc_call(request: &HcCallRequest) -> Option<Vec<u8>> {
    let json = serde_json::to_vec(request).ok()?;
    let fat_input = write_output(&json);
    let (ptr, len) = decode_fat_ptr(fat_input);
    let fat = unsafe { _host_hc_call(ptr, len) };
    read_host_result(fat)
}
