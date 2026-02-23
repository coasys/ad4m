//! WASM-based Language runtime for AD4M.
//!
//! This module provides a WASM language loader and executor that runs AD4M Language
//! modules compiled to WebAssembly. Each language gets its own isolated WASM instance
//! with its own linear memory. Host functions bridge to the existing Rust services.
//!
//! Feature-gated behind `wasm-languages`.

pub mod abi;
pub mod error;
#[cfg(test)]
mod tests;

use std::path::Path;
use std::sync::Arc;

use log::{debug, error, info, warn};
use wasmer::{
    imports, Function, FunctionEnv, FunctionEnvMut, Instance, Memory, MemoryView, Module, Store,
    Value, TypedFunction,
};

use abi::*;
use error::WasmLanguageError;

// ============================================================================
// Host Environment (shared state passed to host functions)
// ============================================================================

/// Environment data available to host functions imported by WASM language modules.
/// Each language instance gets its own `HostEnv`.
#[derive(Clone)]
struct HostEnv {
    /// The language address this instance belongs to.
    language_address: String,
    /// Reference to the WASM instance memory, set after instantiation.
    memory: Option<Memory>,
    /// Guest's `ad4m_alloc` function, set after instantiation.
    alloc_fn: Option<TypedFunction<u32, u32>>,
    /// Tokio runtime handle for bridging sync host functions to async services.
    tokio_handle: tokio::runtime::Handle,
}

impl HostEnv {
    fn new(language_address: String, tokio_handle: tokio::runtime::Handle) -> Self {
        Self {
            language_address,
            memory: None,
            alloc_fn: None,
            tokio_handle,
        }
    }

    fn get_memory(&self) -> Result<&Memory, WasmLanguageError> {
        self.memory.as_ref().ok_or_else(|| {
            WasmLanguageError::MemoryAccessError("Memory not initialised".to_string())
        })
    }
}

// ============================================================================
// Host Function Implementations
// ============================================================================

/// Read a (ptr, len) region from guest memory as bytes.
fn read_guest_bytes(view: &MemoryView, ptr: u32, len: u32) -> Result<Vec<u8>, WasmLanguageError> {
    let mut buf = vec![0u8; len as usize];
    view.read(ptr as u64, &mut buf)
        .map_err(|e| WasmLanguageError::MemoryAccessError(format!("read failed: {}", e)))?;
    Ok(buf)
}

/// Write bytes into guest memory at the given pointer.
fn write_guest_bytes(view: &MemoryView, ptr: u32, data: &[u8]) -> Result<(), WasmLanguageError> {
    view.write(ptr as u64, data)
        .map_err(|e| WasmLanguageError::MemoryAccessError(format!("write failed: {}", e)))?;
    Ok(())
}

/// Allocate memory in the guest and write data into it, returning the guest pointer.
fn alloc_and_write(
    store: &mut impl wasmer::AsStoreMut,
    env: &HostEnv,
    data: &[u8],
) -> Result<u32, WasmLanguageError> {
    let alloc = env.alloc_fn.as_ref().ok_or_else(|| {
        WasmLanguageError::AllocationFailed {
            requested_size: data.len() as u32,
        }
    })?;
    let ptr = alloc.call(store, data.len() as u32).map_err(|e| {
        WasmLanguageError::AllocationFailed {
            requested_size: data.len() as u32,
        }
    })?;
    if ptr == 0 {
        return Err(WasmLanguageError::AllocationFailed {
            requested_size: data.len() as u32,
        });
    }
    let memory = env.get_memory()?;
    let view = memory.view(store);
    write_guest_bytes(&view, ptr, data)?;
    Ok(ptr)
}

/// Host function: `agent_did() -> fat_ptr`
/// Returns the agent's DID as a JSON string.
fn host_agent_did(mut env: FunctionEnvMut<HostEnv>) -> u64 {
    let (host_env, mut store) = env.data_and_store_mut();
    match Ok::<_, deno_core::error::AnyError>(crate::agent::did()) {
        Ok(did) => {
            let json = match serde_json::to_vec(&did) {
                Ok(j) => j,
                Err(e) => {
                    error!("host_agent_did: JSON error: {}", e);
                    return 0;
                }
            };
            match alloc_and_write(&mut store, host_env, &json) {
                Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
                Err(e) => {
                    error!("host_agent_did: alloc error: {}", e);
                    0
                }
            }
        }
        Err(e) => {
            error!("host_agent_did: {}", e);
            0
        }
    }
}

/// Host function: `agent_sign(data_ptr, data_len) -> fat_ptr`
/// Signs data with the agent's key.
fn host_agent_sign(mut env: FunctionEnvMut<HostEnv>, data_ptr: u32, data_len: u32) -> u64 {
    let (host_env, mut store) = env.data_and_store_mut();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_agent_sign: {}", e);
            return 0;
        }
    };
    let view = memory.view(&store);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_agent_sign: read error: {}", e);
            return 0;
        }
    };
    match crate::agent::sign(&data) {
        Ok(signature) => {
            let json = match serde_json::to_vec(&signature) {
                Ok(j) => j,
                Err(e) => {
                    error!("host_agent_sign: JSON error: {}", e);
                    return 0;
                }
            };
            match alloc_and_write(&mut store, host_env, &json) {
                Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
                Err(e) => {
                    error!("host_agent_sign: alloc error: {}", e);
                    0
                }
            }
        }
        Err(e) => {
            error!("host_agent_sign: {}", e);
            0
        }
    }
}

/// Host function: `agent_verify(data_ptr, data_len) -> fat_ptr`
/// Verifies a signature. Input is JSON-serialised AbiVerifyRequest.
fn host_agent_verify(mut env: FunctionEnvMut<HostEnv>, data_ptr: u32, data_len: u32) -> u64 {
    let (host_env, mut store) = env.data_and_store_mut();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_agent_verify: {}", e);
            return 0;
        }
    };
    let view = memory.view(&store);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_agent_verify: read error: {}", e);
            return 0;
        }
    };
    let request: AbiVerifyRequest = match from_json_bytes(&data) {
        Ok(r) => r,
        Err(e) => {
            error!("host_agent_verify: JSON parse error: {}", e);
            return 0;
        }
    };
    let result =
        crate::agent::signatures::verify_string_signed_by_did(&request.did, &request.data, &request.signed_data);
    let is_valid = result.unwrap_or(false);
    let json = match serde_json::to_vec(&is_valid) {
        Ok(j) => j,
        Err(e) => {
            error!("host_agent_verify: JSON error: {}", e);
            return 0;
        }
    };
    match alloc_and_write(&mut store, host_env, &json) {
        Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
        Err(e) => {
            error!("host_agent_verify: alloc error: {}", e);
            0
        }
    }
}

/// Host function: `agent_create_signed_expression(data_ptr, data_len) -> fat_ptr`
/// Creates a signed expression from raw JSON content.
fn host_agent_create_signed_expression(
    mut env: FunctionEnvMut<HostEnv>,
    data_ptr: u32,
    data_len: u32,
) -> u64 {
    let (host_env, mut store) = env.data_and_store_mut();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_agent_create_signed_expression: {}", e);
            return 0;
        }
    };
    let view = memory.view(&store);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_agent_create_signed_expression: read error: {}", e);
            return 0;
        }
    };
    let content: serde_json::Value = match serde_json::from_slice(&data) {
        Ok(v) => v,
        Err(e) => {
            error!("host_agent_create_signed_expression: JSON parse error: {}", e);
            return 0;
        }
    };
    let sorted = crate::js_core::utils::sort_json_value(&content);
    match crate::agent::create_signed_expression(sorted) {
        Ok(expr) => {
            let json = match serde_json::to_vec(&expr) {
                Ok(j) => j,
                Err(e) => {
                    error!("host_agent_create_signed_expression: JSON error: {}", e);
                    return 0;
                }
            };
            match alloc_and_write(&mut store, host_env, &json) {
                Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
                Err(e) => {
                    error!("host_agent_create_signed_expression: alloc error: {}", e);
                    0
                }
            }
        }
        Err(e) => {
            error!("host_agent_create_signed_expression: {}", e);
            0
        }
    }
}

/// Host function: `log_message(ptr, len)`
/// Logs a message from the guest.
fn host_log_message(env: FunctionEnvMut<HostEnv>, ptr: u32, len: u32) {
    let host_env = env.data();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_log_message: {}", e);
            return;
        }
    };
    let view = memory.view(&env);
    match read_guest_bytes(&view, ptr, len) {
        Ok(data) => match String::from_utf8(data) {
            Ok(msg) => info!("[WASM:{}]: {}", host_env.language_address, msg),
            Err(e) => error!("host_log_message: invalid UTF-8: {}", e),
        },
        Err(e) => error!("host_log_message: read error: {}", e),
    }
}

/// Host function: `hash(data_ptr, data_len) -> fat_ptr`
/// Computes an IPFS-compatible CID hash of the given data.
fn host_hash(mut env: FunctionEnvMut<HostEnv>, data_ptr: u32, data_len: u32) -> u64 {
    use cid::Cid;
    use multibase::Base;
    use multihash::{Code, MultihashDigest};

    let (host_env, mut store) = env.data_and_store_mut();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_hash: {}", e);
            return 0;
        }
    };
    let view = memory.view(&store);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_hash: read error: {}", e);
            return 0;
        }
    };
    let data_str = match String::from_utf8(data) {
        Ok(s) => s,
        Err(e) => {
            error!("host_hash: invalid UTF-8: {}", e);
            return 0;
        }
    };
    let multihash = Code::Sha2_256.digest(data_str.as_bytes());
    let cid = Cid::new_v1(0, multihash);
    let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());
    let hash_str = format!("Qm{}", encoded_cid);
    let json = match serde_json::to_vec(&hash_str) {
        Ok(j) => j,
        Err(e) => {
            error!("host_hash: JSON error: {}", e);
            return 0;
        }
    };
    match alloc_and_write(&mut store, host_env, &json) {
        Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
        Err(e) => {
            error!("host_hash: alloc error: {}", e);
            0
        }
    }
}

/// Host function: `hc_call(data_ptr, data_len) -> fat_ptr`
/// Calls a Holochain zome function. Input is JSON-serialised AbiHcCallRequest.
fn host_hc_call(mut env: FunctionEnvMut<HostEnv>, data_ptr: u32, data_len: u32) -> u64 {
    let (host_env, mut store) = env.data_and_store_mut();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_hc_call: {}", e);
            return 0;
        }
    };
    let view = memory.view(&store);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_hc_call: read error: {}", e);
            return 0;
        }
    };
    let request: AbiHcCallRequest = match from_json_bytes(&data) {
        Ok(r) => r,
        Err(e) => {
            error!("host_hc_call: JSON parse error: {}", e);
            return 0;
        }
    };

    let language_address = host_env.language_address.clone();
    let handle = host_env.tokio_handle.clone();

    // Bridge sync -> async using block_in_place to avoid deadlock in tokio runtime
    let result = tokio::task::block_in_place(|| {
        handle.block_on(async {
            let hc_service = match crate::holochain_service::interface::maybe_get_holochain_service().await {
                Some(s) => s,
                None => {
                    return Err(anyhow::anyhow!("Holochain service not available"));
                }
            };
            let payload = if request.payload.is_empty() {
                None
            } else {
                Some(holochain::prelude::ExternIO(request.payload))
            };
            hc_service.call_zome_function(
                language_address,
                request.dna_nick,
                request.zome_name,
                request.fn_name,
                payload,
            ).await
        })
    });

    let response = match result {
        Ok(zome_response) => {
            match zome_response {
                holochain::prelude::ZomeCallResponse::Ok(extern_io) => {
                    serde_json::json!({"Ok": extern_io.0})
                }
                other => {
                    serde_json::json!({"error": format!("{:?}", other)})
                }
            }
        }
        Err(e) => serde_json::json!({"error": format!("{}", e)}),
    };

    let json = match serde_json::to_vec(&response) {
        Ok(j) => j,
        Err(e) => {
            error!("host_hc_call: JSON serialize error: {}", e);
            return 0;
        }
    };
    match alloc_and_write(&mut store, host_env, &json) {
        Ok(ptr) => encode_fat_ptr(ptr, json.len() as u32),
        Err(e) => {
            error!("host_hc_call: alloc error: {}", e);
            0
        }
    }
}

/// Host function: `perspective_diff_received(data_ptr, data_len)`
/// Notifies the executor of a perspective diff from a link language.
fn host_perspective_diff_received(
    env: FunctionEnvMut<HostEnv>,
    data_ptr: u32,
    data_len: u32,
) {
    let host_env = env.data();
    let language_address = host_env.language_address.clone();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_perspective_diff_received: {}", e);
            return;
        }
    };
    let view = memory.view(&env);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_perspective_diff_received: read error: {}", e);
            return;
        }
    };
    let diff: crate::types::PerspectiveDiff = match serde_json::from_slice(&data) {
        Ok(d) => d,
        Err(e) => {
            error!("host_perspective_diff_received: JSON parse error: {}", e);
            return;
        }
    };
    crate::perspectives::handle_perspective_diff_from_link_language(diff, language_address);
}

/// Host function: `sync_state_changed(state)`
/// Notifies the executor of a sync state change.
fn host_sync_state_changed(env: FunctionEnvMut<HostEnv>, data_ptr: u32, data_len: u32) {
    let host_env = env.data();
    let language_address = host_env.language_address.clone();
    let memory = match host_env.get_memory() {
        Ok(m) => m.clone(),
        Err(e) => {
            error!("host_sync_state_changed: {}", e);
            return;
        }
    };
    let view = memory.view(&env);
    let data = match read_guest_bytes(&view, data_ptr, data_len) {
        Ok(d) => d,
        Err(e) => {
            error!("host_sync_state_changed: read error: {}", e);
            return;
        }
    };
    let state: crate::graphql::graphql_types::PerspectiveState = match serde_json::from_slice(&data)
    {
        Ok(s) => s,
        Err(e) => {
            error!("host_sync_state_changed: JSON parse error: {}", e);
            return;
        }
    };
    crate::perspectives::handle_sync_state_changed_from_link_language(state, language_address);
}

// ============================================================================
// WASM Language Instance
// ============================================================================

/// A loaded and instantiated WASM language module.
pub struct WasmLanguageInstance {
    store: Store,
    instance: Instance,
    #[allow(dead_code)]
    env: FunctionEnv<HostEnv>,
    capabilities: LanguageCapabilities,
    language_name: String,
    language_address: String,
}

impl WasmLanguageInstance {
    /// Read the result of a guest function call from a fat pointer.
    fn read_result(&self, fat_ptr: u64) -> Result<Vec<u8>, WasmLanguageError> {
        if fat_ptr == 0 {
            return Ok(Vec::new());
        }
        let (ptr, len) = decode_fat_ptr(fat_ptr);
        if ptr == 0 || len == 0 {
            return Ok(Vec::new());
        }
        let memory = self
            .instance
            .exports
            .get_memory("memory")
            .map_err(|e| WasmLanguageError::MemoryAccessError(format!("{}", e)))?;
        let view = memory.view(&self.store);
        read_guest_bytes(&view, ptr, len)
    }

    /// Read the result as a JSON string.
    fn read_result_string(&self, fat_ptr: u64) -> Result<String, WasmLanguageError> {
        let bytes = self.read_result(fat_ptr)?;
        if bytes.is_empty() {
            return Ok(String::new());
        }
        String::from_utf8(bytes).map_err(WasmLanguageError::from)
    }

    /// Write input data to guest memory and return (ptr, len).
    fn write_input(&mut self, data: &[u8]) -> Result<(u32, u32), WasmLanguageError> {
        let alloc_fn: TypedFunction<u32, u32> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_alloc")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_alloc: {}", e)))?;
        let ptr = alloc_fn
            .call(&mut self.store, data.len() as u32)
            .map_err(|e| WasmLanguageError::AllocationFailed {
                requested_size: data.len() as u32,
            })?;
        if ptr == 0 {
            return Err(WasmLanguageError::AllocationFailed {
                requested_size: data.len() as u32,
            });
        }
        let memory = self
            .instance
            .exports
            .get_memory("memory")
            .map_err(|e| WasmLanguageError::MemoryAccessError(format!("{}", e)))?;
        let view = memory.view(&self.store);
        write_guest_bytes(&view, ptr, data)?;
        Ok((ptr, data.len() as u32))
    }

    /// Deallocate memory in the guest.
    fn dealloc(&mut self, ptr: u32, size: u32) -> Result<(), WasmLanguageError> {
        let dealloc_fn: TypedFunction<(u32, u32), ()> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_dealloc")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_dealloc: {}", e)))?;
        dealloc_fn
            .call(&mut self.store, ptr, size)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("dealloc failed: {}", e)))?;
        Ok(())
    }

    /// Get the language name.
    pub fn name(&self) -> &str {
        &self.language_name
    }

    /// Get the language address.
    pub fn address(&self) -> &str {
        &self.language_address
    }

    /// Get the language capabilities.
    pub fn capabilities(&self) -> &LanguageCapabilities {
        &self.capabilities
    }

    /// Call `expression_get(address) -> Option<Expression>`.
    pub fn expression_get(
        &mut self,
        address: &str,
    ) -> Result<Option<AbiExpression>, WasmLanguageError> {
        if !self.capabilities.has_expression_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable(
                "ad4m_expression_get".to_string(),
            ));
        }
        let input = to_json_bytes(&address)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_expression_get")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_expression_get: {}", e)))?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(None);
        }
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(None);
        }
        // Try to deserialise; if it's a null JSON value, return None
        let value: serde_json::Value = from_json_bytes(&bytes)?;
        if value.is_null() {
            return Ok(None);
        }
        let expr: AbiExpression = serde_json::from_value(value)?;
        Ok(Some(expr))
    }

    /// Call `expression_put(content) -> Address`.
    pub fn expression_put(
        &mut self,
        content: &serde_json::Value,
    ) -> Result<String, WasmLanguageError> {
        if !self.capabilities.has_put_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable(
                "ad4m_expression_put".to_string(),
            ));
        }
        let input = to_json_bytes(content)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_expression_put")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_expression_put: {}", e)))?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        let bytes = self.read_result(result)?;
        let address: String = from_json_bytes(&bytes)?;
        Ok(address)
    }

    /// Call `interactions(address) -> Vec<Interaction>`.
    pub fn interactions(
        &mut self,
        address: &str,
    ) -> Result<Vec<AbiInteraction>, WasmLanguageError> {
        if !self.capabilities.has_interactions {
            return Ok(Vec::new());
        }
        let input = to_json_bytes(&address)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_interactions")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_interactions: {}", e)))?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(Vec::new());
        }
        let interactions: Vec<AbiInteraction> = from_json_bytes(&bytes)?;
        Ok(interactions)
    }

    /// Call `is_immutable_expression(address) -> bool`.
    pub fn is_immutable_expression(&mut self, address: &str) -> Result<bool, WasmLanguageError> {
        if !self.capabilities.has_is_immutable_expression {
            return Ok(false);
        }
        let input = to_json_bytes(&address)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u32> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_is_immutable_expression")
            .map_err(|e| {
                WasmLanguageError::MissingExport(format!("ad4m_is_immutable_expression: {}", e))
            })?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        Ok(result != 0)
    }

    /// Call `teardown()`.
    pub fn teardown(&mut self) -> Result<(), WasmLanguageError> {
        if !self.capabilities.has_teardown {
            return Ok(());
        }
        let func: TypedFunction<(), ()> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_teardown")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_teardown: {}", e)))?;
        func.call(&mut self.store)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        Ok(())
    }

    /// Call `link_add(link_json) -> LinkExpression`.
    pub fn link_add(
        &mut self,
        link: &AbiLink,
    ) -> Result<AbiLinkExpression, WasmLanguageError> {
        if !self.capabilities.has_link_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable(
                "ad4m_link_add".to_string(),
            ));
        }
        let input = to_json_bytes(link)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_link_add")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_link_add: {}", e)))?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        let bytes = self.read_result(result)?;
        let link_expr: AbiLinkExpression = from_json_bytes(&bytes)?;
        Ok(link_expr)
    }

    /// Call `link_remove(link_expr_json)`.
    pub fn link_remove(
        &mut self,
        link: &AbiLinkExpression,
    ) -> Result<(), WasmLanguageError> {
        if !self.capabilities.has_link_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable(
                "ad4m_link_remove".to_string(),
            ));
        }
        let input = to_json_bytes(link)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), ()> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_link_remove")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_link_remove: {}", e)))?;
        func.call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        Ok(())
    }

    /// Call `link_get_links(query_json) -> Vec<LinkExpression>`.
    pub fn link_get_links(
        &mut self,
        query: &serde_json::Value,
    ) -> Result<Vec<AbiLinkExpression>, WasmLanguageError> {
        if !self.capabilities.has_link_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable(
                "ad4m_link_get_links".to_string(),
            ));
        }
        let input = to_json_bytes(query)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_link_get_links")
            .map_err(|e| {
                WasmLanguageError::MissingExport(format!("ad4m_link_get_links: {}", e))
            })?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(Vec::new());
        }
        let links: Vec<AbiLinkExpression> = from_json_bytes(&bytes)?;
        Ok(links)
    }

    /// Call `ad4m_sync() -> Result<(), Error>`.
    pub fn sync(&mut self) -> Result<(), WasmLanguageError> {
        if !self.capabilities.has_links_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable("ad4m_sync".to_string()));
        }
        let func: TypedFunction<(), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_sync")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_sync: {}", e)))?;
        let result = func
            .call(&mut self.store)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(());
        }
        let bytes = self.read_result(result)?;
        // Check for error response
        if let Ok(val) = serde_json::from_slice::<serde_json::Value>(&bytes) {
            if let Some(err) = val.get("error") {
                return Err(WasmLanguageError::RuntimeError(err.as_str().unwrap_or("unknown error").to_string()));
            }
        }
        Ok(())
    }

    /// Call `ad4m_commit(diff_json) -> Option<String>`.
    pub fn commit(&mut self, diff: &AbiPerspectiveDiff) -> Result<Option<String>, WasmLanguageError> {
        if !self.capabilities.has_links_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable("ad4m_commit".to_string()));
        }
        let input = to_json_bytes(diff)?;
        let (ptr, len) = self.write_input(&input)?;
        let func: TypedFunction<(u32, u32), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_commit")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_commit: {}", e)))?;
        let result = func
            .call(&mut self.store, ptr, len)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(None);
        }
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(None);
        }
        let val: serde_json::Value = from_json_bytes(&bytes)?;
        if let Some(err) = val.get("error") {
            return Err(WasmLanguageError::RuntimeError(err.as_str().unwrap_or("unknown error").to_string()));
        }
        let revision: Option<String> = serde_json::from_value(val)?;
        Ok(revision)
    }

    /// Call `ad4m_render() -> Option<Perspective>` (returns links as JSON).
    pub fn render(&mut self) -> Result<Option<Vec<AbiLinkExpression>>, WasmLanguageError> {
        if !self.capabilities.has_links_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable("ad4m_render".to_string()));
        }
        let func: TypedFunction<(), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_render")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_render: {}", e)))?;
        let result = func
            .call(&mut self.store)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(None);
        }
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(None);
        }
        let val: serde_json::Value = from_json_bytes(&bytes)?;
        if let Some(err) = val.get("error") {
            return Err(WasmLanguageError::RuntimeError(err.as_str().unwrap_or("unknown error").to_string()));
        }
        let links: Option<Vec<AbiLinkExpression>> = serde_json::from_value(val)?;
        Ok(links)
    }

    /// Call `ad4m_current_revision() -> Option<String>`.
    pub fn current_revision(&mut self) -> Result<Option<String>, WasmLanguageError> {
        if !self.capabilities.has_links_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable("ad4m_current_revision".to_string()));
        }
        let func: TypedFunction<(), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_current_revision")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_current_revision: {}", e)))?;
        let result = func
            .call(&mut self.store)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(None);
        }
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(None);
        }
        let val: serde_json::Value = from_json_bytes(&bytes)?;
        if let Some(err) = val.get("error") {
            return Err(WasmLanguageError::RuntimeError(err.as_str().unwrap_or("unknown error").to_string()));
        }
        let revision: Option<String> = serde_json::from_value(val)?;
        Ok(revision)
    }

    /// Call `ad4m_others() -> Vec<String>`.
    pub fn others(&mut self) -> Result<Vec<String>, WasmLanguageError> {
        if !self.capabilities.has_links_adapter {
            return Err(WasmLanguageError::FunctionNotAvailable("ad4m_others".to_string()));
        }
        let func: TypedFunction<(), u64> = self
            .instance
            .exports
            .get_typed_function(&self.store, "ad4m_others")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_others: {}", e)))?;
        let result = func
            .call(&mut self.store)
            .map_err(|e| WasmLanguageError::RuntimeError(format!("{}", e)))?;
        if result == 0 {
            return Ok(vec![]);
        }
        let bytes = self.read_result(result)?;
        if bytes.is_empty() {
            return Ok(vec![]);
        }
        let val: serde_json::Value = from_json_bytes(&bytes)?;
        if let Some(err) = val.get("error") {
            return Err(WasmLanguageError::RuntimeError(err.as_str().unwrap_or("unknown error").to_string()));
        }
        let dids: Vec<String> = serde_json::from_value(val)?;
        Ok(dids)
    }

}

// ============================================================================
// WASM Language Loader
// ============================================================================

/// Loads and instantiates a WASM language module from a file path.
///
/// Each call creates a fresh WASM store and instance with isolated linear memory.
/// Host functions are injected as imports under the "ad4m" namespace.
pub fn load_wasm_language(
    wasm_path: &Path,
    language_address: &str,
) -> Result<WasmLanguageInstance, WasmLanguageError> {
    info!(
        "Loading WASM language from {} (address: {})",
        wasm_path.display(),
        language_address
    );

    // Read the WASM bytes
    let wasm_bytes = std::fs::read(wasm_path)?;

    load_wasm_language_from_bytes(&wasm_bytes, language_address)
}

/// Loads and instantiates a WASM language module from raw bytes.
pub fn load_wasm_language_from_bytes(
    wasm_bytes: &[u8],
    language_address: &str,
) -> Result<WasmLanguageInstance, WasmLanguageError> {
    // Create store with default engine (Cranelift, matching Holochain)
    let mut store = Store::default();

    // Compile the module
    let module = Module::new(&store, wasm_bytes)
        .map_err(|e| WasmLanguageError::CompilationError(format!("{}", e)))?;

    // Create host environment
    let host_env = HostEnv::new(language_address.to_string(), tokio::runtime::Handle::current());
    let env = FunctionEnv::new(&mut store, host_env);

    // Define host function imports
    let import_object = imports! {
        HOST_MODULE_NAME => {
            host_functions::AGENT_DID => Function::new_typed_with_env(&mut store, &env, host_agent_did),
            host_functions::AGENT_SIGN => Function::new_typed_with_env(&mut store, &env, host_agent_sign),
            host_functions::AGENT_VERIFY => Function::new_typed_with_env(&mut store, &env, host_agent_verify),
            host_functions::AGENT_CREATE_SIGNED_EXPRESSION => Function::new_typed_with_env(&mut store, &env, host_agent_create_signed_expression),
            host_functions::LOG_MESSAGE => Function::new_typed_with_env(&mut store, &env, host_log_message),
            host_functions::HASH => Function::new_typed_with_env(&mut store, &env, host_hash),
            host_functions::HC_CALL => Function::new_typed_with_env(&mut store, &env, host_hc_call),
            host_functions::PERSPECTIVE_DIFF_RECEIVED => Function::new_typed_with_env(&mut store, &env, host_perspective_diff_received),
            host_functions::SYNC_STATE_CHANGED => Function::new_typed_with_env(&mut store, &env, host_sync_state_changed),
        }
    };

    // Instantiate the module
    let instance = Instance::new(&mut store, &module, &import_object)
        .map_err(|e| WasmLanguageError::RuntimeError(format!("Instantiation failed: {}", e)))?;

    // Set memory and alloc function in the environment
    {
        let memory = instance
            .exports
            .get_memory("memory")
            .map_err(|e| WasmLanguageError::MissingExport(format!("memory: {}", e)))?
            .clone();
        let alloc_fn: TypedFunction<u32, u32> = instance
            .exports
            .get_typed_function(&store, "ad4m_alloc")
            .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_alloc: {}", e)))?;
        let mut env_mut = env.as_mut(&mut store);
        env_mut.memory = Some(memory);
        env_mut.alloc_fn = Some(alloc_fn);
    }

    // Validate ABI version
    let abi_version_fn: TypedFunction<(), u32> = instance
        .exports
        .get_typed_function(&store, "ad4m_abi_version")
        .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_abi_version: {}", e)))?;
    let abi_version = abi_version_fn
        .call(&mut store)
        .map_err(|e| WasmLanguageError::RuntimeError(format!("ad4m_abi_version call failed: {}", e)))?;
    if abi_version < AD4M_LANGUAGE_ABI_MIN_VERSION || abi_version > AD4M_LANGUAGE_ABI_VERSION {
        return Err(WasmLanguageError::AbiVersionMismatch {
            expected_min: AD4M_LANGUAGE_ABI_MIN_VERSION,
            expected_max: AD4M_LANGUAGE_ABI_VERSION,
            actual: abi_version,
        });
    }
    info!("WASM language ABI version: {}", abi_version);

    // Get language name
    let name_fn: TypedFunction<(), u64> = instance
        .exports
        .get_typed_function(&store, "ad4m_language_name")
        .map_err(|e| WasmLanguageError::MissingExport(format!("ad4m_language_name: {}", e)))?;
    let name_fat_ptr = name_fn
        .call(&mut store)
        .map_err(|e| WasmLanguageError::RuntimeError(format!("ad4m_language_name call failed: {}", e)))?;
    let name_bytes = {
        let (ptr, len) = decode_fat_ptr(name_fat_ptr);
        let memory = instance
            .exports
            .get_memory("memory")
            .map_err(|e| WasmLanguageError::MemoryAccessError(format!("{}", e)))?;
        let view = memory.view(&store);
        read_guest_bytes(&view, ptr, len)?
    };
    let language_name = String::from_utf8(name_bytes)?;
    info!("Loaded WASM language: {}", language_name);

    // Detect capabilities from exports
    let exports: std::collections::HashSet<String> = instance
        .exports
        .iter()
        .map(|(name, _)| name.to_string())
        .collect();

    let capabilities = LanguageCapabilities {
        has_expression_adapter: exports.contains("ad4m_expression_get"),
        has_put_adapter: exports.contains("ad4m_expression_put"),
        has_link_adapter: exports.contains("ad4m_link_add")
            && exports.contains("ad4m_link_remove")
            && exports.contains("ad4m_link_get_links"),
        has_interactions: exports.contains("ad4m_interactions"),
        has_teardown: exports.contains("ad4m_teardown"),
        has_is_immutable_expression: exports.contains("ad4m_is_immutable_expression"),
        has_links_adapter: exports.contains("ad4m_sync")
            && exports.contains("ad4m_commit")
            && exports.contains("ad4m_render"),
    };

    debug!(
        "Language capabilities: expression={}, put={}, link={}, interactions={}, teardown={}, immutable={}, links_adapter={}",
        capabilities.has_expression_adapter,
        capabilities.has_put_adapter,
        capabilities.has_link_adapter,
        capabilities.has_interactions,
        capabilities.has_teardown,
        capabilities.has_is_immutable_expression,
        capabilities.has_links_adapter,
    );

    Ok(WasmLanguageInstance {
        store,
        instance,
        env,
        capabilities,
        language_name,
        language_address: language_address.to_string(),
    })
}

// ============================================================================
// WASM Language Registry
// ============================================================================

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    /// Global registry of loaded WASM language instances.
    static ref WASM_LANGUAGE_REGISTRY: Mutex<HashMap<String, Arc<Mutex<WasmLanguageInstance>>>> =
        Mutex::new(HashMap::new());
}

/// Load a WASM language and register it in the global registry.
pub fn register_wasm_language(
    wasm_path: &Path,
    language_address: &str,
) -> Result<(), WasmLanguageError> {
    let instance = load_wasm_language(wasm_path, language_address)?;
    let mut registry = WASM_LANGUAGE_REGISTRY
        .lock()
        .map_err(|e| WasmLanguageError::RuntimeError(format!("Registry lock poisoned: {}", e)))?;
    registry.insert(
        language_address.to_string(),
        Arc::new(Mutex::new(instance)),
    );
    info!(
        "Registered WASM language at address: {}",
        language_address
    );
    Ok(())
}

/// Get a reference to a loaded WASM language instance.
pub fn get_wasm_language(
    language_address: &str,
) -> Result<Arc<Mutex<WasmLanguageInstance>>, WasmLanguageError> {
    let registry = WASM_LANGUAGE_REGISTRY
        .lock()
        .map_err(|e| WasmLanguageError::RuntimeError(format!("Registry lock poisoned: {}", e)))?;
    registry
        .get(language_address)
        .cloned()
        .ok_or_else(|| {
            WasmLanguageError::RuntimeError(format!(
                "No WASM language registered at address: {}",
                language_address
            ))
        })
}

/// Unload a WASM language from the registry, calling teardown if available.
pub fn unregister_wasm_language(language_address: &str) -> Result<(), WasmLanguageError> {
    let mut registry = WASM_LANGUAGE_REGISTRY
        .lock()
        .map_err(|e| WasmLanguageError::RuntimeError(format!("Registry lock poisoned: {}", e)))?;
    if let Some(instance_arc) = registry.remove(language_address) {
        let mut instance = instance_arc
            .lock()
            .map_err(|e| WasmLanguageError::RuntimeError(format!("Instance lock poisoned: {}", e)))?;
        if instance.capabilities().has_teardown {
            if let Err(e) = instance.teardown() {
                warn!("Error during WASM language teardown for {}: {}", language_address, e);
            }
        }
        info!("Unregistered WASM language: {}", language_address);
    }
    Ok(())
}

/// Check if a language address corresponds to a loaded WASM language.
pub fn is_wasm_language(language_address: &str) -> bool {
    WASM_LANGUAGE_REGISTRY
        .lock()
        .map(|registry| registry.contains_key(language_address))
        .unwrap_or(false)
}
