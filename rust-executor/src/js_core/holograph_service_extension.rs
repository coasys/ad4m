//! Step 6c deno op surface for the holograph wires.
//!
//! Mirrors `holochain_service_extension.rs`: a thin extension that
//! registers `op2(async)` ops calling into a process-global service
//! (here `HolographRuntime`), plus an `esm` bootstrap file that
//! installs `globalThis.HOLOGRAPH_SERVICE` so `language_bootstrap.js`
//! can build the per-language `__holographDelegate__`.
//!
//! No business logic lives here — every op is a one-line forward to
//! `HolographRuntime` in `holograph_wires.rs`.

use deno_core::op2;

use crate::holograph_wires::{HolographHandle, HolographRuntime, HolographWireError, WireDiff};
use crate::js_core::error::AnyhowWrapperError;

fn wire_to_anyhow(e: HolographWireError) -> AnyhowWrapperError {
    AnyhowWrapperError::from(deno_core::anyhow::anyhow!(e.to_string()))
}

#[op2(async)]
#[bigint]
async fn holograph_create_neighborhood(
    #[string] space_id: String,
    #[string] storage_dir: String,
) -> Result<u64, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    let handle = rt
        .create_neighborhood(&space_id, &storage_dir)
        .await
        .map_err(wire_to_anyhow)?;
    Ok(handle.0)
}

#[op2(async)]
#[string]
async fn holograph_commit(
    #[bigint] handle_id: u64,
    #[serde] diff: WireDiff,
) -> Result<String, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    rt.commit(HolographHandle(handle_id), diff)
        .await
        .map_err(wire_to_anyhow)
}

#[op2(async)]
#[serde]
async fn holograph_render(
    #[bigint] handle_id: u64,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    rt.render(HolographHandle(handle_id))
        .await
        .map_err(wire_to_anyhow)
}

#[op2(async)]
#[serde]
async fn holograph_next_emitted(
    #[bigint] handle_id: u64,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    let next = rt
        .next_emitted(HolographHandle(handle_id))
        .await
        .map_err(wire_to_anyhow)?;
    Ok(serde_json::to_value(next).unwrap_or(serde_json::Value::Null))
}

#[op2(async)]
#[string]
async fn holograph_join_agent(
    #[bigint] handle_id: u64,
    #[string] agent_key_b64: String,
) -> Result<String, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    rt.join_agent(HolographHandle(handle_id), agent_key_b64)
        .await
        .map_err(wire_to_anyhow)
}

#[op2(async)]
#[string]
async fn holograph_current_revision(
    #[bigint] handle_id: u64,
) -> Result<String, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    let v = rt
        .current_revision(HolographHandle(handle_id))
        .await
        .map_err(wire_to_anyhow)?;
    // op2 #[string] doesn't accept Option<String>; the JS shim turns
    // an empty string back into JS `null` (matches the spec since
    // op-id b64 is always non-empty when set).
    Ok(v.unwrap_or_default())
}

#[op2(async)]
#[string]
async fn holograph_latest_revision(#[bigint] handle_id: u64) -> Result<String, AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    let v = rt
        .latest_revision(HolographHandle(handle_id))
        .await
        .map_err(wire_to_anyhow)?;
    Ok(v.unwrap_or_default())
}

#[op2(async)]
async fn holograph_close_neighborhood(#[bigint] handle_id: u64) -> Result<(), AnyhowWrapperError> {
    let rt = HolographRuntime::get();
    rt.close_neighborhood(HolographHandle(handle_id))
        .await
        .map_err(wire_to_anyhow)
}

deno_core::extension!(
    holograph_service,
    ops = [
        holograph_create_neighborhood,
        holograph_commit,
        holograph_render,
        holograph_next_emitted,
        holograph_join_agent,
        holograph_current_revision,
        holograph_latest_revision,
        holograph_close_neighborhood,
    ],
    esm_entry_point = "ext:holograph_service/holograph_service_extension.js",
    esm = [dir "src/js_core", "holograph_service_extension.js"]
);
