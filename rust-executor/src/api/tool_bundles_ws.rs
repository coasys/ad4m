//! Tool bundle WS-native handlers: installing, listing and removing the JavaScript tools apps ship
//! for their own records (`tool_bundles`).
//!
//! A bundle is code that runs on the node for every agent it hosts, like a Language, so the same
//! capabilities and the same rule apply: a user session may list them but not change them.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::tool_bundles::{InstalledBundle, RegistryError, ToolBundleRegistry};
use crate::types::*;

use super::guards::refuse_user_session;
use super::ws_handler::{HandlerMap, NoParams, ParamExt, WsRpcError};

async fn install(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&LANGUAGE_CREATE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    refuse_user_session(&ctx, "toolBundles.install")?;

    let source = params.require_str("source")?;
    let installed = ToolBundleRegistry::global()
        .install(&source)
        .await
        .map_err(|e| match e {
            RegistryError::Store(e) => WsRpcError::internal(e.to_string()),
            other => WsRpcError::bad_request(other.to_string()),
        })?;
    Ok(serde_json::to_value(installed)?)
}

async fn list(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&LANGUAGE_READ_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    Ok(serde_json::to_value(
        ToolBundleRegistry::global().list().await,
    )?)
}

async fn remove(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&LANGUAGE_DELETE_CAPABILITY)
        .map_err(WsRpcError::forbidden)?;
    refuse_user_session(&ctx, "toolBundles.remove")?;

    let address = params.require_str("address")?;
    let removed = ToolBundleRegistry::global()
        .remove(&address)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(removed))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<ToolBundleInstallParams, InstalledBundle>("toolBundles.install", install)
        .long();
    map.method::<NoParams, Vec<InstalledBundle>>("toolBundles.list", list)
        .read();
    map.method::<ToolBundleAddressParams, bool>("toolBundles.remove", remove);
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ToolBundleInstallParams {
    /// The bundle's source: one self-contained ES module exporting `toolsets`.
    pub source: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ToolBundleAddressParams {
    pub address: String,
}
