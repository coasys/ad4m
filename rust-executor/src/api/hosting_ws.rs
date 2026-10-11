//! Hosting WS-native handlers.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;
use crate::types::RequestContext;

use super::guards::refuse_user_session;
use super::types::*;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

async fn get_hosting_info(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    let user_info = if let Some(user_email) = ctx.user_email.clone() {
        let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(&user_email)).ok();
        let hot_wallet_address =
            Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user_email))
                .ok()
                .flatten();
        let free_access = if global_free {
            true
        } else {
            Ad4mDb::with_global_instance(|db| db.get_user_free_access(&user_email)).unwrap_or(false)
        };
        Some(HostingInfoUser {
            email: user_email,
            credits,
            hot_wallet_address,
            free_access,
        })
    } else {
        None
    };

    let rates = Ad4mDb::with_global_instance(|db| db.get_host_rates()).ok();

    let (dna_hash, build_version) = crate::unyt_service::version_info();
    let version = HostingVersionInfo {
        dna_hash,
        build_version,
    };

    Ok(serde_json::to_value(HostingInfoResult {
        user_info,
        rates,
        version,
    })?)
}

async fn get_hosting_wallet(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    // The node's own ledger: reading it can also create the node's ledger key.
    refuse_user_session(&ctx, "hosting.wallet")?;

    let balance = match crate::unyt_service::get_ledger().await {
        Ok(ledger) => Some(ledger),
        Err(e) => {
            log::warn!("Failed to get hot wallet balance: {}", e);
            None
        }
    };

    let pubkey = crate::unyt_service::get_or_create_agent_key().await.ok();

    Ok(serde_json::to_value(HostingWalletResult {
        balance,
        pubkey,
    })?)
}

async fn get_hosting_wallet_history(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    refuse_user_session(&ctx, "hosting.walletHistory")?;

    let history = crate::unyt_service::get_history(None, 50)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(history)
}

async fn request_payment(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: RequestPaymentRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    Ok(serde_json::to_value(HostingRequestPaymentResult {
        success: true,
        amount_hot: body.amount_hot,
    })?)
}

async fn set_hot_wallet(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: SetHotWalletAddressRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = ctx
        .user_email
        .clone()
        .ok_or_else(|| WsRpcError::forbidden("User email required"))?;

    Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(&email, &body.address))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<NoParams, HostingInfoResult>("hosting.info", get_hosting_info)
        .read();
    map.method::<NoParams, HostingWalletResult>("hosting.wallet", get_hosting_wallet)
        .read();
    // The Unyt zome's `get_history` output, passed through unparsed.
    map.method::<NoParams, Value>("hosting.walletHistory", get_hosting_wallet_history)
        .read();
    map.method::<RequestPaymentRequest, HostingRequestPaymentResult>(
        "hosting.requestPayment",
        request_payment,
    );
    map.method::<SetHotWalletAddressRequest, bool>("hosting.setHotWallet", set_hot_wallet);
}

// ── Contracts ───────────────────────────────────────────────────────────────

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingInfoResult {
    /// `null` outside multi-user mode.
    pub user_info: Option<HostingInfoUser>,
    /// `[token, rate]` pairs; `null` when the rates cannot be read.
    pub rates: Option<Vec<(String, f64)>>,
    pub version: HostingVersionInfo,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingInfoUser {
    pub email: String,
    /// `null` when the credits cannot be read.
    pub credits: Option<f64>,
    pub hot_wallet_address: Option<String>,
    pub free_access: bool,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingVersionInfo {
    /// The installed Unyt DNA version; `null` before installation.
    pub dna_hash: Option<String>,
    pub build_version: String,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingWalletResult {
    /// The Unyt zome's `get_ledger` output (open JSON); `null` on failure.
    pub balance: Option<Value>,
    pub pubkey: Option<String>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostingRequestPaymentResult {
    pub success: bool,
    #[serde(rename = "amountHOT")]
    pub amount_hot: String,
}
