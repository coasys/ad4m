//! Hosting WS-native handlers.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;
use crate::types::RequestContext;

use super::types::*;
use super::ws_handler::{HandlerMap, WsRpcError};

async fn get_hosting_info(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    let user_info = if let Some(user_email) = user_email_from_token(ctx.auth_token.clone()) {
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
        Some(serde_json::json!({
            "email": user_email,
            "credits": credits,
            "hotWalletAddress": hot_wallet_address,
            "freeAccess": free_access,
        }))
    } else {
        None
    };

    let rates = Ad4mDb::with_global_instance(|db| db.get_host_rates())
        .ok()
        .and_then(|v| serde_json::to_value(v).ok());

    let (dna_hash, build_version) = crate::unyt_service::version_info();
    let version = Some(serde_json::json!({
        "dnaHash": dna_hash,
        "buildVersion": build_version,
    }));

    Ok(serde_json::to_value(HostingInfoResponse {
        user_info,
        rates,
        version,
    })?)
}

async fn get_hosting_wallet(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let balance = match crate::unyt_service::get_ledger().await {
        Ok(ledger) => Some(ledger),
        Err(e) => {
            log::warn!("Failed to get hot wallet balance: {}", e);
            None
        }
    };

    let pubkey = crate::unyt_service::get_or_create_agent_key().await.ok();

    Ok(serde_json::to_value(HostingWalletResponse {
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

    Ok(serde_json::json!({
        "success": true,
        "amountHOT": body.amount_hot
    }))
}

async fn set_hot_wallet(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: SetHotWalletAddressRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = user_email_from_token(ctx.auth_token.clone())
        .ok_or_else(|| WsRpcError::forbidden("User email required"))?;

    Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(&email, &body.address))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("hosting.info", get_hosting_info);
    map.register("hosting.wallet", get_hosting_wallet);
    map.register("hosting.walletHistory", get_hosting_wallet_history);
    map.register("hosting.requestPayment", request_payment);
    map.register("hosting.setHotWallet", set_hot_wallet);
}
