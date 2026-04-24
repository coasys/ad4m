//! Hosting REST endpoints: /api/v1/hosting/*
//!
//! 3 consolidated endpoints covering hosting info, wallet balance, and history.

use axum::{extract::State, Json};

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;
use ad4m_rest_macros::rest_handler;

/// GET /hosting — combined user info + rates
#[rest_handler(GET, "/hosting", response = "HostingInfoResponse")]
pub async fn get_hosting_info(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<HostingInfoResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // User info for the current user
    let user_info = if let Some(user_email) = user_email_from_token(context.auth_token.clone()) {
        let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(&user_email)).ok();
        let hot_wallet_address =
            Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&user_email))
                .ok()
                .flatten();
        let free_access = Ad4mDb::with_global_instance(|db| db.get_user_free_access(&user_email))
            .unwrap_or(false);
        Some(serde_json::json!({
            "email": user_email,
            "credits": credits,
            "hotWalletAddress": hot_wallet_address,
            "freeAccess": free_access,
        }))
    } else {
        None
    };

    // Host rates
    let rates = Ad4mDb::with_global_instance(|db| db.get_host_rates())
        .ok()
        .and_then(|v| serde_json::to_value(v).ok());

    // Version info
    let (dna_hash, build_version) = crate::unyt_service::version_info();
    let version = Some(serde_json::json!({
        "dnaHash": dna_hash,
        "buildVersion": build_version,
    }));

    Ok(Json(HostingInfoResponse {
        user_info,
        rates,
        version,
    }))
}

/// GET /hosting/wallet — balance from unyt ledger + agent pubkey
#[rest_handler(GET, "/hosting/wallet", response = "HostingWalletResponse")]
pub async fn get_hosting_wallet(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<HostingWalletResponse>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let balance = match crate::unyt_service::get_ledger().await {
        Ok(ledger) => Some(ledger),
        Err(e) => {
            log::warn!("Failed to get hot wallet balance: {}", e);
            None
        }
    };

    let pubkey = crate::unyt_service::get_or_create_agent_key().await.ok();

    Ok(Json(HostingWalletResponse { balance, pubkey }))
}

/// GET /hosting/wallet/history — transaction history
#[rest_handler(GET, "/hosting/wallet/history", response = "unknown")]
pub async fn get_hosting_wallet_history(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_HOSTING_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let history = crate::unyt_service::get_history(None, 50)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(history))
}

/// PUT /hosting/wallet/hot-wallet-address
#[rest_handler(
    PUT,
    "/hosting/wallet/hot-wallet-address",
    request = "SetHotWalletAddressRequest",
    response = "boolean"
)]
pub async fn set_hot_wallet_address(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SetHotWalletAddressRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let email = user_email_from_token(context.auth_token.clone())
        .ok_or_else(|| ApiError::Forbidden("User email required".into()))?;

    Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(&email, &body.address))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /hosting/request-payment
#[rest_handler(
    POST,
    "/hosting/request-payment",
    request = "RequestPaymentRequest",
    response = "unknown"
)]
pub async fn request_payment(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<RequestPaymentRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // TODO: implement actual payment request logic
    Ok(Json(serde_json::json!({
        "success": true,
        "amountHOT": body.amount_hot
    })))
}
