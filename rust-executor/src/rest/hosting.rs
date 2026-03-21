//! Hosting REST endpoints: /api/v1/hosting/*
//!
//! 3 consolidated endpoints.

use axum::{extract::State, Json};

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /hosting — combined user info + rates + version info
pub async fn get_hosting_info(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<HostingInfoResponse>, ApiError> {
    let context = auth.to_request_context();

    // User info (may require RUNTIME_HOSTING_READ_CAPABILITY)
    let user_info = Ad4mDb::with_global_instance(|db| {
        db.get_hosting_user_info()
    })
    .ok()
    .and_then(|v| serde_json::to_value(v).ok());

    // Rates
    let rates = Ad4mDb::with_global_instance(|db| {
        db.get_host_rates_json()
    })
    .ok()
    .and_then(|v| serde_json::to_value(v).ok());

    // Version info
    let version = crate::unyt_service::get_version_info()
        .await
        .ok()
        .and_then(|v| serde_json::to_value(v).ok());

    Ok(Json(HostingInfoResponse {
        user_info,
        rates,
        version,
    }))
}

/// GET /hosting/wallet — balance + pubkey
pub async fn get_hosting_wallet(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<HostingWalletResponse>, ApiError> {
    let balance = crate::wallet::get_hot_wallet_balance()
        .await
        .ok()
        .and_then(|v| serde_json::to_value(v).ok());

    let pubkey = crate::wallet::get_hot_agent_pubkey()
        .await
        .ok();

    Ok(Json(HostingWalletResponse { balance, pubkey }))
}

/// GET /hosting/wallet/history — transaction history
pub async fn get_hosting_wallet_history(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let history = crate::wallet::get_hot_wallet_history()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(history).unwrap_or_default()))
}
