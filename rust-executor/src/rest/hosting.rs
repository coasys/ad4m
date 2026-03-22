//! Hosting REST endpoints: /api/v1/hosting/*
//!
//! 3 consolidated endpoints covering hosting info, wallet balance, and history.

use axum::{extract::State, Json};

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /hosting — combined user info + rates
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
        Some(serde_json::json!({
            "email": user_email,
            "credits": credits,
            "hotWalletAddress": hot_wallet_address,
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
