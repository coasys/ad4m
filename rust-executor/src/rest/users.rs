//! User management REST endpoints: /api/v1/users/*
//!
//! 7 harmonised endpoints + dev-only email test.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /users/multi-user-enabled
pub async fn get_multi_user_enabled(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let enabled = Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled())
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(enabled))
}

/// PUT /users/multi-user-enabled
pub async fn set_multi_user_enabled(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SetMultiUserRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    // Only admin can toggle multi-user mode
    if !context.is_admin_credential {
        return Err(ApiError::Forbidden("Admin credential required".into()));
    }

    Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(body.enabled))
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(true))
}

/// GET /users — list users
pub async fn list_users(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let users = Ad4mDb::with_global_instance(|db| db.list_users())
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(users).unwrap_or_default()))
}

/// GET /users/:email/wallet — wallet address
pub async fn get_user_wallet(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(email): Path<String>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let wallet = Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&email))
        .map_err(|e| ApiError::Internal(e.to_string()))?
        .ok_or_else(|| ApiError::NotFound("Wallet not found".into()))?;

    Ok(Json(wallet))
}

/// POST /users — create user
pub async fn create_user(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreateUserRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let did = crate::user_management::create_user(&body.email, &body.password)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::json!({ "did": did, "success": true })))
}

/// POST /users/login — login user
pub async fn login_user(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<LoginUserRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt = crate::user_management::login_user(&body.email, &body.password, app_name)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::json!({ "jwt": jwt })))
}

/// POST /users/verify-email — verify email code
pub async fn verify_email(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<VerifyEmailRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let verification_type = body.verification_type.as_deref().unwrap_or("signup");
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt = crate::user_management::verify_and_login(
        &body.email,
        &body.code,
        verification_type,
        app_name,
    )
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::json!({ "jwt": jwt })))
}

/// POST /dev/email-test — all email test operations (dev-only)
pub async fn email_test(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<EmailTestRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    // Require ALL capability (admin only)
    check_capability(&context.capabilities, &ALL_CAPABILITY).map_err(|e| ApiError::Forbidden(e))?;

    match body.action.as_str() {
        "send" => {
            // send_test_email requires an EmailService instance; not available as free fn
            let _to = body
                .to
                .ok_or_else(|| ApiError::BadRequest("'to' required".into()))?;
            Err(ApiError::Internal(
                "send_test_email not available as standalone function".into(),
            ))
        }
        "enable" => {
            crate::email_service::enable_test_mode();
            Ok(Json(serde_json::json!(true)))
        }
        "disable" => {
            crate::email_service::disable_test_mode();
            Ok(Json(serde_json::json!(true)))
        }
        "get-code" => {
            let email = body
                .email
                .ok_or_else(|| ApiError::BadRequest("'email' required".into()))?;
            let code = crate::email_service::get_test_code(&email);
            Ok(Json(serde_json::to_value(code).unwrap_or_default()))
        }
        "clear" => {
            crate::email_service::clear_test_codes();
            Ok(Json(serde_json::json!(true)))
        }
        "set-expiry" => {
            // set_test_expiry is not implemented
            Err(ApiError::Internal("set_test_expiry not implemented".into()))
        }
        other => Err(ApiError::BadRequest(format!("Unknown action: {}", other))),
    }
}

/// POST /users/request-verification
pub async fn request_verification(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(_body): Json<RequestVerificationRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    // TODO: implement request_login_verification
    Err(ApiError::Internal(
        "request_verification not yet implemented".into(),
    ))
}
