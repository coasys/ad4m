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
use ad4m_rest_macros::rest_handler;

/// GET /users/multi-user-enabled
#[rest_handler(GET, "/users/multi-user-enabled", response = "boolean")]
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
#[rest_handler(
    PUT,
    "/users/multi-user-enabled",
    request = "SetMultiUserRequest",
    response = "boolean"
)]
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
#[rest_handler(GET, "/users", response = "UserStatistics[]")]
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

    if !crate::user_management::is_multi_user_enabled() {
        return Ok(Json(serde_json::json!([])));
    }

    let users = Ad4mDb::with_global_instance(|db| db.list_user_statistics())
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(users).unwrap_or_default()))
}

/// GET /users/:email/wallet — wallet address
#[rest_handler(GET, "/users/:email/wallet", response = "string")]
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

/// POST /users/free-access — toggle free access for a managed user
#[rest_handler(
    POST,
    "/users/free-access",
    request = "SetUserFreeAccessRequest",
    response = "boolean"
)]
pub async fn set_user_free_access(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SetUserFreeAccessRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    if !context.is_admin_credential {
        return Err(ApiError::Forbidden("Admin credential required".into()));
    }

    let email = body.email.trim().to_lowercase();
    Ad4mDb::with_global_instance(|db| db.set_user_free_access(&email, body.enabled)).map_err(
        |e| {
            let message = e.to_string();
            if message.contains("User not found") {
                ApiError::NotFound(message)
            } else {
                ApiError::Internal(message)
            }
        },
    )?;

    Ok(Json(true))
}

/// POST /users — create user
#[rest_handler(
    POST,
    "/users",
    request = "CreateUserRequest",
    response = "CreateUserResponse"
)]
pub async fn create_user(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<CreateUserRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    use crate::user_management as um;

    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let email = body.email.trim().to_lowercase();

    if !um::is_multi_user_enabled() {
        return Ok(Json(serde_json::json!({
            "did": "",
            "success": false,
            "error": "Multi-user mode is not enabled"
        })));
    }

    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(&email).is_ok());

    if user_exists {
        match um::verify_credentials(&email, &body.password) {
            Ok(()) => {
                let did = crate::agent::AgentService::get_user_did_by_email(&email)
                    .map_err(|e| ApiError::Internal(e.to_string()))?;
                return Ok(Json(serde_json::json!({ "did": did, "success": true })));
            }
            Err(e) => {
                return Ok(Json(serde_json::json!({
                    "did": "",
                    "success": false,
                    "error": e,
                })));
            }
        }
    }

    let did =
        um::create_user(&email, &body.password).map_err(|e| ApiError::Internal(e.to_string()))?;

    // Store minimal agent profile and publish to agent language
    {
        use crate::agent::{AgentContext, AgentService};
        use crate::types::domain::{Agent, Perspective as DomainPerspective};

        let agent = Agent {
            did: did.clone(),
            direct_message_language: None,
            perspective: Some(DomainPerspective { links: vec![] }),
        };
        AgentService::with_global_instance(|svc| svc.store_user_agent_profile(&email, &agent))
            .map_err(|e| {
                ApiError::Internal(format!("Failed to store user agent profile: {}", e))
            })?;

        let ctx = AgentContext::for_user_email(email.clone());
        if let Err(e) = AgentService::publish_agent_to_language(&ctx).await {
            log::warn!("Failed to publish new user to agent language: {}", e);
        }
    }

    let code = um::create_verification_code(&email, "signup")
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    um::send_verification_email(&email, &code, "signup", None)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::json!({ "did": did, "success": true })))
}

/// POST /users/login — login user
#[rest_handler(
    POST,
    "/users/login",
    request = "LoginUserRequest",
    response = "string"
)]
pub async fn login_user(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<LoginUserRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let email = body.email.trim().to_lowercase();
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt = crate::user_management::login_user(&email, &body.password, app_name)
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(jwt))
}

/// POST /users/verify-email — verify email code
#[rest_handler(
    POST,
    "/users/verify-email",
    request = "VerifyEmailRequest",
    response = "string"
)]
pub async fn verify_email(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<VerifyEmailRequest>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let email = body.email.trim().to_lowercase();
    let verification_type = body.verification_type.as_deref().unwrap_or("signup");
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt =
        crate::user_management::verify_and_login(&email, &body.code, verification_type, app_name)
            .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(jwt))
}

/// POST /dev/email-test — all email test operations (dev-only)
#[rest_handler(
    POST,
    "/dev/email-test",
    request = "EmailTestRequest",
    response = "unknown"
)]
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
        "clear" | "clear-codes" => {
            crate::email_service::clear_test_codes();
            Ok(Json(serde_json::json!(true)))
        }
        "set-expiry" => {
            let email = body
                .email
                .ok_or_else(|| ApiError::BadRequest("'email' required".into()))?;
            let verification_type = body
                .verification_type
                .ok_or_else(|| ApiError::BadRequest("'verificationType' required".into()))?;
            let expires_at = body
                .expires_at
                .ok_or_else(|| ApiError::BadRequest("'expiresAt' required".into()))?;

            Ad4mDb::with_global_instance(|db| {
                db.set_verification_code_expiry(&email, &verification_type, expires_at)
            })
            .map_err(|e| ApiError::Internal(e.to_string()))?;

            Ok(Json(serde_json::json!(true)))
        }
        other => Err(ApiError::BadRequest(format!("Unknown action: {}", other))),
    }
}

/// POST /users/request-verification
#[rest_handler(
    POST,
    "/users/request-verification",
    request = "RequestVerificationRequest",
    response = "VerificationRequestResult"
)]
pub async fn request_verification(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<RequestVerificationRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    use crate::user_management as um;

    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let email = body.email.trim().to_lowercase();

    if !um::is_multi_user_enabled() {
        return Ok(Json(serde_json::json!({
            "success": false,
            "message": "Multi-user mode is not enabled",
            "requiresPassword": false,
            "isExistingUser": false,
        })));
    }

    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(&email).is_ok());

    if !user_exists {
        return Ok(Json(serde_json::json!({
            "success": true,
            "message": "No account found yet. Provide a password to create one.",
            "requiresPassword": true,
            "isExistingUser": false,
        })));
    }

    let app_name = body
        .app_info
        .as_ref()
        .and_then(|info| info.get("appName"))
        .and_then(|value| value.as_str())
        .unwrap_or("ad4m");

    match um::request_login_code(&email, Some(app_name)).await {
        Ok(()) => Ok(Json(serde_json::json!({
            "success": true,
            "message": "Verification code sent. Use verify_email_code to complete login.",
            "requiresPassword": false,
            "isExistingUser": true,
        }))),
        Err(e) if e.contains("Please wait") => Ok(Json(serde_json::json!({
            "success": false,
            "message": e,
            "requiresPassword": false,
            "isExistingUser": true,
        }))),
        Err(e) => Err(ApiError::Internal(e.to_string())),
    }
}
