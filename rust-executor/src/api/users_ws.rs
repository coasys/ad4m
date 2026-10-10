//! User management WS-native handlers.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;
use crate::types::{RequestContext, UserCreationResult, UserStatistics, VerificationRequestResult};

use super::types::*;
use super::ws_handler::{HandlerMap, NoParams, ParamExt, WsRpcError};

async fn get_multi_user_enabled(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let enabled = Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(enabled))
}

async fn set_multi_user_enabled(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("Admin credential required"));
    }

    let body: SetMultiUserRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(body.enabled))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
}

async fn list_users(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    if !crate::user_management::is_multi_user_enabled() {
        return Ok(serde_json::json!([]));
    }

    let users = Ad4mDb::with_global_instance(|db| db.list_user_statistics())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(users).unwrap_or_default())
}

async fn get_user_wallet(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_USER_MANAGEMENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let email = params.require_str("email")?;

    let wallet = Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&email))
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .ok_or_else(|| WsRpcError::not_found("Wallet not found"))?;

    Ok(Value::String(wallet))
}

async fn set_user_free_access(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("Admin credential required"));
    }

    let body: SetUserFreeAccessRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();
    Ad4mDb::with_global_instance(|db| db.set_user_free_access(&email, body.enabled)).map_err(
        |e| {
            let message = e.to_string();
            if message.contains("User not found") {
                WsRpcError::not_found(message)
            } else {
                WsRpcError::internal(message)
            }
        },
    )?;

    Ok(Value::Bool(true))
}

async fn create_user(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    use crate::user_management as um;

    check_capability(
        &ctx.capabilities,
        &RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CreateUserRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();

    if !um::is_multi_user_enabled() {
        return Ok(serde_json::json!({
            "did": "",
            "success": false,
            "error": "Multi-user mode is not enabled"
        }));
    }

    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(&email).is_ok());

    if user_exists {
        match um::verify_credentials(&email, &body.password) {
            Ok(()) => {
                let did = crate::agent::AgentService::get_user_did_by_email(&email)
                    .map_err(|e| WsRpcError::internal(e.to_string()))?;
                return Ok(serde_json::json!({ "did": did, "success": true }));
            }
            Err(e) => {
                return Ok(serde_json::json!({
                    "did": "",
                    "success": false,
                    "error": e,
                }));
            }
        }
    }

    let did =
        um::create_user(&email, &body.password).map_err(|e| WsRpcError::internal(e.to_string()))?;

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
                WsRpcError::internal(format!("Failed to store user agent profile: {}", e))
            })?;

        let ctx = AgentContext::for_user_email(email.clone());
        if let Err(e) = AgentService::publish_agent_to_language(&ctx).await {
            log::warn!("Failed to publish new user to agent language: {}", e);
        }
    }

    let code = um::create_verification_code(&email, "signup")
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    um::send_verification_email(&email, &code, "signup", None)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::json!({ "did": did, "success": true }))
}

async fn login_user(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UsersLoginParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt = crate::user_management::login_user(&email, &body.password, app_name)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(jwt))
}

async fn verify_email(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UsersVerifyEmailParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();
    let verification_type = body.verification_type.as_deref().unwrap_or("signup");
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt =
        crate::user_management::verify_and_login(&email, &body.code, verification_type, app_name)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(jwt))
}

async fn email_test(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &ALL_CAPABILITY).map_err(|e| WsRpcError::forbidden(e))?;

    let body: UsersEmailTestParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    match body.action {
        UsersEmailTestAction::Send => Err(WsRpcError::internal(
            "send_test_email not available as standalone function",
        )),
        UsersEmailTestAction::Enable => {
            crate::email_service::enable_test_mode();
            Ok(Value::Bool(true))
        }
        UsersEmailTestAction::Disable => {
            crate::email_service::disable_test_mode();
            Ok(Value::Bool(true))
        }
        UsersEmailTestAction::GetCode => {
            let email = body
                .email
                .ok_or_else(|| WsRpcError::bad_request("'email' required"))?;
            let code = crate::email_service::get_test_code(&email);
            Ok(serde_json::to_value(code).unwrap_or_default())
        }
        UsersEmailTestAction::Clear | UsersEmailTestAction::ClearCodes => {
            crate::email_service::clear_test_codes();
            Ok(Value::Bool(true))
        }
        UsersEmailTestAction::SetExpiry => {
            let email = body
                .email
                .ok_or_else(|| WsRpcError::bad_request("'email' required"))?;
            let verification_type = body
                .verification_type
                .ok_or_else(|| WsRpcError::bad_request("'verificationType' required"))?;
            let expires_at = body
                .expires_at
                .ok_or_else(|| WsRpcError::bad_request("'expiresAt' required"))?;

            Ad4mDb::with_global_instance(|db| {
                db.set_verification_code_expiry(&email, &verification_type, expires_at)
            })
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

            Ok(Value::Bool(true))
        }
    }
}

async fn request_verification(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::user_management as um;

    check_capability(
        &ctx.capabilities,
        &RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: UsersRequestVerificationParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();

    if !um::is_multi_user_enabled() {
        return Ok(serde_json::json!({
            "success": false,
            "message": "Multi-user mode is not enabled",
            "requiresPassword": false,
            "isExistingUser": false,
        }));
    }

    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(&email).is_ok());

    if !user_exists {
        return Ok(serde_json::json!({
            "success": true,
            "message": "No account found yet. Provide a password to create one.",
            "requiresPassword": true,
            "isExistingUser": false,
        }));
    }

    let smtp_available = crate::config::SMTP_CONFIG
        .lock()
        .ok()
        .and_then(|cfg| cfg.clone())
        .map(|c| c.enabled)
        .unwrap_or(false);
    let test_mode = crate::email_service::EMAIL_TEST_MODE
        .lock()
        .ok()
        .map(|mode| *mode)
        .unwrap_or(false);

    if !smtp_available && !test_mode {
        return Ok(serde_json::json!({
            "success": true,
            "message": "Email not configured. Please log in with your password.",
            "requiresPassword": true,
            "isExistingUser": true,
        }));
    }

    let app_name = body
        .app_info
        .as_ref()
        .and_then(|info| info.app_name.as_deref())
        .unwrap_or("ad4m");

    match um::request_login_code(&email, Some(app_name)).await {
        Ok(()) => Ok(serde_json::json!({
            "success": true,
            "message": "Verification code sent. Use verify_email_code to complete login.",
            "requiresPassword": false,
            "isExistingUser": true,
        })),
        Err(e) if e.contains("Please wait") => Ok(serde_json::json!({
            "success": false,
            "message": e,
            "requiresPassword": false,
            "isExistingUser": true,
        })),
        Err(e) => Err(WsRpcError::internal(e.to_string())),
    }
}

async fn users_credits(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    // Stub — not yet implemented
    let _ = params;
    Err(WsRpcError::not_implemented(
        "POST /users/credits is not yet implemented on the server",
    ))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<CreateUserRequest, UserCreationResult>("user.create", create_user);
    // The session JWT.
    map.method::<UsersLoginParams, String>("user.login", login_user);
    // The session JWT.
    map.method::<UsersVerifyEmailParams, String>("user.verifyEmail", verify_email);
    map.method::<NoParams, Vec<UserStatistics>>("user.list", list_users)
        .read();
    map.method::<NoParams, bool>("user.multiUserEnabled", get_multi_user_enabled)
        .read();
    map.method::<SetMultiUserRequest, bool>("user.setMultiUserEnabled", set_multi_user_enabled);
    map.method::<SetUserFreeAccessRequest, bool>("user.freeAccess", set_user_free_access);
    // Always answers 501; the contract is the SDK's call.
    map.method::<UsersSetCreditsParams, bool>("user.credits", users_credits);
    // The user's hot-wallet address (404 when none).
    map.method::<UsersEmailParams, String>("user.wallet", get_user_wallet)
        .read();
    map.method::<UsersEmailTestParams, UsersEmailTestResult>("user.emailTest", email_test);
    map.method::<UsersRequestVerificationParams, VerificationRequestResult>(
        "user.requestVerification",
        request_verification,
    );
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersSetCreditsParams {
    pub email: String,
    pub amount: f64,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersEmailParams {
    pub email: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersLoginParams {
    pub email: String,
    pub password: String,
    /// Defaults to `ad4m`.
    #[ts(optional)]
    pub app_name: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersVerifyEmailParams {
    pub email: String,
    pub code: String,
    /// Defaults to `signup`.
    #[ts(optional)]
    pub verification_type: Option<String>,
    /// Defaults to `ad4m`.
    #[ts(optional)]
    pub app_name: Option<String>,
}

/// The part of the caller's app info that login-code requests read.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersAppInfo {
    #[ts(optional)]
    pub app_name: Option<String>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersRequestVerificationParams {
    pub email: String,
    #[ts(optional)]
    pub app_info: Option<UsersAppInfo>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum UsersEmailTestAction {
    Send,
    Enable,
    Disable,
    GetCode,
    Clear,
    ClearCodes,
    SetExpiry,
}

/// `get-code` and `set-expiry` require `email`; `set-expiry` also requires
/// `verificationType` and `expiresAt`.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersEmailTestParams {
    pub action: UsersEmailTestAction,
    #[ts(optional)]
    pub email: Option<String>,
    #[ts(optional)]
    pub verification_type: Option<String>,
    /// Unix seconds; a JSON number, well inside `Number.MAX_SAFE_INTEGER`.
    #[ts(optional, type = "number")]
    pub expires_at: Option<i64>,
}

/// `get-code` answers the captured code (`null` when none); every other
/// action answers `true`.
#[derive(Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum UsersEmailTestResult {
    Done(bool),
    Code(Option<String>),
}
