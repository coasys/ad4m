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
    ctx.require(&RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let enabled = Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(enabled))
}

async fn set_multi_user_enabled(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    if !ctx.is_operator() {
        return Err(WsRpcError::forbidden("Admin credential required"));
    }

    let body: SetMultiUserRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(body.enabled))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
}

async fn list_users(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&RUNTIME_USER_MANAGEMENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    if !crate::user_management::is_multi_user_enabled() {
        return Ok(serde_json::json!([]));
    }

    let users = Ad4mDb::with_global_instance(|db| db.list_user_statistics())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(users).unwrap_or_default())
}

async fn get_user_wallet(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&RUNTIME_USER_MANAGEMENT_READ_CAPABILITY)
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
    if !ctx.is_operator() {
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

    ctx.require(&RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY)
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
                let did = crate::agent::AgentService::did_for_email(&email)
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
        AgentService::store_managed_agent_profile(&did, &agent).map_err(|e| {
            WsRpcError::internal(format!("Failed to store user agent profile: {}", e))
        })?;

        let ctx = AgentContext::managed(did.clone());
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
    ctx.require(&RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY)
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
    ctx.require(&RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY)
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
    ctx.require(&ALL_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

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

    ctx.require(&RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY)
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

/// user.logout — revokes the caller's own grant: its token stops working and its open
/// sockets close. The user's other logins keep working.
async fn logout(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let grant = ctx.session.user_grant().ok_or_else(|| {
        WsRpcError::bad_request("user.logout ends a user session; this session is not one")
    })?;
    crate::agent::grants::Grants::global()
        .revoke(std::slice::from_ref(grant))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
}

/// user.revokeAllSessions — revokes every grant on a user's agent: logins and apps. A user
/// revokes their own (the current one too); the operator revokes any user's, named by
/// `email`. Logins after it work. A session on the user's agent needs AGENT_UPDATE, as
/// `agent.revokeToken` and `agent.removeApp` do: an app the user approved acts as the
/// user's agent with only what it was granted (#1399), and a read-only one must not end
/// every login. A login holds AGENT_UPDATE.
async fn revoke_all_sessions(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: UsersRevokeAllSessionsParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    if ctx.user_did().is_some() {
        ctx.require(&AGENT_UPDATE_CAPABILITY)
            .map_err(WsRpcError::forbidden)?;
    }
    // The email names the user's agent: it resolves to the DID here, once. `None` when no
    // user with an agent has it.
    let did_of = |email: &str| {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_user(email).ok())
            .map(|user| user.did)
            .filter(|did| !did.is_empty())
    };
    let named = body.email.map(|email| email.trim().to_lowercase());
    let did = match (ctx.user_did(), named) {
        (Some(own), None) => own.to_string(),
        (Some(own), Some(named)) if did_of(&named).as_deref() == Some(own) => own.to_string(),
        (Some(_), Some(_)) => {
            return Err(WsRpcError::forbidden(
                "A user may revoke only their own sessions",
            ))
        }
        (None, Some(named)) if ctx.is_operator() => {
            did_of(&named).ok_or_else(|| WsRpcError::not_found("No user has this email"))?
        }
        (None, None) if ctx.is_operator() => {
            return Err(WsRpcError::bad_request(
                "'email' required: name the user whose sessions to revoke",
            ))
        }
        (None, _) => {
            return Err(WsRpcError::forbidden(
                "Only a user or the operator may revoke a user's sessions",
            ))
        }
    };
    crate::agent::grants::Grants::global()
        .revoke_agent(&crate::agent::AgentContext::managed(did))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(true))
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
    // The handlers check the session's principal: `user.logout` revokes only the caller's
    // own grant, whatever its capabilities; `user.revokeAllSessions` needs AGENT_UPDATE on
    // the user's agent, and only the operator may name another user.
    // Revokes the caller's user token; 400 for any other session. Answers `true`.
    map.method::<NoParams, bool>("user.logout", logout);
    // Revokes every token of a user issued up to now. Answers `true`.
    map.method::<UsersRevokeAllSessionsParams, bool>("user.revokeAllSessions", revoke_all_sessions);
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

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsersRevokeAllSessionsParams {
    /// The user whose sessions to revoke. A user may leave it out or name themselves; the
    /// operator must name the user.
    #[ts(optional)]
    pub email: Option<String>,
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

#[cfg(test)]
mod session_revocation_tests {
    //! #1370: user tokens could not be revoked: no logout, no "log out everywhere".
    use crate::agent::capabilities::{capabilities_from_token, AuthInfo, ListenerReach};
    use crate::agent::grants::Grants;
    use crate::agent::session::{AuthError, Session};
    use crate::agent::AgentContext;
    use crate::api::ws_handler::build_handler_map;
    use crate::db::Ad4mDb;
    use crate::test_utils::{granted_token, managed_user, setup_agent, setup_wallet};
    use crate::types::RequestContext;
    use crate::user_management::generate_user_jwt;
    use serde_json::{json, Value};
    use std::sync::Arc;

    const ADMIN: &str = "the-admin-credential";

    fn setup() {
        setup_wallet();
        setup_agent();
    }

    fn open(token: &str) -> Result<Session, AuthError> {
        Session::open(token, &Some(ADMIN.into()), ListenerReach::Network)
    }

    /// The context of a socket that presents `token`.
    fn ctx(token: &str) -> Arc<RequestContext> {
        let ctx = RequestContext::open(token, &Some(ADMIN.into()), ListenerReach::Network)
            .unwrap_or_else(|e| panic!("the token opens a session: {e}"));
        Arc::new(ctx)
    }

    async fn call(
        op: &str,
        params: Value,
        ctx: Arc<RequestContext>,
    ) -> Result<Value, (u16, String)> {
        build_handler_map()
            .dispatch(op, params, ctx)
            .await
            .map_err(|e| (e.code, e.message))
    }

    fn app_token(name: &str) -> String {
        granted_token(AuthInfo {
            app_name: name.to_string(),
            capabilities: Some(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]),
            ..Default::default()
        })
    }

    // D3: a login is an auto-approved grant on the user's agent, named after the app that
    // logged in.
    #[tokio::test]
    async fn a_login_is_a_grant_on_the_users_agent() {
        setup();
        let email = managed_user("login-grant");
        let token = generate_user_jwt(&email, "Flux").unwrap();
        let session = open(&token).unwrap();
        let grant = session.grant().expect("a login has a grant");
        assert_eq!(
            grant.agent,
            AgentContext::managed(crate::agent::AgentService::did_for_email(&email).unwrap())
        );
        assert_eq!(grant.app_name(), "Flux");
        assert_eq!(grant.request_id, grant.id.0, "a login answers no request");
        assert_eq!(
            grant.did,
            crate::agent::AgentService::did_for_email(&email).unwrap()
        );
    }

    #[tokio::test]
    async fn logout_revokes_the_callers_token_only() {
        setup();
        let email = managed_user("logout");
        let (laptop, phone) = (
            generate_user_jwt(&email, "test").unwrap(),
            generate_user_jwt(&email, "test").unwrap(),
        );
        let (laptop_session, phone_session) = (open(&laptop).unwrap(), open(&phone).unwrap());
        let laptop_socket = laptop_session.ended();

        let reply = call("user.logout", json!({}), ctx(&laptop)).await.unwrap();
        assert_eq!(reply, json!(true));
        assert_eq!(laptop_session.check(), Err(AuthError::Revoked));
        assert!(laptop_socket.is_cancelled(), "the laptop's sockets close");
        assert_eq!(open(&laptop).unwrap_err(), AuthError::Revoked);
        assert!(
            phone_session.check().is_ok(),
            "the user's other token works"
        );
        assert!(open(&phone).is_ok());
    }

    #[tokio::test]
    async fn logout_is_a_bad_request_for_a_session_without_a_user_token() {
        setup();
        let app = app_token("logout app");
        for caller in [ctx(ADMIN), ctx(&app)] {
            let (code, message) = call("user.logout", json!({}), caller).await.unwrap_err();
            assert_eq!(code, 400, "{message}");
            assert!(message.contains("user session"), "{message}");
        }
        assert!(open(&app).is_ok(), "the app's grant stays");
    }

    #[tokio::test]
    async fn revoke_all_refuses_every_earlier_token_and_allows_a_later_one() {
        setup();
        let email = managed_user("revoke-all");
        let other_user = managed_user("revoke-all-other");
        let earlier = [
            generate_user_jwt(&email, "test").unwrap(),
            generate_user_jwt(&email, "test").unwrap(),
        ];
        let sessions: Vec<Session> = earlier.iter().map(|t| open(t).unwrap()).collect();
        let others = generate_user_jwt(&other_user, "test").unwrap();

        let reply = call("user.revokeAllSessions", json!({}), ctx(&earlier[0]))
            .await
            .unwrap();
        assert_eq!(reply, json!(true));
        for (token, session) in earlier.iter().zip(&sessions) {
            assert_eq!(
                session.check(),
                Err(AuthError::Revoked),
                "the current one too"
            );
            assert_eq!(open(token).unwrap_err(), AuthError::Revoked);
            // MCP resolves tokens without a session; it refuses them as well.
            assert!(capabilities_from_token(token.clone(), &None).is_err());
        }

        // A login right after works: no cutoff, no same-second workaround.
        let later = generate_user_jwt(&email, "test").unwrap();
        assert!(open(&later).unwrap().check().is_ok());
        assert!(capabilities_from_token(later, &None).is_ok());
        assert!(open(&others).is_ok(), "another user's token works");
    }

    // Two revoke-alls with a login between them: the second ends that login.
    #[tokio::test]
    async fn a_second_revoke_all_ends_a_login_made_after_the_first() {
        setup();
        let email = managed_user("revoke-all-twice");
        let agent =
            AgentContext::managed(crate::agent::AgentService::did_for_email(&email).unwrap());
        let first = generate_user_jwt(&email, "test").unwrap();
        Grants::global().revoke_agent(&agent).unwrap();
        let between = generate_user_jwt(&email, "test").unwrap();
        assert!(open(&between).is_ok(), "a login after the first works");

        Grants::global().revoke_agent(&agent).unwrap();
        assert_eq!(open(&between).unwrap_err(), AuthError::Revoked);
        assert_eq!(open(&first).unwrap_err(), AuthError::Revoked);
        let after = generate_user_jwt(&email, "test").unwrap();
        assert!(open(&after).is_ok(), "a login after the second works");
    }

    #[tokio::test]
    async fn the_operator_revokes_any_users_sessions_and_a_user_only_their_own() {
        setup();
        let email = managed_user("revoke-all-target");
        let caller = managed_user("revoke-all-caller");
        let target = generate_user_jwt(&email, "test").unwrap();
        let callers = generate_user_jwt(&caller, "test").unwrap();

        let err = call(
            "user.revokeAllSessions",
            json!({ "email": email }),
            ctx(&callers),
        )
        .await
        .unwrap_err();
        assert_eq!(err.0, 403, "{}", err.1);
        assert!(open(&target).is_ok(), "the refused call revoked nothing");

        // A user may name themselves.
        call(
            "user.revokeAllSessions",
            json!({ "email": caller }),
            ctx(&callers),
        )
        .await
        .unwrap();
        assert_eq!(open(&callers).unwrap_err(), AuthError::Revoked);

        call(
            "user.revokeAllSessions",
            json!({ "email": email }),
            ctx(ADMIN),
        )
        .await
        .unwrap();
        assert_eq!(open(&target).unwrap_err(), AuthError::Revoked);
    }

    #[tokio::test]
    async fn revoke_all_refuses_callers_and_targets_it_cannot_serve() {
        setup();
        let app = app_token("revoke-all app");
        let email = managed_user("revoke-all-app-target");
        let target = generate_user_jwt(&email, "test").unwrap();
        let cases = [
            (ctx(&app), json!({ "email": email }), 403),
            (ctx(ADMIN), json!({}), 400),
            (ctx(ADMIN), json!({ "email": "nobody@example.org" }), 404),
        ];
        for (caller, params, expected) in cases {
            let (code, message) = call("user.revokeAllSessions", params.clone(), caller)
                .await
                .unwrap_err();
            assert_eq!(code, expected, "{params}: {message}");
        }
        assert!(open(&target).is_ok(), "the refused calls revoked nothing");
    }

    // A restart reads the revoked grants back from the database.
    #[tokio::test]
    async fn revoked_logins_stay_revoked_after_a_reload() {
        setup();
        let email = managed_user("revoke-all-db");
        let token = generate_user_jwt(&email, "test").unwrap();
        let id = open(&token).unwrap().grant().unwrap().id.clone();
        call(
            "user.revokeAllSessions",
            json!({ "email": email }),
            ctx(ADMIN),
        )
        .await
        .unwrap();

        let reloaded = Grants::default();
        Ad4mDb::with_global_instance(|db| reloaded.load(db)).unwrap();
        assert!(reloaded.get(&id).is_none());
        let listed = reloaded.list(&AgentContext::managed(
            crate::agent::AgentService::did_for_email(&email).unwrap(),
        ));
        assert_eq!(listed.len(), 1);
        assert!(listed[0].is_ended());
    }

    // `agent.revokeToken` answered 404 for a user's JWT: only app tokens could be revoked.
    #[tokio::test]
    async fn the_operator_revokes_a_user_token_by_its_jwt() {
        setup();
        let email = managed_user("revoke-token");
        let (revoked, kept) = (
            generate_user_jwt(&email, "test").unwrap(),
            generate_user_jwt(&email, "test").unwrap(),
        );
        call("agent.revokeToken", json!({ "token": revoked }), ctx(ADMIN))
            .await
            .unwrap();
        assert_eq!(open(&revoked).unwrap_err(), AuthError::Revoked);
        assert!(open(&kept).is_ok());
    }

    // D4: a user token issued before grants has no grant and is refused once; the user logs
    // in again.
    #[tokio::test]
    async fn a_user_token_from_before_grants_is_refused_and_a_new_login_works() {
        setup();
        let email = managed_user("pre-grants");
        let (old, _) = crate::agent::capabilities::token::sign_token(
            "test".to_string(),
            3600,
            Some(email.clone()),
        )
        .unwrap();
        let err = open(&old).unwrap_err();
        assert_eq!(err, AuthError::Revoked);
        assert_eq!(err.close_code(), 1008);
        assert!(open(&generate_user_jwt(&email, "test").unwrap()).is_ok());
    }
}
