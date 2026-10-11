//! User management WS-native handlers.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::db::Ad4mDb;
use crate::types::{RequestContext, UserCreationResult, UserStatistics, VerificationRequestResult};

use super::guards::Guard;
use super::types::*;
use super::ws_handler::{HandlerMap, NoParams, ParamExt, WsRpcError};

async fn get_multi_user_enabled(
    _params: Value,
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
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

async fn list_users(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    if !crate::user_management::is_multi_user_enabled() {
        return Ok(serde_json::json!([]));
    }

    let users = Ad4mDb::with_global_instance(|db| db.list_user_statistics())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(users).unwrap_or_default())
}

async fn get_user_wallet(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
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

async fn create_user(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    use crate::user_management as um;

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

async fn login_user(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: UsersLoginParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let email = body.email.trim().to_lowercase();
    let app_name = body.app_name.as_deref().unwrap_or("ad4m");
    let jwt = crate::user_management::login_user(&email, &body.password, app_name)
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(jwt))
}

async fn verify_email(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
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

async fn email_test(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
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
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    use crate::user_management as um;

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

/// user.logout — revokes the caller's own user token. Its open sockets close.
async fn logout(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let (token_id, expires_at) = ctx.session.user_token().ok_or_else(|| {
        WsRpcError::bad_request("user.logout ends a user session; this session is not one")
    })?;
    crate::agent::revoked::revoke(token_id.clone(), expires_at).map_err(WsRpcError::internal)?;
    Ok(Value::Bool(true))
}

/// user.revokeAllSessions — revokes every token of a user issued up to now. A user revokes
/// their own (the current one too); the operator revokes any user's, named by `email`.
async fn revoke_all_sessions(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    use crate::agent::revoked::{revoke_all_sessions, RevokeAllError};

    let body: UsersRevokeAllSessionsParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let named = body.email.map(|email| email.trim().to_lowercase());
    let email = match (ctx.user_email(), named) {
        (Some(own), None) => own.to_string(),
        (Some(own), Some(named)) if named == own => named,
        (Some(_), Some(_)) => {
            return Err(WsRpcError::forbidden(
                "A user may revoke only their own sessions",
            ))
        }
        (None, Some(named)) if ctx.is_operator() => named,
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
    revoke_all_sessions(&email).map_err(|e| match e {
        RevokeAllError::NoSuchUser => WsRpcError::not_found(e.to_string()),
        RevokeAllError::Store(_) => WsRpcError::internal(e.to_string()),
    })?;
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
    // Signing up, logging in and verifying an email: callers without a token hold these
    // capabilities while multi-user mode is on.
    map.method::<CreateUserRequest, UserCreationResult>("user.create", create_user)
        .guard(Guard::Anonymous(
            RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY.clone(),
        ));
    // The session JWT.
    map.method::<UsersLoginParams, String>("user.login", login_user)
        .guard(Guard::Anonymous(
            RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY.clone(),
        ));
    // The session JWT.
    map.method::<UsersVerifyEmailParams, String>("user.verifyEmail", verify_email)
        .guard(Guard::Anonymous(
            RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY.clone(),
        ));
    // No guard on these two: they check no capability, since a user acts on their own
    // sessions whatever their capabilities, and every guard checks one. Only the operator may
    // name another user. The handlers check the session's principal.
    // Revokes the caller's user token; 400 for any other session. Answers `true`.
    map.method::<NoParams, bool>("user.logout", logout);
    // Revokes every token of a user issued up to now. Answers `true`.
    map.method::<UsersRevokeAllSessionsParams, bool>("user.revokeAllSessions", revoke_all_sessions);
    // Every user's record: the default user capabilities leave this capability out.
    map.method::<NoParams, Vec<UserStatistics>>("user.list", list_users)
        .read()
        .guard(Guard::Node(RUNTIME_USER_MANAGEMENT_READ_CAPABILITY.clone()));
    map.method::<NoParams, bool>("user.multiUserEnabled", get_multi_user_enabled)
        .read()
        .guard(Guard::Anonymous(
            RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
        ));
    // Operator only, with no guard: the handlers answer every other caller "Admin credential
    // required" (403). A guard checks a capability first, so it would answer a caller without
    // `ALL` with the capability's message instead.
    map.method::<SetMultiUserRequest, bool>("user.setMultiUserEnabled", set_multi_user_enabled);
    map.method::<SetUserFreeAccessRequest, bool>("user.freeAccess", set_user_free_access);
    // Always answers 501; the contract is the SDK's call. No guard: it checks no capability,
    // and every guard checks one.
    map.method::<UsersSetCreditsParams, bool>("user.credits", users_credits);
    // Any user's hot-wallet address, named by email (404 when none).
    map.method::<UsersEmailParams, String>("user.wallet", get_user_wallet)
        .read()
        .guard(Guard::Node(RUNTIME_USER_MANAGEMENT_READ_CAPABILITY.clone()));
    // Test hooks of the email service, for the node's own test runs.
    map.method::<UsersEmailTestParams, UsersEmailTestResult>("user.emailTest", email_test)
        .guard(Guard::Node(ALL_CAPABILITY.clone()));
    map.method::<UsersRequestVerificationParams, VerificationRequestResult>(
        "user.requestVerification",
        request_verification,
    )
    .guard(Guard::Anonymous(
        RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY.clone(),
    ));
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
mod operator_tests {
    use crate::agent::capabilities::{get_user_default_capabilities, ALL_CAPABILITY};
    use crate::api::ws_handler::HandlerMap;
    use crate::types::RequestContext;
    use serde_json::json;
    use std::sync::Arc;

    // Only the admin credential: an app that holds every capability is refused too, before
    // the handler reads the params or the DB.
    #[tokio::test]
    async fn node_wide_user_settings_refuse_an_app_holding_every_capability_and_users() {
        let mut map = HandlerMap::new();
        super::register_ws_handlers(&mut map);
        let callers = [
            RequestContext::app_for_test(vec![ALL_CAPABILITY.clone()]),
            RequestContext::user_for_test(
                "settings@example.org",
                "did:key:settings",
                get_user_default_capabilities(),
            ),
        ];
        for (method, params) in [
            ("user.setMultiUserEnabled", json!({ "enabled": true })),
            (
                "user.freeAccess",
                json!({ "email": "a@example.org", "enabled": true }),
            ),
        ] {
            for ctx in &callers {
                let err = map
                    .dispatch(method, params.clone(), Arc::new(ctx.clone()))
                    .await
                    .unwrap_err();
                assert_eq!(err.code, 403, "{method}: {}", err.message);
                assert_eq!(err.message, "Admin credential required", "{method}");
            }
        }
    }
}

#[cfg(test)]
mod session_revocation_tests {
    //! #1370: user tokens could not be revoked: no logout, no "log out everywhere".
    use crate::agent::capabilities::{check_not_revoked, decode_jwt, ListenerReach};
    use crate::agent::revoked::SessionCutoffs;
    use crate::agent::session::{AuthError, Session};
    use crate::api::auth::AuthContext;
    use crate::api::ws_handler::build_handler_map;
    use crate::db::Ad4mDb;
    use crate::test_utils::{managed_user, setup_agent, setup_wallet, use_test_apps_file};
    use crate::types::RequestContext;
    use crate::user_management::generate_user_jwt;
    use serde_json::{json, Value};
    use std::sync::Arc;

    const ADMIN: &str = "the-admin-credential";

    fn setup() {
        setup_wallet();
        setup_agent();
        use_test_apps_file();
    }

    fn open(token: &str) -> Result<Session, AuthError> {
        Session::open(token, &Some(ADMIN.into()), ListenerReach::Network)
    }

    /// The context of a socket that presents `token`.
    fn ctx(token: &str) -> Arc<RequestContext> {
        let auth = AuthContext::open(token, &Some(ADMIN.into()), false, ListenerReach::Network)
            .unwrap_or_else(|e| panic!("the token opens a session: {e}"));
        Arc::new(auth.to_request_context())
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

    #[tokio::test]
    async fn logout_revokes_the_callers_token_only() {
        setup();
        let email = managed_user("logout");
        let (laptop, phone) = (
            generate_user_jwt(&email, "test").unwrap(),
            generate_user_jwt(&email, "test").unwrap(),
        );
        let (laptop_session, phone_session) = (open(&laptop).unwrap(), open(&phone).unwrap());

        let reply = call("user.logout", json!({}), ctx(&laptop)).await.unwrap();
        assert_eq!(reply, json!(true));
        assert_eq!(laptop_session.check(), Err(AuthError::Revoked));
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
        let app = crate::agent::capabilities::generate_jwt(
            "logout app".to_string(),
            3600,
            Default::default(),
        )
        .unwrap();
        for caller in [ctx(ADMIN), ctx(&app)] {
            let (code, message) = call("user.logout", json!({}), caller).await.unwrap_err();
            assert_eq!(code, 400, "{message}");
            assert!(message.contains("user session"), "{message}");
        }
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
            assert!(check_not_revoked(&decode_jwt(token.clone()).unwrap()).is_err());
        }

        // Issued within the same second as the cutoff, yet after it.
        let later = generate_user_jwt(&email, "test").unwrap();
        assert!(open(&later).unwrap().check().is_ok());
        assert!(check_not_revoked(&decode_jwt(later).unwrap()).is_ok());
        assert!(open(&others).is_ok(), "another user's token works");
    }

    // Two revoke-alls within one second, with a login between them: the second must still
    // end that login, whose iat is the first cutoff + 1.
    #[tokio::test]
    async fn a_second_revoke_all_in_the_same_second_ends_a_login_made_between_them() {
        setup();
        let email = managed_user("revoke-all-twice");
        let first = generate_user_jwt(&email, "test").unwrap();
        crate::agent::revoked::revoke_all_sessions(&email).unwrap();
        let between = generate_user_jwt(&email, "test").unwrap();
        assert!(
            open(&between).is_ok(),
            "a login after the first cutoff works"
        );

        crate::agent::revoked::revoke_all_sessions(&email).unwrap();
        assert_eq!(open(&between).unwrap_err(), AuthError::Revoked);
        assert_eq!(open(&first).unwrap_err(), AuthError::Revoked);
        let after = generate_user_jwt(&email, "test").unwrap();
        assert!(
            open(&after).is_ok(),
            "a login after the second cutoff works"
        );
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
        let app = crate::agent::capabilities::generate_jwt(
            "revoke-all app".to_string(),
            3600,
            crate::agent::capabilities::AuthInfo {
                capabilities: Some(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]),
                ..Default::default()
            },
        )
        .unwrap();
        let email = managed_user("revoke-all-app-target");
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
        assert_eq!(SessionCutoffs::global().get("nobody@example.org"), None);
    }

    // A restart reads the cutoff back from the users table.
    #[tokio::test]
    async fn the_cutoff_is_in_the_database() {
        setup();
        let email = managed_user("revoke-all-db");
        let token = generate_user_jwt(&email, "test").unwrap();
        call(
            "user.revokeAllSessions",
            json!({ "email": email }),
            ctx(ADMIN),
        )
        .await
        .unwrap();

        let reloaded = Ad4mDb::with_global_instance(SessionCutoffs::load).unwrap();
        let issued_at = decode_jwt(token).unwrap().issued_at();
        assert!(reloaded.refuses(&email, issued_at));
        assert_eq!(reloaded.get(&email), SessionCutoffs::global().get(&email));
    }

    #[test]
    fn a_reopened_database_file_keeps_the_cutoff() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("cutoffs.sqlite");
        let path = path.to_str().unwrap();
        let db = Ad4mDb::new(path).unwrap();
        db.add_user_prehashed("kept@example.org", "did:key:kept", "unused")
            .unwrap();
        assert!(db
            .set_user_sessions_valid_after("kept@example.org", 1_000)
            .unwrap());
        assert!(
            !db.set_user_sessions_valid_after("nobody@example.org", 1_000)
                .unwrap(),
            "no row for an unknown user"
        );
        drop(db);

        let cutoffs = SessionCutoffs::load(&Ad4mDb::new(path).unwrap()).unwrap();
        assert_eq!(cutoffs.get("kept@example.org"), Some(1_000));
        assert!(cutoffs.refuses("kept@example.org", 1_000));
        assert!(!cutoffs.refuses("kept@example.org", 1_001));
    }

    #[test]
    fn a_cutoff_never_moves_back() {
        let cutoffs = SessionCutoffs::default();
        cutoffs.set("a@example.org", 2_000);
        cutoffs.set("a@example.org", 1_000);
        assert_eq!(cutoffs.get("a@example.org"), Some(2_000));
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
}
