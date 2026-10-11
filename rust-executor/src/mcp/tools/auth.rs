//! Authentication tools
//!
//! Tools for JWT auth, capability tokens, multi-user signup/login.

use super::{Ad4mMcpHandler, RequestAuth};
use crate::agent::capabilities::{
    apps_map, defs::ALL_CAPABILITY, generate_capability_token, permit_capability,
    request_capability as cap_request_capability, AuthInfo, AuthInfoExtended,
};
use crate::agent::session::{AuthError, Principal, Session};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for email/password login (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct LoginEmailParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a capability token (local connect flow)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestCapabilityParams {
    /// Application name requesting access
    pub app_name: String,
    /// Application description
    pub app_desc: String,
    /// Optional application domain
    #[serde(default)]
    pub app_domain: Option<String>,
    /// Optional application URL
    #[serde(default)]
    pub app_url: Option<String>,
}

/// Parameters for generating a JWT from a capability request
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GenerateJwtParams {
    /// Request ID returned from request_capability
    pub request_id: String,
    /// 6-digit code from the executor log
    pub code: String,
}

/// Parameters for user signup (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SignupParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a login verification code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestLoginVerificationParams {
    /// User email address
    pub email: String,
}

/// Parameters for verifying an email code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct VerifyEmailCodeParams {
    /// User email address
    pub email: String,
    /// 6-digit verification code
    pub code: String,
    /// Type: "signup" or "login"
    pub verification_type: String,
}

/// Parameters for checking authentication status (no params needed)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AuthStatusParams {}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Login with email and password (multi-user mode)
    #[tool(
        description = "Login to a multi-user AD4M executor using email and password. Returns a JWT token on success that will be used for subsequent operations."
    )]
    pub async fn login_email(&self, params: Parameters<LoginEmailParams>) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::login_user(&email, &params.0.password, "mcp-agent") {
            Ok(token) => {
                self.store_token_and_respond(token, Some(&email), "Login successful.")
                    .await
            }
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Request a capability token (local connect flow - step 1)
    #[tool(
        description = "Request a capability token (step 1/2 of local auth flow). This is the primary way to authenticate with a local/single-user AD4M executor. Returns request_id and code — pass both to generate_jwt to get a JWT token. On an executor started with --admin-credential, the request is only auto-permitted (code returned inline) when the caller's session holds the PERMIT capability: the admin credential, or a token with full rights. Any other caller, a multi-user user token included, gets only the request_id; the executor admin approves the request and provides the code out-of-band. For multi-user executors, use login_email or signup instead. Note: when using the ad4m-executor CLI, the verification code is logged to stdout."
    )]
    pub async fn request_capability(
        &self,
        params: Parameters<RequestCapabilityParams>,
        context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> String {
        // `request_capability` is in AUTH_TOOLS, so `call_tool` never rejects it —
        // it must stay reachable to *create* a request.
        self.request_capability_as(RequestAuth::of(&context), params.0)
            .await
    }

    /// Context-free body of [`Self::request_capability`]: tests cannot build an rmcp
    /// `RequestContext`. Creating the request needs only that the tool stay reachable;
    /// auto-permitting it (the inline code that generate_jwt mints into an ALL_CAPABILITY
    /// token) needs the PERMIT capability, which a multi-user user token does not hold.
    pub(crate) async fn request_capability_as(
        &self,
        request: RequestAuth,
        params: RequestCapabilityParams,
    ) -> String {
        let caller_can_permit = self.caller_can_permit(request).await;
        self.handle_capability_request(params, caller_can_permit)
            .await
    }

    /// Body of [`Self::request_capability`], split from the transport wrapper so
    /// tests can drive both permit verdicts: a `RequestContext<RoleServer>`
    /// cannot be fabricated outside rmcp (its `Peer` constructor is `pub(crate)`,
    /// see `harness_bridge.rs`), so the wrapper only adopts the header credential
    /// and asks whether the session may permit; everything decidable lives here.
    ///
    /// The request itself is always created — that publishes the capability-request
    /// exception the ADAM Launcher listens for, which is the legitimate pairing
    /// flow. What a caller whose session lacks PERMIT must NOT get is the auto-permit and the
    /// inline code: the code is the mint secret (`generate_jwt` exchanges it for a
    /// signed ALL_CAPABILITY JWT valid on every executor surface), and returning it
    /// to any caller that could reach the MCP port made `--admin-credential`
    /// decorative (issue #851, sub-problem 3).
    ///
    /// Scope: without an admin credential, a caller with no token on a loopback
    /// listener is the operator, so it still gets the inline code. On a multi-user
    /// node that includes a user who drops their token or opens a new session
    /// (#1059). Such a server binds loopback only (#1033); on a listener beyond
    /// loopback (an explicit `MCP_HOST`) that caller is anonymous and gets no code.
    pub(crate) async fn handle_capability_request(
        &self,
        p: RequestCapabilityParams,
        caller_can_permit: bool,
    ) -> String {
        let auth_info = AuthInfo {
            app_name: p.app_name.clone(),
            app_desc: p.app_desc.clone(),
            app_domain: p.app_domain.clone(),
            app_url: p.app_url.clone(),
            app_icon_path: None,
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            user_email: None,
        };

        let request_id = cap_request_capability(auth_info.clone()).await;

        if !caller_can_permit {
            return json!({
                "request_id": request_id,
                "message": "Capability request created but NOT auto-permitted: this session may \
                    not grant capabilities. The executor admin must approve the request (ADAM \
                    Launcher), then call generate_jwt with this request_id and the code shown \
                    to the admin. Only the admin credential, or a token that holds the PERMIT \
                    capability, auto-permits; a user token never does."
            })
            .to_string();
        }

        match permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: auth_info,
        }) {
            Ok(code) => {
                // Unified secret-log gate (rust-executor/LOGGING.md):
                // AD4M_LOG_SECRETS=1 opts in to the raw capability code;
                // otherwise redacted. Consistent with agent_ws
                // auto-permit challenge and email_service verification
                // code.
                let code_repr = if std::env::var("AD4M_LOG_SECRETS")
                    .map(|v| v == "1")
                    .unwrap_or(false)
                {
                    code.clone()
                } else {
                    "<redacted; set AD4M_LOG_SECRETS=1 to log>".to_string()
                };
                log::debug!(
                    "🔐 MCP capability request permitted (request_id={}, code={})",
                    request_id,
                    code_repr
                );
                json!({
                    "request_id": request_id,
                    "code": code,
                    "message": "Capability requested and auto-permitted. Use generate_jwt with these values to get a token."
                })
                .to_string()
            }
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to permit capability: {}", e)
            })
            .to_string(),
        }
    }

    /// Generate a JWT from a capability request (local connect flow - step 2)
    #[tool(
        description = "Generate a JWT token (step 2/2 of local auth flow). Pass the request_id and code from request_capability. The JWT is stored in the session and used for all subsequent operations automatically."
    )]
    pub async fn generate_jwt(&self, params: Parameters<GenerateJwtParams>) -> String {
        let p = &params.0;

        match generate_capability_token(p.request_id.clone(), p.code.clone()).await {
            Ok(cap_token) => {
                self.store_token_and_respond(
                    cap_token.expose_once(),
                    None,
                    "JWT generated and stored. You are now authenticated.",
                )
                .await
            }
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to generate JWT: {}", e)
            })
            .to_string(),
        }
    }

    /// Sign up a new user (multi-user mode)
    #[tool(
        description = "Create a new user account (multi-user mode). Sends a verification email with a code. Use verify_email_code to complete signup."
    )]
    pub async fn signup(&self, params: Parameters<SignupParams>) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::signup_user(&email, &params.0.password, Some("MCP Agent")).await {
            Ok(did) => json!({
                "success": true,
                "did": did,
                "message": "User created. Check your email for a verification code and call verify_email_code."
            }).to_string(),
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Request a login verification code (multi-user mode)
    #[tool(
        description = "Request a login verification code to be sent to the user's email. Use verify_email_code to complete login."
    )]
    pub async fn request_login_verification(
        &self,
        params: Parameters<RequestLoginVerificationParams>,
    ) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::request_login_code(&email, Some("MCP Agent")).await {
            Ok(()) => json!({
                "success": true,
                "message": "Verification code sent. Use verify_email_code to complete login."
            })
            .to_string(),
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Verify an email code for signup or login (multi-user mode)
    #[tool(
        description = "Verify an email code to complete signup or login. Returns a JWT token on success. The verification_type must be 'signup' or 'login'."
    )]
    pub async fn verify_email_code(&self, params: Parameters<VerifyEmailCodeParams>) -> String {
        use crate::user_management as um;
        let p = &params.0;
        let email = p.email.trim().to_lowercase();

        match um::verify_and_login(&email, &p.code, &p.verification_type, "mcp-agent") {
            Ok(token) => {
                self.store_token_and_respond(
                    token,
                    Some(&email),
                    "Email verified. Token stored for subsequent operations.",
                )
                .await
            }
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Check current authentication status
    #[tool(
        description = "Check the current authentication status of the MCP session: whether its token (the Authorization header, or the one a login tool set) works, who it acts as and which capabilities it holds."
    )]
    pub async fn auth_status(
        &self,
        _params: Parameters<AuthStatusParams>,
        context: rmcp::service::RequestContext<rmcp::RoleServer>,
    ) -> String {
        self.auth_status_as(RequestAuth::of(&context)).await
    }

    /// Context-free body of [`Self::auth_status`]: the same session every other tool call
    /// resolves, so `authenticated` is true exactly when a tool call would pass (#851 1).
    pub(crate) async fn auth_status_as(&self, request: RequestAuth) -> String {
        let caller = self.caller(request).await;
        let app_name = match &caller {
            Ok(session) => match &session.principal {
                Principal::App { token_id, .. } => apps_map::app_name_of(token_id),
                _ => None,
            },
            Err(_) => None,
        };
        auth_status_json(
            caller.as_deref(),
            app_name,
            crate::user_management::executor_is_unlocked(),
        )
    }
}

/// Build the `auth_status` answer from what decides it: the caller's session (or why its
/// token opens none), the app's name for an app token, and whether the executor's wallet
/// is unlocked.
///
/// A locked executor is the case worth care. Nothing authenticates until the operator
/// calls `unlockAgent`, so reporting a bare `authenticated: false` sends the agent to
/// `login_email` / `request_capability`, which cannot succeed — the observed failure
/// mode is a re-auth loop, or an agent that abandons the tools and hand-rolls HTTP.
/// `executor_locked` is therefore reported in every branch, and when it is set the
/// message names the operator action instead of an agent action.
fn auth_status_json(
    caller: Result<&Session, &AuthError>,
    app_name: Option<String>,
    unlocked: bool,
) -> String {
    const LOCKED_MESSAGE: &str = "Executor is locked: its admin has not unlocked the agent yet \
         (keys are held in memory only, so this happens after every restart). No login can \
         succeed until the executor operator calls unlockAgent. Ask them, then retry.";
    let message = |unlocked_message: String| {
        if unlocked {
            json!(unlocked_message)
        } else {
            json!(LOCKED_MESSAGE)
        }
    };

    match caller {
        // `authenticated` describes the session, which a lock does not end. `executor_locked`
        // describes the node. Both are reported rather than folded together, because the
        // caller's next action differs: wait for the operator, versus obtain a token.
        Ok(session) if session.is_authenticated() => {
            let principal = match session.principal {
                Principal::Operator => "operator",
                Principal::App { .. } => "app",
                Principal::User { .. } => "user",
                Principal::Anonymous { .. } => unreachable!("an authenticated session"),
            };
            json!({
                "authenticated": true,
                "executor_locked": !unlocked,
                "principal": principal,
                "app_name": app_name,
                "user_email": session.user().map(|(email, _)| email),
                "did": session.acts_as_did(),
                "capabilities": session.capabilities(),
                "message": if unlocked { serde_json::Value::Null } else { json!(LOCKED_MESSAGE) },
            })
            .to_string()
        }
        Ok(_) => json!({
            "authenticated": false,
            "executor_locked": !unlocked,
            "principal": "anonymous",
            "message": message(format!("Not authenticated. {}", super::AUTHENTICATE_HINT)),
        })
        .to_string(),
        Err(e) => json!({
            "authenticated": false,
            "executor_locked": !unlocked,
            "error": e.to_string(),
            "message": if *e == AuthError::Locked {
                json!(LOCKED_MESSAGE)
            } else {
                message(format!(
                    "Token set but not accepted: {e}. {}",
                    super::AUTHENTICATE_HINT
                ))
            },
        })
        .to_string(),
    }
}

#[cfg(test)]
mod auth_status_tests {
    use super::*;
    use crate::agent::capabilities::{ListenerReach, ALL_CAPABILITY};
    use crate::agent::session::TokenId;
    use serde_json::Value;

    fn parse(caller: Result<&Session, &AuthError>, unlocked: bool) -> Value {
        let json = auth_status_json(caller, Some("mcp-agent".to_string()), unlocked);
        serde_json::from_str(&json).expect("valid JSON")
    }

    fn session(principal: Principal) -> Session {
        Session::for_test(principal, ListenerReach::Network)
    }

    fn anonymous() -> Session {
        session(Principal::Anonymous { caps: vec![] })
    }

    fn user() -> Session {
        session(Principal::User {
            token_id: TokenId("t".into()),
            email: "agent@example.org".into(),
            did: "did:key:agent".into(),
            caps: vec![ALL_CAPABILITY.clone()],
        })
    }

    #[test]
    fn no_token_on_an_unlocked_executor_tells_the_agent_to_authenticate() {
        let v = parse(Ok(&anonymous()), true);
        assert_eq!(v["authenticated"], false);
        assert_eq!(v["executor_locked"], false);
        assert_eq!(v["principal"], "anonymous");
        assert!(v["message"].as_str().unwrap().contains("login_email"));
    }

    #[test]
    fn no_token_on_a_locked_executor_names_the_operator_action_instead() {
        let v = parse(Ok(&anonymous()), false);
        assert_eq!(v["executor_locked"], true);
        let msg = v["message"].as_str().unwrap();
        assert!(msg.contains("unlockAgent"), "message was: {msg}");
        // The regression this guards: advising an agent to log in when no login can work.
        assert!(!msg.contains("login_email"), "message was: {msg}");
    }

    #[test]
    fn a_token_on_a_locked_executor_reports_the_lock_not_the_token() {
        for e in [AuthError::Locked, AuthError::Invalid("bad".into())] {
            let v = parse(Err(&e), false);
            assert_eq!(v["authenticated"], false);
            assert_eq!(v["executor_locked"], true);
            assert!(v["message"].as_str().unwrap().contains("unlockAgent"));
        }
    }

    #[test]
    fn a_token_that_does_not_work_on_an_unlocked_executor_says_why() {
        for (e, why) in [
            (AuthError::Invalid("InvalidToken".into()), "InvalidToken"),
            (AuthError::Expired, "expired"),
            (AuthError::Revoked, "Unauthorized access"),
        ] {
            let v = parse(Err(&e), true);
            assert_eq!(v["authenticated"], false);
            assert_eq!(v["executor_locked"], false);
            let msg = v["message"].as_str().unwrap();
            assert!(msg.contains("not accepted") && msg.contains(why), "{msg}");
        }
    }

    #[test]
    fn a_working_session_on_an_unlocked_executor_is_the_ordinary_answer() {
        let v = parse(Ok(&user()), true);
        assert_eq!(v["authenticated"], true);
        assert_eq!(v["executor_locked"], false);
        assert_eq!(v["principal"], "user");
        assert_eq!(v["user_email"], "agent@example.org");
        assert_eq!(v["did"], "did:key:agent");
        assert_eq!(v["capabilities"].as_array().unwrap().len(), 1);
        assert!(v["message"].is_null());
    }

    #[test]
    fn a_working_session_on_a_locked_executor_stays_authenticated_and_says_why_nothing_works() {
        let v = parse(Ok(&user()), false);
        assert_eq!(
            v["authenticated"], true,
            "a lock does not invalidate a token"
        );
        assert_eq!(v["executor_locked"], true);
        assert!(v["message"].as_str().unwrap().contains("unlockAgent"));
    }
}

/// `auth_status` through the handler: it reports the session the tools use (#851).
#[cfg(test)]
mod auth_status_session_tests {
    use super::*;
    use crate::agent::capabilities::{ListenerReach, ALL_CAPABILITY};
    use crate::mcp::server::McpContext;
    use crate::test_utils::{ensure_db, setup_agent, setup_wallet};
    use serde_json::Value;

    const ADMIN: &str = "auth-status-admin";

    fn handler(reach: ListenerReach, admin: Option<&str>) -> Ad4mMcpHandler {
        Ad4mMcpHandler::new(McpContext::new(admin.map(Into::into), reach, false))
    }

    async fn status(handler: &Ad4mMcpHandler, header: Option<&str>) -> Value {
        let request = RequestAuth {
            header: header.map(str::to_string),
            reach: None,
        };
        serde_json::from_str(&handler.auth_status_as(request).await).unwrap()
    }

    // #851 1 + 2: the admin credential authenticates every tool, but auth_status decoded
    // it as a JWT and answered "could not decode".
    #[tokio::test]
    async fn the_admin_credential_reports_the_operator() {
        setup_wallet();
        setup_agent();
        let handler = handler(ListenerReach::Network, Some(ADMIN));
        let v = status(&handler, Some(ADMIN)).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "operator");
        assert_eq!(v["did"], crate::agent::did());
        // The header joined the MCP session: a later call without it still reports it.
        assert_eq!(status(&handler, None).await["principal"], "operator");
    }

    // #851 1: no token on the loopback listener of a node without a credential passes every
    // tool as the operator, but auth_status answered "Not authenticated".
    #[tokio::test]
    async fn no_token_on_a_local_node_without_a_credential_reports_the_operator() {
        ensure_db();
        let v = status(&handler(ListenerReach::Loopback, None), None).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "operator");
    }

    #[tokio::test]
    async fn an_app_token_reports_the_app_and_its_capabilities() {
        setup_wallet();
        setup_agent();
        crate::test_utils::use_test_apps_file();
        let token = crate::agent::capabilities::token::generate_jwt(
            "auth-status-app".to_string(),
            3600,
            crate::agent::capabilities::AuthInfo {
                capabilities: Some(vec![ALL_CAPABILITY.clone()]),
                ..Default::default()
            },
        )
        .unwrap();
        let request_id = format!("auth-status-{}", uuid::Uuid::new_v4());
        let app = crate::agent::capabilities::AuthInfoExtended {
            request_id: request_id.clone(),
            auth: crate::agent::capabilities::AuthInfo {
                app_name: "auth-status-app".to_string(),
                ..Default::default()
            },
        };
        crate::test_utils::insert_app_with_token(request_id.clone(), app, &token);

        let handler = handler(ListenerReach::Network, Some(ADMIN));
        let v = status(&handler, Some(&token)).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "app");
        assert_eq!(v["app_name"], "auth-status-app");
        assert_eq!(v["capabilities"].as_array().unwrap().len(), 1);

        // #851 1, the other direction: a revoked token decoded fine, so auth_status said
        // `authenticated: true` while every tool refused it.
        crate::agent::capabilities::apps_map::revoke_app(&request_id).unwrap();
        let v = status(&handler, None).await;
        assert_eq!(v["authenticated"], false, "{v}");
        assert!(v["error"].as_str().unwrap().contains("Unauthorized access"));
        crate::agent::capabilities::apps_map::remove_app(&request_id).unwrap();
    }

    #[tokio::test]
    async fn a_user_token_reports_the_user() {
        setup_wallet();
        setup_agent();
        let email = "auth.status.user@example.org";
        crate::agent::AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let v = status(&handler(ListenerReach::Network, Some(ADMIN)), Some(&token)).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "user");
        assert_eq!(v["user_email"], email);
        assert_eq!(
            v["did"],
            crate::agent::AgentService::get_user_did_by_email(email).unwrap()
        );
    }

    #[tokio::test]
    async fn an_expired_token_reports_why() {
        setup_wallet();
        setup_agent();
        let token = crate::test_utils::expired_user_token("auth.status.expired@example.org");
        let v = status(&handler(ListenerReach::Network, Some(ADMIN)), Some(&token)).await;
        assert_eq!(v["authenticated"], false);
        assert!(v["error"].as_str().unwrap().contains("expired"), "{v}");
    }

    #[tokio::test]
    async fn no_token_on_a_node_with_a_credential_is_anonymous() {
        ensure_db();
        let v = status(&handler(ListenerReach::Loopback, Some(ADMIN)), None).await;
        assert_eq!(v["authenticated"], false);
        assert_eq!(v["principal"], "anonymous");
    }
}

#[cfg(test)]
mod capability_mint_tests {
    use super::*;
    use crate::agent::capabilities::ListenerReach;
    use crate::mcp::server::McpContext;

    fn handler_with_admin_credential() -> Ad4mMcpHandler {
        Ad4mMcpHandler::new(McpContext::new(
            Some("test-admin-credential".into()),
            ListenerReach::Network,
            false,
        ))
    }

    fn mint_params() -> RequestCapabilityParams {
        RequestCapabilityParams {
            app_name: "some-network-caller".to_string(),
            app_desc: "a caller that has not authenticated".to_string(),
            app_domain: None,
            app_url: None,
        }
    }

    /// The vulnerability of issue #851 sub-problem 3: anyone who could reach the
    /// MCP port received an auto-permitted ALL_CAPABILITY mint code inline,
    /// bypassing the admin credential entirely. The code IS the mint secret —
    /// with it, `generate_jwt` hands out an all-capability JWT.
    ///
    /// This test failed on dev (b9da4e65f) before the fix: the response carried
    /// `"code"` and "auto-permitted".
    #[tokio::test]
    async fn an_unauthenticated_caller_gets_no_mint_code() {
        let handler = handler_with_admin_credential();
        let resp = handler
            .handle_capability_request(mint_params(), false)
            .await;
        let v: serde_json::Value = serde_json::from_str(&resp).expect("valid JSON");
        assert!(
            v.get("code").is_none(),
            "unauthenticated caller received a capability mint code: {resp}"
        );
        // The pairing flow must survive: the request is still created, so the
        // Launcher can show it and the admin can approve it out-of-band.
        assert!(v["request_id"].as_str().is_some(), "response was: {resp}");
        assert!(
            v["message"].as_str().unwrap_or_default().contains("admin"),
            "message should point at admin approval: {resp}"
        );
    }

    #[tokio::test]
    async fn an_authenticated_caller_still_gets_a_permitted_code() {
        use crate::agent::capabilities::pending::PendingApprovals;

        let handler = handler_with_admin_credential();
        let resp = handler.handle_capability_request(mint_params(), true).await;
        let v: serde_json::Value = serde_json::from_str(&resp).expect("valid JSON");
        let request_id = v["request_id"].as_str().expect("request_id present");
        let code = v["code"]
            .as_str()
            .expect("authenticated caller should receive the code inline");
        // Not just an inline code: the permit must actually be registered,
        // i.e. generate_jwt with this pair would succeed.
        assert!(
            PendingApprovals::global().has_code(request_id, code),
            "permit was not registered for the returned request_id/code"
        );
    }

    /// The no-credential convenience must survive the gate: on an executor
    /// started WITHOUT --admin-credential, a bare caller (no session token, no
    /// Authorization header) on a loopback listener is the operator, so it still
    /// gets the auto-permit and the inline code. On a network listener (an
    /// explicit MCP_HOST) the same caller is anonymous and does not (#1059).
    /// Runs the tool's real path, `request_capability_as`, with no header.
    #[tokio::test]
    async fn without_admin_credential_a_local_caller_is_still_auto_permitted() {
        crate::test_utils::ensure_db();
        let gets_code = |reach| async move {
            let handler = Ad4mMcpHandler::new(McpContext::new(None, reach, false));
            let resp = handler
                .request_capability_as(RequestAuth::default(), mint_params())
                .await;
            let v: serde_json::Value = serde_json::from_str(&resp).expect("valid JSON");
            v["code"].as_str().is_some()
        };
        assert!(
            gets_code(ListenerReach::Loopback).await,
            "no-credential flow on loopback should still return the code inline"
        );
        assert!(
            !gets_code(ListenerReach::Network).await,
            "#1059: no operator over the network"
        );
    }

    /// The escalation of issue #851 sub-problem 3, through the capability layer: a
    /// multi-user user token authenticates but holds no PERMIT capability, so it must
    /// not auto-permit an ALL_CAPABILITY mint. A token that does hold PERMIT still may.
    #[tokio::test]
    async fn a_token_without_permit_may_not_auto_permit() {
        use crate::agent::capabilities::{
            get_user_default_capabilities, token::generate_jwt, types::AuthInfo, Capability,
            ALL_CAPABILITY, DEFAULT_TOKEN_VALID_PERIOD,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();

        let jwt_with = |caps: Vec<Capability>| {
            generate_jwt(
                "test-app".to_string(),
                DEFAULT_TOKEN_VALID_PERIOD,
                AuthInfo {
                    app_name: "test-app".to_string(),
                    app_desc: String::new(),
                    app_domain: None,
                    app_url: None,
                    app_icon_path: None,
                    capabilities: Some(caps),
                    user_email: None,
                },
            )
            .expect("the wallet is unlocked in tests")
        };

        // Through the tool's own path: the header credential joins a fresh session, then
        // the permit check decides. Returns whether the reply carries a mint code.
        let gets_code = |header: String| async move {
            let resp = handler_with_admin_credential()
                .request_capability_as(
                    RequestAuth {
                        header: Some(header),
                        reach: None,
                    },
                    mint_params(),
                )
                .await;
            let v: serde_json::Value = serde_json::from_str(&resp).unwrap();
            v.get("code").is_some()
        };

        // A user-capability token authenticates but cannot auto-permit.
        assert!(
            !gets_code(jwt_with(get_user_default_capabilities())).await,
            "a user token without PERMIT received a mint code"
        );
        // A token that holds PERMIT (here, ALL_CAPABILITY) still auto-permits.
        assert!(gets_code(jwt_with(vec![ALL_CAPABILITY.clone()])).await);
        // The admin credential auto-permits.
        assert!(gets_code("test-admin-credential".to_string()).await);
    }
}
