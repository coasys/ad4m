//! Authentication tools
//!
//! Tools for JWT auth, capability tokens, multi-user signup/login. Each calls the RPC method
//! that does its job through the handler map: `user.login`, `user.create`,
//! `user.requestVerification`, `user.verifyEmail`, `agent.requestCapability`,
//! `agent.generateJwt` as a caller without a token, and `agent.permitCapability` as the
//! caller. `auth_status` has no RPC method: it reports the MCP session's [`Session`].

use super::{call_method_as, Ad4mMcpHandler, RequestAuth};
use crate::agent::capabilities::{defs::ALL_CAPABILITY, AuthInfo, AuthInfoExtended};
use crate::agent::session::{AuthError, Principal, Session};
use crate::api::ws_handler::WsRpcError;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;

/// The app name of the tokens the login tools obtain.
const MCP_APP_NAME: &str = "mcp-agent";

/// A login tool's answer when its user-management method refuses or fails. Callers without a
/// token hold the user-management capabilities only while multi-user mode is on, so the
/// guard's refusal names the mode, as the handlers do.
fn user_management_error(e: WsRpcError) -> String {
    let error = if e.code == 403 && !crate::user_management::is_multi_user_enabled() {
        "Multi-user mode is not enabled".to_string()
    } else {
        e.message
    };
    json!({ "success": false, "error": error }).to_string()
}

/// The answer to a method result outside the method's contract.
fn unexpected_result(method: &str, result: &Value) -> String {
    json!({ "success": false, "error": format!("{method} answered {result}") }).to_string()
}

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
    /// Optional agent to act as on a multi-user executor: a user's email or an agent's DID.
    /// Absent: the executor's main agent. That agent's owner approves the request.
    #[serde(default)]
    pub agent: Option<String>,
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
        let email = params.0.email.trim().to_lowercase();
        let login = json!({
            "email": email,
            "password": params.0.password,
            "appName": MCP_APP_NAME,
        });
        match self.call_without_token("user.login", login).await {
            Ok(Value::String(token)) => {
                self.store_token_and_respond(token, Some(&email), "Login successful.")
                    .await
            }
            Ok(other) => unexpected_result("user.login", &other),
            Err(e) => user_management_error(e),
        }
    }

    /// Request a capability token (local connect flow - step 1)
    #[tool(
        description = "Request a capability token (step 1/2 of local auth flow). This is the primary way to authenticate with a local/single-user AD4M executor. Returns request_id and code — pass both to generate_jwt to get a JWT token. Optional `agent` (a user's email or an agent's DID) asks for a token acting as that agent; absent, the main agent. On an executor started with --admin-credential, the request is only auto-permitted (code returned inline) when the caller may approve it: the admin credential, or a token that holds the PERMIT capability and acts as the requested agent (a user token for that user's own agent). Any other caller gets only the request_id; the agent's owner approves the request and provides the code out-of-band. For multi-user executors, use login_email or signup instead. Note: when using the ad4m-executor CLI, the verification code is logged to stdout."
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
    /// auto-permitting it (the inline code that generate_jwt mints into a token) needs a
    /// session that may approve the request (`agent.permitCapability`'s guard): the
    /// operator, or a session that acts as the requested agent and holds PERMIT, as a
    /// user's own token does for that user's agent.
    pub(crate) async fn request_capability_as(
        &self,
        request: RequestAuth,
        params: RequestCapabilityParams,
    ) -> String {
        let permitter = self.caller(request).await.ok();
        self.handle_capability_request(params, permitter).await
    }

    /// Body of [`Self::request_capability`], split from the transport wrapper so
    /// tests can drive it as any session: a `RequestContext<RoleServer>`
    /// cannot be fabricated outside rmcp (its `Peer` constructor is `pub(crate)`,
    /// see `harness_bridge.rs`), so the wrapper only adopts the header credential
    /// and resolves the caller; everything decidable lives here.
    ///
    /// The request itself is always created (`agent.requestCapability`, as a caller without
    /// a token, for the agent `p.agent` names: the main agent when absent) — that publishes
    /// the capability-request exception the agent's owner listens for, which is the
    /// legitimate pairing flow. The auto-permit and the inline code are
    /// `agent.permitCapability` as `permitter`, so its guard (`Owns { Request }`, D12)
    /// decides: the operator, or a session that holds PERMIT and acts as the requested agent
    /// (a user for their own agent); never an anonymous caller, and never a caller whose
    /// token does not work (`None`). The code is the mint secret (`generate_jwt` exchanges it
    /// for a signed token on that agent), and returning it to any caller that could reach the
    /// MCP port made `--admin-credential` decorative (issue #851, sub-problem 3). The request
    /// asks for ALL_CAPABILITY; the method's handler keeps only what the permitter's session
    /// holds (`permit_capability_within`), and a grant on a user's agent holds no more than
    /// that user does (`grant_caps_for`).
    ///
    /// Scope: without an admin credential, a caller with no token on a loopback
    /// listener is the operator, so it still gets the inline code. On a multi-user
    /// executor that includes a user who drops their token or opens a new session
    /// (#1059). Such a server binds loopback only (#1033); on a listener beyond
    /// loopback (an explicit `MCP_HOST`) that caller is anonymous and gets no code.
    pub(crate) async fn handle_capability_request(
        &self,
        p: RequestCapabilityParams,
        permitter: Option<Arc<Session>>,
    ) -> String {
        let app_domain = p.app_domain.clone().unwrap_or_default();
        let auth_info = AuthInfo {
            app_name: p.app_name.clone(),
            app_desc: p.app_desc.clone(),
            app_domain: Some(app_domain.clone()),
            app_url: p.app_url.clone(),
            app_icon_path: None,
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            user_email: None,
        };
        let mut request = json!({ "authInfo": {
            "appName": p.app_name,
            "appDesc": p.app_desc,
            "appDomain": app_domain,
            "appUrl": p.app_url,
            "capabilities": [&*ALL_CAPABILITY],
        }});
        if let Some(agent) = &p.agent {
            request["agent"] = json!(agent);
        }
        let request_id = match self
            .call_without_token("agent.requestCapability", request)
            .await
        {
            Ok(Value::String(request_id)) => request_id,
            Ok(other) => return unexpected_result("agent.requestCapability", &other),
            Err(e) => {
                return json!({
                    "success": false,
                    "error": format!("Failed to request capability: {}", e.message)
                })
                .to_string()
            }
        };

        let not_permitted = || {
            json!({
                "request_id": request_id,
                "message": "Capability request created but NOT auto-permitted: this session may \
                    not grant capabilities on the requested agent. Its owner (the executor admin \
                    for the main agent, the user for theirs) must approve the request, then call \
                    generate_jwt with this request_id and the code shown to them. Only the admin \
                    credential, or a token that holds the PERMIT capability and acts as the \
                    requested agent, auto-permits."
            })
            .to_string()
        };
        let Some(permitter) = permitter else {
            return not_permitted();
        };
        let permit = AuthInfoExtended {
            request_id: request_id.clone(),
            auth: auth_info,
        };
        let permit = json!({ "auth": serde_json::to_string(&permit).unwrap_or_default() });
        // As the permitter, through the method's guard and handler: the handler approves
        // within the permitter's own capabilities (`permit_capability_within`).
        match call_method_as(permitter, "agent.permitCapability", permit).await {
            // The guard's refusal: no PERMIT (403), the request is for an agent the caller
            // does not act as (404, D6), or none of the requested capabilities is the
            // caller's own (403).
            Err(e) if matches!(e.code, 401 | 403 | 404) => not_permitted(),
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to permit capability: {}", e.message)
            })
            .to_string(),
            Ok(Value::String(code)) => {
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
            Ok(other) => unexpected_result("agent.permitCapability", &other),
        }
    }

    /// Generate a JWT from a capability request (local connect flow - step 2)
    #[tool(
        description = "Generate a JWT token (step 2/2 of local auth flow). Pass the request_id and code from request_capability. The JWT is stored in the session and used for all subsequent operations automatically."
    )]
    pub async fn generate_jwt(&self, params: Parameters<GenerateJwtParams>) -> String {
        let p = &params.0;

        let generate = json!({ "requestId": p.request_id, "rand": p.code });
        match self.call_without_token("agent.generateJwt", generate).await {
            Ok(Value::String(token)) => {
                self.store_token_and_respond(
                    token,
                    None,
                    "JWT generated and stored. You are now authenticated.",
                )
                .await
            }
            Ok(other) => unexpected_result("agent.generateJwt", &other),
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to generate JWT: {}", e.message)
            })
            .to_string(),
        }
    }

    /// Sign up a new user (multi-user mode)
    #[tool(
        description = "Create a new user account (multi-user mode). Sends a verification email with a code. Use verify_email_code to complete signup."
    )]
    pub async fn signup(&self, params: Parameters<SignupParams>) -> String {
        let create = json!({
            "email": params.0.email.trim().to_lowercase(),
            "password": params.0.password,
        });
        match self.call_without_token("user.create", create).await {
            Ok(created) if created["success"] == true => json!({
                "success": true,
                "did": created["did"],
                "message": "User created. Check your email for a verification code and call verify_email_code."
            }).to_string(),
            Ok(created) => json!({"success": false, "error": created["error"]}).to_string(),
            Err(e) => user_management_error(e),
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
        let request = json!({
            "email": params.0.email.trim().to_lowercase(),
            "appInfo": { "appName": "MCP Agent" },
        });
        match self
            .call_without_token("user.requestVerification", request)
            .await
        {
            Ok(answer) => {
                let mut result = json!({
                    "success": answer["success"],
                    "message": answer["message"],
                    "requires_password": answer["requiresPassword"],
                    "is_existing_user": answer["isExistingUser"],
                });
                if answer["success"] != true {
                    result["error"] = answer["message"].clone();
                }
                result.to_string()
            }
            Err(e) => user_management_error(e),
        }
    }

    /// Verify an email code for signup or login (multi-user mode)
    #[tool(
        description = "Verify an email code to complete signup or login. Returns a JWT token on success. The verification_type must be 'signup' or 'login'."
    )]
    pub async fn verify_email_code(&self, params: Parameters<VerifyEmailCodeParams>) -> String {
        let p = &params.0;
        let email = p.email.trim().to_lowercase();
        let verify = json!({
            "email": email,
            "code": p.code,
            "verificationType": p.verification_type,
            "appName": MCP_APP_NAME,
        });
        match self.call_without_token("user.verifyEmail", verify).await {
            Ok(Value::String(token)) => {
                self.store_token_and_respond(
                    token,
                    Some(&email),
                    "Email verified. Token stored for subsequent operations.",
                )
                .await
            }
            Ok(other) => unexpected_result("user.verifyEmail", &other),
            Err(e) => user_management_error(e),
        }
    }

    /// Call the RPC method `method` as a caller without a token on this MCP listener: the
    /// operator on a loopback listener of an executor without an admin credential, anonymous
    /// otherwise. The login and capability-request tools call this way, whatever session
    /// the MCP session holds: they obtain the session that replaces it, as an RPC client
    /// does on a connection without a token.
    async fn call_without_token(&self, method: &str, params: Value) -> Result<Value, WsRpcError> {
        let session = self
            .tokenless_session()
            .map_err(|e| WsRpcError::unauthorized(e.to_string()))?;
        call_method_as(Arc::new(session), method, params).await
    }

    /// The session of a caller without a token on this request: `context.reach` is the
    /// request's own reach (`Ad4mMcpHandler::for_request`), so a proxied request is never the
    /// operator.
    pub(crate) fn tokenless_session(&self) -> Result<Session, AuthError> {
        Session::open("", &self.context.admin_credential, self.context.reach)
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
        // The app is the grant's: a token carries no app name.
        let app_name = match &caller {
            Ok(session) => session.grant().map(|grant| grant.app_name().to_string()),
            Err(_) => None,
        };
        auth_status_json(
            caller.as_deref(),
            app_name,
            crate::user_management::executor_is_unlocked(),
        )
    }
}

/// What `auth_status` says of a token that decodes but opens no session because no active
/// grant stands behind it.
const NO_ACTIVE_GRANT: &str =
    "no active grant: the token was revoked, its app removed, or it predates grants";

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
        // describes the executor. Both are reported rather than folded together, because the
        // caller's next action differs: wait for the operator, versus obtain a token.
        Ok(session) if session.is_authenticated() => {
            // The wire keeps its two names for a grant: "app" on the main agent, "user" on
            // a managed agent.
            let principal = match &session.principal {
                Principal::Operator => "operator",
                Principal::Agent(agent) if agent.is_main_agent() => "app",
                Principal::Agent(_) => "user",
                Principal::Anonymous => unreachable!("an authenticated session"),
            };
            json!({
                "authenticated": true,
                "executor_locked": !unlocked,
                "principal": principal,
                "app_name": app_name,
                // The login name of the managed agent's account, for clients that read it;
                // the agent itself is `did` (D16).
                "user_email": session
                    .managed_did()
                    .and_then(|did| crate::agent::AgentService::email_for_did(did).ok()),
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
        Err(e) => {
            // A token that decodes but has no active grant (revoked, removed, or from before
            // grants) says so, rather than the RPC refusal's "Unauthorized access".
            let why = match e {
                AuthError::Revoked => NO_ACTIVE_GRANT.to_string(),
                e => e.to_string(),
            };
            json!({
                "authenticated": false,
                "executor_locked": !unlocked,
                "error": why,
                "message": if *e == AuthError::Locked {
                    json!(LOCKED_MESSAGE)
                } else {
                    message(format!(
                        "Token set but not accepted: {why}. {}",
                        super::AUTHENTICATE_HINT
                    ))
                },
            })
            .to_string()
        }
    }
}

#[cfg(test)]
mod auth_status_tests {
    use super::*;
    use crate::agent::capabilities::ALL_CAPABILITY;
    use crate::agent::AgentContext;
    use serde_json::Value;

    fn parse(caller: Result<&Session, &AuthError>, unlocked: bool) -> Value {
        let json = auth_status_json(caller, Some("mcp-agent".to_string()), unlocked);
        serde_json::from_str(&json).expect("valid JSON")
    }

    fn anonymous() -> Session {
        Session::for_test(Principal::Anonymous)
    }

    /// A grant on a managed agent whose account is `agent@example.org`: the report reads the
    /// email from the account (D16).
    fn user() -> Session {
        crate::test_utils::ensure_test_db();
        crate::db::Ad4mDb::with_global_instance(|db| {
            db.add_user_prehashed("agent@example.org", "did:key:agent", "")
        })
        .expect("the agent's account");
        Session::for_test(Principal::Agent(AgentContext::managed(
            "did:key:agent".into(),
        )))
        .with_caps(vec![ALL_CAPABILITY.clone()])
        .with_did("did:key:agent")
    }

    // The wire names a grant on the main agent "app" and one on a managed agent "user".
    #[test]
    fn a_grant_on_the_main_agent_reports_an_app() {
        let app = Session::for_test(Principal::Agent(AgentContext::main_agent()))
            .with_did("did:key:main");
        let v = parse(Ok(&app), true);
        assert_eq!(v["principal"], "app");
        assert_eq!(v["user_email"], Value::Null);
        assert_eq!(parse(Ok(&user()), true)["principal"], "user");
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
            (AuthError::Revoked, "no active grant"),
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
    use crate::test_utils::{ensure_test_db, setup_agent, setup_wallet};
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

    // #851 1: no token on the loopback listener of an executor without a credential passes every
    // tool as the operator, but auth_status answered "Not authenticated".
    #[tokio::test]
    async fn no_token_on_a_local_executor_without_a_credential_reports_the_operator() {
        ensure_test_db();
        let v = status(&handler(ListenerReach::Loopback, None), None).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "operator");
    }

    #[tokio::test]
    async fn an_app_token_reports_the_app_and_its_capabilities() {
        setup_wallet();
        setup_agent();
        let token = crate::test_utils::granted_token(crate::agent::capabilities::AuthInfo {
            app_name: "auth-status-app".to_string(),
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            ..Default::default()
        });

        let handler = handler(ListenerReach::Network, Some(ADMIN));
        let v = status(&handler, Some(&token)).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "app");
        assert_eq!(v["app_name"], "auth-status-app");
        assert_eq!(v["capabilities"].as_array().unwrap().len(), 1);

        // #851 1, the other direction: a revoked token decoded fine, so auth_status said
        // `authenticated: true` while every tool refused it. It also called a token without
        // a grant "undecodable"; it names the missing grant.
        let grants = crate::agent::grants::Grants::global();
        grants.revoke(&grants.find(&token)).unwrap();
        let v = status(&handler, None).await;
        assert_eq!(v["authenticated"], false, "{v}");
        assert!(
            v["error"].as_str().unwrap().contains("no active grant"),
            "{v}"
        );
        assert!(
            v["message"].as_str().unwrap().contains("no active grant"),
            "{v}"
        );
        grants.remove(&grants.find(&token)).unwrap();
    }

    #[tokio::test]
    async fn a_user_token_reports_the_user() {
        setup_wallet();
        setup_agent();
        let email = "auth.status.user@example.org";
        let did = crate::test_utils::create_test_user(email);
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let v = status(&handler(ListenerReach::Network, Some(ADMIN)), Some(&token)).await;
        assert_eq!(v["authenticated"], true, "{v}");
        assert_eq!(v["principal"], "user");
        assert_eq!(v["user_email"], email);
        assert_eq!(v["did"], did);
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
    async fn no_token_on_an_executor_with_a_credential_is_anonymous() {
        ensure_test_db();
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
            agent: None,
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
        crate::test_utils::ensure_test_db();
        let handler = handler_with_admin_credential();
        let anonymous = Session::for_test(Principal::Anonymous).with_caps(
            crate::agent::capabilities::anonymous_capabilities_when(false),
        );
        let resp = handler
            .handle_capability_request(mint_params(), Some(Arc::new(anonymous)))
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
        use crate::agent::capabilities::pending::PendingRequests;

        crate::test_utils::ensure_test_db();
        let handler = handler_with_admin_credential();
        let operator = Session::for_test(Principal::Operator);
        let resp = handler
            .handle_capability_request(mint_params(), Some(Arc::new(operator)))
            .await;
        let v: serde_json::Value = serde_json::from_str(&resp).expect("valid JSON");
        let request_id = v["request_id"].as_str().expect("request_id present");
        let code = v["code"]
            .as_str()
            .expect("authenticated caller should receive the code inline");
        // Not just an inline code: the permit must actually be registered,
        // i.e. generate_jwt with this pair would succeed.
        assert!(
            PendingRequests::global().has_code(request_id, code),
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
        crate::test_utils::ensure_test_db();
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

    /// The escalation of issue #851 sub-problem 3, through the capability layer: a token
    /// must not auto-permit a request it may not approve (D12). A token without PERMIT never
    /// does; a token with it only for the agent it acts as. Users hold PERMIT on their own
    /// agent (#1399), so a user token auto-permits requests for that user and no other.
    #[tokio::test]
    async fn only_a_caller_that_may_approve_the_request_auto_permits_it() {
        use crate::agent::capabilities::{
            types::AuthInfo, Capability, AGENT_READ_CAPABILITY, ALL_CAPABILITY,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();

        let jwt_with = |caps: Vec<Capability>| {
            crate::test_utils::granted_token(AuthInfo {
                app_name: "test-app".to_string(),
                app_desc: String::new(),
                app_domain: None,
                app_url: None,
                app_icon_path: None,
                capabilities: Some(caps),
                user_email: None,
            })
        };
        let alice = crate::test_utils::managed_user("mcp-alice");
        let alice_token = crate::user_management::generate_user_jwt(&alice, "mcp").unwrap();
        let bob = crate::test_utils::managed_user("mcp-bob");

        // Through the tool's own path: the header credential joins a fresh session, then
        // the permit check decides. Returns whether the reply carries a mint code.
        let gets_code = |header: String, agent: Option<String>| async move {
            let resp = handler_with_admin_credential()
                .request_capability_as(
                    RequestAuth {
                        header: Some(header),
                        reach: None,
                    },
                    RequestCapabilityParams {
                        agent,
                        ..mint_params()
                    },
                )
                .await;
            let v: serde_json::Value = serde_json::from_str(&resp).unwrap();
            v.get("code").is_some()
        };

        // A token without PERMIT authenticates but cannot auto-permit.
        assert!(
            !gets_code(jwt_with(vec![AGENT_READ_CAPABILITY.clone()]), None).await,
            "a token without PERMIT received a mint code"
        );
        // A user may approve for their own agent only.
        assert!(
            !gets_code(alice_token.clone(), None).await,
            "a user token received a mint code for the main agent"
        );
        assert!(
            !gets_code(alice_token.clone(), Some(bob.clone())).await,
            "a user token received a mint code for another user"
        );
        assert!(gets_code(alice_token, Some(alice.clone())).await);
        // A main-agent token that holds PERMIT (here, ALL_CAPABILITY) auto-permits for the
        // main agent, never for a user's.
        assert!(gets_code(jwt_with(vec![ALL_CAPABILITY.clone()]), None).await);
        assert!(!gets_code(jwt_with(vec![ALL_CAPABILITY.clone()]), Some(alice.clone())).await);
        // The admin credential auto-permits for any agent.
        assert!(gets_code("test-admin-credential".to_string(), None).await);
        assert!(gets_code("test-admin-credential".to_string(), Some(alice)).await);
    }

    /// `agent.permitCapability` is `Owns { Request }`: a user session permits only requests
    /// for its own agent, whatever its capabilities. The tool checked only PERMIT, so a user
    /// token holding it got the code for the main agent.
    #[tokio::test]
    async fn a_user_session_holding_permit_gets_no_mint_code_for_the_main_agent() {
        crate::test_utils::ensure_test_db();
        let user = Session::for_test(Principal::Agent(crate::agent::AgentContext::managed(
            "did:key:mint-user".into(),
        )))
        .with_caps(vec![ALL_CAPABILITY.clone()])
        .with_did("did:key:mint-user");
        let resp = handler_with_admin_credential()
            .handle_capability_request(mint_params(), Some(Arc::new(user)))
            .await;
        let v: serde_json::Value = serde_json::from_str(&resp).expect("valid JSON");
        assert!(v.get("code").is_none(), "{resp}");
        assert!(v["request_id"].is_string(), "{resp}");
    }
}

/// The login tools call their user-management methods as a caller without a token.
#[cfg(test)]
mod login_tests {
    use super::*;
    use crate::agent::capabilities::ListenerReach;
    use crate::mcp::server::McpContext;
    use crate::test_utils::{ensure_test_db, setup_agent, setup_wallet, MultiUserMode};

    fn handler() -> Ad4mMcpHandler {
        Ad4mMcpHandler::new(McpContext::new(
            Some("login-admin".into()),
            ListenerReach::Network,
            false,
        ))
    }

    fn parse(answer: &str) -> Value {
        serde_json::from_str(answer).expect(answer)
    }

    // The guard refuses a caller without a token while multi-user mode is off; the answer
    // still names the mode (`mcp-auth.test.ts` reads it).
    #[tokio::test]
    async fn logging_in_without_multi_user_mode_names_the_mode() {
        ensure_test_db();
        let answer = parse(
            &handler()
                .login_email(Parameters(LoginEmailParams {
                    email: "nobody@example.org".into(),
                    password: "p".into(),
                }))
                .await,
        );
        assert_eq!(answer["success"], false, "{answer}");
        assert_eq!(answer["error"], "Multi-user mode is not enabled");
        let signup = parse(
            &handler()
                .signup(Parameters(SignupParams {
                    email: "nobody@example.org".into(),
                    password: "p".into(),
                }))
                .await,
        );
        assert_eq!(
            signup["error"], "Multi-user mode is not enabled",
            "{signup}"
        );
    }

    // An MCP session that holds a user session logs in as another user: the login runs as a
    // caller without a token, not as the session it replaces.
    #[tokio::test]
    async fn a_logged_in_session_logs_in_again() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let email = &format!("mcp.login-{}@example.org", uuid::Uuid::new_v4().simple());
        crate::user_management::create_user(email, "secret-pass").unwrap();
        let first = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let handler = Ad4mMcpHandler::new(
            McpContext::new(Some("login-admin".into()), ListenerReach::Network, false)
                .with_token(&first),
        );
        let answer = parse(
            &handler
                .login_email(Parameters(LoginEmailParams {
                    email: email.into(),
                    password: "secret-pass".into(),
                }))
                .await,
        );
        assert_eq!(answer["success"], true, "{answer}");
        let wrong = parse(
            &handler
                .login_email(Parameters(LoginEmailParams {
                    email: email.into(),
                    password: "wrong".into(),
                }))
                .await,
        );
        assert_eq!(wrong["success"], false, "{wrong}");
    }

    // `user.requestVerification` tells a caller without an account to sign up with a
    // password; the tool answered "User not found".
    #[tokio::test]
    async fn requesting_a_code_without_an_account_asks_for_a_password() {
        setup_wallet();
        let _multi_user = MultiUserMode::on();
        let answer = parse(
            &handler()
                .request_login_verification(Parameters(RequestLoginVerificationParams {
                    email: "no.account@example.org".into(),
                }))
                .await,
        );
        assert_eq!(answer["success"], true, "{answer}");
        assert_eq!(answer["requires_password"], true, "{answer}");
        assert_eq!(answer["is_existing_user"], false, "{answer}");
    }
}
