//! Authentication tools
//!
//! Tools for JWT auth, capability tokens, multi-user signup/login.

use super::Ad4mMcpHandler;
use crate::agent::capabilities::{
    defs::ALL_CAPABILITY, generate_capability_token, permit_capability,
    request_capability as cap_request_capability, token::decode_jwt, AuthInfo, AuthInfoExtended,
};
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
        description = "Request a capability token (step 1/2 of local auth flow). This is the primary way to authenticate with a local/single-user AD4M executor. Returns request_id and code — pass both to generate_jwt to get a JWT token. For multi-user executors, use login_email or signup instead. Note: when using the ad4m-executor CLI, the verification code is logged to stdout."
    )]
    pub async fn request_capability(&self, params: Parameters<RequestCapabilityParams>) -> String {
        let p = &params.0;

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
                    cap_token,
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
    #[tool(description = "Check the current authentication status of the MCP session.")]
    pub async fn auth_status(&self, _params: Parameters<AuthStatusParams>) -> String {
        let token = self.context.auth_token.read().await;
        let session = match token.as_deref() {
            Some(t) if !t.is_empty() => match decode_jwt(t.to_string()) {
                Ok(claims) => SessionToken::Valid {
                    app_name: claims.capabilities.app_name,
                    user_email: claims.capabilities.user_email,
                    has_capabilities: claims.capabilities.capabilities.is_some(),
                },
                Err(_) => SessionToken::Undecodable,
            },
            _ => SessionToken::Absent,
        };
        auth_status_json(session, crate::user_management::executor_is_unlocked())
    }
}

/// What the session's stored token turned out to be.
///
/// Decoding happens in the tool method, not in `auth_status_json`, because `decode_jwt`
/// reaches into the wallet — the very subsystem whose lock state is under test. Keeping
/// the decode outside is what lets the tests below drive the real answer function.
#[derive(Debug, PartialEq, Eq)]
enum SessionToken {
    Absent,
    Undecodable,
    Valid {
        app_name: String,
        user_email: Option<String>,
        has_capabilities: bool,
    },
}

/// Build the `auth_status` answer from the two things that decide it: what the session's
/// token is, and whether the executor's wallet is unlocked.
///
/// A locked executor is the case worth care. Nothing authenticates until the operator
/// calls `unlockAgent`, so reporting a bare `authenticated: false` sends the agent to
/// `login_email` / `request_capability`, which cannot succeed — the observed failure
/// mode is a re-auth loop, or an agent that abandons the tools and hand-rolls HTTP.
/// `executor_locked` is therefore reported in every branch, and when it is set the
/// message names the operator action instead of an agent action.
fn auth_status_json(session: SessionToken, unlocked: bool) -> String {
    const LOCKED_MESSAGE: &str = "Executor is locked: its admin has not unlocked the agent yet \
         (keys are held in memory only, so this happens after every restart). No login can \
         succeed until the executor operator calls unlockAgent. Ask them, then retry.";

    match session {
        // `authenticated` describes the session's credential, which a lock does not
        // invalidate. `executor_locked` describes the node. Both are reported rather
        // than folded together, because the caller's next action differs: wait for
        // the operator, versus obtain a token.
        SessionToken::Valid {
            app_name,
            user_email,
            has_capabilities,
        } => json!({
            "authenticated": true,
            "executor_locked": !unlocked,
            "app_name": app_name,
            "user_email": user_email,
            "has_capabilities": has_capabilities,
            "message": if unlocked { serde_json::Value::Null } else { json!(LOCKED_MESSAGE) },
        })
        .to_string(),
        SessionToken::Undecodable => json!({
            "authenticated": false,
            "executor_locked": !unlocked,
            "token_type": "unknown",
            "message": if unlocked {
                "Token set but invalid - could not decode"
            } else {
                LOCKED_MESSAGE
            },
        })
        .to_string(),
        SessionToken::Absent => json!({
            "authenticated": false,
            "executor_locked": !unlocked,
            "message": if unlocked {
                "Not authenticated. Use request_capability + generate_jwt, login_email, or signup + verify_email_code to authenticate."
            } else {
                LOCKED_MESSAGE
            },
        })
        .to_string(),
    }
}

#[cfg(test)]
mod auth_status_tests {
    use super::{auth_status_json, SessionToken};
    use serde_json::Value;

    fn parse(session: SessionToken, unlocked: bool) -> Value {
        serde_json::from_str(&auth_status_json(session, unlocked)).expect("valid JSON")
    }

    fn valid() -> SessionToken {
        SessionToken::Valid {
            app_name: "mcp-agent".to_string(),
            user_email: Some("agent@example.org".to_string()),
            has_capabilities: true,
        }
    }

    #[test]
    fn no_token_on_an_unlocked_executor_tells_the_agent_to_authenticate() {
        let v = parse(SessionToken::Absent, true);
        assert_eq!(v["authenticated"], false);
        assert_eq!(v["executor_locked"], false);
        assert!(v["message"].as_str().unwrap().contains("login_email"));
    }

    #[test]
    fn no_token_on_a_locked_executor_names_the_operator_action_instead() {
        let v = parse(SessionToken::Absent, false);
        assert_eq!(v["executor_locked"], true);
        let msg = v["message"].as_str().unwrap();
        assert!(msg.contains("unlockAgent"), "message was: {msg}");
        // The regression this guards: advising an agent to log in when no login can work.
        assert!(!msg.contains("login_email"), "message was: {msg}");
    }

    #[test]
    fn an_undecodable_token_on_a_locked_executor_reports_the_lock_not_the_token() {
        let v = parse(SessionToken::Undecodable, false);
        assert_eq!(v["authenticated"], false);
        assert_eq!(v["executor_locked"], true);
        assert!(v["message"].as_str().unwrap().contains("unlockAgent"));
    }

    #[test]
    fn an_undecodable_token_on_an_unlocked_executor_still_reports_the_token() {
        let v = parse(SessionToken::Undecodable, true);
        assert_eq!(v["executor_locked"], false);
        assert!(v["message"].as_str().unwrap().contains("could not decode"));
    }

    #[test]
    fn a_valid_token_on_an_unlocked_executor_is_the_ordinary_answer() {
        let v = parse(valid(), true);
        assert_eq!(v["authenticated"], true);
        assert_eq!(v["executor_locked"], false);
        assert_eq!(v["app_name"], "mcp-agent");
        assert!(v["message"].is_null());
    }

    #[test]
    fn a_valid_token_on_a_locked_executor_stays_authenticated_and_says_why_nothing_works() {
        let v = parse(valid(), false);
        assert_eq!(
            v["authenticated"], true,
            "a lock does not invalidate a token"
        );
        assert_eq!(v["executor_locked"], true);
        assert!(v["message"].as_str().unwrap().contains("unlockAgent"));
    }
}
