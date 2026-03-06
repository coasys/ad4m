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
    /// Check that the agent wallet is unlocked; return Err(json) if locked.
    fn check_wallet_unlocked() -> Result<(), String> {
        let wallet = crate::wallet::Wallet::instance();
        let wallet_lock = wallet.lock().expect("wallet lock");
        let wallet_ref = wallet_lock.as_ref().expect("wallet instance");
        if !wallet_ref.is_unlocked() {
            return Err(serde_json::json!({
                "success": false,
                "error": "Agent wallet is locked. Call agentUnlock first (or use login_email for multi-user mode)."
            }).to_string());
        }
        Ok(())
    }

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
        if let Err(msg) = Self::check_wallet_unlocked() {
            return msg;
        }

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
                println!("MCP capability request - code: {}", code);
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
        if let Err(msg) = Self::check_wallet_unlocked() {
            return msg;
        }

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

        match &*token {
            Some(t) if !t.is_empty() => {
                match decode_jwt(t.clone()) {
                    Ok(claims) => json!({
                        "authenticated": true,
                        "app_name": claims.capabilities.app_name,
                        "user_email": claims.capabilities.user_email,
                        "has_capabilities": claims.capabilities.capabilities.is_some(),
                    })
                    .to_string(),
                    Err(_) => {
                        json!({
                            "authenticated": false,
                            "token_type": "unknown",
                            "message": "Token set but invalid - could not decode"
                        })
                        .to_string()
                    }
                }
            }
            _ => json!({
                "authenticated": false,
                "message": "Not authenticated. Use request_capability + generate_jwt, login_email, or signup + verify_email_code to authenticate."
            })
            .to_string(),
        }
    }
}
