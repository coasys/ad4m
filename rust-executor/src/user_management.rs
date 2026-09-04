//! Shared user management functions for MCP and REST auth flows.
//!
//! Extracted to avoid code duplication between MCP tools and REST handlers.

use crate::agent::capabilities::{
    get_user_default_capabilities, token::generate_jwt as generate_jwt_token, AuthInfo,
    DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::AgentService;
use crate::db::Ad4mDb;

/// Check if multi-user mode is enabled.
pub fn is_multi_user_enabled() -> bool {
    crate::db_backend::db_backend()
        .get_multi_user_enabled()
        .unwrap_or(false)
}

/// Create a verification code for the given email and type ("signup" or "login").
pub fn create_verification_code(email: &str, verification_type: &str) -> Result<String, String> {
    crate::db_backend::db_backend()
        .create_verification_code(email, verification_type)
        .map_err(|e| format!("Failed to create verification code: {}", e))
}

/// Verify a code for the given email and type.
pub fn verify_code(email: &str, code: &str, verification_type: &str) -> Result<bool, String> {
    crate::db_backend::db_backend()
        .verify_code(email, code, verification_type)
        .map_err(|e| format!("Verification failed: {}", e))
}

/// Send a verification email. Handles SMTP config and test mode.
pub async fn send_verification_email(
    email: &str,
    code: &str,
    verification_type: &str,
    app_name: Option<&str>,
) -> Result<(), String> {
    let smtp_config_opt = crate::config::SMTP_CONFIG
        .lock()
        .ok()
        .and_then(|cfg| cfg.clone())
        .filter(|config| config.enabled);
    let test_mode = crate::email_service::EMAIL_TEST_MODE
        .lock()
        .ok()
        .map(|mode| *mode)
        .unwrap_or(false);

    if test_mode || smtp_config_opt.is_some() {
        let smtp_config = if test_mode && smtp_config_opt.is_none() {
            crate::config::SmtpConfig {
                enabled: true,
                host: "test.localhost".to_string(),
                port: 587,
                username: "test".to_string(),
                password: "test".to_string(),
                from_address: "test@localhost".to_string(),
            }
        } else {
            smtp_config_opt.unwrap()
        };
        let email_service = crate::email_service::EmailService::new(smtp_config);
        email_service
            .send_verification_email(email, code, verification_type, app_name, None)
            .await
            .map_err(|e| format!("Failed to send verification email: {}", e))?;
    }

    Ok(())
}

/// Create a new user: ensure key, get DID, save wallet, add to DB.
pub fn create_user(email: &str, password: &str) -> Result<String, String> {
    // Ensure user key exists
    AgentService::ensure_user_key_exists(email)
        .map_err(|e| format!("Failed to create user key: {}", e))?;

    // Get DID
    let did = AgentService::get_user_did_by_email(email)
        .map_err(|e| format!("Failed to get user DID: {}", e))?;

    // Save wallet
    AgentService::with_global_instance(|s| {
        if let Some(p) = &s.passphrase {
            s.save(p.clone());
        }
    });

    // Check if user already exists (local DB or shared DB)
    let user_exists = match crate::db_backend::db_backend().get_user(email) {
        Ok(_) => true,
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("not found")
                || msg.contains("No user")
                || msg.contains("Query returned no rows")
            {
                false
            } else {
                return Err(format!("Failed to check user: {}", msg));
            }
        }
    };
    if user_exists {
        return Err("User already exists".to_string());
    }

    // Also check shared DB to prevent duplicates across executors.
    // Use a fixed namespace ("shared:platform") so all executors share one user table,
    // regardless of each executor's individual agent DID.
    let config = crate::config::get_global_config();
    if config.db_backend.as_deref() == Some("shared") {
        let backend = crate::db_backend::db_backend();
        if let Ok(Some(_)) = backend.get("shared:platform", "users", email) {
            return Err("User already exists".to_string());
        }
    }

    // Hash password once — use the same hash for both local and shared DB
    let password_hash =
        Ad4mDb::hash_password(password).map_err(|e| format!("Failed to hash password: {}", e))?;

    // Add user to local DB
    crate::db_backend::db_backend()
        .add_user_prehashed(email, &did, &password_hash)
        .map_err(|e| format!("Failed to add user: {}", e))?;

    // Also store in shared DB for cross-executor access
    if config.db_backend.as_deref() == Some("shared") {
        let backend = crate::db_backend::db_backend();
        let user_data = serde_json::json!({
            "username": email,
            "did": &did,
            "password_hash": &password_hash,
        });
        if let Err(e) = backend.upsert("shared:platform", "users", email, user_data) {
            log::warn!("Failed to sync user to shared DB: {}", e);
        }
    }

    Ok(did)
}

/// Generate a JWT token for a user with default capabilities.
pub fn generate_user_jwt(email: &str, app_name: &str) -> Result<String, String> {
    let auth_info = AuthInfo {
        app_name: app_name.to_string(),
        app_desc: format!("{} user session", app_name),
        app_domain: Some("mcp".to_string()),
        app_url: Some("https://ad4m.dev/mcp".to_string()),
        app_icon_path: None,
        capabilities: Some(get_user_default_capabilities()),
        user_email: Some(email.to_string()),
    };

    generate_jwt_token(
        auth_info.app_name.clone(),
        DEFAULT_TOKEN_VALID_PERIOD,
        auth_info,
    )
    .map_err(|e| format!("Failed to generate token: {}", e))
}

/// Verify user credentials (email + password). Returns Ok(()) on success.
/// Falls back to shared DB when the user record only exists on another executor.
pub fn verify_credentials(email: &str, password: &str) -> Result<(), String> {
    // Try local DB first
    let local_result = crate::db_backend::db_backend().verify_user_password(email, password);

    match local_result {
        Ok(true) => {
            // Local verification succeeded
            if !AgentService::user_exists(email) {
                return Err("User key not found on executor".to_string());
            }
            return Ok(());
        }
        Ok(false) => {
            // Password wrong (user found locally but password doesn't match)
            return Err("Invalid credentials".to_string());
        }
        Err(_) => {
            // User not found in local DB — try shared DB fallback
        }
    }

    // Shared DB fallback: user was created on another executor
    let config = crate::config::get_global_config();
    if config.db_backend.as_deref() != Some("shared") {
        return Err("Invalid credentials".to_string());
    }

    let backend = crate::db_backend::db_backend();
    let user_data = backend
        .get("shared:platform", "users", email)
        .map_err(|e| format!("Shared DB lookup failed: {}", e))?
        .ok_or_else(|| "Invalid credentials".to_string())?;

    // Extract password_hash from shared record and verify
    let stored_hash = user_data
        .get("password_hash")
        .and_then(|h| h.as_str())
        .ok_or_else(|| "Invalid credentials".to_string())?;

    let pw_ok = Ad4mDb::verify_password(password, stored_hash)
        .map_err(|e| format!("Password verification failed: {}", e))?;
    if !pw_ok {
        return Err("Invalid credentials".to_string());
    }

    // Ensure the user key exists in the shared wallet
    if !AgentService::user_exists(email) {
        AgentService::ensure_user_key_exists(email)
            .map_err(|e| format!("Failed to create user key: {}", e))?;
    }

    // Import user to local DB for future logins
    let user_did = user_data.get("did").and_then(|d| d.as_str()).unwrap_or("");
    if let Err(e) = crate::db_backend::db_backend().add_user_prehashed(email, user_did, stored_hash)
    {
        log::warn!("Failed to import user to local DB: {}", e);
    }

    Ok(())
}

/// Full login flow: check multi-user, verify credentials, generate JWT.
pub fn login_user(email: &str, password: &str, app_name: &str) -> Result<String, String> {
    if !is_multi_user_enabled() {
        return Err("Multi-user mode is not enabled".to_string());
    }
    verify_credentials(email, password)?;
    generate_user_jwt(email, app_name)
}

/// Full signup flow: check multi-user, create user, generate verification code, send email.
pub async fn signup_user(
    email: &str,
    password: &str,
    app_name: Option<&str>,
) -> Result<String, String> {
    if !is_multi_user_enabled() {
        return Err("Multi-user mode is not enabled".to_string());
    }
    let did = create_user(email, password)?;
    let code = create_verification_code(email, "signup")?;
    send_verification_email(email, &code, "signup", app_name).await?;
    Ok(did)
}

/// Full login verification flow: check multi-user, create code, send email.
pub async fn request_login_code(email: &str, app_name: Option<&str>) -> Result<(), String> {
    if !is_multi_user_enabled() {
        return Err("Multi-user mode is not enabled".to_string());
    }
    user_exists(email)?;

    crate::db_backend::db_backend()
        .check_and_update_rate_limit(email)
        .map_err(|e| e.to_string())?;

    let code = create_verification_code(email, "login")?;
    send_verification_email(email, &code, "login", app_name).await?;
    Ok(())
}

/// Full email verification flow: verify code, generate JWT.
pub fn verify_and_login(
    email: &str,
    code: &str,
    verification_type: &str,
    app_name: &str,
) -> Result<String, String> {
    if !is_multi_user_enabled() {
        return Err("Multi-user mode is not enabled".to_string());
    }
    let verified = verify_code(email, code, verification_type)?;
    if !verified {
        return Err("Invalid verification code".to_string());
    }
    if !AgentService::user_exists(email) {
        return Err("User key not found on executor".to_string());
    }
    generate_user_jwt(email, app_name)
}

/// Check if a user exists in both DB and AgentService.
pub fn user_exists(email: &str) -> Result<(), String> {
    let db_exists = match crate::db_backend::db_backend().get_user(email) {
        Ok(_) => true,
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("not found")
                || msg.contains("No user")
                || msg.contains("Query returned no rows")
            {
                false
            } else {
                return Err(format!("Failed to check user: {}", msg));
            }
        }
    };
    if !db_exists {
        return Err("User not found".to_string());
    }
    if !AgentService::user_exists(email) {
        return Err("User key not found on executor".to_string());
    }
    Ok(())
}
