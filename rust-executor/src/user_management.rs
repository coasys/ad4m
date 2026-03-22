//! Shared user management functions for MCP and GraphQL auth flows.
//!
//! Extracted to avoid code duplication between MCP tools and GraphQL mutation resolvers.

use crate::agent::capabilities::{
    get_user_default_capabilities, token::generate_jwt as generate_jwt_token, AuthInfo,
    DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::AgentService;
use crate::db::Ad4mDb;

/// Check if multi-user mode is enabled.
pub fn is_multi_user_enabled() -> bool {
    Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false))
}

/// Create a verification code for the given email and type ("signup" or "login").
pub fn create_verification_code(email: &str, verification_type: &str) -> Result<String, String> {
    let db = Ad4mDb::global_instance();
    let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
    let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
    db_ref
        .create_verification_code(email, verification_type)
        .map_err(|e| format!("Failed to create verification code: {}", e))
}

/// Verify a code for the given email and type.
pub fn verify_code(email: &str, code: &str, verification_type: &str) -> Result<bool, String> {
    let db = Ad4mDb::global_instance();
    let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
    let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
    db_ref
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

    // Check if user already exists
    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(email).is_ok());
    if user_exists {
        return Err("User already exists".to_string());
    }

    // Add user to DB
    {
        let db = Ad4mDb::global_instance();
        let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
        let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
        db_ref
            .add_user(email, &did, password)
            .map_err(|e| format!("Failed to add user: {}", e))?;
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
pub fn verify_credentials(email: &str, password: &str) -> Result<(), String> {
    let password_valid = Ad4mDb::with_global_instance(|db| {
        db.verify_user_password(email, password).unwrap_or(false)
    });
    if !password_valid {
        return Err("Invalid credentials".to_string());
    }
    if !AgentService::user_exists(email) {
        return Err("User key not found on executor".to_string());
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
    let db_exists = Ad4mDb::with_global_instance(|db| db.get_user(email).is_ok());
    if !db_exists {
        return Err("User not found".to_string());
    }
    if !AgentService::user_exists(email) {
        return Err("User key not found on executor".to_string());
    }
    Ok(())
}
