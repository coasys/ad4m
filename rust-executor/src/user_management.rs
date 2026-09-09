//! Shared user management functions for MCP and REST auth flows.
//!
//! Extracted to avoid code duplication between MCP tools and REST handlers.

use crate::agent::capabilities::{
    get_user_default_capabilities, token::generate_jwt as generate_jwt_token, AuthInfo,
    DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::AgentService;
use crate::db::Ad4mDb;
use crate::wallet::wallet_backend;

/// Returns `Err` with an operator-facing message when the executor wallet is locked.
///
/// After a restart, in-memory keys are gone until the admin calls `unlockAgent`. Any
/// call to `AgentService::user_exists` before that point returns `false`, which would
/// otherwise surface as "User key not found" — indistinguishable from a deleted account.
/// Calling this guard first gives callers a clear, actionable message.
fn check_executor_unlocked() -> Result<(), String> {
    if !wallet_backend().is_unlocked() {
        return Err(
            "Executor is locked: its admin has not unlocked the agent yet (keys are held in \
             memory only, so this happens after every restart). Ask the executor operator to \
             call unlockAgent, then retry."
                .to_string(),
        );
    }
    Ok(())
}

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
    check_executor_unlocked()?;

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
    let user_exists = Ad4mDb::with_global_instance(|db| db.get_user(email).is_ok());
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
    {
        let db = Ad4mDb::global_instance();
        let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
        let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
        db_ref
            .add_user_prehashed(email, &did, &password_hash)
            .map_err(|e| format!("Failed to add user: {}", e))?;
    }

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
    check_executor_unlocked()?;

    // Try local DB first
    let local_result = Ad4mDb::with_global_instance(|db| db.verify_user_password(email, password));

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
    {
        let db = Ad4mDb::global_instance();
        let db_lock = db.lock().expect("Couldn't get lock on Ad4mDb");
        let db_ref = db_lock.as_ref().expect("Ad4mDb not initialized");
        if let Err(e) = db_ref.add_user_prehashed(email, user_did, stored_hash) {
            log::warn!("Failed to import user to local DB: {}", e);
        }
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

    Ad4mDb::with_global_instance(|db| db.check_and_update_rate_limit(email))
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
    check_executor_unlocked()?;
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
    check_executor_unlocked()?;
    let db_exists = Ad4mDb::with_global_instance(|db| db.get_user(email).is_ok());
    if !db_exists {
        return Err("User not found".to_string());
    }
    if !AgentService::user_exists(email) {
        return Err("User key not found on executor".to_string());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{set_global_config, Ad4mConfig};
    use crate::wallet::{try_init_wallet_backend, LocalWallet, WalletBackend};
    use std::sync::Arc;

    /// Initialise the in-process globals that create_user touches.
    ///
    /// `AgentService` is initialised deliberately, even though the guard returns
    /// long before `create_user` reaches it. Without it the *negative* direction —
    /// the run with the guard deleted, which is what proves the guard is
    /// load-bearing — panics on "AgentService not initialized" inside
    /// `ensure_user_key_exists` and never reaches the row assertion. It would
    /// still go red, but for the wrong reason, and a test that goes red for the
    /// wrong reason does not defend the claim in #982.
    fn setup() {
        set_global_config(Ad4mConfig::default());
        Ad4mDb::init_global_instance(":memory:").expect("init in-memory DB");
        // A fresh LocalWallet has no keys → is_unlocked() returns false.
        // try_init is idempotent: if another test already set the backend it is a no-op.
        let _ = try_init_wallet_backend(Arc::new(LocalWallet::new()) as Arc<dyn WalletBackend>);
        crate::test_utils::setup_agent();
    }

    /// Invariant: create_user on a locked executor must return Err before writing any DB row.
    ///
    /// Without the check_executor_unlocked() guard, the wallet-save is silently
    /// skipped (passphrase is None while locked), but the user row is still written.
    /// The account is then permanently unusable and the email cannot be re-registered
    /// (issue #982, found by Lal during #973 round-4 testing).
    #[test]
    fn create_user_on_locked_executor_errors_and_leaves_no_row() {
        setup();

        let backend = wallet_backend();

        // If a prior test left the wallet unlocked, lock it now so the guard
        // sees the locked-executor state.  lock() is a no-op when keys are None,
        // but is_unlocked() is already false in that case, so the assert below holds.
        let was_unlocked = backend.is_unlocked();
        let test_pass = "test-982-lock-passphrase";
        if was_unlocked {
            backend.lock(test_pass);
        }
        assert!(
            !backend.is_unlocked(),
            "wallet must be locked at the start of this test"
        );

        let email = "create-user-locked-982@example.com";

        // Act: call create_user while the executor is locked.
        let result = create_user(email, "any-password");

        // The row is checked FIRST, before the return value, on purpose. The row
        // is the damage #982 describes; the Err is only how the caller learns of
        // it. Asserting the return value first would panic before this line in a
        // run where the guard is missing, so the test would report a wrong return
        // value and never observe the row that actually bricks the account. In
        // that order it also stays a real guard if a future refactor moves the
        // check somewhere that still returns Err but writes the row anyway.
        let row_written = Ad4mDb::with_global_instance(|db| db.get_user(email).is_ok());
        assert!(
            !row_written,
            "create_user must not write a user row when the executor is locked — \
             a row here bricks the account: the wallet was never saved, and the \
             email can never be registered again"
        );

        // And the caller must be told, with the operator-facing locked message.
        assert!(
            result.is_err(),
            "create_user must return Err when the executor is locked"
        );
        let err = result.unwrap_err();
        assert!(
            err.contains("Executor is locked"),
            "error must mention 'Executor is locked'; got: {err}"
        );

        // Restore wallet state so subsequent tests are not affected.
        if was_unlocked {
            backend.unlock(test_pass).expect("restore wallet unlock");
        }
    }
}
