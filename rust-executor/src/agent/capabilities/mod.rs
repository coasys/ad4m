pub mod apps_map;
pub mod defs;
pub mod requests_map;
pub mod token;
pub mod types;

pub use defs::*;
use requests_map::{get_request, insert_request, remove_request};
pub use token::*;
#[allow(ambiguous_glob_reexports)]
pub use types::*;

use crate::pubsub::{get_global_pubsub, APPS_CHANGED, EXCEPTION_OCCURRED_TOPIC};
use crate::types::*;
use crate::utils::constant_time_eq;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub const DEFAULT_TOKEN_VALID_PERIOD: u64 = 180 * 24 * 60 * 60; // 180 days in seconds

/// Wrong codes a capability request may receive before it stops accepting any code.
/// The code has six digits, so without a limit anyone who learned a request id could
/// try every code.
pub const MAX_CODE_ATTEMPTS: u32 = 5;

lazy_static! {
    static ref FAILED_CODE_ATTEMPTS: std::sync::Mutex<HashMap<String, u32>> =
        std::sync::Mutex::new(HashMap::new());
}

// Cache for last_seen timestamps to avoid repeated database lookups
// Maps user_email -> (last_checked_timestamp, last_seen_value)
#[derive(Clone)]
struct LastSeenCacheEntry {
    last_checked: i64,    // When we last checked the database
    last_seen_value: i64, // The last_seen value we got from DB
}

lazy_static! {
    static ref LAST_SEEN_CACHE: Arc<RwLock<HashMap<String, LastSeenCacheEntry>>> =
        Arc::new(RwLock::new(HashMap::new()));
}

const CACHE_TTL_SECONDS: i64 = 300; // 5 minutes cache TTL

/// Minimum interval (seconds) between two `users.last_seen` writes for the same
/// user, so an active user's `last_seen` can be this stale. The auto-processor
/// supervisor's online window depends on it
/// ([`crate::perspectives::auto_processor::watcher::MANAGED_USER_ONLINE_WINDOW_S`], #1070).
pub const LAST_SEEN_WRITE_THROTTLE_S: i64 = 300;

/// Who can connect to the listener a request arrived on.
///
/// Without an admin credential the executor runs the single-user local trust model: a caller
/// with no token is the operator. That premise holds on a loopback listener and nowhere else,
/// so it is checked per listener instead of assumed (#1059). The API server attaches each
/// listener's reach where it binds it (`api::listener_router`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListenerReach {
    /// Bound to a loopback address: only processes on this machine connect, including any
    /// proxy or tunnel running here. The API reads a request that carries a `Forwarded`,
    /// `X-Forwarded-For` or `X-Real-IP` header as `Network` (`api::auth::listener_reach`). A
    /// proxy that sets none of them (a raw TCP forward, `ssh -R`) still reads as `Loopback`,
    /// so its remote clients are the operator: a node behind a proxy needs an admin credential.
    Loopback,
    /// Bound to any other address: whoever can route to it connects.
    Network,
}

impl ListenerReach {
    pub fn of(addr: &std::net::SocketAddr) -> Self {
        if addr.ip().is_loopback() {
            ListenerReach::Loopback
        } else {
            ListenerReach::Network
        }
    }
}

/// [`is_admin_credential_token_on`] for a caller on a loopback listener: for the MCP server,
/// where [`capabilities_from_token`] explains why loopback holds.
pub fn is_admin_credential_token(token: &str, admin_credential: &Option<String>) -> bool {
    is_admin_credential_token_on(token, admin_credential, ListenerReach::Loopback)
}

/// Returns true if the given token is the admin_credential that grants launcher-level access.
/// When admin_credential is Some, the token must match it exactly (constant-time).
/// When admin_credential is None (legacy single-user mode), an empty token is treated as admin,
/// but only on a loopback listener. On a network listener it is an anonymous caller (#1059).
pub fn is_admin_credential_token_on(
    token: &str,
    admin_credential: &Option<String>,
    reach: ListenerReach,
) -> bool {
    match admin_credential {
        Some(cred) => constant_time_eq(token, cred),
        None => token.is_empty() && reach == ListenerReach::Loopback,
    }
}

pub fn check_capability(
    capabilities: &Result<Vec<Capability>, String>,
    expected: &Capability,
) -> Result<(), String> {
    let capabilities = capabilities.clone()?;
    let custom_cap_match = |cap: &Capability, expected: &Capability| -> bool {
        if cap.with.domain != WILD_CARD && cap.with.domain != expected.with.domain {
            return false;
        }

        if !cap.with.pointers.contains(&WILD_CARD.to_string())
            && expected
                .with
                .pointers
                .iter()
                .any(|p| !cap.with.pointers.contains(p))
        {
            return false;
        }

        if !cap.can.contains(&WILD_CARD.to_string())
            && expected.can.iter().any(|c| !cap.can.contains(c))
        {
            return false;
        }

        true
    };

    if !capabilities
        .iter()
        .any(|cap| custom_cap_match(cap, expected))
    {
        return Err(format!(
            "Capability is not matched, you have capabilities: {:?}, expected: {:?}",
            capabilities, expected
        ));
    }

    Ok(())
}

pub fn check_token_revoked(token: &String) -> Result<(), String> {
    // Use constant-time comparison to prevent timing attacks
    if let Some(app) = apps_map::get_apps()
        .iter()
        .find(|app| constant_time_eq(&app.token, token))
    {
        if app.revoked.unwrap_or(false) {
            return Err("Unauthorized access".to_string());
        }
    };

    Ok(())
}

pub fn user_email_from_token(token: String) -> Option<String> {
    if token.is_empty() {
        return None;
    }

    // Check if multi-user mode is enabled - if not, never return a user context
    use crate::db::Ad4mDb;
    let multi_user_enabled =
        Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

    if !multi_user_enabled {
        return None;
    }

    // Try to decode JWT and extract user email from sub field
    if let Ok(claims) = decode_jwt(token) {
        claims.sub
    } else {
        None
    }
}

/// Update last_seen timestamp for the user from the auth token
/// Throttled to one write per [`LAST_SEEN_WRITE_THROTTLE_S`] to reduce database writes
/// Uses an in-memory cache to avoid blocking the async runtime with repeated DB lookups
pub async fn track_last_seen_from_token(token: String) {
    use crate::db::Ad4mDb;

    if let Some(user_email) = user_email_from_token(token) {
        let now = chrono::Utc::now().timestamp();

        // Check cache first (non-blocking read)
        {
            let cache = LAST_SEEN_CACHE.read().await;
            if let Some(entry) = cache.get(&user_email) {
                let cache_age = now - entry.last_checked;
                if cache_age < CACHE_TTL_SECONDS {
                    // Cache is fresh, check if update is needed based on cached value
                    let time_since_last_seen = now - entry.last_seen_value;
                    if time_since_last_seen < LAST_SEEN_WRITE_THROTTLE_S {
                        // Still inside the throttle period, no need to update
                        log::trace!(
                            "last_seen tracking for {}: cache hit, no update needed (last_seen={}, age={}s)",
                            user_email, entry.last_seen_value, time_since_last_seen
                        );
                        return;
                    }
                }
            }
        }

        // Cache miss or stale - need to check database
        // Use spawn_blocking to avoid blocking the async runtime
        let user_email_clone = user_email.clone();
        let should_update = tokio::task::spawn_blocking(move || {
            Ad4mDb::with_global_instance(|db| {
                if let Ok(user) = db.get_user(&user_email_clone) {
                    if let Some(last_seen) = user.last_seen {
                        let throttle_cutoff = now.saturating_sub(LAST_SEEN_WRITE_THROTTLE_S);

                        // Handle unrealistic future timestamps by treating them as stale
                        // (allow some clock skew tolerance of 1 minute)
                        let should_update = if last_seen > now + 60 {
                            log::warn!(
                                "last_seen tracking for {}: unrealistic future timestamp {}, treating as stale",
                                user_email_clone, last_seen
                            );
                            true
                        } else {
                            last_seen < throttle_cutoff
                        };

                        log::trace!("last_seen tracking for {}: last_seen={}, throttle_cutoff={}, should_update={}",
                            user_email_clone, last_seen, throttle_cutoff, should_update);
                        (should_update, Some(last_seen))
                    } else {
                        log::debug!(
                            "last_seen tracking for {}: never seen before, updating now",
                            user_email_clone
                        );
                        (true, None) // Never updated, do it now
                    }
                } else {
                    log::warn!(
                        "last_seen tracking: user {} not found in database",
                        user_email_clone
                    );
                    (false, None) // User not found
                }
            })
        })
        .await;

        let (should_update, last_seen_value) = match should_update {
            Ok((update, value)) => (update, value),
            Err(e) => {
                log::error!(
                    "Failed to check last_seen status (spawn_blocking join error): {:?}",
                    e
                );
                return;
            }
        };

        // Update cache with the value we got from DB
        if let Some(last_seen_val) = last_seen_value {
            let mut cache = LAST_SEEN_CACHE.write().await;
            cache.insert(
                user_email.clone(),
                LastSeenCacheEntry {
                    last_checked: now,
                    last_seen_value: last_seen_val,
                },
            );
        }

        if should_update {
            log::debug!("Updating last_seen for user: {}", user_email);

            // Perform the update in spawn_blocking
            let user_email_for_update = user_email.clone();
            let update_result = tokio::task::spawn_blocking(move || {
                Ad4mDb::with_global_instance(|db| db.update_user_last_seen(&user_email_for_update))
            })
            .await;

            match update_result {
                Ok(Ok(())) => {
                    // Update succeeded, refresh cache with new timestamp
                    let mut cache = LAST_SEEN_CACHE.write().await;
                    cache.insert(
                        user_email,
                        LastSeenCacheEntry {
                            last_checked: now,
                            last_seen_value: now,
                        },
                    );
                }
                Ok(Err(e)) => {
                    log::error!(
                        "Failed to update last_seen for user {}: {:?}",
                        user_email,
                        e
                    );
                }
                Err(e) => {
                    log::error!(
                        "Failed to update last_seen for user {} (spawn_blocking join error): {:?}",
                        user_email,
                        e
                    );
                }
            }
        }
    }
}

/// [`capabilities_on`] for a caller on a loopback listener.
///
/// For the MCP server. Loopback holds there because `mcp::server::resolve_host`
/// binds an MCP server with no admin credential to loopback, and its HTTPS listener does not
/// start without one. The exception is an explicit `MCP_HOST`, which it warns about.
pub fn capabilities_from_token(
    token: String,
    admin_credential: Option<String>,
) -> Result<Vec<Capability>, String> {
    capabilities_on(token, admin_credential, ListenerReach::Loopback)
}

/// The capabilities of a caller that presents `token` on a listener with this reach.
pub fn capabilities_on(
    token: String,
    admin_credential: Option<String>,
    reach: ListenerReach,
) -> Result<Vec<Capability>, String> {
    // The same test that decides `is_admin_credential`, so the two cannot disagree.
    if is_admin_credential_token_on(&token, &admin_credential, reach) {
        return Ok(vec![ALL_CAPABILITY.clone()]);
    }

    if token.is_empty() {
        // An anonymous caller: the node has an admin credential, or the listener is on the
        // network. For empty tokens, check if multi-user mode is enabled
        // If so, allow user creation (registration), login, and checking enabled status
        // READ capability is intentionally excluded to prevent unauthenticated user enumeration
        use crate::db::Ad4mDb;
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if multi_user_enabled {
            return Ok(vec![
                AGENT_AUTH_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
            ]);
        }

        return Ok(vec![
            AGENT_AUTH_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
        ]);
    }

    check_token_revoked(&token)?;

    let claims = decode_jwt(token).map_err(|e| e.to_string())?;

    if claims.capabilities.capabilities.is_none() {
        Ok(vec![AGENT_AUTH_CAPABILITY.clone()])
    } else {
        Ok(claims.capabilities.capabilities.unwrap())
    }
}

pub async fn request_capability(auth_info: AuthInfo) -> String {
    let request_id = uuid::Uuid::new_v4().to_string();
    let app_name = auth_info.app_name.clone();

    let auth_extended = AuthInfoExtended {
        request_id: request_id.clone(),
        auth: auth_info,
    };

    let exception_info = ExceptionInfo {
        title: "Request to authenticate application".to_string(),
        message: format!(
            "{} is waiting for authentication, open the ADAM Launcher for more information.",
            app_name
        ),
        r#type: ExceptionType::CapabilityRequested,
        addon: Some(serde_json::to_string(&auth_extended).unwrap()),
    };

    get_global_pubsub()
        .await
        .publish(
            &EXCEPTION_OCCURRED_TOPIC,
            &serde_json::to_string(&exception_info).unwrap(),
        )
        .await;

    request_id
}

pub fn permit_capability(auth_info_extended: AuthInfoExtended) -> Result<String, String> {
    let rand = gen_random_digits();
    let request_key = gen_request_key(&auth_info_extended.request_id, &rand);
    insert_request(request_key.clone(), auth_info_extended.auth.clone())?;
    Ok(rand)
}

pub async fn generate_capability_token(request_id: String, rand: String) -> Result<String, String> {
    let auth_key = gen_request_key(&request_id, &rand);

    let auth = {
        let mut attempts = FAILED_CODE_ATTEMPTS.lock().map_err(|e| e.to_string())?;
        match get_request(&auth_key)? {
            Some(auth) => {
                attempts.remove(&request_id);
                auth
            }
            // Callers without a token reach this, so only ids with a permitted request get an
            // entry, and the entry goes when the request does. A made-up id records nothing.
            None if !requests_map::has_requests_for(&request_id)? => {
                attempts.remove(&request_id);
                return Err("Can't find permitted request".to_string());
            }
            None => {
                let count = attempts.entry(request_id.clone()).or_insert(0);
                *count += 1;
                if *count >= MAX_CODE_ATTEMPTS {
                    requests_map::remove_requests_for(&request_id)?;
                    attempts.remove(&request_id);
                    return Err(
                        "Too many wrong codes for this request; request access again".to_string(),
                    );
                }
                return Err("Can't find permitted request".to_string());
            }
        }
    };

    let cap_token = token::generate_jwt(
        auth.app_name.clone(),
        DEFAULT_TOKEN_VALID_PERIOD,
        auth.clone(),
    )
    .map_err(|e| e.to_string())?;

    remove_request(&auth_key)?;

    let auth_for_publish = auth.clone();

    apps_map::insert_app(
        request_id.clone(),
        AuthInfoExtended {
            request_id: request_id.clone(),
            auth,
        },
        cap_token.clone(),
    )?;

    // The event reaches every session that can approve apps; the token goes only to the caller.
    let apps_changed = Apps {
        auth: auth_for_publish,
        request_id: request_id.clone(),
        revoked: Some(false),
        token: String::new(),
    };
    get_global_pubsub()
        .await
        .publish(
            &APPS_CHANGED,
            &serde_json::to_string(&Some(apps_changed)).unwrap_or_else(|_| "null".to_string()),
        )
        .await;

    Ok(cap_token)
}

pub fn gen_random_digits() -> String {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    rng.gen_range(100000..1000000).to_string()
}

pub fn gen_request_key(request_id: &str, rand: &str) -> String {
    format!("{}-{}", request_id, rand)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_capability_is_expected() {
        let all_capability = &ALL_CAPABILITY;
        assert_eq!(all_capability.with.domain, "*");
        assert_eq!(all_capability.with.pointers, vec!["*"]);
        assert_eq!(all_capability.can, vec!["*"]);
    }

    #[test]
    fn agent_auth_capability_is_expected() {
        let agent_auth_capability = &AGENT_AUTH_CAPABILITY;
        assert_eq!(agent_auth_capability.with.domain, "agent");
        assert_eq!(agent_auth_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_auth_capability.can, vec!["AUTHENTICATE"]);
    }

    #[test]
    fn agent_read_capability_is_expected() {
        let agent_read_capability = &AGENT_READ_CAPABILITY;
        assert_eq!(agent_read_capability.with.domain, "agent");
        assert_eq!(agent_read_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_read_capability.can, vec!["READ"]);
    }

    #[test]
    fn runtime_user_management_read_enabled_capability_is_expected() {
        let capability = &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY;
        assert_eq!(capability.with.domain, "runtime.user_management");
        assert_eq!(capability.with.pointers, vec!["enabled"]);
        assert_eq!(capability.can, vec!["READ"]);
    }

    #[test]
    fn runtime_user_management_login_capability_is_expected() {
        let capability = &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY;
        assert_eq!(capability.with.domain, "runtime.user_management");
        assert_eq!(capability.with.pointers, vec!["*"]);
        assert_eq!(capability.can, vec!["LOGIN"]);
    }

    #[test]
    fn agent_create_capability_is_expected() {
        let agent_create_capability = &AGENT_CREATE_CAPABILITY;
        assert_eq!(agent_create_capability.with.domain, "agent");
        assert_eq!(agent_create_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_create_capability.can, vec!["CREATE"]);
    }

    #[test]
    fn query_capability_is_expected() {
        let capability = perspective_query_capability(vec!["123".to_string(), "456".to_string()]);
        assert_eq!(capability.with.domain, "perspective");
        assert_eq!(capability.with.pointers, vec!["123", "456"]);
        assert_eq!(capability.can, vec!["READ"]);
    }

    #[test]
    fn agent_with_all_capability_can_permit_an_auth_request() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_PERMIT_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_all_capability_can_request_agent_status() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_READ_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_all_capability_can_mutate_the_agent() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_CREATE_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_request_the_agent_status() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_READ_CAPABILITY
        )
        .is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_mutate_the_agent() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_CREATE_CAPABILITY
        )
        .is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_can_request_an_auth() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_AUTH_CAPABILITY
        )
        .is_ok());
    }

    #[test]
    fn agent_with_agent_read_capability_can_request_the_agent_status() {
        assert!(check_capability(
            &Ok(vec![AGENT_READ_CAPABILITY.clone()]),
            &AGENT_READ_CAPABILITY
        )
        .is_ok());
    }

    #[test]
    fn agent_with_perspective_query_capability_can_query_a_perspective() {
        let query_capability = perspective_query_capability(vec!["*".to_string()]);
        let expected_capability = perspective_query_capability(vec!["123".to_string()]);
        assert!(check_capability(&Ok(vec![query_capability]), &expected_capability).is_ok());
    }

    #[test]
    fn gen_random_digits_returns_a_6_digit_string() {
        let rand = gen_random_digits();
        assert!(rand.len() == 6 && rand.chars().all(|c| c.is_ascii_digit()));
    }

    #[test]
    fn gen_request_key_joins_the_request_id_and_rand() {
        let key = gen_request_key("my-request-id", "123456");
        assert_eq!(key, "my-request-id-123456");
    }

    #[test]
    fn a_listener_is_loopback_only_on_a_loopback_address() {
        let reach = |addr: &str| ListenerReach::of(&addr.parse().unwrap());
        assert_eq!(reach("127.0.0.1:12000"), ListenerReach::Loopback);
        assert_eq!(reach("[::1]:12000"), ListenerReach::Loopback);
        assert_eq!(reach("0.0.0.0:12000"), ListenerReach::Network);
        assert_eq!(reach("[::]:12000"), ListenerReach::Network);
        assert_eq!(reach("192.168.1.5:12000"), ListenerReach::Network);
    }

    // #1059: without an admin credential, an empty token is the operator only on loopback.
    // On the network it gets what an anonymous caller gets on a node with a credential:
    // enough to request a capability, nothing to read with.
    #[test]
    fn without_a_credential_an_empty_token_is_the_operator_only_on_loopback() {
        let empty = String::new();
        assert!(is_admin_credential_token_on(
            &empty,
            &None,
            ListenerReach::Loopback
        ));
        assert!(!is_admin_credential_token_on(
            &empty,
            &None,
            ListenerReach::Network
        ));

        let local = Ok(capabilities_on(empty.clone(), None, ListenerReach::Loopback).unwrap());
        assert!(check_capability(&local, &AGENT_READ_CAPABILITY).is_ok());

        // The anonymous set reads the multi-user setting.
        let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        let remote = Ok(capabilities_on(empty, None, ListenerReach::Network).unwrap());
        assert!(check_capability(&remote, &AGENT_READ_CAPABILITY).is_err());
        assert!(check_capability(&remote, &AGENT_AUTH_CAPABILITY).is_ok());
    }

    // The admin credential works on any listener: it is what makes a network listener safe.
    #[test]
    fn the_admin_credential_is_the_operator_on_the_network() {
        let credential = Some("the-admin-credential".to_string());
        assert!(is_admin_credential_token_on(
            "the-admin-credential",
            &credential,
            ListenerReach::Network
        ));
        let capabilities = capabilities_on(
            "the-admin-credential".to_string(),
            credential,
            ListenerReach::Network,
        );
        assert!(check_capability(&capabilities, &AGENT_READ_CAPABILITY).is_ok());
    }

    #[test]
    fn unauthenticated_users_can_check_multi_user_enabled() {
        // Empty token (unauthenticated) should have capability to check if multi-user is enabled
        let capabilities = capabilities_from_token(String::new(), None).unwrap();
        assert!(
            check_capability(
                &Ok(capabilities),
                &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY
            )
            .is_ok(),
            "Unauthenticated users should be able to check if multi-user mode is enabled"
        );
    }

    #[test]
    fn unauthenticated_users_get_login_capability_in_multi_user_mode() {
        // Empty token (unauthenticated) should have the right capabilities
        let capabilities = capabilities_from_token(String::new(), None).unwrap();

        // Should have read enabled capability (to check if multi-user is on)
        assert!(
            check_capability(
                &Ok(capabilities.clone()),
                &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY
            )
            .is_ok(),
            "Unauthenticated users should be able to check if multi-user mode is enabled"
        );

        // Should have auth capability (for standard capability request flow)
        assert!(
            check_capability(&Ok(capabilities.clone()), &AGENT_AUTH_CAPABILITY).is_ok(),
            "Unauthenticated users should have auth capability"
        );

        // Note: We can't easily test the multi-user mode grant of LOGIN and CREATE capabilities
        // without setting up the database, but the logic is tested by integration tests
    }
}

#[cfg(test)]
mod app_token_tests {
    use super::*;
    use std::time::Duration;

    fn test_auth_info() -> AuthInfo {
        AuthInfo {
            app_name: "Test app".to_string(),
            app_desc: "An app for tests".to_string(),
            app_domain: Some("test.example".to_string()),
            app_url: Some("https://test.example".to_string()),
            app_icon_path: None,
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            user_email: None,
        }
    }

    async fn permitted_request() -> (String, String) {
        let request_id = request_capability(test_auth_info()).await;
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: test_auth_info(),
        })
        .unwrap();
        (request_id, code)
    }

    // The code has six digits. Without a limit, anyone who learned a request id could try
    // every code and receive the app's token.
    #[tokio::test]
    async fn a_request_stops_accepting_codes_after_too_many_wrong_ones() {
        let (request_id, code) = permitted_request().await;
        let wrong = if code == "100000" { "100001" } else { "100000" };
        let mut last_err = String::new();
        for _ in 0..MAX_CODE_ATTEMPTS {
            last_err = generate_capability_token(request_id.clone(), wrong.to_string())
                .await
                .unwrap_err();
        }
        assert!(last_err.contains("Too many wrong codes"), "{}", last_err);
        assert!(generate_capability_token(request_id.clone(), code)
            .await
            .is_err());
    }

    // `agent.generateJwt` needs no token, so anyone can send made-up request ids. They must
    // not leave entries behind, or repeated calls fill memory.
    #[tokio::test]
    async fn made_up_request_ids_leave_no_attempt_entries() {
        for i in 0..10_000 {
            let _ = generate_capability_token(format!("made-up-{}", i), "100000".to_string()).await;
        }
        let left = FAILED_CODE_ATTEMPTS
            .lock()
            .unwrap()
            .keys()
            .filter(|id| id.starts_with("made-up-"))
            .count();
        assert_eq!(left, 0);
    }

    // The entry for a real request goes once its codes are gone.
    #[tokio::test]
    async fn a_locked_out_request_leaves_no_attempt_entry() {
        let (request_id, code) = permitted_request().await;
        let wrong = if code == "100000" { "100001" } else { "100000" };
        for _ in 0..MAX_CODE_ATTEMPTS {
            let _ = generate_capability_token(request_id.clone(), wrong.to_string()).await;
        }
        assert!(!FAILED_CODE_ATTEMPTS
            .lock()
            .unwrap()
            .contains_key(&request_id));
    }

    // Request ids are UUIDs, which contain dashes, and the stored keys are `{id}-{code}`.
    // Wrong codes sent for only the start of someone else's id must not lock that request out.
    #[tokio::test]
    async fn wrong_codes_for_a_prefix_of_an_id_leave_that_request_alone() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let dir = tempfile::tempdir().unwrap();
        apps_map::set_data_file_path(dir.path().join("apps.json").to_str().unwrap().to_string());
        let (request_id, code) = permitted_request().await;
        let prefix = request_id.split('-').next().unwrap().to_string();
        assert_ne!(prefix, request_id);
        for _ in 0..MAX_CODE_ATTEMPTS {
            let _ = generate_capability_token(prefix.clone(), "100000".to_string()).await;
        }
        assert!(generate_capability_token(request_id, code).await.is_ok());
    }

    #[tokio::test]
    async fn a_correct_code_after_a_few_wrong_ones_still_works() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let dir = tempfile::tempdir().unwrap();
        apps_map::set_data_file_path(dir.path().join("apps.json").to_str().unwrap().to_string());
        let (request_id, code) = permitted_request().await;
        let wrong = if code == "100000" { "100001" } else { "100000" };
        for _ in 0..MAX_CODE_ATTEMPTS - 1 {
            assert!(
                generate_capability_token(request_id.clone(), wrong.to_string())
                    .await
                    .is_err()
            );
        }
        assert!(generate_capability_token(request_id, code).await.is_ok());
    }

    // The `apps-changed` event and the app list reach sessions other than the app, so
    // neither may carry the token. The executor still knows the token for revocation.
    #[tokio::test]
    async fn app_events_and_the_client_app_list_carry_no_token() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let dir = tempfile::tempdir().unwrap();
        apps_map::set_data_file_path(dir.path().join("apps.json").to_str().unwrap().to_string());
        let mut events = get_global_pubsub().await.subscribe(&APPS_CHANGED).await;

        let (request_id, code) = permitted_request().await;
        let token = generate_capability_token(request_id.clone(), code)
            .await
            .unwrap();
        assert!(!token.is_empty());

        let event = loop {
            let msg = tokio::time::timeout(Duration::from_secs(5), events.recv())
                .await
                .expect("an apps-changed event arrives")
                .unwrap();
            if let Ok(Some(app)) = serde_json::from_str::<Option<Apps>>(&msg) {
                if app.request_id == request_id {
                    break app;
                }
            }
        };
        assert!(event.token.is_empty(), "the event carried the app token");

        let listed = apps_map::client_view()
            .into_iter()
            .find(|app| app.request_id == request_id)
            .expect("the app appears in the list");
        assert!(
            listed.token.is_empty(),
            "the app list carried the app token"
        );

        assert!(check_token_revoked(&token).is_ok());
        apps_map::revoke_app(&request_id).unwrap();
        assert!(check_token_revoked(&token).is_err());
    }
}
