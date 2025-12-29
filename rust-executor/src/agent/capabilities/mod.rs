pub mod apps_map;
pub mod defs;
pub mod requests_map;
pub mod token;
pub mod types;

pub use defs::*;
use requests_map::{get_request, insert_request, remove_request};
pub use token::*;
pub use types::*;

use crate::graphql::graphql_types::*;
use crate::pubsub::{get_global_pubsub, APPS_CHANGED, EXCEPTION_OCCURRED_TOPIC};

pub const DEFAULT_TOKEN_VALID_PERIOD: u64 = 180 * 24 * 60 * 60; // 180 days in seconds

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
    if let Some(app) = apps_map::get_apps().iter().find(|app| app.token == *token) {
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
/// This is throttled to only update once every 5 minutes to reduce database writes
pub fn track_last_seen_from_token(token: String) {
    use crate::db::Ad4mDb;

    if let Some(user_email) = user_email_from_token(token) {
        // Check if we should update (throttle to every 5 minutes)
        let should_update = Ad4mDb::with_global_instance(|db| {
            if let Ok(user) = db.get_user(&user_email) {
                if let Some(last_seen) = user.last_seen {
                    let five_min_ago = (chrono::Utc::now().timestamp() - 300) as i32;
                    let should_update = last_seen < five_min_ago;
                    log::trace!("last_seen tracking for {}: last_seen={}, five_min_ago={}, should_update={}", 
                        user_email, last_seen, five_min_ago, should_update);
                    should_update
                } else {
                    log::debug!("last_seen tracking for {}: never seen before, updating now", user_email);
                    true // Never updated, do it now
                }
            } else {
                log::warn!("last_seen tracking: user {} not found in database", user_email);
                false // User not found
            }
        });

        if should_update {
            log::debug!("Updating last_seen for user: {}", user_email);
            if let Err(e) = Ad4mDb::with_global_instance(|db| db.update_user_last_seen(&user_email)) {
                log::error!("Failed to update last_seen for user {}: {:?}", user_email, e);
            }
        }
    }
}

pub fn capabilities_from_token(
    token: String,
    admin_credential: Option<String>,
) -> Result<Vec<Capability>, String> {
    match admin_credential {
        Some(admin_credential) => {
            if token == admin_credential {
                return Ok(vec![ALL_CAPABILITY.clone()]);
            }
        }
        None => {
            if token.is_empty() {
                return Ok(vec![ALL_CAPABILITY.clone()]);
            }
        }
    }

    if token.is_empty() {
        // For empty tokens, check if multi-user mode is enabled
        // If so, allow user creation and login operations
        use crate::db::Ad4mDb;
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if multi_user_enabled {
            return Ok(vec![
                AGENT_AUTH_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY.clone(),
                RUNTIME_USER_MANAGEMENT_READ_CAPABILITY.clone(),
            ]);
        }

        return Ok(vec![AGENT_AUTH_CAPABILITY.clone()]);
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

    let auth = get_request(&auth_key)?.ok_or("Can't find permitted request")?;

    let cap_token = token::generate_jwt(
        auth.app_name.clone(),
        DEFAULT_TOKEN_VALID_PERIOD,
        auth.clone(),
    )
    .map_err(|e| e.to_string())?;

    remove_request(&auth_key)?;

    apps_map::insert_app(
        request_id.clone(),
        AuthInfoExtended {
            request_id: request_id.clone(),
            auth,
        },
        cap_token.clone(),
    )?;

    get_global_pubsub()
        .await
        .publish(&APPS_CHANGED, &String::from(""))
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
}
