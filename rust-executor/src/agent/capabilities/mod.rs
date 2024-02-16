pub mod defs;
pub mod types;
pub mod token;

pub use defs::*;
pub use types::*;
pub use token::*;

use crate::pubsub::{EXCEPTION_OCCURRED_TOPIC, get_global_pubsub};
use crate::graphql::graphql_types::*;

pub fn check_capability(capabilities: &Result<Vec<Capability>, String>, expected: &Capability) -> Result<(), String> {
    let capabilities = capabilities.clone()?;
    let custom_cap_match = |cap: &Capability, expected: &Capability| -> bool {
        if cap.with.domain != WILD_CARD && cap.with.domain != expected.with.domain {
            return false;
        }

        if !cap.with.pointers.contains(&WILD_CARD.to_string()) &&
           expected.with.pointers.iter().any(|p| !cap.with.pointers.contains(p)) {
            return false;
        }

        if !cap.can.contains(&WILD_CARD.to_string()) &&
           expected.can.iter().any(|c| !cap.can.contains(c)) {
            return false;
        }

        true
    };

    if !capabilities.iter().any(|cap| custom_cap_match(cap, expected)) {
        return Err(format!(
            "Capability is not matched, you have capabilities: {:?}, expected: {:?}",
            capabilities, expected
        ));
    }

    Ok(())
}

pub fn capabilities_from_token(token: String, admin_credential: Option<String>) -> Result<Vec<Capability>, String> {
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
  
    if token == "" {
        return Ok(vec![AGENT_AUTH_CAPABILITY.clone()]);
    }
  
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
        message: format!("{} is waiting for authentication, open the ADAM Launcher for more information.", app_name),
        r#type: ExceptionType::CapabilityRequested, 
        addon: Some(serde_json::to_string(&auth_extended).unwrap()),
    };

    get_global_pubsub()
        .await
        .publish(&EXCEPTION_OCCURRED_TOPIC, &serde_json::to_string(&exception_info).unwrap())
        .await;

    request_id
}


pub struct App {
    token: String,
    revoked: bool,
}

pub fn check_token_authorized(apps: &[App], token: &str, is_admin_credential: bool) -> Result<(), String> {
    if !is_admin_credential {
        if !apps.is_empty() && !token.is_empty() {
            let filtered_apps: Vec<&App> = apps.iter().filter(|app| app.token == token).collect();

            if filtered_apps.is_empty() {
                return Err("Unauthorized access".to_string());
            } else {
                let no_revoked: Vec<&App> = filtered_apps.into_iter().filter(|app| !app.revoked).collect();
                if no_revoked.is_empty() {
                    return Err("Unauthorized access".to_string());
                }
            }
        }
    }
    Ok(())
}




pub const DEFAULT_TOKEN_VALID_PERIOD: i64 = 180 * 24 * 60 * 60; // 180 days in seconds

#[allow(dead_code)]
pub fn gen_random_digits() -> String {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    rng.gen_range(100000..1000000).to_string()
}

#[allow(dead_code)]
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
        assert!(check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_PERMIT_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_all_capability_can_request_agent_status() {
        assert!(check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_READ_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_all_capability_can_mutate_the_agent() {
        assert!(check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_CREATE_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_request_the_agent_status() {
        assert!(check_capability(&Ok(vec![AGENT_AUTH_CAPABILITY.clone()]), &AGENT_READ_CAPABILITY).is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_mutate_the_agent() {
        assert!(check_capability(&Ok(vec![AGENT_AUTH_CAPABILITY.clone()]), &AGENT_CREATE_CAPABILITY).is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_can_request_an_auth() {
        assert!(check_capability(&Ok(vec![AGENT_AUTH_CAPABILITY.clone()]), &AGENT_AUTH_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_agent_read_capability_can_request_the_agent_status() {
        assert!(check_capability(&Ok(vec![AGENT_READ_CAPABILITY.clone()]), &AGENT_READ_CAPABILITY).is_ok());
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
        assert!(rand.len() == 6 && rand.chars().all(|c| c.is_digit(10)));
    }

    #[test]
    fn gen_request_key_joins_the_request_id_and_rand() {
        let key = gen_request_key("my-request-id", "123456");
        assert_eq!(key, "my-request-id-123456");
    }
}

