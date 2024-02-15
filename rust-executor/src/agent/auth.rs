use log::debug;
use super::jwt::{Capability, Resource};

//pub type Capabilities = Vec<Capability>;


const WILD_CARD: &str = "*";

// capabilities operations
const READ: &str = "READ";
const CREATE: &str = "CREATE";
const UPDATE: &str = "UPDATE";
const DELETE: &str = "DELETE";
const SUBSCRIBE: &str = "SUBSCRIBE";

// capabilities domains
const AGENT: &str = "agent";
const EXPRESSION: &str = "expression";
const LANGUAGE: &str = "language";
const PERSPECTIVE: &str = "perspective";
const NEIGHBOURHOOD: &str = "neighbourhood";
const RUNTIME: &str = "runtime";
const RUNTIME_TRUSTED_AGENTS: &str = "runtime.trusted_agents";
const RUNTIME_KNOWN_LINK_LANGUAGES: &str = "runtime.known_link_languages";
const RUNTIME_FRIENDS: &str = "runtime.friends";
const RUNTIME_MESSAGES: &str = "runtime.messages";

// admin capabilities
lazy_static! {
    pub static ref ALL_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: WILD_CARD.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![WILD_CARD.to_string()],
    };
    // agent related capabilities
    pub static ref AGENT_AUTH_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["AUTHENTICATE".to_string()],
    };
    pub static ref AGENT_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };
    pub static ref AGENT_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };
    pub static ref AGENT_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };
    pub static ref AGENT_LOCK_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["LOCK".to_string()],
    };
    pub static ref AGENT_UNLOCK_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["UNLOCK".to_string()],
    };
    pub static ref AGENT_PERMIT_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["PERMIT".to_string()],
    };
    pub static ref AGENT_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };
    pub static ref AGENT_SIGN_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["SIGN".to_string()],
    };

    // expression related capabilities
    pub static ref EXPRESSION_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: EXPRESSION.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };
    pub static ref EXPRESSION_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: EXPRESSION.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };
    pub static ref EXPRESSION_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: EXPRESSION.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };

    // language related capabilities
    pub static ref LANGUAGE_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: LANGUAGE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };
    pub static ref LANGUAGE_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: LANGUAGE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };
    pub static ref LANGUAGE_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: LANGUAGE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };
    pub static ref LANGUAGE_DELETE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: LANGUAGE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![DELETE.to_string()],
    };

    // perspective related capabilities
    pub static ref PERSPECTIVE_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: PERSPECTIVE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };
}

#[allow(dead_code)]
pub fn perspective_query_capability(pointers: Vec<String>) -> Capability {
    Capability {
        with: Resource {
            domain: PERSPECTIVE.to_string(),
            pointers,
        },
        can: vec![READ.to_string()],
    }
}

#[allow(dead_code)]
pub fn perspective_update_capability(pointers: Vec<String>) -> Capability {
    Capability {
        with: Resource {
            domain: PERSPECTIVE.to_string(),
            pointers,
        },
        can: vec![UPDATE.to_string()],
    }
}

#[allow(dead_code)]
pub fn perspective_delete_capability(pointers: Vec<String>) -> Capability {
    Capability {
        with: Resource {
            domain: PERSPECTIVE.to_string(),
            pointers,
        },
        can: vec![DELETE.to_string()],
    }
}

lazy_static! {
    pub static ref PERSPECTIVE_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: PERSPECTIVE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };

    // neighbourhood related capabilities
    pub static ref NEIGHBOURHOOD_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: NEIGHBOURHOOD.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref NEIGHBOURHOOD_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: NEIGHBOURHOOD.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref NEIGHBOURHOOD_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: NEIGHBOURHOOD.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };

    // runtime related capabilities
    pub static ref RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_TRUSTED_AGENTS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_TRUSTED_AGENTS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_TRUSTED_AGENTS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![DELETE.to_string()],
    };

    pub static ref RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_KNOWN_LINK_LANGUAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_KNOWN_LINK_LANGUAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_KNOWN_LINK_LANGUAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![DELETE.to_string()],
    };

    pub static ref RUNTIME_FRIENDS_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_FRIENDS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_FRIENDS_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_FRIENDS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_FRIENDS_DELETE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_FRIENDS.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![DELETE.to_string()],
    };

    pub static ref RUNTIME_FRIEND_STATUS_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: "runtime.friend_status".to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_MY_STATUS_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: "runtime.my_status".to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };

    pub static ref RUNTIME_HC_AGENT_INFO_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: "runtime.hc_agent_info".to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: "runtime.hc_agent_info".to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_MESSAGES_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_MESSAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_MESSAGES_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_MESSAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_MESSAGES_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_MESSAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };

    pub static ref RUNTIME_QUIT_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["QUIT".to_string()],
    };

    pub static ref RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: "runtime.exception".to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };
}

pub struct App {
    token: String,
    revoked: bool,
}

pub fn capabilities_from_token(token: String, admin_credential: Option<String>) -> Vec<Capability> {
    if let Some(admin_credential) = admin_credential {
        if token == admin_credential {
            return vec![ALL_CAPABILITY.clone()];
        }
    }
  
    if token == "" {
        return vec![AGENT_AUTH_CAPABILITY.clone()];
    }
  
    match super::jwt::decode_jwt(token) {
        Ok(claims) => {
            if claims.capabilities.capabilities.is_none() {
                vec![AGENT_AUTH_CAPABILITY.clone()]
            } else {
                claims.capabilities.capabilities.unwrap()
            }
        }
        Err(e) => {
            debug!("Error decoding capability token: {:?}", e);
            vec![AGENT_AUTH_CAPABILITY.clone()]
        }
    }
}

#[allow(dead_code)]
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

pub fn check_capability(capabilities: &[Capability], expected: &Capability) -> Result<(), String> {
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

#[allow(dead_code)]
pub const DEFAULT_TOKEN_VALID_PERIOD: i64 = 180 * 24 * 60 * 60; // 180 days in seconds


#[allow(dead_code)]
pub struct AuthInfoExtended {
    pub request_id: String,
    pub auth: AuthInfo,
}

#[allow(dead_code)]
pub struct AuthInfo {
    pub app_name: String,
    pub app_desc: String,
    pub app_domain: String,
    pub app_url: Option<String>,
    pub app_icon_path: Option<String>,
    pub capabilities: Option<Vec<Capability>>,
}

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
        assert!(check_capability(&[ALL_CAPABILITY.clone()], &AGENT_PERMIT_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_all_capability_can_request_agent_status() {
        assert!(check_capability(&[ALL_CAPABILITY.clone()], &AGENT_READ_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_all_capability_can_mutate_the_agent() {
        assert!(check_capability(&[ALL_CAPABILITY.clone()], &AGENT_CREATE_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_request_the_agent_status() {
        assert!(check_capability(&[AGENT_AUTH_CAPABILITY.clone()], &AGENT_READ_CAPABILITY).is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_mutate_the_agent() {
        assert!(check_capability(&[AGENT_AUTH_CAPABILITY.clone()], &AGENT_CREATE_CAPABILITY).is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_can_request_an_auth() {
        assert!(check_capability(&[AGENT_AUTH_CAPABILITY.clone()], &AGENT_AUTH_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_agent_read_capability_can_request_the_agent_status() {
        assert!(check_capability(&[AGENT_READ_CAPABILITY.clone()], &AGENT_READ_CAPABILITY).is_ok());
    }

    #[test]
    fn agent_with_perspective_query_capability_can_query_a_perspective() {
        let query_capability = perspective_query_capability(vec!["*".to_string()]);
        let expected_capability = perspective_query_capability(vec!["123".to_string()]);
        assert!(check_capability(&[query_capability], &expected_capability).is_ok());
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

