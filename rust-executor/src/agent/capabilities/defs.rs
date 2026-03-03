use super::types::{Capability, Resource};
pub const WILD_CARD: &str = "*";

// capabilities operations
pub const READ: &str = "READ";
pub const CREATE: &str = "CREATE";
pub const UPDATE: &str = "UPDATE";
pub const DELETE: &str = "DELETE";
pub const SUBSCRIBE: &str = "SUBSCRIBE";
pub const PROMPT: &str = "PROMPT";
pub const TRANSCRIBE: &str = "TRANSCRIBE";
pub const VERIFY: &str = "VERIFY";

// capabilities domains
pub const AGENT: &str = "agent";
pub const EXPRESSION: &str = "expression";
pub const LANGUAGE: &str = "language";
pub const PERSPECTIVE: &str = "perspective";
pub const NEIGHBOURHOOD: &str = "neighbourhood";
pub const RUNTIME: &str = "runtime";
pub const RUNTIME_TRUSTED_AGENTS: &str = "runtime.trusted_agents";
pub const RUNTIME_KNOWN_LINK_LANGUAGES: &str = "runtime.known_link_languages";
pub const RUNTIME_FRIENDS: &str = "runtime.friends";
pub const RUNTIME_MESSAGES: &str = "runtime.messages";
pub const RUNTIME_USER_MANAGEMENT: &str = "runtime.user_management";
pub const AI: &str = "artificial intelligence";

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

    // AI related capabilities
    pub static ref AI_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref AI_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref AI_UPDATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![UPDATE.to_string()],
    };

    pub static ref AI_DELETE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![DELETE.to_string()],
    };

    pub static ref AI_PROMPT_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![PROMPT.to_string()],
    };

    pub static ref AI_TRANSCRIBE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![TRANSCRIBE.to_string()],
    };

    pub static ref AI_ALL_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: AI.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![WILD_CARD.to_string()],
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

    // User management capabilities for multi-user mode

    // Allows reading whether multi-user mode is enabled - given to unauthenticated users
    // This allows ad4m-connect to detect multi-user mode before authentication
    // without exposing user enumeration
    pub static ref RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_USER_MANAGEMENT.to_string(),
            pointers: vec!["enabled".to_string()],
        },
        can: vec![READ.to_string()],
    };

    // Allows user login - given to unauthenticated users in multi-user mode
    pub static ref RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_USER_MANAGEMENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec!["LOGIN".to_string()],
    };

    pub static ref RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_USER_MANAGEMENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![CREATE.to_string()],
    };

    pub static ref RUNTIME_USER_MANAGEMENT_READ_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_USER_MANAGEMENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![READ.to_string()],
    };

    pub static ref RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY: Capability = Capability {
        with: Resource {
            domain: RUNTIME_USER_MANAGEMENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![VERIFY.to_string()],
    };
}

/// Returns the default set of capabilities for a regular user.
/// This includes all capabilities needed for normal app usage (perspectives, languages, expressions, etc.)
/// but excludes admin capabilities like AI model management (CREATE/UPDATE/DELETE) and runtime config changes.
pub fn get_user_default_capabilities() -> Vec<Capability> {
    vec![
        // Agent capabilities - allow users to manage their own agent
        AGENT_AUTH_CAPABILITY.clone(),
        AGENT_READ_CAPABILITY.clone(),
        AGENT_CREATE_CAPABILITY.clone(),
        AGENT_UPDATE_CAPABILITY.clone(),
        AGENT_SIGN_CAPABILITY.clone(),
        AGENT_SUBSCRIBE_CAPABILITY.clone(),
        // Note: Excluding LOCK, UNLOCK, PERMIT as these are admin operations

        // Expression capabilities - allow users to create and manage expressions
        EXPRESSION_READ_CAPABILITY.clone(),
        EXPRESSION_CREATE_CAPABILITY.clone(),
        EXPRESSION_UPDATE_CAPABILITY.clone(),
        // Language capabilities - allow users to use and manage languages
        LANGUAGE_READ_CAPABILITY.clone(),
        LANGUAGE_CREATE_CAPABILITY.clone(),
        LANGUAGE_UPDATE_CAPABILITY.clone(),
        LANGUAGE_DELETE_CAPABILITY.clone(),
        // Perspective capabilities - allow users to manage their perspectives
        PERSPECTIVE_CREATE_CAPABILITY.clone(),
        perspective_query_capability(vec![WILD_CARD.to_string()]),
        perspective_update_capability(vec![WILD_CARD.to_string()]),
        perspective_delete_capability(vec![WILD_CARD.to_string()]),
        PERSPECTIVE_SUBSCRIBE_CAPABILITY.clone(),
        // Neighbourhood capabilities - allow users to create and manage neighbourhoods
        NEIGHBOURHOOD_CREATE_CAPABILITY.clone(),
        NEIGHBOURHOOD_READ_CAPABILITY.clone(),
        NEIGHBOURHOOD_UPDATE_CAPABILITY.clone(),
        // Runtime capabilities for social features
        RUNTIME_TRUSTED_AGENTS_READ_CAPABILITY.clone(),
        RUNTIME_TRUSTED_AGENTS_CREATE_CAPABILITY.clone(),
        RUNTIME_TRUSTED_AGENTS_DELETE_CAPABILITY.clone(),
        RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY.clone(),
        RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY.clone(),
        RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY.clone(),
        RUNTIME_FRIENDS_READ_CAPABILITY.clone(),
        RUNTIME_FRIENDS_CREATE_CAPABILITY.clone(),
        RUNTIME_FRIENDS_DELETE_CAPABILITY.clone(),
        RUNTIME_FRIEND_STATUS_READ_CAPABILITY.clone(),
        RUNTIME_MY_STATUS_UPDATE_CAPABILITY.clone(),
        RUNTIME_HC_AGENT_INFO_READ_CAPABILITY.clone(),
        RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY.clone(),
        RUNTIME_MESSAGES_READ_CAPABILITY.clone(),
        RUNTIME_MESSAGES_CREATE_CAPABILITY.clone(),
        RUNTIME_MESSAGES_SUBSCRIBE_CAPABILITY.clone(),
        RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY.clone(),
        // Note: Excluding RUNTIME_QUIT_CAPABILITY as this is an admin operation

        // AI capabilities - allow users to use AI (prompt, transcribe) but not manage models
        AI_READ_CAPABILITY.clone(),
        AI_PROMPT_CAPABILITY.clone(),
        AI_TRANSCRIBE_CAPABILITY.clone(),
        // Note: Excluding AI_CREATE_CAPABILITY, AI_UPDATE_CAPABILITY, AI_DELETE_CAPABILITY
        // as these are admin operations for managing AI models

        // Note: Excluding RUNTIME_USER_MANAGEMENT_READ_CAPABILITY as it allows listing all users,
        // which is an admin operation. Regular users should not be able to enumerate other users.
    ]
}
