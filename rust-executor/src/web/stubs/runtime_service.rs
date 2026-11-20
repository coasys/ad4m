//! `crate::runtime_service` on wasm32: no bootstrap seed file and no language-runtime ops in
//! the web executor.
//!
//! Real: `notification_access` (the native file by path).
//! Stubbed: `RuntimeService` without a seed. Trusted agents are the agent itself plus the
//! ones stored in the database (native adds the seed's); there is no direct message language.

#[path = "../../runtime_service/notification_access.rs"]
pub(crate) mod notification_access;

use crate::agent::did;

pub struct RuntimeService;

static RUNTIME: RuntimeService = RuntimeService;

impl RuntimeService {
    /// Always there on wasm32 (native panics before `init_global_instance`).
    pub fn with_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&RuntimeService) -> R,
    {
        func(&RUNTIME)
    }

    /// Empty: no direct message language in the web executor.
    pub fn get_direct_message_language(&self) -> String {
        String::new()
    }

    pub fn get_trusted_agents(&self) -> Vec<String> {
        let mut trusted_agents = crate::db_backend::db_backend()
            .get_all_trusted_agents()
            .unwrap_or_default();
        trusted_agents.push(did());
        trusted_agents.sort();
        trusted_agents.dedup();
        trusted_agents
    }

    pub fn add_trusted_agent(&self, new_agents: Vec<String>) {
        if let Err(e) = crate::db_backend::db_backend().add_trusted_agents(new_agents) {
            log::warn!("add_trusted_agent: {e}");
        }
    }

    pub fn remove_trusted_agent(&self, agents_to_remove: Vec<String>) {
        if let Err(e) = crate::db_backend::db_backend().remove_trusted_agents(agents_to_remove) {
            log::warn!("remove_trusted_agent: {e}");
        }
    }
}
