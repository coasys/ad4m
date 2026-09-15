//! The system-language loading steps that wait for the Holochain conductor.
//!
//! Known link languages and most installed languages (every neighbourhood's link
//! language) install or look up a Holochain app in their constructor, and that call
//! waits in `get_holochain_service()` until the conductor is up. Kept apart from
//! `load_core_system_languages` so a caller that must not wait for the conductor can
//! load the core languages alone and run these once it has started.

use log::{info, warn};

use super::LanguageController;
use crate::runtime_service::RuntimeService;

impl LanguageController {
    /// Preload the known link languages in parallel, then load any other installed
    /// languages from disk. Failures are logged per language and never abort the rest.
    pub async fn load_link_and_installed_languages(&self) {
        let known_link_languages =
            RuntimeService::with_global_instance(|rs| rs.get_know_link_languages());
        if !known_link_languages.is_empty() {
            info!(
                "Installing {} known link languages in parallel",
                known_link_languages.len()
            );
            let results = futures::future::join_all(
                known_link_languages
                    .iter()
                    .map(|addr| self.install_language_from_address(addr, true)),
            )
            .await;
            for (addr, result) in known_link_languages.iter().zip(results) {
                if let Err(e) = result {
                    warn!("Failed to preload known link language {}: {}", addr, e);
                }
            }
        }

        if let Err(e) = self.load_installed_languages().await {
            warn!("Failed to load installed languages: {}", e);
        }
    }
}
