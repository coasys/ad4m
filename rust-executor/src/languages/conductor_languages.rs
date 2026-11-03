//! The system-language loading steps that run after the Holochain conductor is up.
//!
//! Languages that use Holochain call into `holochain_service_once_started()` at each op
//! boundary, so the conductor readiness wait is scoped to each operation rather than to the
//! load step. These languages are still loaded after the conductor starts — not because they
//! must wait for it, but because loading installed languages skips the core system languages
//! by their registered addresses, which only exist after `load_core_system_languages`
//! completes. Kept apart from `load_core_system_languages` so a caller that must reply
//! before the conductor is up can load the core languages alone and run these in the
//! background.

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
