//! Loading the link and installed languages, split by whether each one actually
//! uses Holochain.
//!
//! A language that uses Holochain (registers a hApp or calls a zome in its
//! constructor) waits in `get_holochain_service()` until the conductor is up, so
//! its load must not start earlier. Which languages those are is not guessed from
//! their role: each bundle is inspected with `holochain_usage` before loading.
//! Languages whose bundles show no Holochain usage load immediately — in
//! parallel with the conductor still booting — and only the detected
//! Holochain-users are deferred until `conductor_startup.rs` reports the
//! conductor ready. A bundle that cannot be read or positively classified is
//! treated as Holochain-using (deferred), so a classifier miss can only delay a
//! load, never break one that worked before.
//!
//! Kept apart from `load_core_system_languages` so a caller that must not wait
//! for the conductor can load the core languages alone and run these once the
//! conductor startup is underway.

use std::path::PathBuf;

use log::{info, warn};

use super::{holochain_usage, LanguageController};
use crate::runtime_service::RuntimeService;

/// A language whose bundle uses Holochain, fetched and saved to disk but not
/// loaded yet. `load_deferred_holochain_languages` loads these once the
/// conductor is up.
pub struct DeferredLanguage {
    pub address: String,
    pub bundle_path: PathBuf,
    pub is_system_language: bool,
}

impl LanguageController {
    /// Fetch the known link languages and scan the installed languages, load every
    /// one whose bundle does not use Holochain, and return the ones that do for
    /// loading once the conductor is up. Failures are logged per language and
    /// never abort the rest.
    pub async fn load_link_and_installed_languages(&self) -> Vec<DeferredLanguage> {
        let mut deferred = Vec::new();

        let known_link_languages =
            RuntimeService::with_global_instance(|rs| rs.get_know_link_languages());
        if !known_link_languages.is_empty() {
            info!(
                "Fetching {} known link languages in parallel",
                known_link_languages.len()
            );
            let results = futures::future::join_all(
                known_link_languages
                    .iter()
                    .map(|addr| self.ensure_language_bundle_on_disk(addr)),
            )
            .await;

            let mut load_now = Vec::new();
            for (addr, result) in known_link_languages.iter().zip(results) {
                match result {
                    Err(e) => warn!("Failed to fetch known link language {}: {}", addr, e),
                    Ok(bundle_path) => {
                        if self.is_language_loaded(addr).await {
                            continue;
                        }
                        if holochain_usage::bundle_file_uses_holochain(&bundle_path) {
                            deferred.push(DeferredLanguage {
                                address: addr.clone(),
                                bundle_path,
                                is_system_language: true,
                            });
                        } else {
                            load_now.push((addr.clone(), bundle_path));
                        }
                    }
                }
            }

            if !load_now.is_empty() {
                info!(
                    "Preloading {} known link languages that don't use Holochain in parallel",
                    load_now.len()
                );
                let results = futures::future::join_all(
                    load_now
                        .iter()
                        .map(|(_, bundle_path)| self.load_language(bundle_path.clone(), true)),
                )
                .await;
                for ((addr, _), result) in load_now.iter().zip(results) {
                    if let Err(e) = result {
                        warn!("Failed to preload known link language {}: {}", addr, e);
                    }
                }
            }
        }

        for (address, bundle_path) in self.installed_language_bundles().await {
            if holochain_usage::bundle_file_uses_holochain(&bundle_path) {
                deferred.push(DeferredLanguage {
                    address,
                    bundle_path,
                    is_system_language: false,
                });
                continue;
            }
            info!("Loading installed language from disk: {}", address);
            match self.load_language(bundle_path, false).await {
                Ok(_) => {
                    info!(
                        "Successfully loaded installed language: {}",
                        self.language_label(&address).await
                    );
                }
                Err(e) => {
                    warn!("Failed to load language {}: {}", address, e);
                }
            }
        }

        if !deferred.is_empty() {
            info!(
                "Deferring {} Holochain-using languages until the conductor is up: {:?}",
                deferred.len(),
                deferred
                    .iter()
                    .map(|d| d.address.as_str())
                    .collect::<Vec<_>>()
            );
        }
        deferred
    }

    /// Load the Holochain-using languages deferred by
    /// `load_link_and_installed_languages`. Call only once the conductor is up —
    /// these bundles wait for it in their constructors. Failures are logged per
    /// language and never abort the rest.
    pub async fn load_deferred_holochain_languages(&self, deferred: Vec<DeferredLanguage>) {
        if deferred.is_empty() {
            return;
        }
        info!(
            "Loading {} deferred Holochain-using languages in parallel",
            deferred.len()
        );
        let results = futures::future::join_all(deferred.iter().map(|language| {
            self.load_language(language.bundle_path.clone(), language.is_system_language)
        }))
        .await;
        for (language, result) in deferred.iter().zip(results) {
            match result {
                Ok(_) => info!(
                    "Loaded deferred Holochain-using language: {}",
                    self.language_label(&language.address).await
                ),
                Err(e) => warn!(
                    "Failed to load deferred Holochain-using language {}: {}",
                    language.address, e
                ),
            }
        }
    }
}
