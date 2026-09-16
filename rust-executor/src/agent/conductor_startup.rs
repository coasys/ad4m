//! Starting the Holochain conductor, and the languages that need it, when the main agent is
//! generated or unlocked — without making the reply wait for it unless it has to.
//!
//! `agent.generate` and `agent.unlock` used to await the conductor before loading any
//! language. The conductor starts one cell per installed app in turn, and each rebuilds its
//! DHT summary from its local store first, so the wait grows with every link language
//! installed (18 apps took ~19s offline, 8s of it one cell).
//!
//! The handlers now start the conductor first and load the core system languages alongside
//! it. Whether the reply then waits for the conductor depends on the seed. The default seed's
//! agent, neighbourhood and perspective languages don't touch Holochain, so the reply goes
//! out while cells are still starting. A seed whose system languages do (the integration
//! test seed's agent language registers a DNA in its constructor) waits for the conductor
//! inside `get_holochain_service()`, as before. That's why the conductor must be started
//! before those languages load, not after: their constructors would otherwise wait for a
//! conductor that starts only once they finish.

use std::sync::atomic::{AtomicBool, Ordering};

use log::{error, info, warn};
use tokio::sync::oneshot;

use crate::agent::AgentService;
use crate::holochain_service::{
    maybe_get_holochain_service, ConductorStarting, HolochainService, LocalConductorConfig,
};
use crate::languages::error::LanguageError;
use crate::languages::LanguageController;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC};

/// Set while a startup task is running. A second generate/unlock arriving before the
/// conductor is up would otherwise see no Holochain service and start a second conductor
/// on the same data path. Held through `InFlight`, so it clears when the task ends however
/// it ends, and a later unlock (after a lock, or after a failed start) runs the startup again.
static STARTUP_IN_FLIGHT: AtomicBool = AtomicBool::new(false);

/// Serializes `load_core_system_languages`. It loads each core language unconditionally and
/// registers the runtime only once the constructor has run, so two concurrent generate/unlock
/// calls would each build a runtime for the same language, and the later one would replace
/// the earlier one without tearing it down.
static CORE_LANGUAGE_LOAD: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// A claim on a flag, released when dropped.
struct InFlight(&'static AtomicBool);

impl InFlight {
    /// `None` means the flag is already claimed.
    fn claim(flag: &'static AtomicBool) -> Option<Self> {
        flag.compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .ok()
            .map(|_| Self(flag))
    }
}

impl Drop for InFlight {
    fn drop(&mut self) {
        self.0.store(false, Ordering::SeqCst);
    }
}

/// A conductor startup, waiting for the caller to load the core system languages.
///
/// The link and installed languages load only after that load has succeeded: loading
/// installed languages skips the system ones by their registered addresses, which exist only
/// once the core languages finish, so running both at once could load the agent language a
/// second time and tear down the instance being loaded. If the core load fails, or this is
/// dropped before it finishes (the request was cancelled), the task skips them; they load on
/// first use instead.
pub struct ConductorStartup {
    /// `None` when another startup was already in flight: this caller is a follower.
    core_loaded: Option<oneshot::Sender<bool>>,
}

impl ConductorStartup {
    /// Load the core system languages, one caller at a time, then let the startup task load
    /// the link and installed languages.
    ///
    /// A follower (a generate/unlock that arrived while another startup was in flight) skips
    /// the load when the core languages are already registered. `load_core_system_languages`
    /// reloads the language language unconditionally, and the in-flight task may by then be
    /// loading link languages through it.
    pub async fn load_core_languages(
        mut self,
        language_language_only: bool,
    ) -> Result<(), LanguageError> {
        let result = {
            let _one_at_a_time = CORE_LANGUAGE_LOAD.lock().await;
            if self.core_loaded.is_none() && core_languages_registered(language_language_only).await
            {
                info!("Core system languages already loaded by the startup in flight");
                Ok(())
            } else {
                LanguageController::global_instance()
                    .load_core_system_languages(language_language_only)
                    .await
            }
        };
        if let Some(tx) = self.core_loaded.take() {
            let _ = tx.send(result.is_ok());
        }
        result
    }
}

async fn core_languages_registered(language_language_only: bool) -> bool {
    let controller = LanguageController::global_instance();
    if controller.get_language_language().await.is_err() {
        return false;
    }
    language_language_only
        || (controller.get_agent_language().await.is_ok()
            && controller.get_neighbourhood_language().await.is_ok()
            && controller.get_perspective_language().await.is_ok())
}

/// Start the conductor (unless it is already running) in the background, and once the
/// caller has loaded the core system languages, the link and installed languages.
/// Call this before loading the core languages. A failed start is logged and announced as an
/// `agent-status-changed` event carrying the error, since the reply may already have gone.
pub fn spawn_conductor_startup(passphrase: String) -> ConductorStartup {
    let Some(in_flight) = InFlight::claim(&STARTUP_IN_FLIGHT) else {
        info!("Holochain startup already in progress; not starting another");
        return ConductorStartup { core_loaded: None };
    };
    // Before spawning, so a request arriving before the task's first poll already waits
    // rather than seeing no service and no start.
    let starting = ConductorStarting::begin();
    let (core_loaded_tx, core_loaded_rx) = oneshot::channel();

    tokio::spawn(async move {
        if maybe_get_holochain_service().await.is_none() {
            info!("Holochain service not initialized. Initializing...");
            let config = crate::config::get_global_config();
            let hc_config = LocalConductorConfig::from_ad4m_config(&config, passphrase);
            let result = HolochainService::init(hc_config).await;
            drop(starting);
            if let Err(e) = result {
                error!("Error initializing Holochain: {:?}", e);
                // Skip the link and installed languages: without a conductor each would wait
                // 120s for it and panic, one after another, while this task kept the in-flight
                // flag and so refused a retried unlock. They load on first use instead, and
                // a successful retry loads the rest. Released before announcing, so a client
                // that retries on the event isn't refused.
                drop(in_flight);
                announce_startup_failure(format!("Holochain init failed: {}", e)).await;
                return;
            }
            info!("Holochain init complete");
        } else {
            drop(starting);
        }

        // `false` is a failed core load; `Err` is the handle dropped before it finished.
        if !matches!(core_loaded_rx.await, Ok(true)) {
            warn!("Core system languages did not load; skipping link and installed languages");
            return;
        }

        let language_language_only = crate::config::get_global_config()
            .language_language_only
            .unwrap_or(false);
        if !language_language_only {
            LanguageController::global_instance()
                .load_link_and_installed_languages()
                .await;
        }

        drop(in_flight);
        info!("Holochain and link languages ready");
    });

    ConductorStartup {
        core_loaded: Some(core_loaded_tx),
    }
}

async fn announce_startup_failure(message: String) {
    let mut status = AgentService::with_global_instance(|agent_service| agent_service.dump());
    status.error = Some(message);
    get_global_pubsub()
        .await
        .publish(
            &AGENT_STATUS_CHANGED_TOPIC,
            &serde_json::to_string(&status).unwrap_or_else(|e| {
                error!("Failed to serialize agent for pubsub: {e}");
                String::new()
            }),
        )
        .await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_second_claim_is_refused_until_the_first_is_dropped() {
        static FLAG: AtomicBool = AtomicBool::new(false);
        let first = InFlight::claim(&FLAG).expect("first claim");
        assert!(InFlight::claim(&FLAG).is_none());
        drop(first);
        assert!(InFlight::claim(&FLAG).is_some());
    }

    #[tokio::test]
    async fn a_claim_is_released_when_its_task_panics() {
        static FLAG: AtomicBool = AtomicBool::new(false);
        let claim = InFlight::claim(&FLAG).expect("claim");
        let task = tokio::spawn(async move {
            let _claim = claim;
            panic!("language load panicked");
        });
        assert!(task.await.is_err());
        assert!(InFlight::claim(&FLAG).is_some());
    }
}
