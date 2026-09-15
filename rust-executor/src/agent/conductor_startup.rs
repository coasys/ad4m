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
use std::time::Duration;

use log::{error, info};
use tokio::sync::oneshot;

use crate::agent::AgentService;
use crate::holochain_service::{
    maybe_get_holochain_service, HolochainService, HolochainServiceInterface, LocalConductorConfig,
};
use crate::languages::LanguageController;
use crate::pubsub::{get_global_pubsub, AGENT_STATUS_CHANGED_TOPIC};

/// Set while a startup task is running. A second generate/unlock arriving before the
/// conductor is up would otherwise see no Holochain service and start a second conductor
/// on the same data path, or load the same languages twice concurrently. Cleared when the
/// task ends, so a later unlock (after a lock, or after a failed start) runs the startup
/// again, as it did when this was inline.
static STARTUP_IN_FLIGHT: AtomicBool = AtomicBool::new(false);

/// Set only while `HolochainService::init` is running — what
/// `holochain_service_once_started` waits on.
static CONDUCTOR_STARTING: AtomicBool = AtomicBool::new(false);

/// The Holochain service, waiting for it while a startup task is bringing the conductor up.
///
/// For callers that took `maybe_get_holochain_service()` to mean "not running" because the
/// conductor used to be up by the time unlock replied. They now wait out the start instead
/// of failing in the seconds after unlock, and still get `None` at once when nothing is
/// starting it (a locked agent, Holochain disabled, or a start that failed).
pub async fn holochain_service_once_started() -> Option<HolochainServiceInterface> {
    loop {
        if let Some(service) = maybe_get_holochain_service().await {
            return Some(service);
        }
        if !CONDUCTOR_STARTING.load(Ordering::SeqCst) {
            return maybe_get_holochain_service().await;
        }
        tokio::time::sleep(Duration::from_millis(200)).await;
    }
}

/// Claims the in-flight flag. `false` means a startup task is already running.
fn claim(flag: &AtomicBool) -> bool {
    flag.compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
}

/// A started conductor startup, waiting to be told the core system languages are loaded.
///
/// The link and installed languages load only after that: loading installed languages skips
/// the system ones by their registered addresses, which exist only once the core languages
/// finish, so running both at once could load the agent language a second time and tear
/// down the instance being loaded. Dropping this without calling
/// `core_languages_loaded` (an error path) lets the task carry on.
pub struct ConductorStartup {
    core_loaded: Option<oneshot::Sender<()>>,
}

impl ConductorStartup {
    pub fn core_languages_loaded(mut self) {
        if let Some(tx) = self.core_loaded.take() {
            let _ = tx.send(());
        }
    }
}

/// Start the conductor (unless it is already running) in the background, and once the
/// caller reports the core system languages loaded, the link and installed languages.
/// Call this before loading the core languages. Failures are logged and announced as an
/// `agent-status-changed` event carrying the error, since the reply may already have gone.
pub fn spawn_conductor_startup(passphrase: String) -> ConductorStartup {
    if !claim(&STARTUP_IN_FLIGHT) {
        info!("Holochain startup already in progress; not starting another");
        return ConductorStartup { core_loaded: None };
    }
    // Before spawning, so a request arriving before the task's first poll already waits
    // rather than seeing no service and no start.
    CONDUCTOR_STARTING.store(true, Ordering::SeqCst);
    let (core_loaded_tx, core_loaded_rx) = oneshot::channel();

    tokio::spawn(async move {
        let mut errors: Vec<String> = Vec::new();

        if maybe_get_holochain_service().await.is_none() {
            info!("Holochain service not initialized. Initializing...");
            let config = crate::config::get_global_config();
            let hc_config = LocalConductorConfig::from_ad4m_config(&config, passphrase);
            let result = HolochainService::init(hc_config).await;
            CONDUCTOR_STARTING.store(false, Ordering::SeqCst);
            if let Err(e) = result {
                error!("Error initializing Holochain: {:?}", e);
                errors.push(format!("Holochain init failed: {}", e));
            } else {
                info!("Holochain init complete");
            }
        } else {
            CONDUCTOR_STARTING.store(false, Ordering::SeqCst);
        }

        // Err means the handle was dropped without reporting, which is also "go ahead".
        let _ = core_loaded_rx.await;

        let language_language_only = crate::config::get_global_config()
            .language_language_only
            .unwrap_or(false);
        if !language_language_only {
            LanguageController::global_instance()
                .load_link_and_installed_languages()
                .await;
        }

        STARTUP_IN_FLIGHT.store(false, Ordering::SeqCst);

        if errors.is_empty() {
            info!("Holochain and link languages ready");
            return;
        }

        let mut status = AgentService::with_global_instance(|agent_service| agent_service.dump());
        status.error = Some(errors.join("; "));
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
    });

    ConductorStartup {
        core_loaded: Some(core_loaded_tx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_second_claim_is_refused_until_released() {
        let flag = AtomicBool::new(false);
        assert!(claim(&flag));
        assert!(!claim(&flag));
        flag.store(false, Ordering::SeqCst);
        assert!(claim(&flag));
    }
}
