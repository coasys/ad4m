//! Starting the Holochain conductor, and the languages that need it, after the main
//! agent is generated or unlocked — without holding up the reply.
//!
//! `agent.generate` and `agent.unlock` used to await the conductor before replying. The
//! conductor starts one cell per installed app in turn, and each rebuilds its DHT summary
//! from its local store first, so the wait grows with every neighbourhood joined (an
//! account with 18 took ~19s offline, 8s of it one cell). Nothing in the reply needs it:
//! perspectives answer from their local store, the agent, neighbourhood and perspective
//! languages don't touch Holochain, and a link language calling into Holochain before the
//! conductor is up already waits for it in `get_holochain_service()`.

use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use log::{error, info};

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

/// Start the conductor (unless it is already running) and then the link and installed
/// languages, in the background. Failures are logged and announced as an
/// `agent-status-changed` event carrying the error, since the reply has already gone.
pub fn spawn_conductor_startup(passphrase: String) {
    if !claim(&STARTUP_IN_FLIGHT) {
        info!("Holochain startup already in progress; not starting another");
        return;
    }
    // Before spawning, so a request arriving between the reply and the task's first poll
    // already waits rather than seeing no service and no start.
    CONDUCTOR_STARTING.store(true, Ordering::SeqCst);

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
