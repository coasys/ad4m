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
use std::sync::Mutex;

use log::{error, info, warn};
use tokio::sync::watch;

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

/// The core-load outcome of the startup in flight: `None` until the caller that started it
/// has loaded the core languages, then whether that succeeded. Callers that arrive while it
/// is in flight wait on this instead of loading again. Replaced in the same critical section
/// that claims `STARTUP_IN_FLIGHT`, so they never read an earlier startup's outcome.
type CoreLoadOutcome = watch::Receiver<Option<bool>>;
static CURRENT_CORE_LOAD: Mutex<Option<CoreLoadOutcome>> = Mutex::new(None);

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
    role: Role,
}

enum Role {
    /// Started the startup in flight; runs the core load and reports its outcome.
    Leader(watch::Sender<Option<bool>>),
    /// Arrived while a startup was in flight; shares its outcome instead of loading again.
    /// `load_core_system_languages` reloads the language language unconditionally, and the
    /// in-flight task may by then be installing link languages through it. `None` only if no
    /// startup ever published an outcome.
    Follower(Option<CoreLoadOutcome>),
}

impl ConductorStartup {
    /// Load the core system languages, one caller at a time, then let the startup task load
    /// the link and installed languages. A caller that arrived while a startup was in flight
    /// waits for that startup's core load and returns its outcome instead.
    pub async fn load_core_languages(
        self,
        language_language_only: bool,
    ) -> Result<(), LanguageError> {
        match self.role {
            Role::Leader(outcome) => {
                let result = {
                    let _one_at_a_time = CORE_LANGUAGE_LOAD.lock().await;
                    LanguageController::global_instance()
                        .load_core_system_languages(language_language_only)
                        .await
                };
                outcome.send_replace(Some(result.is_ok()));
                result
            }
            Role::Follower(outcome) => {
                if matches!(wait_for_outcome(outcome).await, Some(true)) {
                    info!("Core system languages loaded by the startup in flight");
                    Ok(())
                } else {
                    Err(LanguageError::LoadError {
                        address: "system languages".to_string(),
                        message: "the concurrent generate/unlock failed to load them".to_string(),
                    })
                }
            }
        }
    }
}

/// The outcome once reported; `None` if the reporting handle was dropped first.
async fn wait_for_outcome(outcome: Option<CoreLoadOutcome>) -> Option<bool> {
    let mut outcome = outcome?;
    let reported = outcome.wait_for(|o| o.is_some()).await.ok()?;
    *reported
}

/// Start the conductor (unless it is already running) in the background, and once the
/// caller has loaded the core system languages, the link and installed languages.
/// Call this before loading the core languages. A failed start is logged and announced as an
/// `agent-status-changed` event carrying the error, since the reply may already have gone.
pub fn spawn_conductor_startup(passphrase: String) -> ConductorStartup {
    let (in_flight, core_loaded_tx, core_loaded_rx) = {
        let mut current = CURRENT_CORE_LOAD.lock().unwrap_or_else(|e| e.into_inner());
        let Some(in_flight) = InFlight::claim(&STARTUP_IN_FLIGHT) else {
            info!("Holochain startup already in progress; not starting another");
            return ConductorStartup {
                role: Role::Follower(current.clone()),
            };
        };
        let (tx, rx) = watch::channel(None);
        *current = Some(rx.clone());
        (in_flight, tx, rx)
    };
    // Before spawning, so a request arriving before the task's first poll already waits
    // rather than seeing no service and no start.
    let starting = ConductorStarting::begin();

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

        // `Some(false)` is a failed core load; `None` is the handle dropped before it finished.
        if wait_for_outcome(Some(core_loaded_rx)).await != Some(true) {
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
        role: Role::Leader(core_loaded_tx),
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

    fn follower() -> (watch::Sender<Option<bool>>, ConductorStartup) {
        let (tx, rx) = watch::channel(None);
        let startup = ConductorStartup {
            role: Role::Follower(Some(rx)),
        };
        (tx, startup)
    }

    #[tokio::test]
    async fn a_follower_succeeds_once_the_leader_loads_the_core() {
        let (leader, startup) = follower();
        let load = tokio::spawn(startup.load_core_languages(false));
        leader.send_replace(Some(true));
        assert!(load.await.unwrap().is_ok());
    }

    #[tokio::test]
    async fn a_follower_fails_when_the_leader_core_load_fails() {
        let (leader, startup) = follower();
        let load = tokio::spawn(startup.load_core_languages(false));
        leader.send_replace(Some(false));
        assert!(load.await.unwrap().is_err());
    }

    #[tokio::test]
    async fn a_follower_fails_when_the_leader_is_dropped_before_reporting() {
        let (leader, startup) = follower();
        let load = tokio::spawn(startup.load_core_languages(false));
        drop(leader);
        assert!(load.await.unwrap().is_err());
    }
}
