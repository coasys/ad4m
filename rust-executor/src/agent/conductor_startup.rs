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
//!
//! The link and installed languages are split the same way, but per bundle instead of per
//! role: once the core load succeeds, `conductor_languages.rs` inspects each bundle and
//! loads the ones that don't use Holochain while the conductor is still booting; only the
//! detected Holochain-users wait for the init outcome (and are skipped when init fails).

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;
use std::time::Duration;

use log::{error, info, warn};
use tokio::sync::watch;

use crate::agent::AgentService;
use crate::holochain_service::{
    maybe_get_holochain_service, ConductorStarting, HolochainService, LocalConductorConfig,
    SERVICE_WAIT,
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
    /// in-flight task may by then be installing link languages through it. `Option` only
    /// because the static's type allows it: `claim_startup` sets the receiver in the same
    /// critical section that claims the flag, so whenever a claim fails there is one.
    Follower(Option<CoreLoadOutcome>),
}

/// The result of claiming the startup: either this caller starts it, or it joins the one in
/// flight.
enum Claim {
    Leader(watch::Sender<Option<bool>>, InFlight),
    Follower(Option<CoreLoadOutcome>),
}

/// Claim `STARTUP_IN_FLIGHT`, or join the startup that holds it. The claim and the
/// replacement of `CURRENT_CORE_LOAD` happen under one lock; that is what makes a follower
/// attach to the startup in flight rather than read a finished one's outcome.
fn claim_startup() -> Claim {
    let mut current = CURRENT_CORE_LOAD.lock().unwrap_or_else(|e| e.into_inner());
    match InFlight::claim(&STARTUP_IN_FLIGHT) {
        Some(in_flight) => {
            let (tx, rx) = watch::channel(None);
            *current = Some(rx);
            Claim::Leader(tx, in_flight)
        }
        None => Claim::Follower(current.clone()),
    }
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
            Role::Follower(outcome) => follow(outcome, SERVICE_WAIT).await,
        }
    }
}

/// A follower's core load: the in-flight startup's outcome, waited for up to `timeout`.
/// Bounded like the Holochain service accessors, because a leader stuck in its core load
/// (e.g. a system language waiting on a conductor that never starts) keeps
/// `STARTUP_IN_FLIGHT` claimed, making every later generate/unlock a follower; unbounded,
/// each of those requests would hang with nothing logged.
async fn follow(outcome: Option<CoreLoadOutcome>, timeout: Duration) -> Result<(), LanguageError> {
    let message = match tokio::time::timeout(timeout, wait_for_outcome(outcome)).await {
        Ok(Some(true)) => {
            info!("Core system languages loaded by the startup in flight");
            return Ok(());
        }
        Ok(_) => "the concurrent generate/unlock failed to load them".to_string(),
        Err(_) => {
            warn!(
                "Startup in flight has not loaded the core system languages after {}s; giving up waiting",
                timeout.as_secs()
            );
            format!(
                "the concurrent generate/unlock did not load them within {}s",
                timeout.as_secs()
            )
        }
    };
    Err(LanguageError::LoadError {
        address: "system languages".to_string(),
        message,
    })
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
///
/// `passphrase` is used only if this call actually starts the conductor. It is ignored when
/// another generate/unlock's startup is in flight, and when a conductor is already running:
/// the lair keystore's passphrase is fixed when it is created, and a second `init` would
/// fail or put a second conductor on the same data path.
pub fn spawn_conductor_startup(passphrase: String) -> ConductorStartup {
    let (core_loaded_tx, in_flight) = match claim_startup() {
        Claim::Leader(tx, in_flight) => (tx, in_flight),
        Claim::Follower(outcome) => {
            info!("Holochain startup already in progress; not starting another");
            return ConductorStartup {
                role: Role::Follower(outcome),
            };
        }
    };
    let core_loaded_rx = core_loaded_tx.subscribe();
    // Before spawning, so a request arriving before the task's first poll already waits
    // rather than seeing no service and no start.
    let starting = ConductorStarting::begin();

    tokio::spawn(async move {
        // Conductor init and the conductor-independent language loads run
        // concurrently: while cells are still starting, the languages whose
        // bundles don't use Holochain (see `conductor_languages.rs`) already
        // load. Only the detected Holochain-users wait for the init outcome.
        let init = async {
            if maybe_get_holochain_service().await.is_none() {
                info!("Holochain service not initialized. Initializing...");
                let config = crate::config::get_global_config();
                let hc_config = LocalConductorConfig::from_ad4m_config(&config, passphrase);
                let result = HolochainService::init(hc_config).await;
                drop(starting);
                if let Err(e) = result {
                    error!("Error initializing Holochain: {:?}", e);
                    return Err(format!("Holochain init failed: {}", e));
                }
                info!("Holochain init complete");
            } else {
                drop(starting);
            }
            Ok(())
        };

        let load_non_holochain_languages = async {
            // `Some(false)` is a failed core load; `None` is the handle dropped
            // before it finished.
            if wait_for_outcome(Some(core_loaded_rx)).await != Some(true) {
                warn!("Core system languages did not load; skipping link and installed languages");
                return None;
            }
            let language_language_only = crate::config::get_global_config()
                .language_language_only
                .unwrap_or(false);
            if language_language_only {
                return Some(Vec::new());
            }
            Some(
                LanguageController::global_instance()
                    .load_link_and_installed_languages()
                    .await,
            )
        };

        let (init_result, deferred) = tokio::join!(init, load_non_holochain_languages);

        if let Err(message) = init_result {
            // Skip the deferred Holochain-using languages: without a conductor each
            // would wait 120s for it and panic, one after another, while this task
            // kept the in-flight flag and so refused a retried unlock. They load on
            // first use instead, and a successful retry loads the rest. Released
            // before announcing, so a client that retries on the event isn't refused.
            //
            // Releasing early lets a retry become a second leader while this startup's
            // core load may still be running. That is safe only because this path
            // returns without loading languages (the non-Holochain loads above have
            // already finished by the time the join resolves): the two core loads are
            // serialized by `CORE_LANGUAGE_LOAD`, but nothing else keeps a language
            // load here from overlapping the other leader's core load (see
            // `ConductorStartup`). Keep this path free of language loads, including
            // "try the deferred languages anyway".
            drop(in_flight);
            announce_startup_failure(message).await;
            return;
        }

        if let Some(deferred) = deferred {
            LanguageController::global_instance()
                .load_deferred_holochain_languages(deferred)
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

    // The one test touching the module's statics; unit tests run with --test-threads=1.
    #[tokio::test]
    async fn followers_attach_to_the_startup_in_flight_not_a_finished_one() {
        let Claim::Leader(first, first_in_flight) = claim_startup() else {
            panic!("the first claim should lead");
        };
        let Claim::Follower(Some(mut joined)) = claim_startup() else {
            panic!("a claim while the first is held should follow it");
        };
        first.send_replace(Some(false));
        assert_eq!(*joined.borrow_and_update(), Some(false));

        drop(first_in_flight);
        let Claim::Leader(second, second_in_flight) = claim_startup() else {
            panic!("a claim after the first is released should lead again");
        };
        let Claim::Follower(Some(later)) = claim_startup() else {
            panic!("a claim while the second is held should follow it");
        };
        assert_eq!(
            *later.borrow(),
            None,
            "must not see the first startup's outcome"
        );
        second.send_replace(Some(true));
        assert_eq!(*later.borrow(), Some(true));
        drop(second_in_flight);
    }

    #[tokio::test]
    async fn a_follower_gives_up_when_the_leader_never_reports() {
        let (_leader, rx) = watch::channel(None);
        let result = follow(Some(rx), Duration::from_millis(300)).await;
        assert!(result.is_err());
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
