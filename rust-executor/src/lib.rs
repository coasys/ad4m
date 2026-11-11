#[macro_use]
extern crate lazy_static;

pub mod api;
pub mod at_rest;
pub mod bots;
pub mod config;
pub mod config_file;
pub mod config_rpc;
pub mod email_service;
pub mod entanglement_service;
pub mod exit;
#[cfg(unix)]
pub mod fd_limit;
mod globals;
pub mod helpers;
pub mod holochain_service;
pub mod identity;
pub mod js_core;
pub mod mcp;
pub mod perspective_snapshot;
pub mod perspectives;
pub mod presence;
mod prolog_service;
pub mod runtime_service;
pub mod secret;
pub mod sfu;
pub mod unyt_service;
pub mod user_management;
pub mod utils;
pub mod wallet;

pub mod agent;
pub mod ai_service;
pub mod billing;
mod dapp_server;
pub mod db;
pub mod db_backend;
mod events;
pub mod init;
pub mod languages;
pub mod log_files;
pub mod logging;
mod neighbourhoods;
mod pubsub;
pub mod tool_bundles;
use rustls::crypto::aws_lc_rs;
#[cfg(test)]
mod test_utils;
pub mod types;

use std::thread::JoinHandle;

use deno_core::error::AnyError;
use log::{error, info, warn};

use crate::prolog_service::init_prolog_service;
use crate::{
    agent::AgentService, ai_service::AIService, dapp_server::serve_dapp, db::Ad4mDb,
    languages::LanguageController, runtime_service::RuntimeService, utils::find_port,
};
pub use config::Ad4mConfig;
pub use secret::Secret;

#[cfg(unix)]
use libc::{sigaction, sigemptyset, sighandler_t, SA_ONSTACK, SIGURG};
#[cfg(unix)]
use std::ptr;

#[cfg(unix)]
extern "C" fn handle_sigurg(_: libc::c_int) {
    //println!("Received SIGURG signal, but ignoring it.");
}

fn find_and_set_port(config_port: &mut Option<u16>, start_port: u16, service_name: &str) {
    if config_port.is_none() {
        match find_port(start_port, 40000) {
            Ok(port) => *config_port = Some(port),
            Err(e) => {
                let error_string = format!("Failed to find port for {}: {}", service_name, e);
                error!("{}", error_string);
                panic!("{}", error_string);
            }
        }
    }
}

/// Standalone holochain signal receiver task.
/// Routes signals from Holochain to per-language runtimes.
async fn holochain_signal_receiver() {
    use holochain::prelude::Signal;

    loop {
        if let Some(holochain_service) = holochain_service::maybe_get_holochain_service().await {
            let mut stream_receiver = holochain_service.stream_receiver.lock().await;
            if let Some(signal) = stream_receiver.recv().await {
                match signal.clone() {
                    Signal::App {
                        cell_id,
                        zome_name,
                        signal: payload,
                    } => {
                        let dna_hash_raw = cell_id.dna_hash().get_raw_39().to_vec();
                        let agent_pubkey_raw = cell_id.agent_pubkey().get_raw_39().to_vec();
                        let cell_id_key = format!(
                            "{}:{}",
                            dna_hash_raw
                                .iter()
                                .map(|b| format!("{:02x}", b))
                                .collect::<String>(),
                            agent_pubkey_raw
                                .iter()
                                .map(|b| format!("{:02x}", b))
                                .collect::<String>()
                        );

                        // Decode the signal payload from msgpack to JSON using rmpv
                        // for proper Binary type handling
                        let payload_bytes = payload.into_inner().as_bytes().to_vec();
                        let payload_str = {
                            let mut cursor = std::io::Cursor::new(&payload_bytes);
                            match rmpv::decode::read_value(&mut cursor) {
                                Ok(msgpack_val) => {
                                    let json_val = holochain_service::holochain_service_extension::msgpack_value_to_json(msgpack_val);
                                    serde_json::to_string(&json_val)
                                        .unwrap_or_else(|_| "null".to_string())
                                }
                                Err(e) => {
                                    log::warn!(
                                        "Failed to decode signal payload from msgpack: {}",
                                        e
                                    );
                                    format!(
                                        "{}",
                                        js_core::ExternWrapper(holochain::prelude::ExternIO::from(
                                            payload_bytes
                                        ))
                                    )
                                }
                            }
                        };
                        // Check if this signal is from the Unyt alliance DNA
                        if unyt_service::is_alliance_cell(&cell_id_key).await {
                            let payload_json: serde_json::Value =
                                serde_json::from_str(&payload_str)
                                    .unwrap_or(serde_json::Value::Null);
                            tokio::spawn(async move {
                                unyt_service::handle_signal(&payload_json).await;
                            });
                            continue;
                        }

                        let maybe_lang_address: Option<String> = {
                            let handlers = js_core::languages_extension::HOLOCHAIN_SIGNAL_HANDLERS
                                .read()
                                .unwrap_or_else(|p| p.into_inner());
                            handlers.get(&cell_id_key).cloned()
                        };
                        if let Some(lang_address) = maybe_lang_address {
                            // Build the signal argument as a JSON value so no raw
                            // data is interpolated directly into executable JS.
                            let payload_json: serde_json::Value =
                                serde_json::from_str(&payload_str)
                                    .unwrap_or(serde_json::Value::Null);
                            let args = serde_json::json!({
                                "cell_id": [dna_hash_raw, agent_pubkey_raw],
                                "zome_name": zome_name.to_string(),
                                "payload": payload_json,
                            });
                            let args_json =
                                serde_json::to_string(&args).unwrap_or_else(|_| "null".to_string());
                            let signal_script = format!(
                                "await globalThis.__handleHolochainSignal__({})",
                                args_json
                            );
                            let lang_addr = lang_address.clone();
                            tokio::spawn(async move {
                                let controller = languages::LanguageController::global_instance();
                                if let Err(e) = controller
                                    .execute_on_language(&lang_addr, &signal_script)
                                    .await
                                {
                                    log::warn!(
                                        "Failed to route Holochain signal to language {}: {}",
                                        lang_addr,
                                        e
                                    );
                                }
                            });
                        } else {
                            log::debug!(
                                "No per-language runtime registered for Holochain signal from cell {}",
                                cell_id_key
                            );
                        }
                    }
                    Signal::System(_) => {
                        info!("Received system signal");
                    }
                    // HC 0.7.0 added Signal::AppDirect (direct peer-to-peer
                    // signal that bypasses the DHT). AD4M's per-language
                    // signal routing goes through Signal::App only; direct
                    // signals are logged and ignored here — individual
                    // languages that want direct-signal support can wire it
                    // in later via a dedicated handler.
                    //
                    // Matched explicitly so any *future* Signal variant
                    // surfaces as an unhandled-variant compile error instead
                    // of being silently rebranded as an AppDirect.
                    // (Data's PR #907 review MED-4.)
                    Signal::AppDirect { .. } => {
                        log::debug!(
                            "Received Signal::AppDirect (HC 0.7 direct peer signal; \
                             not routed to per-language handlers)"
                        );
                    }
                }
            } else {
                // stream_receiver.recv() returned None — channel closed.
                // Back off before re-acquiring the service; this rarely
                // happens in normal operation.
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
            }
        } else {
            // Holochain service not yet available (still booting or
            // disabled). 500ms between readiness checks.
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
        }
    }
}

/// Unlocks the main agent with the passphrase an operator supplied at
/// startup (`AD4M_UNLOCK_PASSPHRASE_FILE`), the way `agent.unlock` does, so
/// a restarted headless executor serves requests without a person or a
/// script unlocking it. Call after [`run`] has returned. Without an agent
/// (not generated yet) it only logs. Neither path logs the passphrase.
pub async fn unlock_agent_at_startup(passphrase: String) {
    let initialized = AgentService::with_global_instance(|agent| agent.is_initialized());
    if !initialized {
        warn!(
            "AD4M_UNLOCK_PASSPHRASE_FILE is set but there is no agent yet: generate one \
             (agent.generate); later starts unlock it from the file"
        );
        return;
    }
    const FAILED: &str = "Unlocking the agent at startup with the passphrase from \
                          AD4M_UNLOCK_PASSPHRASE_FILE failed";
    const STAYS_LOCKED: &str = "The executor stays up with the agent locked; check the file, \
                                then unlock with agent.unlock or restart";
    match api::agent_ws::unlock_main_agent(passphrase).await {
        Ok(status) if status.is_unlocked => match status.error {
            None => info!(
                "Agent unlocked at startup with the passphrase from AD4M_UNLOCK_PASSPHRASE_FILE"
            ),
            Some(error) => error!(
                "Agent unlocked at startup with the passphrase from \
                 AD4M_UNLOCK_PASSPHRASE_FILE, but starting its services failed: {error}"
            ),
        },
        Ok(status) => error!(
            "{FAILED}: {}. {STAYS_LOCKED}",
            status.error.unwrap_or_default()
        ),
        Err(e) => error!("{FAILED}: {}. {STAYS_LOCKED}", e.message),
    }
}

/// How long the graceful shutdown that a signal starts may take before the process exits anyway.
/// Below the 5 s after which test harnesses escalate to SIGKILL, so the cleanup that does finish
/// (the Holochain conductor's own shutdown is bounded at 3 s) is not cut short by them.
#[cfg(unix)]
const SHUTDOWN_DEADLINE: std::time::Duration = std::time::Duration::from_secs(4);

/// Whether the executor was started with SIGHUP ignored, as `nohup` does. Read before any handler
/// is installed: installing one replaces the inherited disposition.
#[cfg(unix)]
fn sighup_ignored() -> bool {
    // SAFETY: a null new action only reads the current disposition into `current`.
    unsafe {
        let mut current: libc::sigaction = std::mem::zeroed();
        libc::sigaction(libc::SIGHUP, std::ptr::null(), &mut current) == 0
            && current.sa_sigaction == libc::SIG_IGN
    }
}

/// Runs the REST server and the deno core runtime.
///
/// Returns the REST API server's thread. It ends with `Err` when the server
/// cannot start or stops, e.g. when the API port is taken. `run` never exits
/// the process itself, because the launcher embeds it and must stay up to show
/// the error. Executor binaries pass the handle to [`exit_when_api_fails`].
pub async fn run(mut config: Ad4mConfig) -> JoinHandle<Result<(), AnyError>> {
    #[cfg(unix)]
    unsafe {
        let mut action: sigaction = std::mem::zeroed();
        action.sa_flags = SA_ONSTACK;
        action.sa_sigaction = handle_sigurg as *const () as sighandler_t;
        sigemptyset(&mut action.sa_mask);

        if libc::sigaction(SIGURG, &action, ptr::null_mut()) != 0 {
            error!("Failed to set up SIGURG signal handler");
        }
    }

    // The graceful stop: `runtime.quit`, `runtime.restart`, the signals and `--watch-stdin`
    // start it (`exit::request_shutdown`), and it exits with the reason they give.
    let shutdown_rx = exit::graceful_stop_channel();

    // Spawn a task that listens for OS signals (SIGTERM, SIGINT, SIGHUP) and triggers shutdown.
    // It is the process's only handler for them: the executor binaries install none of their
    // own (one would replace this one). SIGHUP, a closed terminal, gets the same cleanup, unless
    // the executor was started with SIGHUP ignored (`nohup`): then it stays ignored.
    // It logs and never prints: stdout may be a pipe whose reader (the launcher) is gone, and
    // `println!` panics on the broken pipe.
    #[cfg(unix)]
    {
        tokio::spawn(async {
            use tokio::signal::unix::{signal, Signal, SignalKind};
            let mut sigint =
                signal(SignalKind::interrupt()).expect("failed to install SIGINT handler");
            let mut sigterm =
                signal(SignalKind::terminate()).expect("failed to install SIGTERM handler");
            let mut sighup = (!sighup_ignored())
                .then(|| signal(SignalKind::hangup()).expect("failed to install SIGHUP handler"));
            async fn hangup(sighup: &mut Option<Signal>) {
                match sighup {
                    Some(sighup) => {
                        sighup.recv().await;
                    }
                    None => std::future::pending().await,
                }
            }

            let received = tokio::select! {
                _ = sigint.recv() => "SIGINT",
                _ = sigterm.recv() => "SIGTERM",
                _ = hangup(&mut sighup) => "SIGHUP",
            };

            // Start the graceful stop before logging: a log write that blocks (a launcher that
            // no longer drains stderr) must not hold up the cleanup.
            exit::request_shutdown(exit::ExitReason::Stopped);
            info!("Received {received}, initiating graceful shutdown...");

            // The cleanup must not keep the process alive: a second signal, or the deadline,
            // ends it without waiting any longer.
            tokio::select! {
                _ = sigint.recv() => warn!("⚠️ Received SIGINT again, exiting without finishing the cleanup"),
                _ = sigterm.recv() => warn!("⚠️ Received SIGTERM again, exiting without finishing the cleanup"),
                _ = hangup(&mut sighup) => warn!("⚠️ Received SIGHUP again, exiting without finishing the cleanup"),
                _ = tokio::time::sleep(SHUTDOWN_DEADLINE) => warn!(
                    "⚠️ Cleanup did not finish within {:?}, exiting without it",
                    SHUTDOWN_DEADLINE
                ),
            }
            exit::exit_now(1);
        });
    }

    // Spawn the shutdown handler that waits for the signal and cleans up. It reads the PID
    // file from here: a stop may come before the global config is set.
    let pid_file = config.pid_file.clone();
    tokio::spawn(async move {
        if let Ok(reason) = shutdown_rx.await {
            info!("Shutdown signal received ({reason:?}), cleaning up...");

            // 1. Shut down Holochain conductor gracefully
            if let Some(holochain_service) = holochain_service::maybe_get_holochain_service().await
            {
                info!("Shutting down Holochain conductor...");
                match holochain_service.shutdown().await {
                    Ok(()) => info!("Holochain conductor shut down cleanly"),
                    Err(e) => warn!("Error shutting down Holochain conductor: {}", e),
                }
            }

            // 2. Remove PID file if it was configured
            if let Some(ref pid_file) = pid_file {
                let _ = std::fs::remove_file(pid_file);
                info!("Removed PID file: {}", pid_file);
            }

            info!(
                "Graceful shutdown complete, exiting with {}.",
                reason.code()
            );
            exit::exit_now(reason.code());
        }
    });

    // One executor per data folder, before anything in it is opened, and before the log
    // rotation: a refused start must not rename the running executor's log. It reports to
    // stdout.
    if let Err(e) = exit::hold_data_folder(&config.data_folder()) {
        crate::logging::init_cli_logging(config.log_config.as_ref());
        return refused_start(e);
    }

    // Logging: to rotated files in `log_dir` when set, else to stdout. Respects RUST_LOG.
    match config.log_dir.as_deref() {
        Some(dir) => {
            if let Err(e) = crate::logging::init_file_logging(
                std::path::Path::new(dir),
                config.log_config.as_ref(),
            ) {
                // No file logger: the reason goes to stdout.
                crate::logging::init_cli_logging(config.log_config.as_ref());
                return failed_start(format!("Cannot write the executor log in {dir}: {e}"));
            }
        }
        None => crate::logging::init_cli_logging(config.log_config.as_ref()),
    }

    // `exit_with_parent`: end with the process that started us, however it ended. A launcher
    // that is SIGKILLed, crashes or exits without stopping us (a cancelled CI job, a crashed
    // Electron) sends no signal, and an executor nobody stops runs on with nobody to talk to.
    // The parent's PID is polled, not watched with PR_SET_PDEATHSIG, so it works on macOS too.
    #[cfg(unix)]
    if config.exit_with_parent.unwrap_or(false) {
        let parent = std::os::unix::process::parent_id();
        std::thread::Builder::new()
            .name("parent-watch".to_string())
            .spawn(move || {
                while std::os::unix::process::parent_id() == parent {
                    std::thread::sleep(std::time::Duration::from_millis(500));
                }
                // The parent going away is a stop (exit 0), through the run contract's one entry.
                exit::request_shutdown(exit::ExitReason::Stopped);
                // Nothing in this thread logs: a stderr nobody drains would block the write, and
                // with it the forced exit below. The line goes out from a thread of its own.
                std::thread::spawn(move || {
                    warn!(
                        "The process that started the executor (PID {parent}) is gone, shutting \
                         down; exiting anyway after {SHUTDOWN_DEADLINE:?} if cleanup is not done"
                    )
                });
                std::thread::sleep(SHUTDOWN_DEADLINE);
                exit::exit_now(1);
            })
            .expect("failed to start the parent-watch thread");
    }

    // Before any perspective store or language opens files.
    #[cfg(unix)]
    crate::fd_limit::raise_fd_limit_at_startup();

    config.prepare();

    if let Err(message) = config.check_admin_credential() {
        error!("{}", message);
        panic!("{}", message);
    }

    // Out of the config before it is copied anywhere (the global config, the API thread).
    let storage_secret = config.storage_secret.take();

    // The storage keys and the wallet's keys are in this process's memory once unlocked. The
    // executor binaries did this first thing; an embedder's process gets it here.
    #[cfg(unix)]
    if let Err(e) = exit::forbid_core_dumps() {
        warn!("Could not turn off core dumps for the executor: {e}");
    }
    // Files and folders made from here on are private to this user, the ones libraries make
    // with the default mode too (the perspectives' SPARQL stores, which add files while they
    // run). For an embedder this applies to its whole process.
    at_rest::files::private_umask();

    // Write PID file if requested via config.
    // Test harnesses can set pid_file to get a reliable PID for targeted cleanup.
    if let Some(ref pid_file) = config.pid_file {
        let pid = std::process::id();
        if let Err(e) = std::fs::write(pid_file, pid.to_string()) {
            warn!("Failed to write PID file {}: {}", pid_file, e);
        } else {
            info!("Wrote PID {} to {}", pid, pid_file);
        }
    }

    // A supervisor's pipe: when it closes, the supervisor is gone and the executor stops.
    // After the PID file, so the stop that removes it never comes before it is written.
    // `std::io::stdin()` is the process's one buffered reader: with `--custody-channel` the CLI
    // took the credential line from it (`config::read_custody_credential`), and the watcher
    // reads on from what is left in the same buffer.
    if config.watch_stdin == Some(true) {
        exit::stop_when_closed(std::io::stdin(), || {
            info!("stdin closed (--watch-stdin): stopping");
            exit::request_shutdown(exit::ExitReason::Stopped);
        });
    }

    // Store config globally so services (e.g. agent mutation resolvers) can access it
    crate::config::set_global_config(config.clone());

    // The data folders, before the keyring is read from one. Each new one is 0700, and the data
    // folder itself is closed to 0700 whoever made it (the data-folder lock, an older executor).
    let data_root = std::path::PathBuf::from(
        config
            .app_data_path
            .as_ref()
            .expect("App data path not set in Ad4mConfig"),
    );
    {
        let base = data_root.join("ad4m");
        let dirs = [
            data_root.clone(),
            base.clone(),
            base.join("data"),
            base.join("languages"),
            base.join("languages").join("temp"),
            base.join("h"),
            base.join("h").join("d"),
            base.join("h").join("c"),
        ];
        for dir in &dirs {
            if let Err(e) = at_rest::files::create_private_dir_all(dir) {
                error!("Failed to create data directory {:?}: {}", dir, e);
                panic!(
                    "Cannot continue without required data directory {:?}: {}",
                    dir, e
                );
            }
        }
        at_rest::files::close_if_open(&data_root);
        at_rest::files::close_if_open(&base);
    }

    // No store opens before the storage secret arrived. With the secret from the embedding
    // shell or the secret file the keyring opens here; without it, or when it does not open
    // the keyring, the API starts now and answers only `runtime.storageStatus` and
    // `storage.unlock` until the operator unlocks (`at_rest`).
    let gate = std::sync::Arc::new(at_rest::StorageGate::new(
        &data_root,
        config.allow_secret_on_data_disk.unwrap_or(false),
    ));
    if at_rest::install(gate.clone()).is_err() {
        return failed_start(
            "The executor already ran in this process: one executor per process".to_string(),
        );
    }
    let unlocked = match &storage_secret {
        Some(secret) => match gate.unlock(secret) {
            Ok(outcome) => {
                info!(
                    "Storage unlocked with the secret from {:?}{}",
                    secret.source,
                    if outcome.created {
                        "; keyring made"
                    } else {
                        ""
                    }
                );
                true
            }
            Err(e) => {
                error!("The storage secret of this start did not open the keyring: {e}");
                false
            }
        },
        None => false,
    };
    drop(storage_secret);
    let mut early_api = None;
    if !unlocked {
        find_and_set_port(&mut config.port, 4000, "REST API");
        warn!(
            "Storage sealed (keyring {}): waiting for storage.unlock, e.g. `ad4m-executor storage unlock` on this machine",
            gate.keys().keyring_id().to_hex()
        );
        let (api, api_ended) = spawn_api_thread(config.clone());
        tokio::select! {
            _ = gate.unsealed() => early_api = Some(api),
            // The API could not start (port taken) or stopped: nobody can unlock any more.
            _ = api_ended => return api,
        }
    }

    // Initialise the wallet backend based on config.
    // "shared" mode connects to an external HTTP wallet service;
    // everything else (including unset) uses the in-process LocalWallet.
    {
        use std::sync::Arc;
        let backend: Arc<dyn crate::wallet::WalletBackend> = match config.wallet_backend.as_deref()
        {
            Some("shared") => {
                let url = config
                    .wallet_backend_url
                    .as_ref()
                    .expect("WALLET_BACKEND_URL required when wallet_backend = shared");
                let token = config
                    .internal_api_token
                    .as_ref()
                    .expect("INTERNAL_API_TOKEN required for shared backends");
                info!("Initialising shared wallet backend at {}", url);
                // The Worker's bearer token: the shared wallet sends it on every call.
                Arc::new(crate::wallet::SharedWallet::new(
                    url.clone(),
                    token.expose().to_string(),
                ))
            }
            _ => {
                info!("Initialising local wallet backend");
                Arc::new(crate::wallet::LocalWallet::new())
            }
        };
        crate::wallet::init_wallet_backend(backend);
    }

    // Initialise the database backend.
    // "shared" mode delegates to the platform Worker's internal DB API;
    // everything else uses the in-process Ad4mDb (LocalDb).
    {
        use std::sync::Arc;
        let backend: Arc<dyn crate::db_backend::DbBackend> = match config.db_backend.as_deref() {
            Some("shared") => {
                let url = config
                    .db_backend_url
                    .as_ref()
                    .expect("DB_BACKEND_URL required when db_backend = shared");
                let token = config
                    .internal_api_token
                    .as_ref()
                    .expect("INTERNAL_API_TOKEN required for shared backends");
                info!("Initialising shared database backend at {}", url);
                // The Worker's bearer token: the shared database sends it on every call.
                Arc::new(crate::db_backend::SharedDb::new(
                    url.clone(),
                    token.expose().to_string(),
                ))
            }
            _ => {
                info!("Initialising local database backend");
                Arc::new(crate::db_backend::LocalDb::new())
            }
        };
        crate::db_backend::init_db_backend(backend);
    }

    // Restore perspective data from the platform backend if running in shared mode.
    // Downloads the tar.gz snapshot and extracts to the data directory
    // before perspectives initialise (so OxiGraph opens with restored data).
    if config.wallet_backend.as_deref() == Some("shared") {
        match crate::perspective_snapshot::restore_perspectives(&config) {
            Ok(true) => info!("Restored perspectives from remote snapshot"),
            Ok(false) => info!("No remote snapshot found — starting with fresh perspectives"),
            Err(e) => log::warn!("Perspective restore failed (continuing without): {}", e),
        }
    }

    // ── Token separation assertion ──────────────────────────────────────
    // internal_api_token (executor → Worker) must differ from admin_credential
    // (client → executor) to maintain trust boundary separation. Same value
    // means compromise of any admin-capable client leaks platform-internal auth.
    if config.wallet_backend.as_deref() == Some("shared") {
        if let (Some(internal), Some(admin)) =
            (&config.internal_api_token, &config.admin_credential)
        {
            if admin.expose_for_compare(internal.expose()) {
                panic!(
                    "INTERNAL_API_TOKEN must differ from ADMIN_CREDENTIAL in shared mode. \
                     Using the same value collapses two trust boundaries."
                );
            }
        }
    }

    aws_lc_rs::default_provider()
        .install_default()
        .expect("Failed to install rustls' aws_lc_rs crypto provider");

    // The database file goes under its key before anything opens it: a plaintext one from an
    // older executor moves now, and on failure the executor runs on it in the clear.
    match at_rest::database::prepare(gate.keys(), &data_root.join("ad4m_db.sqlite"), Some(&gate)) {
        Ok(at_rest::database::DatabaseStep::Keyed(_)) => {}
        Ok(at_rest::database::DatabaseStep::KeptPlain(e)) => {
            warn!("The database stays in the clear this start: {e}");
        }
        Err(e) => return failed_start(e),
    }

    info!("Initializing Ad4mDb...");

    Ad4mDb::init_global_instance(
        config
            .app_data_path
            .as_ref()
            .map(|path| {
                std::path::Path::new(path)
                    .join("ad4m_db.sqlite")
                    .to_string_lossy()
                    .into_owned()
            })
            .expect("App data path not set in Ad4mConfig")
            .as_str(),
    )
    .expect("Failed to initialize Ad4mDb");
    // SQLite makes its files with the process umask: only the owner reads them.
    for name in [
        "ad4m_db.sqlite",
        "ad4m_db.sqlite-wal",
        "ad4m_db.sqlite-shm",
        "ad4m_db.sqlite-journal",
    ] {
        let file = data_root.join(name);
        if file.exists() {
            if let Err(e) = at_rest::files::restrict(&file) {
                warn!("Could not make {} private: {e}", file.display());
            }
        }
    }

    // Managed users made before each user had its own data key get theirs now, before any of
    // their sessions writes.
    if gate.keys().is_open() {
        let usernames = match crate::db_backend::db_backend().list_users() {
            Ok(users) => users.into_iter().map(|u| u.username).collect::<Vec<_>>(),
            Err(e) => return failed_start(format!("Could not list the managed users: {e}")),
        };
        match at_rest::persons::prepare(gate.keys(), &usernames) {
            Ok(0) => {}
            Ok(made) => info!("Made the data keys of {made} managed users"),
            Err(e) => return failed_start(e),
        }
    }

    // The bots and their grants, which every call of a bot's session reads. Without them no
    // managed agent's session is served (`bots::classify`): a bot must not pass as a user.
    if let Err(e) = crate::bots::init() {
        error!("bots: {e}; no managed agent's session is served until this is fixed and the executor restarts");
    }

    // The seed names the network's identity store, which the identity module reads below.
    info!("Initializing Runtime service...");
    RuntimeService::init_global_instance(
        std::path::Path::new(&config.app_data_path.clone().unwrap().to_string())
            .join("mainnet_seed.seed")
            .to_string_lossy()
            .into_owned(),
    );

    // Grants are the allowlist of tokens: load them before the identity module starts its
    // watchers, so a verdict change they answer acts on the loaded grants (a revoked row is
    // cancelled here before any replay reaches it).
    if let Err(e) = agent::grants::load_at_start() {
        // Without its grants the API would refuse every token: it does not start.
        return failed_start(format!("Could not load the grants: {e}"));
    }

    // The key-log cache lives in the same database file.
    {
        let db_path = std::path::Path::new(
            config
                .app_data_path
                .as_ref()
                .expect("App data path not set in Ad4mConfig"),
        )
        .join("ad4m_db.sqlite");
        let settings = crate::identity::IdentitySettings::from_config(
            &config,
            RuntimeService::seed_identity_store().as_ref(),
        );
        if let Err(e) = settings.check() {
            return failed_start(e);
        }
        if let Err(e) = crate::identity::init(&db_path, &settings) {
            return failed_start(format!("Could not open the key-log cache: {e}"));
        }
        // Before any agent signs: a link's anchor is read from the key map with no I/O.
        crate::identity::load_local_identities().await;
    }

    // Set multi-user mode before starting services to avoid race condition
    if let Some(enable_multi_user) = config.enable_multi_user {
        if enable_multi_user {
            info!("Enabling multi-user mode...");
            crate::db_backend::db_backend()
                .set_multi_user_enabled(true)
                .expect("Failed to enable multi-user mode");
        }
    }
    // `check_admin_credential` refused `--enable-multi-user` without a credential; the mode
    // may also be on in the database, from `user.setMultiUserEnabled` on an earlier run.
    let multi_user_on = crate::user_management::is_multi_user_enabled();
    if let Err(reason) =
        crate::config::refuse_multi_user_from_db(&config.admin_credential, multi_user_on)
            .and_then(|()| {
                crate::config::refuse_custody_channel_with_multi_user(
                    config.custody_channel == Some(true),
                    multi_user_on,
                )
            })
            .and_then(|()| {
                crate::config::refuse_custody_channel_without_its_credential(
                    config.custody_channel == Some(true),
                    &config.custody_credential,
                    &config.admin_credential,
                )
            })
    {
        return refused_start(
            exit::StartRefused::new(exit::ExitReason::ConfigRefused, reason).into(),
        );
    }

    info!("Initializing AI service...");
    AIService::init_global_instance()
        .await
        .expect("Couldn't initialize AI service");

    info!("Initializing Agent service...");
    AgentService::init_global_instance(config.app_data_path.clone().unwrap());

    // Load agent data from disk (wallet cipher, DID, etc.) if previously initialized, then
    // end the key-bound grants the persisted verdicts refuse — a grant's `agent_speaks`
    // reads the main agent's DID, so the load comes first; the API serves no session yet.
    load_agent_and_end_revoked_key_grants(crate::identity::service().ok().as_deref()).await;

    // Notifications of older executors name their owner by email; move them to DIDs.
    if let Some(main_agent_did) = AgentService::with_global_instance(|a| a.did.clone()) {
        crate::db_backend::db_backend()
            .migrate_notification_owners(&main_agent_did)
            .expect("Failed to migrate notification owners");
    }

    // Spawn background task to clean up expired verification codes every 5 minutes
    tokio::spawn(async {
        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(300)).await;
            if let Err(e) = crate::db_backend::db_backend().cleanup_expired_codes() {
                error!("Failed to cleanup expired verification codes: {}", e);
            } else {
                info!("Cleaned up expired verification codes");
            }
        }
    });

    if config.admin_credential.is_none() {
        // Only reachable with insecure_no_admin_credential (checked above).
        warn!("╔══════════════════════════════════════════════════════════════╗");
        warn!("║  SECURITY WARNING: --insecure-no-admin-credential is set     ║");
        warn!("║  A caller with no token gets ALL_CAPABILITY (full admin      ║");
        warn!("║  access) on a loopback listener. On --localhost false, TLS,  ║");
        warn!("║  or a request with a forwarding header it is anonymous.      ║");
        warn!("║  Multi-user mode is refused: it needs an admin credential.   ║");
        warn!("║  This mode is intended for local testing ONLY.               ║");
        warn!("╚══════════════════════════════════════════════════════════════╝");
    }

    {
        info!("Initializing Prolog service...");
        init_prolog_service().await;
    }

    {
        info!("Initializing SFU service...");
        let gossip: std::sync::Arc<dyn crate::sfu::CascadeGossip> = build_sfu_gossip(&config).await;
        let (bind_addr, public_ip) = crate::sfu::server::sfu_media_addrs(
            config.sfu_bind_addr.as_deref(),
            config.localhost.unwrap_or(true),
            config.sfu_udp_port,
            config.sfu_public_addr.as_deref(),
        )
        .unwrap_or_else(|message| {
            error!("{}", message);
            panic!("{}", message);
        });
        let fixed_port = Some(bind_addr.port()).filter(|port| *port != 0);
        let sfu_config = crate::sfu::server::SfuServerConfig {
            bind_addr,
            stun_server: config.sfu_stun_server.clone(),
            public_ip,
            preferred_node: config.sfu_preferred_node.clone(),
            cascade_peer_dids: config
                .sfu_cascade_peers
                .as_deref()
                .unwrap_or(&[])
                .iter()
                .filter_map(|entry| entry.split_once('=').map(|(did, _)| did.to_string()))
                .collect(),
        };
        match crate::sfu::SfuService::start(sfu_config, gossip).await {
            Ok(_) => {}
            // A fixed port is what the router forwards: running without it
            // would leave calls broken with nothing to say why.
            Err(e) if fixed_port.is_some() => {
                error!("{}", e);
                panic!("{}", e);
            }
            Err(e) => warn!("SFU service failed to start: {}", e),
        }
    }

    find_and_set_port(&mut config.port, 4000, "REST API");
    find_and_set_port(&mut config.hc_admin_port, 2000, "Holochain admin");
    find_and_set_port(&mut config.hc_app_port, 1337, "Holochain app");

    // Initialize V8 platform for multi-threaded use (must happen before any Deno workers)
    {
        use std::sync::Once;
        static V8_FLAGS_INIT: Once = Once::new();
        V8_FLAGS_INIT.call_once(|| {
            deno_core::v8::V8::set_flags_from_string("--max-opt=0");
            // deno v2.9: init_platform signature changed — was (Option, bool),
            // now takes only Option<v8::SharedRef<v8::Platform>>. The second
            // `bool` argument (`predictable`) was removed.
            deno_core::JsRuntime::init_platform(None);
        });
    }

    // Set languages directory based on app data path (must be before LanguageController)
    crate::utils::set_languages_directory(config.app_data_path.as_ref().unwrap());

    // The languages' storage files go under their keys before any language loads. Each link
    // language's files are sealed under its neighbourhood's perspective store, so the routing
    // cache learns every neighbourhood's link language first.
    match db_backend::db_backend().get_all_perspectives() {
        Ok(handles) => {
            for handle in handles {
                if let Some(nh) = handle.neighbourhood.as_ref() {
                    let language = nh.data.link_language.clone();
                    perspectives::register_link_language_perspective(language, handle.clone());
                }
            }
        }
        Err(e) => {
            warn!("Could not list the perspectives before sealing the languages' storage: {e}")
        }
    }
    match at_rest::lang_storage::prepare(
        gate.keys(),
        &crate::utils::languages_directory(),
        Some(&gate),
    ) {
        Ok(at_rest::lang_storage::LanguageStorageStep::Sealed { sealed }) => {
            info!("The languages' storage is under its keys ({sealed} files sealed now)");
        }
        Ok(at_rest::lang_storage::LanguageStorageStep::KeptPlain { sealed, error }) => {
            warn!(
                "The languages' storage stays partly in the clear this start ({sealed} files sealed): {error}"
            );
        }
        Err(e) => return failed_start(e),
    }

    LanguageController::init_global_instance();

    // NOTE: system languages are loaded from the agent.generate/agent.unlock handlers:
    // the core ones inline, the conductor-dependent ones in `agent::conductor_startup`.

    // Set app data path for perspectives module
    perspectives::set_app_data_path(config.app_data_path.clone().unwrap());

    perspectives::open_at_start();

    // Start periodic perspective snapshots in shared mode.
    if config.wallet_backend.as_deref() == Some("shared") {
        crate::perspective_snapshot::spawn_periodic_backup(config.clone());
    }

    // Start periodic memory diagnostics (logs RSS, jemalloc stats,
    // per-perspective data structure sizes every 30s).
    perspectives::memory_diagnostics::start_memory_diagnostics();

    let app_dir = config
        .app_data_path
        .as_ref()
        .expect("App data path not set in Ad4mConfig")
        .clone();

    info!("Starting dapp server...");

    if let Some(true) = config.run_dapp_server {
        std::thread::spawn(|| {
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .thread_name(String::from("dapp_server"))
                .enable_all()
                .build()
                .unwrap();
            if let Err(e) = runtime.block_on(serve_dapp(8080, app_dir)) {
                error!("Failed to start dapp server: {:?}", e);
            }
        });
    };

    // Start holochain signal receiver and Unyt service only when holochain runs
    if config.run_holochain.unwrap_or(true) {
        // Start holochain signal receiver as standalone task
        tokio::spawn(crate::holochain_signal_receiver());

        // Eagerly install Unyt alliance DNA in the background (only if membrane proof is available).
        tokio::spawn(async {
            if unyt_service::get_membrane_proof().is_none() {
                info!("No Unyt membrane proof stored — skipping eager DNA install");
                return;
            }
            // Wait for the conductor without a limit: the agent unlock may take a while.
            while !crate::holochain_service::maybe_get_holochain_service()
                .await
                .is_some_and(|service| !service.is_stopped())
            {
                tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
            }
            match unyt_service::ensure_installed().await {
                Ok(()) => info!("Unyt alliance DNA ready"),
                Err(e) => error!("Failed to install Unyt alliance DNA: {}", e),
            }
        });

        // Spawn payment completion polling (every 30 seconds)
        tokio::spawn(async {
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(30)).await;
                unyt_service::check_pending_payments().await;
                unyt_service::check_pending_sends().await;
            }
        });
    } else {
        info!(
            "Holochain disabled (run_holochain=false) — skipping signal receiver and Unyt service"
        );
    }

    // Spawn credit change flush loop (every 2 seconds)
    // When any credit mutation marks a user dirty, this drains the set
    // and publishes updated HostingUserInfo only for affected users.
    tokio::spawn(async {
        use crate::events::Event;
        use crate::pubsub::{get_global_pubsub, DIRTY_CREDIT_USERS};

        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

            // Drain dirty users
            let dirty_emails: Vec<String> = {
                let mut set = match DIRTY_CREDIT_USERS.lock() {
                    Ok(set) => set,
                    Err(e) => {
                        error!("Credit flush: failed to lock dirty set: {}", e);
                        continue;
                    }
                };
                set.drain().collect()
            };

            if dirty_emails.is_empty() {
                continue;
            }

            let pubsub = get_global_pubsub().await;
            let global_free = match crate::db_backend::db_backend().get_free_hosting_enabled() {
                Ok(v) => v,
                Err(e) => {
                    error!("Credit flush: get_free_hosting_enabled failed: {}", e);
                    true // default to free on transient errors to avoid wrongly blocking users
                }
            };
            for email in &dirty_emails {
                let free_access = if global_free {
                    true
                } else {
                    match crate::db_backend::db_backend().get_user_free_access(email) {
                        Ok(v) => v,
                        Err(e) => {
                            error!(
                                "Credit flush: get_user_free_access failed for {}: {}",
                                email, e
                            );
                            continue;
                        }
                    }
                };
                let remaining_credits = if free_access {
                    "unlimited".to_string()
                } else {
                    match crate::db_backend::db_backend().get_user_credits(email) {
                        Ok(credits) => format!("{}", credits),
                        Err(e) => {
                            error!("Credit flush: get_user_credits failed for {}: {}", email, e);
                            continue;
                        }
                    }
                };
                let hot_wallet_address =
                    match crate::db_backend::db_backend().get_user_hot_wallet(email) {
                        Ok(v) => v,
                        Err(e) => {
                            error!(
                                "Credit flush: get_user_hot_wallet failed for {}: {}",
                                email, e
                            );
                            continue;
                        }
                    };

                let info = crate::types::HostingUserInfo {
                    email: email.clone(),
                    remaining_credits,
                    hot_wallet_address,
                    free_access,
                };

                for event in Event::hosting_user_info(&info) {
                    pubsub.publish_event(event);
                }
            }
        }
    });

    // Check if MCP mode is enabled — run MCP server alongside REST API
    if config.enable_mcp == Some(true) {
        info!("Starting MCP server alongside REST API...");
        let admin_credential = config.admin_credential.clone();
        // Cloned out here, not read inside the closure: a non-Copy field read
        // in there would move `config`, which the API server thread below
        // still needs.
        let mcp_tls = config.tls.clone();
        let mcp_host = mcp::server::host_following_localhost(
            config.localhost.unwrap_or(true),
            config.tls.is_some(),
            std::env::var("MCP_HOST").ok().as_deref(),
        );

        std::thread::spawn(move || {
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .thread_name(String::from("mcp_server"))
                .enable_all()
                .build()
                .unwrap();
            let mcp_config = mcp::server::McpServerConfig {
                port: config.mcp_port.unwrap_or(3001),
                dynamic_class_tools: config.dynamic_class_tools.unwrap_or(false),
                // The same certificate the RPC port terminates with. MCP has
                // none of its own to issue or renew.
                tls: mcp_tls,
                host: mcp_host,
            };
            if let Err(e) = runtime.block_on(mcp::start_mcp_server(admin_credential, mcp_config)) {
                error!("MCP server error: {:?}", e);
            }
        });
    }

    // Every store is open: the gate lets every method through.
    gate.mark_open();
    info!("Storage open");

    match early_api {
        // It has served the storage methods since the start.
        Some(api) => api,
        None => {
            info!("Starting REST API server...");
            spawn_api_thread(config).0
        }
    }
}

/// The REST API on a thread of its own, and a receiver that resolves when that thread's
/// server ended (it failed to start, or stopped).
fn spawn_api_thread(
    config: Ad4mConfig,
) -> (
    JoinHandle<Result<(), AnyError>>,
    tokio::sync::oneshot::Receiver<()>,
) {
    let (ended_tx, ended_rx) = tokio::sync::oneshot::channel();
    let api = std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .thread_name(String::from("rest_server"))
            .enable_all()
            .build()
            .unwrap();
        let result = runtime.block_on(api::start_server(config));
        if let Err(e) = &result {
            error!("REST API server failed: {:?}", e);
        }
        let _ = ended_tx.send(());
        result
    });
    (api, ended_rx)
}

/// `run`'s order for the start-up pass over the key-bound grants: the persisted agent is
/// read in first (as the old JS executor's `AGENT.load()` did), then the cached verdicts
/// end the grants they refuse. `agent_speaks` asks a grant's agent's DID — before
/// `AgentService::init_global_instance` that read panics ("AgentService not initialized",
/// inside the singleton's lock, so it poisons too), and before `load` reads the file in it
/// finds no agent and ends a grant it should keep. The API has not started serving, so the
/// pass still precedes every session it acts on.
pub(crate) async fn load_agent_and_end_revoked_key_grants(
    service: Option<&crate::identity::service::IdentityService>,
) {
    AgentService::with_mutable_global_instance(|agent_service| {
        if agent_service.is_initialized() {
            agent_service.load();
            info!("Agent loaded from disk");
        } else {
            info!("Agent not yet initialized (first run)");
        }
    });
    if let Some(service) = service {
        crate::identity::key_login::end_key_grants_revoked_in_cache(
            service,
            crate::agent::grants::Grants::global(),
        )
        .await;
    }
}

/// The handle [`run`] returns when the executor cannot start: it ends at once with `reason`, as
/// a failed API does, so the launcher shows it and executor binaries exit through
/// [`exit_when_api_fails`].
fn failed_start(reason: String) -> JoinHandle<Result<(), AnyError>> {
    refused_start(deno_core::anyhow::anyhow!(reason))
}

/// [`failed_start`] with the error itself, so a [`exit::StartRefused`] keeps its exit code.
fn refused_start(error: AnyError) -> JoinHandle<Result<(), AnyError>> {
    error!("{error}");
    std::thread::spawn(move || Err(error))
}

/// Build the cascade-gossip transport based on the SFU-related fields
/// of [`Ad4mConfig`].  Returns a [`NoopGossip`] when cascade is
/// unconfigured, a [`TcpGossip`] when `sfu_cascade_listen` is set, or
/// falls back to no-op (with a warning) if the requested transport
/// fails to bind.
async fn build_sfu_gossip(config: &Ad4mConfig) -> std::sync::Arc<dyn crate::sfu::CascadeGossip> {
    let max = config.sfu_max_participants_per_node.unwrap_or(8);
    let local_did = match config.sfu_local_did.clone() {
        Some(did) => did,
        None => {
            return std::sync::Arc::new(crate::sfu::NoopGossip::new("self".to_string(), max));
        }
    };
    let Some(listen) = config.sfu_cascade_listen.as_ref() else {
        return std::sync::Arc::new(crate::sfu::NoopGossip::new(local_did, max));
    };
    let bind_addr: std::net::SocketAddr = match listen.parse() {
        Ok(addr) => addr,
        Err(e) => {
            warn!(
                "SFU cascade listen `{}` parse error: {} — running standalone",
                listen, e
            );
            return std::sync::Arc::new(crate::sfu::NoopGossip::new(local_did, max));
        }
    };
    let peers: Vec<crate::sfu::GossipPeer> = config
        .sfu_cascade_peers
        .as_deref()
        .unwrap_or(&[])
        .iter()
        .filter_map(|entry| {
            let (did, addr) = entry.split_once('=')?;
            let addr: std::net::SocketAddr = addr.parse().ok()?;
            Some(crate::sfu::GossipPeer {
                did: did.to_string(),
                addr,
            })
        })
        .collect();
    info!(
        "SFU cascade: local_did={}, listen={}, {} peer(s)",
        local_did,
        bind_addr,
        peers.len()
    );
    match crate::sfu::TcpGossip::start(local_did.clone(), max, bind_addr, peers).await {
        Ok(g) => g as std::sync::Arc<dyn crate::sfu::CascadeGossip>,
        Err(e) => {
            warn!(
                "SFU cascade gossip failed to start: {} — running standalone",
                e
            );
            std::sync::Arc::new(crate::sfu::NoopGossip::new(local_did, max))
        }
    }
}

/// For executor binaries: exit once the REST API thread from [`run`] fails or panics, with the
/// code of a refused start ([`exit::ExitReason`]: 78, 79, 80) or 1.
///
/// Without it, a failed API ends only its own thread and leaves a process
/// running with no API. The JS test harness then waits for the
/// "API server starting" line until mocha's 1200 s timeout. Embedders that
/// must outlive a failed API (the launcher) join the handle themselves.
pub fn exit_when_api_fails(api_thread: JoinHandle<Result<(), AnyError>>) {
    std::thread::spawn(move || {
        let (reason, code) = match api_thread.join() {
            Ok(Ok(())) => return,
            Ok(Err(e)) => (format!("{:?}", e), exit::exit_code_of(&e)),
            Err(_) => (String::from("the REST API thread panicked"), 1),
        };
        error!("REST API server failed, exiting with {code}: {reason}");
        exit::exit_now(code);
    });
}

#[cfg(test)]
mod start_tests {
    // Grants that did not load used to panic inside `run`, which the launcher runs in its own
    // process. The failure now comes back through the handle `run` returns.
    #[test]
    fn a_failed_start_reports_its_reason_through_the_handle() {
        let reason = super::failed_start("Could not load the grants: no database".to_string())
            .join()
            .expect("the handle does not panic")
            .unwrap_err();
        assert_eq!(reason.to_string(), "Could not load the grants: no database");
    }
}
