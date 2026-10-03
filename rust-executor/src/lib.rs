#[macro_use]
extern crate lazy_static;

pub mod api;
pub mod config;
pub mod email_service;
pub mod entanglement_service;
mod globals;
pub mod helpers;
pub mod holochain_service;
pub mod js_core;
pub mod mcp;
pub mod perspective_snapshot;
pub mod perspectives;
mod prolog_service;
pub mod runtime_service;
pub mod services;
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
pub mod init;
pub mod languages;
pub mod logging;
mod neighbourhoods;
mod pubsub;
use rustls::crypto::aws_lc_rs;
#[cfg(test)]
mod test_utils;
pub mod types;

use std::thread::JoinHandle;

use log::{error, info, warn};
use tokio::sync::oneshot;

use crate::prolog_service::init_prolog_service;
use crate::{
    agent::AgentService, ai_service::AIService, dapp_server::serve_dapp, db::Ad4mDb,
    languages::LanguageController, runtime_service::RuntimeService, utils::find_port,
};
pub use config::Ad4mConfig;

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

/// Runs the REST server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) -> JoinHandle<()> {
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

    // Set up graceful shutdown channel.
    // The sender is stored globally so runtime_quit and signal handlers can trigger shutdown.
    let (shutdown_tx, shutdown_rx) = oneshot::channel::<()>();
    {
        let mut guard = crate::globals::SHUTDOWN_TX.lock().unwrap();
        *guard = Some(shutdown_tx);
    }

    // Spawn a task that listens for OS signals (SIGTERM/SIGINT) and triggers shutdown.
    // This replaces the old ctrlc handler in the CLI binaries with an in-executor handler
    // that allows graceful cleanup of Holochain conductor and databases.
    #[cfg(unix)]
    {
        tokio::spawn(async {
            use tokio::signal;
            let ctrl_c = signal::ctrl_c();
            let mut sigterm = signal::unix::signal(signal::unix::SignalKind::terminate())
                .expect("failed to install SIGTERM handler");

            tokio::select! {
                _ = ctrl_c => info!("Received SIGINT, initiating graceful shutdown..."),
                _ = sigterm.recv() => info!("Received SIGTERM, initiating graceful shutdown..."),
            }

            // Trigger shutdown via the global channel
            if let Some(tx) = crate::globals::SHUTDOWN_TX.lock().unwrap().take() {
                let _ = tx.send(());
            }
        });
    }

    // Spawn the shutdown handler that waits for the signal and cleans up
    tokio::spawn(async move {
        if shutdown_rx.await.is_ok() {
            info!("Shutdown signal received, cleaning up...");

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
            let shutdown_config = crate::config::get_global_config();
            if let Some(ref pid_file) = shutdown_config.pid_file {
                let _ = std::fs::remove_file(pid_file);
                info!("Removed PID file: {}", pid_file);
            }

            info!("Graceful shutdown complete, exiting.");
            std::process::exit(0);
        }
    });

    // Initialize logging for CLI (stdout)
    // Respects RUST_LOG environment variable if set
    crate::logging::init_cli_logging(None);
    config.prepare();

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

    // Store config globally so services (e.g. agent mutation resolvers) can access it
    crate::config::set_global_config(config.clone());

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
                Arc::new(crate::wallet::SharedWallet::new(url.clone(), token.clone()))
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
                Arc::new(crate::db_backend::SharedDb::new(url.clone(), token.clone()))
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
            if internal == admin {
                panic!(
                    "INTERNAL_API_TOKEN must differ from ADMIN_CREDENTIAL in shared mode. \
                     Using the same value collapses two trust boundaries."
                );
            }
        }
    }

    // Create data directories that were previously created by the JS executor's Config.init().
    // These must exist before any service tries to write to them.
    {
        let app_data_path = config
            .app_data_path
            .as_ref()
            .expect("App data path not set in Ad4mConfig");
        let base = std::path::Path::new(app_data_path).join("ad4m");
        let dirs = [
            base.clone(),
            base.join("data"),
            base.join("languages"),
            base.join("languages").join("temp"),
            base.join("h"),
            base.join("h").join("d"),
            base.join("h").join("c"),
        ];
        for dir in &dirs {
            if let Err(e) = std::fs::create_dir_all(dir) {
                error!("Failed to create data directory {:?}: {}", dir, e);
                panic!(
                    "Cannot continue without required data directory {:?}: {}",
                    dir, e
                );
            }
        }
    }

    aws_lc_rs::default_provider()
        .install_default()
        .expect("Failed to install rustls' aws_lc_rs crypto provider");

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

    // Set multi-user mode before starting services to avoid race condition
    if let Some(enable_multi_user) = config.enable_multi_user {
        if enable_multi_user {
            info!("Enabling multi-user mode...");
            Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(true))
                .expect("Failed to enable multi-user mode");
        }
    }

    info!("Initializing AI service...");
    AIService::init_global_instance()
        .await
        .expect("Couldn't initialize AI service");

    info!("Initializing Agent service...");
    AgentService::init_global_instance(config.app_data_path.clone().unwrap());

    // Load agent data from disk (wallet cipher, DID, etc.) if previously initialized.
    // On the old JS-based executor this was done by the JS AgentService calling AGENT.load().
    AgentService::with_mutable_global_instance(|agent_service| {
        if agent_service.is_initialized() {
            agent_service.load();
            info!("Agent loaded from disk");
        } else {
            info!("Agent not yet initialized (first run)");
        }
    });

    // Spawn background task to clean up expired verification codes every 5 minutes
    tokio::spawn(async {
        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(300)).await;
            if let Err(e) = Ad4mDb::with_global_instance(|db| db.cleanup_expired_codes()) {
                error!("Failed to cleanup expired verification codes: {}", e);
            } else {
                info!("Cleaned up expired verification codes");
            }
        }
    });

    info!("Initializing Runtime service...");
    RuntimeService::init_global_instance(
        std::path::Path::new(&config.app_data_path.clone().unwrap().to_string())
            .join("mainnet_seed.seed")
            .to_string_lossy()
            .into_owned(),
    );

    agent::capabilities::apps_map::set_data_file_path(
        config
            .app_data_path
            .as_ref()
            .map(|path| {
                std::path::Path::new(path)
                    .join("apps_data.json")
                    .to_string_lossy()
                    .into_owned()
            })
            .expect("App data path not set in Ad4mConfig"),
    );

    if config
        .admin_credential
        .as_deref()
        .map(|s| s.is_empty())
        .unwrap_or(true)
    {
        warn!("╔══════════════════════════════════════════════════════════════╗");
        warn!("║  SECURITY WARNING: no adminCredential configured             ║");
        warn!("║  Every request — including unauthenticated ones — receives  ║");
        warn!("║  ALL_CAPABILITY (full admin access to this executor).        ║");
        warn!("║  This mode is intended for local testing ONLY.               ║");
        warn!("║  Set adminCredential in your config before going to prod.    ║");
        warn!("╚══════════════════════════════════════════════════════════════╝");
    }

    {
        info!("Initializing Prolog service...");
        init_prolog_service().await;
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

    LanguageController::init_global_instance();

    // NOTE: system languages are loaded from the agent.generate/agent.unlock handlers:
    // the core ones inline, the conductor-dependent ones in `agent::conductor_startup`.

    // Set app data path for perspectives module
    perspectives::set_app_data_path(config.app_data_path.clone().unwrap());

    perspectives::initialize_from_db();

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

    // The Holochain signal pipeline and Unyt's background work run inside
    // their services (`services::builtins`), started with the API server.

    // Check if MCP mode is enabled — run MCP server alongside REST API
    if config.enable_mcp == Some(true) {
        info!("Starting MCP server alongside REST API...");
        let admin_credential = config.admin_credential.clone();
        // Cloned out here, not read inside the closure: a non-Copy field read
        // in there would move `config`, which the API server thread below
        // still needs.
        let mcp_tls = config.tls.clone();

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
                ..Default::default()
            };
            if let Err(e) = runtime.block_on(mcp::start_mcp_server(
                admin_credential,
                None, // No pre-set auth token
                mcp_config,
            )) {
                error!("MCP server error: {:?}", e);
            }
        });
    }

    info!("Starting REST API server...");

    std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .thread_name(String::from("rest_server"))
            .enable_all()
            .build()
            .unwrap();
        runtime.block_on(api::start_server(config)).unwrap();
    })
}
