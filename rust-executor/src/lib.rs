#[macro_use]
extern crate lazy_static;

pub mod config;
pub mod email_service;
pub mod entanglement_service;
mod globals;
pub mod graphql;
pub mod holochain_service;
pub mod js_core;
mod prolog_service;
pub mod runtime_service;
mod surreal_service;
pub mod utils;
mod wallet;

pub mod agent;
pub mod ai_service;
mod dapp_server;
pub mod db;
pub mod init;
pub mod languages;
pub mod logging;
mod neighbourhoods;
pub mod perspectives;
mod pubsub;
use rustls::crypto::aws_lc_rs;
#[cfg(test)]
mod test_utils;
pub mod types;

use std::thread::JoinHandle;

use log::{error, info, warn};

use crate::{
    agent::AgentService, ai_service::AIService, dapp_server::serve_dapp, db::Ad4mDb,
    languages::LanguageController, prolog_service::init_prolog_service,
    runtime_service::RuntimeService, utils::find_port,
};
pub use config::Ad4mConfig;
pub use holochain_service::run_local_hc_services;
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
                        let maybe_lang_address: Option<String> = {
                            let handlers = js_core::languages_extension::HOLOCHAIN_SIGNAL_HANDLERS
                                .read()
                                .await;
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
                }
            } else {
                tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
            }
        } else {
            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        }
    }
}

/// Runs the GraphQL server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) -> JoinHandle<()> {
    #[cfg(unix)]
    unsafe {
        let mut action: sigaction = std::mem::zeroed();
        action.sa_flags = SA_ONSTACK;
        action.sa_sigaction = handle_sigurg as sighandler_t;
        sigemptyset(&mut action.sa_mask);

        if libc::sigaction(SIGURG, &action, ptr::null_mut()) != 0 {
            eprintln!("Failed to set up SIGURG signal handler");
        }
    }

    // Initialize logging for CLI (stdout)
    // Respects RUST_LOG environment variable if set
    crate::logging::init_cli_logging(None);
    config.prepare();

    // Store config globally so services (e.g. agent mutation resolvers) can access it
    crate::config::set_global_config(config.clone());

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
                panic!("Cannot continue without required data directory {:?}: {}", dir, e);
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

    if let Some(admin_credential) = &config.admin_credential {
        if admin_credential.is_empty() {
            warn!(
                "adminCredential is not set or empty, empty token will possess admin capabilities."
            );
        }
    } else {
        warn!("adminCredential is not set or empty, empty token will possess admin capabilities.");
    }

    info!("Initializing Prolog service...");
    init_prolog_service().await;

    find_and_set_port(&mut config.gql_port, 4000, "GraphQL");
    find_and_set_port(&mut config.hc_admin_port, 2000, "Holochain admin");
    find_and_set_port(&mut config.hc_app_port, 1337, "Holochain app");

    // Initialize V8 platform for multi-threaded use (must happen before any Deno workers)
    {
        use std::sync::Once;
        static V8_FLAGS_INIT: Once = Once::new();
        V8_FLAGS_INIT.call_once(|| {
            deno_core::v8::V8::set_flags_from_string("--no-opt");
            deno_core::JsRuntime::init_platform(None, false);
        });
    }

    // Set languages directory based on app data path (must be before LanguageController)
    crate::utils::set_languages_directory(config.app_data_path.as_ref().unwrap());

    LanguageController::init_global_instance();

    // NOTE: load_system_languages() is called directly from Rust in
    // agent_generate/agent_unlock mutation resolvers.

    // Set app data path for perspectives module (needed for file-based SurrealDB)
    perspectives::set_app_data_path(config.app_data_path.clone().unwrap());

    perspectives::initialize_from_db();

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

    // Start holochain signal receiver as standalone task
    tokio::spawn(crate::holochain_signal_receiver());

    info!("Starting GraphQL...");

    std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .thread_name(String::from("graphql_server"))
            .enable_all()
            .build()
            .unwrap();
        runtime.block_on(graphql::start_server(config)).unwrap();
    })
}
