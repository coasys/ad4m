#[macro_use]
extern crate lazy_static;

pub mod config;
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
mod db;
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

use js_core::JsCore;

use crate::{
    agent::AgentService, ai_service::AIService, dapp_server::serve_dapp, db::Ad4mDb,
    languages::LanguageController, prolog_service::init_prolog_service,
    runtime_service::RuntimeService, utils::find_port,
};
pub use config::Ad4mConfig;
pub use holochain_service::run_local_hc_services;
use libc::{sigaction, sigemptyset, sighandler_t, SA_ONSTACK, SIGURG};
use std::ptr;

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

/// Runs the GraphQL server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) -> JoinHandle<()> {
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

    info!("Initializing AI service...");
    AIService::init_global_instance()
        .await
        .expect("Couldn't initialize AI service");

    info!("Initializing Agent service...");
    AgentService::init_global_instance(config.app_data_path.clone().unwrap());

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

    info!("Starting js_core...");
    let mut js_core_handle = JsCore::start(config.clone()).await;
    js_core_handle.initialized().await;
    info!("js_core initialized.");

    LanguageController::init_global_instance(js_core_handle.clone());
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

    info!("Starting GraphQL...");

    std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .thread_name(String::from("graphql_server"))
            .enable_all()
            .build()
            .unwrap();
        runtime
            .block_on(graphql::start_server(js_core_handle, config))
            .unwrap();
    })
}
