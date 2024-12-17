#[macro_use]
extern crate lazy_static;

pub mod config;
mod entanglement_service;
mod globals;
pub mod graphql;
mod holochain_service;
mod js_core;
mod prolog_service;
mod runtime_service;
mod utils;
mod wallet;

pub mod agent;
pub mod ai_service;
mod dapp_server;
mod db;
pub mod init;
pub mod languages;
mod neighbourhoods;
pub mod perspectives;
mod pubsub;
#[cfg(test)]
mod test_utils;
pub mod types;

use std::{env, thread::JoinHandle};

use log::{error, info, warn};

use js_core::JsCore;

use crate::{
    agent::AgentService, ai_service::AIService, dapp_server::serve_dapp, db::Ad4mDb,
    languages::LanguageController, prolog_service::init_prolog_service,
    runtime_service::RuntimeService,
};
pub use config::Ad4mConfig;
pub use holochain_service::run_local_hc_services;
use libc::{sigaction, sigemptyset, sighandler_t, SA_ONSTACK, SIGURG};
use std::ptr;

extern "C" fn handle_sigurg(_: libc::c_int) {
    //println!("Received SIGURG signal, but ignoring it.");
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

    env::set_var(
        "RUST_LOG",
        "holochain=warn,wasmer_compiler_cranelift=warn,rust_executor=debug,warp::server",
    );
    let _ = env_logger::try_init();
    config.prepare();

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
