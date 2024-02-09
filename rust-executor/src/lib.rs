pub mod config;
mod globals;
pub mod graphql;
mod holochain_service;
mod js_core;
mod prolog_service;
mod utils;
mod wallet;
use tokio;


pub mod init;
mod pubsub;
mod dapp_server;

use std::{env, thread::JoinHandle};
use log::info;

use js_core::JsCore;

pub use config::Ad4mConfig;
pub use holochain_service::run_local_hc_services;

use crate::{prolog_service::init_prolog_service, dapp_server::serve_dapp};

/// Runs the GraphQL server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) -> JoinHandle<()> {
    env::set_var("RUST_LOG", "holochain=warn,wasmer_compiler_cranelift=warn,rust_executor=debug,warp::server");
    let _ = env_logger::try_init();
    config.prepare();

    info!("Initializing Prolog service...");
    init_prolog_service().await;

    info!("Starting js_core...");
    let mut js_core_handle = JsCore::start(config.clone()).await;
    js_core_handle.initialized().await;
    info!("js_core initialized.");

    info!("Starting GraphQL...");

    if config.run_dapp_server.unwrap() {
        std::thread::spawn(|| {
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .thread_name(String::from("dapp_server"))
                .enable_all()
                .build()
                .unwrap();
            runtime.block_on(serve_dapp(8080)).unwrap();
        });
    };

    std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .thread_name(String::from("graphql_server"))
            .enable_all()
            .build()
            .unwrap();
        runtime.block_on(graphql::start_server(
            js_core_handle,
            config.gql_port.expect("Did not get gql port"),
            config.app_data_path.expect("Did not get app data path")
        )).unwrap();
    })
}