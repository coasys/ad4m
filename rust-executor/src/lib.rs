#[cfg(test)]

mod config;
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
#[macro_use]
extern crate rust_embed;

use std::env;
use tracing::{info, error};

use js_core::JsCore;

pub use config::Ad4mConfig;
pub use holochain_service::run_local_hc_services;

use crate::{prolog_service::init_prolog_service, dapp_server::serve_dapp};

/// Runs the GraphQL server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) {
    env::set_var("RUST_LOG", "rust_executor=info,warp::server");
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
        tokio::task::spawn_blocking(move || {
            let result = serve_dapp(4200);
            tokio::runtime::Handle::current().block_on(async {
                match result.await {
                    Ok(_) => {
                        info!("GraphQL server stopped.");
                        std::process::exit(0);
                    }
                    Err(err) => {
                        error!("GraphQL server stopped with error: {}", err);
                        std::process::exit(1);
                    }
                }
            });
        });
    }

    match graphql::start_server(
        js_core_handle,
        config.gql_port.expect("Did not get gql port"),
    )
    .await
    {
        Ok(_) => {
            info!("GraphQL server stopped.");
            std::process::exit(0);
        }
        Err(err) => {
            error!("GraphQL server stopped with error: {}", err);
            std::process::exit(1);
        }
    };
}

/// Runs the GraphQL server and the deno core runtime
pub async fn run_with_tokio(mut config: Ad4mConfig) {
    env::set_var("RUST_LOG", "rust_executor=info,warp::server");
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
        tokio::task::spawn_blocking(move || {
            let result = serve_dapp(4200);
            tokio::runtime::Handle::current().block_on(async {
                match result.await {
                    Ok(_) => {
                        info!("GraphQL server stopped.");
                        std::process::exit(0);
                    }
                    Err(err) => {
                        error!("GraphQL server stopped with error: {}", err);
                        std::process::exit(1);
                    }
                }
            });
        });
    }

    tokio::task::spawn_blocking(move || {
        let result = graphql::start_server(
            js_core_handle,
            config.gql_port.expect("Did not get gql port"),
        );
        tokio::runtime::Handle::current().block_on(async {
            match result.await {
                Ok(_) => {
                    info!("GraphQL server stopped.");
                    std::process::exit(0);
                }
                Err(err) => {
                    error!("GraphQL server stopped with error: {}", err);
                    std::process::exit(1);
                }
            }
        });
    });
}