mod config;
mod globals;
mod graphql;
mod js_core;
mod utils;
mod wallet;

pub mod init;
mod pubsub;

use log::{error, info};
use std::env;

//use graphql::start_server;
use js_core::JsCore;

pub use config::Ad4mConfig;

/// Runs the GraphQL server and the deno core runtime
pub async fn run(mut config: Ad4mConfig) {
    env::set_var("RUST_LOG", "rust_executor=trace,warp::server");
    let _ = env_logger::try_init();
    config.prepare();

    info!("Starting js_core...");
    let mut js_core_handle = JsCore::start(config.clone()).await;
    js_core_handle.initialized().await;
    info!("js_core initialized.");

    info!("Starting GraphQL...");
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
    }
}
