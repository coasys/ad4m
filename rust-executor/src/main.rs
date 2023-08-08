mod globals;
mod graphql;
mod holochain_service;
mod js_core;
mod prolog;
mod prolog_service;
mod utils;
mod wallet;

pub mod init;
mod pubsub;

use tracing::{error, info};
use rust_executor::Ad4mConfig;
use std::env;

use js_core::JsCore;

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    prolog::run();
    env::set_var("RUST_LOG", "rust_executor=info");
    env_logger::try_init();

    let mut config = Ad4mConfig::default();
    config.prepare();
    info!("Starting js_core... with config: {:#?}", config);

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
