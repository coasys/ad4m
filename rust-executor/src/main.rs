mod globals;
mod graphql;
mod js_core;
mod utils;
mod wallet;

pub mod init;
mod pubsub;

use log::{error, info};
use rust_executor::Ad4mConfig;
use std::env;

use graphql::warp_server::start_server;
use js_core::JsCore;

#[tokio::main]
async fn main() {
    env::set_var("RUST_LOG", "rust_executor=info");
    env_logger::init();

    info!("Starting js_core...");
    let mut js_core_handle = JsCore::start(Ad4mConfig::default()).await;
    js_core_handle.initialized().await;
    info!("js_core initialized.");

    info!("Starting GraphQL...");
    match start_server(js_core_handle).await {
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
