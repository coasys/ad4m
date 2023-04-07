mod graphql;
mod js_core;
mod wallet;

use std::env;

use graphql::start_server;
use js_core::JsCore;
use log::info;

#[tokio::main]
async fn main() {
    env::set_var("RUST_LOG", "info");
    env_logger::init();

    info!("Starting js_core...");       
    let mut js_core_handle = JsCore::start();
    js_core_handle.initialized().await;
    info!("js_core initialized.");
    info!("Starting GraphQL...");
    start_server(js_core_handle).await;
    //let fut_res = tokio::try_join!(graphql::start_server(), js_core::JsCore::run());
    //if let Err(error) = fut_res {
    //    eprintln!("error: {}", error);
    //}
}
