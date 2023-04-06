mod graphql;
mod js_core;

use js_core::JsCore;
use graphql::start_server;

#[tokio::main]
async fn main() {
    let mut js_core = JsCore::new();
    let mut js_core_handle = js_core.start();
    js_core_handle.initialized().await;
    println!("js_core initialized.");
    println!("Starting GraphQL...");
    start_server().await;
    //let fut_res = tokio::try_join!(graphql::start_server(), js_core::JsCore::run());
    //if let Err(error) = fut_res {
    //    eprintln!("error: {}", error);
    //}
}
