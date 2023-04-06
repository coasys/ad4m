mod graphql;
mod js_core;

#[actix_rt::main]
async fn main() {
    let fut_res = tokio::try_join!(graphql::start_server(), js_core::JsCore::run());
    if let Err(error) = fut_res {
        eprintln!("error: {}", error);
    }
}
