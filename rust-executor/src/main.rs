mod js_core;
mod graphql;


#[actix_web::main]
async fn main() {
    let (graphql, js_core) = tokio::join!(graphql::start_server(), js_core::JsCore::run());
    if let Err(error) = graphql.and(js_core) {
        eprintln!("error: {}", error);
    }
}
