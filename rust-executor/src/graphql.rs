use actix_web::{
    middleware,
    web::{self, Data},
    App, Error, HttpResponse, HttpServer,
};
use deno_core::error::AnyError;
use juniper::RootNode;
use juniper_actix::{graphiql_handler, graphql_handler, playground_handler};
use std::env;
use std::io::Write;
//use std::sync::Arc;

mod graphql_types;
mod mutation_resolvers;
mod query_resolvers;
mod subscription_resolvers;

use mutation_resolvers::*;
use query_resolvers::*;
use subscription_resolvers::*;

pub struct MyContext;

impl juniper::Context for MyContext {}

type Schema = RootNode<'static, Query, Mutation, Subscription>;

fn schema() -> Schema {
    Schema::new(Query, Mutation, Subscription)
}

async fn graphiql_route() -> Result<HttpResponse, Error> {
    graphiql_handler("/graphql", None).await
}

async fn playground_route() -> Result<HttpResponse, Error> {
    playground_handler("/graphql", None).await
}

async fn graphql_route(
    req: actix_web::HttpRequest,
    payload: actix_web::web::Payload,
    schema: web::Data<Schema>,
) -> Result<HttpResponse, Error> {
    graphql_handler(&schema, &(), req, payload).await
}

pub async fn start_server() -> Result<(), AnyError> {
    env::set_var("RUST_LOG", "info");
    env_logger::init();

    //let arc_schema = Arc::new(schema());

    schema().as_schema_language();
    let mut file = std::fs::File::create("schema.gql").unwrap();
    file.write_all(schema().as_schema_language().as_bytes())
        .unwrap();

    //Start the server
    let server = HttpServer::new(move || {
        App::new()
            .app_data(Data::new(schema()))
            .wrap(middleware::Compress::default())
            .wrap(middleware::Logger::default())
            .service(
                web::resource("/graphql")
                    .route(web::post().to(graphql_route))
                    .route(web::get().to(graphql_route)),
            )
            .service(web::resource("/playground").route(web::get().to(playground_route)))
            .service(web::resource("/graphiql").route(web::get().to(graphiql_route)))
    });
    server
        .bind("127.0.0.1:8080")
        .unwrap()
        .run()
        .await
        .map_err(|e| e.into())
}
