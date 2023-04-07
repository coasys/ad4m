use actix_web::{
    middleware,
    web::{self, Data},
    App, Error, HttpResponse, HttpServer,
};
use deno_core::error::AnyError;
use juniper::RootNode;
use juniper_actix::{graphiql_handler, graphql_handler, playground_handler};
use std::io::Write;

mod graphql_types;
mod mutation_resolvers;
mod query_resolvers;
mod subscription_resolvers;

use mutation_resolvers::*;
use query_resolvers::*;
use subscription_resolvers::*;

use crate::js_core::JsCoreHandle;

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
    playground_handler("/", None).await
}

async fn graphql_route(
    req: actix_web::HttpRequest,
    payload: actix_web::web::Payload,
    schema: web::Data<Schema>,
    deno_connect: web::Data<JsCoreHandle>,
) -> Result<HttpResponse, Error> {
    graphql_handler(&schema, &deno_connect, req, payload).await
}

pub async fn start_server(js_core_handle: JsCoreHandle) -> Result<(), AnyError> {
    schema().as_schema_language();
    let mut file = std::fs::File::create("schema.gql").unwrap();
    file.write_all(schema().as_schema_language().as_bytes())
        .unwrap();

    //Start the server
    let server = HttpServer::new(move || {
        App::new()
            .app_data(Data::new(schema()))
            .app_data(Data::new(js_core_handle.clone()))
            .wrap(middleware::Compress::default())
            .wrap(middleware::Logger::default())
            .service(
                web::resource("/")
                    .route(web::post().to(graphql_route))
                    .route(web::get().to(graphql_route)),
            )
            .service(web::resource("/playground").route(web::get().to(playground_route)))
            .service(web::resource("/graphiql").route(web::get().to(graphiql_route)))
    });
    server
        .bind("127.0.0.1:8080")
        .expect("Could not bind to port 8080")
        .run()
        .await
        .map_err(|e| e.into())
}
