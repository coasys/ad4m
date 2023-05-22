pub mod graphql_types;
mod mutation_resolvers;
mod query_resolvers;
mod subscription_resolvers;
mod utils;

use graphql_types::RequestContext;
use mutation_resolvers::*;
use query_resolvers::*;
use subscription_resolvers::*;

use crate::js_core::JsCoreHandle;

use std::collections::HashMap;
use std::sync::Arc;
use std::{convert::Infallible, io::Write};

use deno_core::error::AnyError;
use futures::FutureExt as _;
use juniper::{InputValue, RootNode};
use juniper_graphql_transport_ws::ConnectionConfig;
use juniper_warp::{playground_filter, subscriptions::serve_graphql_transport_ws};
use warp::{http::Response, Filter};

impl juniper::Context for RequestContext {}

type Schema = RootNode<'static, Query, Mutation, Subscription>;

fn schema() -> Schema {
    Schema::new(Query, Mutation, Subscription)
}

pub async fn start_server(js_core_handle: JsCoreHandle, port: u16) -> Result<(), AnyError> {
    let log = warp::log("warp::server");

    let mut file = std::fs::File::create("schema.gql").unwrap();
    file.write_all(schema().as_schema_language().as_bytes())
        .unwrap();

    let homepage = warp::path::end().map(|| {
        Response::builder()
            .header("content-type", "text/html")
            .body("<html><h1>AD4M Executor</h1><div>visit <a href=\"/playground\">graphql playground</a> to explore the executor</html>")
    });

    let qm_schema = schema();
    let js_core_handle_cloned1 = js_core_handle.clone();
    let qm_state = warp::any()
        .and(warp::header::<String>("authorization"))
        .map(move |header| RequestContext {
            capability: header,
            js_handle: js_core_handle_cloned1.clone(),
        });
    let qm_graphql_filter = juniper_warp::make_graphql_filter(qm_schema, qm_state.boxed());

    let root_node = Arc::new(schema());

    let routes = (warp::path("graphql")
        .and(warp::ws())
        .map(move |ws: warp::ws::Ws| {
            let root_node = root_node.clone();
            let js_core_handle = js_core_handle.clone();
            ws.on_upgrade(move |websocket| async move {
                serve_graphql_transport_ws(
                    websocket,
                    root_node,
                    |val: HashMap<String, InputValue>| async move {
                        let auth_header = val
                            .get("authorization")
                            .map(|v| v.as_string_value().unwrap_or("").to_string());
                        let context = RequestContext {
                            capability: auth_header.unwrap_or(String::from("")),
                            js_handle: js_core_handle.clone(),
                        };
                        Ok(ConnectionConfig::new(context))
                            as Result<ConnectionConfig<_>, Infallible>
                    },
                )
                .map(|r| {
                    if let Err(e) = r {
                        log::error!("Websocket error: {e}");
                    }
                })
                .await
            })
        }))
    .map(|reply| {
        // TODO#584: remove this workaround
        warp::reply::with_header(reply, "Sec-WebSocket-Protocol", "graphql-transport-ws")
    })
    .or(warp::post()
        .and(warp::path("graphql"))
        .and(qm_graphql_filter))
    .or(warp::get()
        .and(warp::path("playground"))
        .and(playground_filter("/graphql", Some("/subscriptions"))))
    .or(homepage)
    .with(log);

    warp::serve(routes).run(([127, 0, 0, 1], port)).await;
    Ok(())
}
