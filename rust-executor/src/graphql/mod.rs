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
use crate::Ad4mConfig;
use crate::agent::auth::capabilities_from_token;

use std::collections::HashMap;
use std::sync::Arc;
use std::{convert::Infallible, io::Write};

use deno_core::error::AnyError;
use futures::FutureExt as _;
use juniper::{InputValue, RootNode};
use juniper_graphql_transport_ws::ConnectionConfig;
use juniper_warp::{playground_filter, subscriptions::serve_graphql_transport_ws};
use warp::{http::Response, Filter};
use std::path::Path;

impl juniper::Context for RequestContext {}

type Schema = RootNode<'static, Query, Mutation, Subscription>;

fn schema() -> Schema {
    Schema::new(Query, Mutation, Subscription)
}

pub async fn start_server(js_core_handle: JsCoreHandle, config: Ad4mConfig) -> Result<(), AnyError> {
    let port = config.gql_port.expect("Did not get gql port");
    let app_data_path = config.app_data_path.expect("Did not get app data path");
    let log = warp::log("warp::server");
    let admin_credential = config.admin_credential.clone();

    let mut file = std::fs::File::create(
        Path::new(&app_data_path).join("schema.gql")
    ).unwrap();

    file.write_all(schema().as_schema_language().as_bytes()).unwrap();

    let homepage = warp::path::end().map(|| {
        Response::builder()
            .header("content-type", "text/html")
            .body("<html><h1>AD4M Executor</h1><div>visit <a href=\"/playground\">graphql playground</a> to explore the executor</html>")
    });

    let qm_schema = schema();
    let js_core_handle_cloned1 = js_core_handle.clone();

    let default_auth = warp::any().map(|| {
        String::from("")
    });

    let qm_state = warp::any()
        .and(warp::header::<String>("authorization"))
        .or(default_auth)
        .unify()
        .map(move |auth_header| {
            //println!("Request body: {}", std::str::from_utf8(body_data::bytes()).expect("error converting bytes to &str"));
            let capabilities = capabilities_from_token(auth_header, admin_credential.clone());
            RequestContext {
                capabilities,
                js_handle: js_core_handle_cloned1.clone(),
            }
        });
    let qm_graphql_filter = juniper_warp::make_graphql_filter(qm_schema, qm_state.boxed());

    let root_node = Arc::new(schema());

    let admin_credential_arc = Arc::new(config.admin_credential.clone());
    let routes = (warp::path("graphql")
        .and(warp::ws())
        .map(move |ws: warp::ws::Ws| {
            let root_node = root_node.clone();
            let js_core_handle = js_core_handle.clone();
            let admin_credential_arc = admin_credential_arc.clone();
            ws.on_upgrade(move |websocket| async move {
                serve_graphql_transport_ws(
                    websocket,
                    root_node,
                    |val: HashMap<String, InputValue>| async move {
                        let mut auth_header = String::from("");

                        if let Some(headers) = val.get("headers") {
                            let headers = headers.to_object_value().unwrap();
                            if let Some(auth) = headers.get("authorization") {
                                auth_header = match auth.as_string_value() {
                                    Some(s) => s.to_string(),
                                    None => String::from(""),
                                };
                            }
                        };

                        let capabilities = capabilities_from_token(auth_header, admin_credential_arc.as_ref().clone());

                        let context = RequestContext {
                            capabilities,
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
