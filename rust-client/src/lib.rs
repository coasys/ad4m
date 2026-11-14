// On wasm32 the whole client runs on one thread: the browser's `Closure`/
// web-sys types aren't `Send`, so every `Arc` shared between the tasks trips
// `arc_with_non_send_sync`. `Rc` cannot replace it — the same code must be
// `Send` on native — and `Arc` on a single thread is sound and costs nothing.
#![cfg_attr(target_arch = "wasm32", allow(clippy::arc_with_non_send_sync))]

use std::sync::Arc;

use agent::AgentClient;
use anyhow::Result;
use expressions::ExpressionsClient;
use languages::LanguagesClient;
use neighbourhoods::NeighbourhoodsClient;
use perspectives::PerspectivesClient;
use runtime::RuntimeClient;
use ws_rpc::WsRpcClient;

extern crate anyhow;
extern crate serde;
extern crate serde_json;
extern crate tokio;
extern crate urlencoding;

pub mod agent;
pub mod batch;
pub mod expressions;
pub mod flow;
pub mod instances;
pub mod languages;
pub mod literal;
pub mod model;
pub mod neighbourhoods;
pub mod perspective_proxy;
pub mod perspectives;
mod rt;
pub mod runtime;
pub mod subject_proxy;
pub mod subscription;
mod transport;
pub mod types;
pub mod ws_rpc;

#[cfg(all(test, not(target_arch = "wasm32")))]
mod drift;
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) mod testkit;

pub struct Ad4mClient {
    pub agent: AgentClient,
    pub languages: LanguagesClient,
    pub neighbourhoods: NeighbourhoodsClient,
    pub perspectives: PerspectivesClient,
    pub expressions: ExpressionsClient,
    pub runtime: RuntimeClient,
    ws: Arc<WsRpcClient>,
}

impl Ad4mClient {
    pub async fn connect(executor_url: String, cap_token: String) -> Result<Self> {
        let ws = Arc::new(WsRpcClient::connect(&executor_url, &cap_token).await?);

        Ok(Self {
            agent: AgentClient::new(ws.clone()),
            languages: LanguagesClient::new(ws.clone()),
            neighbourhoods: NeighbourhoodsClient::new(ws.clone()),
            perspectives: PerspectivesClient::new(ws.clone()),
            expressions: ExpressionsClient::new(ws.clone()),
            runtime: RuntimeClient::new(ws.clone()),
            ws,
        })
    }

    /// Receive the server-push events named in `events` (event type →
    /// perspective uuids, or `null` for all). See `WsRpcClient::watch_events`.
    pub async fn watch_events(
        &self,
        events: serde_json::Value,
    ) -> Result<tokio::sync::broadcast::Receiver<serde_json::Value>> {
        self.ws.watch_events(events).await
    }

    /// The shared RPC socket — `Model`, `FlowApi` and `Batch` take it, so a
    /// caller builds one client and hands pieces of it to each:
    /// `Model::new(client.ws(), uuid, "Recipe")`.
    pub fn ws(&self) -> &Arc<WsRpcClient> {
        &self.ws
    }
}
