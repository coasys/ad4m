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
pub mod expressions;
pub mod languages;
pub mod literal;
pub mod neighbourhoods;
pub mod perspective_proxy;
pub mod perspectives;
pub mod runtime;
pub mod subject_proxy;
pub mod types;
pub mod ws_rpc;

pub struct Ad4mClient {
    pub agent: AgentClient,
    pub languages: LanguagesClient,
    pub neighbourhoods: NeighbourhoodsClient,
    pub perspectives: PerspectivesClient,
    pub expressions: ExpressionsClient,
    pub runtime: RuntimeClient,
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
            runtime: RuntimeClient::new(ws),
        })
    }
}
