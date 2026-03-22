use std::sync::Arc;

use agent::AgentClient;
use expressions::ExpressionsClient;
use languages::LanguagesClient;
use neighbourhoods::NeighbourhoodsClient;
use perspectives::PerspectivesClient;
use runtime::RuntimeClient;

extern crate anyhow;
extern crate reqwest;
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
mod util;

pub struct Ad4mClient {
    pub agent: AgentClient,
    pub languages: LanguagesClient,
    pub neighbourhoods: NeighbourhoodsClient,
    pub perspectives: PerspectivesClient,
    pub expressions: ExpressionsClient,
    pub runtime: RuntimeClient,
}

pub struct ClientInfo {
    pub executor_url: String,
    pub cap_token: String,
}

impl Ad4mClient {
    pub fn new(executor_url: String, cap_token: String) -> Self {
        let info = Arc::new(ClientInfo {
            executor_url,
            cap_token,
        });

        Self {
            agent: AgentClient::new(info.clone()),
            languages: LanguagesClient::new(info.clone()),
            neighbourhoods: NeighbourhoodsClient::new(info.clone()),
            perspectives: PerspectivesClient::new(info.clone()),
            expressions: ExpressionsClient::new(info.clone()),
            runtime: RuntimeClient::new(info),
        }
    }
}
