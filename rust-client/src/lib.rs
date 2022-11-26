use std::sync::Arc;

use agent::AgentClient;
use languages::LanguagesClient;
use neighbourhoods::NeighbourhoodsClient;
use perspectives::PerspectivesClient;
use runtime::RuntimeClient;

extern crate anyhow;
extern crate async_tungstenite;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate graphql_client;
extern crate rand;
extern crate regex;
extern crate reqwest;
extern crate rustyline;
extern crate tokio;

pub mod agent;
pub mod languages;
pub mod literal;
pub mod neighbourhoods;
pub mod perspectives;
pub mod perspective_proxy;
pub mod runtime;
pub mod types;
mod util;

pub struct Ad4mClient {
    pub agent: AgentClient,
    pub languages: LanguagesClient,
    pub neighbourhoods: NeighbourhoodsClient,
    pub perspectives: PerspectivesClient,
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
            runtime: RuntimeClient::new(info),
        }
    }
}
