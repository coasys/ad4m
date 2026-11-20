//! `crate::config_rpc` on wasm32: there is no config file in the web executor, so
//! `runtime.config` / `runtime.setConfig` are not registered. Only the `config-changed`
//! payload type, for the event list; same shape as native `config_rpc::ConfigChanged`.

use serde::{Deserialize, Serialize};
use ts_rs::TS;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ConfigChanged {
    pub keys: Vec<String>,
    pub restart_pending: bool,
}
