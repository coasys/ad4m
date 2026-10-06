//! Holochain: `holochain.conductor`, operator access to the embedded conductor.

use async_trait::async_trait;
use schemars::JsonSchema;
use serde::Deserialize;
use serde_json::Value;

use super::{internal, params, require_admin, to_value, NoParams};
use crate::holochain_service::{get_holochain_service, maybe_get_holochain_service};
use crate::services::builtin::{
    CallContext, ServiceError, ServiceHealth, ServiceImplementation, StartContext,
};
use crate::services::interface::{Risk, Selection};
use crate::services::schema_export::{InterfaceBuilder, MethodOptions};

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct AgentInfosParams {
    pub agent_infos: Vec<String>,
}

pub fn conductor_interface() -> Value {
    InterfaceBuilder::new(
        "holochain.conductor",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "The executor's embedded Holochain conductor.",
    )
    .action(
        "READ",
        "See Holochain peers",
        "Read the conductor's agent infos and network metrics.",
        Risk::Safe,
    )
    .action(
        "PEERS",
        "Add Holochain peers",
        "Tell the conductor about other agents.",
        Risk::Write,
    )
    .action(
        "ADMIN",
        "Operate Holochain",
        "Restart the conductor.",
        Risk::Admin,
    )
    .method::<NoParams, Vec<String>>(
        "agentInfos",
        "READ",
        "The conductor's agent infos (encoded).",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<AgentInfosParams, bool>(
        "addAgentInfos",
        "PEERS",
        "Add other agents' infos to the conductor.",
        MethodOptions::default(),
    )
    .method::<NoParams, String>(
        "networkMetrics",
        "READ",
        "The conductor's network metrics (JSON text).",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<NoParams, bool>(
        "restart",
        "ADMIN",
        "Restart the conductor from its stored configuration. Needs the admin credential.",
        MethodOptions {
            long: true,
            ..Default::default()
        },
    )
    .build()
}

pub struct Holochain;

fn enabled() -> Result<(), ServiceError> {
    let enabled = crate::config::try_get_global_config()
        .and_then(|c| c.run_holochain)
        .unwrap_or(true);
    if enabled {
        Ok(())
    } else {
        Err(ServiceError::Unavailable(
            "Holochain is disabled on this executor (run_holochain=false)".into(),
        ))
    }
}

#[async_trait]
impl ServiceImplementation for Holochain {
    async fn start(&self, _ctx: StartContext) -> Result<(), String> {
        Ok(())
    }

    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }

    async fn health(&self) -> ServiceHealth {
        if enabled().is_err() {
            return ServiceHealth::Degraded("Holochain is disabled on this executor".into());
        }
        match maybe_get_holochain_service().await {
            Some(_) => ServiceHealth::Running,
            None => ServiceHealth::Degraded("conductor not started yet".into()),
        }
    }

    async fn call(&self, method: &str, p: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        enabled()?;
        match method {
            "agentInfos" => to_value(
                get_holochain_service()
                    .await
                    .agent_infos()
                    .await
                    .map_err(internal)?,
            ),
            "addAgentInfos" => {
                let p: AgentInfosParams = params(p)?;
                get_holochain_service()
                    .await
                    .add_agent_infos(p.agent_infos)
                    .await
                    .map_err(internal)?;
                to_value(true)
            }
            "networkMetrics" => to_value(
                get_holochain_service()
                    .await
                    .get_network_metrics()
                    .await
                    .map_err(internal)?,
            ),
            "restart" => {
                require_admin(&ctx)?;
                // Shuts the running conductor down, waits for its port, and
                // starts it again from the stored config.
                crate::holochain_service::HolochainService::restart_service()
                    .await
                    .map_err(|e| internal(format!("Holochain restart failed: {e}")))?;
                to_value(true)
            }
            other => Err(internal(format!("holochain has no method {}", other))),
        }
    }
}
