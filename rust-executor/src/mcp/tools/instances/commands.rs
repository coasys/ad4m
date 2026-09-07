//! `execute_commands` — run raw SDNA actions against an instance.
//!
//! The escape hatch below the typed `instance_*` tools: a JSON array of
//! `{action, source, predicate, target}` commands (the same shape SDNA
//! constructors / setters are written in) applied to one instance, with
//! `this` / `value` substitution. Kept on the static surface because it is
//! the only way over MCP to run an action the class declares but no
//! `instance_*` tool models (custom actions, local-only links, …).

use super::{error_json, pretty, Ad4mMcpHandler};
use crate::perspectives::perspective_instance::{Command, Parameter};
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

/// Parameters for executing commands on a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ExecuteCommandsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Commands to execute as JSON string (array of Command objects)
    pub commands: String,
    /// Expression address (subject instance)
    pub expression_address: String,
    /// Optional parameters as JSON string
    pub parameters: Option<String>,
}

impl Ad4mMcpHandler {
    /// Execute commands (actions) on a subject instance
    #[tool(
        description = "Execute commands (actions) on a subject instance. Commands are JSON arrays of {source, predicate, target, action} objects."
    )]
    pub async fn execute_commands(&self, params: Parameters<ExecuteCommandsParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                let commands: Vec<Command> = match serde_json::from_str(&p.commands) {
                    Ok(cmds) => cmds,
                    Err(e) => return error_json(format!("Error parsing commands JSON: {e}")),
                };

                let parameters: Vec<Parameter> = match &p.parameters {
                    Some(params_str) => match serde_json::from_str(params_str) {
                        Ok(parsed) => parsed,
                        Err(e) => return error_json(format!("Error parsing parameters JSON: {e}")),
                    },
                    None => Vec::new(),
                };

                match perspective
                    .execute_commands(
                        commands,
                        p.expression_address.clone(),
                        parameters,
                        None,
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => pretty(&json!({
                        "executed": true,
                        "perspective_id": p.perspective_id,
                        "expression_address": p.expression_address
                    })),
                    Err(e) => error_json(format!("Error executing commands: {e}")),
                }
            }
            Err(e) => e,
        }
    }
}
