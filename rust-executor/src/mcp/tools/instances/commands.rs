//! `execute_commands` — run raw SDNA actions against an instance.
//!
//! The escape hatch below the typed `instance_*` tools: a JSON array of
//! `{action, source, predicate, target}` commands (the same shape SDNA
//! constructors / setters are written in) applied to one instance, with
//! `this` / `value` substitution. Kept on the static surface because it is
//! the only way over MCP to run an action the class declares but no
//! `instance_*` tool models (custom actions, local-only links, …).
//!
//! It calls the RPC method `perspective.executeCommands` through the handler map, so the
//! method's guard applies.

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

        // The method parses both again; parsing here keeps the tool's own messages.
        if let Err(e) = serde_json::from_str::<Vec<Command>>(&p.commands) {
            return error_json(format!("Error parsing commands JSON: {e}"));
        }
        if let Some(parameters) = &p.parameters {
            if let Err(e) = serde_json::from_str::<Vec<Parameter>>(parameters) {
                return error_json(format!("Error parsing parameters JSON: {e}"));
            }
        }

        let params = json!({
            "uuid": p.perspective_id,
            "commands": p.commands,
            "expression": p.expression_address,
            "parameters": p.parameters,
        });
        let executed = self
            .call::<serde_json::Value>("perspective.executeCommands", params, |e| {
                error_json(format!("Error executing commands: {}", e.message))
            })
            .await;
        match executed {
            Ok(_) => pretty(&json!({
                "executed": true,
                "perspective_id": p.perspective_id,
                "expression_address": p.expression_address
            })),
            Err(e) => e,
        }
    }
}
