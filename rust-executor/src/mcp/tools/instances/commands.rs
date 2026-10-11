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
use crate::mcp::tools::perspective_error;
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
        match self
            .call_method("perspective.executeCommands", params)
            .await
        {
            Ok(_) => pretty(&json!({
                "executed": true,
                "perspective_id": p.perspective_id,
                "expression_address": p.expression_address
            })),
            Err(e) if e.code == 500 => {
                error_json(format!("Error executing commands: {}", e.message))
            }
            Err(e) => perspective_error(&p.perspective_id, e),
        }
    }
}

/// The tool answers as its RPC method's guard does (#851).
#[cfg(test)]
mod guard_tests {
    use super::*;
    use crate::agent::capabilities::{
        perspective_update_capability, PERSPECTIVE_CREATE_CAPABILITY,
    };
    use crate::mcp::tools::test_support::{app, handler_as, perspective, setup, status};

    // The tool checked PERSPECTIVE_CREATE; the method checks the update capability on the
    // perspective.
    #[tokio::test]
    async fn execute_commands_needs_the_update_capability() {
        setup();
        let uuid = perspective(None).await;
        let run = |caps| {
            let uuid = uuid.clone();
            async move {
                handler_as(app(caps))
                    .execute_commands(Parameters(ExecuteCommandsParams {
                        perspective_id: uuid,
                        commands: r#"[{"action":"addLink","source":"this","predicate":"test://p","target":"test://t"}]"#.into(),
                        expression_address: "test://subject".into(),
                        parameters: None,
                    }))
                    .await
            }
        };
        let refused = run(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]).await;
        assert_eq!(status(&refused), Some(403), "{refused}");
        let executed = run(vec![perspective_update_capability(vec![uuid.clone()])]).await;
        assert!(executed.contains("\"executed\": true"), "{executed}");
        crate::perspectives::remove_perspective(&uuid).await;
    }
}
