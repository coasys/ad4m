//! Flow (state machine) tools
//!
//! Tools for managing flows — finite state machines that track expression state transitions.

use super::Ad4mMcpHandler;
use crate::perspectives::perspective_instance::SdnaType;
use crate::types::LinkQuery;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for adding a flow definition
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddFlowParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// SHACL flow definition as JSON string
    pub shacl_json: String,
}

/// Parameters for listing flows
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetFlowsParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for flow operations on an expression
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowExprParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
}

/// Parameters for running a flow action
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowRunActionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
    /// Action name to execute
    pub action_name: String,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Add a flow (state machine definition) to a perspective
    #[tool(
        description = "Register a flow (finite state machine) in a perspective. Flows define states and transitions for expressions."
    )]
    pub async fn add_flow(&self, params: Parameters<AddFlowParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                match perspective
                    .add_sdna(
                        p.flow_name.clone(),
                        String::new(),
                        SdnaType::Flow,
                        Some(p.shacl_json.clone()),
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "success": true,
                            "perspective_id": p.perspective_id,
                            "flow_name": p.flow_name,
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding flow: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// List all flows defined in a perspective
    #[tool(description = "Get all flow (state machine) definitions registered in a perspective.")]
    pub async fn get_flows(&self, params: Parameters<GetFlowsParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                match perspective
                    .get_links(&LinkQuery {
                        source: Some("ad4m://self".to_string()),
                        predicate: Some("ad4m://has_flow".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => {
                        let flow_names: Vec<String> =
                            links.iter().map(|l| l.data.target.clone()).collect();
                        serde_json::to_string_pretty(&json!({
                            "flows": flow_names,
                            "count": flow_names.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying flows: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get the current state of an expression in a flow
    #[tool(
        description = "Get the current state of an expression within a flow (state machine). Returns the state name and value."
    )]
    pub async fn flow_state(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                match perspective
                    .get_links(&LinkQuery {
                        source: Some(Self::encode_literal(&p.flow_name)),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => {
                        for link in &links {
                            if link
                                .data
                                .predicate
                                .as_ref()
                                .map_or(false, |p| p.contains("stateCheck"))
                            {
                                if let Ok(state_links) = perspective
                                    .get_links(&LinkQuery {
                                        source: Some(p.expression_address.clone()),
                                        predicate: link.data.predicate.clone(),
                                        target: Some(link.data.target.clone()),
                                        ..Default::default()
                                    })
                                    .await
                                {
                                    if !state_links.is_empty() {
                                        return serde_json::to_string_pretty(&json!({
                                            "expression": p.expression_address,
                                            "flow": p.flow_name,
                                            "state": link.data.source.clone(),
                                        }))
                                        .unwrap_or_else(|e| format!("Error: {}", e));
                                    }
                                }
                            }
                        }
                        format!(
                            "Expression {} is not in any state of flow {}",
                            p.expression_address, p.flow_name
                        )
                    }
                    Err(e) => format!("Error querying flow state: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get available actions for an expression in a flow
    #[tool(
        description = "Get the available transition actions for an expression in its current flow state."
    )]
    pub async fn flow_actions(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                match perspective
                    .get_links(&LinkQuery {
                        source: Some(Self::encode_literal(&p.flow_name)),
                        predicate: Some("ad4m://flow_transition".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => {
                        let actions: Vec<String> =
                            links.iter().map(|l| l.data.target.clone()).collect();
                        serde_json::to_string_pretty(&json!({
                            "expression": p.expression_address,
                            "flow": p.flow_name,
                            "available_actions": actions,
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying flow actions: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Start a flow on an expression
    #[tool(
        description = "[NOT YET IMPLEMENTED] Start a flow (state machine) on an expression, putting it into the initial state. Requires SHACLFlow parsing which is not yet available."
    )]
    pub async fn flow_start(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;
        json!({"error": "flow_start is not yet implemented — requires SHACLFlow parsing in Rust", "expression": p.expression_address, "flow": p.flow_name}).to_string()
    }

    /// Execute a transition action on an expression in a flow
    #[tool(
        description = "[NOT YET IMPLEMENTED] Execute a transition action on an expression within a flow (state machine). Requires SHACLFlow command execution which is not yet available."
    )]
    pub async fn flow_run_action(&self, params: Parameters<FlowRunActionParams>) -> String {
        let p = &params.0;
        json!({"error": "flow_run_action is not yet implemented — requires SHACLFlow command execution", "expression": p.expression_address, "flow": p.flow_name, "action": p.action_name}).to_string()
    }
}
