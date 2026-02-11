//! MCP Tools for AD4M Subject/Model operations
//!
//! These tools expose AD4M's Subject system via MCP, allowing AI agents to
//! work with typed models (get properties, run actions) instead of raw links.

use super::server::McpContext;
use crate::agent::AgentContext;
use crate::perspectives::{all_perspectives, get_perspective};
use crate::perspectives::perspective_instance::{Command, Parameter, SubjectClassOption};
use crate::perspectives::utils::prolog_resolution_to_string;
use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{ServerInfo, ServerCapabilities, ToolsCapability, ProtocolVersion, Implementation},
    tool, tool_handler, tool_router,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Tool Parameter Types
// ============================================================================

/// Parameters for listing perspectives
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListPerspectivesParams {}

/// Parameters for listing subject classes in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListSubjectClassesParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for querying subject instances
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QuerySubjectsParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Message", "Channel", "Todo")
    pub class_name: String,
    /// Optional Prolog query for filtering
    pub query: Option<String>,
}

/// Parameters for getting subject data
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectDataParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address (subject instance ID)
    pub expression_address: String,
}

/// Parameters for creating a subject
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct CreateSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address for the new subject
    pub expression_address: String,
    /// Initial property values as JSON string
    pub initial_values: Option<String>,
}

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

/// Parameters for running a Prolog query
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct InferParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Prolog query string
    pub query: String,
}

// ============================================================================
// MCP Handler
// ============================================================================

/// AD4M MCP Handler - exposes Subject operations as MCP tools
#[derive(Clone)]
pub struct Ad4mMcpHandler {
    context: McpContext,
    tool_router: ToolRouter<Self>,
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for Ad4mMcpHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability {
                    list_changed: Some(false),
                }),
                ..Default::default()
            },
            server_info: Implementation {
                name: "ad4m-executor".to_string(),
                title: Some("AD4M Executor MCP".to_string()),
                version: env!("CARGO_PKG_VERSION").to_string(),
                description: Some("AD4M Subject/Model operations via MCP".to_string()),
                icons: None,
                website_url: Some("https://ad4m.dev".to_string()),
            },
            ..Default::default()
        }
    }
}

#[tool_router]
impl Ad4mMcpHandler {
    pub fn new(context: McpContext) -> Self {
        Self {
            context,
            tool_router: Self::tool_router(),
        }
    }

    /// Get auth token from context
    async fn get_auth_token(&self) -> Option<String> {
        self.context.auth_token.read().await.clone()
    }

    // ========================================================================
    // MCP TOOLS
    // ========================================================================

    /// List all perspectives available to the current user
    #[tool(description = "List all AD4M perspectives available to the current user")]
    async fn list_perspectives(&self, _params: Parameters<ListPerspectivesParams>) -> String {
        let perspectives = all_perspectives();
        let mut result: Vec<serde_json::Value> = Vec::new();
        for p in perspectives.iter() {
            let handle = p.persisted.lock().await;
            result.push(json!({
                "uuid": handle.uuid,
                "name": handle.name,
                "shared_url": handle.shared_url,
                "has_neighbourhood": handle.neighbourhood.is_some(),
            }));
        }
        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// List all subject classes (model types) defined in a perspective
    #[tool(description = "List all subject classes (model types) defined in a perspective. Returns the available models you can query and create instances of.")]
    async fn list_subject_classes(&self, params: Parameters<ListSubjectClassesParams>) -> String {
        let uuid = &params.0.perspective_id;
        
        match get_perspective(uuid) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                // Use Prolog to get subject classes
                let query = "subject_class(ClassName, _)".to_string();
                match perspective.prolog_query_with_context(query, &agent_context).await {
                    Ok(result) => {
                        prolog_resolution_to_string(result)
                    }
                    Err(e) => format!("Error getting subject classes: {}", e)
                }
            }
            None => format!("Perspective not found: {}", uuid),
        }
    }

    /// Query instances of a subject class with optional filters
    #[tool(description = "Query instances of a subject class (model) with optional Prolog filters. Returns all instances matching the criteria.")]
    async fn query_subjects(&self, params: Parameters<QuerySubjectsParams>) -> String {
        let p = &params.0;
        
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                // Build Prolog query for subject instances
                let query = if let Some(filter) = &p.query {
                    format!(
                        r#"subject_class("{}", C), instance(C, Base), {}"#,
                        p.class_name, filter
                    )
                } else {
                    format!(
                        r#"subject_class("{}", C), instance(C, Base)"#,
                        p.class_name
                    )
                };
                
                match perspective.prolog_query_with_context(query, &agent_context).await {
                    Ok(result) => {
                        prolog_resolution_to_string(result)
                    }
                    Err(e) => format!("Error querying subjects: {}", e)
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get all data (properties) for a specific subject instance
    #[tool(description = "Get all data (properties and values) for a specific subject instance. Returns the complete state of the model instance.")]
    async fn get_subject_data(&self, params: Parameters<GetSubjectDataParams>) -> String {
        let p = &params.0;
        
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                let subject_class: SubjectClassOption = match serde_json::from_value(json!({
                    "className": p.class_name
                })) {
                    Ok(sc) => sc,
                    Err(e) => return format!("Error creating subject class: {}", e),
                };
                
                match perspective.get_subject_data(subject_class, p.expression_address.clone(), &agent_context).await {
                    Ok(data) => data,
                    Err(e) => format!("Error getting subject data: {}", e)
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Create a new subject instance
    #[tool(description = "Create a new subject instance (model object) with optional initial property values.")]
    async fn create_subject(&self, params: Parameters<CreateSubjectParams>) -> String {
        let p = &params.0;
        
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                let subject_class: SubjectClassOption = match serde_json::from_value(json!({
                    "className": p.class_name
                })) {
                    Ok(sc) => sc,
                    Err(e) => return format!("Error creating subject class: {}", e),
                };
                
                let initial_values: Option<serde_json::Value> = 
                    p.initial_values.as_ref().and_then(|v| serde_json::from_str(v).ok());
                
                match perspective.create_subject(
                    subject_class,
                    p.expression_address.clone(),
                    initial_values,
                    None, // batch_id
                    &agent_context,
                ).await {
                    Ok(_) => {
                        let result = json!({
                            "created": true,
                            "perspective_id": p.perspective_id,
                            "class_name": p.class_name,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error creating subject: {}", e)
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Execute commands (actions) on a subject instance
    #[tool(description = "Execute commands (actions) on a subject instance. Commands are JSON arrays of {source, predicate, target, action} objects.")]
    async fn execute_commands(&self, params: Parameters<ExecuteCommandsParams>) -> String {
        let p = &params.0;
        
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                // Parse commands from JSON string
                let commands: Vec<Command> = match serde_json::from_str(&p.commands) {
                    Ok(cmds) => cmds,
                    Err(e) => return format!("Error parsing commands: {}", e),
                };
                
                // Parse parameters from JSON string
                let parameters: Vec<Parameter> = p.parameters.as_ref()
                    .map(|params_str| serde_json::from_str(params_str).unwrap_or_default())
                    .unwrap_or_default();
                
                match perspective.execute_commands(
                    commands,
                    p.expression_address.clone(),
                    parameters,
                    None, // batch_id
                    &agent_context,
                ).await {
                    Ok(_) => {
                        let result = json!({
                            "executed": true,
                            "perspective_id": p.perspective_id,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error executing commands: {}", e)
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Run a Prolog query for complex reasoning
    #[tool(description = "Run a Prolog query on a perspective for complex reasoning and custom queries. Use this for advanced queries not covered by other tools.")]
    async fn infer(&self, params: Parameters<InferParams>) -> String {
        let p = &params.0;
        
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let auth_token = self.get_auth_token().await.unwrap_or_default();
                let agent_context = AgentContext::from_auth_token(auth_token);
                
                match perspective.prolog_query_with_context(p.query.clone(), &agent_context).await {
                    Ok(result) => {
                        prolog_resolution_to_string(result)
                    }
                    Err(e) => format!("Error running query: {}", e)
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }
}
