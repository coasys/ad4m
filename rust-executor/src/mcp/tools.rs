//! MCP Tools for AD4M — Agent-Centric Distributed Application Meta-ontology
//!
//! AD4M provides a subjective graph database where data is stored as **links**
//! (RDF-like triples: source → predicate → target) in **perspectives**
//! (personal knowledge graphs). **Subject classes** (models) define typed
//! schemas using SHACL, giving structure to the raw link graph. **Neighbourhoods**
//! enable shared perspectives synced via Holochain for real-time P2P collaboration.
//!
//! This module exposes AD4M's functionality via MCP (Model Context Protocol),
//! enabling AI agents to work with typed models instead of raw links.

use super::server::McpContext;
use crate::agent::capabilities::{
    capabilities_from_token, check_capability,
    defs::{ALL_CAPABILITY, PERSPECTIVE_CREATE_CAPABILITY},
    generate_capability_token, permit_capability, request_capability as cap_request_capability,
    token::decode_jwt,
    AuthInfo, AuthInfoExtended, Capability,
};
use crate::agent::AgentContext;
use crate::graphql::graphql_types::{LinkQuery, LinkStatus, PerspectiveHandle, PerspectiveState};
use crate::perspectives::perspective_instance::{Command, Parameter, SdnaType, SubjectClassOption};
use crate::perspectives::utils::prolog_resolution_to_string;
use crate::perspectives::{add_perspective, all_perspectives, get_perspective};
use crate::types::Link;
use rmcp::{
    handler::server::{router::tool::ToolRouter, tool::ToolCallContext, wrapper::Parameters},
    model::{
        CallToolRequestParams, CallToolResult, Content, Implementation, ListToolsResult,
        PaginatedRequestParams, ProtocolVersion, ServerCapabilities, ServerInfo, Tool,
        ToolsCapability,
    },
    service::RequestContext,
    tool, tool_router, ErrorData, RoleServer, ServerHandler,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;

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
// Authentication Parameter Types
// ============================================================================

/// Parameters for email/password login (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct LoginEmailParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a capability token (local connect flow)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestCapabilityParams {
    /// Application name requesting access
    pub app_name: String,
    /// Application description
    pub app_desc: String,
    /// Optional application domain
    #[serde(default)]
    pub app_domain: Option<String>,
    /// Optional application URL
    #[serde(default)]
    pub app_url: Option<String>,
}

/// Parameters for generating a JWT from a capability request
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GenerateJwtParams {
    /// Request ID returned from request_capability
    pub request_id: String,
    /// 6-digit code from the executor log
    pub code: String,
}

/// Parameters for user signup (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SignupParams {
    /// User email address
    pub email: String,
    /// User password
    pub password: String,
}

/// Parameters for requesting a login verification code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RequestLoginVerificationParams {
    /// User email address
    pub email: String,
}

/// Parameters for verifying an email code (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct VerifyEmailCodeParams {
    /// User email address
    pub email: String,
    /// 6-digit verification code
    pub code: String,
    /// Type: "signup" or "login"
    pub verification_type: String,
}

/// Parameters for checking authentication status (no params needed)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AuthStatusParams {}

// ============================================================================
// Link & Perspective Parameter Types
// ============================================================================

/// Parameters for adding a link to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddLinkParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Link source URI
    pub source: String,
    /// Link predicate URI
    pub predicate: String,
    /// Link target URI
    pub target: String,
}

/// Parameters for querying links in a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QueryLinksParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Optional source URI filter
    pub source: Option<String>,
    /// Optional predicate URI filter
    pub predicate: Option<String>,
    /// Optional target URI filter
    pub target: Option<String>,
}

/// Parameters for adding SDNA (subject class definition) to a perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddModelParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// SHACL shape definition as JSON string
    pub shacl_json: String,
}

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

/// Parameters for creating a new perspective
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddPerspectiveParams {
    /// Name for the new perspective
    pub name: String,
}

/// Parameters for setting a property on a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetSubjectPropertyParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name (e.g., "Channel", "Message")
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Property name to set (e.g., "name", "body")
    pub property_name: String,
    /// Value to set (will be wrapped as literal if needed)
    pub value: String,
}

/// Parameters for getting a collection from a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance
    pub expression_address: String,
    /// Collection name (e.g., "messages", "members")
    pub collection_name: String,
}

/// Parameters for adding an item to a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddToCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to add
    pub item_address: String,
}

/// Parameters for removing an item from a subject collection
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RemoveFromCollectionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject instance (parent)
    pub expression_address: String,
    /// Collection name
    pub collection_name: String,
    /// Expression address of the item to remove
    pub item_address: String,
}

/// Parameters for getting children of a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetSubjectChildrenParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name of the parent (optional)
    #[serde(default)]
    pub class_name: Option<String>,
    /// Expression address of the parent subject
    pub expression_address: String,
    /// Optional: filter children to only this class name
    pub child_class_name: Option<String>,
}

/// Parameters for deleting a subject instance
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct DeleteSubjectParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// Expression address of the subject to delete
    pub expression_address: String,
}

// ============================================================================
// Agent Profile Parameter Types
// ============================================================================

/// Parameters for getting the agent's public profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetAgentProfileParams {}

/// Parameters for setting the agent's profile
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfileParams {
    /// Display username
    pub username: Option<String>,
    /// Given (first) name
    pub given_name: Option<String>,
    /// Family (last) name
    pub family_name: Option<String>,
    /// Email address
    pub email: Option<String>,
    /// Bio/description text
    pub bio: Option<String>,
}

/// Parameters for setting the agent's profile picture
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetAgentProfilePictureParams {
    /// Base64-encoded image data (raw base64, NOT a data URI)
    pub image_base64: String,
    /// Image MIME type (e.g. "image/png", "image/jpeg"). Defaults to "image/png"
    pub mime_type: Option<String>,
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

impl ServerHandler for Ad4mMcpHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability {
                    list_changed: Some(true),
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

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, ErrorData> {
        let mut tools = self.tool_router.list_all();
        tools.extend(self.generate_dynamic_tools().await);
        Ok(ListToolsResult {
            tools,
            meta: None,
            next_cursor: None,
        })
    }

    async fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<CallToolResult, ErrorData> {
        let tool_name = request.name.to_string();

        if self.tool_router.has_route(&tool_name) {
            let peer = context.peer.clone();
            let tcc = ToolCallContext::new(self, request, context);
            let result = self.tool_router.call(tcc).await?;

            // Notify clients about tool list changes after model/flow is added
            if (tool_name == "add_model" || tool_name == "add_flow")
                && result.is_error != Some(true)
            {
                let _ = peer.notify_tool_list_changed().await;
            }

            return Ok(result);
        }

        // Handle dynamic SHACL-generated tools
        self.handle_dynamic_tool(&tool_name, request.arguments)
            .await
    }

    fn get_tool(&self, name: &str) -> Option<Tool> {
        self.tool_router.get(name).cloned()
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

    /// Get agent context, requiring authentication
    async fn get_agent_context(&self) -> Result<AgentContext, String> {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => Ok(AgentContext::from_auth_token(token)),
            _ => Err("Authentication required. Use request_capability + generate_jwt, login_email, or signup + verify_email_code first.".to_string()),
        }
    }

    /// Get capabilities from the stored auth token (reuses same logic as GraphQL RequestContext)
    async fn get_capabilities(&self) -> Result<Vec<Capability>, String> {
        let token = self.get_auth_token().await;
        let admin_cred = self.context.admin_credential.clone();
        capabilities_from_token(token.unwrap_or_default(), admin_cred)
    }

    /// Store an auth token in the session and return a success JSON response
    async fn store_token_and_respond(
        &self,
        token: String,
        email: Option<&str>,
        message: &str,
    ) -> String {
        let mut token_guard = self.context.auth_token.write().await;
        *token_guard = Some(token.clone());

        let mut resp = json!({
            "success": true,
            "token": token,
            "message": message,
        });
        if let Some(e) = email {
            resp["user_email"] = json!(e);
        }
        resp.to_string()
    }

    /// Get agent context for read operations - allows unauthenticated access for local/main agent
    async fn get_agent_context_for_read(&self) -> AgentContext {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => AgentContext::from_auth_token(token),
            _ => AgentContext::from_auth_token(String::new()),
        }
    }

    /// Get a perspective by ID after checking write capabilities.
    /// Common pattern used by most dynamic tool handlers.
    async fn get_writable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<crate::perspectives::perspective_instance::PerspectiveInstance, String> {
        let capabilities = self.get_capabilities().await;
        check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY)
            .map_err(|e| format!("Capability error: {}", e))?;
        get_perspective(perspective_id)
            .ok_or_else(|| format!("Perspective not found: {}", perspective_id))
    }

    /// Extract a required string argument from the args map
    fn require_arg<'a>(
        args: &'a serde_json::Map<String, serde_json::Value>,
        key: &str,
    ) -> Result<&'a str, String> {
        args.get(key)
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("Missing required parameter: {}", key))
    }

    /// Resolve a literal value from a link target URI.
    /// Handles: literal://string:X, literal://json:{signed expression with "data" field},
    /// and URL-encoded variants.
    fn resolve_literal_value(target: &str) -> String {
        // Simple literal://string: prefix
        if let Some(value) = target.strip_prefix("literal://string:") {
            // URL-decode in case it's encoded
            return urlencoding::decode(value)
                .unwrap_or_else(|_| value.into())
                .to_string();
        }

        // literal://json: — may be a signed expression with "data" field
        if let Some(json_part) = target.strip_prefix("literal://json:") {
            let decoded = urlencoding::decode(json_part)
                .unwrap_or_else(|_| json_part.into())
                .to_string();
            if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&decoded) {
                if let Some(data) = parsed.get("data") {
                    return match data {
                        serde_json::Value::String(s) => s.clone(),
                        other => other.to_string(),
                    };
                }
            }
            return decoded;
        }

        // Fallback: return as-is
        target.to_string()
    }

    /// Escape string for safe use in Prolog queries
    fn escape_prolog_string(s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('"', "\\\"")
            .replace('\'', "\\'")
    }

    // ========================================================================
    // MCP TOOLS
    // ========================================================================

    /// List all perspectives available to the current user
    #[tool(
        description = "List all AD4M perspectives. A perspective is a subjective graph database — a personal collection of links (RDF-like triples: source → predicate → target) that can be queried, modified, and optionally shared as a 'neighbourhood' for real-time P2P collaboration. Each has a UUID and a human-readable name."
    )]
    async fn list_perspectives(&self, _params: Parameters<ListPerspectivesParams>) -> String {
        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

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

    /// Get all models (subject classes) defined in a perspective
    #[tool(
        description = "Get all models (SHACL subject classes) defined in a perspective. Models are schemas that give structure to the raw link graph — like database table definitions. Each model defines typed properties and collections. Use query_subjects to find instances, get_subject_data to read them, or use the dynamic per-class tools (e.g. channel_create, message_get)."
    )]
    async fn get_models(&self, params: Parameters<ListSubjectClassesParams>) -> String {
        let uuid = &params.0.perspective_id;

        match get_perspective(uuid) {
            Some(perspective) => {
                // Query SHACL shapes stored as links: X --rdf://type--> ad4m://SubjectClass
                let links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some("ad4m://SubjectClass".to_string()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(class_links) => {
                        let classes: Vec<String> = class_links
                            .iter()
                            .map(|l| {
                                // Extract class name from source URI (e.g., "flux://Channel" -> "Channel")
                                l.data
                                    .source
                                    .split("://")
                                    .last()
                                    .unwrap_or(&l.data.source)
                                    .to_string()
                            })
                            .collect();
                        serde_json::to_string_pretty(&classes)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error listing subject classes: {}", e),
                }
            }
            None => format!("Perspective not found: {}", uuid),
        }
    }

    /// Query instances of a subject class
    #[tool(
        description = "Query all instances of a subject class (model). Returns expression addresses of all instances. Use get_subject_data to read each instance's properties."
    )]
    async fn query_subjects(&self, params: Parameters<QuerySubjectsParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                // Find the target_class URI from the SHACL shape
                // Shape links: {target_class} --rdf://type--> ad4m://SubjectClass
                // AND: {target_class} --ad4m://shape--> {shapeUri}
                // We match by class name extracted from target_class URI

                // First get all subject classes
                let class_links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some("ad4m://SubjectClass".to_string()),
                        ..Default::default()
                    })
                    .await;

                let target_class = match class_links {
                    Ok(links) => links.iter().find_map(|l| {
                        let name = l.data.source.split("://").last().unwrap_or("");
                        if name == p.class_name {
                            Some(l.data.source.clone())
                        } else {
                            None
                        }
                    }),
                    Err(e) => return format!("Error finding class: {}", e),
                };

                let target_class = match target_class {
                    Some(tc) => tc,
                    None => return format!("Subject class '{}' not found", p.class_name),
                };

                // Find instances: links where source has rdf://type -> target_class
                let instance_links = perspective
                    .get_links(&LinkQuery {
                        predicate: Some("rdf://type".to_string()),
                        target: Some(target_class),
                        ..Default::default()
                    })
                    .await;

                match instance_links {
                    Ok(links) => {
                        // Filter out the class definition itself (ad4m://SubjectClass link)
                        let instances: Vec<String> =
                            links.iter().map(|l| l.data.source.clone()).collect();
                        serde_json::to_string_pretty(&instances)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying instances: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get all data (properties) for a specific subject instance
    #[tool(
        description = "Get all data (properties and values) for a specific subject instance. Returns the complete state of the model instance as a JSON object with property names and values."
    )]
    async fn get_subject_data(&self, params: Parameters<GetSubjectDataParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                // Resolve all properties from SHACL shape and read their values from links
                let name_literal = format!("literal://string:shacl://{}", p.class_name);
                let shape_links = match perspective
                    .get_links(&LinkQuery {
                        source: Some(name_literal),
                        predicate: Some("ad4m://shacl_shape_uri".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => links,
                    Err(e) => return format!("Error querying SHACL shape: {}", e),
                };

                if shape_links.is_empty() {
                    return format!("No SHACL shape found for class '{}'", p.class_name);
                }

                let shape_uri = &shape_links[0].data.target;

                // Get all property shapes
                let prop_links = match perspective
                    .get_links(&LinkQuery {
                        source: Some(shape_uri.clone()),
                        predicate: Some("sh://property".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => links,
                    Err(e) => return format!("Error querying properties: {}", e),
                };

                let mut data = serde_json::Map::new();

                for prop_link in &prop_links {
                    let prop_uri = &prop_link.data.target;

                    // Extract property name from URI: "flux://Channel.name" -> "name"
                    let prop_name = prop_uri
                        .rsplit_once('.')
                        .map(|(_, name)| name.to_string())
                        .unwrap_or_else(|| prop_uri.clone());

                    // Get the predicate path for this property
                    let path_links = match perspective
                        .get_links(&LinkQuery {
                            source: Some(prop_uri.clone()),
                            predicate: Some("sh://path".to_string()),
                            ..Default::default()
                        })
                        .await
                    {
                        Ok(links) => links,
                        Err(_) => continue,
                    };

                    if let Some(path_link) = path_links.first() {
                        let predicate = &path_link.data.target;

                        // Check if this is a collection
                        let is_collection = match perspective
                            .get_links(&LinkQuery {
                                source: Some(prop_uri.clone()),
                                predicate: Some("rdf://type".to_string()),
                                target: Some("ad4m://CollectionShape".to_string()),
                                ..Default::default()
                            })
                            .await
                        {
                            Ok(links) => !links.is_empty(),
                            Err(_) => false,
                        };

                        // Query the actual value links
                        let value_links = match perspective
                            .get_links(&LinkQuery {
                                source: Some(p.expression_address.clone()),
                                predicate: Some(predicate.clone()),
                                ..Default::default()
                            })
                            .await
                        {
                            Ok(links) => links,
                            Err(_) => continue,
                        };

                        if is_collection {
                            let items: Vec<String> =
                                value_links.iter().map(|l| l.data.target.clone()).collect();
                            data.insert(
                                prop_name,
                                serde_json::Value::Array(
                                    items.into_iter().map(serde_json::Value::String).collect(),
                                ),
                            );
                        } else if let Some(link) = value_links.first() {
                            let value = Self::resolve_literal_value(&link.data.target);
                            data.insert(prop_name, serde_json::Value::String(value));
                        }
                    }
                }

                serde_json::to_string_pretty(&serde_json::Value::Object(data))
                    .unwrap_or_else(|e| format!("Error: {}", e))
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Create a new subject instance
    #[tool(
        description = "Create a new subject instance (model object) with optional initial property values."
    )]
    async fn create_subject(&self, params: Parameters<CreateSubjectParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                let subject_class: SubjectClassOption = match serde_json::from_value(json!({
                    "className": p.class_name
                })) {
                    Ok(sc) => sc,
                    Err(e) => return format!("Error creating subject class: {}", e),
                };

                // Parse initial_values and propagate errors instead of swallowing them
                let initial_values: Option<serde_json::Value> = match &p.initial_values {
                    Some(v) => match serde_json::from_str(v) {
                        Ok(parsed) => Some(parsed),
                        Err(e) => return format!("Error parsing initial_values JSON: {}", e),
                    },
                    None => None,
                };

                match perspective
                    .create_subject(
                        subject_class,
                        p.expression_address.clone(),
                        initial_values,
                        None, // batch_id
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "created": true,
                            "perspective_id": p.perspective_id,
                            "class_name": p.class_name,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error creating subject: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Execute commands (actions) on a subject instance
    #[tool(
        description = "Execute commands (actions) on a subject instance. Commands are JSON arrays of {source, predicate, target, action} objects."
    )]
    async fn execute_commands(&self, params: Parameters<ExecuteCommandsParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                // Parse commands from JSON string
                let commands: Vec<Command> = match serde_json::from_str(&p.commands) {
                    Ok(cmds) => cmds,
                    Err(e) => return format!("Error parsing commands JSON: {}", e),
                };

                // Parse parameters from JSON string - propagate errors
                let parameters: Vec<Parameter> = match &p.parameters {
                    Some(params_str) => match serde_json::from_str(params_str) {
                        Ok(parsed) => parsed,
                        Err(e) => return format!("Error parsing parameters JSON: {}", e),
                    },
                    None => Vec::new(),
                };

                match perspective
                    .execute_commands(
                        commands,
                        p.expression_address.clone(),
                        parameters,
                        None, // batch_id
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "executed": true,
                            "perspective_id": p.perspective_id,
                            "expression_address": p.expression_address
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error executing commands: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Run a Prolog query for complex reasoning
    #[tool(
        description = "Run a Prolog query on a perspective for complex reasoning. The link graph is exposed as Prolog facts (triple/3), enabling pattern matching and inference beyond simple link queries. Example: 'triple(X, \"rdf://type\", \"ad4m://SubjectClass\")' finds all subject classes. Use for advanced queries not covered by other tools."
    )]
    async fn infer(&self, params: Parameters<InferParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                match perspective
                    .prolog_query_with_context(p.query.clone(), &agent_context)
                    .await
                {
                    Ok(result) => prolog_resolution_to_string(result),
                    Err(e) => format!("Error running query: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    // ========================================================================
    // LINK & PERSPECTIVE TOOLS
    // ========================================================================

    /// Add a link to a perspective
    #[tool(
        description = "Add a link (RDF-like triple) to a perspective. Links are the fundamental data unit — all data (properties, type markers, collections) is stored as links. Example: source='did:key:abc' predicate='ad4m://name' target='literal://string:Alice'. In shared neighbourhoods, links sync to all members."
    )]
    async fn add_link(&self, params: Parameters<AddLinkParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                let link = Link {
                    source: p.source.clone(),
                    predicate: Some(p.predicate.clone()),
                    target: p.target.clone(),
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(decorated) => {
                        let result = json!({
                            "success": true,
                            "link": {
                                "source": decorated.data.source,
                                "predicate": decorated.data.predicate,
                                "target": decorated.data.target,
                                "timestamp": decorated.timestamp,
                            }
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding link: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Query links in a perspective
    #[tool(
        description = "Query links in a perspective. Links are RDF-like triples with source, predicate, and target. Filter by any combination — omit a filter to match all values for that field. Example: source='expr://abc' with no predicate/target returns all links from that address. Use predicate filter to find specific property values."
    )]
    async fn query_links(&self, params: Parameters<QueryLinksParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let agent_context = self.get_agent_context_for_read().await;
                let _ = &agent_context; // used for consistency, get_links doesn't need it currently

                let query = LinkQuery {
                    source: p.source.clone(),
                    predicate: p.predicate.clone(),
                    target: p.target.clone(),
                    ..Default::default()
                };

                match perspective.get_links(&query).await {
                    Ok(links) => {
                        let result: Vec<serde_json::Value> = links
                            .iter()
                            .map(|l| {
                                json!({
                                    "source": l.data.source,
                                    "predicate": l.data.predicate,
                                    "target": l.data.target,
                                    "timestamp": l.timestamp,
                                    "author": l.author,
                                })
                            })
                            .collect();
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying links: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Add a model (subject class definition) to a perspective
    #[tool(
        description = "Register a model (subject class) using a SHACL JSON definition. This defines the schema — properties, collections, types — for typed objects in the perspective. Once registered, dynamic MCP tools are auto-generated for the class: {class}_create, {class}_get, {class}_set_{property}, {class}_add_{collection}, etc. The tool list updates after registration."
    )]
    async fn add_model(&self, params: Parameters<AddModelParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                match perspective
                    .add_sdna(
                        p.class_name.clone(),
                        String::new(), // no prolog code, using SHACL
                        SdnaType::SubjectClass,
                        Some(p.shacl_json.clone()),
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "success": true,
                            "perspective_id": p.perspective_id,
                            "class_name": p.class_name,
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding SDNA: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Add a flow (state machine definition) to a perspective
    #[tool(
        description = "Register a flow (finite state machine) in a perspective. Flows define states and transitions for expressions."
    )]
    async fn add_flow(&self, params: Parameters<AddFlowParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

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
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// List all flows defined in a perspective
    #[tool(description = "Get all flow (state machine) definitions registered in a perspective.")]
    async fn get_flows(&self, params: Parameters<GetFlowsParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
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
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get the current state of an expression in a flow
    #[tool(
        description = "Get the current state of an expression within a flow (state machine). Returns the state name and value."
    )]
    async fn flow_state(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                // Get flow definition links
                match perspective
                    .get_links(&LinkQuery {
                        source: Some(format!("literal://string:{}", p.flow_name)),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => {
                        // Check state check patterns against expression
                        for link in &links {
                            if link
                                .data
                                .predicate
                                .as_ref()
                                .map_or(false, |p| p.contains("stateCheck"))
                            {
                                // Check if expression matches this state
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
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get available actions for an expression in a flow
    #[tool(
        description = "Get the available transition actions for an expression in its current flow state."
    )]
    async fn flow_actions(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;
        // For now, return all transitions - proper implementation needs flow state detection
        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                match perspective
                    .get_links(&LinkQuery {
                        source: Some(format!("literal://string:{}", p.flow_name)),
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
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Start a flow on an expression
    #[tool(
        description = "Start a flow (state machine) on an expression, putting it into the initial state."
    )]
    async fn flow_start(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                // TODO: Proper flow start requires loading the flow definition
                // and executing its start actions. For now, return a placeholder.
                // The full implementation needs SHACLFlow parsing in Rust.
                serde_json::to_string_pretty(&json!({
                    "success": false,
                    "expression": p.expression_address,
                    "flow": p.flow_name,
                    "message": "Flow start not yet fully implemented - requires SHACLFlow parsing"
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Create a new perspective
    #[tool(
        description = "Create a new perspective (local knowledge graph). Returns the UUID. You can then add links, register models (subject classes), and create typed instances within it. To share it for collaboration, convert it to a neighbourhood."
    )]
    async fn add_perspective(&self, params: Parameters<AddPerspectiveParams>) -> String {
        let p = &params.0;

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        // Check capability (same as GraphQL mutation_resolvers)
        let capabilities = self.get_capabilities().await;
        if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
            return format!("Capability error: {}", e);
        }

        let uuid = uuid::Uuid::new_v4().to_string();
        let handle = PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some(p.name.clone()),
            neighbourhood: None,
            shared_url: None,
            state: PerspectiveState::Private,
            owners: None,
        };

        match add_perspective(handle, None).await {
            Ok(_) => {
                let result = json!({
                    "success": true,
                    "uuid": uuid,
                    "name": p.name,
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!("Error creating perspective: {}", e),
        }
    }

    // ========================================================================
    // SUBJECT PROPERTY & COLLECTION TOOLS (Ad4mModel parity)
    // ========================================================================

    /// Set a property value on a subject instance (like `instance.name = "value"` in JS Ad4mModel)
    #[tool(
        description = "Set a property on a subject instance. Works at the model level — you provide the property name (e.g. 'name', 'body') and the tool handles the underlying link operations. No need to know predicates or link structure."
    )]
    async fn set_subject_property(&self, params: Parameters<SetSubjectPropertyParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                // Look up the SHACL property path for this property name
                // SHACL properties are stored as links: propertyShapeUri --sh://name--> "propertyName"
                // and propertyShapeUri --sh://path--> "predicateUri"
                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.property_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving property '{}': {}", p.property_name, e)
                    }
                };

                // Use setSingleTarget pattern: remove old, add new
                // First remove existing links with this predicate
                let existing = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate.clone()),
                        ..Default::default()
                    })
                    .await;

                if let Ok(links) = existing {
                    for link in links {
                        let _ = perspective.remove_link(link.into(), None).await;
                    }
                }

                // Add new link with the value
                let target = if p.value.starts_with("literal://") || p.value.contains("://") {
                    p.value.clone()
                } else {
                    format!("literal://string:{}", p.value)
                };

                let link = Link {
                    source: p.expression_address.clone(),
                    predicate: Some(predicate),
                    target,
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(_) => serde_json::to_string_pretty(&json!({
                        "success": true,
                        "property": p.property_name,
                        "value": p.value,
                    }))
                    .unwrap_or_else(|e| format!("Error: {}", e)),
                    Err(e) => format!("Error setting property: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get all items in a named collection on a subject instance
    #[tool(
        description = "Get all items in a collection property of a subject instance (e.g., all messages in a channel). Returns a list of expression addresses. Works at the model level — just provide the collection name."
    )]
    async fn get_subject_collection(
        &self,
        params: Parameters<GetSubjectCollectionParams>,
    ) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let _agent_context = self.get_agent_context_for_read().await;

                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(items) => {
                        let targets: Vec<String> =
                            items.iter().map(|l| l.data.target.clone()).collect();
                        serde_json::to_string_pretty(&json!({
                            "collection": p.collection_name,
                            "items": targets,
                            "count": targets.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying collection: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Add an item to a collection on a subject instance
    #[tool(
        description = "Add an item to a collection on a subject instance (e.g., add a message to a channel). Creates the link between parent and child using the correct predicate for the collection."
    )]
    async fn add_to_collection(&self, params: Parameters<AddToCollectionParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                let link = Link {
                    source: p.expression_address.clone(),
                    predicate: Some(predicate),
                    target: p.item_address.clone(),
                };

                match perspective
                    .add_link(link, LinkStatus::Shared, None, &agent_context)
                    .await
                {
                    Ok(_) => serde_json::to_string_pretty(&json!({
                        "success": true,
                        "collection": p.collection_name,
                        "item": p.item_address,
                    }))
                    .unwrap_or_else(|e| format!("Error: {}", e)),
                    Err(e) => format!("Error adding to collection: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Remove an item from a collection on a subject instance
    #[tool(
        description = "Remove an item from a collection on a subject instance. Removes the link between parent and child."
    )]
    async fn remove_from_collection(
        &self,
        params: Parameters<RemoveFromCollectionParams>,
    ) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                let predicate = match self
                    .resolve_property_predicate(&perspective, &p.class_name, &p.collection_name)
                    .await
                {
                    Ok(pred) => pred,
                    Err(e) => {
                        return format!("Error resolving collection '{}': {}", p.collection_name, e)
                    }
                };

                // Find and remove the specific link
                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some(predicate),
                        target: Some(p.item_address.clone()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(found) => {
                        let mut removed = 0;
                        for link in found {
                            if let Ok(_) = perspective.remove_link(link.into(), None).await {
                                removed += 1;
                            }
                        }
                        serde_json::to_string_pretty(&json!({
                            "success": true,
                            "removed": removed,
                            "collection": p.collection_name,
                            "item": p.item_address,
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error finding link to remove: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    // ========================================================================
    // SUBJECT CHILDREN & DELETE TOOLS
    // ========================================================================

    /// Get all children of a subject instance (linked via ad4m://has_child)
    #[tool(
        description = "Get all subjects that are children of a given subject (linked via ad4m://has_child). Optionally filter by child class name. Returns a list of child addresses."
    )]
    async fn get_subject_children(&self, params: Parameters<GetSubjectChildrenParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let _agent_context = self.get_agent_context_for_read().await;

                // Query has_child links from the parent
                let links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        predicate: Some("ad4m://has_child".to_string()),
                        ..Default::default()
                    })
                    .await;

                match links {
                    Ok(child_links) => {
                        let mut children: Vec<serde_json::Value> = Vec::new();

                        for link in &child_links {
                            let child_addr = &link.data.target;

                            // If child_class_name filter is specified, check the child's rdf://type
                            if let Some(ref filter_class) = p.child_class_name {
                                let type_links = perspective
                                    .get_links(&LinkQuery {
                                        source: Some(child_addr.clone()),
                                        predicate: Some("rdf://type".to_string()),
                                        ..Default::default()
                                    })
                                    .await;

                                let matches = match type_links {
                                    Ok(tl) => tl.iter().any(|l| {
                                        l.data.target.split("://").last().unwrap_or("")
                                            == filter_class.as_str()
                                    }),
                                    Err(_) => false,
                                };

                                if !matches {
                                    continue;
                                }
                            }

                            children.push(json!({
                                "address": child_addr,
                            }));
                        }

                        serde_json::to_string_pretty(&json!({
                            "parent": p.expression_address,
                            "children": children,
                            "count": children.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying children: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Delete a subject instance by running its destructor actions and removing all associated links
    #[tool(
        description = "Delete a subject instance. Removes all links where the subject is the source (its properties, type markers, and collection links). This effectively removes the subject from the perspective."
    )]
    async fn delete_subject(&self, params: Parameters<DeleteSubjectParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = match self.get_agent_context().await {
                    Ok(ctx) => ctx,
                    Err(e) => return format!("Authentication error: {}", e),
                };

                let capabilities = self.get_capabilities().await;
                if let Err(e) = check_capability(&capabilities, &PERSPECTIVE_CREATE_CAPABILITY) {
                    return format!("Capability error: {}", e);
                }

                // Remove all links where this subject is the source
                let source_links = perspective
                    .get_links(&LinkQuery {
                        source: Some(p.expression_address.clone()),
                        ..Default::default()
                    })
                    .await;

                let mut removed = 0;
                if let Ok(links) = source_links {
                    for link in links {
                        if perspective.remove_link(link.into(), None).await.is_ok() {
                            removed += 1;
                        }
                    }
                }

                // Also remove links where this subject is the target (e.g. has_child links pointing to it)
                let target_links = perspective
                    .get_links(&LinkQuery {
                        target: Some(p.expression_address.clone()),
                        ..Default::default()
                    })
                    .await;

                if let Ok(links) = target_links {
                    for link in links {
                        if perspective.remove_link(link.into(), None).await.is_ok() {
                            removed += 1;
                        }
                    }
                }

                serde_json::to_string_pretty(&json!({
                    "success": true,
                    "deleted": p.expression_address,
                    "links_removed": removed,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    // ========================================================================
    // SHACL Property/Collection Resolution Helpers
    // ========================================================================

    /// Resolve a property name to its predicate URI using SHACL shape links
    async fn resolve_property_predicate(
        &self,
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        class_name: &str,
        property_name: &str,
    ) -> Result<String, String> {
        // SHACL shapes are stored as links in the perspective.
        // Property shape URIs encode the name: {namespace}{ClassName}.{propertyName}
        // e.g. "flux://Channel.name", "flux://Channel.messages"
        //
        // Resolution:
        //   literal://string:shacl://{ClassName} --ad4m://shacl_shape_uri--> {shapeUri}
        //   {shapeUri} --sh://property--> {propertyShapeUri}  (URI contains ".{propName}")
        //   {propertyShapeUri} --sh://path--> {predicateUri}

        // Step 1: Find shape URI for class
        let name_literal = format!("literal://string:shacl://{}", class_name);
        let shape_links = perspective
            .get_links(&LinkQuery {
                source: Some(name_literal),
                predicate: Some("ad4m://shacl_shape_uri".to_string()),
                ..Default::default()
            })
            .await
            .map_err(|e| format!("Error querying SHACL shape: {}", e))?;

        if shape_links.is_empty() {
            return Err(format!("No SHACL shape found for class '{}'", class_name));
        }

        let shape_uri = &shape_links[0].data.target;

        // Step 2: Find all property shape URIs
        let prop_links = perspective
            .get_links(&LinkQuery {
                source: Some(shape_uri.clone()),
                predicate: Some("sh://property".to_string()),
                ..Default::default()
            })
            .await
            .map_err(|e| format!("Error querying properties: {}", e))?;

        // Step 3: Match property by name extracted from URI
        // Property shape URIs are like "flux://Channel.name" — name is after the last dot
        for prop_link in &prop_links {
            let prop_uri = &prop_link.data.target;

            // Extract property name from URI: "flux://Channel.name" -> "name"
            let prop_name_from_uri = prop_uri
                .rsplit_once('.')
                .map(|(_, name)| name)
                .unwrap_or("");

            if prop_name_from_uri == property_name {
                // Found it — get the path (predicate)
                let path_links = perspective
                    .get_links(&LinkQuery {
                        source: Some(prop_uri.clone()),
                        predicate: Some("sh://path".to_string()),
                        ..Default::default()
                    })
                    .await
                    .map_err(|e| format!("Error querying property path: {}", e))?;

                if let Some(path_link) = path_links.first() {
                    return Ok(path_link.data.target.clone());
                }
            }
        }

        Err(format!(
            "Property '{}' not found in class '{}'",
            property_name, class_name
        ))
    }

    /// Resolve a collection name to its predicate URI using SHACL shape links
    // ========================================================================
    // AUTHENTICATION TOOLS
    // ========================================================================

    /// Login with email and password (multi-user mode)
    #[tool(
        description = "Login to a multi-user AD4M executor using email and password. Returns a JWT token on success that will be used for subsequent operations."
    )]
    async fn login_email(&self, params: Parameters<LoginEmailParams>) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::login_user(&email, &params.0.password, "mcp-agent") {
            Ok(token) => {
                self.store_token_and_respond(token, Some(&email), "Login successful.")
                    .await
            }
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Request a capability token (local connect flow - step 1)
    #[tool(
        description = "Request a capability token (step 1/2 of local auth flow). This is the primary way to authenticate with a local/single-user AD4M executor. Returns request_id and code — pass both to generate_jwt to get a JWT token. For multi-user executors, use login_email or signup instead."
    )]
    async fn request_capability(&self, params: Parameters<RequestCapabilityParams>) -> String {
        let p = &params.0;

        let auth_info = AuthInfo {
            app_name: p.app_name.clone(),
            app_desc: p.app_desc.clone(),
            app_domain: p.app_domain.clone(),
            app_url: p.app_url.clone(),
            app_icon_path: None,
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            user_email: None,
        };

        let request_id = cap_request_capability(auth_info.clone()).await;

        // Auto-permit the capability request
        match permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: auth_info,
        }) {
            Ok(code) => {
                println!("MCP capability request - code: {}", code);
                json!({
                    "request_id": request_id,
                    "code": code,
                    "message": "Capability requested and auto-permitted. Use generate_jwt with these values to get a token."
                })
                .to_string()
            }
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to permit capability: {}", e)
            })
            .to_string(),
        }
    }

    /// Generate a JWT from a capability request (local connect flow - step 2)
    #[tool(
        description = "Generate a JWT token (step 2/2 of local auth flow). Pass the request_id and code from request_capability. The JWT is stored in the session and used for all subsequent operations automatically."
    )]
    async fn generate_jwt(&self, params: Parameters<GenerateJwtParams>) -> String {
        let p = &params.0;

        match generate_capability_token(p.request_id.clone(), p.code.clone()).await {
            Ok(cap_token) => {
                self.store_token_and_respond(
                    cap_token,
                    None,
                    "JWT generated and stored. You are now authenticated.",
                )
                .await
            }
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to generate JWT: {}", e)
            })
            .to_string(),
        }
    }

    /// Sign up a new user (multi-user mode)
    #[tool(
        description = "Create a new user account (multi-user mode). Sends a verification email with a code. Use verify_email_code to complete signup."
    )]
    async fn signup(&self, params: Parameters<SignupParams>) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::signup_user(&email, &params.0.password, Some("MCP Agent")).await {
            Ok(did) => json!({
                "success": true,
                "did": did,
                "message": "User created. Check your email for a verification code and call verify_email_code."
            }).to_string(),
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Request a login verification code (multi-user mode)
    #[tool(
        description = "Request a login verification code to be sent to the user's email. Use verify_email_code to complete login."
    )]
    async fn request_login_verification(
        &self,
        params: Parameters<RequestLoginVerificationParams>,
    ) -> String {
        use crate::user_management as um;
        let email = params.0.email.trim().to_lowercase();

        match um::request_login_code(&email, Some("MCP Agent")).await {
            Ok(()) => json!({
                "success": true,
                "message": "Verification code sent. Use verify_email_code to complete login."
            })
            .to_string(),
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Verify an email code for signup or login (multi-user mode)
    #[tool(
        description = "Verify an email code to complete signup or login. Returns a JWT token on success. The verification_type must be 'signup' or 'login'."
    )]
    async fn verify_email_code(&self, params: Parameters<VerifyEmailCodeParams>) -> String {
        use crate::user_management as um;
        let p = &params.0;
        let email = p.email.trim().to_lowercase();

        match um::verify_and_login(&email, &p.code, &p.verification_type, "mcp-agent") {
            Ok(token) => {
                self.store_token_and_respond(
                    token,
                    Some(&email),
                    "Email verified. Token stored for subsequent operations.",
                )
                .await
            }
            Err(e) => json!({"success": false, "error": e}).to_string(),
        }
    }

    /// Check current authentication status
    #[tool(description = "Check the current authentication status of the MCP session.")]
    async fn auth_status(&self, _params: Parameters<AuthStatusParams>) -> String {
        let token = self.context.auth_token.read().await;

        match &*token {
            Some(t) if !t.is_empty() => {
                // Try to decode and get info
                match decode_jwt(t.clone()) {
                    Ok(claims) => json!({
                        "authenticated": true,
                        "app_name": claims.capabilities.app_name,
                        "user_email": claims.capabilities.user_email,
                        "has_capabilities": claims.capabilities.capabilities.is_some(),
                    })
                    .to_string(),
                    Err(_) => {
                        json!({
                            "authenticated": false,
                            "token_type": "unknown",
                            "message": "Token set but invalid - could not decode"
                        })
                        .to_string()
                    }
                }
            }
            _ => json!({
                "authenticated": false,
                "message": "Not authenticated. Use request_capability + generate_jwt, login_email, or signup + verify_email_code to authenticate."
            })
            .to_string(),
        }
    }

    // ========================================================================
    // AGENT PROFILE TOOLS
    // ========================================================================

    /// Get the current agent's public profile
    #[tool(
        description = "Get the current agent's public profile (username, name, bio, profile picture URLs). This is the identity that other agents and Flux users see in neighbourhoods."
    )]
    async fn get_agent_profile(&self, _params: Parameters<GetAgentProfileParams>) -> String {
        let _agent_context = self.get_agent_context_for_read().await;

        // Query agent via JS resolver
        let mut js = self.context.js_handle.clone();
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;

        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };

        let agent: serde_json::Value = match serde_json::from_str(&result) {
            Ok(v) => v,
            Err(e) => return json!({"error": format!("Failed to parse agent: {}", e)}).to_string(),
        };

        let did = agent.get("did").and_then(|v| v.as_str()).unwrap_or("");
        let links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array());

        let mut profile = json!({"did": did});

        let predicates = vec![
            ("sioc://has_username", "username"),
            ("sioc://has_given_name", "given_name"),
            ("sioc://has_family_name", "family_name"),
            ("sioc://has_email", "email"),
            ("sioc://has_bio", "bio"),
            ("sioc://has_profile_image", "profile_image"),
            ("sioc://has_profile_thumbnail_image", "profile_thumbnail"),
        ];

        if let Some(links) = links {
            for link in links {
                let source = link.pointer("/data/source").and_then(|v| v.as_str()).unwrap_or("");
                let predicate = link.pointer("/data/predicate").and_then(|v| v.as_str()).unwrap_or("");
                let target = link.pointer("/data/target").and_then(|v| v.as_str()).unwrap_or("");

                if source == "flux://profile" {
                    for (pred_uri, field_name) in &predicates {
                        if predicate == *pred_uri {
                            let value = Self::resolve_literal_value(target);
                            profile[field_name] = json!(value);
                        }
                    }
                }
            }
        }

        profile.to_string()
    }

    /// Set the current agent's public profile fields
    #[tool(
        description = "Set the current agent's public profile (username, name, bio, email). These fields are visible to other agents and Flux users in neighbourhoods. Only provided fields are updated; omitted fields keep their current values."
    )]
    async fn set_agent_profile(&self, params: Parameters<SetAgentProfileParams>) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        // Get current agent state
        let mut js = self.context.js_handle.clone();
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;
        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let agent: serde_json::Value = match serde_json::from_str(&result) {
            Ok(v) => v,
            Err(e) => return json!({"error": format!("Failed to parse agent: {}", e)}).to_string(),
        };

        let did = agent.get("did").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let current_links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .cloned()
            .unwrap_or_default();

        let profile_text_predicates = vec![
            "sioc://has_username",
            "sioc://has_given_name",
            "sioc://has_family_name",
            "sioc://has_email",
            "sioc://has_bio",
        ];

        // Collect current values
        let mut current_values = std::collections::HashMap::new();
        let mut preserved_links: Vec<serde_json::Value> = Vec::new();

        for link in &current_links {
            let source = link.pointer("/data/source").and_then(|v| v.as_str()).unwrap_or("");
            let predicate = link.pointer("/data/predicate").and_then(|v| v.as_str()).unwrap_or("");
            let target = link.pointer("/data/target").and_then(|v| v.as_str()).unwrap_or("");

            if source == "flux://profile" && profile_text_predicates.contains(&predicate) {
                current_values.insert(predicate.to_string(), target.to_string());
                continue; // Will be replaced
            }
            // Keep image links and other links
            preserved_links.push(json!({
                "author": did,
                "timestamp": link.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": source, "predicate": predicate, "target": target}
            }));
        }

        let p = &params.0;
        let now = chrono::Utc::now();

        let fields: Vec<(&str, Option<&String>)> = vec![
            ("sioc://has_username", p.username.as_ref()),
            ("sioc://has_given_name", p.given_name.as_ref()),
            ("sioc://has_family_name", p.family_name.as_ref()),
            ("sioc://has_email", p.email.as_ref()),
            ("sioc://has_bio", p.bio.as_ref()),
        ];

        let mut all_links = preserved_links;
        for (i, (predicate, new_value)) in fields.iter().enumerate() {
            let target = match new_value {
                Some(v) => format!("literal://string:{}", v),
                None => match current_values.get(*predicate) {
                    Some(existing) => existing.clone(),
                    None => continue,
                },
            };
            let ts = now + chrono::Duration::milliseconds(i as i64);
            all_links.push(json!({
                "author": did,
                "timestamp": ts.to_rfc3339(),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": "flux://profile", "predicate": predicate, "target": target}
            }));
        }

        // Update via JS resolver
        let perspective_json = json!({"links": all_links});
        let update_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}))"#,
            serde_json::to_string(&perspective_json).unwrap()
        );

        match js.execute(update_script).await {
            Ok(_) => {
                let mut updated = json!({"success": true});
                if let Some(u) = p.username.as_ref() { updated["username"] = json!(u); }
                if let Some(g) = p.given_name.as_ref() { updated["given_name"] = json!(g); }
                if let Some(f) = p.family_name.as_ref() { updated["family_name"] = json!(f); }
                if let Some(e) = p.email.as_ref() { updated["email"] = json!(e); }
                if let Some(b) = p.bio.as_ref() { updated["bio"] = json!(b); }
                updated.to_string()
            }
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }

    /// Set the agent's profile picture
    #[tool(
        description = "Set the current agent's profile picture. Provide raw base64-encoded image data (NOT a data URI). The image will be uploaded to the centralized file store and linked in the agent's public profile. For best results, use a square image (Flux will display it as a circle)."
    )]
    async fn set_agent_profile_picture(&self, params: Parameters<SetAgentProfilePictureParams>) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let mut js = self.context.js_handle.clone();
        let mime = params.0.mime_type.as_deref().unwrap_or("image/png");
        let file_storage_lang = "QmzSYwdjqeP9D13Sfmyc5HcabM9jL3DtPyhadnF6dQXu4FjVSbQ";

        // Upload image via expressionCreate
        let content = json!({
            "data_base64": params.0.image_base64,
            "name": "profile-image",
            "file_type": mime,
        });
        let create_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "expressionCreate", {{ content: {}, languageAddress: {:?} }}))"#,
            serde_json::to_string(&serde_json::to_string(&content).unwrap()).unwrap(),
            file_storage_lang
        );

        let profile_img = match js.execute(create_script).await {
            Ok(r) => {
                let addr: String = serde_json::from_str(&r).unwrap_or(r);
                addr
            }
            Err(e) => return json!({"error": format!("Failed to upload image: {}", e)}).to_string(),
        };

        // Get current agent perspective
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;
        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let agent: serde_json::Value = serde_json::from_str(&result).unwrap_or(json!({}));
        let did = agent.get("did").and_then(|v| v.as_str()).unwrap_or("").to_string();

        let current_links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .cloned()
            .unwrap_or_default();

        // Keep all links except image links
        let mut all_links: Vec<serde_json::Value> = Vec::new();
        for link in &current_links {
            let predicate = link.pointer("/data/predicate").and_then(|v| v.as_str()).unwrap_or("");
            if predicate == "sioc://has_profile_image" || predicate == "sioc://has_profile_thumbnail_image" {
                continue;
            }
            let source = link.pointer("/data/source").and_then(|v| v.as_str()).unwrap_or("");
            let target = link.pointer("/data/target").and_then(|v| v.as_str()).unwrap_or("");
            all_links.push(json!({
                "author": did,
                "timestamp": link.get("timestamp").and_then(|v| v.as_str()).unwrap_or(""),
                "proof": {"invalid": false, "key": "", "signature": ""},
                "data": {"source": source, "predicate": predicate, "target": target}
            }));
        }

        let now = chrono::Utc::now().to_rfc3339();
        all_links.push(json!({
            "author": did, "timestamp": now,
            "proof": {"invalid": false, "key": "", "signature": ""},
            "data": {"source": "flux://profile", "predicate": "sioc://has_profile_image", "target": profile_img}
        }));
        all_links.push(json!({
            "author": did, "timestamp": now,
            "proof": {"invalid": false, "key": "", "signature": ""},
            "data": {"source": "flux://profile", "predicate": "sioc://has_profile_thumbnail_image", "target": profile_img}
        }));

        let perspective_json = json!({"links": all_links});
        let update_script = format!(
            r#"JSON.stringify(await core.callResolver("Mutation", "agentUpdatePublicPerspective", {{ perspective: {} }}))"#,
            serde_json::to_string(&perspective_json).unwrap()
        );

        match js.execute(update_script).await {
            Ok(_) => json!({
                "success": true,
                "profile_image": profile_img,
                "message": "Profile picture updated. For best results in Flux, use a square image."
            }).to_string(),
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }
}

// ============================================================================
// Dynamic SHACL Tool Generation
// ============================================================================

use super::shacl::{self, ShaclProperty};

impl Ad4mMcpHandler {
    /// Generate dynamic MCP tools from SHACL subject classes across all perspectives
    async fn generate_dynamic_tools(&self) -> Vec<Tool> {
        let perspectives = all_perspectives();
        let mut tools = Vec::new();
        let mut seen_classes = std::collections::HashSet::new();

        for p in perspectives.iter() {
            let uuid = {
                let handle = p.persisted.lock().await;
                handle.uuid.clone()
            };

            let perspective = match get_perspective(&uuid) {
                Some(p) => p,
                None => continue,
            };

            let classes = shacl::load_classes(&perspective).await;

            for class in &classes {
                if !seen_classes.insert(class.name_lower.clone()) {
                    continue;
                }

                tools.push(Self::make_create_tool(&class.name, &class.properties));
                tools.push(Self::make_query_tool(&class.name));
                tools.push(Self::make_get_tool(&class.name));
                tools.push(Self::make_delete_tool(&class.name));
                // Per-property set tools and collection tools
                for prop in &class.properties {
                    if prop.is_collection {
                        tools.push(Self::make_collection_get_tool(&class.name, &prop.name));
                        tools.push(Self::make_collection_add_tool(&class.name, &prop.name));
                        tools.push(Self::make_collection_remove_tool(&class.name, &prop.name));
                    } else {
                        tools.push(Self::make_set_property_tool(&class.name, &prop.name));
                    }
                }
            }
        }

        tools
    }

    /// Extract property information from a SHACL shape
    fn make_tool_schema(
        properties: Vec<(&str, &str)>,
        required: Vec<&str>,
    ) -> Arc<serde_json::Map<String, serde_json::Value>> {
        let mut props = serde_json::Map::new();
        for (name, desc) in properties {
            props.insert(
                name.to_string(),
                json!({ "type": "string", "description": desc }),
            );
        }
        let mut schema = serde_json::Map::new();
        schema.insert("type".to_string(), json!("object"));
        schema.insert("properties".to_string(), serde_json::Value::Object(props));
        schema.insert("required".to_string(), json!(required));
        Arc::new(schema)
    }

    fn make_create_tool(class_name: &str, properties: &[ShaclProperty]) -> Tool {
        let name_lower = class_name.to_lowercase();
        let mut prop_entries: Vec<(String, String)> = vec![
            ("perspective_id".to_string(), "Perspective UUID".to_string()),
            (
                "expression_address".to_string(),
                format!("Address for the new {} instance", class_name),
            ),
        ];
        for p in properties {
            if !p.is_collection {
                prop_entries.push((p.name.clone(), format!("{} property value", p.name)));
            }
        }
        let props: Vec<(&str, &str)> = prop_entries
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();
        let prop_names: Vec<&str> = properties
            .iter()
            .filter(|p| !p.is_collection)
            .map(|p| p.name.as_str())
            .collect();

        Tool::new(
            format!("{}_create", name_lower),
            format!(
                "Create a new {} instance. Properties: {}",
                class_name,
                prop_names.join(", ")
            ),
            Self::make_tool_schema(props, vec!["perspective_id", "expression_address"]),
        )
    }

    fn make_query_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_query", name_lower),
            format!(
                "Query all {} instances in a perspective. Returns expression addresses.",
                class_name
            ),
            Self::make_tool_schema(
                vec![("perspective_id", "Perspective UUID")],
                vec!["perspective_id"],
            ),
        )
    }

    fn make_get_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_get", name_lower),
            format!(
                "Get all properties and values of a {} instance. Returns a JSON object with property names as keys. Scalar properties return single values; collections return arrays.",
                class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    fn make_set_property_tool(class_name: &str, property_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let prop_lower = property_name.to_lowercase();
        let value_desc = format!("New value for {}", property_name);
        Tool::new(
            format!("{}_set_{}", name_lower, prop_lower),
            format!(
                "Set the '{}' property on a {} instance.",
                property_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", &value_desc),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_collection_get_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_get_{}", name_lower, coll_lower),
            format!(
                "Get all items in the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    fn make_collection_add_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_add_{}", name_lower, coll_lower),
            format!(
                "Add an item to the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", "Value to add to the collection"),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_collection_remove_tool(class_name: &str, collection_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        let coll_lower = collection_name.to_lowercase();
        Tool::new(
            format!("{}_remove_{}", name_lower, coll_lower),
            format!(
                "Remove an item from the '{}' collection of a {} instance.",
                collection_name, class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    ("expression_address", "Expression address of the instance"),
                    ("value", "Value to remove from the collection"),
                ],
                vec!["perspective_id", "expression_address", "value"],
            ),
        )
    }

    fn make_delete_tool(class_name: &str) -> Tool {
        let name_lower = class_name.to_lowercase();
        Tool::new(
            format!("{}_delete", name_lower),
            format!(
                "Delete a {} instance and all its associated links.",
                class_name
            ),
            Self::make_tool_schema(
                vec![
                    ("perspective_id", "Perspective UUID"),
                    (
                        "expression_address",
                        "Expression address of the instance to delete",
                    ),
                ],
                vec!["perspective_id", "expression_address"],
            ),
        )
    }

    /// Handle a dynamic SHACL tool call
    async fn handle_dynamic_tool(
        &self,
        tool_name: &str,
        arguments: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<CallToolResult, ErrorData> {
        let args = arguments.unwrap_or_default();

        // Parse tool name: {class_name}_{operation} or {class_name}_{operation}_{property}
        let (class_name_lower, rest) = match tool_name.split_once('_') {
            Some((cls, rest)) => (cls, rest),
            None => {
                return Ok(CallToolResult::error(vec![Content::text(format!(
                    "Unknown tool: {}",
                    tool_name
                ))]));
            }
        };

        // rest could be "create", "query", "get", "delete", "set_propname", "add_propname", "remove_propname", "get_propname"
        let (operation, property_name) = if let Some((op, prop)) = rest.split_once('_') {
            (op, Some(prop.to_string()))
        } else {
            (rest, None)
        };

        if !matches!(
            operation,
            "create" | "query" | "get" | "update" | "delete" | "set" | "add" | "remove"
        ) {
            return Ok(CallToolResult::error(vec![Content::text(format!(
                "Unknown tool: {}",
                tool_name
            ))]));
        }

        let perspective_id = match args.get("perspective_id").and_then(|v| v.as_str()) {
            Some(id) => id.to_string(),
            None => {
                return Ok(CallToolResult::error(vec![Content::text(
                    "Missing required parameter: perspective_id",
                )]));
            }
        };

        // Find actual class name (preserving original case)
        let class_name = {
            let perspective = match get_perspective(&perspective_id) {
                Some(p) => p,
                None => {
                    return Ok(CallToolResult::error(vec![Content::text(format!(
                        "Perspective not found: {}",
                        perspective_id
                    ))]));
                }
            };
            match shacl::find_class_name(&perspective, class_name_lower).await {
                Some(name) => name,
                None => {
                    return Ok(CallToolResult::error(vec![Content::text(format!(
                        "Subject class '{}' not found in perspective {}",
                        class_name_lower, perspective_id
                    ))]));
                }
            }
        };

        let result = match operation {
            "create" => {
                self.handle_dynamic_create(&perspective_id, &class_name, &args)
                    .await
            }
            "query" => {
                self.handle_dynamic_query(&perspective_id, &class_name)
                    .await
            }
            "get" => {
                if let Some(ref prop) = property_name {
                    // {class}_get_{collection} — get collection items
                    self.handle_dynamic_get_collection(&perspective_id, &class_name, prop, &args)
                        .await
                } else {
                    self.handle_dynamic_get(&perspective_id, &class_name, &args)
                        .await
                }
            }
            "set" => {
                // {class}_set_{property}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_set_property(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "add" => {
                // {class}_add_{collection}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_add_collection(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "remove" => {
                // {class}_remove_{collection}
                let prop = property_name.as_deref().unwrap_or("");
                self.handle_dynamic_remove_collection(&perspective_id, &class_name, prop, &args)
                    .await
            }
            "update" => {
                self.handle_dynamic_update(&perspective_id, &class_name, &args)
                    .await
            }
            "delete" => {
                self.handle_dynamic_delete(&perspective_id, &class_name, &args)
                    .await
            }
            _ => unreachable!(),
        };

        Ok(CallToolResult::success(vec![Content::text(result)]))
    }

    async fn handle_dynamic_create(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        // Build initial_values from non-system property args
        let initial_values: Option<serde_json::Value> = {
            let props: serde_json::Map<String, serde_json::Value> = args
                .iter()
                .filter(|(k, _)| {
                    k.as_str() != "perspective_id" && k.as_str() != "expression_address"
                })
                .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), json!(s))))
                .collect();
            if props.is_empty() {
                None
            } else {
                Some(serde_json::Value::Object(props))
            }
        };

        let subject_class: SubjectClassOption = match serde_json::from_value(json!({
            "className": class_name
        })) {
            Ok(sc) => sc,
            Err(e) => return format!("Error: {}", e),
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        match perspective
            .create_subject(
                subject_class,
                expression_address.clone(),
                initial_values,
                None,
                &agent_context,
            )
            .await
        {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "created": true,
                "perspective_id": perspective_id,
                "class_name": class_name,
                "expression_address": expression_address
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error creating subject: {}", e),
        }
    }

    async fn handle_dynamic_query(&self, perspective_id: &str, class_name: &str) -> String {
        let perspective = match get_perspective(perspective_id) {
            Some(p) => p,
            None => return format!("Perspective not found: {}", perspective_id),
        };

        // Find target_class URI
        let class_links = match perspective
            .get_links(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        let target_class = match class_links.iter().find_map(|l| {
            let name = l.data.source.split("://").last().unwrap_or("");
            if name == class_name {
                Some(l.data.source.clone())
            } else {
                None
            }
        }) {
            Some(tc) => tc,
            None => return format!("Subject class '{}' not found", class_name),
        };

        // Find instances
        let instance_links = match perspective
            .get_links(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some(target_class),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        let instances: Vec<String> = instance_links
            .iter()
            .map(|l| l.data.source.clone())
            .collect();
        serde_json::to_string_pretty(&instances).unwrap_or_else(|e| format!("Error: {}", e))
    }

    async fn handle_dynamic_get(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let perspective = match get_perspective(perspective_id) {
            Some(p) => p,
            None => return format!("Perspective not found: {}", perspective_id),
        };

        // Reuse get_subject_data logic
        let name_literal = format!("literal://string:shacl://{}", class_name);
        let shape_links = match perspective
            .get_links(&LinkQuery {
                source: Some(name_literal),
                predicate: Some("ad4m://shacl_shape_uri".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        if shape_links.is_empty() {
            return format!("No SHACL shape found for class '{}'", class_name);
        }

        let shape_uri = &shape_links[0].data.target;
        let prop_links = match perspective
            .get_links(&LinkQuery {
                source: Some(shape_uri.clone()),
                predicate: Some("sh://property".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(e) => return format!("Error: {}", e),
        };

        let mut data = serde_json::Map::new();
        for prop_link in &prop_links {
            let prop_uri = &prop_link.data.target;
            let prop_name = prop_uri
                .rsplit_once('.')
                .map(|(_, name)| name.to_string())
                .unwrap_or_else(|| prop_uri.clone());

            let path_links = match perspective
                .get_links(&LinkQuery {
                    source: Some(prop_uri.clone()),
                    predicate: Some("sh://path".to_string()),
                    ..Default::default()
                })
                .await
            {
                Ok(links) => links,
                Err(_) => continue,
            };

            if let Some(path_link) = path_links.first() {
                let predicate = &path_link.data.target;

                let is_collection = match perspective
                    .get_links(&LinkQuery {
                        source: Some(prop_uri.clone()),
                        predicate: Some("rdf://type".to_string()),
                        target: Some("ad4m://CollectionShape".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => !links.is_empty(),
                    Err(_) => false,
                };

                let value_links = match perspective
                    .get_links(&LinkQuery {
                        source: Some(expression_address.clone()),
                        predicate: Some(predicate.clone()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => links,
                    Err(_) => continue,
                };

                if is_collection {
                    let items: Vec<String> =
                        value_links.iter().map(|l| l.data.target.clone()).collect();
                    data.insert(
                        prop_name,
                        serde_json::Value::Array(
                            items.into_iter().map(serde_json::Value::String).collect(),
                        ),
                    );
                } else if let Some(link) = value_links.first() {
                    let value = Self::resolve_literal_value(&link.data.target);
                    data.insert(prop_name, serde_json::Value::String(value));
                }
            }
        }

        serde_json::to_string_pretty(&serde_json::Value::Object(data))
            .unwrap_or_else(|e| format!("Error: {}", e))
    }

    async fn handle_dynamic_update(
        &self,
        perspective_id: &str,
        class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        let mut updated = Vec::new();
        for (key, value) in args {
            if key == "perspective_id" || key == "expression_address" {
                continue;
            }
            let value_str = match value.as_str() {
                Some(s) => s.to_string(),
                None => value.to_string(),
            };

            let predicate = match self
                .resolve_property_predicate(&perspective, class_name, key)
                .await
            {
                Ok(pred) => pred,
                Err(e) => return format!("Error resolving property '{}': {}", key, e),
            };

            // Remove old values
            if let Ok(links) = perspective
                .get_links(&LinkQuery {
                    source: Some(expression_address.clone()),
                    predicate: Some(predicate.clone()),
                    ..Default::default()
                })
                .await
            {
                for link in links {
                    let _ = perspective.remove_link(link.into(), None).await;
                }
            }

            // Add new value
            let target = if value_str.starts_with("literal://") || value_str.contains("://") {
                value_str.clone()
            } else {
                format!("literal://string:{}", value_str)
            };

            let link = Link {
                source: expression_address.clone(),
                predicate: Some(predicate),
                target,
            };

            match perspective
                .add_link(link, LinkStatus::Shared, None, &agent_context)
                .await
            {
                Ok(_) => updated.push(key.clone()),
                Err(e) => return format!("Error setting property '{}': {}", key, e),
            }
        }

        serde_json::to_string_pretty(&json!({
            "success": true,
            "updated_properties": updated,
        }))
        .unwrap_or_else(|e| format!("Error: {}", e))
    }

    async fn handle_dynamic_delete(
        &self,
        perspective_id: &str,
        _class_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        let mut removed = 0;
        if let Ok(links) = perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                ..Default::default()
            })
            .await
        {
            for link in links {
                if perspective.remove_link(link.into(), None).await.is_ok() {
                    removed += 1;
                }
            }
        }

        if let Ok(links) = perspective
            .get_links(&LinkQuery {
                target: Some(expression_address.clone()),
                ..Default::default()
            })
            .await
        {
            for link in links {
                if perspective.remove_link(link.into(), None).await.is_ok() {
                    removed += 1;
                }
            }
        }

        serde_json::to_string_pretty(&json!({
            "success": true,
            "deleted": expression_address,
            "links_removed": removed,
        }))
        .unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// Handle {class}_set_{property} — set a single property on a subject instance
    async fn handle_dynamic_set_property(
        &self,
        perspective_id: &str,
        class_name: &str,
        property_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Resolve property predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, property_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving property '{}': {}", property_name, e),
        };

        // Remove existing links with this predicate (setSingleTarget pattern)
        let existing = perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate.clone()),
                ..Default::default()
            })
            .await;

        if let Ok(links) = existing {
            for link in links {
                let _ = perspective.remove_link(link.into(), None).await;
            }
        }

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            format!("literal://string:{}", value)
        };

        let link = Link {
            source: expression_address.clone(),
            predicate: Some(predicate),
            target,
        };

        match perspective
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "success": true,
                "expression_address": expression_address,
                "property": property_name,
                "value": value,
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error setting property '{}': {}", property_name, e),
        }
    }

    /// Handle {class}_get_{collection} — get items in a collection
    async fn handle_dynamic_get_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        // Query all links with this predicate from the expression
        match perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate),
                ..Default::default()
            })
            .await
        {
            Ok(links) => {
                let items: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
                serde_json::to_string_pretty(&json!({
                    "expression_address": expression_address,
                    "collection": collection_name,
                    "items": items,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!("Error getting collection '{}': {}", collection_name, e),
        }
    }

    /// Handle {class}_add_{collection} — add item to a collection
    async fn handle_dynamic_add_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Resolve collection predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        let agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            format!("literal://string:{}", value)
        };

        let link = Link {
            source: expression_address.clone(),
            predicate: Some(predicate),
            target,
        };

        match perspective
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => serde_json::to_string_pretty(&json!({
                "success": true,
                "expression_address": expression_address,
                "collection": collection_name,
                "added": value,
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
            Err(e) => format!("Error adding to collection '{}': {}", collection_name, e),
        }
    }

    /// Handle {class}_remove_{collection} — remove item from a collection
    async fn handle_dynamic_remove_collection(
        &self,
        perspective_id: &str,
        class_name: &str,
        collection_name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> String {
        let expression_address = match Self::require_arg(args, "expression_address") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };
        let value = match Self::require_arg(args, "value") {
            Ok(v) => v.to_string(),
            Err(e) => return e,
        };

        let mut perspective = match self.get_writable_perspective(perspective_id).await {
            Ok(p) => p,
            Err(e) => return e,
        };

        // Resolve collection predicate via SHACL
        let predicate = match self
            .resolve_property_predicate(&perspective, class_name, collection_name)
            .await
        {
            Ok(pred) => pred,
            Err(e) => return format!("Error resolving collection '{}': {}", collection_name, e),
        };

        // Find and remove the link with matching target
        let target = if value.starts_with("literal://") || value.contains("://") {
            value.clone()
        } else {
            format!("literal://string:{}", value)
        };

        match perspective
            .get_links(&LinkQuery {
                source: Some(expression_address.clone()),
                predicate: Some(predicate),
                target: Some(target),
                ..Default::default()
            })
            .await
        {
            Ok(links) => {
                let mut removed = 0;
                for link in links {
                    if perspective.remove_link(link.into(), None).await.is_ok() {
                        removed += 1;
                    }
                }
                serde_json::to_string_pretty(&json!({
                    "success": true,
                    "expression_address": expression_address,
                    "collection": collection_name,
                    "removed": value,
                    "links_removed": removed,
                }))
                .unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => format!(
                "Error removing from collection '{}': {}",
                collection_name, e
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    // Test-only struct that mirrors McpContext but without JsCoreHandle
    // This is necessary because JsCoreHandle requires complex channel setup
    struct TestAuthContext {
        admin_credential: Option<String>,
        auth_token: Arc<RwLock<Option<String>>>,
    }

    impl TestAuthContext {
        fn new(admin_credential: Option<String>) -> Self {
            Self {
                admin_credential,
                auth_token: Arc::new(RwLock::new(None)),
            }
        }

        async fn get_auth_token(&self) -> Option<String> {
            self.auth_token.read().await.clone()
        }
    }

    // Test the auth_status logic directly without needing full MCP handler
    #[tokio::test]
    async fn test_auth_status_unauthenticated() {
        let ctx = TestAuthContext::new(None);
        let token = ctx.get_auth_token().await;

        // Simulate auth_status logic
        let result = match token {
            Some(t) if !t.is_empty() => "authenticated",
            _ => "not_authenticated",
        };

        assert_eq!(result, "not_authenticated");
    }

    #[tokio::test]
    async fn test_auth_token_stores_value() {
        let ctx = TestAuthContext::new(None);

        // Simulate token storage logic
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("test-token".to_string());
        }

        let token = ctx.get_auth_token().await;
        assert_eq!(token, Some("test-token".to_string()));
    }

    #[tokio::test]
    async fn test_admin_credential_check() {
        let ctx = TestAuthContext::new(Some("my-admin-secret".to_string()));

        // Set admin credential as token
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("my-admin-secret".to_string());
        }

        let token = ctx.get_auth_token().await;
        let is_admin = token.as_ref() == ctx.admin_credential.as_ref();

        assert!(is_admin);
    }

    #[tokio::test]
    async fn test_invalid_admin_credential() {
        let ctx = TestAuthContext::new(Some("my-admin-secret".to_string()));

        // Set wrong credential
        {
            let mut token_guard = ctx.auth_token.write().await;
            *token_guard = Some("wrong-secret".to_string());
        }

        let token = ctx.get_auth_token().await;
        let is_admin = token.as_ref() == ctx.admin_credential.as_ref();

        assert!(!is_admin);
    }

    #[test]
    fn test_escape_prolog_string() {
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string(r#"test"value"#),
            r#"test\"value"#
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string(r"test\path"),
            r"test\\path"
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("test'quote"),
            r"test\'quote"
        );
        // Test newline and carriage return escaping
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("line1\nline2"),
            r"line1\nline2"
        );
        assert_eq!(
            Ad4mMcpHandler::escape_prolog_string("text\r\nmore"),
            r"text\r\nmore"
        );
    }

    // Integration tests for full login flow would need the database initialized
    // See tests/js/tests/mcp-auth.test.ts for full integration tests
}
