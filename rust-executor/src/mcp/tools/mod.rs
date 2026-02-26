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

pub mod dynamic;
pub mod params;

pub use params::*;

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

use serde_json::json;

// ============================================================================
// MCP Handler
// ============================================================================

/// AD4M MCP Handler - exposes Subject operations as MCP tools
#[derive(Clone)]
pub struct Ad4mMcpHandler {
    context: McpContext,
    tool_router: ToolRouter<Self>,
}

/// Tool names that can be called without authentication.
/// These are the auth bootstrapping tools for multi-user mode.
const AUTH_TOOLS: &[&str] = &[
    "login_email",
    "signup",
    "verify_email_code",
    "request_login_verification",
    "request_capability",
    "generate_jwt",
    "auth_status",
];

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

        // Enforce authentication for non-auth tools
        if !AUTH_TOOLS.contains(&tool_name.as_str()) {
            let token = self.get_auth_token().await;
            match &token {
                Some(t) if !t.is_empty() => {
                    // Validate the token is a real credential (admin or valid JWT)
                    if let Some(admin) = &self.context.admin_credential {
                        if t == admin {
                            // Admin credential — full access
                        } else {
                            // Must be a valid JWT — verify it decodes
                            if decode_jwt(t.clone()).is_err() {
                                return Ok(CallToolResult::error(vec![Content::text(
                                    "Authentication failed: invalid or expired token. Use login_email, signup, or request_capability to authenticate."
                                )]));
                            }
                        }
                    } else {
                        // No admin credential configured — any non-empty token accepted
                        // (single-user mode with capability token)
                    }
                }
                _ => {
                    return Ok(CallToolResult::error(vec![Content::text(
                        "Authentication required. Call login_email, signup, or request_capability first to obtain a token."
                    )]));
                }
            }
        }

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

    /// Get a perspective by ID, verifying the agent is authenticated and has the required capability.
    /// This is the standard entry point for tool handlers — DRYs out the repeated
    /// get_perspective + get_agent_context + check_capability pattern.
    async fn get_perspective_with_auth(
        &self,
        perspective_id: &str,
        required_capability: &Capability,
    ) -> Result<
        (
            crate::perspectives::perspective_instance::PerspectiveInstance,
            AgentContext,
        ),
        String,
    > {
        let agent_context = self.get_agent_context().await?;
        let capabilities = self.get_capabilities().await;
        check_capability(&capabilities, required_capability)
            .map_err(|e| format!("Capability error: {}", e))?;
        let perspective = get_perspective(perspective_id)
            .ok_or_else(|| format!("Perspective not found: {}", perspective_id))?;
        // TODO: check that this user/agent actually has access to this perspective
        Ok((perspective, agent_context))
    }

    /// Convenience wrapper for write operations (most common case)
    async fn get_writable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<
        (
            crate::perspectives::perspective_instance::PerspectiveInstance,
            AgentContext,
        ),
        String,
    > {
        self.get_perspective_with_auth(perspective_id, &PERSPECTIVE_CREATE_CAPABILITY)
            .await
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
        description = "Get all data (properties and values) for a specific subject instance. Returns the complete state of the model instance as a JSON object with property names and values. Example: get_subject_data(perspective_id='abc-123', class_name='Message', expression_address='literal://string:xyz') returns {body: 'Hello', author: 'did:key:...', timestamp: '2026-01-01'}."
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
        description = "Create a new subject instance (model object) with optional initial property values. Example: create_subject(perspective_id='abc-123', class_name='Channel', initial_values='{\"name\": \"general\"}') creates a Channel with the given name. Returns the new instance's expression address."
    )]
    async fn create_subject(&self, params: Parameters<CreateSubjectParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
        }
    }

    /// Execute commands (actions) on a subject instance
    #[tool(
        description = "Execute commands (actions) on a subject instance. Commands are JSON arrays of {source, predicate, target, action} objects."
    )]
    async fn execute_commands(&self, params: Parameters<ExecuteCommandsParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
        }
    }

    /// Run a Prolog query for complex reasoning
    #[tool(
        description = "Run a Prolog query on a perspective for complex reasoning. The link graph is exposed as Prolog facts (triple/3), enabling pattern matching and inference beyond simple link queries. Example: 'triple(X, \"rdf://type\", \"ad4m://SubjectClass\")' finds all subject classes. Use for advanced queries not covered by other tools."
    )]
    async fn infer(&self, params: Parameters<InferParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((perspective, agent_context)) => {
                match perspective
                    .prolog_query_with_context(p.query.clone(), &agent_context)
                    .await
                {
                    Ok(result) => prolog_resolution_to_string(result),
                    Err(e) => format!("Error running query: {}", e),
                }
            }
            Err(e) => e,
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
        }
    }

    /// Add a flow (state machine definition) to a perspective
    #[tool(
        description = "Register a flow (finite state machine) in a perspective. Flows define states and transitions for expressions."
    )]
    async fn add_flow(&self, params: Parameters<AddFlowParams>) -> String {
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
        }
    }

    /// Execute a transition action on an expression in a flow
    #[tool(
        description = "Execute a transition action on an expression within a flow (state machine). The expression must be in a state that has the given action available. Use flow_actions to see available actions for the current state. Example: flow_run_action(perspective_id='abc', flow_name='MessageFlow', expression_address='literal://string:xyz', action_name='approve')."
    )]
    async fn flow_run_action(&self, params: Parameters<FlowRunActionParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((_perspective, _agent_context)) => {
                // TODO: Flow action execution requires running the action's commands
                // on the perspective instance. The actions are defined in SHACL Flow SDNA
                // and consist of add/remove link commands. Implementation needs:
                // 1. Load flow definition from SDNA
                // 2. Find current state of expression
                // 3. Verify action is available in current state
                // 4. Execute the action's commands (add/remove links)
                // 5. Return new state
                json!({
                    "success": false,
                    "expression": p.expression_address,
                    "flow": p.flow_name,
                    "action": p.action_name,
                    "message": "Flow action execution not yet implemented — requires SHACL Flow command execution"
                })
                .to_string()
            }
            Err(e) => e,
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
        description = "Set a property on a subject instance. Works at the model level — you provide the property name (e.g. 'name', 'body') and the tool handles the underlying link operations. No need to know predicates or link structure. Example: set_subject_property(perspective_id='abc-123', class_name='Channel', expression_address='literal://string:xyz', property_name='name', value='general')."
    )]
    async fn set_subject_property(&self, params: Parameters<SetSubjectPropertyParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
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

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
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
            Err(e) => e,
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
        description = "Request a capability token (step 1/2 of local auth flow). This is the primary way to authenticate with a local/single-user AD4M executor. Returns request_id and code — pass both to generate_jwt to get a JWT token. For multi-user executors, use login_email or signup instead. Note: when using the ad4m-executor CLI, the verification code is logged to stdout."
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
                let source = link
                    .pointer("/data/source")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");
                let predicate = link
                    .pointer("/data/predicate")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");
                let target = link
                    .pointer("/data/target")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");

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

        let did = agent
            .get("did")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
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
            let source = link
                .pointer("/data/source")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let predicate = link
                .pointer("/data/predicate")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let target = link
                .pointer("/data/target")
                .and_then(|v| v.as_str())
                .unwrap_or("");

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
                if let Some(u) = p.username.as_ref() {
                    updated["username"] = json!(u);
                }
                if let Some(g) = p.given_name.as_ref() {
                    updated["given_name"] = json!(g);
                }
                if let Some(f) = p.family_name.as_ref() {
                    updated["family_name"] = json!(f);
                }
                if let Some(e) = p.email.as_ref() {
                    updated["email"] = json!(e);
                }
                if let Some(b) = p.bio.as_ref() {
                    updated["bio"] = json!(b);
                }
                updated.to_string()
            }
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }

    /// Set the agent's profile picture
    #[tool(
        description = "Set the current agent's profile picture. Provide raw base64-encoded image data (NOT a data URI). The image will be uploaded to the centralized file store and linked in the agent's public profile. For best results, use a square image (Flux will display it as a circle)."
    )]
    async fn set_agent_profile_picture(
        &self,
        params: Parameters<SetAgentProfilePictureParams>,
    ) -> String {
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
            Err(e) => {
                return json!({"error": format!("Failed to upload image: {}", e)}).to_string()
            }
        };

        // Get current agent perspective
        let script = r#"JSON.stringify(await core.callResolver("Query", "agent", {}))"#;
        let result = match js.execute(script.to_string()).await {
            Ok(r) => r,
            Err(e) => return json!({"error": format!("Failed to get agent: {}", e)}).to_string(),
        };
        let agent: serde_json::Value = serde_json::from_str(&result).unwrap_or(json!({}));
        let did = agent
            .get("did")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let current_links = agent
            .get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .cloned()
            .unwrap_or_default();

        // Keep all links except image links
        let mut all_links: Vec<serde_json::Value> = Vec::new();
        for link in &current_links {
            let predicate = link
                .pointer("/data/predicate")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            if predicate == "sioc://has_profile_image"
                || predicate == "sioc://has_profile_thumbnail_image"
            {
                continue;
            }
            let source = link
                .pointer("/data/source")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let target = link
                .pointer("/data/target")
                .and_then(|v| v.as_str())
                .unwrap_or("");
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
            })
            .to_string(),
            Err(e) => json!({"error": format!("Failed to update profile: {}", e)}).to_string(),
        }
    }

    // ========================================================================
    // GENERIC AGENT PUBLIC PERSPECTIVE TOOLS
    // ========================================================================

    /// Get an agent's public perspective as raw links
    #[tool(
        description = "Get an agent's public perspective — the set of links they publish publicly via their DID. Contains profile info, capabilities, and any other public statements. Pass a DID to look up another agent, or omit to get your own. Returns a JSON array of links."
    )]
    async fn get_agent_public_perspective(
        &self,
        params: Parameters<GetAgentPublicPerspectiveParams>,
    ) -> String {
        let _agent_context = self.get_agent_context_for_read().await;

        let mut js = self.context.js_handle.clone();

        let js_code = if let Some(did) = &params.0.did {
            format!(
                "JSON.stringify(await core.callResolver(\"Query\", \"agentByDID\", {{ did: \"{}\" }}))",
                did
            )
        } else {
            "JSON.stringify(await core.callResolver(\"Query\", \"agent\", {}))".to_string()
        };

        match js.execute(js_code).await {
            Ok(result) => result,
            Err(e) => {
                json!({"error": format!("Failed to get agent perspective: {}", e)}).to_string()
            }
        }
    }

    /// Set the agent's public perspective from raw links
    #[tool(
        description = "Set the current agent's public perspective — replaces ALL public links with the provided array. This is the low-level API; use set_agent_profile for structured profile updates. WARNING: This replaces the entire perspective, so include all links you want to keep. Format: [{\"source\": \"...\", \"predicate\": \"...\", \"target\": \"...\"}]."
    )]
    async fn set_agent_public_perspective(
        &self,
        params: Parameters<SetAgentPublicPerspectiveParams>,
    ) -> String {
        let _agent_context = match self.get_agent_context().await {
            Ok(ctx) => ctx,
            Err(e) => return format!("Authentication error: {}", e),
        };

        // Parse the links JSON
        let links: Vec<serde_json::Value> = match serde_json::from_str(&params.0.links_json) {
            Ok(l) => l,
            Err(e) => return json!({"error": format!("Invalid links JSON: {}", e)}).to_string(),
        };

        // Convert to the format expected by agentUpdatePublicPerspective
        let perspective = json!({ "links": links });

        let mut js = self.context.js_handle.clone();

        let js_code = format!(
            "JSON.stringify(await core.callResolver(\"Mutation\", \"agentUpdatePublicPerspective\", {{ perspective: {} }}))",
            perspective
        );

        match js.execute(js_code).await {
            Ok(result) => result,
            Err(e) => {
                json!({"error": format!("Failed to update agent perspective: {}", e)}).to_string()
            }
        }
    }

    // ========================================================================
    // MODEL SUBSCRIPTION TOOLS
    // ========================================================================

    /// Subscribe to model changes in a perspective
    #[tool(
        description = "Generate a SurrealQL query for watching changes to a subject class in a perspective. Returns a subscription_id and the query to pass to the waker process. The waker uses perspectiveSubscribeSurrealQuery (same mechanism as Flux UI) for live updates. Flow: 1) Call this tool to get the query, 2) Store subscription_id + context in memory, 3) Pass query + id to the waker, 4) When woken, use MCP tools to fetch the latest data."
    )]
    async fn subscribe_to_model(&self, params: Parameters<SubscribeToModelParams>) -> String {
        let _capabilities = match self.get_capabilities().await {
            Ok(c) => c,
            Err(e) => return format!("Authentication error: {}", e),
        };

        let p = &params.0;

        // Verify the perspective exists
        if get_perspective(&p.perspective_id).is_none() {
            return json!({"error": format!("Perspective not found: {}", p.perspective_id)})
                .to_string();
        }

        // Build SurrealQL query for the waker's perspectiveSubscribeSurrealQuery
        let query = if let Some(ref parent) = p.parent_address {
            // Watch for new children of a specific parent (e.g., messages in a channel)
            format!(
                "SELECT * FROM link WHERE source = 'literal://string:{}' AND predicate = 'ad4m://has_child'",
                parent
            )
        } else if let Some(ref entry_type) = p.entry_type {
            // Watch for instances by explicit entry type predicate
            format!(
                "SELECT * FROM link WHERE predicate = 'flux://entry_type' AND target = '{}'",
                entry_type
            )
        } else {
            // Default: watch for all new links (broad subscription)
            "SELECT * FROM link ORDER BY timestamp DESC LIMIT 50".to_string()
        };

        // Generate a subscription ID
        let subscription_id = uuid::Uuid::new_v4().to_string();

        json!({
            "subscription_id": subscription_id,
            "perspective_id": p.perspective_id,
            "class_name": p.class_name,
            "parent_address": p.parent_address,
            "surreal_query": query,
            "waker_config": {
                "id": subscription_id,
                "perspective": p.perspective_id,
                "query": query,
            },
            "message": format!(
                "Subscription {} created for {} changes{}. Add the waker_config entry to your waker's config file and restart it. The waker uses perspectiveSubscribeSurrealQuery (same as Flux UI) for live change detection. Store this subscription_id in your memory with its context so you know what to do when woken.",
                subscription_id,
                p.class_name,
                p.parent_address.as_ref().map(|a| format!(" under parent {}", a)).unwrap_or_default()
            ),
        }).to_string()
    }
}
