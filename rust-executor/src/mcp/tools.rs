//! MCP Tools for AD4M Subject/Model operations
//!
//! These tools expose AD4M's Subject system via MCP, allowing AI agents to
//! work with typed models (get properties, run actions) instead of raw links.

use super::server::McpContext;
use crate::agent::capabilities::{
    capabilities_from_token, check_capability,
    defs::PERSPECTIVE_CREATE_CAPABILITY,
    get_user_default_capabilities,
    token::{decode_jwt, generate_jwt},
    AuthInfo, Capability, DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::graphql::graphql_types::{LinkQuery, LinkStatus, PerspectiveHandle, PerspectiveState};
use crate::perspectives::perspective_instance::{Command, Parameter, SdnaType, SubjectClassOption};
use crate::perspectives::utils::prolog_resolution_to_string;
use crate::perspectives::{add_perspective, all_perspectives, get_perspective};
use crate::types::Link;
use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{Implementation, ProtocolVersion, ServerCapabilities, ServerInfo, ToolsCapability},
    tool, tool_handler, tool_router, ServerHandler,
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

/// Parameters for setting an existing token directly
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetTokenParams {
    /// JWT capability token
    pub token: String,
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
pub struct AddSdnaParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Subject class name
    pub class_name: String,
    /// SHACL shape definition as JSON string
    pub shacl_json: String,
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
    /// Subject class name of the parent
    pub class_name: String,
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

    /// Get agent context, requiring authentication
    async fn get_agent_context(&self) -> Result<AgentContext, String> {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => Ok(AgentContext::from_auth_token(token)),
            _ => Err("Authentication required. Use login_email or set_token first.".to_string()),
        }
    }

    /// Get capabilities from the stored auth token (reuses same logic as GraphQL RequestContext)
    async fn get_capabilities(&self) -> Result<Vec<Capability>, String> {
        let token = self.get_auth_token().await;
        let admin_cred = self.context.admin_credential.clone();
        capabilities_from_token(token.unwrap_or_default(), admin_cred)
    }

    /// Get agent context for read operations - allows unauthenticated access for local/main agent
    async fn get_agent_context_for_read(&self) -> AgentContext {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => AgentContext::from_auth_token(token),
            _ => AgentContext::from_auth_token(String::new()),
        }
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
    #[tool(description = "List all AD4M perspectives available to the current user")]
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

    /// List all subject classes (model types) defined in a perspective
    #[tool(
        description = "List all subject classes (model types) defined in a perspective. Returns the available models you can query and create instances of."
    )]
    async fn list_subject_classes(&self, params: Parameters<ListSubjectClassesParams>) -> String {
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
        description = "Run a Prolog query on a perspective for complex reasoning and custom queries. Use this for advanced queries not covered by other tools. Note: Query is executed as raw Prolog."
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
    #[tool(description = "Add a link (source, predicate, target) to a perspective.")]
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
        description = "Query links in a perspective with optional source, predicate, and target filters."
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

    /// Add SDNA (subject class definition) to a perspective
    #[tool(
        description = "Register a subject class in a perspective using SHACL JSON definition. This defines the schema for typed model objects."
    )]
    async fn add_sdna(&self, params: Parameters<AddSdnaParams>) -> String {
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

    /// Create a new perspective
    #[tool(description = "Create a new perspective (local knowledge graph) with a given name.")]
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
                    .resolve_collection_predicate(&perspective, &p.class_name, &p.collection_name)
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
                    .resolve_collection_predicate(&perspective, &p.class_name, &p.collection_name)
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
                    .resolve_collection_predicate(&perspective, &p.class_name, &p.collection_name)
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
    async fn get_subject_children(
        &self,
        params: Parameters<GetSubjectChildrenParams>,
    ) -> String {
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
                                        l.data
                                            .target
                                            .split("://")
                                            .last()
                                            .unwrap_or("")
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
    async fn resolve_collection_predicate(
        &self,
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        class_name: &str,
        collection_name: &str,
    ) -> Result<String, String> {
        // Same as resolve_property_predicate but looks for collection=true properties
        // For now, use the same resolution — collections are also properties with a path
        self.resolve_property_predicate(perspective, class_name, collection_name)
            .await
    }

    // ========================================================================
    // AUTHENTICATION TOOLS
    // ========================================================================

    /// Login with email and password (multi-user mode)
    #[tool(
        description = "Login to a multi-user AD4M executor using email and password. Returns a JWT token on success that will be used for subsequent operations."
    )]
    async fn login_email(&self, params: Parameters<LoginEmailParams>) -> String {
        let p = &params.0;

        // Normalize email
        let email = p.email.trim().to_lowercase();

        // Check if multi-user mode is enabled
        let multi_user_enabled =
            Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));

        if !multi_user_enabled {
            return json!({
                "success": false,
                "error": "Multi-user mode is not enabled. Use set_token with an admin credential instead."
            })
            .to_string();
        }

        // Verify user credentials
        let password_valid = Ad4mDb::with_global_instance(|db| {
            db.verify_user_password(&email, &p.password)
                .unwrap_or(false)
        });

        if !password_valid {
            return json!({
                "success": false,
                "error": "Invalid credentials"
            })
            .to_string();
        }

        // Check user exists in agent service
        if !AgentService::user_exists(&email) {
            return json!({
                "success": false,
                "error": "User key not found on executor"
            })
            .to_string();
        }

        // Create auth info with user-scoped capabilities
        let auth_info = AuthInfo {
            app_name: "mcp-agent".to_string(),
            app_desc: "MCP AI Agent".to_string(),
            app_domain: Some("mcp".to_string()),
            app_url: Some("https://ad4m.dev/mcp".to_string()),
            app_icon_path: None,
            capabilities: Some(get_user_default_capabilities()),
            user_email: Some(email.clone()),
        };

        // Generate JWT token
        match generate_jwt(
            auth_info.app_name.clone(),
            DEFAULT_TOKEN_VALID_PERIOD,
            auth_info,
        ) {
            Ok(cap_token) => {
                // Store the token in context for subsequent operations
                let mut token_guard = self.context.auth_token.write().await;
                *token_guard = Some(cap_token.clone());

                json!({
                    "success": true,
                    "token": cap_token,
                    "user_email": email,
                    "message": "Login successful. Token stored for subsequent operations."
                })
                .to_string()
            }
            Err(e) => json!({
                "success": false,
                "error": format!("Failed to generate token: {}", e)
            })
            .to_string(),
        }
    }

    /// Set an existing JWT token for authentication
    #[tool(
        description = "Set an existing JWT capability token for authentication. Use this for local executors with admin credentials or when you already have a valid token."
    )]
    async fn set_token(&self, params: Parameters<SetTokenParams>) -> String {
        let token = params.0.token.trim().to_string();

        if token.is_empty() {
            return json!({
                "success": false,
                "error": "Token cannot be empty"
            })
            .to_string();
        }

        // Validate the token format and extract info
        match decode_jwt(token.clone()) {
            Ok(claims) => {
                // Store the token
                let mut token_guard = self.context.auth_token.write().await;
                *token_guard = Some(token.clone());

                json!({
                    "success": true,
                    "app_name": claims.capabilities.app_name,
                    "user_email": claims.capabilities.user_email,
                    "message": "Token set successfully."
                })
                .to_string()
            }
            Err(e) => {
                // Check if it's the admin credential (not a JWT)
                if let Some(admin_cred) = &self.context.admin_credential {
                    if &token == admin_cred {
                        let mut token_guard = self.context.auth_token.write().await;
                        *token_guard = Some(token.clone());

                        return json!({
                            "success": true,
                            "is_admin": true,
                            "message": "Admin credential set successfully."
                        })
                        .to_string();
                    }
                }

                json!({
                    "success": false,
                    "error": format!("Invalid token: {}", e)
                })
                .to_string()
            }
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
                        // Might be admin credential
                        if let Some(admin_cred) = &self.context.admin_credential {
                            if t == admin_cred {
                                return json!({
                                    "authenticated": true,
                                    "is_admin": true,
                                    "message": "Authenticated with admin credential"
                                })
                                .to_string();
                            }
                        }
                        json!({
                            "authenticated": false,
                            "token_type": "unknown",
                            "message": "Token set but invalid - could not decode and not recognized as admin credential"
                        })
                        .to_string()
                    }
                }
            }
            _ => json!({
                "authenticated": false,
                "message": "Not authenticated. Use login_email or set_token to authenticate."
            })
            .to_string(),
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
    async fn test_set_token_stores_value() {
        let ctx = TestAuthContext::new(None);

        // Simulate set_token logic
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
