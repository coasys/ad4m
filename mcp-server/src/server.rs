//! AD4M MCP Server implementation

use ad4m_client::Ad4mClient;
use anyhow::Result;
use rmcp::{
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{Implementation, ProtocolVersion, ServerCapabilities, ServerInfo, ToolsCapability},
    tool, tool_handler, tool_router, ServerHandler,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Parameters for list_perspectives tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListPerspectivesParams {}

/// Parameters for get_perspective tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetPerspectiveParams {
    /// Perspective UUID
    pub uuid: String,
}

/// Parameters for query_links tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct QueryLinksParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Source URI filter (optional)
    pub source: Option<String>,
    /// Target URI filter (optional)  
    pub target: Option<String>,
    /// Predicate URI filter (optional)
    pub predicate: Option<String>,
}

/// Parameters for add_link tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddLinkParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Source URI
    pub source: String,
    /// Target URI
    pub target: String,
    /// Predicate URI (optional)
    pub predicate: Option<String>,
}

/// Parameters for run_prolog tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct RunPrologParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Prolog query string
    pub query: String,
}

/// Parameters for create_perspective tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct CreatePerspectiveParams {
    /// Name for the new perspective
    pub name: String,
}

/// Parameters for login tool (multi-user mode)
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct LoginParams {
    /// Email address
    pub email: String,
    /// Password
    pub password: String,
}

/// Parameters for set_token tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct SetTokenParams {
    /// Capability token (JWT)
    pub token: String,
}

/// Parameters for auth_status tool
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AuthStatusParams {}

/// AD4M MCP Server
#[derive(Clone)]
pub struct Ad4mMcpServer {
    executor_url: String,
    token: Arc<RwLock<Option<String>>>,
    tool_router: ToolRouter<Self>,
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for Ad4mMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability {
                    list_changed: Some(false),
                }),
                ..Default::default()
            },
            server_info: Implementation::from_build_env(),
            ..Default::default()
        }
    }
}

#[tool_router]
impl Ad4mMcpServer {
    pub async fn new(
        executor_url: String,
        token: Option<String>,
        _admin_credential: Option<String>,
    ) -> Result<Self> {
        // Validate connection if we have a token
        if let Some(ref t) = token {
            let client = Ad4mClient::new(executor_url.clone(), t.clone());
            match client.runtime.info().await {
                Ok(info) => {
                    tracing::info!("Connected to AD4M executor v{}", info.ad4m_executor_version);
                }
                Err(e) => {
                    tracing::warn!("Could not validate connection: {}", e);
                }
            }
        }

        Ok(Self {
            executor_url,
            token: Arc::new(RwLock::new(token)),
            tool_router: Self::tool_router(),
        })
    }

    /// Get AD4M client with current token
    async fn get_client(&self) -> Result<Ad4mClient> {
        let token = self.token.read().await;
        let t = token
            .clone()
            .ok_or_else(|| anyhow::anyhow!("No authentication token configured"))?;
        Ok(Ad4mClient::new(self.executor_url.clone(), t))
    }

    /// Serve via stdio transport
    pub async fn serve_stdio(self) -> Result<()> {
        use rmcp::{transport::stdio, ServiceExt};

        tracing::info!("Starting MCP server on stdio");
        let transport = stdio();
        let running = self.serve(transport).await?;
        running.waiting().await?;
        Ok(())
    }

    // === MCP TOOLS ===

    /// List all AD4M perspectives available to the current user
    #[tool(description = "List all AD4M perspectives available to the current user")]
    async fn list_perspectives(&self, _params: Parameters<ListPerspectivesParams>) -> String {
        match self.get_client().await {
            Ok(client) => match client.perspectives.all().await {
                Ok(perspectives) => {
                    let result: Vec<serde_json::Value> = perspectives
                        .iter()
                        .map(|p| {
                            json!({
                                "uuid": p.uuid,
                                "name": p.name,
                                "shared_url": p.shared_url,
                                "has_neighbourhood": p.neighbourhood.is_some(),
                            })
                        })
                        .collect();
                    serde_json::to_string_pretty(&result)
                        .unwrap_or_else(|e| format!("Error: {}", e))
                }
                Err(e) => format!("Error listing perspectives: {}", e),
            },
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    /// Get a snapshot of a specific perspective (all links)
    #[tool(description = "Get a snapshot of a specific perspective with all its links")]
    async fn get_perspective(&self, params: Parameters<GetPerspectiveParams>) -> String {
        let uuid = &params.0.uuid;
        match self.get_client().await {
            Ok(client) => {
                match client.perspectives.snapshot(uuid.clone()).await {
                    Ok(snapshot) => {
                        // Convert to simple JSON - snapshot.links is Vec<LinkExpression>
                        let links: Vec<serde_json::Value> = snapshot
                            .links
                            .iter()
                            .map(|link| {
                                json!({
                                    "source": link.data.source,
                                    "target": link.data.target,
                                    "predicate": link.data.predicate,
                                    "author": link.author,
                                    "timestamp": link.timestamp,
                                })
                            })
                            .collect();
                        let result = json!({
                            "uuid": uuid,
                            "link_count": links.len(),
                            "links": links
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error getting perspective snapshot: {}", e),
                }
            }
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    /// Query links in a perspective with optional filters
    #[tool(
        description = "Query links in a perspective with optional source/target/predicate filters"
    )]
    async fn query_links(&self, params: Parameters<QueryLinksParams>) -> String {
        let p = &params.0;
        match self.get_client().await {
            Ok(client) => {
                match client
                    .perspectives
                    .query_links(
                        p.perspective_id.clone(),
                        p.source.clone(),
                        p.target.clone(),
                        p.predicate.clone(),
                        None, // from_date
                        None, // until_date
                        None, // limit
                    )
                    .await
                {
                    Ok(links) => {
                        let result: Vec<serde_json::Value> = links
                            .iter()
                            .map(|link| {
                                json!({
                                    "source": link.data.source,
                                    "target": link.data.target,
                                    "predicate": link.data.predicate,
                                    "author": link.author,
                                    "timestamp": link.timestamp,
                                })
                            })
                            .collect();
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying links: {}", e),
                }
            }
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    /// Add a link to a perspective
    #[tool(description = "Add a new link to a perspective")]
    async fn add_link(&self, params: Parameters<AddLinkParams>) -> String {
        let p = &params.0;
        match self.get_client().await {
            Ok(client) => {
                match client
                    .perspectives
                    .add_link(
                        p.perspective_id.clone(),
                        p.source.clone(),
                        p.target.clone(),
                        p.predicate.clone(),
                        None, // status
                    )
                    .await
                {
                    Ok(_link) => {
                        let result = json!({
                            "created": true,
                            "perspective_id": p.perspective_id,
                            "source": p.source,
                            "target": p.target,
                            "predicate": p.predicate
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding link: {}", e),
                }
            }
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    /// Run a Prolog query on a perspective
    #[tool(
        description = "Run a Prolog query on a perspective for complex reasoning and model queries"
    )]
    async fn run_prolog(&self, params: Parameters<RunPrologParams>) -> String {
        let p = &params.0;
        match self.get_client().await {
            Ok(client) => {
                match client
                    .perspectives
                    .infer(p.perspective_id.clone(), p.query.clone())
                    .await
                {
                    Ok(result) => serde_json::to_string_pretty(&result)
                        .unwrap_or_else(|e| format!("Error: {}", e)),
                    Err(e) => format!("Error running Prolog query: {}", e),
                }
            }
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    /// Create a new perspective
    #[tool(description = "Create a new perspective with the given name")]
    async fn create_perspective(&self, params: Parameters<CreatePerspectiveParams>) -> String {
        let name = &params.0.name;
        match self.get_client().await {
            Ok(client) => match client.perspectives.add(name.clone()).await {
                Ok(uuid) => {
                    let result = json!({
                        "created": true,
                        "uuid": uuid,
                        "name": name
                    });
                    serde_json::to_string_pretty(&result)
                        .unwrap_or_else(|e| format!("Error: {}", e))
                }
                Err(e) => format!("Error creating perspective: {}", e),
            },
            Err(e) => format!("Error getting client: {}", e),
        }
    }

    // === AUTHENTICATION TOOLS ===

    /// Login with email and password (multi-user mode)
    #[tool(
        description = "Login to AD4M with email and password. Use this for multi-user hosted instances. Returns a JWT token on success."
    )]
    async fn login(&self, params: Parameters<LoginParams>) -> String {
        let p = &params.0;
        // For login, we don't need an existing token
        let client = Ad4mClient::new(self.executor_url.clone(), String::new());

        match client
            .runtime
            .login_user(p.email.clone(), p.password.clone())
            .await
        {
            Ok(jwt) => {
                // Store the token for subsequent calls
                let mut token = self.token.write().await;
                *token = Some(jwt.clone());

                let result = json!({
                    "success": true,
                    "message": "Login successful. Token has been stored for subsequent calls.",
                    "token_preview": format!("{}...", &jwt[..std::cmp::min(20, jwt.len())])
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => {
                let result = json!({
                    "success": false,
                    "error": format!("{}", e)
                });
                serde_json::to_string_pretty(&result)
                    .unwrap_or_else(|_| format!("Login failed: {}", e))
            }
        }
    }

    /// Set authentication token directly
    #[tool(
        description = "Set the authentication token directly. Use this if you have a capability token from the AD4M launcher or another source."
    )]
    async fn set_token(&self, params: Parameters<SetTokenParams>) -> String {
        let new_token = params.0.token.clone();

        // Validate the token by trying to get agent status
        let client = Ad4mClient::new(self.executor_url.clone(), new_token.clone());

        match client.agent.status().await {
            Ok(status) => {
                // Token is valid, store it
                let mut token = self.token.write().await;
                *token = Some(new_token);

                let result = json!({
                    "success": true,
                    "message": "Token validated and stored.",
                    "agent_did": status.did,
                    "is_unlocked": status.is_unlocked
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => {
                let result = json!({
                    "success": false,
                    "error": format!("Token validation failed: {}", e),
                    "hint": "Make sure the token is valid and the AD4M executor is running."
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|_| format!("Error: {}", e))
            }
        }
    }

    /// Check authentication status
    #[tool(
        description = "Check current authentication status. Returns agent info if authenticated, or authentication requirements if not."
    )]
    async fn auth_status(&self, _params: Parameters<AuthStatusParams>) -> String {
        let token = self.token.read().await;

        if token.is_none() {
            let result = json!({
                "authenticated": false,
                "message": "No authentication token configured.",
                "options": [
                    "Use 'login' tool with email/password for multi-user mode",
                    "Use 'set_token' tool to provide a capability token",
                    "Start the server with --token flag"
                ]
            });
            return serde_json::to_string_pretty(&result)
                .unwrap_or_else(|e| format!("Error: {}", e));
        }

        let client = Ad4mClient::new(self.executor_url.clone(), token.clone().unwrap());

        match client.agent.me().await {
            Ok(agent) => {
                let result = json!({
                    "authenticated": true,
                    "agent": {
                        "did": agent.did,
                        "direct_message_language": agent.direct_message_language
                    }
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
            Err(e) => {
                let result = json!({
                    "authenticated": false,
                    "error": format!("Token appears invalid: {}", e),
                    "hint": "Try logging in again or providing a new token."
                });
                serde_json::to_string_pretty(&result).unwrap_or_else(|e| format!("Error: {}", e))
            }
        }
    }
}
