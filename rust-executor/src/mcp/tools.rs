//! MCP Tools for AD4M Subject/Model operations
//!
//! These tools expose AD4M's Subject system via MCP, allowing AI agents to
//! work with typed models (get properties, run actions) instead of raw links.

use super::server::McpContext;
use crate::agent::capabilities::{
    get_user_default_capabilities,
    token::{decode_jwt, generate_jwt},
    AuthInfo, DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::perspectives::perspective_instance::{Command, Parameter, SubjectClassOption};
use crate::perspectives::utils::prolog_resolution_to_string;
use crate::perspectives::{all_perspectives, get_perspective};
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

    /// Get agent context for read operations - allows unauthenticated access for local/main agent
    async fn get_agent_context_for_read(&self) -> AgentContext {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => AgentContext::from_auth_token(token),
            _ => AgentContext::from_auth_token(String::new()),
        }
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
                let agent_context = self.get_agent_context_for_read().await;

                // Use Prolog to get subject classes
                let query = "subject_class(ClassName, _)".to_string();
                match perspective
                    .prolog_query_with_context(query, &agent_context)
                    .await
                {
                    Ok(result) => prolog_resolution_to_string(result),
                    Err(e) => format!("Error getting subject classes: {}", e),
                }
            }
            None => format!("Perspective not found: {}", uuid),
        }
    }

    /// Query instances of a subject class with optional filters
    #[tool(
        description = "Query instances of a subject class (model) with optional Prolog filters. Returns all instances matching the criteria. WARNING: The 'query' parameter accepts raw Prolog syntax and is intended for trusted AI agents only."
    )]
    async fn query_subjects(&self, params: Parameters<QuerySubjectsParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(perspective) => {
                let agent_context = self.get_agent_context_for_read().await;

                // Escape class_name to prevent Prolog injection
                let escaped_class_name = Self::escape_prolog_string(&p.class_name);

                // Build Prolog query for subject instances
                // Note: The filter query is passed directly as it's meant to be Prolog code
                // Users should be aware this accepts raw Prolog syntax
                let query = if let Some(filter) = &p.query {
                    format!(
                        r#"subject_class("{}", C), instance(C, Base), {}"#,
                        escaped_class_name, filter
                    )
                } else {
                    format!(
                        r#"subject_class("{}", C), instance(C, Base)"#,
                        escaped_class_name
                    )
                };

                match perspective
                    .prolog_query_with_context(query, &agent_context)
                    .await
                {
                    Ok(result) => prolog_resolution_to_string(result),
                    Err(e) => format!("Error querying subjects: {}", e),
                }
            }
            None => format!("Perspective not found: {}", p.perspective_id),
        }
    }

    /// Get all data (properties) for a specific subject instance
    #[tool(
        description = "Get all data (properties and values) for a specific subject instance. Returns the complete state of the model instance."
    )]
    async fn get_subject_data(&self, params: Parameters<GetSubjectDataParams>) -> String {
        let p = &params.0;

        match get_perspective(&p.perspective_id) {
            Some(mut perspective) => {
                let agent_context = self.get_agent_context_for_read().await;

                let subject_class: SubjectClassOption = match serde_json::from_value(json!({
                    "className": p.class_name
                })) {
                    Ok(sc) => sc,
                    Err(e) => return format!("Error creating subject class: {}", e),
                };

                match perspective
                    .get_subject_data(subject_class, p.expression_address.clone(), &agent_context)
                    .await
                {
                    Ok(data) => data,
                    Err(e) => format!("Error getting subject data: {}", e),
                }
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
