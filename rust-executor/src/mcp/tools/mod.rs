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
//!
//! ## Module Structure
//!
//! Tools are organized by domain:
//! - `perspectives` — perspective & link operations
//! - `subjects` — subject CRUD, properties, collections
//! - `flows` — flow state machine tools
//! - `auth` — authentication & JWT tools
//! - `profiles` — agent profile management
//! - `subscriptions` — waker query generation
//! - `dynamic` — auto-generated SHACL-based tools

pub mod auth;
pub mod children;
pub mod dynamic;
pub mod flows;
pub mod languages;
pub mod neighbourhoods;
pub mod perspectives;
pub mod profiles;
pub mod subjects;
pub mod subscriptions;

use super::server::McpContext;
use crate::agent::capabilities::{
    capabilities_from_token, check_capability, defs::PERSPECTIVE_CREATE_CAPABILITY, Capability,
};
use crate::agent::AgentContext;
use crate::graphql::graphql_types::PerspectiveHandle;
use crate::perspectives::get_perspective;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use rmcp::{
    handler::server::{router::tool::ToolRouter, tool::ToolCallContext},
    model::{
        CallToolRequestParams, CallToolResult, Content, Implementation, ListToolsResult,
        PaginatedRequestParams, ProtocolVersion, ServerCapabilities, ServerInfo, ToolsCapability,
    },
    service::RequestContext,
    ErrorData, RoleServer, ServerHandler,
};

use serde_json::json;

// HTTP request parts injected by rmcp's streamable-HTTP transport into each request's extensions.
// Used to extract the Authorization header for admin-credential bypass.
use crate::utils::constant_time_eq;
use axum::http::request::Parts as HttpRequestParts;

// ============================================================================
// MCP Handler
// ============================================================================

/// AD4M MCP Handler - exposes Subject operations as MCP tools
#[derive(Clone)]
pub struct Ad4mMcpHandler {
    pub(crate) context: McpContext,
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

        // Check if tool requires authentication
        if !AUTH_TOOLS.contains(&tool_name.as_str()) {
            if !self.check_auth(&tool_name, &context).await {
                return Ok(CallToolResult::error(vec![Content::text(
                    json!({"error": "Authentication required. Use request_capability + generate_jwt, login_email, or signup to authenticate."}).to_string()
                )]));
            }
        }

        self.dispatch_tool(request, context).await
    }
}

impl Ad4mMcpHandler {
    pub fn new(context: McpContext) -> Self {
        Self {
            context,
            tool_router: Self::tool_router(),
        }
    }

    /// Unified authentication check for MCP tool calls.
    ///
    /// Different MCP clients provide credentials in different ways:
    /// - Some store the JWT/credential in the MCP session (via generate_jwt/login_email)
    /// - Some send it per-request in the HTTP Authorization header (mcporter, .mcp.json headers)
    /// - Some do both
    ///
    /// This method checks all sources and stores the token in the session if found
    /// via HTTP header, so downstream helpers (get_agent_context, get_capabilities) work.
    async fn check_auth(&self, _tool_name: &str, context: &RequestContext<RoleServer>) -> bool {
        let session_token = self
            .context
            .auth_token
            .read()
            .await
            .clone()
            .unwrap_or_default();

        let admin_cred = self.context.admin_credential.as_deref();

        // 1. Session token matches admin credential → pass
        if let Some(cred) = admin_cred {
            if !session_token.is_empty() && constant_time_eq(&session_token, cred) {
                return true;
            }
        }

        // 2. Session token is a valid JWT → pass
        if !session_token.is_empty() {
            let caps = capabilities_from_token(session_token.clone(), admin_cred.map(String::from));
            if caps.is_ok() {
                return true;
            }
        }

        // 3. Try HTTP Authorization header (for clients like mcporter that send
        //    credentials per-request rather than using the MCP session)
        let http_header_value = context
            .extensions
            .get::<HttpRequestParts>()
            .and_then(|parts| parts.headers.get(axum::http::header::AUTHORIZATION))
            .and_then(|h| h.to_str().ok())
            .map(|h| {
                let mut parts = h.splitn(2, char::is_whitespace);
                let scheme = parts.next().unwrap_or_default();
                let value = parts.next().unwrap_or_default().trim_start();

                if scheme.eq_ignore_ascii_case("bearer") && !value.is_empty() {
                    value.to_string()
                } else {
                    h.to_string()
                }
            });

        if let Some(ref header_token) = http_header_value {
            // 3a. Header matches admin credential → store & pass
            if let Some(cred) = admin_cred {
                if constant_time_eq(header_token, cred) {
                    let mut guard = self.context.auth_token.write().await;
                    *guard = Some(cred.to_string());
                    return true;
                }
            }

            // 3b. Header is a valid JWT → store & pass
            if !header_token.is_empty() {
                let caps =
                    capabilities_from_token(header_token.clone(), admin_cred.map(String::from));
                if caps.is_ok() {
                    let mut guard = self.context.auth_token.write().await;
                    *guard = Some(header_token.clone());
                    return true;
                }
            }
        }

        // 4. No admin credential configured and no token → single-user local mode
        //    (mirrors GraphQL localhost trust model)
        if admin_cred.is_none() && session_token.is_empty() && http_header_value.is_none() {
            return true;
        }

        // Everything else → reject
        false
    }

    /// Build the tool router manually, registering all tools from domain modules.
    fn tool_router() -> ToolRouter<Self> {
        ToolRouter::<Self>::new()
            // perspectives.rs
            .with_route((Self::list_perspectives_tool_attr(), Self::list_perspectives))
            .with_route((Self::get_models_tool_attr(), Self::get_models))
            .with_route((Self::add_perspective_tool_attr(), Self::add_perspective))
            .with_route((Self::add_link_tool_attr(), Self::add_link))
            .with_route((Self::query_links_tool_attr(), Self::query_links))
            .with_route((Self::add_model_tool_attr(), Self::add_model))
            .with_route((Self::infer_tool_attr(), Self::infer))
            // subjects.rs
            .with_route((Self::query_subjects_tool_attr(), Self::query_subjects))
            .with_route((Self::get_subject_data_tool_attr(), Self::get_subject_data))
            .with_route((Self::create_subject_tool_attr(), Self::create_subject))
            .with_route((Self::execute_commands_tool_attr(), Self::execute_commands))
            .with_route((
                Self::set_subject_property_tool_attr(),
                Self::set_subject_property,
            ))
            .with_route((
                Self::get_subject_collection_tool_attr(),
                Self::get_subject_collection,
            ))
            .with_route((Self::add_to_collection_tool_attr(), Self::add_to_collection))
            .with_route((
                Self::remove_from_collection_tool_attr(),
                Self::remove_from_collection,
            ))
            .with_route((
                Self::get_subject_children_tool_attr(),
                Self::get_subject_children,
            ))
            .with_route((Self::delete_subject_tool_attr(), Self::delete_subject))
            // flows.rs
            .with_route((Self::add_flow_tool_attr(), Self::add_flow))
            .with_route((Self::get_flows_tool_attr(), Self::get_flows))
            .with_route((Self::flow_state_tool_attr(), Self::flow_state))
            .with_route((Self::flow_actions_tool_attr(), Self::flow_actions))
            .with_route((Self::flow_start_tool_attr(), Self::flow_start))
            .with_route((Self::flow_run_action_tool_attr(), Self::flow_run_action))
            // auth.rs
            .with_route((Self::login_email_tool_attr(), Self::login_email))
            .with_route((
                Self::request_capability_tool_attr(),
                Self::request_capability,
            ))
            .with_route((Self::generate_jwt_tool_attr(), Self::generate_jwt))
            .with_route((Self::signup_tool_attr(), Self::signup))
            .with_route((
                Self::request_login_verification_tool_attr(),
                Self::request_login_verification,
            ))
            .with_route((Self::verify_email_code_tool_attr(), Self::verify_email_code))
            .with_route((Self::auth_status_tool_attr(), Self::auth_status))
            // profiles.rs
            .with_route((Self::get_agent_profile_tool_attr(), Self::get_agent_profile))
            .with_route((Self::set_agent_profile_tool_attr(), Self::set_agent_profile))
            .with_route((
                Self::set_agent_profile_picture_tool_attr(),
                Self::set_agent_profile_picture,
            ))
            .with_route((
                Self::get_agent_public_perspective_tool_attr(),
                Self::get_agent_public_perspective,
            ))
            .with_route((
                Self::set_agent_public_perspective_tool_attr(),
                Self::set_agent_public_perspective,
            ))
            // children.rs
            .with_route((Self::add_child_tool_attr(), Self::add_child))
            .with_route((Self::get_children_tool_attr(), Self::get_children))
            // subscriptions.rs
            .with_route((
                Self::generate_waker_query_tool_attr(),
                Self::generate_waker_query,
            ))
            .with_route((
                Self::get_mention_waker_config_tool_attr(),
                Self::get_mention_waker_config,
            ))
            // languages.rs
            .with_route((Self::language_meta_tool_attr(), Self::language_meta))
            // neighbourhoods.rs
            .with_route((
                Self::list_link_language_templates_tool_attr(),
                Self::list_link_language_templates,
            ))
            .with_route((
                Self::neighbourhood_publish_from_perspective_tool_attr(),
                Self::neighbourhood_publish_from_perspective,
            ))
            .with_route((
                Self::neighbourhood_join_from_url_tool_attr(),
                Self::neighbourhood_join_from_url,
            ))
    }

    /// Dispatch a tool call to either the static router or dynamic SHACL tools
    async fn dispatch_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<CallToolResult, ErrorData> {
        let tool_name = request.name.to_string();

        if self.tool_router.has_route(&tool_name) {
            let tcc = ToolCallContext::new(self, request, context);
            let result = self.tool_router.call(tcc).await?;
            Ok(result)
        } else {
            // Try dynamic SHACL tools
            self.handle_dynamic_tool(&tool_name, request.arguments)
                .await
        }
    }

    // ========================================================================
    // Shared Helpers
    // ========================================================================

    /// Get auth token from context
    pub(crate) async fn get_auth_token(&self) -> Option<String> {
        self.context.auth_token.read().await.clone()
    }

    /// Get agent context, requiring authentication
    pub(crate) async fn get_agent_context(&self) -> Result<AgentContext, String> {
        match self.get_auth_token().await {
            Some(token) if !token.is_empty() => Ok(AgentContext::from_auth_token(token)),
            _ => Err("Authentication required. Use request_capability + generate_jwt, login_email, or signup + verify_email_code first.".to_string()),
        }
    }

    /// Get capabilities from the stored auth token (reuses same logic as GraphQL RequestContext)
    pub(crate) async fn get_capabilities(&self) -> Result<Vec<Capability>, String> {
        let token = self.get_auth_token().await;
        let admin_cred = self.context.admin_credential.clone();
        capabilities_from_token(token.unwrap_or_default(), admin_cred)
    }

    /// Store an auth token in the session and return a success JSON response
    pub(crate) async fn store_token_and_respond(
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

    /// Get the user's email from the JWT token (for multi-user perspective isolation)
    pub(crate) async fn get_user_email(&self) -> Option<String> {
        let token = self.get_auth_token().await.unwrap_or_default();
        crate::agent::capabilities::user_email_from_token(token)
    }

    /// Check if the current user can access a perspective.
    /// In multi-user mode, only perspectives owned by the user (or unowned) are accessible.
    /// In single-user mode, all perspectives are accessible.
    pub(crate) async fn can_access_perspective(&self, perspective: &PerspectiveHandle) -> bool {
        let user_email = self.get_user_email().await;
        crate::graphql::query_resolvers::can_access_perspective(&user_email, perspective)
    }

    /// Get a perspective by ID, verifying the agent is authenticated, has the required capability,
    /// and has access to this perspective (multi-user isolation).
    /// This is the standard entry point for tool handlers — DRYs out the repeated
    /// get_perspective + get_agent_context + check_capability + ownership check pattern.
    pub(crate) async fn get_perspective_with_auth(
        &self,
        perspective_id: &str,
        required_capability: &crate::agent::capabilities::Capability,
    ) -> Result<(PerspectiveInstance, AgentContext), String> {
        let perspective = get_perspective(perspective_id).ok_or_else(|| {
            json!({"error": format!("Perspective not found: {}", perspective_id)}).to_string()
        })?;

        // Check perspective ownership/access (multi-user isolation)
        let handle = perspective.persisted.lock().await.clone();
        if !self.can_access_perspective(&handle).await {
            return Err(
                json!({"error": format!("Perspective not found: {}", perspective_id)}).to_string(),
            );
        }

        let agent_context = self.get_agent_context().await?;

        let capabilities = self.get_capabilities().await;
        if let Err(e) = check_capability(&capabilities, required_capability) {
            return Err(format!("Capability error: {}", e));
        }

        Ok((perspective, agent_context))
    }

    /// Convenience wrapper for read operations — checks perspective access but no write capability.
    ///
    /// In multi-user mode (admin_credential is set), authentication is required before checking
    /// perspective ownership. This prevents unauthenticated clients from reading main-agent
    /// perspectives by exploiting the "no JWT → main agent context" fallback.
    ///
    /// In single-user mode (no admin_credential), behaviour mirrors GraphQL: local trust model,
    /// unauthenticated access is allowed (same as GQL localhost behaviour).
    pub(crate) async fn get_readable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<PerspectiveInstance, String> {
        // Defense in depth: in multi-user/admin mode, require a valid auth token before
        // checking perspective ownership. call_tool() should already have blocked
        // unauthenticated requests, but we enforce this here too to be safe.
        if self.context.admin_credential.is_some() {
            self.get_agent_context()
                .await
                .map_err(|e| json!({"error": e}).to_string())?;
        }

        let perspective = get_perspective(perspective_id).ok_or_else(|| {
            json!({"error": format!("Perspective not found: {}", perspective_id)}).to_string()
        })?;

        let handle = perspective.persisted.lock().await.clone();
        if !self.can_access_perspective(&handle).await {
            return Err(
                json!({"error": format!("Perspective not found: {}", perspective_id)}).to_string(),
            );
        }

        Ok(perspective)
    }

    /// Convenience wrapper for write operations (most common case)
    pub(crate) async fn get_writable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<(PerspectiveInstance, AgentContext), String> {
        self.get_perspective_with_auth(perspective_id, &PERSPECTIVE_CREATE_CAPABILITY)
            .await
    }

    /// Extract a required string argument from the args map (used by dynamic tools)
    pub(crate) fn require_arg<'a>(
        args: &'a serde_json::Map<String, serde_json::Value>,
        key: &str,
    ) -> Result<&'a str, String> {
        args.get(key)
            .and_then(|v| v.as_str())
            .ok_or_else(|| format!("Missing required parameter: {}", key))
    }

    /// Resolve a literal value from a link target URI.
    /// Delegates to rust-client's `Literal` implementation for proper URL decoding.
    pub(crate) fn resolve_literal_value(target: &str) -> String {
        use ad4m_client::literal::Literal;
        if let Ok(literal) = Literal::from_url(target.to_string()) {
            match literal.get() {
                Ok(value) => value.to_string(),
                Err(_) => target.to_string(),
            }
        } else {
            target.to_string()
        }
    }

    /// Encode a string value as a literal:// URL.
    /// Uses rust-client's `Literal` for proper URL encoding.
    pub(crate) fn encode_literal(value: &str) -> String {
        use ad4m_client::literal::Literal;
        Literal::from_string(value.to_string())
            .to_url()
            .unwrap_or_else(|_| format!("literal://string:{}", value))
    }

    /// Get the SHACL name literal for a class, trying both encoded and raw formats.
    /// Flux's TypeScript Literal doesn't URL-encode inner URIs, producing
    /// "literal://string:shacl://Class", while Rust's Literal produces
    /// "literal://string:shacl%3A%2F%2FClass". Returns (encoded, raw).
    pub(crate) fn shacl_name_variants(class_name: &str) -> (String, String) {
        let encoded = Self::encode_literal(&format!("shacl://{}", class_name));
        let raw = format!("literal://string:shacl://{}", class_name);
        (encoded, raw)
    }

    /// Query SHACL shape links for a class, trying encoded then raw name format.
    pub(crate) async fn get_shacl_shape_links(
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        class_name: &str,
    ) -> Vec<crate::types::DecoratedLinkExpression> {
        let (encoded, raw) = Self::shacl_name_variants(class_name);
        let links = perspective
            .get_links(&crate::graphql::graphql_types::LinkQuery {
                source: Some(encoded),
                predicate: Some("ad4m://shacl_shape_uri".to_string()),
                ..Default::default()
            })
            .await
            .unwrap_or_default();
        if !links.is_empty() {
            return links;
        }
        perspective
            .get_links(&crate::graphql::graphql_types::LinkQuery {
                source: Some(raw),
                predicate: Some("ad4m://shacl_shape_uri".to_string()),
                ..Default::default()
            })
            .await
            .unwrap_or_default()
    }

    /// Escape string for safe use in Prolog queries
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn escape_prolog_string(s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('"', "\\\"")
            .replace('\'', "\\'")
    }

    /// Resolve a property name to its predicate URI using SHACL shape links.
    /// Delegates to the shared shacl::resolve_property_predicate helper.
    pub(crate) async fn resolve_property_predicate(
        &self,
        perspective: &PerspectiveInstance,
        class_name: &str,
        property_name: &str,
    ) -> Result<String, String> {
        super::shacl::resolve_property_predicate(perspective, class_name, property_name).await
    }

    /// Resolve a property value through the appropriate language, respecting
    /// the SHACL `resolve_language` setting for the property.
    ///
    /// If the value already has a URI scheme (e.g. `literal://...`, `did:...`),
    /// it is returned as-is. Otherwise, the value is parsed as JSON to recover
    /// its native type (boolean, number, etc.) and resolved through the
    /// perspective's `resolve_property_value` method, which uses the language
    /// controller when a `resolve_language` is set on the property.
    ///
    /// Handles case-insensitive property name matching: dynamic tool names are
    /// lowercased but SHACL stores original case. We resolve to the original
    /// case before calling into the perspective.
    pub(crate) async fn create_property_expression(
        perspective: &PerspectiveInstance,
        class_name: &str,
        property_name: &str,
        value: &str,
        agent_context: &crate::agent::AgentContext,
    ) -> String {
        if value.starts_with("literal://") || value.contains("://") {
            return value.to_string();
        }

        // Resolve the original-cased property name from SHACL.
        // Dynamic tools lowercase the property name (e.g. "isconversation"),
        // but the SHACL links store the original case (e.g. "isConversation").
        let original_property_name = {
            let props: Vec<crate::mcp::shacl::ShaclProperty> =
                crate::mcp::shacl::load_class_properties(perspective, class_name).await;
            let prop_lower = property_name.to_lowercase();
            props
                .into_iter()
                .find(|p| p.name == property_name || p.name.to_lowercase() == prop_lower)
                .map(|p| p.name)
                .unwrap_or_else(|| property_name.to_string())
        };

        // Try to parse as JSON to recover native types (bool, number).
        // MCP tool schemas declare all values as strings, so "false" arrives
        // as the string "false" — but the language controller needs json!(false).
        let json_value: serde_json::Value = serde_json::from_str(value)
            .unwrap_or_else(|_| serde_json::Value::String(value.to_string()));

        match perspective
            .resolve_property_value(
                class_name,
                &original_property_name,
                &json_value,
                agent_context,
            )
            .await
        {
            Ok(url) => url,
            Err(e) => {
                log::warn!(
                    "Failed to resolve property value for {}.{}: {}, falling back to encode_literal",
                    class_name,
                    original_property_name,
                    e
                );
                Self::encode_literal(value)
            }
        }
    }
}
