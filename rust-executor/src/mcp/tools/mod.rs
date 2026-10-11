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
//! - `instances` — the static, class-agnostic surface for typed data:
//!   `describe_perspective`, the `instance_*` family (class name as a
//!   parameter), the raw `add_child` / `get_children` tree ops and
//!   `execute_commands`
//! - `flows` — flow state machine tools
//! - `auth` — authentication & JWT tools
//! - `profiles` — agent profile management
//! - `subscriptions` — waker query generation
//! - `neighbourhoods` / `languages` — P2P sharing and language metadata
//! - `dynamic` — auto-generated per-class SHACL tools (exposed over MCP only
//!   with `dynamicClassTools`; always fed to the in-process harness)

pub mod auth;
pub mod docs;
pub mod dynamic;
pub mod flows;
pub mod harness_bridge;
pub mod instances;
pub mod languages;
pub mod neighbourhoods;
pub mod perspectives;
pub mod profiles;
pub mod provider_impl;
pub(crate) mod side_effects;
pub mod subscriptions;

use super::caller::Opened;
use super::server::McpContext;
use crate::agent::capabilities::{
    perspective_query_capability, perspective_update_capability, Capability, ListenerReach,
};
use crate::agent::session::{AuthError, Session};
use crate::agent::AgentContext;
use crate::api::guards::{Admission, Guard};
use crate::api::perspectives_ws::ACCESS_DENIED;
use crate::api::ws_handler::{shared_handler_map, WsRpcError};
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

use serde_json::{json, Value};
use std::sync::Arc;

// HTTP request parts injected by rmcp's streamable-HTTP transport into each request's extensions.
use axum::http::request::Parts as HttpRequestParts;

/// What a refused caller is told: how to authenticate.
const AUTHENTICATE_HINT: &str = "Use request_capability + generate_jwt, login_email, or signup + \
     verify_email_code to authenticate.";

/// The refusal for a caller without a working session: anonymous (`None`), or a token that
/// opens none or stopped working (`Some`). Every refusal starts "Authentication required".
pub(crate) fn authentication_required(why: Option<&AuthError>) -> String {
    match why {
        None => format!("Authentication required. {AUTHENTICATE_HINT}"),
        Some(e) => format!("Authentication required: {e}. {AUTHENTICATE_HINT}"),
    }
}

/// What one HTTP request contributes to authentication: its `Authorization` credential and
/// the reach of the listener it arrived on. Calls that do not come over HTTP (the
/// interpretation harness, tests) contribute nothing.
#[derive(Default)]
pub(crate) struct RequestAuth {
    pub header: Option<String>,
    pub reach: Option<ListenerReach>,
}

impl RequestAuth {
    pub(crate) fn of(context: &RequestContext<RoleServer>) -> Self {
        let parts = context.extensions.get::<HttpRequestParts>();
        RequestAuth {
            header: parts.and_then(bearer_credential),
            reach: parts.map(crate::api::auth::listener_reach),
        }
    }
}

/// The credential carried by this request's `Authorization` header, if any.
///
/// A `Bearer <value>` header yields `<value>`; any other scheme yields the whole header
/// verbatim, because an admin credential may be sent raw. Returns `None` when the header
/// is absent — which callers must distinguish from an empty or unusable one.
fn bearer_credential(parts: &HttpRequestParts) -> Option<String> {
    parts
        .headers
        .get(axum::http::header::AUTHORIZATION)
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
        })
}

// ============================================================================
// MCP Handler
// ============================================================================

/// AD4M MCP Handler - exposes Subject operations as MCP tools
#[derive(Clone)]
pub struct Ad4mMcpHandler {
    pub(crate) context: McpContext,
    tool_router: ToolRouter<Self>,
}

/// What an MCP client sees in the `initialize` response. Kept short — it
/// points at `get_documentation`, which carries the real text.
pub(crate) const SERVER_INSTRUCTIONS: &str =
    "AD4M executor: an agent-centric P2P knowledge graph. \
If you are new to AD4M, call get_documentation(topic=\"overview\") first — it explains the \
tool surface (describe_perspective + the generic instance_* tools, which take a class_name \
parameter), the workflow and the rules for writing data that humans and other agents can \
use; topic=\"usage\" is the working guide (reading and writing instances, the child tree, \
common traps), topic=\"flux\" is app-specific and only for spaces the Flux app created, \
topic=\"models\" teaches authoring your own subject classes with add_model, and \
topic=\"architecture\" goes deeper into the data model. Typical flow: authenticate (or \
nothing, if the executor was started with the admin credential for you) -> list_perspectives \
or neighbourhood_join_from_url -> describe_perspective -> instance_query / instance_transcript \
to read, instance_create to write.";

/// Tool names that can be called without authentication.
/// These are the auth bootstrapping tools for multi-user mode, plus the
/// documentation (which is what tells a cold agent how to authenticate).
///
/// Being listed here only means `call_tool` does not refuse an anonymous caller or a
/// token that does not work. `request_capability` still withholds the auto-permit (and
/// the mint code) from any session that `agent.permitCapability`'s guard refuses — see
/// `handle_capability_request` in auth.rs (issue #851, sub-problem 3).
pub(crate) const AUTH_TOOLS: &[&str] = &[
    "get_documentation",
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
            instructions: Some(SERVER_INSTRUCTIONS.to_string()),
            ..Default::default()
        }
    }

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, ErrorData> {
        Ok(ListToolsResult {
            tools: self.exposed_tools().await,
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

        // Every call resolves its caller, so a header token opens (and the MCP session
        // remembers) a session before any tool runs, auth tools included: `auth_status`
        // then reports the session the other tools use. The auth tools run whatever the
        // outcome; every other tool needs a working, authenticated session.
        let caller = self.caller(RequestAuth::of(&context)).await;
        if !AUTH_TOOLS.contains(&tool_name.as_str()) {
            if let Err(refusal) = authenticated(caller) {
                return Ok(CallToolResult::error(vec![Content::text(
                    json!({ "error": refusal }).to_string(),
                )]));
            }
        }

        self.dispatch_tool(request, context).await
    }
}

/// `opened` if it is a working session that is not anonymous, else the refusal.
fn authenticated(opened: Opened) -> Result<Arc<Session>, String> {
    match opened {
        Ok(session) if session.is_authenticated() => Ok(session),
        Ok(_) => Err(authentication_required(None)),
        Err(e) => Err(authentication_required(Some(&e))),
    }
}

/// The RPC request context of `session`, as the RPC socket builds it: the session and its
/// capabilities. MCP has no auto-permit setting and passes no raw token.
pub(crate) fn request_context(session: Arc<Session>) -> crate::types::RequestContext {
    crate::types::RequestContext {
        capabilities: Ok(session.capabilities()),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        cancel_token: None,
        session,
    }
}

/// Call the RPC method `method` with `params` as `session`, through the shared handler map.
pub(crate) async fn call_method_as(
    session: Arc<Session>,
    method: &str,
    params: Value,
) -> Result<Value, WsRpcError> {
    shared_handler_map()
        .dispatch(method, params, Arc::new(request_context(session)))
        .await
}

/// A tool's answer when its RPC method refuses or fails: the method's message and status
/// code, `{"error": .., "status": 401 | 403 | 404 | ..}`.
pub(crate) fn method_error(e: WsRpcError) -> String {
    json!({ "error": e.message, "status": e.code }).to_string()
}

/// [`method_error`] for a method on perspective `id`. A missing perspective and one that is not
/// the caller's both answer "Perspective not found" (404), so a refusal does not reveal that a
/// perspective exists.
pub(crate) fn perspective_error(id: &str, e: WsRpcError) -> String {
    if e.code == 404 || (e.code == 403 && e.message == ACCESS_DENIED) {
        method_error(WsRpcError::not_found(format!(
            "Perspective not found: {id}"
        )))
    } else {
        method_error(e)
    }
}

/// Decode an RPC method's result into the type its contract names.
pub(crate) fn decode<T: serde::de::DeserializeOwned>(value: Value) -> Result<T, String> {
    serde_json::from_value(value).map_err(|e| json!({ "error": e.to_string() }).to_string())
}

impl Ad4mMcpHandler {
    pub fn new(context: McpContext) -> Self {
        Self {
            context,
            tool_router: Self::tool_router(),
        }
    }

    /// The caller of one call: the MCP session's [`Session`], opened from the request's
    /// header token or a login tool's token and checked again now. See
    /// [`crate::mcp::caller::Caller::resolve`].
    pub(crate) async fn caller(&self, request: RequestAuth) -> Opened {
        self.context
            .caller
            .resolve(
                request.header.as_deref(),
                &self.context.admin_credential,
                request.reach.unwrap_or(self.context.reach),
            )
            .await
    }

    /// The caller of a tool call, which must hold a working session that is not anonymous.
    /// `call_tool` resolved it with the request's header already, so this reads what the
    /// MCP session remembers, checked again. The in-process harness reaches tools without
    /// `call_tool`, so the check here is what refuses its dead or anonymous callers.
    pub(crate) async fn session(&self) -> Result<Arc<Session>, String> {
        authenticated(self.caller(RequestAuth::default()).await)
    }

    /// Call the RPC method `method` with `params` as this tool call's caller, through the
    /// handler map: the method's guard decides who may call it, exactly as on the RPC socket
    /// (#851). A caller without a working, authenticated session gets 401.
    pub(crate) async fn call_method(
        &self,
        method: &str,
        params: Value,
    ) -> Result<Value, WsRpcError> {
        let session = self.session().await.map_err(WsRpcError::unauthorized)?;
        call_method_as(session, method, params).await
    }

    /// The steps of [`Self::call_method`] before the handler (404, 400, the guard), for a tool
    /// that must stop a refused caller before work of its own that precedes the call.
    pub(crate) async fn admit_method(
        &self,
        method: &str,
        params: &Value,
    ) -> Result<(), WsRpcError> {
        let session = self.session().await.map_err(WsRpcError::unauthorized)?;
        match shared_handler_map()
            .admit(method, params, &request_context(session))
            .await?
        {
            Admission::Granted => Ok(()),
            // A method that hides existence answers `null`: as missing to a tool.
            Admission::Hidden => Err(WsRpcError::not_found(format!("{method}: not found"))),
        }
    }

    /// Build the tool router manually, registering all tools from domain modules.
    fn tool_router() -> ToolRouter<Self> {
        ToolRouter::<Self>::new()
            // perspectives.rs
            .with_route((Self::list_perspectives_tool_attr(), Self::list_perspectives))
            .with_route((Self::add_perspective_tool_attr(), Self::add_perspective))
            .with_route((Self::add_link_tool_attr(), Self::add_link))
            .with_route((Self::query_links_tool_attr(), Self::query_links))
            .with_route((Self::add_model_tool_attr(), Self::add_model))
            // instances/
            .with_route((
                Self::describe_perspective_tool_attr(),
                Self::describe_perspective,
            ))
            .with_route((Self::instance_create_tool_attr(), Self::instance_create))
            .with_route((Self::instance_query_tool_attr(), Self::instance_query))
            .with_route((Self::instance_get_tool_attr(), Self::instance_get))
            .with_route((Self::instance_update_tool_attr(), Self::instance_update))
            .with_route((
                Self::instance_add_to_collection_tool_attr(),
                Self::instance_add_to_collection,
            ))
            .with_route((
                Self::instance_remove_from_collection_tool_attr(),
                Self::instance_remove_from_collection,
            ))
            .with_route((Self::instance_remove_tool_attr(), Self::instance_remove))
            .with_route((
                Self::instance_transcript_tool_attr(),
                Self::instance_transcript,
            ))
            .with_route((Self::add_child_tool_attr(), Self::add_child))
            .with_route((Self::get_children_tool_attr(), Self::get_children))
            .with_route((Self::execute_commands_tool_attr(), Self::execute_commands))
            // docs.rs
            .with_route((Self::get_documentation_tool_attr(), Self::get_documentation))
            // flows.rs
            .with_route((Self::add_flow_tool_attr(), Self::add_flow))
            .with_route((Self::get_flows_tool_attr(), Self::get_flows))
            .with_route((Self::flow_state_tool_attr(), Self::flow_state))
            .with_route((Self::flow_actions_tool_attr(), Self::flow_actions))
            .with_route((
                Self::flow_proposal_accept_tool_attr(),
                Self::flow_proposal_accept,
            ))
            .with_route((
                Self::flow_proposal_reject_tool_attr(),
                Self::flow_proposal_reject,
            ))
            .with_route((
                Self::flow_valid_outputs_tool_attr(),
                Self::flow_valid_outputs,
            ))
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
            .with_route((Self::get_my_did_tool_attr(), Self::get_my_did))
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
            self.dispatch_non_router_tool(&tool_name, request.arguments)
                .await
        }
    }

    /// The tool list this MCP transport advertises: every static `#[tool]`
    /// on the router, plus the dynamic per-class SHACL tools **only when**
    /// `dynamicClassTools` is enabled. With the flag off (the default) the
    /// list is constant regardless of which social DNA is loaded — that is
    /// what lets plugin manifests and agent configs declare the AD4M tools
    /// statically. The generic `instance_*` tools cover per-class operations
    /// in that mode.
    ///
    /// Note this is deliberately NOT what the in-process harness sees:
    /// `harness_bridge::list_tool_schemas` always merges the dynamic tools.
    pub(crate) async fn exposed_tools(&self) -> Vec<rmcp::model::Tool> {
        let mut tools = self.tool_router.list_all();
        if self.context.dynamic_class_tools {
            tools.extend(self.generate_dynamic_tools().await);
        }
        tools
    }

    /// Handle a call to a tool that is not on the static router. That is
    /// either a dynamic per-class SHACL tool (when exposed) or an unknown
    /// name. When dynamic tools are hidden, calling one by name is refused
    /// with a pointer to the static equivalent — hidden tools must not be
    /// silently callable, or the "stable surface" guarantee would only hold
    /// for `tools/list` and not for `tools/call`.
    pub(crate) async fn dispatch_non_router_tool(
        &self,
        tool_name: &str,
        arguments: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<CallToolResult, ErrorData> {
        if self.context.dynamic_class_tools {
            return self.handle_dynamic_tool(tool_name, arguments).await;
        }
        Ok(CallToolResult::error(vec![Content::text(
            json!({
                "error": format!(
                    "Unknown tool: {}. Dynamic per-class tools are not exposed on this MCP \
                     server (config `dynamicClassTools` is false). Use the generic instance \
                     tools instead: describe_perspective, instance_create, instance_query, \
                     instance_get, instance_update, instance_add_to_collection, \
                     instance_remove_from_collection, instance_remove, instance_transcript \
                     — pass the class name as `class_name`; add_child / get_children for \
                     the raw ad4m://has_child tree. To re-enable per-class tools start the \
                     executor with `--dynamic-class-tools true`.",
                    tool_name
                ),
            })
            .to_string(),
        )]))
    }

    // ========================================================================
    // Shared Helpers
    // ========================================================================

    /// The agent the caller acts as: a user as that user, the operator and apps as the
    /// node's main agent ([`Session::acts_as`]).
    pub(crate) async fn get_agent_context(&self) -> Result<AgentContext, String> {
        Ok(self.session().await?.acts_as())
    }

    /// Open the MCP session's session from a login tool's token and return a success JSON
    /// response.
    pub(crate) async fn store_token_and_respond(
        &self,
        token: String,
        email: Option<&str>,
        message: &str,
    ) -> String {
        self.context
            .caller
            .sign_in(&token, &self.context.admin_credential, self.context.reach)
            .await;

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

    /// Whether the caller may read perspective `uuid`: the guard of the RPC methods that
    /// read one. The dynamic tool list offers the classes of these perspectives only.
    pub(crate) async fn may_read_perspective(&self, uuid: &str) -> bool {
        self.admitted_perspective(uuid, perspective_query_capability)
            .await
            .is_ok()
    }

    /// Perspective `uuid` and the caller's session, if the caller passes
    /// `Guard::perspective(capability)`, the guard of the RPC methods that read
    /// (`perspective_query_capability`) or change (`perspective_update_capability`) a
    /// perspective. The operator reaches every perspective, a user only its own, an app the
    /// unowned ones and the main agent's; every caller but the operator needs the capability.
    /// A refusal answers as [`perspective_error`]; a caller without a working, authenticated
    /// session gets 401 (the in-process harness does not go through `call_tool`).
    async fn admitted_perspective(
        &self,
        uuid: &str,
        capability: fn(Vec<String>) -> Capability,
    ) -> Result<(PerspectiveInstance, Arc<Session>), String> {
        let session = self
            .session()
            .await
            .map_err(|e| method_error(WsRpcError::unauthorized(e)))?;
        Guard::perspective(capability)
            .admit(
                "this tool",
                &json!({ "uuid": uuid }),
                &request_context(session.clone()),
            )
            .await
            .map_err(|e| perspective_error(uuid, e))?;
        // Removed between the guard and here: missing, as the guard would answer.
        let perspective = get_perspective(uuid)
            .ok_or_else(|| perspective_error(uuid, WsRpcError::not_found(String::new())))?;
        Ok((perspective, session))
    }

    /// Perspective `perspective_id`, if the caller may read it: the guard of the perspective
    /// RPC methods that read, `Guard::perspective(perspective_query_capability)`.
    pub(crate) async fn get_readable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<PerspectiveInstance, String> {
        let (perspective, _) = self
            .admitted_perspective(perspective_id, perspective_query_capability)
            .await?;
        Ok(perspective)
    }

    /// Perspective `perspective_id` and the agent the caller writes as, if the caller may
    /// change it: the guard of the perspective RPC methods that write,
    /// `Guard::perspective(perspective_update_capability)`.
    pub(crate) async fn get_writable_perspective(
        &self,
        perspective_id: &str,
    ) -> Result<(PerspectiveInstance, AgentContext), String> {
        let (perspective, session) = self
            .admitted_perspective(perspective_id, perspective_update_capability)
            .await?;
        Ok((perspective, session.acts_as()))
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

    /// Encode a string value as a `literal:string:` URL.
    /// Uses rust-client's `Literal` for proper URL encoding.
    pub(crate) fn encode_literal(value: &str) -> String {
        use ad4m_client::literal::Literal;
        Literal::from_string(value.to_string())
            .to_url()
            .unwrap_or_else(|_| format!("literal:string:{}", value))
    }

    /// Wrap a bare string as a literal URI; pass an already-URI value through.
    ///
    /// The canonical literal spelling is single-colon (`literal:string:…`), so
    /// a `contains("://")` check does NOT recognise it and re-wraps it into
    /// `literal:string:literal%3Astring%3A…` — a node nothing else in the
    /// graph points at. That double-wrap made `subscribe_to_children` watch a
    /// parent that never gets children (found live 2026-09-15). Delegates to
    /// [`instances::link_target`], the one place that owns the URI-or-bare
    /// decision (including legacy `literal://` normalisation).
    pub(crate) fn wrap_bare_as_literal(value: &str) -> String {
        instances::link_target(value)
    }

    /// Get the SHACL name literal for a class, trying both encoded and raw formats.
    /// Flux's TypeScript Literal doesn't URL-encode inner URIs, producing
    /// "literal:string:shacl://Class", while Rust's Literal produces
    /// "literal:string:shacl%3A%2F%2FClass". Returns (encoded, raw).
    pub(crate) fn shacl_name_variants(class_name: &str) -> (String, String) {
        let encoded = Self::encode_literal(&format!("shacl://{}", class_name));
        let raw = format!("literal:string:shacl://{}", class_name);
        (encoded, raw)
    }

    /// Query SHACL shape links for a class, trying encoded then raw name format.
    pub(crate) async fn get_shacl_shape_links(
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        class_name: &str,
    ) -> Vec<crate::types::DecoratedLinkExpression> {
        let (encoded, raw) = Self::shacl_name_variants(class_name);
        let links = perspective
            .get_links(&crate::types::LinkQuery {
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
            .get_links(&crate::types::LinkQuery {
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

    /// Resolve a property value through the appropriate storage path, respecting
    /// the SHACL `resolveLanguage` setting for the property.
    ///
    /// If the value already has a URI scheme (e.g. `literal:...`, `did:...`),
    /// it is returned as-is. Otherwise, the value is parsed as JSON to recover
    /// its native type (boolean, number, etc.) and resolved through the
    /// perspective's `resolve_property_value` method.
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
        // Short-circuit only when the value is a well-formed absolute IRI —
        // the same predicate the SPARQL query builder uses to gate its
        // `UNION { ?s <pred> <val> }` IRI arm. Sharing the check keeps write
        // and read agreeing on what "already a URI" means; otherwise values
        // like `"Note: buy milk"` would slip through and become invalid
        // `NamedNode::new_unchecked` targets that no query can match.
        if crate::perspectives::model_query::is_safe_iri_target(value) {
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

/// What the tool tests that call as one kind of caller share.
#[cfg(test)]
pub(crate) mod test_support {
    use super::*;
    use crate::agent::session::{Principal, TokenId};

    /// A handler whose MCP session holds a session for `principal`, on a network listener of
    /// a node with an admin credential.
    pub(crate) fn handler_as(principal: Principal) -> Ad4mMcpHandler {
        let mut context = McpContext::new(Some("test-admin".into()), ListenerReach::Network, false);
        context.caller = crate::mcp::caller::Caller::holding(Ok(Arc::new(Session::for_test(
            principal,
            ListenerReach::Network,
        ))));
        Ad4mMcpHandler::new(context)
    }

    pub(crate) fn app(caps: Vec<Capability>) -> Principal {
        Principal::App {
            token_id: TokenId("mcp-test-app".into()),
            caps,
        }
    }

    pub(crate) fn user(email: &str, did: &str, caps: Vec<Capability>) -> Principal {
        Principal::User {
            token_id: TokenId(format!("mcp-test-{email}")),
            email: email.into(),
            did: did.into(),
            caps,
        }
    }

    /// The wallet, the main agent and the database the tools need.
    pub(crate) fn setup() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_db();
    }

    /// A new perspective owned by `owners` (`None`: unowned). Returns its uuid.
    pub(crate) async fn perspective(owners: Option<Vec<String>>) -> String {
        let handle = crate::types::PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            owners,
            ..Default::default()
        };
        let uuid = handle.uuid.clone();
        crate::perspectives::add_perspective(handle, None)
            .await
            .expect("a test perspective");
        uuid
    }

    /// Starts the runtime service on an empty bootstrap seed, which the link language template
    /// reads need, and stops it when dropped. Without it they panic holding its lock.
    pub(crate) struct RuntimeService;

    impl RuntimeService {
        pub(crate) fn start() -> Self {
            use crate::runtime_service::RuntimeService as Service;
            let dir = tempfile::tempdir().expect("a temp dir");
            let seed = dir.path().join("seed.json");
            let empty = json!({
                "trustedAgents": [], "knownLinkLanguages": [], "directMessageLanguage": "",
                "agentLanguage": "", "perspectiveLanguage": "", "neighbourhoodLanguage": "",
                "languageLanguageBundle": "",
            });
            std::fs::write(&seed, empty.to_string()).expect("a seed file");
            Service::init_global_instance(seed.to_string_lossy().into_owned());
            RuntimeService
        }
    }

    impl Drop for RuntimeService {
        fn drop(&mut self) {
            let instance = crate::runtime_service::RuntimeService::global_instance();
            *instance.lock().unwrap_or_else(|p| p.into_inner()) = None;
        }
    }

    /// The `status` of a tool's answer to a refused or failed RPC method ([`method_error`]),
    /// `None` for any other answer.
    pub(crate) fn status(answer: &str) -> Option<u64> {
        serde_json::from_str::<Value>(answer)
            .ok()?
            .get("status")?
            .as_u64()
    }
}

#[cfg(test)]
mod agent_context_tests {
    use super::*;
    use crate::agent::capabilities::PERSPECTIVE_CREATE_CAPABILITY;
    use crate::test_utils::{
        ensure_db, expired_user_token, setup_agent, setup_wallet, MultiUserMode,
    };

    fn handler(session_token: &str) -> Ad4mMcpHandler {
        Ad4mMcpHandler::new(
            McpContext::new(Some("test-admin".into()), ListenerReach::Network, false)
                .with_token(session_token),
        )
    }

    // MCP tools used to act as the node's main agent once a session token stopped decoding.
    #[tokio::test]
    async fn an_expired_session_token_acts_as_nobody() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let token = expired_user_token("expired@example.org");
        let err = handler(&token).get_agent_context().await.unwrap_err();
        assert!(err.contains("Unauthorized"), "{err}");
    }

    #[tokio::test]
    async fn the_session_admin_credential_acts_as_the_main_agent() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let context = handler("test-admin").get_agent_context().await.unwrap();
        assert!(context.is_main_agent);
    }

    // Profile tools answered with the node's DID once a session token stopped decoding.
    #[tokio::test]
    async fn a_profile_tool_with_an_expired_token_does_not_answer_as_the_node() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let token = expired_user_token("expired.profile@example.org");
        let answer = handler(&token)
            .get_my_did(rmcp::handler::server::wrapper::Parameters(
                super::profiles::GetAgentProfileParams {},
            ))
            .await;
        assert_ne!(answer, crate::agent::did());
        assert!(answer.contains("Unauthorized"), "{answer}");
    }

    #[tokio::test]
    async fn a_user_session_token_acts_as_its_user() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let email = "mcp.user@example.org";
        crate::agent::AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let handler = handler(&token);
        let context = handler.get_agent_context().await.unwrap();
        assert_eq!(context.user_email.as_deref(), Some(email));
        // A user never acts as the node: its own DID, not the main agent's.
        let did = handler.get_my_did(profile_params()).await;
        assert_eq!(
            did,
            crate::agent::AgentService::get_user_did_by_email(email).unwrap()
        );
        assert_ne!(did, crate::agent::did());
    }

    fn profile_params(
    ) -> rmcp::handler::server::wrapper::Parameters<super::profiles::GetAgentProfileParams> {
        rmcp::handler::server::wrapper::Parameters(super::profiles::GetAgentProfileParams {})
    }

    fn app_token(capabilities: Vec<Capability>) -> String {
        setup_wallet();
        setup_agent();
        crate::agent::capabilities::token::generate_jwt(
            "mcp session test".to_string(),
            3600,
            crate::agent::capabilities::AuthInfo {
                capabilities: Some(capabilities),
                ..Default::default()
            },
        )
        .unwrap()
    }

    // #851 2: the admin credential authenticates as the operator, as on RPC.
    #[tokio::test]
    async fn the_admin_credential_is_the_operator_with_every_capability() {
        setup_wallet();
        setup_agent();
        let session = handler("test-admin").session().await.unwrap();
        assert!(session.is_operator());
        assert!(session.may(&crate::agent::capabilities::ALL_CAPABILITY));
        assert_eq!(
            handler("test-admin").get_my_did(profile_params()).await,
            crate::agent::did()
        );
    }

    #[tokio::test]
    async fn an_app_token_is_limited_to_its_capabilities_and_acts_as_the_node() {
        let token = app_token(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]);
        let handler = handler(&token);
        let session = handler.session().await.unwrap();
        assert!(session.may(&PERSPECTIVE_CREATE_CAPABILITY));
        assert!(!session.may(&crate::agent::capabilities::LANGUAGE_READ_CAPABILITY));
        assert!(handler.get_agent_context().await.unwrap().is_main_agent);
        assert!(!gets_mint_code(&handler).await);
    }

    /// Whether `request_capability` hands `handler`'s caller the inline mint code.
    async fn gets_mint_code(handler: &Ad4mMcpHandler) -> bool {
        let answer = handler
            .request_capability_as(
                RequestAuth::default(),
                super::auth::RequestCapabilityParams {
                    app_name: "mcp session test".into(),
                    app_desc: "d".into(),
                    app_domain: None,
                    app_url: None,
                },
            )
            .await;
        serde_json::from_str::<Value>(&answer).expect(&answer)["code"].is_string()
    }

    #[tokio::test]
    async fn a_token_revoked_mid_session_is_refused_by_every_tool() {
        let token = app_token(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]);
        let handler = handler(&token);
        let session = handler.session().await.unwrap();
        let crate::agent::session::Principal::App { token_id, .. } = &session.principal else {
            panic!("expected an app session");
        };
        crate::agent::revoked::RevokedSet::global().revoke(token_id.clone(), u64::MAX);

        let err = handler.session().await.unwrap_err();
        assert!(err.starts_with("Authentication required"), "{err}");
        let did = handler.get_my_did(profile_params()).await;
        assert!(did.contains("Unauthorized access"), "{did}");
        let listed = handler
            .list_perspectives(rmcp::handler::server::wrapper::Parameters(
                super::perspectives::ListPerspectivesParams {},
            ))
            .await;
        assert!(listed.contains("Authentication required"), "{listed}");
    }

    // A user who logs out (their session cutoff moves past the token) loses the MCP session.
    #[tokio::test]
    async fn a_user_session_ends_at_the_users_cutoff() {
        setup_wallet();
        setup_agent();
        let email = "mcp.cutoff@example.org";
        crate::agent::AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let handler = handler(&token);
        assert!(handler.session().await.is_ok());

        crate::agent::revoked::SessionCutoffs::global().set(email, u64::MAX);
        let err = handler.get_agent_context().await.unwrap_err();
        assert!(err.contains("Unauthorized access"), "{err}");
    }

    #[tokio::test]
    async fn without_a_credential_no_token_is_the_operator_only_on_loopback() {
        ensure_db();
        let local = Ad4mMcpHandler::new(McpContext::new(None, ListenerReach::Loopback, false));
        assert!(local.session().await.unwrap().is_operator());
        assert!(gets_mint_code(&local).await);

        // #1059: an MCP_HOST bind beyond loopback makes no-token callers anonymous.
        let remote = Ad4mMcpHandler::new(McpContext::new(None, ListenerReach::Network, false));
        let err = remote.session().await.unwrap_err();
        assert!(err.starts_with("Authentication required."), "{err}");
        assert!(!gets_mint_code(&remote).await);
    }

    // A proxied request reads as a network one, as on the API: no operator through a proxy.
    #[tokio::test]
    async fn a_forwarded_request_without_a_token_is_anonymous() {
        ensure_db();
        let handler = Ad4mMcpHandler::new(McpContext::new(None, ListenerReach::Loopback, false));
        let request = axum::http::Request::builder()
            .header("x-forwarded-for", "203.0.113.9")
            .extension(ListenerReach::Loopback)
            .body(())
            .unwrap();
        let (parts, _) = request.into_parts();
        let auth = RequestAuth {
            header: bearer_credential(&parts),
            reach: Some(crate::api::auth::listener_reach(&parts)),
        };
        assert!(!handler.caller(auth).await.unwrap().is_authenticated());
    }

    #[test]
    fn the_bearer_scheme_is_stripped_and_a_raw_credential_kept() {
        let parts = |value: &str| {
            let request = axum::http::Request::builder()
                .header("authorization", value)
                .body(())
                .unwrap();
            request.into_parts().0
        };
        assert_eq!(
            bearer_credential(&parts("Bearer abc")).as_deref(),
            Some("abc")
        );
        assert_eq!(
            bearer_credential(&parts("bearer abc")).as_deref(),
            Some("abc")
        );
        assert_eq!(bearer_credential(&parts("abc")).as_deref(), Some("abc"));
        let none = axum::http::Request::builder()
            .body(())
            .unwrap()
            .into_parts()
            .0;
        assert_eq!(bearer_credential(&none), None);
    }
}
