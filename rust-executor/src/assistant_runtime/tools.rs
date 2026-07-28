//! Tool providers for the assistant loop.
//!
//! Tool execution sits behind a small provider abstraction so the model's
//! granted tools can come from more than one source. Two providers ship today:
//!
//! * [`BuiltinTools`] — in-process perspective / neighbourhood graph
//!   operations, fully implemented (zero HTTP; direct
//!   `PerspectiveInstance`/`neighbourhoods` calls).
//! * [`McpToolProvider`] — external MCP servers configured on an assistant,
//!   connected via a live `rmcp` client. Each `McpServer` is connected on run
//!   start (stdio → child process; http/streamable/sse → streamable-HTTP;
//!   websocket unsupported by the pinned rmcp), its tools discovered via
//!   `list_tools` and mapped to OpenAI `ToolDef`s, and calls dispatched via
//!   `call_tool`. A server that fails to connect or enumerate is logged and
//!   skipped, so the model is never offered an unfulfillable tool and one bad
//!   server never fails the whole set.
//!
//! Dispatch is a plain enum (no `async-trait` dependency); [`ToolSet`]
//! aggregates providers and routes a call to whichever owns the tool name.

use anyhow::{anyhow, Result};
use serde_json::{json, Value};

use rmcp::model::{CallToolRequestParams, Tool};
use rmcp::service::RunningService;
use rmcp::transport::streamable_http_client::StreamableHttpClientTransportConfig;
use rmcp::transport::{ConfigureCommandExt, StreamableHttpClientTransport, TokioChildProcess};
use rmcp::{RoleClient, ServiceExt};
use tokio::process::Command;

use crate::agent::AgentContext;
use crate::api::openai_compat::types::{FunctionDef, ToolDef};
use crate::perspectives::get_perspective;
use crate::types::{Link, LinkQuery, LinkStatus, Perspective};

use super::entities::{decode_literal, McpServer};

/// One source of tools.
pub enum ToolProvider {
    Builtin(BuiltinTools),
    Mcp(McpToolProvider),
}

impl ToolProvider {
    pub fn tools(&self) -> Vec<ToolDef> {
        match self {
            ToolProvider::Builtin(b) => b.tools(),
            ToolProvider::Mcp(m) => m.tools(),
        }
    }

    pub fn owns(&self, name: &str) -> bool {
        self.tools().iter().any(|t| t.function.name == name)
    }

    pub async fn execute(&self, name: &str, arguments: &str) -> Result<String> {
        match self {
            ToolProvider::Builtin(b) => b.execute(name, arguments).await,
            ToolProvider::Mcp(m) => m.execute(name, arguments).await,
        }
    }
}

/// The set of tools granted to one assistant, across providers.
pub struct ToolSet {
    providers: Vec<ToolProvider>,
}

impl ToolSet {
    pub fn new(providers: Vec<ToolProvider>) -> Self {
        Self { providers }
    }

    /// The OpenAI-shaped tool definitions to render into the system prompt.
    pub fn tool_defs(&self) -> Vec<ToolDef> {
        self.providers.iter().flat_map(|p| p.tools()).collect()
    }

    pub fn is_empty(&self) -> bool {
        self.providers.iter().all(|p| p.tools().is_empty())
    }

    /// Route a tool call to the provider that owns the name.
    pub async fn execute(&self, name: &str, arguments: &str) -> Result<String> {
        for p in &self.providers {
            if p.owns(name) {
                return p.execute(name, arguments).await;
            }
        }
        Err(anyhow!("Unknown tool: {}", name))
    }
}

// ---------------------------------------------------------------------------
// Built-in perspective / neighbourhood tools
// ---------------------------------------------------------------------------

/// In-process graph tools scoped to the run's conversation perspective (with
/// an optional `perspective_uuid` override on each call).
pub struct BuiltinTools {
    pub perspective_uuid: String,
}

impl BuiltinTools {
    pub fn new(perspective_uuid: String) -> Self {
        Self { perspective_uuid }
    }

    fn tools(&self) -> Vec<ToolDef> {
        vec![
            def(
                "perspective_add_link",
                "Add an RDF-like link (source, predicate, target) to the active perspective's knowledge graph. Use literal:string:<value> targets for scalar values.",
                json!({
                    "type": "object",
                    "properties": {
                        "source": {"type": "string", "description": "Subject URI"},
                        "predicate": {"type": "string", "description": "Predicate URI"},
                        "target": {"type": "string", "description": "Target URI or literal"},
                        "perspective_uuid": {"type": "string", "description": "Optional perspective override; defaults to the active conversation perspective"}
                    },
                    "required": ["source", "predicate", "target"]
                }),
            ),
            def(
                "perspective_query_links",
                "Query links in the active perspective, filtering by any of source/predicate/target. Returns matching triples with decoded literal targets.",
                json!({
                    "type": "object",
                    "properties": {
                        "source": {"type": "string"},
                        "predicate": {"type": "string"},
                        "target": {"type": "string"},
                        "perspective_uuid": {"type": "string"}
                    }
                }),
            ),
            def(
                "perspective_get_subject",
                "Read all properties (outgoing links) of a subject/base URI in the active perspective, returned as a predicate→value map.",
                json!({
                    "type": "object",
                    "properties": {
                        "base": {"type": "string", "description": "The subject/base URI to read"},
                        "perspective_uuid": {"type": "string"}
                    },
                    "required": ["base"]
                }),
            ),
            def(
                "neighbourhood_publish",
                "Publish a perspective as a shared neighbourhood using a link-language template address. Returns the neighbourhood URL that others can join.",
                json!({
                    "type": "object",
                    "properties": {
                        "perspective_uuid": {"type": "string"},
                        "link_language": {"type": "string", "description": "Link-language template or cloned language address"},
                        "name": {"type": "string", "description": "Optional neighbourhood name"}
                    },
                    "required": ["link_language"]
                }),
            ),
            def(
                "neighbourhood_join",
                "Join a neighbourhood from its URL (neighbourhood://...), creating a local synced perspective. Returns the new perspective uuid.",
                json!({
                    "type": "object",
                    "properties": {
                        "url": {"type": "string"}
                    },
                    "required": ["url"]
                }),
            ),
        ]
    }

    fn perspective_arg(&self, v: &Value) -> String {
        v.get("perspective_uuid")
            .and_then(Value::as_str)
            .map(|s| s.to_string())
            .unwrap_or_else(|| self.perspective_uuid.clone())
    }

    async fn execute(&self, name: &str, arguments: &str) -> Result<String> {
        let v: Value = serde_json::from_str(arguments).unwrap_or_else(|_| json!({}));
        let ctx = AgentContext::main_agent();

        match name {
            "perspective_add_link" => {
                let uuid = self.perspective_arg(&v);
                let mut p =
                    get_perspective(&uuid).ok_or_else(|| anyhow!("Perspective not found: {uuid}"))?;
                let link = Link {
                    source: req_str(&v, "source")?,
                    predicate: Some(req_str(&v, "predicate")?),
                    target: req_str(&v, "target")?,
                };
                let d = p.add_link(link, LinkStatus::Shared, None, &ctx).await?;
                Ok(json!({
                    "success": true,
                    "link": {
                        "source": d.data.source,
                        "predicate": d.data.predicate,
                        "target": d.data.target,
                        "timestamp": d.timestamp,
                    }
                })
                .to_string())
            }
            "perspective_query_links" => {
                let uuid = self.perspective_arg(&v);
                let p =
                    get_perspective(&uuid).ok_or_else(|| anyhow!("Perspective not found: {uuid}"))?;
                let query = LinkQuery {
                    source: v.get("source").and_then(Value::as_str).map(str::to_string),
                    predicate: v.get("predicate").and_then(Value::as_str).map(str::to_string),
                    target: v.get("target").and_then(Value::as_str).map(str::to_string),
                    ..Default::default()
                };
                let links = p.get_links(&query).await.unwrap_or_default();
                let rows: Vec<Value> = links
                    .into_iter()
                    .map(|l| {
                        json!({
                            "source": l.data.source,
                            "predicate": l.data.predicate,
                            "target": l.data.target,
                            "value": decode_literal(&l.data.target),
                        })
                    })
                    .collect();
                Ok(json!({ "count": rows.len(), "links": rows }).to_string())
            }
            "perspective_get_subject" => {
                let uuid = self.perspective_arg(&v);
                let base = req_str(&v, "base")?;
                let p =
                    get_perspective(&uuid).ok_or_else(|| anyhow!("Perspective not found: {uuid}"))?;
                let props = super::store::load_props(&p, &base).await;
                Ok(json!({ "base": base, "properties": props }).to_string())
            }
            "neighbourhood_publish" => {
                let uuid = self.perspective_arg(&v);
                let link_language = req_str(&v, "link_language")?;
                let meta = Perspective { links: Vec::new() };
                let url = crate::neighbourhoods::neighbourhood_publish_from_perspective_with_context(
                    &uuid,
                    link_language,
                    meta,
                    &ctx,
                )
                .await?;
                Ok(json!({ "success": true, "neighbourhood_url": url }).to_string())
            }
            "neighbourhood_join" => {
                let url = req_str(&v, "url")?;
                let handle =
                    crate::neighbourhoods::install_neighbourhood_with_context(url, &ctx).await?;
                Ok(json!({ "success": true, "perspective_uuid": handle.uuid }).to_string())
            }
            other => Err(anyhow!("Unknown built-in tool: {other}")),
        }
    }
}

// ---------------------------------------------------------------------------
// MCP tool provider — live rmcp client
// ---------------------------------------------------------------------------

/// One connected external MCP server: the live rmcp client session plus the
/// tool definitions discovered from it.
struct McpConnection {
    server_name: String,
    client: RunningService<RoleClient, ()>,
    tools: Vec<ToolDef>,
}

/// Tools from an assistant's configured external MCP servers, connected via
/// rmcp. Servers are connected once when the provider is built (and the
/// sessions are cached for the provider's — i.e. the run's — lifetime). A
/// server that fails to connect or enumerate tools is logged and skipped, so
/// the model is never offered a tool the executor cannot fulfil, and one bad
/// server never fails the whole tool set.
///
/// Transports (rmcp 0.15): `stdio` → [`TokioChildProcess`];
/// `http`/`streamable` → [`StreamableHttpClientTransport`]; `sse` → the same
/// streamable-HTTP client (0.15 folds SSE into `client-side-sse` — there is no
/// standalone SSE client transport); `websocket` is unsupported (this rmcp has
/// no ws client transport) and is skipped with a clear warning.
pub struct McpToolProvider {
    connections: Vec<McpConnection>,
}

impl McpToolProvider {
    /// Connect to every configured server and discover its tools. Failures are
    /// logged and skipped; never panics, never fails the whole set.
    pub async fn connect(servers: Vec<McpServer>) -> Self {
        let mut connections = Vec::new();
        for server in servers {
            match connect_server(&server).await {
                Ok(conn) => {
                    log::info!(
                        "assistant_runtime: connected MCP server '{}' ({} tools)",
                        conn.server_name,
                        conn.tools.len()
                    );
                    connections.push(conn);
                }
                Err(e) => log::warn!(
                    "assistant_runtime: MCP server '{}' (transport '{}') unavailable, skipping: {}",
                    server.name,
                    server.transport,
                    e
                ),
            }
        }
        Self { connections }
    }

    /// A provider with no connections (fallback / tests).
    pub fn empty() -> Self {
        Self {
            connections: Vec::new(),
        }
    }

    fn tools(&self) -> Vec<ToolDef> {
        self.connections
            .iter()
            .flat_map(|c| c.tools.clone())
            .collect()
    }

    async fn execute(&self, name: &str, arguments: &str) -> Result<String> {
        for conn in &self.connections {
            if conn.tools.iter().any(|t| t.function.name == name) {
                return call_mcp_tool(&conn.client, name, arguments).await;
            }
        }
        Err(anyhow!("MCP tool '{name}' not found on any connected server"))
    }
}

/// Connect one server (dispatching on its transport) and enumerate its tools.
async fn connect_server(server: &McpServer) -> Result<McpConnection> {
    let client: RunningService<RoleClient, ()> = match server.transport.as_str() {
        "stdio" => {
            let mut parts = server.command.split_whitespace();
            let program = parts
                .next()
                .ok_or_else(|| anyhow!("stdio server '{}' has an empty command", server.name))?
                .to_string();
            let args: Vec<String> = parts.map(str::to_string).collect();
            let transport = TokioChildProcess::new(Command::new(&program).configure(|cmd| {
                for arg in &args {
                    cmd.arg(arg);
                }
            }))
            .map_err(|e| anyhow!("failed to spawn '{program}': {e}"))?;
            ()
                .serve(transport)
                .await
                .map_err(|e| anyhow!("stdio MCP handshake failed: {e:?}"))?
        }
        "http" | "streamable" | "sse" => {
            if server.url.is_empty() {
                return Err(anyhow!(
                    "'{}' server '{}' has no url",
                    server.transport,
                    server.name
                ));
            }
            if server.transport == "sse" {
                log::info!(
                    "assistant_runtime: MCP server '{}' declares transport 'sse'; the pinned rmcp \
                     folds SSE into the streamable-HTTP client (no standalone SSE transport), \
                     connecting via streamable-HTTP.",
                    server.name
                );
            }
            // `from_config` builds the transport's own reqwest client
            // internally, so the executor's reqwest version is never involved.
            let mut config = StreamableHttpClientTransportConfig::with_uri(server.url.clone());
            if !server.auth.is_empty() {
                // Bearer token value (no `Bearer ` prefix), per the config API.
                config.auth_header = Some(server.auth.clone());
            }
            let transport = StreamableHttpClientTransport::from_config(config);
            ()
                .serve(transport)
                .await
                .map_err(|e| anyhow!("streamable-HTTP MCP handshake failed: {e:?}"))?
        }
        "websocket" | "ws" => {
            return Err(anyhow!(
                "transport 'websocket' is unsupported by the pinned rmcp (no ws client transport)"
            ));
        }
        other => return Err(anyhow!("unknown transport '{other}'")),
    };

    let raw_tools = client
        .list_all_tools()
        .await
        .map_err(|e| anyhow!("list_tools failed: {e}"))?;
    let tools = raw_tools.iter().map(tool_to_def).collect();

    Ok(McpConnection {
        server_name: server.name.clone(),
        client,
        tools,
    })
}

/// Call one MCP tool and flatten its result content to a string.
async fn call_mcp_tool(
    client: &RunningService<RoleClient, ()>,
    name: &str,
    arguments: &str,
) -> Result<String> {
    // The OpenAI `function.arguments` is a JSON *string* of an object.
    let parsed: Value = serde_json::from_str(arguments).unwrap_or(Value::Null);
    let arguments = match parsed {
        Value::Object(map) => Some(map),
        _ => None,
    };

    let result = client
        .call_tool(CallToolRequestParams {
            meta: None,
            name: name.to_string().into(),
            arguments,
            task: None,
        })
        .await
        .map_err(|e| anyhow!("call_tool '{name}' failed: {e}"))?;

    // Prefer concatenated text blocks; fall back to structured content / raw.
    let text = result
        .content
        .iter()
        .filter_map(|c| c.as_text().map(|t| t.text.clone()))
        .collect::<Vec<_>>()
        .join("\n");
    if !text.is_empty() {
        Ok(text)
    } else if let Some(structured) = result.structured_content {
        Ok(structured.to_string())
    } else {
        Ok(serde_json::to_string(&result.content).unwrap_or_default())
    }
}

/// Map an rmcp [`Tool`] to an OpenAI-shaped [`ToolDef`]: name, description, and
/// `inputSchema` → `parameters`.
fn tool_to_def(tool: &Tool) -> ToolDef {
    ToolDef {
        kind: "function".to_string(),
        function: FunctionDef {
            name: tool.name.to_string(),
            description: tool.description.as_ref().map(|d| d.to_string()),
            parameters: Some(Value::Object((*tool.input_schema).clone())),
        },
    }
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

fn def(name: &str, description: &str, parameters: Value) -> ToolDef {
    ToolDef {
        kind: "function".to_string(),
        function: FunctionDef {
            name: name.to_string(),
            description: Some(description.to_string()),
            parameters: Some(parameters),
        },
    }
}

fn req_str(v: &Value, key: &str) -> Result<String> {
    v.get(key)
        .and_then(Value::as_str)
        .map(|s| s.to_string())
        .ok_or_else(|| anyhow!("Missing required argument: {key}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_advertises_graph_tools() {
        let t = BuiltinTools::new("uuid-1".into());
        let names: Vec<String> = t.tools().into_iter().map(|d| d.function.name).collect();
        assert!(names.contains(&"perspective_add_link".to_string()));
        assert!(names.contains(&"perspective_query_links".to_string()));
        assert!(names.contains(&"neighbourhood_publish".to_string()));
    }

    #[test]
    fn mcp_tool_maps_to_tooldef() {
        use std::sync::Arc;
        let mut schema = serde_json::Map::new();
        schema.insert("type".to_string(), json!("object"));
        schema.insert(
            "properties".to_string(),
            json!({ "q": { "type": "string" } }),
        );
        let tool = Tool::new("search", "Search the web", Arc::new(schema));

        let def = tool_to_def(&tool);
        assert_eq!(def.kind, "function");
        assert_eq!(def.function.name, "search");
        assert_eq!(def.function.description.as_deref(), Some("Search the web"));
        let params = def.function.parameters.expect("parameters mapped");
        assert_eq!(params["type"], json!("object"));
        assert_eq!(params["properties"]["q"]["type"], json!("string"));
    }

    #[test]
    fn empty_mcp_provider_advertises_no_tools() {
        assert!(McpToolProvider::empty().tools().is_empty());
    }

    #[test]
    fn toolset_routes_by_ownership() {
        let set = ToolSet::new(vec![
            ToolProvider::Builtin(BuiltinTools::new("u".into())),
            ToolProvider::Mcp(McpToolProvider::empty()),
        ]);
        assert!(!set.is_empty());
        assert!(set
            .tool_defs()
            .iter()
            .any(|d| d.function.name == "perspective_add_link"));
    }
}
