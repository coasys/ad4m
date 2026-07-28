//! Tool providers for the assistant loop.
//!
//! Tool execution sits behind a small provider abstraction so the model's
//! granted tools can come from more than one source. Two providers ship today:
//!
//! * [`BuiltinTools`] — in-process perspective / neighbourhood graph
//!   operations, fully implemented (zero HTTP; direct
//!   `PerspectiveInstance`/`neighbourhoods` calls).
//! * [`McpToolProvider`] — external MCP servers configured on an assistant.
//!   The live MCP **client** transport is a documented follow-up: `rmcp` is
//!   currently built with server features only, so wiring a client means
//!   enabling `rmcp`'s `client` + `transport-*-client` features and connecting
//!   each `McpServer`. Until then this provider exposes **no** tools and any
//!   attempt to call one returns an explicit error (never a silent stub). The
//!   provider boundary is the seam that follow-up work slots into without
//!   touching the loop.
//!
//! Dispatch is a plain enum (no `async-trait` dependency); [`ToolSet`]
//! aggregates providers and routes a call to whichever owns the tool name.

use anyhow::{anyhow, Result};
use serde_json::{json, Value};

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
// MCP tool provider (live client transport is a documented follow-up)
// ---------------------------------------------------------------------------

/// Tools from an assistant's configured external MCP servers.
///
/// FOLLOW-UP: connecting requires enabling `rmcp`'s client features
/// (`client`, `transport-streamable-http-client`, `transport-sse-client`) in
/// `rust-executor/Cargo.toml` and, per configured [`McpServer`], establishing a
/// session, running `list_tools`, and forwarding `call_tool`. That work slots
/// in behind this provider without touching the run loop or [`ToolSet`]. Until
/// it lands this provider is inert: no tools are advertised and any call fails
/// loudly.
pub struct McpToolProvider {
    servers: Vec<McpServer>,
}

impl McpToolProvider {
    pub fn new(servers: Vec<McpServer>) -> Self {
        if !servers.is_empty() {
            log::warn!(
                "assistant_runtime: {} MCP server(s) configured on this assistant but the MCP \
                 client transport is not yet wired (follow-up: enable rmcp client features and \
                 connect). Their tools are unavailable for this run.",
                servers.len()
            );
        }
        Self { servers }
    }

    fn tools(&self) -> Vec<ToolDef> {
        // No tools until the client transport is wired — deliberately empty so
        // the model is never offered a tool the executor cannot fulfil.
        Vec::new()
    }

    async fn execute(&self, name: &str, _arguments: &str) -> Result<String> {
        Err(anyhow!(
            "MCP tool '{name}' is unavailable: the MCP client transport is not yet wired \
             (follow-up behind McpToolProvider). {} server(s) configured.",
            self.servers.len()
        ))
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
    fn mcp_provider_is_inert_until_wired() {
        let p = McpToolProvider::new(vec![McpServer {
            id: "m".into(),
            name: "srv".into(),
            transport: "http".into(),
            url: "http://x".into(),
            ..Default::default()
        }]);
        assert!(p.tools().is_empty());
    }

    #[test]
    fn toolset_routes_by_ownership() {
        let set = ToolSet::new(vec![
            ToolProvider::Builtin(BuiltinTools::new("u".into())),
            ToolProvider::Mcp(McpToolProvider::new(vec![])),
        ]);
        assert!(!set.is_empty());
        assert!(set
            .tool_defs()
            .iter()
            .any(|d| d.function.name == "perspective_add_link"));
    }
}
