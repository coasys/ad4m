//! MCP (Model Context Protocol) server for AD4M
//!
//! Provides a standardized interface for AI agents to interact with AD4M's
//! distributed knowledge graph. Agents can create perspectives, register SHACL
//! models, create/query/modify typed subject instances, manage collections,
//! and work with flows (state machines) — all through MCP's tool-calling protocol.
//!
//! ## Architecture
//!
//! - [`server`] — HTTP transport setup and auth middleware
//! - [`shacl`] — SHACL class/property parsing from perspective links
//! - `tools` — MCP tool definitions (static + dynamic SHACL-generated)

pub mod server;
pub mod shacl;
mod tools;

pub use server::{start_mcp_server, McpServerConfig};
