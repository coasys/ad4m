//! MCP (Model Context Protocol) server for AD4M
//!
//! Exposes AD4M Subject operations via MCP, enabling AI agents to work with
//! typed models instead of raw links.

mod server;
mod tools;

pub use server::start_mcp_server;
