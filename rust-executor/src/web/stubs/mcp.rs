//! `crate::mcp` on wasm32: no MCP server or tools in the web executor. Only `shacl`, the
//! native `mcp/shacl.rs` by path: ring 2 reads SHACL classes through it.

#[path = "../../mcp/shacl.rs"]
pub mod shacl;
