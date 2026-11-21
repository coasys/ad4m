// Crate root of the web executor (wasm32); modules are added by later slices.
// Included by src/lib.rs, so no inner doc comment (`//!`) is possible here.

pub mod rt;
mod web;
