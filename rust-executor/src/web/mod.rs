//! Code that exists only in the web executor (wasm32). Declared in `lib_wasm.rs` only.

pub mod rt;
pub mod scryer_types;
pub mod time;
// `prolog_service/` is `crate::prolog_service`, declared by path in `lib_wasm.rs`.
pub mod stubs;
