//! Code that exists only in the web executor (wasm32). Declared in `lib_wasm.rs` only.

pub mod rt;
pub mod scryer_types;
pub mod time;
// `prolog_service/` is declared by the flip (S8), as `crate::prolog_service`: it names
// `crate::types`, which `lib_wasm.rs` does not have yet.
