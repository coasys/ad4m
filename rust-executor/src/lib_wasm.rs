// Crate root of the web executor (wasm32); modules are added by later slices.
// Included by src/lib.rs, so no inner doc comment (`//!`) is possible here.

pub mod rt;
mod web;

// Ring 2 names scryer-prolog's answer types by their crate path (`use scryer_prolog::Term;` in
// perspectives/utils.rs). scryer-prolog does not build for wasm32: this crate takes the name,
// and its root re-exports the stand-ins from web/scryer_types.rs.
extern crate self as scryer_prolog;
pub use web::scryer_types::{LeafAnswer, Term};
