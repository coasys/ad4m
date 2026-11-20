// Crate root of the web executor (wasm32): ring 2 real, the modules outside it stubbed.
// Included by src/lib.rs, so no inner doc comment (`//!`) is possible here.

#[macro_use]
extern crate lazy_static;

// ---- ring 2: the executor's own modules, unchanged ----
pub mod agent;
#[path = "api/mod_wasm.rs"]
pub mod api;
pub mod billing;
pub mod config;
pub mod config_file;
pub mod db;
pub mod db_backend;
mod events;
mod globals;
pub mod helpers;
pub mod log_files;
pub mod logging;
pub mod perspectives;
pub mod presence;
#[path = "web/prolog_service/mod.rs"]
mod prolog_service;
mod pubsub;
pub mod rt;
pub mod secret;
pub mod types;
pub mod user_management;
pub mod utils;
pub mod wallet;
pub use config::Ad4mConfig;
pub use secret::Secret;

pub mod web;

// ---- outside ring 2: hand-written stubs (src/web/stubs/), only the items ring 2 names ----
#[path = "web/stubs/ai_service.rs"]
pub mod ai_service;
#[path = "web/stubs/ai_sessions.rs"]
pub mod ai_sessions;
#[path = "web/stubs/config_rpc.rs"]
pub mod config_rpc;
#[path = "web/stubs/email_service.rs"]
pub mod email_service;
#[path = "entanglement_service/mod.rs"]
pub mod entanglement_service;
#[path = "web/stubs/holochain_service.rs"]
pub mod holochain_service;
#[path = "web/stubs/identity.rs"]
pub mod identity;
#[path = "web/stubs/languages/mod.rs"]
pub mod languages;
#[path = "web/stubs/mcp.rs"]
pub mod mcp;
#[path = "web/stubs/runtime_service.rs"]
pub mod runtime_service;
#[path = "web/stubs/sfu.rs"]
pub mod sfu;

// Ring 2 names scryer-prolog's answer types by their crate path (`use scryer_prolog::Term;` in
// perspectives/utils.rs). scryer-prolog does not build for wasm32: this crate takes the name,
// and its root re-exports the stand-ins from web/scryer_types.rs.
extern crate self as scryer_prolog;
pub use web::scryer_types::{LeafAnswer, Term};
