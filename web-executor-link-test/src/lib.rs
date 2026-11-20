//! Link test for the web executor: `scripts/web-executor/check.sh` builds this cdylib for
//! wasm32 in release. The export reaches ring 2 (every real RPC handler, SQLite, Oxigraph),
//! so the linker keeps that code and a missing symbol fails the build. Empty on native.
#![cfg(target_arch = "wasm32")]

use rust_executor::api::ws_handler::build_handler_map;
use rust_executor::db::Ad4mDb;
use rust_executor::perspectives::sparql_store::SparqlStore;

/// The number of RPC methods the web executor registers; 0 when the in-memory database or
/// link store does not open.
#[no_mangle]
pub extern "C" fn ad4m_web_link_probe() -> u32 {
    if Ad4mDb::init_global_instance(":memory:").is_err() {
        return 0;
    }
    if SparqlStore::new(None).is_err() {
        return 0;
    }
    build_handler_map().len() as u32
}
