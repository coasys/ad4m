//! What the link probe reaches also runs: under Node, an in-memory SQLite database, an
//! in-memory Oxigraph store, and the handler map of the methods the web executor serves.
#![cfg(target_arch = "wasm32")]

use rust_executor::api::ws_handler::build_handler_map;
use rust_executor::db::Ad4mDb;
use rust_executor::perspectives::sparql_store::SparqlStore;
use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
fn sqlite_and_oxigraph_open_in_memory() {
    Ad4mDb::init_global_instance(":memory:").expect("SQLite opens :memory: on wasm32");
    SparqlStore::new(None).expect("Oxigraph opens in memory on wasm32");
}

#[wasm_bindgen_test]
fn the_handler_map_serves_the_ring_2_namespaces_only() {
    let map = build_handler_map();
    let methods: Vec<&str> = map.guards().into_iter().map(|(name, _)| name).collect();
    // agent_ws, perspectives_ws and perspective_instances_ws register; every other handler
    // module registers nothing on wasm32 (api/mod_wasm.rs).
    let outside: Vec<&&str> = methods
        .iter()
        .filter(|m| !m.starts_with("agent.") && !m.starts_with("perspective."))
        .collect();
    assert!(outside.is_empty(), "methods outside ring 2: {outside:?}");
    // The calls the in-memory boot (S9) answers first.
    for method in [
        "agent.generate",
        "agent.status",
        "perspective.create",
        "perspective.addLink",
        "perspective.queryLinks",
        "perspective.querySparql",
    ] {
        assert!(methods.contains(&method), "{method} is not registered");
    }
}
