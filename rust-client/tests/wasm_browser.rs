//! Browser smoke test for the wasm transport: the `web_sys::WebSocket`
//! backend connects to a real `ad4m-executor`, authenticates, and runs
//! real queries — the same calls the native integration test makes.
//!
//! Driven by `scripts/test-wasm-client.sh`, which starts a scratch
//! executor (`--insecure-no-admin-credential`, no Holochain) and exports
//! `EXECUTOR_URL` + the webdriver env. By hand:
//!
//!   EXECUTOR_URL=http://127.0.0.1:PORT \
//!   CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner \
//!   CHROMEDRIVER=/snap/bin/chromium.chromedriver \
//!   cargo test --target wasm32-unknown-unknown --test wasm_browser

#![cfg(target_arch = "wasm32")]

use ad4m_client::Ad4mClient;
use serde_json::{json, Value};
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

fn executor_url() -> String {
    option_env!("EXECUTOR_URL")
        .unwrap_or("http://127.0.0.1:12000")
        .to_string()
}

#[wasm_bindgen_test]
async fn wasm_client_connects_authenticates_and_queries() {
    let client = Ad4mClient::connect(executor_url(), String::new())
        .await
        .expect("connect to executor");
    assert!(client.ws().is_connected());

    // A normal-timeout read first: proves the pending-map round trip.
    let st0 = client.agent.status().await.expect("agent.status");
    assert!(
        !st0.is_initialized,
        "fresh data dir must start uninitialised"
    );

    // Auth: --insecure-no-admin-credential accepts the empty token, so
    // this is the real authenticated session — agent.generate mints the
    // agent on the fresh data dir (a LONG_METHODS call — its reply must
    // arrive over the browser socket, not a shim).
    let status = client
        .agent
        .generate("wasm-pass".to_string())
        .await
        .expect("agent.generate");
    assert!(status.is_unlocked);
    assert!(status.did.is_some());

    // A perspective, then a links query over it — a real executor read.
    let handle = client
        .perspectives
        .add("wasm-a6".to_string())
        .await
        .expect("perspective.add");
    assert!(!handle.uuid.is_empty());
    let links: Value = client
        .ws()
        .call("perspective.queryLinks", json!({"uuid": handle.uuid}))
        .await
        .expect("perspective.queryLinks");
    assert!(links.is_array());

    // Event watch round-trip: set_watch sends and awaits the reply — the
    // full pending-map path over the browser transport.
    let _rx = client
        .watch_events(json!({"perspective-added": null}))
        .await
        .expect("watch_events");

    client.ws().close().await;
    assert!(!client.ws().is_connected());
}
