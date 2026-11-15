//! Browser smoke test for the wasm transport: the `web_sys::WebSocket`
//! backend connects to a real `ad4m-executor`, authenticates, and runs
//! real queries — the same calls the native integration test makes.
//!
//! Driven by `scripts/test-wasm-client.sh`, which starts a scratch
//! executor with an admin credential (no Holochain) and the scripted peer
//! `scripts/wasm-test-server.py`, and exports `EXECUTOR_URL`,
//! `EXECUTOR_PROXY_URL` (the counting proxy), `FAKE_PEER_URL`,
//! `ADMIN_TOKEN` + the webdriver env. By hand:
//!
//!   EXECUTOR_URL=http://127.0.0.1:PORT ADMIN_TOKEN=... \
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

/// The executor's real admin credential (it contains characters a query
/// string must encode).
fn admin_token() -> String {
    option_env!("ADMIN_TOKEN")
        .expect("ADMIN_TOKEN: run through scripts/test-wasm-client.sh")
        .to_string()
}

/// The admin client, with the main agent generated and unlocked whatever
/// test runs first.
async fn admin() -> Ad4mClient {
    let client = Ad4mClient::connect(executor_url(), admin_token())
        .await
        .expect("connect to executor with the admin credential");
    let status = client.agent.status().await.expect("agent.status");
    if !status.is_initialized {
        let status = client
            .agent
            .generate(PASSPHRASE.to_string())
            .await
            .expect("agent.generate");
        assert!(status.is_unlocked);
    } else if !status.is_unlocked {
        client
            .agent
            .unlock(PASSPHRASE.to_string(), false)
            .await
            .expect("agent.unlock");
    }
    client
}

const PASSPHRASE: &str = "wasm-pass";

#[wasm_bindgen_test]
async fn wasm_client_connects_authenticates_and_queries() {
    // Auth: the executor runs with an admin credential, so this is a real
    // authenticated session (agent.generate, when this test runs first, is
    // a LONG_METHODS call whose reply crosses the browser socket).
    let client = admin().await;
    assert!(client.ws().is_connected());
    let status = client.agent.status().await.expect("agent.status");
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
