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

use ad4m_client::model::{CreateOptions, Model, ModelQuery, ModelSubscription};
use ad4m_client::Ad4mClient;
use futures_util::future::{select, Either};
use serde_json::{json, Map, Value};
use std::cell::Cell;
use std::future::Future;
use std::rc::Rc;
use std::time::Duration;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
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

// ── helpers for the reconnect / auth tests ─────────────────────────────

/// The executor behind the scripted peer's counting proxy: every TCP
/// connection the client opens (each dial, each redial) counts, and
/// `cut()` drops them all without a close frame (a network loss).
fn proxy_url() -> String {
    option_env!("EXECUTOR_PROXY_URL")
        .expect("EXECUTOR_PROXY_URL: run through scripts/test-wasm-client.sh")
        .to_string()
}

async fn peer_get(path: &str) -> Value {
    let base = option_env!("FAKE_PEER_URL").expect("FAKE_PEER_URL");
    let window = web_sys::window().unwrap();
    let resp = JsFuture::from(window.fetch_with_str(&format!("{base}/{path}")))
        .await
        .unwrap();
    let resp: web_sys::Response = resp.dyn_into().unwrap();
    let text = JsFuture::from(resp.text().unwrap()).await.unwrap();
    serde_json::from_str(&text.as_string().unwrap()).unwrap()
}

/// How many connections the proxy has accepted so far.
async fn dials() -> u64 {
    peer_get("count/proxy").await["accepted"].as_u64().unwrap()
}

/// Drop every proxied connection: an ordinary network close (1006).
async fn cut() {
    peer_get("cut").await;
}

async fn sleep(d: Duration) {
    gloo_timers::future::sleep(d).await
}

/// `fut`, or a panic naming `what` after `d`.
async fn within<T>(d: Duration, what: &str, fut: impl Future<Output = T>) -> T {
    let timer = gloo_timers::future::sleep(d);
    match select(std::pin::pin!(fut), std::pin::pin!(timer)).await {
        Either::Left((v, _)) => v,
        Either::Right(_) => panic!("timed out after {d:?}: {what}"),
    }
}

/// Poll `cond` every 50 ms until it holds, or panic naming `what`.
async fn until(d: Duration, what: &str, mut cond: impl FnMut() -> bool) {
    within(d, what, async {
        while !cond() {
            sleep(Duration::from_millis(50)).await;
        }
    })
    .await
}

/// A real app token: requested, permitted by the operator, redeemed. It
/// opens a grant on the main agent, so revoking it ends its sessions.
async fn app_token(admin: &Ad4mClient, name: &str) -> String {
    let auth = json!({
        "appName": name,
        "appDesc": "wasm browser test",
        "appDomain": "test.ad4m.org",
        "appUrl": "https://test.ad4m.org",
        "capabilities": [{"with": {"domain": "*", "pointers": ["*"]}, "can": ["*"]}],
    });
    let request_id: String = admin
        .ws()
        .call("agent.requestCapability", json!({ "authInfo": auth }))
        .await
        .expect("agent.requestCapability");
    let rand: String = admin
        .ws()
        .call(
            "agent.permitCapability",
            json!({ "auth": json!({"requestId": request_id, "auth": auth}).to_string() }),
        )
        .await
        .expect("agent.permitCapability");
    admin
        .ws()
        .call(
            "agent.generateJwt",
            json!({"requestId": request_id, "rand": rand}),
        )
        .await
        .expect("agent.generateJwt")
}

const NOTE_SDNA: &str = r#"{
  "target_class": "ad4m://WasmNote",
  "constructor_actions": [
    {"action": "addLink", "source": "this", "predicate": "ad4m://wasmnote/title", "target": "placeholder"}
  ],
  "properties": [
    {"path": "ad4m://wasmnote/title", "name": "title", "identity": true, "min_count": 1, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://wasmnote/title", "target": "value"}]}
  ]
}"#;

/// A perspective with the note class, and a model over it.
async fn note_model(client: &Ad4mClient, name: &str) -> Model {
    let handle = client
        .perspectives
        .add(name.to_string())
        .await
        .expect("perspective.add");
    client
        .ws()
        .call::<Value>(
            "perspective.addSdna",
            json!({
                "uuid": handle.uuid,
                "name": "WasmNote",
                "sdnaCode": "",
                "sdnaType": "subject_class",
                "shaclJson": NOTE_SDNA,
            }),
        )
        .await
        .expect("perspective.addSdna");
    Model::new(client.ws(), handle.uuid, "WasmNote")
}

async fn create_note(model: &Model, title: &str) {
    let mut props = Map::new();
    props.insert("title".into(), json!(title));
    model
        .create(props, CreateOptions::default())
        .await
        .expect("create a note");
}

/// Read the subscription until a result lists a note titled `title`.
async fn until_note(sub: &mut ModelSubscription, title: &str) {
    within(
        Duration::from_secs(20),
        &format!("a live result with {title}"),
        async {
            loop {
                let r = sub.next().await.expect("the live query ended");
                if r.instances.iter().any(|i| i["title"] == json!(title)) {
                    return;
                }
            }
        },
    )
    .await
}

/// A reconnect callback counter.
async fn count_reconnects(client: &Ad4mClient) -> Rc<Cell<u32>> {
    let n = Rc::new(Cell::new(0));
    let n2 = n.clone();
    client.ws().on_reconnect(move || n2.set(n2.get() + 1)).await;
    n
}

/// The reconnect backoff starts at 1 s (± jitter) and doubles: 4 s of
/// silence covers at least two redials a non-terminal close would make.
const NO_REDIAL_WINDOW: Duration = Duration::from_secs(4);

// ── item 3: a real credential ──────────────────────────────────────────

/// The admin credential (it needs URL encoding) authenticates an RPC and
/// receives a watched event over the browser socket.
#[wasm_bindgen_test]
async fn a_real_credential_authenticates_calls_and_receives_events() {
    let client = admin().await;
    let mut events = client
        .watch_events(json!({"perspective-added": null}))
        .await
        .expect("events.watch");
    let handle = client
        .perspectives
        .add("wasm-event".to_string())
        .await
        .expect("perspective.add");
    let event = within(Duration::from_secs(10), "perspective-added", async {
        loop {
            let e = events.recv().await.expect("the event stream ended");
            if e.to_string().contains(&handle.uuid) {
                return e;
            }
        }
    })
    .await;
    assert!(event.to_string().contains("perspective-added"), "{event}");
    client.ws().close().await;
}

// ── item 1: server-driven 1008 ─────────────────────────────────────────

/// An invalid token: the executor completes the upgrade, then closes with
/// 1008. A call fails at once (not at its 30 s deadline) and the client
/// never dials again.
#[wasm_bindgen_test]
async fn an_invalid_token_is_refused_with_1008_and_never_redials() {
    let before = dials().await;
    // The upgrade succeeds (a browser cannot read a refused upgrade's
    // status), so the connect does too; the refusal is the close after it.
    let client = Ad4mClient::connect(proxy_url(), "not-a-valid-token".to_string())
        .await
        .expect("the upgrade itself succeeds");
    let r = within(
        Duration::from_secs(5),
        "a call on a refused token",
        client.agent.status(),
    )
    .await;
    let err = r
        .expect_err("a call on a refused token succeeded")
        .to_string();
    assert!(
        err.contains("authentication refused") || err.contains("closed"),
        "{err}"
    );
    assert!(!client.ws().is_connected());
    sleep(NO_REDIAL_WINDOW).await;
    assert_eq!(
        dials().await,
        before + 1,
        "the client dialled again after 1008"
    );
}

/// A session whose token is revoked: the executor closes it with 1008.
/// The live query ends, calls fail at once, and the client never dials
/// again.
#[wasm_bindgen_test]
async fn a_revoked_session_closes_with_1008_ends_its_queries_and_never_redials() {
    let admin = admin().await;
    let token = app_token(&admin, "wasm-revoked").await;
    let app = Ad4mClient::connect(proxy_url(), token.clone())
        .await
        .expect("connect with the app token");
    let model = note_model(&app, "wasm-revoked").await;
    let mut sub = model
        .subscribe(&ModelQuery::default())
        .await
        .expect("subscribe");
    within(Duration::from_secs(10), "the first result", sub.next()).await;
    let before = dials().await;

    let _: Value = admin
        .ws()
        .call("agent.revokeToken", json!({ "token": token }))
        .await
        .expect("agent.revokeToken");

    let ended = within(Duration::from_secs(10), "the live query to end", sub.next()).await;
    assert!(
        ended.is_none(),
        "the live query outlived the revoked session"
    );
    let err = within(
        Duration::from_secs(5),
        "a call after revoke",
        app.agent.status(),
    )
    .await
    .expect_err("a call on a revoked session succeeded")
    .to_string();
    assert!(
        err.contains("authentication refused") || err.contains("closed"),
        "{err}"
    );
    sleep(NO_REDIAL_WINDOW).await;
    assert_eq!(dials().await, before, "the client dialled again after 1008");
    admin.ws().close().await;
}

// ── item 2: 1013 and an ordinary network close ─────────────────────────

/// An ordinary network close (the proxy drops the TCP connections): one
/// reconnect sequence, one callback, the watch restored.
#[wasm_bindgen_test]
async fn a_network_close_reconnects_once_and_restores_the_watch() {
    let client = Ad4mClient::connect(proxy_url(), admin_token())
        .await
        .unwrap();
    let reconnects = count_reconnects(&client).await;
    let mut events = client
        .watch_events(json!({"perspective-added": null}))
        .await
        .unwrap();
    cut().await;
    until(Duration::from_secs(10), "a reconnect", || {
        reconnects.get() >= 1
    })
    .await;
    sleep(Duration::from_secs(3)).await;
    assert_eq!(reconnects.get(), 1, "one close ran more than one reconnect");
    assert!(client.ws().is_connected());

    let handle = client
        .perspectives
        .add("wasm-after-cut".into())
        .await
        .unwrap();
    within(
        Duration::from_secs(10),
        "an event after the reconnect",
        async {
            loop {
                let e = events.recv().await.expect("the event stream ended");
                if e.to_string().contains(&handle.uuid) {
                    return;
                }
            }
        },
    )
    .await;
    client.ws().close().await;
}

/// The wallet locked while the socket is down: each redial is closed
/// with 1013 and retried (not terminal). After the unlock the same token
/// gets back in: one reconnect callback, the watch restored, the live
/// query re-opened and receiving fresh updates.
#[wasm_bindgen_test]
async fn a_locked_wallet_closes_with_1013_and_the_same_token_recovers() {
    let admin = admin().await;
    let token = app_token(&admin, "wasm-locked").await;
    let app = Ad4mClient::connect(proxy_url(), token).await.unwrap();
    let model = note_model(&app, "wasm-locked").await;
    let mut sub = model.subscribe(&ModelQuery::default()).await.unwrap();
    within(Duration::from_secs(10), "the first result", sub.next()).await;
    let reconnects = count_reconnects(&app).await;
    let mut events = app
        .watch_events(json!({"perspective-added": null}))
        .await
        .unwrap();

    admin
        .agent
        .lock(PASSPHRASE.into())
        .await
        .expect("agent.lock");
    let before = dials().await;
    cut().await;
    // Observe the locked phase, then unlock before asserting: a failure
    // must not leave the shared wallet locked for the other tests.
    let mut redials = 0;
    for _ in 0..200 {
        redials = dials().await - before;
        if redials >= 2 {
            break;
        }
        sleep(Duration::from_millis(50)).await;
    }
    let reconnected_while_locked = reconnects.get();
    let connected_while_locked = app.ws().is_connected();
    admin
        .agent
        .unlock(PASSPHRASE.into(), false)
        .await
        .expect("agent.unlock");
    assert!(
        redials >= 2,
        "1013 was terminal: {redials} redial(s) in 10 s"
    );
    assert_eq!(reconnected_while_locked, 0);
    assert!(!connected_while_locked);

    until(
        Duration::from_secs(40),
        "a reconnect after the unlock",
        || reconnects.get() >= 1,
    )
    .await;
    assert!(app.ws().is_connected());

    let handle = app
        .perspectives
        .add("wasm-after-unlock".into())
        .await
        .unwrap();
    within(
        Duration::from_secs(10),
        "an event after the unlock",
        async {
            loop {
                let e = events.recv().await.expect("the event stream ended");
                if e.to_string().contains(&handle.uuid) {
                    return;
                }
            }
        },
    )
    .await;
    create_note(&model, "after-unlock").await;
    until_note(&mut sub, "after-unlock").await;
    assert_eq!(reconnects.get(), 1, "more than one reconnect callback");
    admin.ws().close().await;
    app.ws().close().await;
}

// ── A6-5: a panicking reconnect callback ───────────────────────────────

/// A callback that panics (wasm has no unwinding: the panic traps the
/// task that runs it) must not take the reconnect supervisor or the
/// callbacks after it down: the next callback still runs, and a second
/// outage is still redialled.
#[wasm_bindgen_test]
async fn a_panicking_reconnect_callback_does_not_stop_reconnects() {
    let client = Ad4mClient::connect(proxy_url(), admin_token())
        .await
        .unwrap();
    client
        .ws()
        .on_reconnect(|| panic!("a6-5: this reconnect callback panics on purpose"))
        .await;
    let reconnects = count_reconnects(&client).await;

    cut().await;
    until(Duration::from_secs(10), "the first reconnect", || {
        reconnects.get() >= 1
    })
    .await;
    cut().await;
    until(Duration::from_secs(10), "the second reconnect", || {
        reconnects.get() >= 2
    })
    .await;
    assert!(client.ws().is_connected());
    client
        .agent
        .status()
        .await
        .expect("a call after both reconnects");
    client.ws().close().await;
}

// ── item 4: explicit close and disconnect leave nothing behind ─────────

/// Every exception that reaches the page uncaught (a handler that throws).
fn trap_uncaught() -> (
    Rc<std::cell::RefCell<Vec<String>>>,
    wasm_bindgen::closure::Closure<dyn FnMut(web_sys::Event)>,
) {
    let errors = Rc::new(std::cell::RefCell::new(Vec::new()));
    let handler = wasm_bindgen::closure::Closure::wrap(Box::new({
        let errors = errors.clone();
        move |ev: web_sys::Event| {
            let msg = js_sys::Reflect::get(&ev, &"message".into())
                .ok()
                .and_then(|m| m.as_string())
                .unwrap_or_else(|| ev.type_());
            errors.borrow_mut().push(msg);
        }
    }) as Box<dyn FnMut(_)>);
    web_sys::window()
        .unwrap()
        .add_event_listener_with_callback("error", handler.as_ref().unchecked_ref())
        .unwrap();
    (errors, handler)
}

async fn open_proxied() -> u64 {
    peer_get("count/proxy").await["open"].as_u64().unwrap()
}

/// `disconnect_for_testing` and `close` sever the real socket (the proxy
/// sees it end), raise no uncaught exception from a handler of the
/// dropped socket, and `close` leaves no redial behind.
#[wasm_bindgen_test]
async fn close_and_disconnect_release_the_socket_without_uncaught_errors() {
    let (errors, handler) = trap_uncaught();
    let open_before = open_proxied().await;
    let client = Ad4mClient::connect(proxy_url(), admin_token())
        .await
        .unwrap();
    let reconnects = count_reconnects(&client).await;

    client.ws().disconnect_for_testing().await;
    until(
        Duration::from_secs(10),
        "the reconnect after a disconnect",
        || reconnects.get() == 1,
    )
    .await;
    client
        .agent
        .status()
        .await
        .expect("a call after the reconnect");

    let dials_at_close = dials().await;
    client.ws().close().await;
    assert!(!client.ws().is_connected());
    within(Duration::from_secs(5), "the proxied socket to end", async {
        while open_proxied().await > open_before {
            sleep(Duration::from_millis(50)).await;
        }
    })
    .await;
    sleep(NO_REDIAL_WINDOW).await;
    assert_eq!(dials().await, dials_at_close, "a redial after close");
    assert!(client.agent.status().await.is_err(), "a call after close");
    assert_eq!(*errors.borrow(), Vec::<String>::new());
    web_sys::window()
        .unwrap()
        .remove_event_listener_with_callback("error", handler.as_ref().unchecked_ref())
        .unwrap();
}

// ── item 6: live query disposal in the browser ─────────────────────────

/// A disposed live query ends its stream at once, and later writes reach
/// no result; a live query opened next to it still updates.
#[wasm_bindgen_test]
async fn a_disposed_live_query_ends_and_a_sibling_keeps_updating() {
    let client = admin().await;
    let model = note_model(&client, "wasm-dispose").await;
    let mut gone = model.subscribe(&ModelQuery::default()).await.unwrap();
    let mut kept = model.subscribe(&ModelQuery::default()).await.unwrap();
    within(Duration::from_secs(10), "the first result", gone.next()).await;
    within(Duration::from_secs(10), "the first result", kept.next()).await;

    gone.dispose().await;
    assert!(gone.id().await.is_none(), "the disposed query kept its id");
    create_note(&model, "after-dispose").await;
    until_note(&mut kept, "after-dispose").await;
    let after = within(Duration::from_secs(5), "the disposed stream", gone.next()).await;
    assert!(after.is_none(), "a disposed live query delivered {after:?}");
    client.ws().close().await;
}
