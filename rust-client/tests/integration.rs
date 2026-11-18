//! Real-executor integration coverage for the A4 client surface: `Model`
//! CRUD + subscriptions, batches, reconnect resync, and `FlowInstance`.
//!
//! Spawns the `ad4m-executor` binary like `tests/js`' `spawnExecutor` does:
//! `EXECUTOR_PATH` if set, else `<target dir>/release/ad4m-executor` under
//! `CARGO_TARGET_DIR` or the workspace `target/`. Skips (prints and passes)
//! when no binary exists — build one with `cargo build --release -p
//! ad4m-executor` to run it.
//!
//! Two ways the skip must *not* hide a failure (F11):
//! - `AD4M_REQUIRE_EXECUTOR=1`: CI's gate — a missing binary is a failure.
//! - `EXECUTOR_PATH` set but dangling: a misspelled path is a failure.

use ad4m_client::flow::{FlowApi, FlowInstance, FlowState, FlowTransition, ShaclFlow};
use ad4m_client::model::{CreateOptions, Model, ModelQuery};
use ad4m_client::Ad4mClient;
use serde_json::{json, Map, Value};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::net::{TcpListener, TcpStream};

fn executor_binary() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("EXECUTOR_PATH") {
        let p = PathBuf::from(p);
        // An explicit path that doesn't resolve is a misspelled
        // environment, not an absent executor — skipping here reports
        // success without reaching an assertion (F11).
        assert!(p.exists(), "EXECUTOR_PATH does not exist: {}", p.display());
        return Some(p);
    }
    let mut candidates = Vec::new();
    if let Ok(dir) = std::env::var("CARGO_TARGET_DIR") {
        candidates.push(PathBuf::from(dir).join("release/ad4m-executor"));
    }
    // Workspace root = rust-client/..
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    candidates.push(root.join("target/release/ad4m-executor"));
    let found = candidates.into_iter().find(|p| p.exists());
    if found.is_none()
        && std::env::var("AD4M_REQUIRE_EXECUTOR").is_ok_and(|v| v != "0" && !v.is_empty())
    {
        panic!(
            "AD4M_REQUIRE_EXECUTOR is set but no ad4m-executor binary was found \
             (checked CARGO_TARGET_DIR/release and target/release; set EXECUTOR_PATH)"
        );
    }
    found
}

fn free_port() -> u16 {
    std::net::TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

struct Executor {
    child: Child,
    url: String,
    port: u16,
    _data: tempfile::TempDir,
}

fn start_executor() -> Option<Executor> {
    let binary = executor_binary()?;
    let data = tempfile::tempdir().unwrap();
    let port = free_port();
    // `init` first — as `tests/js/startExecutor` does: `run` on a data dir
    // that never saw `init` leaves the runtime service without its seed and
    // `agent.generate` panics executor-side. The seed is the one the JS
    // suites use; its language addresses may not resolve, which is fine —
    // only the language-language is started and our reads don't need it.
    let seed = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../tests/js/bootstrapSeed.json");
    let init = Command::new(&binary)
        .args([
            "init",
            "--data-path",
            data.path().to_str().unwrap(),
            "--network-bootstrap-seed",
            seed.to_str().unwrap(),
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("ad4m-executor init");
    assert!(init.success(), "ad4m-executor init failed");
    let child = Command::new(&binary)
        .args([
            "run",
            "--app-data-path",
            data.path().to_str().unwrap(),
            "--port",
            &port.to_string(),
            "--run-holochain",
            "false",
            "--language-language-only",
            "true",
            "--run-dapp-server",
            "false",
            "--insecure-no-admin-credential",
        ])
        .env("AD4M_EXIT_WITH_PARENT", "true")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn ad4m-executor");
    Some(Executor {
        child,
        url: format!("http://127.0.0.1:{port}"),
        port,
        _data: data,
    })
}

/// A TCP relay in front of the executor that the test can take offline:
/// `set_online(false)` cuts every relayed connection and refuses new ones
/// (accepted, then closed before the WebSocket handshake), so a client
/// behind it stays disconnected, retrying, for as long as the test wants,
/// while other clients reach the executor directly.
struct Relay {
    url: String,
    online: Arc<AtomicBool>,
    conns: Arc<Mutex<Vec<tokio::task::JoinHandle<()>>>>,
    accept: tokio::task::JoinHandle<()>,
}

impl Relay {
    async fn start(upstream_port: u16) -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        let online = Arc::new(AtomicBool::new(true));
        let conns: Arc<Mutex<Vec<tokio::task::JoinHandle<()>>>> = Arc::default();
        let (on, cs) = (online.clone(), conns.clone());
        let accept = tokio::spawn(async move {
            while let Ok((mut down, _)) = listener.accept().await {
                if !on.load(Ordering::SeqCst) {
                    drop(down);
                    continue;
                }
                let relay = tokio::spawn(async move {
                    if let Ok(mut up) = TcpStream::connect(("127.0.0.1", upstream_port)).await {
                        let _ = tokio::io::copy_bidirectional(&mut down, &mut up).await;
                    }
                });
                cs.lock().unwrap().push(relay);
            }
        });
        Self {
            url: format!("http://127.0.0.1:{port}"),
            online,
            conns,
            accept,
        }
    }

    fn set_online(&self, online: bool) {
        self.online.store(online, Ordering::SeqCst);
        if !online {
            for relay in self.conns.lock().unwrap().drain(..) {
                relay.abort();
            }
        }
    }
}

impl Drop for Relay {
    fn drop(&mut self) {
        self.accept.abort();
        self.set_online(false);
    }
}

/// Poll `cond` every 100 ms until it holds; panic with `what` after `secs`.
async fn wait_until(secs: u64, what: &str, mut cond: impl FnMut() -> bool) {
    let deadline = std::time::Instant::now() + Duration::from_secs(secs);
    while !cond() {
        assert!(std::time::Instant::now() < deadline, "{what}");
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}

/// The executor takes seconds to come up — poll the socket until it takes.
async fn connect(url: &str) -> Ad4mClient {
    let deadline = std::time::Instant::now() + Duration::from_secs(120);
    loop {
        match Ad4mClient::connect(url.to_string(), String::new()).await {
            Ok(c) => return c,
            Err(e) => {
                assert!(
                    std::time::Instant::now() < deadline,
                    "executor did not come up in 120s: {e}"
                );
                tokio::time::sleep(Duration::from_millis(500)).await;
            }
        }
    }
}

impl Drop for Executor {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// The test class: one required single-valued `title`, one optional
/// `xsd://boolean` `done`, one `hasMany` collection `tags` — the smallest
/// shape `perspective.registerShapes` accepts that can prove a collection edit
/// rides the collection RPCs, modelled on the hardwired
/// `flow_instance.json`.
const NOTE_SDNA: &str = r#"{
  "target_class": "ad4m://IntegrationNote",
  "constructor_actions": [
    {"action": "addLink", "source": "this", "predicate": "ad4m://note/title", "target": "placeholder"}
  ],
  "properties": [
    {"path": "ad4m://note/title", "name": "title", "identity": true, "min_count": 1, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/title", "target": "value"}]},
    {"path": "ad4m://note/done", "name": "done", "datatype": "xsd://boolean", "min_count": 0, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/done", "target": "value"}]},
    {"path": "ad4m://note/tag", "name": "tags", "node_kind": "IRI", "relation_kind": "hasMany", "collection": true}
  ]
}"#;

fn props(entries: &[(&str, Value)]) -> Map<String, Value> {
    entries
        .iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect()
}

async fn next_result(
    sub: &mut ad4m_client::model::ModelSubscription,
    what: &str,
) -> ad4m_client::model::ModelResult {
    tokio::time::timeout(Duration::from_secs(30), sub.next())
        .await
        .unwrap_or_else(|_| panic!("timed out waiting for subscription result: {what}"))
        .expect("subscription ended")
}

/// Read results until one satisfies `want`, under one 30 s deadline for the
/// whole wait — results that do not match (an update still unread from
/// before a re-open, say) are skipped, not failed on.
async fn next_result_where(
    sub: &mut ad4m_client::model::ModelSubscription,
    what: &str,
    want: impl Fn(&ad4m_client::model::ModelResult) -> bool,
) -> ad4m_client::model::ModelResult {
    tokio::time::timeout(Duration::from_secs(30), async {
        loop {
            let result = sub.next().await.expect("subscription ended");
            if want(&result) {
                return result;
            }
        }
    })
    .await
    .unwrap_or_else(|_| panic!("timed out waiting for subscription result: {what}"))
}

#[tokio::test]
async fn model_batch_subscription_reconnect_and_flow() {
    let Some(executor) = start_executor() else {
        eprintln!("SKIP: no ad4m-executor binary (set EXECUTOR_PATH or build --release)");
        return;
    };
    // The client under test reaches the executor through a relay the test
    // can take down; the independent writer below connects directly.
    let relay = Relay::start(executor.port).await;
    let client = connect(&relay.url).await;
    // Fresh data dir → mint the agent (`agent.generate` is a long call).
    let status = client
        .agent
        .generate("passphrase".to_string())
        .await
        .unwrap();
    assert!(status.is_unlocked);

    // A perspective and the model class.
    let handle = client
        .perspectives
        .add("a4-integration".to_string())
        .await
        .unwrap();
    let uuid = handle.uuid.clone();
    client
        .ws()
        .call::<Value>(
            "perspective.registerShapes",
            json!({
                "uuid": uuid,
                "shapes": [serde_json::from_str::<Value>(NOTE_SDNA).unwrap()],
            }),
        )
        .await
        .unwrap();
    let model = Model::new(client.ws(), uuid.clone(), "IntegrationNote");

    // ── create + get + query ────────────────────────────────────────────
    let created = model
        .create(
            props(&[("title", json!("first")), ("done", json!(false))]),
            CreateOptions::default(),
        )
        .await
        .unwrap();
    let got = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert_eq!(got["title"], json!("first"));
    // Declared `xsd://boolean`: a stored `false` hydrates as a bool, not "false".
    assert_eq!(got["done"], json!(false));
    assert!(got["done"].is_boolean());
    let all = model.find_all(&ModelQuery::default()).await.unwrap();
    assert_eq!(all.len(), 1);
    assert_eq!(all[0]["title"], json!("first"));
    assert_eq!(all[0]["done"], json!(false));

    // ── subscribe: initial result, then a live update ───────────────────
    let mut sub = model.subscribe(&ModelQuery::default()).await.unwrap();
    let initial = next_result(&mut sub, "initial").await;
    assert_eq!(initial.instances.len(), 1);

    model
        .create(
            props(&[("title", json!("second"))]),
            CreateOptions::default(),
        )
        .await
        .unwrap();
    let updated = next_result(&mut sub, "second create").await;
    assert_eq!(updated.instances.len(), 2);

    // ── update + dirty save ──────────────────────────────────────────────
    model
        .update(&created.base_uri, props(&[("done", json!(true))]), None)
        .await
        .unwrap();
    let updated = next_result(&mut sub, "update").await;
    let row = updated
        .instances
        .iter()
        .find(|i| i["id"] == json!(created.base_uri))
        .unwrap();
    // The `true` round trip comes back as a bool too — both polarities of
    // the declared boolean property stay typed end to end.
    assert_eq!(row["done"], json!(true));
    assert!(row["done"].is_boolean());

    // ── dirty save on a collection (R6): `instanceUpdate` refuses
    // collections (`CollectionNotSettable`), so `Instance::save` diffs the
    // array against the snapshot and goes through the item-level
    // collection RPCs — proven by the writes landing at all. ────────────
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("title", json!("first-edited"));
    inst.set("tags", json!(["tag://a", "tag://b"]));
    inst.save(None).await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert_eq!(row["title"], json!("first-edited"));
    let mut tags: Vec<String> = row["tags"]
        .as_array()
        .expect("tags hydrates as an array")
        .iter()
        .map(|v| v.as_str().unwrap().to_string())
        .collect();
    tags.sort();
    assert_eq!(tags, ["tag://a", "tag://b"]);

    // A pure reorder is not even dirty — a collection is a set, so this
    // save is a no-op, not a rejected `instanceUpdate`.
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!(["tag://b", "tag://a"]));
    assert!(!inst.is_dirty());
    inst.save(None).await.unwrap();

    // Removal diffs item by item too: `tag://a` leaves the collection.
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!(["tag://b"]));
    inst.save(None).await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert_eq!(row["tags"], json!(["tag://b"]));

    // ── batch: two collection saves into one batch commit the LAST saved
    // value (N5). The second save's diff runs against the first save's
    // staged state, which the executor reads batch-aware. Both directions:
    // add then remove, remove then add. ─────────────────────────────────
    let tags_of = |row: &Value| -> Vec<String> {
        let mut tags: Vec<String> = row["tags"]
            .as_array()
            .map(|a| a.iter().map(|v| v.as_str().unwrap().to_string()).collect())
            .unwrap_or_default();
        tags.sort();
        tags
    };
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!([]));
    inst.save(None).await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert!(tags_of(&row).is_empty(), "start: no tags, got {row}");

    // [] -> save [a] into B -> save [] into B -> commit B -> []
    let batch = model.batch().await.unwrap();
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!(["tag://a"]));
    inst.save(Some(batch.id().to_string())).await.unwrap();
    inst.set("tags", json!([]));
    inst.save(Some(batch.id().to_string())).await.unwrap();
    batch.commit().await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert!(
        tags_of(&row).is_empty(),
        "add then remove in one batch must commit [], got {:?}",
        tags_of(&row)
    );

    // [a] committed -> save [] into B -> save [a] into B -> commit B -> [a]
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!(["tag://a"]));
    inst.save(None).await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert_eq!(tags_of(&row), ["tag://a"]);
    let batch = model.batch().await.unwrap();
    let mut inst = model.instance(created.base_uri.clone(), row.as_object().unwrap().clone());
    inst.set("tags", json!([]));
    inst.save(Some(batch.id().to_string())).await.unwrap();
    inst.set("tags", json!(["tag://a"]));
    inst.save(Some(batch.id().to_string())).await.unwrap();
    batch.commit().await.unwrap();
    let row = model.get(&created.base_uri, None).await.unwrap().unwrap();
    assert_eq!(
        tags_of(&row),
        ["tag://a"],
        "remove then add in one batch must commit [a]"
    );

    // ── batch: writes invisible until commit ─────────────────────────────
    let batch = model.batch().await.unwrap();
    model
        .create(
            props(&[("title", json!("batched"))]),
            CreateOptions {
                batch_id: Some(batch.id().to_string()),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    // The batch write is not in the live graph yet.
    let during = model.find_all(&ModelQuery::default()).await.unwrap();
    assert_eq!(during.len(), 2);
    batch.commit().await.unwrap();
    let after = model.find_all(&ModelQuery::default()).await.unwrap();
    assert_eq!(after.len(), 3);
    let _ = next_result(&mut sub, "commit").await;

    // ── batch: a failed write is not rolled back per call. The caller
    // discards the whole batch: the good write staged before it never
    // lands, and a second discard of the same batch is refused (`false`
    // from the executor, an error here). ──────────────────────────────────
    let batch = model.batch().await.unwrap();
    let in_batch = || CreateOptions {
        batch_id: Some(batch.id().to_string()),
        ..Default::default()
    };
    model
        .create(props(&[("title", json!("staged"))]), in_batch())
        .await
        .unwrap();
    // `title` is required: this write fails inside the batch.
    assert!(model
        .create(props(&[("done", json!(true))]), in_batch())
        .await
        .is_err());
    batch.discard().await.unwrap();
    let after = model.find_all(&ModelQuery::default()).await.unwrap();
    assert_eq!(
        after.len(),
        3,
        "a discarded batch must leave nothing behind"
    );
    assert!(after.iter().all(|r| r["title"] != json!("staged")));
    assert!(
        batch.discard().await.is_err(),
        "discarding a batch the executor no longer holds must fail"
    );

    // ── outage: another client writes while this one is offline. The
    // relay holds the client disconnected; a second client, connected
    // straight to the executor, creates a row; only then does the relay
    // come back. The client's re-opened subscription must deliver that
    // row: nothing pushed it while the client was away, so only the
    // re-open's fresh result can carry it. ────────────────────────────────
    let before_outage = sub.id().await;
    assert!(before_outage.is_some());
    relay.set_online(false);
    wait_until(30, "client never noticed the outage", || {
        !client.ws().is_connected()
    })
    .await;

    let writer = connect(&executor.url).await;
    Model::new(writer.ws(), uuid.clone(), "IntegrationNote")
        .create(
            props(&[("title", json!("written-while-offline"))]),
            CreateOptions::default(),
        )
        .await
        .unwrap();
    assert!(
        !client.ws().is_connected(),
        "the client must still be offline when the other client's row lands"
    );
    writer.ws().close().await;

    relay.set_online(true);
    wait_until(60, "client did not reconnect after the outage", || {
        client.ws().is_connected()
    })
    .await;
    let deadline = std::time::Instant::now() + Duration::from_secs(30);
    loop {
        match sub.id().await {
            Some(id) if Some(&id) != before_outage.as_ref() => break,
            _ => {
                assert!(
                    std::time::Instant::now() < deadline,
                    "subscription did not re-open after the outage"
                );
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
    // An update still unread from before the outage can come out of the
    // stream ahead of the re-open's snapshot: read on until the snapshot
    // with the offline row arrives.
    next_result_where(
        &mut sub,
        "the re-opened subscription's result with the row written during the outage",
        |r| {
            r.instances.len() == 4
                && r.instances
                    .iter()
                    .any(|i| i["title"] == json!("written-while-offline"))
        },
    )
    .await;

    // ── reconnect: write through the gap. `call` waits for a socket, so
    // the create lands once the client reconnects; the re-opened
    // subscription's fresh full result (now 5 rows) is not deduped — the
    // subscriber sees the resync, not a silent gap. The executor-side id
    // also changes: a re-open mints a new subscription. ───────────────────
    let old_sub_id = sub.id().await;
    assert!(old_sub_id.is_some());
    client.ws().disconnect_for_testing().await;

    let ws2 = client.ws().clone();
    let uuid2 = uuid.clone();
    let write = tokio::spawn(async move {
        Model::new(&ws2, uuid2, "IntegrationNote")
            .create(
                props(&[("title", json!("fourth"))]),
                CreateOptions::default(),
            )
            .await
    });

    // Wait for the socket to come back.
    let deadline = std::time::Instant::now() + Duration::from_secs(60);
    while !client.ws().is_connected() {
        assert!(
            std::time::Instant::now() < deadline,
            "client did not reconnect"
        );
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    write.await.unwrap().unwrap();

    // The re-opened subscription has a NEW executor-side id.
    let deadline = std::time::Instant::now() + Duration::from_secs(30);
    loop {
        match sub.id().await {
            Some(id) if Some(&id) != old_sub_id.as_ref() => break,
            _ => {
                assert!(
                    std::time::Instant::now() < deadline,
                    "subscription did not re-open after reconnect"
                );
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
    next_result_where(&mut sub, "post-reconnect result with 5 rows", |r| {
        r.instances.len() == 5
    })
    .await;
    sub.dispose().await;

    // ── flow: register, start, propose, fire ─────────────────────────────
    let api = FlowApi::new(client.ws(), uuid.clone());
    let mut flow = ShaclFlow::new("Delivery", "coasys://a4test/");
    flow.states = vec![
        FlowState {
            name: "Ordered".into(),
            value: 0.0,
            ..Default::default()
        },
        FlowState {
            name: "Shipped".into(),
            value: 1.0,
            ..Default::default()
        },
    ];
    flow.transitions = vec![FlowTransition {
        action_name: "Ship".into(),
        from_state: "Ordered".into(),
        to_state: "Shipped".into(),
        actions: vec![],
    }];
    api.add_flow("Delivery", &flow).await.unwrap();

    let instance = FlowInstance::start(&api, "Delivery", "ad4m://note-1")
        .await
        .unwrap();
    assert_eq!(instance.current_state_name(), Some("Ordered"));
    assert_eq!(instance.available_transitions().len(), 1);

    // {n:1} quorum — the proposal fires the transition at once.
    let outcome = instance
        .propose_transition("Shipped", None, None)
        .await
        .unwrap();
    assert!(outcome.recorded_vote);
    assert_eq!(outcome.derived_state, "Shipped");
    assert!(!outcome.outcomes.is_empty(), "the n:1 proposal must fire");

    let states = api
        .flow_states(Some(&[instance.uri().to_string()]))
        .await
        .unwrap();
    assert_eq!(states[0].current_state.as_deref(), Some("Shipped"));

    // findAll sees the run.
    let runs = FlowInstance::find_all(&api, None).await.unwrap();
    assert_eq!(runs.len(), 1);
    assert_eq!(runs[0].subject(), "ad4m://note-1");

    client.ws().close().await;
    drop(executor);
}
