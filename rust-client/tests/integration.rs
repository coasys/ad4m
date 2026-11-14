//! Real-executor integration coverage for the A4 client surface: `Model`
//! CRUD + subscriptions, batches, reconnect resync, and `FlowInstance`.
//!
//! Spawns the `ad4m-executor` binary like `tests/js`' `spawnExecutor` does:
//! `EXECUTOR_PATH` if set, else `<target dir>/release/ad4m-executor` under
//! `CARGO_TARGET_DIR` or the workspace `target/`. Skips (prints and passes)
//! when no binary exists — build one with `cargo build --release -p
//! ad4m-executor` to run it.

use ad4m_client::flow::{FlowApi, FlowInstance, FlowState, FlowTransition, ShaclFlow};
use ad4m_client::model::{CreateOptions, Model, ModelQuery};
use ad4m_client::Ad4mClient;
use serde_json::{json, Map, Value};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

fn executor_binary() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("EXECUTOR_PATH") {
        let p = PathBuf::from(p);
        return p.exists().then_some(p);
    }
    let mut candidates = Vec::new();
    if let Ok(dir) = std::env::var("CARGO_TARGET_DIR") {
        candidates.push(PathBuf::from(dir).join("release/ad4m-executor"));
    }
    // Workspace root = rust-client/..
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    candidates.push(root.join("target/release/ad4m-executor"));
    candidates.into_iter().find(|p| p.exists())
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
        _data: data,
    })
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
/// `xsd://boolean` `done` — the smallest shape `perspective.addSdna`
/// accepts, modelled on the hardwired `flow_instance.json`.
const NOTE_SDNA: &str = r#"{
  "target_class": "ad4m://IntegrationNote",
  "constructor_actions": [
    {"action": "addLink", "source": "this", "predicate": "ad4m://note/title", "target": "placeholder"}
  ],
  "properties": [
    {"path": "ad4m://note/title", "name": "title", "identity": true, "min_count": 1, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/title", "target": "value"}]},
    {"path": "ad4m://note/done", "name": "done", "datatype": "xsd://boolean", "min_count": 0, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/done", "target": "value"}]}
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

#[tokio::test]
async fn model_batch_subscription_reconnect_and_flow() {
    let Some(executor) = start_executor() else {
        eprintln!("SKIP: no ad4m-executor binary (set EXECUTOR_PATH or build --release)");
        return;
    };
    let client = connect(&executor.url).await;
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
            "perspective.addSdna",
            json!({
                "uuid": uuid,
                "name": "IntegrationNote",
                "sdnaCode": "",
                "sdnaType": "subject_class",
                "shaclJson": NOTE_SDNA,
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

    // ── reconnect: write through the gap. `call` waits for a socket, so
    // the create lands once the client reconnects; the re-opened
    // subscription's fresh full result (now 4 rows) is not deduped — the
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
    let updated = next_result(&mut sub, "post-reconnect resync").await;
    assert_eq!(updated.instances.len(), 4);
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
