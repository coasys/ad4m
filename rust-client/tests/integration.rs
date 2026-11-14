//! Real-executor integration coverage for the A4 client surface: `Model`
//! CRUD + subscriptions, batches, reconnect resync, and `FlowInstance`.
//!
//! The spawn/connect harness lives in `tests/common/`. The suite skips
//! (prints and passes) when no binary exists — unless
//! `AD4M_REQUIRE_EXECUTOR=1`, which makes a missing binary fail. Build one
//! with `cargo build --release -p ad4m-executor` to run it.

mod common;

use ad4m_client::flow::{FlowApi, FlowInstance, FlowState, FlowTransition, ShaclFlow};
use ad4m_client::model::{CreateOptions, Model, ModelQuery};
use serde_json::{json, Map, Value};
use std::time::Duration;

use common::{connect, start_executor};

/// The test class: one required single-valued `title`, one optional `done`
/// — the smallest shape `perspective.addSdna` accepts, modelled on the
/// hardwired `flow_instance.json`.
const NOTE_SDNA: &str = r#"{
  "target_class": "ad4m://IntegrationNote",
  "constructor_actions": [
    {"action": "addLink", "source": "this", "predicate": "ad4m://note/title", "target": "placeholder"}
  ],
  "properties": [
    {"path": "ad4m://note/title", "name": "title", "identity": true, "min_count": 1, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/title", "target": "value"}]},
    {"path": "ad4m://note/done", "name": "done", "min_count": 0, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "ad4m://note/done", "target": "value"}]}
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
        return; // require_binary() printed the skip
    };
    let client = connect(&executor.url, "").await;
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
    let all = model.find_all(&ModelQuery::default()).await.unwrap();
    assert_eq!(all.len(), 1);
    assert_eq!(all[0]["title"], json!("first"));

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
    // Hydrated values come back as their literal strings.
    assert_eq!(row["done"], json!("true"));

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
