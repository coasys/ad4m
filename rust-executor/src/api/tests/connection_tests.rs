//! The RPC socket's per-connection loop (`ws_rpc::serve`), driven through
//! channels in place of a WebSocket: client messages in, replies and events
//! out, with the real event stream on the global pubsub.

use std::sync::Arc;
use std::time::Duration;

use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::api::events_ws::build_event_stream_for;
use crate::api::tests::protocol_v2_tests::admin_ctx;
use crate::api::ws_handler::{build_handler_map, HandlerMap};
use crate::api::ws_rpc::{serve, Connection};
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC};

const DID: &str = "did:key:alice";

struct Socket {
    input: Option<mpsc::UnboundedSender<String>>,
    out: mpsc::UnboundedReceiver<String>,
    served: tokio::task::JoinHandle<()>,
}

impl Socket {
    async fn open(handler_map: HandlerMap) -> Self {
        let (tx, out) = mpsc::unbounded_channel();
        let (input, incoming) = mpsc::unbounded_channel();
        let events = build_event_stream_for(String::new(), Some(DID.into()), None, false).await;
        let conn = Connection::new(Arc::new(handler_map), admin_ctx(), String::new(), tx);
        let served = tokio::spawn(serve(conn, UnboundedReceiverStream::new(incoming), events));
        Self {
            input: Some(input),
            out,
            served,
        }
    }

    fn send(&self, msg: Value) {
        self.input.as_ref().unwrap().send(msg.to_string()).unwrap();
    }

    /// Next message sent to the client.
    async fn next(&mut self) -> Value {
        let msg = tokio::time::timeout(Duration::from_secs(10), self.out.recv())
            .await
            .expect("a message within 10 s")
            .expect("the connection is open");
        serde_json::from_str(&msg).unwrap()
    }

    /// Next message that is not an unrelated event from a parallel test.
    async fn next_of(&mut self, keep: impl Fn(&Value) -> bool) -> Value {
        loop {
            let msg = self.next().await;
            if keep(&msg) {
                return msg;
            }
        }
    }

    async fn reply(&mut self, id: &str) -> Value {
        self.next_of(|m| m["id"] == id).await
    }

    /// Perspectives of this run's link events received within 300 ms.
    async fn link_events(&mut self, run: &str) -> Vec<String> {
        let mut seen = vec![];
        let _ = tokio::time::timeout(Duration::from_millis(300), async {
            while let Some(msg) = self.out.recv().await {
                let v: Value = serde_json::from_str(&msg).unwrap();
                if v["type"] == "link-added" && v["link"]["data"]["source"] == run {
                    seen.push(v["perspectiveUuid"].as_str().unwrap().to_string());
                }
            }
        })
        .await;
        seen.sort();
        seen
    }

    /// Close the client side of the socket.
    fn close(&mut self) {
        self.input = None;
    }
}

async fn publish_link(perspective: &str, run: &str) {
    let event = json!({
        "perspectiveUuid": perspective,
        "owner": DID,
        "link": { "author": DID, "timestamp": run,
                  "data": { "source": run, "predicate": null, "target": "t" } }
    });
    get_global_pubsub()
        .await
        .publish(&PERSPECTIVE_LINK_ADDED_TOPIC, &event.to_string())
        .await;
}

async fn publish_both(a: &str, b: &str, run: &str) {
    publish_link(a, run).await;
    publish_link(b, run).await;
}

#[tokio::test]
async fn watch_limits_link_events_and_unwatch_restores_them() {
    let run = uuid::Uuid::new_v4().to_string();
    let (a, b) = (format!("A-{run}"), format!("B-{run}"));
    let mut socket = Socket::open(build_handler_map()).await;

    publish_both(&a, &b, &run).await;
    assert_eq!(
        socket.link_events(&run).await,
        vec![a.clone(), b.clone()],
        "no watch: every event"
    );

    socket.send(json!({ "id": "w", "type": "events.watch", "params": { "perspectives": [a] } }));
    let reply = socket.reply("w").await;
    assert_eq!(
        reply["result"]["watching"],
        json!({ "types": null, "perspectives": [a] })
    );
    publish_both(&a, &b, &run).await;
    assert_eq!(socket.link_events(&run).await, vec![a.clone()]);

    socket.send(json!({ "id": "u", "type": "events.unwatch" }));
    assert_eq!(
        socket.reply("u").await,
        json!({ "id": "u", "result": { "watching": null } })
    );
    publish_both(&a, &b, &run).await;
    assert_eq!(socket.link_events(&run).await, vec![a, b]);
}

#[tokio::test]
async fn malformed_watch_replies_400_and_keeps_the_old_interest() {
    let run = uuid::Uuid::new_v4().to_string();
    let (a, b) = (format!("A-{run}"), format!("B-{run}"));
    let mut socket = Socket::open(build_handler_map()).await;

    socket.send(json!({ "id": "w1", "type": "events.watch", "params": { "perspectives": [a] } }));
    socket.reply("w1").await;
    socket.send(json!({ "id": "w2", "type": "events.watch", "params": { "perspectives": "B" } }));
    let reply = socket.reply("w2").await;
    assert_eq!(reply["error"]["code"], json!(400));
    assert!(reply.get("result").is_none());

    publish_both(&a, &b, &run).await;
    assert_eq!(socket.link_events(&run).await, vec![a], "still watching A");
}

#[tokio::test]
async fn closing_the_socket_ends_the_event_task() {
    let mut socket = Socket::open(build_handler_map()).await;
    socket.send(json!({ "type": "ping" }));
    assert_eq!(
        socket.next_of(|m| m["type"] == "pong").await,
        json!({ "type": "pong" })
    );

    socket.close();
    tokio::time::timeout(Duration::from_secs(5), &mut socket.served)
        .await
        .expect("serve returns once the socket closes")
        .unwrap();
    // Every sender is gone, the event task's included: the channel closes
    // even though the event stream itself never ends.
    let rest = tokio::time::timeout(Duration::from_secs(5), async {
        while socket.out.recv().await.is_some() {}
    })
    .await;
    assert!(
        rest.is_ok(),
        "the event task still holds the socket's sender"
    );
}

// ── X8: operation handles on the socket ─────────────────────────────────────

/// `agent.unlock` answers at once; `ai.prompt` runs until cancelled. Both
/// are in `ASYNC_METHODS`, so `{ async: true }` makes them operations.
fn operation_handlers() -> HandlerMap {
    let mut map = HandlerMap::new();
    map.register("agent.unlock", |_params, _ctx| async { Ok(json!("done")) });
    map.register("ai.prompt", |_params, _ctx| async {
        tokio::time::sleep(Duration::from_secs(300)).await;
        Ok(json!("too late"))
    });
    map
}

fn is_completion(msg: &Value) -> bool {
    msg["type"] == "operation-completed"
}

#[tokio::test]
async fn the_ack_arrives_before_the_completion() {
    let mut socket = Socket::open(operation_handlers()).await;
    socket.send(json!({ "id": "r1", "type": "agent.unlock", "params": { "async": true } }));
    let first = socket
        .next_of(|m| m["id"] == "r1" || is_completion(m))
        .await;
    assert_eq!(first["id"], json!("r1"), "the ack comes first: {first}");
    let operation_id = first["result"]["operationId"].clone();
    assert!(operation_id.is_string());
    let done = socket.next_of(is_completion).await;
    assert_eq!(
        done,
        json!({ "type": "operation-completed", "operationId": operation_id, "result": "done" })
    );
}

#[tokio::test]
async fn cancelling_an_operation_right_after_the_ack_completes_it_with_499() {
    let mut socket = Socket::open(operation_handlers()).await;
    socket.send(json!({ "id": "p1", "type": "ai.prompt", "params": { "async": true } }));
    let operation_id = socket.reply("p1").await["result"]["operationId"].clone();

    socket.send(
        json!({ "id": "c1", "type": "request.cancel", "params": { "targetId": operation_id } }),
    );
    assert_eq!(
        socket.reply("c1").await["result"],
        json!({ "cancelled": true, "targetId": operation_id })
    );
    let done = socket.next_of(is_completion).await;
    assert_eq!(done["operationId"], operation_id);
    assert_eq!(done["error"]["code"], json!(499));
}

#[tokio::test]
async fn a_watch_does_not_filter_operation_completions() {
    let mut socket = Socket::open(operation_handlers()).await;
    socket.send(json!({ "id": "w", "type": "events.watch", "params": { "types": [] } }));
    socket.reply("w").await;
    socket.send(json!({ "id": "r2", "type": "agent.unlock", "params": { "async": true } }));
    let operation_id = socket.reply("r2").await["result"]["operationId"].clone();
    let done = socket.next_of(is_completion).await;
    assert_eq!(done["operationId"], operation_id);
    assert_eq!(done["result"], json!("done"));
}
