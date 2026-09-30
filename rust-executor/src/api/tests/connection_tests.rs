//! The RPC socket's per-connection loop (`ws_rpc::serve`), driven through
//! channels in place of a WebSocket: client messages in, replies and events
//! out, with the real event stream on the global pubsub.

use std::sync::Arc;
use std::time::Duration;

use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::api::events_ws::build_event_stream_for;
use crate::api::tests::protocol_tests::{admin_ctx, registered_perspective};
use crate::api::ws_handler::build_handler_map;
use crate::api::ws_rpc::{serve, Connection};
use crate::pubsub::{
    get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
};

const DID: &str = "did:key:alice";

struct Socket {
    input: Option<mpsc::UnboundedSender<String>>,
    out: mpsc::UnboundedReceiver<String>,
    served: tokio::task::JoinHandle<()>,
}

impl Socket {
    async fn open() -> Self {
        let (tx, out) = mpsc::unbounded_channel();
        let (input, incoming) = mpsc::unbounded_channel();
        let events = build_event_stream_for(String::new(), Some(DID.into()), None, false).await;
        let conn = Connection::new(
            Arc::new(build_handler_map()),
            admin_ctx(),
            String::new(),
            tx,
        );
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

    /// Close the client side of the socket and wait until it is served.
    async fn close(&mut self) {
        self.input = None;
        tokio::time::timeout(Duration::from_secs(5), &mut self.served)
            .await
            .expect("serve returns once the socket closes")
            .unwrap();
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
async fn a_socket_gets_only_the_events_it_watches() {
    let run = uuid::Uuid::new_v4().to_string();
    let (a, b) = (format!("A-{run}"), format!("B-{run}"));
    let mut socket = Socket::open().await;

    publish_both(&a, &b, &run).await;
    assert!(
        socket.link_events(&run).await.is_empty(),
        "no watch: no events"
    );

    socket.send(json!({ "id": "w", "type": "events.watch", "params": { "link-added": [a] } }));
    assert_eq!(
        socket.reply("w").await,
        json!({ "id": "w", "result": true })
    );
    publish_both(&a, &b, &run).await;
    assert_eq!(socket.link_events(&run).await, vec![a.clone()]);

    socket.send(json!({ "id": "u", "type": "events.unwatch" }));
    socket.reply("u").await;
    publish_both(&a, &b, &run).await;
    assert!(socket.link_events(&run).await.is_empty(), "unwatched");
}

#[tokio::test]
async fn closing_the_socket_ends_the_event_task() {
    let mut socket = Socket::open().await;
    socket.send(json!({ "type": "ping" }));
    assert_eq!(
        socket.next_of(|m| m["type"] == "pong").await,
        json!({ "type": "pong" })
    );

    socket.close().await;
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

#[tokio::test]
async fn query_updates_reach_a_socket_without_a_watch() {
    // A live query update as the perspective publishes it: every socket of
    // the owner gets it, whatever the socket watches.
    let p = registered_perspective(&[]).await;
    let mut socket = Socket::open().await;
    socket.send(json!({ "id": "w", "type": "events.watch", "params": { "agent-updated": null } }));
    socket.reply("w").await;

    let sub = uuid::Uuid::new_v4().to_string();
    let update = json!({ "uuid": p.0, "subscriptionId": sub, "result": "[]" });
    get_global_pubsub()
        .await
        .publish(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC, &update.to_string())
        .await;

    let got = socket.next_of(|m| m["subscriptionId"] == sub).await;
    assert_eq!(
        got,
        json!({ "type": "query-subscription-update", "uuid": p.0, "subscriptionId": sub, "result": "[]" })
    );
}
