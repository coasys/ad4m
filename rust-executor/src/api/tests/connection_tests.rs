//! The RPC socket's per-connection loop (`ws_rpc::serve`), driven through
//! channels in place of a WebSocket: client messages in, replies and events
//! out, with the real event stream on the global pubsub.

use std::sync::Arc;
use std::time::Duration;

use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::api::events_ws::build_event_stream_for;
use crate::api::tests::protocol_tests::{admin_conn_ctx, registered_perspective};
use crate::api::ws_handler::{build_handler_map, HandlerMap};
use crate::api::ws_rpc::{serve, Connection};
use crate::pubsub::{
    get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
};

const DID: &str = "did:key:alice";
const QUERY: &str = "SELECT ?s WHERE { ?s ?p ?o }";

struct Socket {
    connection_id: String,
    input: Option<mpsc::UnboundedSender<String>>,
    out: mpsc::UnboundedReceiver<String>,
    served: tokio::task::JoinHandle<()>,
}

impl Socket {
    async fn open() -> Self {
        Self::open_with(build_handler_map()).await
    }

    async fn open_with(handlers: HandlerMap) -> Self {
        let connection_id = uuid::Uuid::new_v4().to_string();
        let (tx, out) = mpsc::unbounded_channel();
        let (input, incoming) = mpsc::unbounded_channel();
        let events = build_event_stream_for(
            String::new(),
            Some(DID.into()),
            None,
            false,
            Some(connection_id.clone()),
        )
        .await;
        let ctx = admin_conn_ctx(&connection_id);
        let conn = Connection::new(Arc::new(handlers), ctx, String::new(), tx);
        let served = tokio::spawn(serve(conn, UnboundedReceiverStream::new(incoming), events));
        Self {
            connection_id,
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

    /// Open a live query on `perspective`; returns its subscription id.
    async fn subscribe(&mut self, perspective: &str) -> String {
        let id = uuid::Uuid::new_v4().to_string();
        self.send(json!({ "id": id, "type": "perspective.subscribeQuery",
            "params": { "uuid": perspective, "query": QUERY } }));
        let reply = self.reply(&id).await;
        reply["result"]["subscriptionId"]
            .as_str()
            .unwrap()
            .to_string()
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
async fn closing_the_socket_ends_its_subscriptions_only() {
    let p = registered_perspective(&[]).await;
    let (mut a, mut b) = (Socket::open().await, Socket::open().await);
    let sub_a = a.subscribe(&p.0).await;
    let sub_b = b.subscribe(&p.0).await;

    a.close().await;
    let perspective = crate::perspectives::get_perspective(&p.0).unwrap();
    assert!(perspective
        .subscription_state(&sub_a, &a.connection_id)
        .await
        .is_none());
    assert!(
        perspective
            .subscription_state(&sub_b, &b.connection_id)
            .await
            .is_some(),
        "the other connection keeps its subscription"
    );
}

/// Poll until `subscription` of `connection` is gone, for up to 2 s.
async fn ended(perspective: &str, subscription: &str, connection: &str) -> bool {
    let perspective = crate::perspectives::get_perspective(perspective).unwrap();
    for _ in 0..40 {
        if perspective
            .subscription_state(subscription, connection)
            .await
            .is_none()
        {
            return true;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    false
}

#[tokio::test]
async fn a_closed_socket_ends_its_subscriptions_without_waiting_for_calls() {
    let p = registered_perspective(&[]).await;
    let (block, late) = (
        Arc::new(tokio::sync::Notify::new()),
        Arc::new(tokio::sync::Notify::new()),
    );
    let late_id = Arc::new(std::sync::Mutex::new(None::<String>));
    let mut handlers = build_handler_map();
    let b = block.clone();
    handlers.register("test.block", move |_, _| {
        let b = b.clone();
        async move {
            b.notified().await;
            Ok(json!(true))
        }
    });
    let (l, slot, uuid) = (late.clone(), late_id.clone(), p.0.clone());
    handlers.register("test.subscribeLate", move |_, ctx| {
        let (l, slot, uuid) = (l.clone(), slot.clone(), uuid.clone());
        async move {
            l.notified().await;
            let (id, _, _) = crate::perspectives::get_perspective(&uuid)
                .unwrap()
                .subscribe_and_query(QUERY.into(), None, ctx.connection_id.clone().unwrap())
                .await
                .unwrap();
            *slot.lock().unwrap() = Some(id);
            Ok(json!(true))
        }
    });

    let mut socket = Socket::open_with(handlers).await;
    let early = socket.subscribe(&p.0).await;
    socket.send(json!({ "id": "b", "type": "test.block" }));
    socket.send(json!({ "id": "l", "type": "test.subscribeLate" }));
    socket.input = None;

    assert!(
        ended(&p.0, &early, &socket.connection_id).await,
        "a live query outlived its socket while a call was in flight"
    );
    late.notify_one();
    let late_sub = loop {
        if let Some(id) = late_id.lock().unwrap().clone() {
            break id;
        }
        tokio::time::sleep(Duration::from_millis(10)).await;
    };
    assert!(
        ended(&p.0, &late_sub, &socket.connection_id).await,
        "a subscribe that completed after the close kept its live query"
    );

    block.notify_one();
    socket.close().await;
}

#[tokio::test]
async fn query_updates_reach_only_their_connection() {
    let (mut a, mut b) = (Socket::open().await, Socket::open().await);
    let sub = uuid::Uuid::new_v4().to_string();
    let update = json!({ "uuid": "p", "subscriptionId": sub, "revision": 1,
        "added": [], "removed": [], "changed": [], "connectionId": a.connection_id });
    get_global_pubsub()
        .await
        .publish(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC, &update.to_string())
        .await;

    let got = a.next_of(|m| m["subscriptionId"] == sub).await;
    assert_eq!(got["type"], json!("query-subscription-update"));
    assert!(got.get("connectionId").is_none(), "routing key stripped");
    let leaked = tokio::time::timeout(
        Duration::from_millis(300),
        b.next_of(|m| m["subscriptionId"] == sub),
    )
    .await;
    assert!(leaked.is_err(), "another connection got the update");
}
