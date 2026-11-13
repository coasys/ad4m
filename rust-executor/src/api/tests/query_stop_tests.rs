//! #1736: a slow SPARQL query must be stoppable.
//!
//! Each test runs an evaluation that is deliberately slow — a five-way cross
//! product over every triple in the store — and proves the *evaluation* ends
//! when its gate fires, not merely that the caller stopped waiting: the
//! test-only `eval_count` counter, incremented for the duration of every
//! `query_internal` eval, must return to zero within a second of the stop.
//!
//! Covered here: the configured deadline (504 over the wire), `request.cancel`
//! on `perspective.subscribeQuery` (499), the socket closing, and a dropped
//! call future (the gate's own drop).

use std::sync::Arc;
use std::time::Duration;

use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::agent::capabilities::get_user_default_capabilities;
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::api::events_ws::build_event_stream;
use crate::api::tests::support::{admin_ctx, registered_perspective};
use crate::api::ws_handler::build_handler_map;
use crate::api::ws_rpc::{serve, Connection};
use crate::config::{get_global_config, set_global_config};
use crate::perspectives::sparql_store::eval_count;
use crate::types::{Link, LinkStatus};

/// Slow on purpose: a nested `FILTER EXISTS` re-scans every quad in the
/// store for each quad of each outer scan — cubic in the quad count, so a
/// few hundred links keep it busy for minutes. It stays stoppable because
/// every quad read of every scan checks the oxigraph cancellation token
/// (a cross product would not: spareval materialises the join's build side
/// and enumerates it in memory, past the token's reach).
const SLOW: &str = "SELECT ?a ?o WHERE { \
    ?a ?p ?o . \
    FILTER EXISTS { \
        ?b ?q ?r . \
        FILTER EXISTS { ?c ?q2 ?r2 . FILTER(STR(?c) = \"urn:no-match\") } \
    } \
}";

const DID: &str = "did:key:querier";

fn listener(did: &str) -> Arc<Session> {
    let user = Principal::Agent(AgentContext::managed(did.to_string()));
    Arc::new(
        Session::for_test(user)
            .with_caps(get_user_default_capabilities())
            .with_did(did),
    )
}

struct Socket {
    input: Option<mpsc::UnboundedSender<String>>,
    out: mpsc::UnboundedReceiver<String>,
    served: tokio::task::JoinHandle<()>,
}

impl Socket {
    async fn open() -> Self {
        let connection_id = uuid::Uuid::new_v4().to_string();
        let ctx = Arc::new(admin_ctx().as_ref().clone().on_connection(&connection_id));
        let (tx, out) = mpsc::unbounded_channel();
        let (input, incoming) = mpsc::unbounded_channel();
        let conn = Connection::new(Arc::new(build_handler_map()), ctx, tx);
        let events = build_event_stream(listener(DID), conn.interest(), Some(connection_id)).await;
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

    async fn next(&mut self) -> Value {
        let msg = tokio::time::timeout(Duration::from_secs(20), self.out.recv())
            .await
            .expect("a message within 20 s")
            .expect("the connection is open");
        serde_json::from_str(&msg).unwrap()
    }

    /// Next message matching `id` (skipping events and other replies).
    async fn reply(&mut self, id: &str) -> Value {
        loop {
            let msg = self.next().await;
            if msg["id"] == id {
                return msg;
            }
        }
    }

    /// Close the client side and wait for the serve loop to end.
    async fn close(self) {
        drop(self.input);
        let _ = tokio::time::timeout(Duration::from_secs(10), self.served).await;
    }
}

/// A perspective seeded with `n` links, registered in the global registry so
/// the WS handlers find it by uuid.
async fn seeded_perspective(n: usize) -> (crate::api::tests::support::Registered, String) {
    let registered = registered_perspective(&[]).await;
    let uuid = registered.0.clone();
    let mut perspective = crate::perspectives::get_perspective(&uuid).unwrap();
    let ctx = AgentContext::main_agent();
    let links: Vec<Link> = (0..n)
        .map(|i| Link {
            source: format!("test://src/{i}"),
            predicate: Some("test://p".to_string()),
            target: format!("test://tgt/{i}"),
        })
        .collect();
    perspective
        .add_links(links, LinkStatus::Local, None, &ctx)
        .await
        .expect("seed links");
    (registered, uuid)
}

/// Waits until `eval_count::running()` reaches `n` (test evals are counted in
/// `query_internal`). Panics after ~10 s.
async fn running_to_be(n: usize) {
    tokio::time::timeout(Duration::from_secs(10), async {
        while eval_count::running() != n {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap_or_else(|_| panic!("expected {n} running evals, got {}", eval_count::running()));
}

/// The evaluation must actually end, not just be unanswered: the counter
/// reaches zero within a second of the stop.
async fn evals_all_ended() {
    tokio::time::timeout(Duration::from_secs(1), async {
        while eval_count::running() != 0 {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap_or_else(|_| {
        panic!(
            "{} evals still running after the stop",
            eval_count::running()
        )
    });
}

/// Restores the global config on drop — a `query_deadline_secs` override must
/// not leak into later tests.
struct ConfigGuard;
impl ConfigGuard {
    fn deadline(secs: u64) -> Self {
        let mut config = get_global_config();
        config.query_deadline_secs = Some(secs);
        set_global_config(config);
        ConfigGuard
    }
}
impl Drop for ConfigGuard {
    fn drop(&mut self) {
        let mut config = get_global_config();
        config.query_deadline_secs = None;
        set_global_config(config);
    }
}

fn query_call(id: &str, uuid: &str, query: &str) -> Value {
    json!({
        "id": id,
        "type": "perspective.querySparql",
        "params": {"uuid": uuid, "query": query}
    })
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn deadline_stops_the_evaluation_itself() {
    let _registered = seeded_perspective(150).await.0;
    let uuid = _registered.0.clone();
    let _deadline = ConfigGuard::deadline(1);
    let mut socket = Socket::open().await;
    eval_count::reset();

    socket.send(query_call("q1", &uuid, SLOW));
    running_to_be(1).await;

    let reply = socket.reply("q1").await;
    assert_eq!(
        reply["error"]["code"].as_i64(),
        Some(504),
        "a deadline stop is a 504 naming the deadline: {reply}"
    );
    assert!(
        reply["error"]["message"]
            .as_str()
            .is_some_and(|m| m.contains("deadline")),
        "the error names the deadline: {reply}"
    );
    evals_all_ended().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn request_cancel_stops_a_subscribe_evaluation() {
    let (_registered, uuid) = seeded_perspective(150).await;
    let mut socket = Socket::open().await;
    eval_count::reset();

    socket.send(json!({
        "id": "s1",
        "type": "perspective.subscribeQuery",
        "params": {
            "uuid": uuid,
            "query": SLOW,
            "subscriptionId": "sub-1"
        }
    }));
    running_to_be(1).await;

    socket.send(json!({
        "id": "c1",
        "type": "request.cancel",
        "params": {"targetId": "s1"}
    }));

    // The cancel is acknowledged, and the subscribe call itself reports 499.
    let cancel_reply = socket.reply("c1").await;
    assert_eq!(cancel_reply["result"]["cancelled"], json!(true));
    let call_reply = socket.reply("s1").await;
    assert_eq!(
        call_reply["error"]["code"].as_i64(),
        Some(499),
        "{call_reply}"
    );

    // The evaluation itself ended, within a second of the cancel.
    evals_all_ended().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn socket_close_stops_the_evaluation() {
    let (_registered, uuid) = seeded_perspective(150).await;
    let socket = Socket::open().await;
    eval_count::reset();

    socket.send(query_call("q1", &uuid, SLOW));
    running_to_be(1).await;

    socket.close().await;
    evals_all_ended().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn dropped_gate_stops_the_evaluation() {
    let (registered, _uuid) = seeded_perspective(150).await;
    eval_count::reset();

    // A gate with no request behind it: its deadline is far off, so only its
    // drop (the call's future ending) can fire the stop.
    let gate = crate::perspectives::sparql_store::EvalGate::request(Duration::from_secs(600), None);
    let stop = gate.stop();
    let perspective = crate::perspectives::get_perspective(&registered.0).unwrap();
    let eval = tokio::spawn(async move {
        perspective
            .sparql_query_cancellable(stop, SLOW.to_string())
            .await
    });
    running_to_be(1).await;

    drop(gate);
    evals_all_ended().await;
    let _ = eval.await;
}
