//! End-to-end tests of Phase 0 (SPEC §15): a test-only built-in service
//! with one method, one scoped event and one stream, reached through the
//! RPC socket's `call` and `events.watch`.

use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use super::builtin::{CallContext, EventEmitter, ServiceError, ServiceHealth, ServiceImplementation, StartContext};
use super::host::{host, ServiceHost};
use super::interface::{Risk, Selection};
use super::registry::{BuiltinManifest, Instancing, Requirement};
use super::schema_export::{InterfaceBuilder, MethodOptions};
use crate::agent::capabilities::types::Resource;
use crate::agent::capabilities::{defs::ALL_CAPABILITY, Capability};
use crate::api::events_ws::build_event_stream_for;
use crate::api::ws_handler::build_handler_map;
use crate::api::ws_rpc::{serve, Connection};
use crate::types::RequestContext;

// ── The test service ────────────────────────────────────────────────────────

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct SayParams {
    room: String,
    text: String,
}

#[derive(Serialize, JsonSchema)]
struct SayResult {
    text: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct CountParams {
    #[schemars(range(min = 1, max = 10))]
    to: u32,
    stream_id: String,
}

#[derive(Serialize, JsonSchema)]
struct CountResult {
    total: u32,
}

#[derive(Serialize, JsonSchema)]
struct Said {
    room: String,
    text: String,
}

#[derive(Serialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
struct CountTick {
    stream_id: String,
    n: u32,
}

/// The echo interface, from its Rust types.
fn echo_interface(author: &str) -> Value {
    InterfaceBuilder::new("echo", author, "1.0.0", Selection::PerUser, "Test service.")
        .action("SAY", "Say things", "Echo text back.", Risk::Safe)
        .method::<SayParams, SayResult>(
            "say",
            "SAY",
            "Say `text` in `room`; everyone watching the room sees it.",
            MethodOptions { read: true, errors: vec![("Muted", 409)], ..Default::default() },
        )
        .method::<CountParams, CountResult>(
            "count",
            "SAY",
            "Count from 1 to `to`, one `count-tick` per number.",
            MethodOptions { long: true, stream: Some("count-tick"), ..Default::default() },
        )
        .event::<Said>("said", "SAY", "Text said in a room.", Some("room"))
        .event::<CountTick>("count-tick", "SAY", "One number of a count.", Some("streamId"))
        .build()
}

#[derive(Default)]
struct Echo {
    events: tokio::sync::OnceCell<EventEmitter>,
}

#[async_trait]
impl ServiceImplementation for Echo {
    async fn start(&self, ctx: StartContext) -> Result<(), String> {
        self.events.set(ctx.events).map_err(|_| "started twice".to_string())
    }
    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }
    async fn health(&self) -> ServiceHealth {
        ServiceHealth::Running
    }
    async fn call(&self, method: &str, params: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        let events = self.events.get().ok_or_else(|| ServiceError::Unavailable("not started".into()))?;
        let owner = ctx.agent_did.clone().ok_or_else(|| ServiceError::Internal("no agent".into()))?;
        let emit = |e: &'static str, p: Value| {
            let (events, owner) = (events.clone(), owner.clone());
            async move { events.emit(e, &owner, p).await.map_err(ServiceError::Internal) }
        };
        match method {
            "say" => {
                let p: SayParams = serde_json::from_value(params).map_err(|e| ServiceError::Internal(e.to_string()))?;
                if p.text == "mute" {
                    return Err(ServiceError::Method {
                        name: "Muted".into(),
                        data: Some(json!({ "room": p.room })),
                        message: "the room is muted".into(),
                    });
                }
                if p.text == "bad result" {
                    return Ok(json!({ "nope": 1 }));
                }
                if p.text == "undeclared" {
                    return Err(ServiceError::method("Gone", "not declared"));
                }
                emit("said", serde_json::to_value(Said { room: p.room, text: p.text.clone() }).unwrap()).await?;
                Ok(serde_json::to_value(SayResult { text: p.text }).unwrap())
            }
            "count" => {
                let p: CountParams = serde_json::from_value(params).map_err(|e| ServiceError::Internal(e.to_string()))?;
                for n in 1..=p.to {
                    emit("count-tick", serde_json::to_value(CountTick { stream_id: p.stream_id.clone(), n }).unwrap()).await?;
                }
                Ok(serde_json::to_value(CountResult { total: p.to }).unwrap())
            }
            other => Err(ServiceError::Internal(format!("no method {}", other))),
        }
    }
}

fn manifest(name: &str, implements: Vec<String>, requires: Vec<Requirement>) -> BuiltinManifest {
    BuiltinManifest {
        name: name.into(),
        author: "did:key:z6MkTestAuthor".into(),
        module: None,
        previous: None,
        version: "1.0.0".into(),
        description: String::new(),
        implements,
        requires,
        instancing: Instancing::Shared,
        runtime: json!({ "kind": "builtin", "id": name }),
    }
}

/// Register the echo interface (under a unique author, so tests sharing the
/// global host do not collide) and start an implementation of it.
async fn echo_on(host: &Arc<ServiceHost>, author: &str) -> (String, String) {
    let doc = host.register_interface(echo_interface(author), None, true).unwrap();
    let implementation = host
        .register_builtin(manifest(&format!("echo-{}", author), vec![doc.hash.clone()], vec![]), Arc::new(Echo::default()))
        .unwrap();
    host.start(&implementation, json!({})).await.unwrap();
    (doc.hash.clone(), implementation)
}

fn grant(domain: String, can: &[&str]) -> Capability {
    Capability {
        with: Resource { domain, pointers: vec!["*".into()] },
        can: can.iter().map(|s| s.to_string()).collect(),
    }
}

fn ctx(did: &str, grants: Vec<Capability>) -> CallContext {
    CallContext {
        caller: super::builtin::Caller::App,
        origin: vec![],
        agent_did: Some(did.into()),
        user: None,
        grants: vec![grants],
        deadline: None,
    }
}

// ── Interface from Rust types ───────────────────────────────────────────────

/// The checked-in document must match the Rust types, so the contract
/// cannot change without the reviewed JSON changing. Regenerate with
/// `UPDATE_SERVICE_FIXTURES=1 cargo test --lib services::tests::echo_interface_is_current`.
#[test]
fn echo_interface_is_current() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/services/fixtures/echo.interface.json");
    let built = serde_json::to_string_pretty(&echo_interface("did:key:z6MkFixture")).unwrap() + "\n";
    if std::env::var("UPDATE_SERVICE_FIXTURES").is_ok() {
        std::fs::write(&path, &built).unwrap();
    }
    let committed = std::fs::read_to_string(&path).expect("fixture exists");
    assert_eq!(committed, built, "echo.interface.json is stale; see the test's doc comment");
    // And the built document is a valid interface.
    super::interface::InterfaceDocument::parse(serde_json::from_str(&committed).unwrap()).unwrap();
}

// ── Host dispatch ───────────────────────────────────────────────────────────

#[tokio::test]
async fn dispatch_answers_with_protocol_codes() {
    let host = ServiceHost::new();
    let (iface, implementation) = echo_on(&host, "did:key:z6MkDispatch").await;
    let module = host.registry().interface(&iface).unwrap().module_id();
    let all = vec![ALL_CAPABILITY.clone()];
    let say = format!("{}.say", iface);

    let ok = host.dispatch(&say, json!({ "room": "r", "text": "hi" }), ctx("did:key:a", all.clone())).await.unwrap();
    assert_eq!(ok, json!({ "text": "hi" }));

    // A grant on the module line works; a grant on another line or action does not.
    let line = grant(format!("service:{}@1", module), &["SAY"]);
    assert!(host.dispatch(&say, json!({ "room": "r", "text": "hi" }), ctx("did:key:a", vec![line])).await.is_ok());
    let other = grant(format!("service:{}@2", module), &["SAY"]);
    let e = host.dispatch(&say, json!({ "room": "r", "text": "hi" }), ctx("did:key:a", vec![other])).await.unwrap_err();
    assert_eq!(e.code, 403);
    assert!(e.message.contains("#SAY"));

    // 400: missing param, unknown param, wrong type.
    for bad in [json!({ "room": "r" }), json!({ "room": "r", "text": "x", "extra": 1 }), json!({ "room": 1, "text": "x" })] {
        assert_eq!(host.dispatch(&say, bad, ctx("did:key:a", all.clone())).await.unwrap_err().code, 400);
    }

    // A declared method error: its code, and `data` with the name.
    let e = host.dispatch(&say, json!({ "room": "r", "text": "mute" }), ctx("did:key:a", all.clone())).await.unwrap_err();
    assert_eq!((e.code, e.data.clone()), (409, Some(json!({ "name": "Muted", "room": "r" }))));
    // An undeclared one is a 500.
    let e = host.dispatch(&say, json!({ "room": "r", "text": "undeclared" }), ctx("did:key:a", all.clone())).await.unwrap_err();
    assert_eq!(e.code, 500);

    // 404: unknown hash or method.
    assert_eq!(host.dispatch("QmNopeNopeNopeNope.say", json!({}), ctx("did:key:a", all.clone())).await.unwrap_err().code, 404);
    assert_eq!(host.dispatch(&format!("{}.nope", iface), json!({}), ctx("did:key:a", all.clone())).await.unwrap_err().code, 404);

    // Pinning the implementation works too.
    let pinned = format!("{}.say", implementation);
    assert!(host.dispatch(&pinned, json!({ "room": "r", "text": "hi" }), ctx("did:key:a", all.clone())).await.is_ok());

    // 503 once stopped.
    host.stop(&implementation).await.unwrap();
    assert_eq!(host.dispatch(&say, json!({ "room": "r", "text": "hi" }), ctx("did:key:a", all.clone())).await.unwrap_err().code, 503);
}

#[tokio::test]
async fn nested_calls_need_every_layer() {
    let host = ServiceHost::new();
    let (iface, _) = echo_on(&host, "did:key:z6MkNested").await;
    let module = host.registry().interface(&iface).unwrap().module_id();
    // A caller service that requires SAY on echo, and one that requires nothing.
    let with = host
        .register_builtin(
            manifest("with", vec![iface.clone()], vec![Requirement { interface: iface.clone(), actions: vec!["SAY".into()], optional: false }]),
            Arc::new(Echo::default()),
        )
        .unwrap();
    let without = host.register_builtin(manifest("without", vec![iface.clone()], vec![]), Arc::new(Echo::default())).unwrap();
    let caller = |implementation: &str| {
        let i = host.registry().implementation(implementation).map(|i| i.grants.clone()).unwrap();
        super::builtin::ServiceCaller { host: host.clone(), implementation: implementation.into(), grants: i }
    };
    let say = format!("{}.say", iface);
    let params = json!({ "room": "r", "text": "hi" });
    let app_all = ctx("did:key:a", vec![ALL_CAPABILITY.clone()]);
    assert!(caller(&with).call(&app_all, &say, params.clone()).await.is_ok());
    // The service lacks the grant, even though the app has everything.
    assert_eq!(caller(&without).call(&app_all, &say, params.clone()).await.unwrap_err().code, 403);
    // The app lacks it, even though the service has it: no confused deputy.
    let app_none = ctx("did:key:a", vec![grant(format!("service:{}@1", module), &["OTHER"])]);
    assert_eq!(caller(&with).call(&app_none, &say, params).await.unwrap_err().code, 403);
}

#[tokio::test]
async fn result_outside_contract_is_logged_for_builtins() {
    let host = ServiceHost::new();
    let (iface, _) = echo_on(&host, "did:key:z6MkResult").await;
    // Builtins are trusted: debug builds log the violation and pass the result.
    let r = host
        .dispatch(&format!("{}.say", iface), json!({ "room": "r", "text": "bad result" }), ctx("did:key:a", vec![ALL_CAPABILITY.clone()]))
        .await
        .unwrap();
    assert_eq!(r, json!({ "nope": 1 }));
}

#[tokio::test]
async fn events_reach_only_their_owner_with_the_grant() {
    let host = ServiceHost::new();
    let (iface, implementation) = echo_on(&host, "did:key:z6MkEvents").await;
    let module = host.registry().interface(&iface).unwrap().module_id();
    let mut rx = host.subscribe_events();
    host.emit_event(&implementation, "said", "did:key:a", json!({ "room": "r", "text": "hi" })).await.unwrap();
    let e = rx.recv().await.unwrap();
    assert_eq!(e.event_type, format!("{}.said", iface));
    let wire: Value = serde_json::from_str(&e.wire).unwrap();
    assert_eq!(wire, json!({ "type": format!("{}.said", iface), "room": "r", "text": "hi" }));
    let yes = vec![grant(format!("service:{}@1", module), &["SAY"])];
    assert!(ServiceHost::delivers(&e, Some("did:key:a"), false, &yes));
    assert!(!ServiceHost::delivers(&e, Some("did:key:b"), false, &yes));
    assert!(ServiceHost::delivers(&e, None, true, &yes));
    assert!(!ServiceHost::delivers(&e, Some("did:key:a"), false, &[]));
    // A payload outside the contract never goes out.
    assert!(host.emit_event(&implementation, "said", "did:key:a", json!({ "room": 1 })).await.is_err());
    assert!(host.emit_event(&implementation, "nope", "did:key:a", json!({})).await.is_err());
}

// ── Over the RPC socket ─────────────────────────────────────────────────────

const ALICE: &str = "did:key:z6MkAliceSocket";

struct Socket {
    input: mpsc::UnboundedSender<String>,
    out: mpsc::UnboundedReceiver<String>,
}

impl Socket {
    async fn open(capabilities: Vec<Capability>) -> Self {
        let (tx, out) = mpsc::unbounded_channel();
        let (input, incoming) = mpsc::unbounded_channel();
        let events = build_event_stream_for(String::new(), Some(ALICE.into()), None, false, capabilities.clone()).await;
        let ctx = Arc::new(RequestContext {
            capabilities: Ok(capabilities),
            auto_permit_cap_requests: false,
            auth_token: String::new(),
            is_admin_credential: false,
            user_email: None,
            user_did: Some(ALICE.into()),
            cancel_token: None,
        });
        let conn = Connection::new(Arc::new(build_handler_map()), ctx, String::new(), tx);
        tokio::spawn(serve(conn, UnboundedReceiverStream::new(incoming), events));
        Self { input, out }
    }

    fn send(&self, msg: Value) {
        self.input.send(msg.to_string()).unwrap();
    }

    async fn next(&mut self) -> Value {
        let msg = tokio::time::timeout(Duration::from_secs(10), self.out.recv())
            .await
            .expect("a message within 10 s")
            .expect("the connection is open");
        serde_json::from_str(&msg).unwrap()
    }

    /// The reply to `id`; events that arrive first are returned too.
    async fn reply(&mut self, id: &str) -> (Value, Vec<Value>) {
        let mut events = Vec::new();
        loop {
            let m = self.next().await;
            if m.get("id") == Some(&json!(id)) {
                return (m, events);
            }
            events.push(m);
        }
    }

    async fn nothing_more(&mut self) {
        assert!(
            tokio::time::timeout(Duration::from_millis(300), self.out.recv()).await.is_err(),
            "no further message"
        );
    }
}

#[tokio::test]
async fn round_trip_over_the_rpc_socket() {
    let (iface, implementation) = echo_on(&host(), "did:key:z6MkSocket").await;
    let module = host().registry().interface(&iface).unwrap().module_id();
    let mut s = Socket::open(vec![grant(format!("service:{}@1", module), &["SAY"]), crate::agent::capabilities::AGENT_READ_CAPABILITY.clone()]).await;

    // Call before any watch: a result, and no events.
    s.send(json!({ "id": "1", "type": format!("{}.say", iface), "params": { "room": "r1", "text": "hi" } }));
    let (reply, events) = s.reply("1").await;
    assert_eq!(reply["result"], json!({ "text": "hi" }));
    assert!(events.is_empty());

    // Watch one room: its events arrive, other rooms' do not.
    s.send(json!({ "id": "w", "type": "events.watch", "params": {
        format!("{}.said", iface): ["r1"],
        format!("{}.count-tick", iface): ["s-1"],
    } }));
    assert_eq!(s.reply("w").await.0["result"], json!(true));
    s.send(json!({ "id": "2", "type": format!("{}.say", iface), "params": { "room": "r2", "text": "elsewhere" } }));
    let (_, events) = s.reply("2").await;
    assert!(events.is_empty());
    s.send(json!({ "id": "3", "type": format!("{}.say", iface), "params": { "room": "r1", "text": "here" } }));
    let mut got = Vec::new();
    while got.len() < 2 {
        got.push(s.next().await);
    }
    let event = got.iter().find(|m| m.get("type").is_some()).unwrap();
    assert_eq!(event, &json!({ "type": format!("{}.said", iface), "room": "r1", "text": "here" }));

    // A stream: the client chose the id and watched it first.
    s.send(json!({ "id": "4", "type": format!("{}.count", iface), "params": { "to": 3, "streamId": "s-1" } }));
    let mut ticks = Vec::new();
    let reply = loop {
        let m = s.next().await;
        if m.get("id") == Some(&json!("4")) {
            break m;
        }
        ticks.push(m["n"].clone());
    };
    // The reply can overtake the last ticks; collect the rest.
    while ticks.len() < 3 {
        ticks.push(s.next().await["n"].clone());
    }
    assert_eq!(reply["result"], json!({ "total": 3 }));
    assert_eq!(ticks, vec![json!(1), json!(2), json!(3)]);

    // Protocol errors over the wire.
    s.send(json!({ "id": "5", "type": format!("{}.say", iface), "params": { "room": "r1" } }));
    assert_eq!(s.reply("5").await.0["error"]["code"], json!(400));
    s.send(json!({ "id": "6", "type": format!("{}.say", iface), "params": { "room": "r1", "text": "mute" } }));
    let err = s.reply("6").await.0["error"].clone();
    assert_eq!(err, json!({ "code": 409, "message": "the room is muted", "data": { "name": "Muted", "room": "r1" } }));

    // services.describe shows the interface with the caller's actions.
    s.send(json!({ "id": "7", "type": "services.describe", "params": { "target": iface } }));
    let d = s.reply("7").await.0["result"].clone();
    assert_eq!(d["interfaces"][0]["grantedActions"], json!(["SAY"]));
    assert_eq!(d["implementations"][0]["hash"], json!(implementation));
    s.send(json!({ "id": "8", "type": "services.interface", "params": { "hash": iface } }));
    assert_eq!(s.reply("8").await.0["result"]["name"], json!("echo"));

    s.nothing_more().await;
    host().stop(&implementation).await.unwrap();
    s.send(json!({ "id": "9", "type": format!("{}.say", iface), "params": { "room": "r1", "text": "hi" } }));
    assert_eq!(s.reply("9").await.0["error"]["code"], json!(503));
}

#[tokio::test]
async fn missing_grant_over_the_socket_is_403_and_hides_events() {
    let (iface, implementation) = echo_on(&host(), "did:key:z6MkNoGrant").await;
    let mut s = Socket::open(vec![crate::agent::capabilities::AGENT_READ_CAPABILITY.clone()]).await;
    s.send(json!({ "id": "w", "type": "events.watch", "params": { format!("{}.said", iface): null } }));
    s.reply("w").await;
    s.send(json!({ "id": "1", "type": format!("{}.say", iface), "params": { "room": "r", "text": "hi" } }));
    assert_eq!(s.reply("1").await.0["error"]["code"], json!(403));
    // An event for this agent emitted by another path still needs the grant.
    host().emit_event(&implementation, "said", ALICE, json!({ "room": "r", "text": "x" })).await.unwrap();
    s.nothing_more().await;
}
