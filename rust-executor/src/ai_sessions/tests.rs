//! The session service end to end: ownership, capabilities, the running-turn rules and
//! the transcript a turn leaves.
//!
//! The model is a script ([`Scripted`]); the tools a turn sees are a stub provider unless
//! a test names the real MCP surface (`provider: None`). `send` spawns its turn as a
//! detached task, so a test waits on the reply row's status, never on a handle. The
//! callers, model, providers and fixtures live in [`super::testkit`]; the tests of what a
//! turn may reach through the real MCP tool surface are in [`super::surface_tests`].

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use serde_json::json;

use super::run::run_turn;
use super::service::{
    self, lifecycle_entry_for_test, running_for_test, AiSessionCreate, AiSessionUpdateParams,
    SessionError,
};
use super::store::ai_session_store;
use super::testkit::*;
use super::types::AiSessionMessageRole;
use super::types::AiTurnStatus::*;
use crate::agent::capabilities::{
    perspective_query_capability, perspective_update_capability, Capability, AI_READ_CAPABILITY,
};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::events::Topic;
use crate::types::RequestContext;

// ── ownership ────────────────────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_session_is_its_owners_alone() {
    setup();
    let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
    let (alice, alice_session) = alice("a1");
    let bob = user("did:key:bob-b1", everything());
    let operator = operator();
    let main_session = create(&operator);

    // Whoever names another's session — another user, or the operator acting as the main
    // agent — gets "not found" from every method. Bob's list is empty; each owner sees
    // exactly their own.
    assert_unreachable(&bob, &alice_session).await;
    assert_unreachable(&operator, &alice_session).await;
    assert_unreachable(&alice, &main_session).await;
    let listed = |caller: &RequestContext| -> Vec<String> {
        service::list(caller)
            .unwrap()
            .into_iter()
            .map(|s| s.id)
            .collect()
    };
    assert_eq!(listed(&alice), vec![alice_session.id.clone()]);
    assert_eq!(listed(&bob), Vec::<String>::new());
    assert!(listed(&operator).contains(&main_session.id));
    assert!(!listed(&operator).contains(&alice_session.id));
    for (owner, own) in [(&alice, &alice_session), (&operator, &main_session)] {
        assert_eq!(service::get(owner, &own.id).unwrap().title, "test");
        assert!(service::messages(owner, &own.id, None, None)
            .unwrap()
            .is_empty());
    }

    // And each one's events go to its own sessions only.
    let mut heard = 0;
    while let Ok(Ok(event)) = tokio::time::timeout(Duration::from_millis(200), events.recv()).await
    {
        let of_main = event.payload["sessionId"] == main_session.id.as_str();
        let of_alice = event.payload["sessionId"] == alice_session.id.as_str();
        if !(of_main || of_alice) {
            continue;
        }
        heard += 1;
        assert_eq!(operator.session.may_receive(&event), of_main);
        assert_eq!(alice.session.may_receive(&event), of_alice);
    }
    assert_eq!(heard, 2, "one `created` event each");
}

// ── capabilities ─────────────────────────────────────────────────────────────

#[test]
fn create_and_update_narrow_the_sessions_capabilities_to_the_callers() {
    setup();
    // The caller holds read on perspective A and nothing else.
    let read_a = perspective_query_capability(vec!["pa".into()]);
    let caller = user("did:key:alice-a3", vec![read_a.clone()]);
    let params = |capabilities: Option<Vec<Capability>>| AiSessionCreate {
        model: Some("m".into()),
        capabilities,
        ..Default::default()
    };

    // Create asks for read+write on A and read on B: only the read on A stays.
    let session = service::create(
        &caller,
        params(Some(vec![
            read_a.clone(),
            perspective_update_capability(vec!["pa".into()]),
            perspective_query_capability(vec!["pb".into()]),
        ])),
    )
    .unwrap();
    assert_eq!(
        session.capabilities,
        vec![read_a.clone()],
        "only the read on A the caller holds stays"
    );

    // Absent: everything the caller holds.
    let session = service::create(&caller, params(None)).unwrap();
    assert_eq!(session.capabilities, vec![read_a.clone()]);

    // An update asking for a capability the caller does not hold narrows to nothing.
    let session = service::update(
        &caller,
        &session.id,
        AiSessionUpdateParams {
            capabilities: Some(vec![perspective_update_capability(vec!["pa".into()])]),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(session.capabilities.is_empty());
}

// ── the running-turn rules ──────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn the_running_turn_rules() {
    setup();
    let did = "did:key:alice-a5";
    let alice = user(did, everything());
    let session = create(&alice);

    // A send runs detached; the reply lands on its row and the session is free.
    let (_, reply) = sent_reply(&alice, &session, deps(Scripted::answering(vec![]))).await;
    assert_eq!(reply.status, Some(Done));
    assert_eq!(reply.content["text"], "done");
    let messages = service::messages(&alice, &session.id, None, None).unwrap();
    assert_eq!(messages.len(), 2);
    assert_eq!(messages[0].role, AiSessionMessageRole::User);
    assert_eq!(messages[0].content["text"], "go");
    assert!(!running_for_test(&session.id));

    // A turn that waits: the session is busy until it is released. Eight running turns
    // per caller at most — on any of their sessions. Another caller's limit is their own.
    let hold = Arc::new(AtomicBool::new(false));
    send_held(&alice, &session, &hold).await;
    assert!(matches!(
        service::send(
            &alice,
            &session.id,
            "two",
            deps(Scripted::answering(vec![]))
        )
        .await,
        Err(SessionError::Busy)
    ));
    let mut replies = Vec::new();
    for _ in 0..service::MAX_RUNNING_PER_CALLER - 1 {
        let extra = create(&alice);
        let started = send_held(&alice, &extra, &hold).await;
        replies.push((Some(did), started.reply_id));
    }
    let ninth = create(&alice);
    assert!(matches!(
        service::send(&alice, &ninth.id, "go", deps(Scripted::answering(vec![]))).await,
        Err(SessionError::TooManyTurns)
    ));
    let bob = user("did:key:bob-a6", everything());
    let his = create(&bob);
    let his_started = send_held(&bob, &his, &hold).await;
    replies.push((Some("did:key:bob-a6"), his_started.reply_id));

    hold.store(true, Ordering::SeqCst);
    for (owner, reply_id) in replies {
        assert_eq!(settled(owner, &reply_id).await.status, Some(Done));
    }

    // Cancel ends the running turn; the model answering afterwards changes nothing:
    // the row stays as the cancel left it.
    let session = create(&alice);
    hold.store(false, Ordering::SeqCst);
    let started = send_held(&alice, &session, &hold).await;
    assert!(service::cancel(&alice, &session.id).unwrap());
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(Cancelled));
    hold.store(true, Ordering::SeqCst);
    let store = ai_session_store();
    tokio::time::sleep(Duration::from_millis(200)).await;
    let row = store
        .message(Some(did), &started.reply_id)
        .unwrap()
        .unwrap();
    assert_eq!(row.status, Some(Cancelled));
    // Nothing running now: cancel says so, and a new send is free to go.
    assert!(!service::cancel(&alice, &session.id).unwrap());
    let (_, reply) = sent_reply(&alice, &session, deps(Scripted::answering(vec![]))).await;
    assert_eq!(reply.status, Some(Done));

    // A turn inside a tool call that never returns: cancel ends its step with it.
    let started = send_go(
        &alice,
        &session,
        tooling(
            Scripted::answering(vec![calls("wait", json!({}))]),
            Arc::new(Stuck),
            None,
        ),
    )
    .await;
    let step = first_step(did, &session.id).await;
    assert_eq!(step.status, Some(Running));
    assert!(service::cancel(&alice, &session.id).unwrap());
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(Cancelled));
    let step = settled(Some(did), &step.id).await;
    assert_eq!(step.status, Some(Interrupted));
    assert!(step.error.is_some());

    // A delete ends a running turn and takes the transcript with it. The turn waits on a
    // model that is about to ask for a tool: answering after the delete, none of it runs
    // and nothing asks the model again.
    hold.store(false, Ordering::SeqCst);
    let model = Scripted::holding_with(hold.clone(), vec![calls("note", json!({}))]);
    let provider = StubProvider::named(&["note"]);
    send_go(
        &alice,
        &session,
        tooling(model.clone(), provider.clone(), None),
    )
    .await;
    until_asked(&model).await;
    assert!(service::delete(&alice, &session.id).unwrap());
    assert!(matches!(
        service::get(&alice, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(service::messages(&alice, &session.id, None, None).is_err());
    assert!(!running_for_test(&session.id));
    tokio::time::sleep(Duration::from_millis(50)).await;
    hold.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(300)).await;
    assert!(provider.calls.lock().unwrap().is_empty());
    assert_eq!(model.times_asked(), 1);
}

#[tokio::test(flavor = "multi_thread")]
async fn only_its_owner_ends_a_sessions_running_turn() {
    setup();
    let did = "did:key:alice-a10";
    let alice = user(did, everything());
    let session = create(&alice);
    let hold = Arc::new(AtomicBool::new(false));
    let started = send_held(&alice, &session, &hold).await;

    // Another user and the operator name the session: neither finds it, and the turn
    // runs on. A send answers "not found" too, not "busy": the answer does not say a
    // turn is running there.
    for other in [user("did:key:bob-a10", everything()), operator()] {
        for outcome in [
            service::delete(&other, &session.id).map(|_| ()),
            service::cancel(&other, &session.id).map(|_| ()),
            service::send(&other, &session.id, "hi", deps(Scripted::answering(vec![])))
                .await
                .map(|_| ()),
        ] {
            assert!(
                matches!(outcome, Err(SessionError::NotFound(_))),
                "{outcome:?}"
            );
        }
        assert!(running_for_test(&session.id));
    }
    // The operator passes the `owns` guard of any id, so over the socket its call reaches
    // the handler. The answer is the same.
    let methods = crate::api::ws_handler::build_handler_map();
    for (method, params) in [
        ("ai.sessionDelete", json!({ "id": session.id })),
        ("ai.sessionCancel", json!({ "sessionId": session.id })),
    ] {
        let refused = methods
            .dispatch(method, params, Arc::new(operator()))
            .await
            .unwrap_err();
        assert_eq!(refused.code, 404, "{method}: {}", refused.message);
        assert!(running_for_test(&session.id), "{method}");
    }

    hold.store(true, Ordering::SeqCst);
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(Done));
    assert_eq!(reply.content["text"], "done");
    assert!(service::get(&alice, &session.id).is_ok());
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_who_may_only_read_changes_no_session_and_starts_no_turn() {
    setup();
    let did = "did:key:alice-a15";
    let alice = user(did, everything());
    let session = create(&alice);
    // One of Alice's apps, holding `artificial intelligence:READ` and nothing else.
    let reader = || Arc::new(user(did, vec![AI_READ_CAPABILITY.clone()]));
    let methods = crate::api::ws_handler::build_handler_map();

    for (method, params) in [
        ("ai.sessionList", json!({})),
        ("ai.session", json!({ "id": session.id })),
        ("ai.sessionMessages", json!({ "sessionId": session.id })),
    ] {
        let answer = methods.dispatch(method, params, reader()).await;
        assert!(answer.is_ok(), "{method}: {:?}", answer.err());
    }
    for (method, params) in [
        ("ai.sessionCreate", json!({})),
        (
            "ai.sessionUpdate",
            json!({ "id": session.id, "title": "theirs" }),
        ),
        (
            "ai.sessionSend",
            json!({ "sessionId": session.id, "text": "hi" }),
        ),
        ("ai.sessionCancel", json!({ "sessionId": session.id })),
        ("ai.sessionDelete", json!({ "id": session.id })),
    ] {
        let refused = methods
            .dispatch(method, params, reader())
            .await
            .expect_err(method);
        assert_eq!(refused.code, 403, "{method}: {}", refused.message);
    }

    // The session is as Alice made it, with nothing said on it.
    let kept = service::get(&alice, &session.id).unwrap();
    assert_eq!(kept.title, "test");
    assert_eq!(service::list(&alice).unwrap().len(), 1);
    assert!(service::messages(&alice, &session.id, None, None)
        .unwrap()
        .is_empty());
    assert!(!running_for_test(&session.id));
}

#[tokio::test(flavor = "multi_thread")]
async fn a_send_started_while_a_delete_is_midway_leaves_no_turn_running() {
    setup();
    let did = "did:key:alice-a16";
    let alice = user(did, everything());
    let session = create(&alice);

    // The gap a send once slipped a whole turn start into: the delete has cleared the
    // running-turn slot and not yet taken the rows.
    let (delete_reached, let_delete_run) = service::pause_delete_before_commit();
    let deleting = {
        let caller = user(did, everything());
        let session_id = session.id.clone();
        tokio::task::spawn_blocking(move || service::delete(&caller, &session_id))
    };
    while !delete_reached.load(Ordering::SeqCst) {
        tokio::time::sleep(Duration::from_millis(5)).await;
    }

    // A send on the session while the delete is parked: at the gap the row is still
    // there to find, and its turn would ask the held model for a write call.
    let released = Arc::new(AtomicBool::new(false));
    let model = Scripted::holding_with(
        released.clone(),
        vec![calls("add_link", json!({"perspective_id": "px"}))],
    );
    let provider = StubProvider::named(&["add_link"]);
    let sending = {
        let model = model.clone();
        let provider = provider.clone();
        let session_id = session.id.clone();
        tokio::spawn(async move {
            service::send(&alice, &session_id, "go", tooling(model, provider, None)).await
        })
    };

    // An admitted send's task asks the model — that is what the delete must not miss.
    // A refused send never asks: give it time to say so, then let the delete run.
    let deadline = std::time::Instant::now() + Duration::from_secs(5);
    while model.times_asked() == 0 && std::time::Instant::now() < deadline {
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    let_delete_run.store(true, Ordering::SeqCst);

    assert!(deleting.await.unwrap().unwrap());
    let outcome = sending.await.unwrap();
    assert!(
        matches!(outcome, Err(SessionError::NotFound(_))),
        "a send on a deleted session answers not-found: {outcome:?}"
    );
    assert!(
        !running_for_test(&session.id),
        "no turn may be running on a session that is gone"
    );
    // Whatever the send's model answers afterwards, none of it runs.
    released.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(200)).await;
    assert!(provider.calls.lock().unwrap().is_empty());
}

#[tokio::test(flavor = "multi_thread")]
async fn another_owners_delete_leaves_the_sessions_lifecycle_lock_in_place() {
    setup();
    let did = "did:key:alice-a17";
    let alice = user(did, everything());
    let session = create(&alice);
    // A send leaves the session's lifecycle lock in the table: the session lives on.
    let (_, reply) = sent_reply(&alice, &session, deps(Scripted::answering(vec![]))).await;
    assert_eq!(reply.status, Some(Done));
    assert!(lifecycle_entry_for_test(Some(did), &session.id));

    // Bob's delete of an id that is not his answers not-found. It must not retire
    // Alice's lock: a send of hers would take a fresh one while her own delete still
    // holds the old, and the two would no longer exclude each other.
    let bob = user("did:key:bob-a17", everything());
    assert!(matches!(
        service::delete(&bob, &session.id),
        Err(SessionError::NotFound(_))
    ));
    assert!(
        lifecycle_entry_for_test(Some(did), &session.id),
        "another owner's not-found retires no lock of this owner's session"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_dead_or_timed_out_turn_ends_its_rows_and_frees_its_session() {
    setup();
    let did = "did:key:alice-a14";
    let alice = user(did, everything());
    let session = create(&alice);

    // A turn whose task dies: the session is free, its reply row left `running`.
    let died = send_go(&alice, &session, deps(Arc::new(Panicking))).await;
    poll("the dead task still holds the session", || {
        (!running_for_test(&session.id)).then_some(())
    })
    .await;
    let store = ai_session_store();
    let left = store.message(Some(did), &died.reply_id).unwrap().unwrap();
    assert_eq!(left.status, Some(Running));

    // A turn out of time is failed, the step it was inside is interrupted.
    let started = send_go(
        &alice,
        &session,
        tooling(
            Scripted::answering(vec![calls("wait", json!({}))]),
            Arc::new(Stuck),
            Some(Duration::from_millis(200)),
        ),
    )
    .await;
    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(Failed));
    assert_eq!(reply.error.as_deref(), Some("the turn ran out of time"));
    let step = first_step(did, &session.id).await;
    assert_eq!(step.status, Some(Interrupted));
    assert!(!running_for_test(&session.id));

    // The next send ends what a dead task left `running`, and runs its own turn.
    let (next, reply) = sent_reply(&alice, &session, deps(Scripted::answering(vec![]))).await;
    let left = store.message(Some(did), &died.reply_id).unwrap().unwrap();
    assert_eq!(left.status, Some(Interrupted));
    assert_eq!(reply.status, Some(Done));
    assert_eq!(reply.id, next.reply_id);
}

#[test]
fn the_startup_sweep_marks_every_running_row_interrupted() {
    setup();
    let (ctx, session) = alice("a9");
    let did = ctx.user_did().unwrap().to_string();
    let store = ai_session_store();
    for (id, role, content) in [
        (
            "a-turn",
            AiSessionMessageRole::Assistant,
            json!({"text": ""}),
        ),
        (
            "a-step",
            AiSessionMessageRole::Tool,
            json!({"name": "t", "arguments": {}}),
        ),
    ] {
        store
            .append_message(
                Some(did.as_str()),
                id,
                &session.id,
                role,
                &content,
                Some(Running),
            )
            .unwrap();
    }

    service::startup_sweep();

    for id in ["a-turn", "a-step"] {
        let row = store.message(Some(did.as_str()), id).unwrap().unwrap();
        assert_eq!(row.status, Some(Interrupted), "{id}");
        assert!(row.error.is_some());
    }
    // A second sweep finds nothing: `interrupted` is a settled state.
    service::startup_sweep();
    let row = store
        .message(Some(did.as_str()), "a-turn")
        .unwrap()
        .unwrap();
    assert_eq!(row.status, Some(Interrupted));
}

// ── the transcript a turn leaves ─────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn the_transcript_a_turn_leaves_and_the_next_one_reads() {
    setup();
    let did = "did:key:alice-b1";
    let alice = user(did, everything());
    let session = create(&alice);

    // A turn whose model calls a known tool and an unknown one: a `tool` row each —
    // name, arguments, target, status and result on the first, the error on the second.
    let first = service::send(
        &alice,
        &session.id,
        "look",
        tooling(
            Scripted::answering(vec![
                calls("describe_perspective", json!({"perspective_id": "px"})),
                calls("add_link", json!({"perspective_id": "px"})),
            ]),
            StubProvider::named(&["describe_perspective"]),
            None,
        ),
    )
    .await
    .unwrap();
    let reply = settled(Some(did), &first.reply_id).await;
    assert_eq!(reply.status, Some(Done));

    let rows = service::messages(&alice, &session.id, None, None).unwrap();
    let roles: Vec<_> = rows.iter().map(|m| m.role).collect();
    assert_eq!(
        roles,
        [
            AiSessionMessageRole::User,
            AiSessionMessageRole::Assistant,
            AiSessionMessageRole::Tool,
            AiSessionMessageRole::Tool,
        ]
    );
    let step = &rows[2];
    assert_eq!(step.content["name"], "describe_perspective");
    assert_eq!(step.content["target"], "px");
    assert_eq!(step.content["result"], "ok:describe_perspective");
    assert_eq!(step.status, Some(Done));
    // A call the provider does not know ends its step failed, with the error.
    assert_eq!(rows[3].status, Some(Failed), "{rows:?}");
    assert!(rows[3].content.get("error").is_some(), "{rows:?}");

    // A second turn asks the model: the history rebuilds the call and its result in
    // the order they happened — the answer's row before the step, the new message last.
    let model = Scripted::answering(vec![]);
    let second = service::send(&alice, &session.id, "and now?", deps(model.clone()))
        .await
        .unwrap();
    settled(Some(did), &second.reply_id).await;

    let asked = model.asked.lock().unwrap();
    let (messages, _) = &asked[0];
    let roles: Vec<_> = messages
        .iter()
        .map(|m| m["role"].as_str().unwrap_or_default())
        .collect();
    assert_eq!(
        roles,
        [
            "system",
            "user",
            "assistant",
            "tool",
            "tool",
            "assistant",
            "user"
        ],
        "{messages:#?}"
    );
    assert_eq!(messages[1]["content"], "look");
    assert_eq!(
        messages[2]["tool_calls"][0]["function"]["name"],
        "describe_perspective"
    );
    assert_eq!(
        messages[3]["tool_call_id"],
        messages[2]["tool_calls"][0]["id"]
    );
    assert_eq!(messages[3]["content"], "ok:describe_perspective");
    assert_eq!(messages[5]["content"], "done");
    assert_eq!(messages[6]["content"], "and now?");
}

#[tokio::test(flavor = "multi_thread")]
async fn a_tool_step_row_says_whether_its_call_writes() {
    setup();
    let did = "did:key:alice-writes";
    let alice = user(did, everything());
    let session = create(&alice);

    // One read call and one write call: each step's row carries `writes`
    // answered by `agent_session::tool_writes` — the flag a client colors
    // "changed something" from instead of guessing at the name.
    let first = service::send(
        &alice,
        &session.id,
        "look, then change",
        tooling(
            Scripted::answering(vec![
                calls("query_links", json!({"perspective_id": "px"})),
                calls("add_link", json!({"perspective_id": "px"})),
            ]),
            StubProvider::named(&["query_links", "add_link"]),
            None,
        ),
    )
    .await
    .unwrap();
    let reply = settled(Some(did), &first.reply_id).await;
    assert_eq!(reply.status, Some(Done));

    let rows = service::messages(&alice, &session.id, None, None).unwrap();
    let steps: Vec<_> = rows
        .iter()
        .filter(|m| m.role == AiSessionMessageRole::Tool)
        .collect();
    assert_eq!(steps.len(), 2, "{rows:?}");
    assert_eq!(steps[0].content["name"], "query_links");
    assert_eq!(
        steps[0].content["writes"],
        json!(false),
        "a read call's row is not a write"
    );
    assert_eq!(steps[1].content["name"], "add_link");
    assert_eq!(
        steps[1].content["writes"],
        json!(true),
        "a write call's row says it writes"
    );
}

// ── the caller's grant behind the turn ──────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_runs_only_while_the_callers_grant_holds() {
    setup();
    let did = "did:key:alice-b7";
    let alice = user(did, everything());
    let session = create(&alice);

    // A caller whose grant holds: the tool call runs, the reply lands done.
    let provider = StubProvider::named(&["add_link"]);
    let reply = sent_tool_call(
        &alice,
        &session,
        vec![calls("add_link", json!({"perspective_id": "px"}))],
        provider.clone(),
    )
    .await;
    assert_eq!(reply.status, Some(Done));
    assert_eq!(
        *provider.calls.lock().unwrap(),
        vec!["add_link".to_string()]
    );

    // The model has been asked and is holding its answer: the turn is inside a
    // completion when the caller's grant ends (a revoke or a logout is the same `ended`
    // cancellation). No call of that completion may run: the turn ends refused, and the
    // write tool it asked for stays uncalled.
    let released = Arc::new(AtomicBool::new(false));
    let model = Scripted::holding_with(
        released.clone(),
        vec![calls("add_link", json!({"perspective_id": "px"}))],
    );
    let provider = StubProvider::named(&["add_link"]);
    let started = send_go(
        &alice,
        &session,
        tooling(model.clone(), provider.clone(), None),
    )
    .await;
    until_asked(&model).await;
    alice.session.grant().unwrap().ended.cancel();
    released.store(true, Ordering::SeqCst);

    let reply = settled(Some(did), &started.reply_id).await;
    assert_eq!(reply.status, Some(Failed));
    assert!(
        provider.calls.lock().unwrap().is_empty(),
        "a call of a dead caller's completion must not run"
    );

    // A session whose grant is already past its end: `check` refuses before the model
    // is asked at all.
    let dead = Session::for_test(Principal::Agent(AgentContext::managed(did.to_string())))
        .with_caps(everything())
        .expiring_at(1);
    let ctx = ctx_for(dead);
    let store = ai_session_store();
    for (id, role, status) in [
        ("u1", AiSessionMessageRole::User, None),
        ("r1", AiSessionMessageRole::Assistant, Some(Running)),
    ] {
        store
            .append_message(
                Some(did),
                id,
                &session.id,
                role,
                &json!({"text": ""}),
                status,
            )
            .unwrap();
    }
    let outcome = run_turn(
        session,
        ctx.session.clone(),
        ctx.acts_as(),
        store,
        deps(Scripted::answering(vec![])),
    )
    .await;
    assert!(outcome.is_err(), "a dead caller refuses the turn");
}

// ── the events ───────────────────────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_sessions_events_reach_its_owners_readers_and_no_one_else() {
    setup();
    let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
    let did = "did:key:alice-c2";
    let alice = user(did, everything());

    // A session's whole life: created, a turn with a tool step, renamed, deleted.
    let session = create(&alice);
    let (_, reply) = sent_reply(
        &alice,
        &session,
        tooling(
            Scripted::answering(vec![calls(
                "describe_perspective",
                json!({"perspective_id": "px"}),
            )]),
            StubProvider::named(&["describe_perspective"]),
            None,
        ),
    )
    .await;
    assert_eq!(reply.status, Some(Done));
    service::update(
        &alice,
        &session.id,
        AiSessionUpdateParams {
            title: Some("renamed".into()),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(service::delete(&alice, &session.id).unwrap());

    // Who is listening: the owner's apps with and without `artificial intelligence:READ`,
    // another user, the operator (the main agent) and a session nobody logged in on.
    let reader = user(did, vec![AI_READ_CAPABILITY.clone()]);
    let no_ai_read = user(did, vec![perspective_query_capability(vec!["px".into()])]);
    let bob = user("did:key:bob-c2", everything());
    let operator = operator();
    let anonymous = ctx_for(Session::for_test(Principal::Anonymous));
    let listeners = [
        ("the owner", &alice, true),
        (
            "the owner's app holding artificial intelligence:READ",
            &reader,
            true,
        ),
        (
            "the owner's app without artificial intelligence:READ",
            &no_ai_read,
            false,
        ),
        ("another user", &bob, false),
        ("the operator", &operator, false),
        ("an anonymous session", &anonymous, false),
    ];

    let mut kinds: Vec<String> = Vec::new();
    while let Ok(Ok(event)) = tokio::time::timeout(Duration::from_millis(300), events.recv()).await
    {
        if event.payload["sessionId"] != session.id.as_str() {
            continue;
        }
        let kind = event.payload["kind"].as_str().unwrap_or_default();
        assert_eq!(event.topic, Topic::AiSessionChanged);
        assert_eq!(
            event.audience,
            crate::events::Audience::Dids(vec![did.to_string()]),
            "only the owner's sessions hear a `{kind}` event"
        );
        for (who, listener, hears) in &listeners {
            assert_eq!(
                listener.session.may_receive(&event),
                *hears,
                "{who} and a `{kind}` event"
            );
        }
        kinds.push(kind.to_string());
        if kind == "created" {
            assert_eq!(event.payload["session"]["id"], session.id);
        }
    }
    let count = |kind: &str| kinds.iter().filter(|k| *k == kind).count();
    assert_eq!(count("created"), 1, "{kinds:?}");
    assert_eq!(count("updated"), 1, "{kinds:?}");
    assert_eq!(count("deleted"), 1, "{kinds:?}");
    // The person's message, the reply running and done, the step running and done.
    assert_eq!(count("message"), 5, "{kinds:?}");
}

// ── credits ──────────────────────────────────────────────────────────────────

/// Free hosting off while it lives: a managed user pays for every model call, as on a
/// hosted node.
struct PaidHosting {
    was_free: bool,
}

impl PaidHosting {
    fn on() -> Self {
        let was_free = Ad4mDb::with_global_instance(|db| {
            let was_free = db.get_free_hosting_enabled()?;
            db.set_free_hosting_enabled(false)?;
            Ok::<_, anyhow::Error>(was_free)
        })
        .unwrap();
        Self { was_free }
    }
}

impl Drop for PaidHosting {
    fn drop(&mut self) {
        let _ = crate::db_backend::db_backend().set_free_hosting_enabled(self.was_free);
    }
}

/// A managed user with `credits` to spend. Returns their DID.
fn account(credits: f64) -> String {
    let id = uuid::Uuid::new_v4();
    let email = format!("user-{id}@example.org");
    let did = format!("did:key:user-{id}");
    Ad4mDb::with_global_instance(|db| {
        db.add_user(&email, &did, "pw")?;
        db.set_user_credits(&email, credits)?;
        Ok::<_, anyhow::Error>(())
    })
    .unwrap();
    did
}

#[tokio::test(flavor = "multi_thread")]
async fn a_user_without_credits_starts_no_turn() {
    setup();
    let _paid = PaidHosting::on();
    let broke = user(&account(0.0), everything());
    let funded = user(&account(5.0), everything());

    // Over the socket `ai.sessionSend` refuses before a row is written or a slot taken.
    let session = create(&broke);
    let refused = crate::api::ws_handler::build_handler_map()
        .dispatch(
            "ai.sessionSend",
            json!({ "sessionId": session.id, "text": "hi" }),
            Arc::new(user(broke.user_did().unwrap(), everything())),
        )
        .await
        .unwrap_err();
    assert_eq!(refused.code, 403, "{}", refused.message);
    assert!(refused.message.contains("credits"), "{}", refused.message);
    assert!(service::messages(&broke, &session.id, None, None)
        .unwrap()
        .is_empty());
    assert!(!running_for_test(&session.id));

    // A turn asks the gate before every completion: a user who cannot pay is refused
    // before the model is asked at all; a funded user's turn runs.
    for (who, works) in [(&broke, false), (&funded, true)] {
        let session = create(who);
        let model = Scripted::answering(vec![]);
        let outcome = run_turn(
            session.clone(),
            who.session.clone(),
            who.acts_as(),
            ai_session_store(),
            deps(model.clone()),
        )
        .await;
        if works {
            assert_eq!(outcome.unwrap(), "done");
        } else {
            assert!(outcome.is_err(), "{outcome:?}");
        }
        assert_eq!(model.times_asked(), works as usize);
    }
}
