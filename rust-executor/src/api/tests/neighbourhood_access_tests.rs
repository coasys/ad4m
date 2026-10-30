//! `neighbourhood.*` methods on a multi-user executor.
//!
//! The perspective that holds a neighbourhood decides who may use it (`Guard::Owns`),
//! by the rule the `perspective.*` methods apply. Knowing another user's perspective uuid must
//! not let a user publish it (which would add them to its owners), inject signals into it, or
//! read who is online in it.

use serde_json::{json, Value};
use tokio::sync::broadcast::Receiver;

use super::multi_user::{expect_code, Executor, RegisteredPerspective};
use crate::db::Ad4mDb;
use crate::events::{Event, Topic};
use crate::pubsub::get_global_pubsub;

fn empty_perspective() -> Value {
    json!({ "links": [] })
}

/// Presence of co-located users comes from `users.last_seen`.
fn mark_online(email: &str) {
    Ad4mDb::with_global_instance(|db| db.update_user_last_seen(email)).expect("record last_seen");
}

/// Signals the executor routed to co-located users of perspective `uuid`, as
/// `(recipient, author)`.
fn delivered_signals(
    rx: &mut Receiver<std::sync::Arc<Event>>,
    uuid: &str,
) -> Vec<(String, String)> {
    let mut delivered = Vec::new();
    while let Ok(event) = rx.try_recv() {
        if event.topic == Topic::NeighbourhoodSignal && event.perspective.as_deref() == Some(uuid) {
            let text = |v: &Value| v.as_str().unwrap_or_default().to_string();
            delivered.push((
                text(&event.payload["recipient"]),
                text(&event.payload["signal"]["author"]),
            ));
        }
    }
    delivered
}

#[tokio::test]
async fn a_user_cannot_signal_or_broadcast_through_another_users_neighbourhood() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let hers = RegisteredPerspective::neighbourhood(Some(vec![alice.did.clone()]));
    let mut signals = get_global_pubsub().await.subscribe_events();

    let signal = json!({
        "uuid": hers.uuid,
        "remoteAgentDid": alice.did,
        "payload": empty_perspective(),
    });
    expect_code(
        executor
            .call("neighbourhood.sendSignal", signal.clone(), &bob.session)
            .await,
        404,
        "bob signalling alice through her neighbourhood",
    );
    let broadcast = json!({ "uuid": hers.uuid, "payload": empty_perspective() });
    expect_code(
        executor
            .call(
                "neighbourhood.sendBroadcast",
                broadcast.clone(),
                &bob.session,
            )
            .await,
        404,
        "bob broadcasting into alice's neighbourhood",
    );
    assert_eq!(
        delivered_signals(&mut signals, &hers.uuid),
        vec![],
        "a refused call must not deliver a forged signal to alice"
    );

    // Alice still reaches her own neighbourhood.
    let sent = executor
        .call("neighbourhood.sendSignal", signal, &alice.session)
        .await
        .expect("alice signals through her own neighbourhood");
    assert_eq!(sent, json!(true));
    assert_eq!(
        delivered_signals(&mut signals, &hers.uuid),
        vec![(alice.did.clone(), alice.did.clone())],
        "alice's signal is signed by her and routed to her"
    );
    // With no link language running, a broadcast gets as far as the network hop.
    let err = expect_code(
        executor
            .call("neighbourhood.sendBroadcast", broadcast, &alice.session)
            .await,
        500,
        "alice broadcasting into her own neighbourhood",
    );
    assert!(
        err.message.contains("no link language installed"),
        "{}",
        err.message
    );
}

#[tokio::test]
async fn a_user_cannot_set_status_or_read_presence_in_another_users_neighbourhood() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    mark_online(&alice.email);
    let hers = RegisteredPerspective::neighbourhood(Some(vec![alice.did.clone()]));
    let uuid = json!({ "uuid": hers.uuid });
    let status = json!({ "uuid": hers.uuid, "status": empty_perspective() });

    for (op, params) in [
        ("neighbourhood.setOnlineStatus", status.clone()),
        ("neighbourhood.onlineAgents", uuid.clone()),
        ("neighbourhood.hasTelepresence", uuid.clone()),
        ("neighbourhood.otherAgents", uuid.clone()),
    ] {
        expect_code(
            executor.call(op, params, &bob.session).await,
            404,
            &format!("bob calling {op} on alice's neighbourhood"),
        );
    }

    // Alice still reaches all of it.
    let online = executor
        .call("neighbourhood.onlineAgents", uuid.clone(), &alice.session)
        .await
        .expect("alice reads presence in her own neighbourhood");
    let online: Vec<&str> = online
        .as_array()
        .expect("online agents")
        .iter()
        .map(|agent| agent["did"].as_str().unwrap_or_default())
        .collect();
    assert_eq!(online, vec![alice.did.as_str()]);
    assert_eq!(
        executor
            .call(
                "neighbourhood.hasTelepresence",
                uuid.clone(),
                &alice.session
            )
            .await
            .expect("alice probes her own neighbourhood"),
        json!(false)
    );
    // The status and member list both live in the link language, which is not running:
    // past the access check, they report exactly that.
    for (op, params) in [
        ("neighbourhood.setOnlineStatus", status),
        ("neighbourhood.otherAgents", uuid),
    ] {
        let err = expect_code(executor.call(op, params, &alice.session).await, 500, op);
        assert!(
            err.message.contains("no link language installed"),
            "{op}: {}",
            err.message
        );
    }
}

#[tokio::test]
async fn a_user_cannot_publish_another_users_perspective_and_become_its_owner() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let hers = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let publish = json!({
        "perspectiveUUID": hers.uuid,
        // No language has this address, so a publish that gets past the access check stops
        // at installing it, before any owner changes.
        "linkLanguage": format!("isolation-test-language-{}", uuid::Uuid::new_v4()),
        "meta": empty_perspective(),
    });

    expect_code(
        executor
            .call("neighbourhood.publish", publish.clone(), &bob.session)
            .await,
        404,
        "bob publishing alice's perspective",
    );
    assert_eq!(hers.owners().await, Some(vec![alice.did.clone()]));

    let err = expect_code(
        executor
            .call("neighbourhood.publish", publish, &alice.session)
            .await,
        500,
        "alice publishing her own perspective",
    );
    assert!(
        err.message.contains("no source available"),
        "alice's publish must reach the link language install: {}",
        err.message
    );
    assert_eq!(hers.owners().await, Some(vec![alice.did.clone()]));
}

/// Unowned perspectives belong to the main agent. The old `otherAgents` check compared DIDs
/// only when an owners list existed, so any user could list the members of those.
#[tokio::test]
async fn a_user_cannot_list_members_of_the_main_agents_unowned_neighbourhood() {
    let executor = Executor::start().await;
    let bob = executor.user("bob");
    let legacy = RegisteredPerspective::neighbourhood(None);
    let uuid = json!({ "uuid": legacy.uuid });

    expect_code(
        executor
            .call("neighbourhood.otherAgents", uuid.clone(), &bob.session)
            .await,
        404,
        "bob listing members of the main agent's neighbourhood",
    );
    let err = expect_code(
        executor
            .call("neighbourhood.otherAgents", uuid, &executor.admin())
            .await,
        500,
        "the operator listing members of the main agent's neighbourhood",
    );
    assert!(
        err.message.contains("no link language installed"),
        "{}",
        err.message
    );
}

/// The operator and apps keep what the `perspective.*` methods give them: the operator
/// reaches every perspective; a main-agent app the main agent's, but not a user's.
#[tokio::test]
async fn operators_and_apps_keep_their_perspective_access() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    mark_online(&alice.email);
    let hers = RegisteredPerspective::neighbourhood(Some(vec![alice.did.clone()]));
    let main_agents = RegisteredPerspective::neighbourhood(Some(vec![executor.main_agent_did()]));
    let mut signals = get_global_pubsub().await.subscribe_events();
    let admin = executor.admin();

    let sent = executor
        .call(
            "neighbourhood.sendSignal",
            json!({ "uuid": hers.uuid, "remoteAgentDid": alice.did, "payload": empty_perspective() }),
            &admin,
        )
        .await
        .expect("the operator signals through a user's neighbourhood");
    assert_eq!(sent, json!(true));
    assert_eq!(delivered_signals(&mut signals, &hers.uuid).len(), 1);
    let online = executor
        .call(
            "neighbourhood.onlineAgents",
            json!({ "uuid": hers.uuid }),
            &admin,
        )
        .await
        .expect("the operator reads presence in a user's neighbourhood");
    assert_eq!(online.as_array().map(Vec::len), Some(1));

    let app = executor.main_agent_app();
    assert_eq!(
        executor
            .call(
                "neighbourhood.hasTelepresence",
                json!({ "uuid": main_agents.uuid }),
                &app
            )
            .await
            .expect("a main-agent app probes the main agent's neighbourhood"),
        json!(false)
    );
    // The answer `perspective.queryLinks` gives this app for her perspective.
    expect_code(
        executor
            .call(
                "neighbourhood.hasTelepresence",
                json!({ "uuid": hers.uuid }),
                &app,
            )
            .await,
        404,
        "a main-agent app probing a user's neighbourhood",
    );
}
