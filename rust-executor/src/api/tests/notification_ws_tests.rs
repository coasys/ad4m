//! Notification access control.
//!
//! A notification posts trigger matches from the perspectives it lists to a
//! webhook URL, so these tests pin whose data a notification can send:
//!
//! - RPC tests dispatch through `build_handler_map()`, the path a WS request
//!   takes, with one hand-built `RequestContext` per caller.
//! - Delivery tests call `PerspectiveInstance::calc_notification_trigger_matches`,
//!   which the notification loop runs to pick the notifications that fire
//!   and the matches it posts to their webhooks.
//! - Stream tests build a session's event stream with `build_event_stream`
//!   and publish triggered notifications to it.

use std::sync::Arc;

use futures::StreamExt;
use serde_json::{json, Value};
use uuid::Uuid;

use crate::agent::capabilities::{
    get_user_default_capabilities, perspective_query_capability, Capability, ListenerReach,
    AGENT_UPDATE_CAPABILITY, ALL_CAPABILITY,
};
use crate::agent::session::{Principal, Session};
use crate::agent::AgentService;
use crate::api::events_ws::build_event_stream;
use crate::api::ws_handler::{build_handler_map, WsRpcError};
use crate::db::Ad4mDb;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{register_perspective, unregister_perspective};
use crate::test_utils::setup_wallet;
use crate::types::{
    ExpressionProof, Link, LinkExpression, LinkStatus, Notification, NotificationInput,
    PerspectiveHandle, PerspectiveState, RequestContext,
};

const TRIGGER: &str = "SELECT ?source ?target WHERE { ?source <test://notify> ?target }";
const ALICE: &str = "alice@notifications.test";
const BOB: &str = "bob@notifications.test";
/// The webhook credential every test notification carries: distinct enough that a
/// response or event containing it can only have copied it.
const WEBHOOK_AUTH: &str = "wh-credential-never-sent-out";

fn setup() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    AgentService::init_global_test_instance();
}

/// Creates the wallet key a managed user gets at sign-up; returns the DID.
fn user(email: &str) -> String {
    AgentService::ensure_user_key_exists(email).expect("user key");
    AgentService::get_user_did_by_email(email).expect("user did")
}

/// A managed user's session.
fn user_ctx(email: &str) -> RequestContext {
    RequestContext::user_for_test(
        email,
        &AgentService::get_user_did_by_email(email).unwrap(),
        get_user_default_capabilities(),
    )
}

/// The launcher's admin credential.
fn admin_ctx() -> RequestContext {
    RequestContext::operator_for_test()
}

/// A main-agent app token that holds only `capabilities`.
fn app_ctx(capabilities: Vec<Capability>) -> RequestContext {
    RequestContext::app_for_test(capabilities)
}

/// Perspectives put in the global registry for one test. Drop takes them out
/// again: later tests in the same process assert on the registry.
#[derive(Default)]
struct Perspectives(Vec<String>);

impl Perspectives {
    fn add(&mut self, owners: Option<Vec<String>>) -> PerspectiveInstance {
        let uuid = Uuid::new_v4().to_string();
        let perspective = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid: uuid.clone(),
                name: Some("notification test".to_string()),
                neighbourhood: None,
                shared_url: None,
                state: PerspectiveState::Private,
                owners,
            },
            None,
        );
        register_perspective(uuid.clone(), perspective.clone());
        self.0.push(uuid);
        perspective
    }
}

impl Drop for Perspectives {
    fn drop(&mut self) {
        for uuid in &self.0 {
            unregister_perspective(uuid);
        }
    }
}

async fn call(op: &str, params: Value, ctx: RequestContext) -> Result<Value, WsRpcError> {
    build_handler_map()
        .dispatch(op, params, Arc::new(ctx))
        .await
}

/// `NotificationInput` as the SDK sends it.
fn input(perspective_ids: &[&str]) -> Value {
    json!({
        "description": "test notification",
        "appName": "Test App",
        "appUrl": "https://app.test",
        "appIconPath": "/icon.png",
        "trigger": TRIGGER,
        "perspectiveIds": perspective_ids,
        "webhookUrl": "https://webhook.test",
        "webhookAuth": WEBHOOK_AUTH,
    })
}

fn update_params(id: &str, perspective_ids: &[&str]) -> Value {
    let mut params = input(perspective_ids);
    params["id"] = json!(id);
    params
}

async fn create(ctx: RequestContext, perspective_ids: &[&str]) -> Result<String, WsRpcError> {
    let id = call("runtime.createNotification", input(perspective_ids), ctx).await?;
    Ok(id.as_str().expect("notification id").to_string())
}

fn stored(id: &str) -> Option<Notification> {
    Ad4mDb::with_global_instance(|db| db.get_notification(id.to_string())).expect("db read")
}

fn all_stored() -> Vec<Notification> {
    Ad4mDb::with_global_instance(|db| db.get_notifications()).expect("db read")
}

fn main_agent_did() -> String {
    AgentService::with_global_instance(|a| a.did.clone()).expect("main agent did")
}

/// The DID that owns a notification of the managed user `email`, or of the
/// main agent for `None`.
fn owner(email: Option<&str>) -> String {
    email.map_or_else(main_agent_did, |email| {
        AgentService::get_user_did_by_email(email).expect("user did")
    })
}

/// Writes a notification row directly, as any write path could have left it.
fn store(perspective_ids: &[&str], user_email: Option<&str>, granted: bool) -> String {
    Ad4mDb::with_global_instance(|db| {
        db.add_notification(
            NotificationInput {
                description: "stored notification".to_string(),
                app_name: "Test App".to_string(),
                app_url: "https://app.test".to_string(),
                app_icon_path: "/icon.png".to_string(),
                trigger: TRIGGER.to_string(),
                perspective_ids: perspective_ids.iter().map(|id| id.to_string()).collect(),
                webhook_url: "https://webhook.test".to_string(),
                webhook_auth: WEBHOOK_AUTH.into(),
            },
            &owner(user_email),
            granted,
        )
    })
    .expect("add notification")
}

async fn add_matching_link(perspective: &mut PerspectiveInstance) {
    perspective
        .add_link_expression(
            LinkExpression {
                author: "did:key:test".to_string(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                data: Link {
                    source: "test://source".to_string(),
                    predicate: Some("test://notify".to_string()),
                    target: "test://target".to_string(),
                },
                proof: ExpressionProof {
                    key: "test-key".to_string(),
                    signature: "test-signature".to_string(),
                },
                status: Some(LinkStatus::Local),
            },
            LinkStatus::Local,
            None,
        )
        .await
        .expect("add link");
}

/// Ids of the notifications that fire for `perspective`.
async fn firing(perspective: &PerspectiveInstance) -> Vec<String> {
    perspective
        .calc_notification_trigger_matches()
        .await
        .expect("trigger matches")
        .into_iter()
        .filter(|(_, matches)| !matches.is_empty())
        .map(|(notification, _)| notification.id)
        .collect()
}

// ── Delivery ──

#[tokio::test]
async fn only_granted_notifications_fire() {
    setup();
    let mut perspectives = Perspectives::default();
    let mut perspective = perspectives.add(None);
    let ungranted = store(&[&perspective.uuid], None, false);
    let granted = store(&[&perspective.uuid], None, true);

    add_matching_link(&mut perspective).await;

    let fired = firing(&perspective).await;
    assert!(
        !fired.contains(&ungranted),
        "an ungranted notification must not fire"
    );
    assert_eq!(fired, vec![granted], "a granted notification must fire");
}

// Delivery reads the grant when a notification fires. A cached notification list would
// keep firing a notification after the operator withdrew its grant.
#[tokio::test]
async fn a_withdrawn_grant_stops_the_notification() {
    setup();
    let mut perspectives = Perspectives::default();
    let mut perspective = perspectives.add(None);
    let id = store(&[&perspective.uuid], None, true);

    add_matching_link(&mut perspective).await;
    assert_eq!(firing(&perspective).await, vec![id.clone()]);

    call(
        "runtime.grantNotification",
        json!({ "id": id, "granted": false }),
        admin_ctx(),
    )
    .await
    .expect("the operator withdraws the grant");
    assert!(
        firing(&perspective).await.is_empty(),
        "a notification fired after its grant was withdrawn"
    );
}

#[tokio::test]
async fn notification_never_fires_for_a_perspective_its_owner_cannot_read() {
    setup();
    user(ALICE);
    let bob_did = user(BOB);
    let mut perspectives = Perspectives::default();
    let mut bobs = perspectives.add(Some(vec![bob_did]));
    let mut main = perspectives.add(None);

    // All granted, all listing both perspectives: only the owner differs.
    let both = [bobs.uuid.as_str(), main.uuid.as_str()];
    let alices_id = store(&both, Some(ALICE), true);
    let bobs_id = store(&both, Some(BOB), true);
    let main_id = store(&both, None, true);

    add_matching_link(&mut bobs).await;
    add_matching_link(&mut main).await;

    assert_eq!(
        firing(&bobs).await,
        vec![bobs_id],
        "only Bob may read Bob's perspective"
    );
    assert_eq!(
        firing(&main).await,
        vec![main_id],
        "a managed user may not read the main agent's perspective"
    );
    assert!(!firing(&bobs).await.contains(&alices_id));
}

#[tokio::test]
async fn users_own_notification_fires_once_created() {
    setup();
    let alice_did = user(ALICE);
    let mut perspectives = Perspectives::default();
    let mut alices = perspectives.add(Some(vec![alice_did]));

    // Managed users have no grant step: their own-data notifications are
    // granted when they are created.
    let id = create(user_ctx(ALICE), &[&alices.uuid])
        .await
        .expect("Alice lists their own perspective");
    add_matching_link(&mut alices).await;

    assert_eq!(firing(&alices).await, vec![id]);
}

// ── Create ──

#[tokio::test]
async fn create_refuses_a_perspective_the_caller_cannot_read() {
    setup();
    let alice_did = user(ALICE);
    let bob_did = user(BOB);
    let mut perspectives = Perspectives::default();
    let alices = perspectives.add(Some(vec![alice_did.clone()]));
    let bobs = perspectives.add(Some(vec![bob_did]));
    let main = perspectives.add(None);

    for foreign in [&bobs.uuid, &main.uuid] {
        let err = create(user_ctx(ALICE), &[&alices.uuid, foreign])
            .await
            .expect_err("a user must not list a perspective they do not own");
        assert_eq!(err.code, 403, "{}", err.message);
    }
    let missing = Uuid::new_v4().to_string();
    let err = create(user_ctx(ALICE), &[&missing])
        .await
        .expect_err("a user must not list a perspective that does not exist");
    assert_eq!(err.code, 404, "{}", err.message);

    // The main agent owns an operator's notification, and it cannot read a
    // managed user's perspective.
    let err = create(admin_ctx(), &[&bobs.uuid])
        .await
        .expect_err("an operator must not list a user's perspective");
    assert_eq!(err.code, 403, "{}", err.message);

    assert!(all_stored().is_empty(), "a refused create stores nothing");

    let id = create(user_ctx(ALICE), &[&alices.uuid])
        .await
        .expect("Alice lists their own perspective");
    let notification = stored(&id).unwrap();
    assert_eq!(notification.owner_did, alice_did);
    assert!(notification.granted);
}

#[tokio::test]
async fn create_requires_query_capability_for_each_perspective() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let other = perspectives.add(None);

    let app = || {
        app_ctx(vec![
            AGENT_UPDATE_CAPABILITY.clone(),
            perspective_query_capability(vec![other.uuid.clone()]),
        ])
    };
    let err = create(app(), &[&main.uuid])
        .await
        .expect_err("an app must not list a perspective it may not query");
    assert_eq!(err.code, 403, "{}", err.message);
    assert!(all_stored().is_empty());

    let id = create(app(), &[&other.uuid])
        .await
        .expect("an app may list a perspective it may query");
    assert!(
        !stored(&id).unwrap().granted,
        "a main-agent notification waits for operator approval"
    );
}

// ── Update and delete ──

#[tokio::test]
async fn user_cannot_update_or_delete_another_users_notification() {
    setup();
    let alice_did = user(ALICE);
    user(BOB);
    let mut perspectives = Perspectives::default();
    let alices = perspectives.add(Some(vec![alice_did]));
    let id = create(user_ctx(ALICE), &[&alices.uuid]).await.unwrap();
    let before = stored(&id);

    let mut params = update_params(&id, &[&alices.uuid]);
    params["webhookUrl"] = json!("https://bob.test/collect");
    let err = call("runtime.updateNotification", params, user_ctx(BOB))
        .await
        .expect_err("Bob must not update Alice's notification");
    assert_eq!(err.code, 404, "{}", err.message);

    let err = call(
        "runtime.deleteNotification",
        json!({ "id": id }),
        user_ctx(BOB),
    )
    .await
    .expect_err("Bob must not delete Alice's notification");
    assert_eq!(err.code, 404, "{}", err.message);

    assert_eq!(stored(&id), before, "Alice's notification is unchanged");

    call(
        "runtime.deleteNotification",
        json!({ "id": id }),
        user_ctx(ALICE),
    )
    .await
    .expect("Alice deletes their own notification");
    assert_eq!(stored(&id), None);
}

#[tokio::test]
async fn update_cannot_change_granted_or_owner() {
    setup();
    let alice_did = user(ALICE);
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let alices = perspectives.add(Some(vec![alice_did.clone()]));

    // The caller's `granted` is ignored.
    let main_id = create(admin_ctx(), &[&main.uuid]).await.unwrap();
    let mut params = update_params(&main_id, &[&main.uuid]);
    params["granted"] = json!(true);
    call("runtime.updateNotification", params.clone(), admin_ctx())
        .await
        .unwrap();
    assert!(!stored(&main_id).unwrap().granted, "update must not grant");

    // The operator approved the old trigger, perspectives and webhook, so an
    // update needs a new approval.
    call(
        "runtime.grantNotification",
        json!({ "id": main_id, "granted": true }),
        admin_ctx(),
    )
    .await
    .unwrap();
    call("runtime.updateNotification", params, admin_ctx())
        .await
        .unwrap();
    assert!(!stored(&main_id).unwrap().granted);

    // A user's notification keeps its owner and grant through their own update.
    let alice_id = create(user_ctx(ALICE), &[&alices.uuid]).await.unwrap();
    let mut params = update_params(&alice_id, &[&alices.uuid]);
    params["description"] = json!("edited");
    params["ownerDid"] = json!(main_agent_did());
    call(
        "runtime.updateNotification",
        params.clone(),
        user_ctx(ALICE),
    )
    .await
    .unwrap();
    let notification = stored(&alice_id).unwrap();
    assert_eq!(notification.owner_did, alice_did);
    assert!(notification.granted);
    assert_eq!(notification.description, "edited");

    // An operator's update keeps the owner and drops the user's grant.
    call("runtime.updateNotification", params, admin_ctx())
        .await
        .unwrap();
    let notification = stored(&alice_id).unwrap();
    assert_eq!(notification.owner_did, alice_did);
    assert!(!notification.granted);
}

#[tokio::test]
async fn update_refuses_a_perspective_the_owner_cannot_read() {
    setup();
    let alice_did = user(ALICE);
    let bob_did = user(BOB);
    let mut perspectives = Perspectives::default();
    let alices = perspectives.add(Some(vec![alice_did]));
    let bobs = perspectives.add(Some(vec![bob_did]));
    let main = perspectives.add(None);
    let id = create(user_ctx(ALICE), &[&alices.uuid]).await.unwrap();
    let before = stored(&id);

    let err = call(
        "runtime.updateNotification",
        update_params(&id, &[&alices.uuid, &bobs.uuid]),
        user_ctx(ALICE),
    )
    .await
    .expect_err("Alice must not add Bob's perspective");
    assert_eq!(err.code, 403, "{}", err.message);

    // The check is on the notification's owner, not on the caller: the
    // operator may read the main agent's perspective, Alice may not.
    let err = call(
        "runtime.updateNotification",
        update_params(&id, &[&main.uuid]),
        admin_ctx(),
    )
    .await
    .expect_err("an operator must not point Alice's notification at other data");
    assert_eq!(err.code, 403, "{}", err.message);

    assert_eq!(stored(&id), before);
}

#[tokio::test]
async fn update_and_delete_of_an_unknown_notification_is_not_found() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let missing = Uuid::new_v4().to_string();

    let err = call(
        "runtime.updateNotification",
        update_params(&missing, &[&main.uuid]),
        admin_ctx(),
    )
    .await
    .expect_err("update of an unknown id");
    assert_eq!(err.code, 404, "{}", err.message);

    let err = call(
        "runtime.deleteNotification",
        json!({ "id": missing }),
        admin_ctx(),
    )
    .await
    .expect_err("delete of an unknown id");
    assert_eq!(err.code, 404, "{}", err.message);
}

#[tokio::test]
async fn operator_may_update_and_delete_any_notification() {
    setup();
    let alice_did = user(ALICE);
    let mut perspectives = Perspectives::default();
    let alices = perspectives.add(Some(vec![alice_did]));
    let id = create(user_ctx(ALICE), &[&alices.uuid]).await.unwrap();

    let mut params = update_params(&id, &[&alices.uuid]);
    params["description"] = json!("edited by the operator");
    call("runtime.updateNotification", params, admin_ctx())
        .await
        .expect("operator updates a user's notification");
    assert_eq!(stored(&id).unwrap().description, "edited by the operator");

    call(
        "runtime.deleteNotification",
        json!({ "id": id }),
        admin_ctx(),
    )
    .await
    .expect("operator deletes a user's notification");
    assert_eq!(stored(&id), None);
}

// An app token with AGENT_UPDATE could approve the notification it had just created, so the
// operator's approval meant nothing.
#[tokio::test]
async fn only_the_admin_credential_grants_notifications() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let app = app_ctx(vec![ALL_CAPABILITY.clone()]);
    let id = create(app.clone(), &[&main.uuid]).await.unwrap();

    user(ALICE);
    for (who, ctx) in [
        ("an app granting its own notification", app),
        ("a managed user", user_ctx(ALICE)),
    ] {
        let err = call(
            "runtime.grantNotification",
            json!({ "id": id, "granted": true }),
            ctx,
        )
        .await
        .expect_err(who);
        assert_eq!(
            (err.code, err.message.as_str()),
            (403, crate::api::guards::OPERATOR_ONLY),
            "{who}"
        );
    }
    assert!(!stored(&id).unwrap().granted);

    call(
        "runtime.grantNotification",
        json!({ "id": id, "granted": true }),
        admin_ctx(),
    )
    .await
    .expect("the operator grants it");
    assert!(stored(&id).unwrap().granted);
}

// Listing returns the caller's own notifications: a user's, or the main agent's for the operator.
#[tokio::test]
async fn each_caller_lists_only_its_own_notifications() {
    setup();
    let alice_did = user(ALICE);
    user(BOB);
    let mut perspectives = Perspectives::default();
    let alices = perspectives.add(Some(vec![alice_did]));
    let main = perspectives.add(None);
    let alice_id = create(user_ctx(ALICE), &[&alices.uuid]).await.unwrap();
    let main_id = create(admin_ctx(), &[&main.uuid]).await.unwrap();

    let ids = |listed: Value| -> Vec<String> {
        let mut ids: Vec<String> = listed
            .as_array()
            .expect("list")
            .iter()
            .map(|n| n["id"].as_str().unwrap().to_string())
            .filter(|id| *id == alice_id || *id == main_id)
            .collect();
        ids.sort();
        ids
    };
    let listed = |ctx| call("runtime.notifications", json!({}), ctx);
    assert_eq!(
        ids(listed(user_ctx(ALICE)).await.unwrap()),
        vec![alice_id.clone()]
    );
    assert_eq!(
        ids(listed(user_ctx(BOB)).await.unwrap()),
        Vec::<String>::new()
    );
    assert_eq!(
        ids(listed(admin_ctx()).await.unwrap()),
        vec![main_id.clone()]
    );
    assert_eq!(
        ids(listed(app_ctx(vec![AGENT_UPDATE_CAPABILITY.clone()]))
            .await
            .unwrap()),
        vec![main_id]
    );
}

// `runtime.importData` stores a notification's `granted` and `owner_did` as they are in the
// file. With AGENT_UPDATE alone, Alice could import a granted notification on Bob's
// perspective and receive Bob's links at her webhook.
#[tokio::test]
async fn only_the_admin_credential_imports_data() {
    setup();
    user(ALICE);
    let bob_did = user(BOB);
    let mut perspectives = Perspectives::default();
    let bobs = perspectives.add(Some(vec![bob_did.clone()]));
    let id = Uuid::new_v4().to_string();
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("import.json");
    std::fs::write(
        &file,
        json!({
            "notifications": [{
                "id": id, "description": "", "app_name": "", "app_url": "", "app_icon_path": "",
                "trigger": TRIGGER,
                "perspective_ids": serde_json::to_string(&[&bobs.uuid]).unwrap(),
                "webhook_url": "https://attacker.test/collect", "webhook_auth": "",
                "granted": true,
                "owner_did": bob_did,
            }],
        })
        .to_string(),
    )
    .unwrap();
    let params = json!({ "type": "db", "filePath": file.to_str().unwrap() });

    // Both callers hold AGENT_UPDATE, and an app every capability: only the admin credential
    // imports.
    for (who, ctx) in [
        ("a managed user", user_ctx(ALICE)),
        (
            "an app token with AGENT_UPDATE",
            app_ctx(vec![AGENT_UPDATE_CAPABILITY.clone()]),
        ),
        (
            "an app token with every capability",
            app_ctx(vec![ALL_CAPABILITY.clone()]),
        ),
    ] {
        let err = call("runtime.importData", params.clone(), ctx)
            .await
            .expect_err(who);
        assert_eq!(
            (err.code, err.message.as_str()),
            (403, crate::api::guards::OPERATOR_ONLY),
            "{who}"
        );
        assert_eq!(stored(&id), None, "{who} imported a notification");
    }

    call("runtime.importData", params, admin_ctx())
        .await
        .expect("the operator imports");
    let imported = stored(&id).expect("imported notification");
    assert!(imported.granted);
    assert_eq!(imported.owner_did, bob_did);
}

/// Fires a notification of `owner` through the delivery's publisher. The webhook URL does
/// not parse, so nothing is posted.
async fn fire(owner: &str) {
    fire_to(owner, "not a url").await;
}

/// Fires a notification of `owner` whose webhook is `webhook_url`.
async fn fire_to(owner: &str, webhook_url: &str) {
    let notification = Notification {
        id: Uuid::new_v4().to_string(),
        granted: true,
        description: String::new(),
        app_name: String::new(),
        app_url: String::new(),
        app_icon_path: String::new(),
        trigger: TRIGGER.to_string(),
        perspective_ids: vec!["perspective".to_string()],
        webhook_url: webhook_url.to_string(),
        webhook_auth: WEBHOOK_AUTH.into(),
        owner_did: owner.to_string(),
    };
    let matches = std::collections::BTreeMap::from([(notification, vec![json!({ "m": 1 })])]);
    PerspectiveInstance::publish_notification_matches("perspective".to_string(), matches).await;
}

/// A session of the managed user `email`, whose DID is `did`.
fn user_session(email: &str, did: &str) -> Arc<Session> {
    let user = Principal::User {
        token_id: crate::agent::session::TokenId(format!("notifications-{email}")),
        email: email.to_string(),
        did: did.to_string(),
        caps: get_user_default_capabilities(),
    };
    Arc::new(Session::for_test(user, ListenerReach::Network))
}

/// The next `notification-triggered` message on `stream`, if one arrives in time.
async fn next<S: futures::Stream<Item = String> + Unpin>(stream: &mut S) -> Option<String> {
    next_containing(stream, "notification-triggered").await
}

/// The next message on `stream` that contains `needle`, if one arrives in time.
async fn next_containing<S: futures::Stream<Item = String> + Unpin>(
    stream: &mut S,
    needle: &str,
) -> Option<String> {
    let notification = async {
        while let Some(msg) = stream.next().await {
            if msg.contains(needle) {
                return Some(msg);
            }
        }
        None
    };
    tokio::time::timeout(std::time::Duration::from_millis(1500), notification)
        .await
        .ok()
        .flatten()
}

// The owner filter in `build_event_stream` was pinned only as a pure function. Replacing the
// stream's guard with `true` sent every triggered notification, webhook secret included, to
// every session, and no test failed. The publisher now names the owner as the audience
// (`matches_notification_owner` before); this goes through the publisher and real streams.
#[tokio::test]
async fn triggered_event_reaches_only_the_owners_stream() {
    setup();
    let alice_did = user(ALICE);
    let bob_did = user(BOB);
    let all = crate::api::event_interest::watching_all;
    let operator = Arc::new(Session::for_test(
        Principal::Operator,
        ListenerReach::Loopback,
    ));
    let alice_session = user_session(ALICE, &alice_did);
    let mut alice = build_event_stream(alice_session, all(), None).await;
    let mut bob = build_event_stream(user_session(BOB, &bob_did), all(), None).await;
    let mut main = build_event_stream(operator, all(), None).await;

    fire(&alice_did).await;
    let a = next(&mut alice).await;
    assert!(
        a.as_deref()
            .is_some_and(|e| e.contains("notification-triggered")),
        "owner stream got {a:?}"
    );
    let a = a.unwrap();
    assert!(
        !a.contains(WEBHOOK_AUTH),
        "the event carried the credential: {a}"
    );
    assert!(a.contains("hasWebhookAuth"), "{a}");
    assert_eq!(next(&mut bob).await, None, "Bob's stream got Alice's event");
    assert_eq!(next(&mut main).await, None, "main stream got Alice's event");

    fire(&main_agent_did()).await;
    let m = next(&mut main).await;
    assert!(
        m.as_deref()
            .is_some_and(|e| e.contains("notification-triggered")),
        "main stream got {m:?}"
    );
    assert_eq!(
        next(&mut alice).await,
        None,
        "Alice's stream got the main agent's event"
    );
}

// The install request reaches every approver's session, and the list reaches the owner.
// Both say whether a webhook credential is set; neither carries it.
#[tokio::test]
async fn the_install_request_and_the_list_never_carry_the_webhook_credential() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let operator = Arc::new(Session::for_test(
        Principal::Operator,
        ListenerReach::Loopback,
    ));
    let all = crate::api::event_interest::watching_all;
    let mut stream = build_event_stream(operator, all(), None).await;

    let id = create(admin_ctx(), &[&main.uuid]).await.unwrap();
    let request = next_containing(&mut stream, "INSTALL_NOTIFICATION_REQUEST")
        .await
        .expect("the operator gets the install request");
    assert!(request.contains(&id), "{request}");
    assert!(request.contains("hasWebhookAuth"), "{request}");
    assert!(!request.contains(WEBHOOK_AUTH), "{request}");

    let listed = call("runtime.notifications", json!({}), admin_ctx())
        .await
        .unwrap();
    let mine = listed
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["id"] == id.as_str())
        .expect("listed");
    assert_eq!(mine["hasWebhookAuth"], true);
    assert!(mine.get("webhookAuth").is_none(), "{mine}");
    assert!(!listed.to_string().contains(WEBHOOK_AUTH), "{listed}");

    // The executor still holds it, for the webhook.
    assert!(stored(&id)
        .unwrap()
        .webhook_auth
        .expose_for_compare(WEBHOOK_AUTH));
}

/// `update_params` with `webhookUrl` set, and `webhookAuth` replaced by `auth` (left out for
/// `None`), as a client that read the notification back sends it.
fn webhook_update(id: &str, perspective_id: &str, url: &str, auth: Option<&str>) -> Value {
    let mut params = update_params(id, &[perspective_id]);
    params["webhookUrl"] = json!(url);
    match auth {
        Some(auth) => params["webhookAuth"] = json!(auth),
        None => {
            params.as_object_mut().unwrap().remove("webhookAuth");
        }
    }
    params
}

fn stored_auth_is(id: &str, auth: &str) -> bool {
    stored(id).unwrap().webhook_auth.expose_for_compare(auth)
}

// Clients see only `hasWebhookAuth`, so an update that leaves out `webhookAuth` keeps the
// stored credential; `""` clears it and a value replaces it.
#[tokio::test]
async fn an_update_without_webhook_auth_keeps_the_stored_credential() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let id = create(admin_ctx(), &[&main.uuid]).await.unwrap();
    let update = |auth| webhook_update(&id, &main.uuid, "https://webhook.test", auth);

    let reply = call("runtime.updateNotification", update(None), admin_ctx())
        .await
        .unwrap();
    assert_eq!(reply, json!(true));
    assert!(stored_auth_is(&id, WEBHOOK_AUTH));

    // `null` reads as left out.
    let mut params = update(None);
    params["webhookAuth"] = Value::Null;
    call("runtime.updateNotification", params, admin_ctx())
        .await
        .unwrap();
    assert!(stored_auth_is(&id, WEBHOOK_AUTH));

    call(
        "runtime.updateNotification",
        update(Some("wh-replaced")),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert!(stored_auth_is(&id, "wh-replaced"));

    call("runtime.updateNotification", update(Some("")), admin_ctx())
        .await
        .unwrap();
    assert!(stored_auth_is(&id, ""));
    assert!(!stored(&id).unwrap().view().has_webhook_auth);
}

// The credential goes only to the webhook it was set for: moving the notification to
// another URL without sending it again would hand the stored credential to the new URL.
#[tokio::test]
async fn moving_the_webhook_needs_the_credential_again() {
    setup();
    let mut perspectives = Perspectives::default();
    let main = perspectives.add(None);
    let id = create(admin_ctx(), &[&main.uuid]).await.unwrap();
    let before = stored(&id);

    let err = call(
        "runtime.updateNotification",
        webhook_update(&id, &main.uuid, "https://attacker.test/collect", None),
        admin_ctx(),
    )
    .await
    .expect_err("the stored credential must not follow the URL");
    assert_eq!(err.code, 400, "{}", err.message);
    assert!(
        err.message.contains("send webhookAuth again"),
        "{}",
        err.message
    );
    assert!(!err.message.contains(WEBHOOK_AUTH), "{}", err.message);
    assert_eq!(stored(&id), before, "a refused update changes nothing");

    // The same URL, written differently, is not a move.
    call(
        "runtime.updateNotification",
        webhook_update(&id, &main.uuid, "HTTPS://webhook.test/", None),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert!(stored_auth_is(&id, WEBHOOK_AUTH));

    let reply = call(
        "runtime.updateNotification",
        webhook_update(&id, &main.uuid, "https://moved.test/hook", Some("wh-moved")),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert!(!reply.to_string().contains("wh-moved"), "{reply}");
    let moved = stored(&id).unwrap();
    assert_eq!(moved.webhook_url, "https://moved.test/hook");
    assert!(moved.webhook_auth.expose_for_compare("wh-moved"));

    // With no credential stored, there is nothing to send along: the URL may move freely.
    call(
        "runtime.updateNotification",
        webhook_update(&id, &main.uuid, "https://moved.test/hook", Some("")),
        admin_ctx(),
    )
    .await
    .unwrap();
    call(
        "runtime.updateNotification",
        webhook_update(&id, &main.uuid, "https://elsewhere.test/hook", None),
        admin_ctx(),
    )
    .await
    .unwrap();
    assert_eq!(
        stored(&id).unwrap().webhook_url,
        "https://elsewhere.test/hook"
    );
}

// Redaction stops at the executor's own outputs: the webhook still gets the credential as
// its bearer token. The body is the triggered event, so it shows only `hasWebhookAuth`.
#[tokio::test]
async fn a_firing_notification_still_sends_the_credential_to_its_webhook() {
    setup();
    let (posted, mut received) = tokio::sync::mpsc::unbounded_channel::<(String, String)>();
    let app = axum::Router::new().route(
        "/hook",
        axum::routing::post(
            move |headers: axum::http::HeaderMap, body: String| async move {
                let auth = headers
                    .get(axum::http::header::AUTHORIZATION)
                    .and_then(|v| v.to_str().ok())
                    .unwrap_or_default()
                    .to_string();
                let _ = posted.send((auth, body));
                "ok"
            },
        ),
    );
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });

    fire_to(&main_agent_did(), &format!("http://{addr}/hook")).await;

    let (auth, body) = tokio::time::timeout(std::time::Duration::from_secs(5), received.recv())
        .await
        .expect("the webhook was called")
        .unwrap();
    assert_eq!(auth, format!("Bearer {WEBHOOK_AUTH}"));
    assert!(!body.contains(WEBHOOK_AUTH), "{body}");
    assert!(body.contains("hasWebhookAuth"), "{body}");
}
