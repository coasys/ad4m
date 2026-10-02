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
    get_user_default_capabilities, perspective_query_capability, Capability, TokenCheck,
    AGENT_UPDATE_CAPABILITY, ALL_CAPABILITY,
};
use crate::agent::AgentService;
use crate::api::events_ws::build_event_stream;
use crate::api::ws_handler::{build_handler_map, WsRpcError};
use crate::db::Ad4mDb;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{register_perspective, unregister_perspective};
use crate::pubsub::{get_global_pubsub, RUNTIME_NOTIFICATION_TRIGGERED_TOPIC};
use crate::test_utils::setup_wallet;
use crate::types::{
    ExpressionProof, Link, LinkExpression, LinkStatus, Notification, NotificationInput,
    PerspectiveHandle, PerspectiveState, RequestContext, TriggeredNotification,
};

const TRIGGER: &str = "SELECT ?source ?target WHERE { ?source <test://notify> ?target }";
const ALICE: &str = "alice@notifications.test";
const BOB: &str = "bob@notifications.test";

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

fn context(
    capabilities: Vec<Capability>,
    is_admin_credential: bool,
    user_email: Option<&str>,
) -> RequestContext {
    RequestContext {
        capabilities: Ok(capabilities),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential,
        user_email: user_email.map(str::to_string),
        user_did: user_email.map(|email| AgentService::get_user_did_by_email(email).unwrap()),
        cancel_token: None,
    }
}

/// A managed user's session.
fn user_ctx(email: &str) -> RequestContext {
    context(get_user_default_capabilities(), false, Some(email))
}

/// The launcher's admin credential.
fn admin_ctx() -> RequestContext {
    context(vec![ALL_CAPABILITY.clone()], true, None)
}

/// A main-agent app token that holds only `capabilities`.
fn app_ctx(capabilities: Vec<Capability>) -> RequestContext {
    context(capabilities, false, None)
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
        "webhookAuth": "secret",
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
                webhook_auth: "secret".to_string(),
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

    let err = call(
        "runtime.grantNotification",
        json!({ "id": id, "granted": true }),
        app,
    )
    .await
    .expect_err("an app must not grant its own notification");
    assert_eq!(err.code, 403);
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

// A managed user's session without a DID must not fall back to the main agent's.
#[tokio::test]
async fn a_user_session_without_a_did_is_refused() {
    setup();
    let mut ctx = context(get_user_default_capabilities(), false, None);
    ctx.user_email = Some("nobody@notifications.test".to_string());
    let err = call("runtime.notifications", json!({}), ctx)
        .await
        .expect_err("no DID, no notifications");
    assert_eq!(err.code, 403);
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

    for (who, ctx) in [
        ("a managed user", user_ctx(ALICE)),
        (
            "an app token with AGENT_UPDATE",
            app_ctx(vec![AGENT_UPDATE_CAPABILITY.clone()]),
        ),
    ] {
        let err = call("runtime.importData", params.clone(), ctx)
            .await
            .expect_err(who);
        assert_eq!(err.code, 403, "{who}");
        // Both callers hold AGENT_UPDATE: the admin check refuses them, not the capability check.
        assert!(err.message.contains("admin credential"), "{who}: {err:?}");
        assert_eq!(stored(&id), None, "{who} imported a notification");
    }

    call("runtime.importData", params, admin_ctx())
        .await
        .expect("the operator imports");
    let imported = stored(&id).expect("imported notification");
    assert!(imported.granted);
    assert_eq!(imported.owner_did, bob_did);
}

/// Serialises the event that delivery publishes for a notification of `owner`.
fn triggered(owner: &str) -> String {
    serde_json::to_string(&TriggeredNotification {
        notification: Notification {
            id: Uuid::new_v4().to_string(),
            granted: true,
            description: String::new(),
            app_name: String::new(),
            app_url: String::new(),
            app_icon_path: String::new(),
            trigger: TRIGGER.to_string(),
            perspective_ids: vec![],
            webhook_url: "https://webhook.test".to_string(),
            webhook_auth: "secret".to_string(),
            owner_did: owner.to_string(),
        },
        perspective_id: "perspective".to_string(),
        trigger_match: "[]".to_string(),
    })
    .unwrap()
}

async fn next<S: futures::Stream<Item = String> + Unpin>(stream: &mut S) -> Option<String> {
    tokio::time::timeout(std::time::Duration::from_millis(1500), stream.next())
        .await
        .ok()
        .flatten()
}

// The owner filter in `build_event_stream` was pinned only as a pure function. Replacing the
// stream's guard with `true` sent every triggered notification, webhook secret included, to
// every session, and no test failed.
#[tokio::test]
async fn triggered_event_reaches_only_the_owners_stream() {
    setup();
    let alice_did = user(ALICE);
    user(BOB);
    let check = || TokenCheck::new("", true);
    let mut alice = build_event_stream(check(), Some(ALICE.to_string()), false).await;
    let mut bob = build_event_stream(check(), Some(BOB.to_string()), false).await;
    let mut main = build_event_stream(check(), None, true).await;
    let pubsub = get_global_pubsub().await;

    pubsub
        .publish(
            &RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
            &triggered(&alice_did),
        )
        .await;
    let a = next(&mut alice).await;
    assert!(
        a.as_deref()
            .is_some_and(|e| e.contains("notification-triggered")),
        "owner stream got {a:?}"
    );
    assert_eq!(next(&mut bob).await, None, "Bob's stream got Alice's event");
    assert_eq!(next(&mut main).await, None, "main stream got Alice's event");

    pubsub
        .publish(
            &RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
            &triggered(&main_agent_did()),
        )
        .await;
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
