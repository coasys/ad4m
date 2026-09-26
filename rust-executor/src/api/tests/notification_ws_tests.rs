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

use std::sync::Arc;

use serde_json::{json, Value};
use uuid::Uuid;

use crate::agent::capabilities::{
    get_user_default_capabilities, perspective_query_capability, Capability,
    AGENT_UPDATE_CAPABILITY, ALL_CAPABILITY,
};
use crate::agent::AgentService;
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

/// Writes a notification row directly, as any write path could have left it.
fn store(perspective_ids: &[&str], user_email: Option<&str>, granted: bool) -> String {
    let id = Ad4mDb::with_global_instance(|db| {
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
            user_email.map(str::to_string),
        )
    })
    .expect("add notification");
    let mut notification = stored(&id).expect("stored notification");
    notification.granted = granted;
    Ad4mDb::with_global_instance(|db| db.update_notification(id.clone(), &notification))
        .expect("set granted");
    id
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
    let alices = perspectives.add(Some(vec![alice_did]));
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
    assert_eq!(notification.user_email.as_deref(), Some(ALICE));
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
    let alices = perspectives.add(Some(vec![alice_did]));

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
    params["userEmail"] = json!(null);
    call(
        "runtime.updateNotification",
        params.clone(),
        user_ctx(ALICE),
    )
    .await
    .unwrap();
    let notification = stored(&alice_id).unwrap();
    assert_eq!(notification.user_email.as_deref(), Some(ALICE));
    assert!(notification.granted);
    assert_eq!(notification.description, "edited");

    // An operator's update keeps the owner and drops the user's grant.
    call("runtime.updateNotification", params, admin_ctx())
        .await
        .unwrap();
    let notification = stored(&alice_id).unwrap();
    assert_eq!(notification.user_email.as_deref(), Some(ALICE));
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
