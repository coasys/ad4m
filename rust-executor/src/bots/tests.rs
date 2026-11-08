//! The bot decision: what each method does for a grant, the grant's rules, the log's ceiling,
//! the reason a write gives, and the activity entry.

use serde_json::json;

use super::*;
use crate::api::ws_handler::{build_handler_map, HandlerMap, NoParams, WsFn};
use crate::db::BotGrantRow;

const BOT: &str = "did:key:bot";

fn bot(state: &str, can: &[&str], spaces: &[&str], expires: Option<u64>) -> BotRow {
    BotRow {
        did: BOT.to_string(),
        username: "bot-1@bots.ad4m".to_string(),
        name: "helper".to_string(),
        owner: "scid1".to_string(),
        offer_id: "offer1".to_string(),
        created_at: 1,
        grant: BotGrantRow {
            spaces: spaces.iter().map(|s| s.to_string()).collect(),
            can: can.iter().map(|s| s.to_string()).collect(),
            expires,
            state: state.to_string(),
            version: 1,
            updated_at: 1,
        },
    }
}

fn reach_of(method: &str, params: Value) -> Reach {
    let map = build_handler_map();
    let guard = map
        .guards()
        .into_iter()
        .find(|(name, _)| *name == method)
        .and_then(|(_, guard)| guard.cloned())
        .unwrap_or_else(|| panic!("{method} is not registered"));
    reach(method, &guard, &params)
}

fn action(action: BotAction, space: Option<Space>) -> Reach {
    Reach::Action { action, space }
}

fn in_space(uuid: &str) -> Option<Space> {
    Some(Space::Perspective(uuid.to_string()))
}

#[test]
fn reach_reads_what_a_method_does_from_its_guard() {
    let link = json!({"source": "a", "target": "b"});
    assert_eq!(
        reach_of("perspective.addLink", json!({"uuid": "p1", "link": link})),
        action(BotAction::Write, in_space("p1"))
    );
    assert_eq!(
        reach_of("perspective.queryLinks", json!({"uuid": "p1", "query": {}})),
        action(BotAction::Read, in_space("p1"))
    );
    assert_eq!(
        reach_of("neighbourhood.join", json!({"url": "neighbourhood://Qm"})),
        action(
            BotAction::Join,
            Some(Space::Url("neighbourhood://Qm".to_string()))
        )
    );
    assert_eq!(
        reach_of("expression.get", json!({"url": "lang://x"})),
        action(BotAction::Read, None)
    );
    assert_eq!(reach_of("agent.get", json!({})), Reach::Own);
    assert_eq!(reach_of("runtime.info", json!({})), Reach::Own);
    for outside in [
        "perspective.remove",
        "perspective.update",
        "perspective.publishSnapshot",
        "perspective.create",
        "neighbourhood.publish",
        "user.create",
        "agent.updateProfile",
        "bot.setGrant",
        "identity.publish",
    ] {
        assert_eq!(
            reach_of(outside, json!({"uuid": "p1"})),
            Reach::Outside,
            "{outside}"
        );
    }
    // A perspective method without its id names no space it could act in.
    assert_eq!(reach_of("perspective.addLink", json!({})), Reach::Outside);
}

#[test]
fn the_grant_decides_in_its_order() {
    let write = action(BotAction::Write, in_space("p1"));
    let active = bot("active", &["read", "write"], &["p1"], None);
    assert_eq!(decide_grant(&active, &write, None, 10), Ok(()));
    assert_eq!(decide_grant(&active, &Reach::Own, None, 10), Ok(()));
    assert_eq!(
        decide_grant(&active, &Reach::Outside, None, 10)
            .unwrap_err()
            .code,
        BOT_GRANT
    );
    // Another space; the same space by its neighbourhood URL.
    let other = action(BotAction::Write, in_space("p2"));
    assert_eq!(
        decide_grant(&active, &other, None, 10).unwrap_err().code,
        BOT_GRANT
    );
    let by_url = bot("active", &["write"], &["neighbourhood://Qm"], None);
    assert_eq!(
        decide_grant(&by_url, &other, Some("neighbourhood://Qm"), 10),
        Ok(())
    );
    // An action outside `can`.
    let reader = bot("active", &["read"], &["p1"], None);
    assert_eq!(
        decide_grant(&reader, &write, None, 10).unwrap_err().code,
        NOT_IN_CAN
    );
    // Paused: its own session still answers.
    let paused = bot("paused", &["read", "write"], &["p1"], None);
    let refused = decide_grant(&paused, &write, None, 10).unwrap_err();
    assert_eq!((refused.code, refused.status), (BOT_PAUSED, 403));
    assert!(refused.message.starts_with("bot-paused: "));
    assert_eq!(decide_grant(&paused, &Reach::Own, None, 10), Ok(()));
    // Expired.
    let expiring = bot("active", &["write"], &["p1"], Some(10));
    assert_eq!(decide_grant(&expiring, &write, None, 9), Ok(()));
    assert_eq!(
        decide_grant(&expiring, &write, None, 10).unwrap_err().code,
        BOT_EXPIRED
    );
    // Revoked: nothing, its own session included.
    let revoked = bot("revoked", &["read", "write"], &["p1"], None);
    for reach in [write, Reach::Own, Reach::Outside] {
        assert_eq!(
            decide_grant(&revoked, &reach, None, 10).unwrap_err().code,
            BOT_REVOKED
        );
    }
}

fn key(verdict: KeyVerdict, scope: Option<(&[&str], u64)>) -> Result<BotKey, BotKeyError> {
    Ok(BotKey {
        key_id: "d2".to_string(),
        verdict,
        scope: scope.map(|(can, expires)| BotScope {
            can: can.iter().map(|s| s.to_string()).collect(),
            expires,
        }),
    })
}

#[test]
fn the_owners_log_is_the_ceiling_of_every_grant() {
    let both: &[&str] = &["read", "write"];
    assert_eq!(
        decide_key(
            key(KeyVerdict::Valid, Some((both, 100))),
            BotAction::Write,
            10
        ),
        Ok(())
    );
    let code = |k, a| decide_key(k, a, 10).unwrap_err().code;
    assert_eq!(
        code(
            key(KeyVerdict::Valid, Some((&["read"], 100))),
            BotAction::Write
        ),
        NOT_IN_CAN
    );
    assert_eq!(
        code(key(KeyVerdict::Valid, Some((both, 10))), BotAction::Read),
        BOT_EXPIRED
    );
    assert_eq!(
        code(key(KeyVerdict::Expired, Some((both, 100))), BotAction::Read),
        BOT_EXPIRED
    );
    assert_eq!(
        code(key(KeyVerdict::Revoked, Some((both, 100))), BotAction::Read),
        BOT_REVOKED
    );
    assert_eq!(
        code(
            key(KeyVerdict::NotInLog, Some((both, 100))),
            BotAction::Read
        ),
        BOT_REVOKED
    );
    // A standing key is no bot's key.
    assert_eq!(
        code(key(KeyVerdict::Valid, None), BotAction::Read),
        BOT_UNBOUND
    );
    assert_eq!(
        code(Err(BotKeyError::NotListed), BotAction::Read),
        BOT_UNBOUND
    );
    let unavailable = decide_key(Err(BotKeyError::StoreUnavailable), BotAction::Read, 10);
    assert_eq!(unavailable.unwrap_err().status, 503);
}

#[test]
fn a_write_and_a_join_give_a_reason_and_a_read_need_not() {
    assert_eq!(decide_reason(BotAction::Read, None), Ok(()));
    for action in [BotAction::Write, BotAction::Join] {
        let refused = decide_reason(action, None).unwrap_err();
        assert_eq!((refused.code, refused.status), (REASON_REQUIRED, 400));
        assert_eq!(
            decide_reason(action, Some("  ")).unwrap_err().code,
            REASON_REQUIRED
        );
        assert_eq!(decide_reason(action, Some("tidy the board")), Ok(()));
    }
}

#[test]
fn the_reason_is_read_from_the_frames_meta_and_cut() {
    let frame = json!({"type": "perspective.addLink", "meta": {"reason": "sort the inbox"}});
    assert_eq!(reason_of(&frame).as_deref(), Some("sort the inbox"));
    assert_eq!(reason_of(&json!({"type": "x"})), None);
    assert_eq!(reason_of(&json!({"meta": {"reason": 3}})), None);
    let long = "é".repeat(REASON_MAX + 20);
    let cut = reason_of(&json!({"meta": {"reason": long}})).unwrap();
    assert_eq!(cut.chars().count(), REASON_MAX);
}

#[test]
fn the_log_names_a_perspective_by_its_neighbourhood_url_when_it_has_one() {
    let write = action(BotAction::Write, in_space("p1"));
    assert_eq!(
        as_logged(write.clone(), Some("neighbourhood://n1")),
        action(
            BotAction::Write,
            Some(Space::Url("neighbourhood://n1".into()))
        )
    );
    assert_eq!(as_logged(write.clone(), None), write);
    let join = action(
        BotAction::Join,
        Some(Space::Url("neighbourhood://n1".into())),
    );
    assert_eq!(as_logged(join.clone(), None), join);
    assert_eq!(
        as_logged(Reach::Own, Some("neighbourhood://n1")),
        Reach::Own
    );
}

#[test]
fn an_activity_entry_names_the_action_the_space_the_reason_and_the_outcome() {
    let row = bot("active", &["write"], &["p1"], None);
    let write = action(BotAction::Write, in_space("p1"));
    let allowed = log::entry(
        &row,
        "perspective.addLink",
        &write,
        Some("why"),
        &log::answered(Ok(())),
        5,
    );
    assert_eq!(allowed.bot_did, BOT);
    assert_eq!(allowed.action.as_deref(), Some("write"));
    assert_eq!(allowed.space.as_deref(), Some("p1"));
    assert_eq!(allowed.reason.as_deref(), Some("why"));
    assert_eq!(allowed.outcome, log::ALLOWED);
    assert_eq!(allowed.at, 5);
    let refusal = decide_grant(&row, &Reach::Outside, None, 5).unwrap_err();
    let refused = log::entry(
        &row,
        "user.create",
        &Reach::Outside,
        None,
        &log::refused(&refusal),
        5,
    );
    assert_eq!(refused.action, None);
    assert_eq!(refused.outcome, BOT_GRANT);
    let failed = WsRpcError {
        code: 404,
        message: "not found".into(),
    };
    assert_eq!(log::answered(Err(&failed)), "failed:404");
}

/// A bot's session on a connection, with a user's default capabilities: the grant, not the
/// token, bounds it.
fn bot_connection(
    row: BotRow,
) -> (
    crate::api::ws_rpc::Connection,
    tokio::sync::mpsc::UnboundedReceiver<String>,
) {
    use crate::agent::capabilities::get_user_default_capabilities;
    use crate::agent::session::{Principal, Session};
    use crate::agent::AgentContext;
    use crate::types::RequestContext;
    let did = row.did.clone();
    remember(row);
    let session = Session::for_test(Principal::Agent(AgentContext::managed(did.clone())))
        .with_caps(get_user_default_capabilities())
        .with_did(&did);
    let (tx, out) = tokio::sync::mpsc::unbounded_channel();
    let conn = crate::api::ws_rpc::Connection::new(
        std::sync::Arc::new(build_handler_map()),
        std::sync::Arc::new(RequestContext::for_session(session)),
        tx,
    );
    (conn, out)
}

/// `events.watch` is answered on the connection before the handler map, so `admit` never
/// sees it: a bot watches only perspectives of its grant, as reads there. A join-only bot
/// watches nothing; a reader watches its space (the grant passes; this executor has no owner
/// log to read, so the log's half answers 503), not another space and not every perspective.
#[tokio::test]
async fn a_bot_watches_only_the_perspectives_of_its_grant() {
    let answer = |out: &mut tokio::sync::mpsc::UnboundedReceiver<String>| -> Value {
        serde_json::from_str(&out.try_recv().expect("an inline reply")).unwrap()
    };
    let code = |reply: &Value| {
        (
            reply["error"]["code"].as_u64().unwrap_or(0),
            reply["error"]["message"]
                .as_str()
                .unwrap_or("")
                .split(':')
                .next()
                .unwrap_or("")
                .to_string(),
        )
    };

    let (conn, mut out) = bot_connection(bot("active", &["join"], &["p1"], None));
    conn.handle_text(
        &json!({"id": "1", "type": "events.watch", "params": {"link-added": ["p1"]}}).to_string(),
    )
    .await;
    assert_eq!(
        code(&answer(&mut out)),
        (403, NOT_IN_CAN.to_string()),
        "join only"
    );

    let (conn, mut out) = bot_connection(bot("active", &["read"], &["p1"], None));
    conn.handle_text(
        &json!({"id": "2", "type": "events.watch", "params": {"link-added": ["p2"]}}).to_string(),
    )
    .await;
    assert_eq!(
        code(&answer(&mut out)),
        (403, BOT_GRANT.to_string()),
        "another space"
    );
    conn.handle_text(
        &json!({"id": "3", "type": "events.watch", "params": {"link-added": null}}).to_string(),
    )
    .await;
    assert_eq!(
        code(&answer(&mut out)),
        (403, BOT_GRANT.to_string()),
        "every perspective"
    );
    conn.handle_text(
        &json!({"id": "4", "type": "events.watch", "params": {"link-added": ["p1"]}}).to_string(),
    )
    .await;
    assert_eq!(
        code(&answer(&mut out)),
        (503, "store-unavailable".to_string()),
        "its space: the grant passed, the owner's log is not readable here"
    );
    conn.handle_text(&json!({"id": "5", "type": "events.unwatch", "params": {}}).to_string())
        .await;
    assert_eq!(answer(&mut out)["result"], json!(true), "unwatch");
}

/// A session ctx for the bot `row` (its managed agent, a user's default capabilities).
fn bot_ctx(row: BotRow) -> std::sync::Arc<crate::types::RequestContext> {
    use crate::agent::capabilities::get_user_default_capabilities;
    use crate::agent::session::{Principal, Session};
    use crate::agent::AgentContext;
    use crate::types::RequestContext;
    let did = row.did.clone();
    remember(row);
    set_loaded(true);
    let session = Session::for_test(Principal::Agent(AgentContext::managed(did.clone())))
        .with_caps(get_user_default_capabilities())
        .with_did(&did);
    let mut ctx = RequestContext::for_session(session);
    ctx.reason = Some("meet".to_string());
    std::sync::Arc::new(ctx)
}

/// A handler map with `neighbourhood.join` (a join is a logged bot action) answered by
/// `handler`, guarded as the real method is. The activity the tests then read back is
/// written at dispatch.
fn join_map(handler: impl WsFn) -> HandlerMap {
    let mut map = HandlerMap::new();
    map.method::<NoParams, Value>("neighbourhood.join", handler)
        .guard(Guard::Cap(
            crate::agent::capabilities::NEIGHBOURHOOD_READ_CAPABILITY.clone(),
        ));
    map
}

/// A bot whose grant and key allow `join` of `neighbourhood://n1`.
fn join_bot() -> BotRow {
    let row = bot("active", &["join"], &["neighbourhood://n1"], None);
    set_test_key(
        &row.did,
        Ok(BotKey {
            key_id: "d2".to_string(),
            verdict: KeyVerdict::Valid,
            scope: Some(BotScope {
                can: vec!["join".to_string()],
                expires: u64::MAX,
            }),
        }),
    );
    row
}

/// The bot's activity rows, newest first.
fn activity(did: &str) -> Vec<crate::db::BotActivityRow> {
    crate::db::Ad4mDb::with_global_instance(|db| db.bot_activity(did, None, 10).unwrap())
}

/// An admitted call the dispatcher then answers `allowed` settles its attempt entry: one
/// row (`started`, then `allowed`), not two.
#[tokio::test]
async fn an_answered_call_settles_its_attempt_entry() {
    crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
    let row = join_bot();
    let ctx = bot_ctx(row.clone());
    let map = join_map(|_, _| async { Ok(Value::Null) });
    let params = json!({"url": "neighbourhood://n1", "meta": {"reason": "meet"}});
    map.dispatch("neighbourhood.join", params, ctx)
        .await
        .unwrap();

    let entries = activity(BOT);
    assert_eq!(entries.len(), 1, "{entries:?}");
    let entry = &entries[0];
    assert_eq!(entry.outcome, log::ALLOWED);
    assert_eq!(entry.method, "neighbourhood.join");
    assert_eq!(entry.action.as_deref(), Some("join"));
    assert_eq!(entry.space.as_deref(), Some("neighbourhood://n1"));
    assert_eq!(entry.reason.as_deref(), Some("meet"));
}

/// A dispatch dropped after admission (a cancelled call) settles the entry `interrupted`:
/// the call never answered, and part of its work may have applied.
#[tokio::test]
async fn a_cancelled_call_leaves_an_interrupted_entry() {
    crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
    let row = join_bot();
    let ctx = bot_ctx(row.clone());
    let ran = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let ran2 = ran.clone();
    let map = join_map(move |_, _| {
        ran2.store(true, std::sync::atomic::Ordering::SeqCst);
        std::future::pending()
    });
    let params = json!({"url": "neighbourhood://n1", "meta": {"reason": "go"}});
    let mut call = Box::pin(map.dispatch("neighbourhood.join", params, ctx));
    // One poll admits the call and starts its handler; the handler then pends.
    let waker = futures::task::noop_waker();
    let mut task_cx = std::task::Context::from_waker(&waker);
    assert!(std::future::Future::poll(call.as_mut(), &mut task_cx).is_pending());
    assert!(ran.load(std::sync::atomic::Ordering::SeqCst));
    drop(call);

    let entries = activity(BOT);
    assert_eq!(entries.len(), 1, "{entries:?}");
    assert_eq!(entries[0].outcome, log::INTERRUPTED);
}

/// A handler that panics unwinds through the dispatch; the dropped `Pending` settles the
/// entry `interrupted` (indeterminate), never `allowed`.
#[tokio::test]
async fn a_panicking_handler_leaves_an_interrupted_entry() {
    crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
    let row = join_bot();
    let ctx = bot_ctx(row.clone());
    let map = join_map(|_, _| async { panic!("the handler panicked") });
    let params = json!({"url": "neighbourhood://n1", "meta": {"reason": "go"}});
    let joined =
        tokio::spawn(async move { map.dispatch("neighbourhood.join", params, ctx).await }).await;
    assert!(joined.unwrap_err().is_panic());

    let entries = activity(BOT);
    assert_eq!(entries.len(), 1, "{entries:?}");
    assert_eq!(entries[0].outcome, log::INTERRUPTED);
}

/// When the attempt entry cannot be written the call is refused and does not run: a bot's
/// write or join never runs unlogged (fail closed).
#[tokio::test]
async fn an_unloggable_call_is_refused() {
    crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
    let row = join_bot();
    let ctx = bot_ctx(row.clone());
    let ran = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let ran2 = ran.clone();
    let map = join_map(move |_, _| {
        ran2.store(true, std::sync::atomic::Ordering::SeqCst);
        async { Ok(Value::Null) }
    });
    let params = json!({"url": "neighbourhood://n1", "meta": {"reason": "go"}});
    log::set_fail_writes(true);
    let refused = map.dispatch("neighbourhood.join", params, ctx).await;
    log::set_fail_writes(false);
    let error = refused.unwrap_err();
    assert_eq!(error.code, 503);
    assert!(error.message.starts_with("log-unavailable: "), "{error:?}");
    assert!(
        !ran.load(std::sync::atomic::Ordering::SeqCst),
        "the handler did not run"
    );
    assert!(activity(BOT).is_empty());
}

/// Until the registry has loaded, a managed agent cannot be told a bot from a user, so its
/// session is refused rather than served with a user's authority.
#[tokio::test]
async fn a_managed_session_is_refused_until_the_registry_has_loaded() {
    use crate::agent::capabilities::get_user_default_capabilities;
    use crate::agent::session::{Principal, Session};
    use crate::agent::AgentContext;
    use crate::types::RequestContext;
    let did = "did:key:some-user";
    let session = Session::for_test(Principal::Agent(AgentContext::managed(did.to_string())))
        .with_caps(get_user_default_capabilities())
        .with_did(did);
    let ctx = RequestContext::for_session(session);
    let map = build_handler_map();

    set_loaded(false);
    let unloaded = classify(did);
    let call = map.admit("agent.get", &json!({}), &ctx).await;
    set_loaded(true);

    let refused = unloaded.unwrap_err();
    assert_eq!(refused.code, 503);
    assert!(
        refused.message.starts_with("bots-unavailable: "),
        "{refused:?}"
    );
    assert_eq!(call.unwrap_err().code, 503);
    assert_eq!(classify(did).unwrap(), None);
    assert!(map.admit("agent.get", &json!({}), &ctx).await.is_ok());
}
