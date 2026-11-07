//! The bot decision: what each method does for a grant, the grant's rules, the log's ceiling,
//! the reason a write gives, and the activity entry.

use serde_json::json;

use super::*;
use crate::api::ws_handler::build_handler_map;
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
fn an_activity_entry_names_the_action_the_space_the_reason_and_the_outcome() {
    let row = bot("active", &["write"], &["p1"], None);
    let write = action(BotAction::Write, in_space("p1"));
    let allowed = log::entry(&row, "perspective.addLink", &write, Some("why"), &Ok(()), 5);
    assert_eq!(allowed.bot_did, BOT);
    assert_eq!(allowed.action.as_deref(), Some("write"));
    assert_eq!(allowed.space.as_deref(), Some("p1"));
    assert_eq!(allowed.reason.as_deref(), Some("why"));
    assert_eq!(allowed.outcome, log::ALLOWED);
    assert_eq!(allowed.at, 5);
    let refusal = decide_grant(&row, &Reach::Outside, None, 5).unwrap_err();
    let refused = log::entry(&row, "user.create", &Reach::Outside, None, &Err(refusal), 5);
    assert_eq!(refused.action, None);
    assert_eq!(refused.outcome, BOT_GRANT);
}
