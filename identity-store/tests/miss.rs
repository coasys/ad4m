//! What a miss in the held folds costs, and the per-log budget that bounds it.
//!
//! A miss rebuilds a log's fold from `chain_entries` with the core's `fold_stored`: the store
//! verified every stored event when it appended it, so the miss verifies no signature (the test
//! replaces every stored proof's signature and the miss still folds). An append past the per-log
//! budget (`Settings::log_fold_bytes`, by `held::fold_bytes_of`: the live part of the fold within
//! it, the whole fold within twice it) is `400 log-too-large` and stores nothing, so every log
//! the store takes can be held and folds in bounded time, and revoking the keys a thief padded
//! the log with frees the budget for the owner.

mod common;

use ad4m_hash_chain::{canonical_json, encode_proof_value};
use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{BotScope, Event};
use axum::http::StatusCode;
use identity_store::held::{fold_bytes_of, held_bytes_of};
use identity_store::routes::MAX_BODY;
use identity_store::{Settings, DEFAULT_HELD_LOG_BYTES, DEFAULT_LOG_FOLD_BYTES};
use serde_json::{json, Value};

use common::agent::{key, Device, Person};
use common::{Reply, TestStore};

fn store_with(held_log_bytes: usize, log_fold_bytes: usize) -> TestStore {
    TestStore::with_settings(Settings {
        pow_bits: 8,
        held_log_bytes,
        log_fold_bytes,
        ..Settings::default()
    })
}

/// `POST /v1/notes/{scid}` signed by `d1`: `503 vouch-unavailable` once the request passes.
async fn note_by_d1(t: &TestStore, person: &Person) -> Reply {
    let path = format!("/v1/notes/{}", person.scid());
    let body = serde_json::to_vec(&json!({ "token": "a-token" })).unwrap();
    let header = person.request(
        "d1",
        &person.device_key(1),
        "POST",
        &path,
        &body,
        t.now() + 60,
    );
    t.send("POST", &path, Some(body), Some(header)).await
}

/// `GET /v1/offers/{scid}` signed by the current master.
async fn offers_by_master(t: &TestStore, person: &Person) -> Reply {
    let path = format!("/v1/offers/{}", person.scid());
    let header = person.master_request("GET", &path, b"", t.now() + 60);
    t.send("GET", &path, None, Some(header)).await
}

/// A `scope` of `bot_id` by the current master naming `read` as often as one append body holds.
fn widest_scope(person: &Person, bot_id: &str, expires: u64) -> Event {
    let mut repeats = 9_400;
    loop {
        let scope = BotScope {
            can: vec!["read".into(); repeats],
            expires,
        };
        let event = build::scope(
            &person.chain,
            &person.current_master(),
            &bot_id.to_string(),
            &scope,
        )
        .unwrap();
        let body = serde_json::to_vec(&json!({ "events": [&event] })).unwrap();
        if body.len() <= MAX_BODY {
            return event;
        }
        repeats -= 50;
    }
}

#[tokio::test]
async fn a_miss_verifies_no_stored_signature_and_an_append_still_verifies_its_events() {
    // A bound of one byte: nothing is held, so every request on alice's log is a miss.
    let t = store_with(1, DEFAULT_LOG_FOLD_BYTES);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let scope = |expires| BotScope {
        can: vec!["read".into()],
        expires,
    };
    let bot_id = t
        .link(
            &mut alice,
            &bot,
            OfferKind::Bot,
            Some(scope(t.now() + 86_400)),
        )
        .await;
    let phone = Device::new("alice phone");
    let phone_id = t.link(&mut alice, &phone, OfferKind::Device, None).await;
    let rescope = build::scope(
        &alice.chain,
        &alice.current_master(),
        &bot_id,
        &scope(t.now() + 2 * 86_400),
    )
    .unwrap();
    let reply = t.append_push(&mut alice, rescope).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let revoke = alice.revoke(&[phone_id.as_str()]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let after = alice.revoke(&[bot_id.as_str()]);
    assert_eq!(alice.head_seq(), 5);

    // Every stored proof's signature replaced by one over other bytes. The proof's hash leaves
    // out `proof`, so the stored hashes and links still hold; only a signature check could tell.
    let wrong = encode_proof_value(&Signer::sign(&key("mallory"), b"not this event"));
    {
        let conn = t.store.db.lock().unwrap();
        let rows: Vec<(i64, String)> = conn
            .prepare("SELECT seq, json FROM chain_entries WHERE chain = ?1")
            .unwrap()
            .query_map([alice.scid()], |r| Ok((r.get(0)?, r.get(1)?)))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert_eq!(rows.len(), 6);
        for (seq, text) in rows {
            let mut event: Value = serde_json::from_str(&text).unwrap();
            for proof in event["proof"].as_array_mut().unwrap() {
                proof["proofValue"] = Value::String(wrong.clone());
            }
            conn.execute(
                "UPDATE chain_entries SET json = ?1 WHERE chain = ?2 AND seq = ?3",
                rusqlite::params![canonical_json(&event).unwrap(), alice.scid(), seq],
            )
            .unwrap();
        }
    }

    // Each request misses and folds the log once, with no signature checked: a full replay
    // would refuse the first stored proof and answer 500.
    let folds = t.store.held.folds();
    let reply = note_by_d1(&t, &alice).await;
    assert_eq!(
        reply.status,
        StatusCode::SERVICE_UNAVAILABLE,
        "{}",
        reply.body
    );
    let reply = offers_by_master(&t, &alice).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 2);

    // A new event is still verified in full: a forged signature by the current master's id is
    // refused, and the owner's own event appends on the stored (unverified) prefix.
    let mut forged = after.clone();
    for proof in &mut forged.proof {
        proof.proof_value = wrong.clone();
    }
    let reply = t.append(&alice.scid(), &[&forged]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "BadProof");
    let reply = t.append_push(&mut alice, after).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn a_log_padded_with_repeated_scope_actions_stops_at_its_fold_budget() {
    let t = TestStore::new();
    assert_eq!(t.store.settings.log_fold_bytes, DEFAULT_LOG_FOLD_BYTES);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(
            &mut alice,
            &bot,
            OfferKind::Bot,
            Some(BotScope {
                can: vec!["read".into()],
                expires: t.now() + 86_400,
            }),
        )
        .await;
    // The attacker's log: each event one full body of `read`, `read`, `read`, ...
    let mut taken = 0;
    let refused = loop {
        let event = widest_scope(&alice, &bot_id, t.now() + 86_400);
        let reply = t.append(&alice.scid(), &[&event]).await;
        if reply.status != StatusCode::OK {
            break reply;
        }
        alice.push(event);
        taken += 1;
        // The default budget stops it at 45 events.
        assert!(taken < 60, "no budget stopped the log");
    };
    assert_eq!(refused.status, StatusCode::BAD_REQUEST, "{}", refused.body);
    assert_eq!(refused.body["error"], "log-too-large");
    // Some tens of such events: a log of ordinary events never comes near the budget.
    assert!(taken >= 20, "{taken} events");
    let stored = held_bytes_of(&alice.chain);
    assert!(stored <= DEFAULT_LOG_FOLD_BYTES, "{stored}");
    eprintln!("{taken} scope events of a full body each: the fold takes {stored} B");

    // The refusal stored nothing, however often it is sent, and the log stays held: no
    // request on it replays it.
    let head = t.store.head(&alice.scid()).unwrap().unwrap();
    assert_eq!(head.seq, alice.head_seq());
    let event = widest_scope(&alice, &bot_id, t.now() + 86_400);
    for _ in 0..3 {
        let reply = t.append(&alice.scid(), &[&event]).await;
        assert_eq!(reply.body["error"], "log-too-large");
    }
    assert_eq!(t.store.head(&alice.scid()).unwrap().unwrap(), head);
    assert!(t.store.held.holds(&alice.scid()));
    let folds = t.store.held.folds();
    let reply = note_by_d1(&t, &alice).await;
    assert_eq!(reply.status, StatusCode::SERVICE_UNAVAILABLE);
    assert_eq!(t.store.held.folds(), folds);
    // A small event still fits under the budget.
    let revoke = alice.revoke(&["d1"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn an_append_whose_fold_would_pass_a_small_budget_is_refused_whole_but_a_rotate_is_not() {
    // alice with a bot key, then two rotations and a scope naming `read` 2 000 times. The budget
    // lies halfway between the folds before and after the scope, measured on a probe store that
    // takes the same appends (the store's own fold, as the budget check sees it).
    async fn alice_with_a_bot(t: &TestStore) -> (Person, String) {
        let mut alice = Person::new("alice");
        t.publish(&alice).await;
        let scope = BotScope {
            can: vec!["read".into()],
            expires: t.now() + 86_400,
        };
        let bot = Device::new("alice bot");
        let bot_id = t.link(&mut alice, &bot, OfferKind::Bot, Some(scope)).await;
        (alice, bot_id)
    }
    let wide = |alice: &Person, bot_id: &str, expires| {
        let scope = BotScope {
            can: vec!["read".into(); 2_000],
            expires,
        };
        build::scope(
            &alice.chain,
            &alice.current_master(),
            &bot_id.to_string(),
            &scope,
        )
        .unwrap()
    };
    let probe = store_with(DEFAULT_HELD_LOG_BYTES, DEFAULT_LOG_FOLD_BYTES);
    let (mut alice, bot_id) = alice_with_a_bot(&probe).await;
    let first = alice.rotate(&[]);
    alice.push(first.clone());
    let second = alice.rotate(&[]);
    alice.push(second.clone());
    let reply = probe.append(&alice.scid(), &[&first, &second]).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let before = probe.store.held.held_bytes();
    let scope = wide(&alice, &bot_id, probe.now() + 86_400);
    let reply = probe.append(&alice.scid(), &[&scope]).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let after = probe.store.held.held_bytes();
    assert!(before + 16_000 < after, "{before} {after}");

    let t = store_with(DEFAULT_HELD_LOG_BYTES, (before + after) / 2);
    let (mut alice, bot_id) = alice_with_a_bot(&t).await;
    let at = alice.head_seq();
    let first = alice.rotate(&[]);
    alice.push(first.clone());
    let second = alice.rotate(&[]);
    alice.push(second.clone());
    let scope = wide(&alice, &bot_id, t.now() + 86_400);
    // A batch that ends past the budget stores none of its events.
    let reply = t.append(&alice.scid(), &[&first, &second, &scope]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "log-too-large");
    assert_eq!(t.store.head(&alice.scid()).unwrap().unwrap().seq, at);
    let reply = t.append(&alice.scid(), &[&first, &second]).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let reply = t.append(&alice.scid(), &[&scope]).await;
    assert_eq!(reply.body["error"], "log-too-large");

    // The log is at its budget. The owner's `rotate` (the recovery from a stolen master) is
    // still taken, and so is a batch of rotations, which take the live part past the budget
    // (each adds an event hash and a master's slot; the master it replaces leaves the live part
    // only its slot). The store's fold holds at least the person's: an append clones the held
    // fold, so its vectors grow from their length.
    let mut grown = 0;
    while fold_bytes_of(&alice.chain).live <= t.store.settings.log_fold_bytes {
        let rotate = alice.rotate(&[]);
        let reply = t.append_push(&mut alice, rotate).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        grown += 1;
        assert!(grown < 1_000, "the rotations never passed the budget");
    }
    let third = alice.rotate(&[]);
    alice.push(third.clone());
    let fourth = alice.rotate(&[]);
    alice.push(fourth.clone());
    let reply = t.append(&alice.scid(), &[&third, &fourth]).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // Any append with another event is still measured, a `rotate` in it included.
    let fifth = alice.rotate(&[]);
    alice.push(fifth.clone());
    let small = scope_of(&alice, &bot_id, 1, t.now() + 86_400);
    let reply = t.append(&alice.scid(), &[&fifth, &small]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "log-too-large");
    assert!(detail(&reply).contains("live part"), "{}", reply.body);
}

/// A `scope` of `bot_id` by the current master naming `read` `repeats` times.
fn scope_of(person: &Person, bot_id: &str, repeats: usize, expires: u64) -> Event {
    let scope = BotScope {
        can: vec!["read".into(); repeats],
        expires,
    };
    build::scope(
        &person.chain,
        &person.current_master(),
        &bot_id.to_string(),
        &scope,
    )
    .unwrap()
}

fn detail(reply: &Reply) -> &str {
    reply.body["detail"].as_str().unwrap_or_default()
}

/// A budget small enough that a few dozen scopes reach it.
const SMALL_BUDGET: usize = 64 << 10;

/// Appends scopes of `bots` (in turn) naming `read` `repeats` times each until the store refuses
/// one; answers the refusal and how many were taken.
async fn pad(
    t: &TestStore,
    person: &mut Person,
    bots: &[String],
    repeats: usize,
) -> (Reply, usize) {
    let mut taken = 0;
    loop {
        let bot = &bots[taken % bots.len()];
        let event = scope_of(person, bot, repeats, t.now() + 86_400);
        let reply = t.append_push(person, event).await;
        if reply.status != StatusCode::OK {
            return (reply, taken);
        }
        taken += 1;
        assert!(taken < 10_000, "no budget stopped the padding");
    }
}

/// Links a bot key to `person`; answers the reply to the `add` and the key id.
async fn try_link_bot(t: &TestStore, person: &mut Person, label: &str) -> (Reply, String) {
    let id = person.chain.state().next_device_id();
    let bot = Device::new(label);
    let scope = BotScope {
        can: vec!["read".into()],
        expires: t.now() + 86_400,
    };
    let (offer, view) = t
        .claimed_offer(person, &bot, OfferKind::Bot, Some(scope))
        .await;
    let add = person.add_claimed(&offer.offer, &view);
    (t.append_push(person, add).await, id)
}

#[tokio::test]
async fn the_owner_recovers_a_log_a_thief_padded_to_the_budget() {
    let t = store_with(DEFAULT_HELD_LOG_BYTES, SMALL_BUDGET);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    // The thief holds the current master: two bot keys, padded with scopes to the budget, the
    // last of them as small as a scope gets, so the fold sits within one action of it.
    let mut bots = Vec::new();
    for label in ["thief bot 1", "thief bot 2"] {
        let (reply, id) = try_link_bot(&t, &mut alice, label).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        bots.push(id);
    }
    let (refused, wide) = pad(&t, &mut alice, &bots, 200).await;
    assert_eq!(refused.body["error"], "log-too-large", "{}", refused.body);
    assert!(wide >= 4, "{wide}");
    let (refused, _) = pad(&t, &mut alice, &bots, 1).await;
    assert_eq!(refused.status, StatusCode::BAD_REQUEST, "{}", refused.body);
    assert_eq!(refused.body["error"], "log-too-large", "{}", refused.body);
    assert!(detail(&refused).contains("live part"), "{}", refused.body);

    // The owner's recovery: the `rotate` is taken, then one `revoke` of the thief's bot keys,
    // which drops their records out of the live part, then a new device.
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let revoke = alice.revoke(&[&bots[0], &bots[1]]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let bytes = fold_bytes_of(&alice.chain);
    // The thief's records left the live part and stayed in the whole fold.
    assert!(bytes.total - bytes.live > SMALL_BUDGET / 2, "{bytes:?}");
    assert!(bytes.live < SMALL_BUDGET / 2, "{bytes:?}");
    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None).await;
    // The revoked records stay in the fold, and the held folds count the whole of it.
    assert!(t.store.held.holds(&alice.scid()));
    assert!(t.store.held.held_bytes() > SMALL_BUDGET);
}

#[tokio::test]
async fn padding_and_revoking_over_and_over_stops_at_twice_the_budget() {
    let t = store_with(DEFAULT_HELD_LOG_BYTES, SMALL_BUDGET);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    // A bot key never revoked, for the scopes after the stop.
    let (reply, anchor) = try_link_bot(&t, &mut alice, "anchor").await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // Each round: a bot key, scopes on it until the live part is full, then its revocation.
    // The revoked records stay in the whole fold, which grows each round until it passes
    // twice the budget. Whatever stops it (a scope of 100 actions, an `add`, a `revoke`) adds
    // less to the fold than a scope of 200 actions does.
    let mut rounds = 0;
    let stopped = 'rounds: loop {
        rounds += 1;
        assert!(rounds < 20, "the whole fold never passed twice the budget");
        let (reply, bot) = try_link_bot(&t, &mut alice, &format!("bot {rounds}")).await;
        if reply.status != StatusCode::OK {
            break 'rounds reply;
        }
        let (refused, _) = pad(&t, &mut alice, std::slice::from_ref(&bot), 100).await;
        assert_eq!(refused.body["error"], "log-too-large", "{}", refused.body);
        if detail(&refused).contains("whole fold") {
            break 'rounds refused;
        }
        let revoke = alice.revoke(&[&bot]);
        let reply = t.append_push(&mut alice, revoke).await;
        if reply.status != StatusCode::OK {
            break 'rounds reply;
        }
    };
    assert_eq!(stopped.status, StatusCode::BAD_REQUEST, "{}", stopped.body);
    assert_eq!(stopped.body["error"], "log-too-large", "{}", stopped.body);
    assert!(detail(&stopped).contains("whole fold"), "{}", stopped.body);
    assert!(rounds >= 2, "{rounds}");
    let bytes = fold_bytes_of(&alice.chain);
    assert!(bytes.total <= 2 * SMALL_BUDGET, "{bytes:?}");
    assert!(bytes.total > SMALL_BUDGET, "{bytes:?}");

    // The log stays full: a scope of 200 actions passes the whole-fold bound.
    let wide = scope_of(&alice, &anchor, 200, t.now() + 86_400);
    let reply = t.append(&alice.scid(), &[&wide]).await;
    assert_eq!(reply.body["error"], "log-too-large", "{}", reply.body);
    assert!(detail(&reply).contains("whole fold"), "{}", reply.body);
    // A `rotate` is still taken at the whole-fold bound, alone or in a batch of rotations, and
    // the log stays full after it.
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let first = alice.rotate(&[]);
    alice.push(first.clone());
    let second = alice.rotate(&[]);
    alice.push(second.clone());
    let reply = t.append(&alice.scid(), &[&first, &second]).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let wide = scope_of(&alice, &anchor, 200, t.now() + 86_400);
    let reply = t.append(&alice.scid(), &[&wide]).await;
    assert_eq!(reply.body["error"], "log-too-large", "{}", reply.body);
    assert!(detail(&reply).contains("whole fold"), "{}", reply.body);
}

#[test]
fn the_held_bound_must_hold_one_log_at_twice_its_budget() {
    assert_eq!(Settings::default().check(), Ok(()));
    // A log's whole fold may take twice the budget, so the bound must hold that.
    const { assert!(DEFAULT_HELD_LOG_BYTES >= 2 * DEFAULT_LOG_FOLD_BYTES) };
    let below = Settings {
        held_log_bytes: 2 * DEFAULT_LOG_FOLD_BYTES - 1,
        ..Settings::default()
    };
    assert!(below
        .check()
        .unwrap_err()
        .contains("below twice LOG_FOLD_BYTES"));
    let once = Settings {
        held_log_bytes: 1 << 20,
        log_fold_bytes: 1 << 20,
        ..Settings::default()
    };
    assert!(once.check().is_err());
    let twice = Settings {
        held_log_bytes: 2 << 20,
        log_fold_bytes: 1 << 20,
        ..Settings::default()
    };
    assert_eq!(twice.check(), Ok(()));
}
