//! What a miss in the held folds costs, and the per-log budget that bounds it.
//!
//! A miss rebuilds a log's fold from `chain_entries` with the core's `fold_stored`: the store
//! verified every stored event when it appended it, so the miss verifies no signature (the test
//! replaces every stored proof's signature and the miss still folds). An append past the per-log
//! budget (`Settings::log_fold_bytes`, by `held::held_bytes_of`) is `400 log-too-large` and
//! stores nothing, so every log the store takes can be held and folds in bounded time.

mod common;

use ad4m_hash_chain::{canonical_json, encode_proof_value};
use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{BotScope, Event};
use axum::http::StatusCode;
use identity_store::held::held_bytes_of;
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
    // still taken, and so is a batch of rotations, which take the log past the budget.
    let mut grown = 0;
    while t.store.held.held_bytes() <= t.store.settings.log_fold_bytes {
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
    let revoke = alice.revoke(&["d1"]);
    let reply = t.append(&alice.scid(), &[&fifth, &revoke]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "log-too-large");
}

#[test]
fn the_held_bound_must_hold_one_log_at_its_budget() {
    assert_eq!(Settings::default().check(), Ok(()));
    const { assert!(DEFAULT_HELD_LOG_BYTES >= DEFAULT_LOG_FOLD_BYTES) };
    let below = Settings {
        held_log_bytes: DEFAULT_LOG_FOLD_BYTES - 1,
        ..Settings::default()
    };
    assert!(below.check().unwrap_err().contains("LOG_FOLD_BYTES"));
    let equal = Settings {
        held_log_bytes: 1 << 20,
        log_fold_bytes: 1 << 20,
        ..Settings::default()
    };
    assert_eq!(equal.check(), Ok(()));
}
