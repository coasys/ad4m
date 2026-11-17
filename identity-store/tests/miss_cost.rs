//! The wall time of one signed request on a miss in the held folds, for the two costliest logs
//! an owner can make: a full log of rotations, and a log padded with `scope` events that each
//! name `read` as often as one append body holds. A measurement, not a check (the checks count
//! replays and signature checks, `tests/miss.rs`), so it is ignored by default:
//!
//! ```bash
//! cargo test --release -p identity-store --test miss_cost -- --ignored --nocapture
//! ```
//!
//! The padded logs are written into `chain_entries` directly, so a length the per-log budget
//! refuses (the 800 events of the review that found the cost) can be measured too; a `scope`
//! changes no key in the key index, so the index stays that of the head.

mod common;

use std::time::Instant;

use ad4m_hash_chain::store::{SqliteChainStore, StoredEntry};
use ad4m_hash_chain::Head;
use ad4m_identity_core::build;
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{event_hash, fold_stored, BotScope, Event, Op};
use axum::http::StatusCode;
use identity_store::held::held_bytes_of;
use identity_store::log::{MAX_EVENTS, MAX_LOG_EVENTS};
use identity_store::routes::MAX_BODY;
use serde_json::json;

use common::agent::{multikey, Device, Person};
use common::{Reply, TestStore};

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

/// Restarts the store (nothing held), then times a request on a miss and one on the held fold.
async fn time_requests(t: TestStore, person: &Person, label: &str) {
    let t = t.restarted();
    for what in ["miss", "held"] {
        let folds = t.store.held.folds();
        let start = Instant::now();
        let reply = note_by_d1(&t, person).await;
        let took = start.elapsed();
        assert_eq!(
            reply.status,
            StatusCode::SERVICE_UNAVAILABLE,
            "{}",
            reply.body
        );
        eprintln!(
            "{label}: one signed request, {what}: {:.1} ms ({} replay)",
            took.as_secs_f64() * 1000.0,
            t.store.held.folds() - folds
        );
    }
}

/// The event after `prev` with `op`, signed as `key_id` (built without folding).
fn next_event(prev: &Event, op: Op, key_id: &str, signer: &ed25519_dalek::SigningKey) -> Event {
    let mut event = Event {
        format: prev.format.clone(),
        scid: prev.scid.clone(),
        seq: prev.seq + 1,
        prev: Some(event_hash(prev).unwrap()),
        op,
        proof: Vec::new(),
    };
    build::cosign(&mut event, key_id, signer).unwrap();
    event
}

#[tokio::test]
#[ignore]
async fn a_miss_on_a_full_rotation_log() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let mut events = alice.events.clone();
    let mut batch = Vec::new();
    for i in 1..MAX_LOG_EVENTS as u32 {
        let op = Op::Rotate {
            master: multikey(&alice.master(i)),
            salt: alice.salt(i),
            next: Some(alice.next(i + 1)),
            revoke: Vec::new(),
        };
        let event = next_event(
            events.last().unwrap(),
            op,
            &format!("m{i}"),
            &alice.master(i),
        );
        events.push(event.clone());
        batch.push(event);
        if batch.len() == MAX_EVENTS || i == MAX_LOG_EVENTS as u32 - 1 {
            let refs: Vec<_> = batch.iter().collect();
            let reply = t.append(&alice.scid(), &refs).await;
            assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
            batch.clear();
        }
    }
    alice.chain = fold_stored(&events).unwrap();
    alice.events = events;
    eprintln!(
        "a full rotation log: {} events, fold {} B",
        alice.events.len(),
        held_bytes_of(&alice.chain)
    );
    time_requests(t, &alice, "a full rotation log").await;
}

#[tokio::test]
#[ignore]
async fn a_miss_on_logs_padded_with_repeated_scope_actions() {
    for n in [45, 800] {
        let t = TestStore::new();
        let mut alice = Person::new("alice");
        t.publish(&alice).await;
        let bot = Device::new("alice bot");
        let scope = BotScope {
            can: vec!["read".into()],
            expires: t.now() + 86_400,
        };
        let bot_id = t.link(&mut alice, &bot, OfferKind::Bot, Some(scope)).await;
        let mut events = alice.events.clone();
        let mut repeats = 9_400;
        let mut padded = Vec::new();
        for _ in 0..n {
            let op = |repeats| Op::Scope {
                key: bot_id.clone(),
                can: vec!["read".to_string(); repeats],
                expires: t.now() + 86_400,
            };
            let mut event = next_event(events.last().unwrap(), op(repeats), "m0", &alice.master(0));
            while serde_json::to_vec(&json!({ "events": [&event] }))
                .unwrap()
                .len()
                > MAX_BODY
            {
                repeats -= 50;
                event = next_event(events.last().unwrap(), op(repeats), "m0", &alice.master(0));
            }
            events.push(event.clone());
            padded.push(event);
        }
        {
            let mut conn = t.store.db.lock().unwrap();
            let tx = conn.transaction().unwrap();
            let head = alice.chain.head();
            let entries: Vec<StoredEntry> = padded
                .iter()
                .map(|e| StoredEntry::from_entry(e, t.now()).unwrap())
                .collect();
            SqliteChainStore::append_in(
                &tx,
                "coasys:kel",
                &alice.scid(),
                Some(&Head {
                    seq: head.seq,
                    hash: head.hash.clone(),
                }),
                &entries,
            )
            .unwrap();
            tx.commit().unwrap();
        }
        alice.chain = fold_stored(&events).unwrap();
        alice.events = events;
        let label = format!("{n} scope events of {repeats} repeats");
        eprintln!("{label}: fold {} B", held_bytes_of(&alice.chain));
        time_requests(t, &alice, &label).await;
    }
}

/// Times `events` sent as one append, `rounds` times, each answered `want`.
async fn time_appends(
    t: &TestStore,
    scid: &str,
    events: &[Event],
    want: (StatusCode, &str),
    label: &str,
) {
    let refs: Vec<_> = events.iter().collect();
    for round in 0..3 {
        let folds = t.store.held.folds();
        let start = Instant::now();
        let reply = t.append(scid, &refs).await;
        let took = start.elapsed();
        assert_eq!(reply.status, want.0, "{}", reply.body);
        assert_eq!(reply.body["error"], want.1, "{}", reply.body);
        eprintln!(
            "{label}, round {round}: {:.1} ms ({} replay)",
            took.as_secs_f64() * 1000.0,
            t.store.held.folds() - folds
        );
    }
}

#[tokio::test]
#[ignore]
async fn a_late_failing_batch_on_a_held_log_at_its_budget() {
    // The owner pads the log to the per-log budget through the store, so it stays held; then
    // one batch of 31 valid small scopes and a 32nd event without a proof, sent again and
    // again: it commits nothing.
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let scope = BotScope {
        can: vec!["read".into()],
        expires: t.now() + 86_400,
    };
    let bot_id = t.link(&mut alice, &bot, OfferKind::Bot, Some(scope)).await;
    let mut repeats = 9_400;
    loop {
        let op = Op::Scope {
            key: bot_id.clone(),
            can: vec!["read".to_string(); repeats],
            expires: t.now() + 86_400,
        };
        let event = next_event(alice.events.last().unwrap(), op, "m0", &alice.master(0));
        if serde_json::to_vec(&json!({ "events": [&event] }))
            .unwrap()
            .len()
            > MAX_BODY
        {
            repeats -= 50;
            continue;
        }
        let reply = t.append(&alice.scid(), &[&event]).await;
        if reply.status != StatusCode::OK {
            assert_eq!(reply.body["error"], "log-too-large", "{}", reply.body);
            break;
        }
        alice.push(event);
    }
    assert!(t.store.held.holds(&alice.scid()));
    let mut batch: Vec<Event> = Vec::new();
    for _ in 0..MAX_EVENTS - 1 {
        let op = Op::Scope {
            key: bot_id.clone(),
            can: vec!["read".into()],
            expires: t.now() + 86_400,
        };
        let prev = batch.last().unwrap_or(alice.events.last().unwrap());
        batch.push(next_event(prev, op, "m0", &alice.master(0)));
    }
    let mut unsigned = next_event(
        batch.last().unwrap(),
        Op::Revoke {
            revoke: vec!["d1".into()],
        },
        "m0",
        &alice.master(0),
    );
    unsigned.proof.clear();
    batch.push(unsigned);
    let label = format!(
        "a log of {} events at its budget (fold {} B), 31 small scopes and one unsigned event",
        alice.events.len(),
        held_bytes_of(&alice.chain)
    );
    time_appends(
        &t,
        &alice.scid(),
        &batch,
        (StatusCode::BAD_REQUEST, "MissingProof"),
        &label,
    )
    .await;
}

#[tokio::test]
#[ignore]
async fn a_rival_at_the_head_of_a_held_full_rotation_log_that_does_not_replay() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let mut events = alice.events.clone();
    let mut batch = Vec::new();
    for i in 1..MAX_LOG_EVENTS as u32 {
        let op = Op::Rotate {
            master: multikey(&alice.master(i)),
            salt: alice.salt(i),
            next: Some(alice.next(i + 1)),
            revoke: Vec::new(),
        };
        let event = next_event(
            events.last().unwrap(),
            op,
            &format!("m{i}"),
            &alice.master(i),
        );
        events.push(event.clone());
        batch.push(event);
        if batch.len() == MAX_EVENTS || i == MAX_LOG_EVENTS as u32 - 1 {
            let refs: Vec<_> = batch.iter().collect();
            let reply = t.append(&alice.scid(), &refs).await;
            assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
            batch.clear();
        }
    }
    assert!(t.store.held.holds(&alice.scid()));
    // At the head's seq, signed by the master current there (retired by the head's rotate),
    // revoking a key the log never listed: it does not replay, so it is a plain conflict.
    let at = events.len() - 1;
    let signer = at as u32 - 1;
    let rival = next_event(
        &events[at - 1],
        Op::Revoke {
            revoke: vec!["d99".into()],
        },
        &format!("m{signer}"),
        &alice.master(signer),
    );
    time_appends(
        &t,
        &alice.scid(),
        &[rival],
        (StatusCode::CONFLICT, "conflict"),
        "a rival at the head of a held full rotation log that does not replay",
    )
    .await;
}
