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
