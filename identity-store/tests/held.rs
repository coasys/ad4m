//! The folded logs the store holds in memory (`src/held.rs`): a signed request checks against
//! the held fold with no replay of the log, a forged one never reads the log, an append's new
//! head is in force for the next request, and the bound drops the least recently used.
//!
//! `Store::held.folds()` counts every replay of a log (or a prefix) from the database, so the
//! tests count replays rather than time them.

mod common;

use ad4m_identity_core::build;
use axum::http::StatusCode;
use identity_store::Settings;
use serde_json::{json, Value};

use common::agent::{key, Person};
use common::TestStore;

/// Grows `person`'s log by `n` events (rotations onto each next master), sent 32 to an append.
async fn grow(t: &TestStore, person: &mut Person, n: usize) {
    let mut sent = 0;
    while sent < n {
        let batch = (n - sent).min(identity_store::log::MAX_EVENTS);
        let mut events = Vec::with_capacity(batch);
        for _ in 0..batch {
            let event = person.rotate(&[]);
            person.push(event.clone());
            events.push(event);
        }
        let refs: Vec<_> = events.iter().collect();
        let reply = t.append(&person.scid(), &refs).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        sent += batch;
    }
}

/// `GET /v1/offers/{scid}`, signed by the current master.
async fn list(t: &TestStore, person: &Person) -> common::Reply {
    let path = format!("/v1/offers/{}", person.scid());
    let header = person.master_request("GET", &path, b"", t.now() + 60);
    t.send("GET", &path, None, Some(header)).await
}

/// `POST /v1/notes/{scid}` signed by the device key `d1`: `503 vouch-unavailable` once the
/// request passes.
async fn note_by_d1(t: &TestStore, person: &Person) -> common::Reply {
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

fn store_with_bound(held_log_events: usize) -> TestStore {
    TestStore::with_settings(Settings {
        pow_bits: 8,
        held_log_events,
        ..Settings::default()
    })
}

fn detail(body: &Value) -> &str {
    body["detail"].as_str().unwrap_or_default()
}

#[tokio::test]
async fn signed_requests_on_a_long_log_never_replay_it() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    grow(&t, &mut alice, 200).await;
    assert_eq!(alice.head_seq(), 200);
    let folds = t.store.held.folds();
    for _ in 0..5 {
        let reply = list(&t, &alice).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        let reply = note_by_d1(&t, &alice).await;
        assert_eq!(
            reply.status,
            StatusCode::SERVICE_UNAVAILABLE,
            "{}",
            reply.body
        );
    }
    // Ten requests and the appends that grew the log: the log was never replayed.
    assert_eq!(t.store.held.folds(), folds);
    assert!(t.store.held.holds(&alice.scid()));
}

#[tokio::test]
async fn a_forged_request_is_refused_without_a_replay() {
    // A bound of one event: alice's log (two events) is never held, so every use of it that
    // needs its state replays it, and the count shows whether the refusal read it.
    let t = store_with_bound(1);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    grow(&t, &mut alice, 1).await;
    assert!(!t.store.held.holds(&alice.scid()));
    let path = format!("/v1/offers/{}", alice.scid());
    let folds = t.store.held.folds();

    // The master's key id, signed by another key.
    let forged = alice.request(
        &alice.master_id(),
        &key("mallory"),
        "GET",
        &path,
        b"",
        t.now() + 60,
    );
    let reply = t.send("GET", &path, None, Some(forged)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert!(detail(&reply.body).contains("signature"), "{}", reply.body);
    // A key id the log does not list.
    let unknown = alice.request("d9", &key("mallory"), "GET", &path, b"", t.now() + 60);
    let reply = t.send("GET", &path, None, Some(unknown)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert!(
        detail(&reply.body).contains("not in the log"),
        "{}",
        reply.body
    );
    assert_eq!(t.store.held.folds(), folds);

    // The same request, signed by the master: it passes, and this unheld log replays once.
    let reply = list(&t, &alice).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
}

#[tokio::test]
async fn after_an_append_the_next_request_checks_against_the_new_head() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    assert_eq!(
        note_by_d1(&t, &alice).await.status,
        StatusCode::SERVICE_UNAVAILABLE
    );
    let folds = t.store.held.folds();

    // The revocation of d1 commits: d1 is refused at once, with no replay.
    let revoke = alice.revoke(&["d1"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let reply = note_by_d1(&t, &alice).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert!(
        detail(&reply.body).contains("not valid at the head"),
        "{}",
        reply.body
    );
    // A rotation: the old master no longer passes on a master route, the new one does.
    let old_master = (alice.master_id(), alice.current_master());
    let rotate = alice.rotate(&[]);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    let path = format!("/v1/offers/{}", alice.scid());
    let by_old = alice.request(
        &old_master.0,
        &old_master.1,
        "GET",
        &path,
        b"",
        t.now() + 60,
    );
    let reply = t.send("GET", &path, None, Some(by_old)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(list(&t, &alice).await.status, StatusCode::OK);
    assert_eq!(t.store.held.folds(), folds);
}

#[tokio::test]
async fn a_fold_held_for_an_older_head_is_never_used() {
    // An event the store takes outside an append (here, written straight into the module's
    // tables, as a restore of the file would) moves the head; the held fold of the old head is
    // dropped and the log folds again.
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    assert_eq!(list(&t, &alice).await.status, StatusCode::OK);
    let revoke = alice.revoke(&["d1"]);
    {
        let mut conn = t.store.db.lock().unwrap();
        let tx = conn.transaction().unwrap();
        let entry = ad4m_hash_chain::store::StoredEntry::from_entry(&revoke, t.now()).unwrap();
        let head = ad4m_hash_chain::Head {
            seq: alice.head_seq(),
            hash: alice.chain.head().hash.clone(),
        };
        ad4m_hash_chain::store::SqliteChainStore::append_in(
            &tx,
            identity_store::db::KEL,
            &alice.scid(),
            Some(&head),
            &[entry],
        )
        .unwrap();
        tx.commit().unwrap();
    }
    alice.push(revoke);
    let folds = t.store.held.folds();
    let reply = note_by_d1(&t, &alice).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert!(
        detail(&reply.body).contains("not valid at the head"),
        "{}",
        reply.body
    );
    assert_eq!(t.store.held.folds(), folds + 1);
}

#[tokio::test]
async fn the_bound_drops_the_least_recently_used_and_a_miss_folds_once() {
    let t = store_with_bound(3);
    let people: Vec<Person> = ["alice", "bob", "carol"]
        .iter()
        .map(|name| Person::new(name))
        .collect();
    for person in &people {
        t.publish(person).await;
    }
    let [alice, bob, carol] = &people[..] else {
        unreachable!()
    };
    assert!(people.iter().all(|p| t.store.held.holds(&p.scid())));
    assert_eq!(t.store.held.held_events(), 3);

    // Alice is used, so bob is now the least recently used: dave's log drops him.
    assert_eq!(list(&t, alice).await.status, StatusCode::OK);
    let dave = Person::new("dave");
    t.publish(&dave).await;
    assert!(!t.store.held.holds(&bob.scid()));
    assert!(t.store.held.holds(&alice.scid()));
    assert!(t.store.held.holds(&carol.scid()));
    assert!(t.store.held.holds(&dave.scid()));
    assert_eq!(t.store.held.held_events(), 3);

    // A request of bob's misses, folds once and holds his log again (carol goes); the next
    // request of his replays nothing.
    let folds = t.store.held.folds();
    assert_eq!(list(&t, bob).await.status, StatusCode::OK);
    assert_eq!(t.store.held.folds(), folds + 1);
    assert!(t.store.held.holds(&bob.scid()));
    assert!(!t.store.held.holds(&carol.scid()));
    assert_eq!(list(&t, bob).await.status, StatusCode::OK);
    assert_eq!(t.store.held.folds(), folds + 1);

    // A log longer than the bound is never held, and holds nothing else out.
    let mut erin = Person::new("erin");
    t.publish(&erin).await;
    grow(&t, &mut erin, 3).await;
    assert!(!t.store.held.holds(&erin.scid()));
    assert!(t.store.held.held_events() <= 3);
}

#[tokio::test]
async fn a_rival_event_no_master_signed_replays_nothing() {
    // An unauthenticated append of a rival to a stored event of a long log: without a proof by
    // the master that signs at that seq, the store answers a plain conflict with no replay of
    // the prefix.
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    grow(&t, &mut alice, 64).await;
    let folds = t.store.held.folds();
    let at = 63;
    let prefix = ad4m_identity_core::fold(&alice.events[..at]).unwrap();
    // A revocation of d1 at seq 63, where the log holds a rotation, signed by another key in
    // place of the master current there.
    let mut rival = build::revoke_unsigned(&prefix, &["d1".to_string()]).unwrap();
    let master_id = prefix.state().current_master().unwrap().id.clone();
    build::cosign(&mut rival, &master_id, &key("mallory")).unwrap();
    assert_eq!(rival.seq, at as u64);
    let reply = t.append(&alice.scid(), &[&rival]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
    assert_ne!(reply.body["error"], "forked", "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds);
    assert_eq!(t.count("chain_forks"), 0);

    // The same rival signed by the master current there (m62, retired since): a fork, found by
    // one replay of the prefix.
    let rival = build::revoke(&prefix, &alice.master(62), &["d1".to_string()]).unwrap();
    let reply = t.append(&alice.scid(), &[&rival]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
    assert_eq!(reply.body["error"], "forked", "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
    // That master already has its fork: filing the rival again is answered with the recorded
    // fork, with no replay, however often.
    for _ in 0..5 {
        let reply = t.append(&alice.scid(), &[&rival]).await;
        assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
        assert_eq!(reply.body["error"], "forked", "{}", reply.body);
    }
    assert_eq!(t.store.held.folds(), folds + 1);
    assert_eq!(t.count("chain_forks"), 1);
}
