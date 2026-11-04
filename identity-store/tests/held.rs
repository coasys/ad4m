//! The folded logs the store holds in memory (`src/held.rs`): a signed request checks against
//! the held fold with no replay of the log, a forged one never reads the log, an append's new
//! head is in force for the next request, and the bound drops the least recently used.
//!
//! `Store::held.folds()` counts every replay of a log (or a prefix) from the database, so the
//! tests count replays rather than time them.

mod common;

use ad4m_identity_core::{build, BotScope, DeviceKey, KeyLife};
use axum::http::StatusCode;
use identity_store::held::{held_bytes_of, state_before};
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

fn store_with_bound(held_log_bytes: usize) -> TestStore {
    TestStore::with_settings(Settings {
        pow_bits: 8,
        held_log_bytes,
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
    // Room for three one-event logs (each the same size: the same keys and lengths).
    let one = held_bytes_of(&Person::new("someone").chain);
    let t = store_with_bound(3 * one);
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
    assert_eq!(t.store.held.held_bytes(), 3 * one);

    // Alice is used, so bob is now the least recently used: dave's log drops him.
    assert_eq!(list(&t, alice).await.status, StatusCode::OK);
    let dave = Person::new("dave");
    t.publish(&dave).await;
    assert!(!t.store.held.holds(&bob.scid()));
    assert!(t.store.held.holds(&alice.scid()));
    assert!(t.store.held.holds(&carol.scid()));
    assert!(t.store.held.holds(&dave.scid()));
    assert_eq!(t.store.held.held_bytes(), 3 * one);

    // A request of bob's misses, folds once and holds his log again (carol goes); the next
    // request of his replays nothing.
    let folds = t.store.held.folds();
    assert_eq!(list(&t, bob).await.status, StatusCode::OK);
    assert_eq!(t.store.held.folds(), folds + 1);
    assert!(t.store.held.holds(&bob.scid()));
    assert!(!t.store.held.holds(&carol.scid()));
    assert_eq!(list(&t, bob).await.status, StatusCode::OK);
    assert_eq!(t.store.held.folds(), folds + 1);

    // A log whose fold is larger than the bound is never held, and holds nothing else out.
    let mut erin = Person::new("erin");
    t.publish(&erin).await;
    grow(&t, &mut erin, 32).await;
    assert!(held_bytes_of(&erin.chain) > 3 * one);
    assert!(!t.store.held.holds(&erin.scid()));
    assert!(t.store.held.held_bytes() <= 3 * one);
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

#[tokio::test]
async fn a_signed_rival_that_does_not_replay_is_refused_on_the_held_fold_with_no_replay() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    grow(&t, &mut alice, 64).await;
    assert!(t.store.held.holds(&alice.scid()));
    let folds = t.store.held.folds();
    let at = 63;
    let prefix = ad4m_identity_core::fold(&alice.events[..at]).unwrap();

    // Signed by the master current at seq 63 (m62, retired by the stored rotation there), but
    // revoking a key the log never listed: a plain conflict, however often, and no replay.
    let rival = build::revoke(&prefix, &alice.master(62), &["d9".to_string()]).unwrap();
    for _ in 0..3 {
        let reply = t.append(&alice.scid(), &[&rival]).await;
        assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
        assert_eq!(reply.body["error"], "conflict", "{}", reply.body);
    }
    // A `rotate` by the key the log rotated onto there (m63, which the key index lets sign it)
    // with another salt: the commitment before seq 63 refuses it, read from the stored event
    // that set it.
    let mut wrong_salt = build::rotate_unsigned(
        &prefix,
        &common::agent::multikey(&alice.master(63)),
        &alice.salt(64),
        Some(alice.next(64)),
        &[],
    )
    .unwrap();
    build::cosign(&mut wrong_salt, "m63", &alice.master(63)).unwrap();
    let reply = t.append(&alice.scid(), &[&wrong_salt]).await;
    assert_eq!(reply.body["error"], "conflict", "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds);
    assert_eq!(t.count("chain_forks"), 0);

    // The same `rotate` with the right salt and another next key replays on the state before
    // seq 63: a fork, confirmed by one replay of the prefix before it is recorded.
    let rival = build::rotate(
        &prefix,
        &alice.master(63),
        &alice.salt(63),
        Some(alice.next(70)),
        &[],
    )
    .unwrap();
    let reply = t.append(&alice.scid(), &[&rival]).await;
    assert_eq!(reply.body["error"], "forked", "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
    assert_eq!(t.count("chain_forks"), 1);

    // Nothing held (a restart): the first such rival folds the log once and holds it; sent
    // again, it replays nothing.
    let t = t.restarted();
    assert!(!t.store.held.holds(&alice.scid()));
    let rival = build::revoke(&prefix, &alice.master(62), &["d9".to_string()]).unwrap();
    let folds = t.store.held.folds();
    for _ in 0..3 {
        let reply = t.append(&alice.scid(), &[&rival]).await;
        assert_eq!(reply.body["error"], "conflict", "{}", reply.body);
    }
    assert_eq!(t.store.held.folds(), folds + 1);
    assert!(t.store.held.holds(&alice.scid()));
}

#[tokio::test]
async fn a_signed_rival_at_the_head_seq_that_does_not_replay_is_refused_with_no_replay() {
    // The same as below the head, at the head's own seq: the rival takes the place of the last
    // stored event, and the state before it is rebuilt from the held fold too.
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    grow(&t, &mut alice, 64).await;
    assert!(t.store.held.holds(&alice.scid()));
    let folds = t.store.held.folds();
    let at = alice.head_seq();
    assert_eq!(at, 64);
    let prefix = ad4m_identity_core::fold(&alice.events[..at as usize]).unwrap();

    // Signed by the master current at the head's seq (m63, retired by the stored rotation
    // there), revoking a key the log never listed: a plain conflict, however often.
    let rival = build::revoke(&prefix, &alice.master(63), &["d9".to_string()]).unwrap();
    assert_eq!(rival.seq, at);
    for _ in 0..3 {
        let reply = t.append(&alice.scid(), &[&rival]).await;
        assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
        assert_eq!(reply.body["error"], "conflict", "{}", reply.body);
    }
    // A `rotate` by the key the log rotated onto at the head (m64) with another salt: the
    // commitment before the head refuses it.
    let mut wrong_salt = build::rotate_unsigned(
        &prefix,
        &common::agent::multikey(&alice.master(64)),
        &alice.salt(65),
        Some(alice.next(65)),
        &[],
    )
    .unwrap();
    build::cosign(&mut wrong_salt, "m64", &alice.master(64)).unwrap();
    let reply = t.append(&alice.scid(), &[&wrong_salt]).await;
    assert_eq!(reply.body["error"], "conflict", "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds);
    assert_eq!(t.count("chain_forks"), 0);
    assert_eq!(t.store.head(&alice.scid()).unwrap().unwrap().seq, at);
}

/// A device key for `life`, with the id the log gives next, added by the current master.
fn add_key(person: &Person, label: &str, life: KeyLife) -> ad4m_identity_core::Event {
    let id = person.chain.state().next_device_id();
    let k = key(label);
    let device = DeviceKey {
        id,
        sign: common::agent::multikey(&k),
        enc: common::agent::enc_of(&k),
        life,
    };
    let mut event = build::add_unsigned(&person.chain, device, "offer", "z3coSig").unwrap();
    build::cosign(&mut event, &person.master_id(), &person.current_master()).unwrap();
    event
}

fn bot(can: &[&str], expires: u64) -> KeyLife {
    KeyLife::Bot(BotScope {
        can: can.iter().map(|s| s.to_string()).collect(),
        expires,
    })
}

#[test]
fn the_state_before_a_seq_rebuilt_from_a_later_state_is_the_fold_of_the_events_before_it() {
    // A log of every op and key life: bot and standing adds, scopes, revocations, rotations
    // that revoke, and the end.
    let mut alice = Person::new("alice");
    let scope = |p: &Person, id: &str, can: &[&str], expires| {
        let scope = BotScope {
            can: can.iter().map(|s| s.to_string()).collect(),
            expires,
        };
        build::scope(&p.chain, &p.current_master(), &id.to_string(), &scope).unwrap()
    };
    let e = add_key(&alice, "bot a", bot(&["read", "write"], 1_750_000_000));
    alice.push(e); // d2
    let e = add_key(&alice, "phone", KeyLife::Standing);
    alice.push(e); // d3
    let e = scope(&alice, "d2", &["read"], 1_800_000_000);
    alice.push(e);
    let e = alice.revoke(&["d3"]);
    alice.push(e);
    let e = alice.rotate(&["d1"]);
    alice.push(e); // m1
    let e = scope(&alice, "d2", &["join", "read", "join"], 1_900_000_000);
    alice.push(e);
    let e = add_key(&alice, "bot b", bot(&["join"], 1_760_000_000));
    alice.push(e); // d4
    let e = alice.revoke(&["d2"]);
    alice.push(e);
    let e = alice.rotate(&[]);
    alice.push(e); // m2
    let e = scope(&alice, "d4", &["read", "write", "join"], 1_950_000_000);
    alice.push(e);
    let end = build::rotate(&alice.chain, &alice.master(3), &alice.salt(3), None, &[]).unwrap();
    alice.push(end); // m3, the end
    assert!(alice.chain.state().ended);
    let full = alice.chain.state();
    for seq in 1..alice.events.len() {
        let want = ad4m_identity_core::fold(&alice.events[..seq]).unwrap();
        let rebuilt = state_before(full, seq as u64, want.state().next.clone());
        assert_eq!(&rebuilt, want.state(), "the state before seq {seq}");
        // From every later head too, not only the last.
        for later in seq..alice.events.len() {
            let at = ad4m_identity_core::fold(&alice.events[..=later]).unwrap();
            assert_eq!(
                state_before(at.state(), seq as u64, want.state().next.clone()),
                *want.state(),
                "the state before seq {seq} from the head at {later}"
            );
        }
    }
}
