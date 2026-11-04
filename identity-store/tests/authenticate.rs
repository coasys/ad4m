//! Nobody without a key of a log makes the store replay it: an append's first event, an offer
//! and a signed request are checked against the key index before the log is read
//! (`log.rs` `authenticate_first`, `offers.rs`, `request.rs`), and no log grows past what its
//! readers read (`log::MAX_LOG_EVENTS`).
//!
//! Every test runs on a log the store does not hold folded (a bound of one event, or a log
//! evicted by others), so any use of the log's state replays it, and counts the replays with
//! `Store::held.folds()`.

mod common;

use ad4m_identity_core::build;
use ad4m_identity_core::link::{sign_offer, OfferKind};
use ad4m_identity_core::{BotScope, Event, Op};
use axum::http::StatusCode;
use identity_store::held::held_bytes_of;
use identity_store::log::MAX_LOG_EVENTS;
use identity_store::Settings;
use serde_json::json;

use common::agent::{key, multikey, Device, Person};
use common::{Reply, TestStore};

fn store_with_bound(held_log_bytes: usize) -> TestStore {
    TestStore::with_settings(Settings {
        pow_bits: 8,
        held_log_bytes,
        ..Settings::default()
    })
}

/// A store holding nothing folded, with alice's log of two events (#0 and a rotation).
async fn unheld_alice() -> (TestStore, Person) {
    let t = store_with_bound(1);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert!(!t.store.held.holds(&alice.scid()));
    (t, alice)
}

/// A revocation of d1 at alice's head, with no proof.
fn stripped(alice: &Person) -> Event {
    let mut event = alice.revoke(&["d1"]);
    event.proof.clear();
    event
}

/// A revocation of d1 at alice's head with one proof naming `key_id`, made by `signer`.
fn signed_as(alice: &Person, key_id: &str, signer: &ed25519_dalek::SigningKey) -> Event {
    let mut event = build::revoke_unsigned(&alice.chain, &["d1".to_string()]).unwrap();
    build::cosign(&mut event, key_id, signer).unwrap();
    event
}

fn refused_as(reply: &Reply, status: StatusCode, error: &str) {
    assert_eq!(reply.status, status, "{}", reply.body);
    assert_eq!(reply.body["error"], error, "{}", reply.body);
}

#[tokio::test]
async fn a_proof_stripped_event_replays_nothing_however_often() {
    let (t, alice) = unheld_alice().await;
    let folds = t.store.held.folds();
    let junk = stripped(&alice);
    for _ in 0..5 {
        let reply = t.append(&alice.scid(), &[&junk]).await;
        refused_as(&reply, StatusCode::BAD_REQUEST, "MissingProof");
    }
    assert_eq!(t.store.held.folds(), folds);
}

#[tokio::test]
async fn an_event_no_current_master_signed_replays_nothing() {
    let (t, alice) = unheld_alice().await;
    let folds = t.store.held.folds();
    let master_id = alice.master_id();
    assert_eq!(master_id, "m1");
    let cases = [
        // A device key of the log, under its own id: the master's proof is missing.
        (
            signed_as(&alice, "d1", &alice.device_key(1)),
            "MissingProof",
        ),
        // A device key of the log, under the master's id.
        (
            signed_as(&alice, &master_id, &alice.device_key(1)),
            "BadProof",
        ),
        // A stranger under the master's id.
        (signed_as(&alice, &master_id, &key("mallory")), "BadProof"),
        // The retired master m0, under its own id and under the current master's.
        (signed_as(&alice, "m0", &alice.master(0)), "MissingProof"),
        (signed_as(&alice, &master_id, &alice.master(0)), "BadProof"),
    ];
    for (event, error) in &cases {
        let reply = t.append(&alice.scid(), &[event]).await;
        refused_as(&reply, StatusCode::BAD_REQUEST, error);
    }
    assert_eq!(t.store.held.folds(), folds);
}

#[tokio::test]
async fn the_current_master_appends_with_one_replay() {
    let (t, mut alice) = unheld_alice().await;
    let folds = t.store.held.folds();
    let revoke = alice.revoke(&["d1"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
}

#[tokio::test]
async fn a_rotate_by_the_committed_key_appends_and_retires_the_old_master() {
    let (t, mut alice) = unheld_alice().await;
    let folds = t.store.held.folds();

    // A rotate onto a key the log did not commit to: refused with no replay.
    let mut onto_another = alice.rotate(&[]);
    if let Op::Rotate { master, .. } = &mut onto_another.op {
        *master = multikey(&key("mallory"));
    }
    onto_another.proof.clear();
    build::cosign(&mut onto_another, "m2", &key("mallory")).unwrap();
    let reply = t.append(&alice.scid(), &[&onto_another]).await;
    refused_as(&reply, StatusCode::BAD_REQUEST, "NextMismatch");
    assert_eq!(t.store.held.folds(), folds);

    // The committed next key rotates: one replay.
    let old = alice.current_master();
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
    assert!(!t.store.held.holds(&alice.scid()));

    // The master it retired signs nothing more, and makes the store replay nothing.
    let by_old = signed_as(&alice, &alice.master_id(), &old);
    let reply = t.append(&alice.scid(), &[&by_old]).await;
    refused_as(&reply, StatusCode::BAD_REQUEST, "BadProof");
    assert_eq!(t.store.held.folds(), folds + 1);
}

#[tokio::test]
async fn cheap_logs_evict_a_victim_and_junk_still_replays_nothing() {
    // Room for eight one-event logs.
    let t = store_with_bound(8 * held_bytes_of(&Person::new("someone").chain));
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    for _ in 0..3 {
        let rotate = alice.rotate(&[]);
        let reply = t.append_push(&mut alice, rotate).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    }
    assert!(t.store.held.holds(&alice.scid()));
    // Eight one-event logs fill the bound: alice's fold goes.
    for i in 0..8 {
        t.publish(&Person::new(&format!("sybil {i}"))).await;
    }
    assert!(!t.store.held.holds(&alice.scid()));

    let folds = t.store.held.folds();
    let junk = stripped(&alice);
    for _ in 0..5 {
        let reply = t.append(&alice.scid(), &[&junk]).await;
        refused_as(&reply, StatusCode::BAD_REQUEST, "MissingProof");
    }
    // A rival to a stored event and an event past the head, both unsigned: plain conflicts.
    let prefix = ad4m_identity_core::fold(&alice.events[..2]).unwrap();
    let mut rival = build::revoke_unsigned(&prefix, &["d1".to_string()]).unwrap();
    build::cosign(&mut rival, "m0", &key("mallory")).unwrap();
    assert_eq!(rival.seq, 2);
    let reply = t.append(&alice.scid(), &[&rival]).await;
    refused_as(&reply, StatusCode::CONFLICT, "conflict");
    let mut gap = alice.revoke(&["d1"]);
    gap.seq += 1;
    let reply = t.append(&alice.scid(), &[&gap]).await;
    refused_as(&reply, StatusCode::CONFLICT, "conflict");
    assert_eq!(t.store.held.folds(), folds);
    assert!(!t.store.held.holds(&alice.scid()));
}

#[tokio::test]
async fn a_log_holds_at_most_what_its_readers_read() {
    assert_eq!(MAX_LOG_EVENTS, 64 * 256);
    let (t, alice) = unheld_alice().await;
    let folds = t.store.held.folds();
    let at = |seq: u64| {
        let mut event = alice.revoke(&["d1"]);
        event.seq = seq;
        event
    };
    // An event at seq 16 384 would be the 16 385th: refused before anything is read.
    let reply = t.append(&alice.scid(), &[&at(MAX_LOG_EVENTS)]).await;
    refused_as(&reply, StatusCode::BAD_REQUEST, "log-full");
    // So is a batch whose last event would be.
    let reply = t
        .append(
            &alice.scid(),
            &[&at(MAX_LOG_EVENTS - 1), &at(MAX_LOG_EVENTS)],
        )
        .await;
    refused_as(&reply, StatusCode::BAD_REQUEST, "log-full");
    // Seq 16 383, the last a log holds, passes the cap (and here is a gap: a plain conflict).
    let reply = t.append(&alice.scid(), &[&at(MAX_LOG_EVENTS - 1)]).await;
    refused_as(&reply, StatusCode::CONFLICT, "conflict");
    assert_eq!(t.store.held.folds(), folds);
}

#[tokio::test]
async fn an_offer_the_master_did_not_sign_replays_nothing() {
    let (t, mut alice) = unheld_alice().await;
    let folds = t.store.held.folds();
    let (offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let forged = sign_offer(offer.offer.clone(), offer.by.clone(), &key("mallory")).unwrap();
    for _ in 0..5 {
        let reply = t.open_offer(&forged).await;
        refused_as(&reply, StatusCode::FORBIDDEN, "signer");
    }
    // Signed by d1 under the master's id, and by the retired master.
    let by_device =
        sign_offer(offer.offer.clone(), offer.by.clone(), &alice.device_key(1)).unwrap();
    let by_old = sign_offer(offer.offer.clone(), offer.by.clone(), &alice.master(0)).unwrap();
    for signed in [&by_device, &by_old] {
        let reply = t.open_offer(signed).await;
        refused_as(&reply, StatusCode::FORBIDDEN, "signer");
    }
    assert_eq!(t.store.held.folds(), folds);
    let reply = t.open_offer(&offer).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    assert_eq!(t.store.held.folds(), folds + 1);
}

#[tokio::test]
async fn a_request_by_a_bot_key_replays_nothing() {
    let (t, mut alice) = unheld_alice().await;
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
    assert!(!t.store.held.holds(&alice.scid()));
    let folds = t.store.held.folds();
    let path = format!("/v1/notes/{}", alice.scid());
    let body = serde_json::to_vec(&json!({ "token": "a-token" })).unwrap();
    for _ in 0..5 {
        // Validly signed by the bot under its own id: refused from the key index alone.
        let header = alice.request(&bot_id, &bot.key, "POST", &path, &body, t.now() + 60);
        let reply = t
            .send("POST", &path, Some(body.clone()), Some(header))
            .await;
        refused_as(&reply, StatusCode::FORBIDDEN, "signer");
        assert!(
            reply.body["detail"]
                .as_str()
                .unwrap_or_default()
                .contains("a bot key signs no request"),
            "{}",
            reply.body
        );
    }
    assert_eq!(t.store.held.folds(), folds);
}

#[tokio::test]
async fn a_request_by_a_revoked_key_replays_nothing() {
    let (t, mut alice) = unheld_alice().await;
    // A device key on a master route.
    let folds = t.store.held.folds();
    let offers = format!("/v1/offers/{}", alice.scid());
    let header = alice.request(
        "d1",
        &alice.device_key(1),
        "GET",
        &offers,
        b"",
        t.now() + 60,
    );
    let reply = t.send("GET", &offers, None, Some(header)).await;
    refused_as(&reply, StatusCode::FORBIDDEN, "signer");
    assert_eq!(t.store.held.folds(), folds);

    let revoke = alice.revoke(&["d1"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let folds = t.store.held.folds();
    let path = format!("/v1/notes/{}", alice.scid());
    let body = serde_json::to_vec(&json!({ "token": "a-token" })).unwrap();
    for _ in 0..5 {
        let header = alice.request(
            "d1",
            &alice.device_key(1),
            "POST",
            &path,
            &body,
            t.now() + 60,
        );
        let reply = t
            .send("POST", &path, Some(body.clone()), Some(header))
            .await;
        refused_as(&reply, StatusCode::FORBIDDEN, "signer");
        assert!(
            reply.body["detail"]
                .as_str()
                .unwrap_or_default()
                .contains("not valid at the head"),
            "{}",
            reply.body
        );
    }
    assert_eq!(t.store.held.folds(), folds);
}
