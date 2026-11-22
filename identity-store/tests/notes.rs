//! Vouching notes (slice 3.C, C2) against the test vouching service (`common::vouch`), which
//! implements the platform's redeem contract: a note with event #0, a note beside a log, who may
//! ask for one (decision 21.3), and how the service's answers map.

mod common;

use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{verify_note, BotScope, Event, VouchNote};
use ad4m_ldk::identity::LogPage;
use axum::http::StatusCode;
use identity_store::Settings;
use serde_json::{json, Value};

use common::agent::{Device, Person};
use common::vouch::{vouching_key, vouching_multikey, VouchServer};
use common::{Reply, TestStore, T0};

/// The time the test service writes into its notes.
const NOTE_TIME: u64 = T0 - 5;

async fn world() -> (TestStore, VouchServer) {
    let server = VouchServer::start(NOTE_TIME).await;
    let t = TestStore::with_settings(TestStore::vouch_settings(&server.url()));
    (t, server)
}

/// `POST` of the person's #0 with `extra` members (`vouch`, `pow`).
async fn post_new(t: &TestStore, person: &Person, extra: Value) -> Reply {
    let mut body = TestStore::append_body(&[&person.events[0]]);
    for (name, value) in extra.as_object().unwrap() {
        body[name] = value.clone();
    }
    t.post(&format!("/v1/logs/{}/events", person.scid()), &body)
        .await
}

async fn page(t: &TestStore, person: &Person) -> LogPage {
    let reply = t.get(&format!("/v1/logs/{}", person.scid())).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    serde_json::from_value(reply.body).unwrap()
}

fn core(note: &ad4m_ldk::identity::VouchNote) -> VouchNote {
    serde_json::from_value(serde_json::to_value(note).unwrap()).unwrap()
}

fn assert_vouch_refused(reply: &Reply) {
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "vouch");
}

fn assert_unavailable(reply: &Reply) {
    assert_eq!(
        reply.status,
        StatusCode::SERVICE_UNAVAILABLE,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["error"], "vouch-unavailable");
}

/// `POST /v1/notes/{scid} {token}` signed by `key_id`/`signer` of `person`.
async fn ask_note(
    t: &TestStore,
    person: &Person,
    key_id: &str,
    signer: &ed25519_dalek::SigningKey,
    token: &str,
) -> Reply {
    let path = format!("/v1/notes/{}", person.scid());
    let body = serde_json::to_vec(&json!({ "token": token })).unwrap();
    let header = person.request(key_id, signer, "POST", &path, &body, t.now() + 60);
    t.send("POST", &path, Some(body), Some(header)).await
}

fn bot_scope(expires: u64) -> BotScope {
    BotScope {
        can: vec!["read".into()],
        expires,
    }
}

#[tokio::test]
async fn a_new_log_with_a_token_and_a_proof_is_400_and_redeems_nothing() {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    let proof = t.pow_for(&alice.scid());
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice", "pow": proof })).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "vouch-and-pow");
    assert_eq!(server.redeems(), 0);
    assert_eq!(t.count("logs"), 0);
}

#[tokio::test]
async fn a_new_log_with_a_valid_token_is_200_and_vouched_and_a_used_token_is_403_with_nothing_committed(
) {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let read = page(&t, &alice).await;
    assert!(read.vouched);
    let note = read.note.expect("the note beside the log");
    assert_eq!(note.did, alice.did());
    assert_eq!(note.method, "email");
    assert_eq!(note.time, NOTE_TIME);
    assert_eq!(note.voucher, format!("did:key:{}", vouching_multikey()));
    assert!(verify_note(&core(&note), &[vouching_multikey()]));
    assert_eq!(t.admitted_by(&alice.scid()), "vouch");

    // The used token for another identity: 403 vouch, nothing committed.
    let bob = Person::new("bob");
    assert_vouch_refused(&post_new(&t, &bob, json!({ "vouch": "t-alice" })).await);
    // A spent token and a token the service never gave: the same.
    server.give("t-spent", "bob@example.org");
    server.spend("t-spent");
    assert_vouch_refused(&post_new(&t, &bob, json!({ "vouch": "t-spent" })).await);
    assert_vouch_refused(&post_new(&t, &bob, json!({ "vouch": "t-never" })).await);
    // A fresh token for an address that already vouched another identity (409 at the service).
    server.give("t-alice-again", "alice@example.org");
    assert_vouch_refused(&post_new(&t, &bob, json!({ "vouch": "t-alice-again" })).await);
    assert_eq!(
        t.get(&format!("/v1/logs/{}", bob.scid())).await.status,
        StatusCode::NOT_FOUND
    );
    assert_eq!(t.count("logs"), 1);
    assert_eq!(t.count("notes"), 1);

    // A repeat of #0 with its token on the log the store holds: 409 with the head, as any
    // repeat of #0, and nothing redeemed.
    let redeems = server.redeems();
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
    assert_eq!(reply.body["head"]["seq"], 0, "{}", reply.body);
    assert_eq!(server.redeems(), redeems);
    assert_eq!(t.count("notes"), 1);
}

#[tokio::test]
async fn a_token_is_redeemed_only_after_event_0_replays() {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    let mut unsigned = alice.json(0);
    unsigned["proof"] = json!([]);
    let reply = t
        .post(
            &format!("/v1/logs/{}/events", alice.scid()),
            &json!({ "events": [unsigned], "vouch": "t-alice" }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(server.redeems(), 0);
    // The token is still good for the valid event.
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(server.redeems(), 1);
}

#[tokio::test]
async fn a_vouched_new_log_takes_nothing_from_the_brake() {
    let server = VouchServer::start(NOTE_TIME).await;
    let t = TestStore::with_settings(Settings {
        brake_global_per_hour: 1,
        brake_addr_burst: 1,
        ..TestStore::vouch_settings(&server.url())
    });
    // The brake's buckets emptied by one no-note log.
    t.publish(&Person::new("carol")).await;
    let dave = Person::new("dave");
    let reply = post_new(&t, &dave, json!({ "pow": t.pow_for(&dave.scid()) })).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    let before = String::from_utf8(t.get("/v1/metrics").await.raw).unwrap();

    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(t.admitted_by(&alice.scid()), "vouch");
    let after = String::from_utf8(t.get("/v1/metrics").await.raw).unwrap();
    assert_eq!(before, after);
}

#[tokio::test]
async fn a_service_that_fails_or_answers_a_note_the_store_does_not_trust_is_503() {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    // A 5xx.
    server.fail(true);
    assert_unavailable(&post_new(&t, &alice, json!({ "vouch": "t-alice" })).await);
    server.fail(false);
    assert_eq!(t.count("logs"), 0);

    // Unreachable: a port nothing listens on.
    let closed = {
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        listener.local_addr().unwrap()
    };
    let unreachable =
        TestStore::with_settings(TestStore::vouch_settings(&format!("http://{closed}")));
    assert_unavailable(&post_new(&unreachable, &alice, json!({ "vouch": "t-alice" })).await);
    // Another credential: the service answers 401.
    let wrong = TestStore::with_settings(Settings {
        vouch_credential: Some(identity_store::vouch::Credential("another".into())),
        ..TestStore::vouch_settings(&server.url())
    });
    assert_unavailable(&post_new(&wrong, &alice, json!({ "vouch": "t-alice" })).await);
    // A note by a key the store does not list in VOUCH_KEYS.
    let other_key = common::agent::multikey(&common::agent::key("another voucher"));
    let untrusting = TestStore::with_settings(Settings {
        vouch_keys: vec![other_key],
        ..TestStore::vouch_settings(&server.url())
    });
    assert_unavailable(&post_new(&untrusting, &alice, json!({ "vouch": "t-alice" })).await);
    assert_eq!(untrusting.count("logs"), 0);

    // The same token for the same identity answers the same note again: the first store takes it.
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn a_second_redeem_of_a_token_for_the_same_identity_answers_the_same_note() {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let did = Person::new("alice").did();
    let first = t.store.redeem("t-alice", &did).await.unwrap();
    let again = t.store.redeem("t-alice", &did).await.unwrap();
    assert_eq!(first, again);
    let other = t
        .store
        .redeem("t-alice", &Person::new("bob").did())
        .await
        .unwrap_err();
    assert_eq!(other.status, StatusCode::FORBIDDEN);
    // The note the test service signs is a note under the test vouching key.
    let made = ad4m_identity_core::sign_note(&did, "email", NOTE_TIME, &vouching_key()).unwrap();
    assert_eq!(core(&first), made);
}

#[tokio::test]
async fn a_note_beside_a_log_is_201_and_a_second_is_409_already_vouched() {
    let (t, server) = world().await;
    let alice = Person::new("alice");
    t.publish(&alice).await;
    assert!(!page(&t, &alice).await.vouched);
    server.give("t-alice", "alice@example.org");
    let reply = ask_note(&t, &alice, "d1", &alice.device_key(1), "t-alice").await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    let note: ad4m_ldk::identity::VouchNote = serde_json::from_value(reply.body).unwrap();
    assert_eq!(note.did, alice.did());
    let read = page(&t, &alice).await;
    assert!(read.vouched);
    assert_eq!(read.note, Some(note));
    // The log keeps how it was admitted.
    assert_eq!(t.admitted_by(&alice.scid()), "brake");

    // A second note: 409, and no token is spent on it.
    server.give("t-alice-2", "alice2@example.org");
    let redeems = server.redeems();
    let reply = ask_note(&t, &alice, "d1", &alice.device_key(1), "t-alice-2").await;
    assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);
    assert_eq!(reply.body["error"], "already-vouched");
    assert_eq!(server.redeems(), redeems);
    // A vouched #0 log has its note too.
    let bob = Person::new("bob");
    server.give("t-bob", "bob@example.org");
    assert_eq!(
        post_new(&t, &bob, json!({ "vouch": "t-bob" })).await.status,
        StatusCode::OK
    );
    server.give("t-bob-2", "bob2@example.org");
    let reply = ask_note(&t, &bob, "d1", &bob.device_key(1), "t-bob-2").await;
    assert_eq!(reply.status, StatusCode::CONFLICT, "{}", reply.body);

    // A token the service refuses: 403 vouch, nothing stored.
    let carol = Person::new("carol");
    t.publish(&carol).await;
    let reply = ask_note(&t, &carol, "d1", &carol.device_key(1), "t-alice").await;
    assert_vouch_refused(&reply);
    assert!(page(&t, &carol).await.note.is_none());
}

#[tokio::test]
async fn a_note_request_by_a_device_key_or_the_master_is_201_and_by_a_bot_or_revoked_key_403() {
    let (t, server) = world().await;
    for (token, email) in [
        ("t-1", "one@example.org"),
        ("t-2", "two@example.org"),
        ("t-3", "three@example.org"),
        ("t-4", "four@example.org"),
    ] {
        server.give(token, email);
    }
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(
            &mut alice,
            &bot,
            OfferKind::Bot,
            Some(bot_scope(T0 + 86_400)),
        )
        .await;
    let phone = Device::new("alice phone");
    let phone_id = t.link(&mut alice, &phone, OfferKind::Device, None).await;
    let revoke = alice.revoke(&[&phone_id]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );

    // A bot key: 403.
    let reply = ask_note(&t, &alice, &bot_id, &bot.key, "t-1").await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    // A key revoked at the head: 403.
    let reply = ask_note(&t, &alice, &phone_id, &phone.key, "t-1").await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(server.redeems(), 0);
    // A device key valid at the head: 201.
    let reply = ask_note(&t, &alice, "d1", &alice.device_key(1), "t-1").await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);

    // The current master key, on another log: 201.
    let bob = Person::new("bob");
    t.publish(&bob).await;
    let master = bob.current_master();
    let reply = ask_note(&t, &bob, &bob.master_id(), &master, "t-2").await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    assert!(page(&t, &bob).await.vouched);
}

#[tokio::test]
async fn a_store_that_no_longer_trusts_the_voucher_shows_the_note_but_not_vouched() {
    let (t, server) = world().await;
    server.give("t-alice", "alice@example.org");
    let alice = Person::new("alice");
    assert_eq!(
        post_new(&t, &alice, json!({ "vouch": "t-alice" }))
            .await
            .status,
        StatusCode::OK
    );
    let t = t.restarted_with(Settings {
        vouch_keys: Vec::new(),
        ..TestStore::vouch_settings(&server.url())
    });
    let read = page(&t, &alice).await;
    assert!(read.note.is_some());
    assert!(!read.vouched);
}

#[tokio::test]
async fn a_settled_page_carries_no_note_and_the_page_with_the_head_does() {
    let (t, server) = world().await;
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    // 257 rotations on #0: the first page of 256 ends below the head and is cached for good.
    let mut rotations: Vec<Event> = Vec::new();
    for _ in 0..257 {
        let rotate = alice.rotate(&[]);
        alice.push(rotate.clone());
        rotations.push(rotate);
    }
    for batch in rotations.chunks(32) {
        let batch: Vec<&Event> = batch.iter().collect();
        let reply = t.append(&alice.scid(), &batch).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    }
    // The note comes after the first page settled: a cached copy of that page could never
    // show it, so no settled page carries one.
    server.give("t-alice", "alice@example.org");
    let reply = ask_note(&t, &alice, "d1", &alice.device_key(1), "t-alice").await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);

    let settled = t.get(&format!("/v1/logs/{}?from=0", alice.scid())).await;
    assert_eq!(
        settled
            .headers
            .get("cache-control")
            .and_then(|v| v.to_str().ok()),
        Some("public, max-age=31536000, immutable")
    );
    let settled: LogPage = serde_json::from_value(settled.body).unwrap();
    assert!(settled.note.is_none());
    assert!(!settled.vouched);

    let last: LogPage = serde_json::from_value(
        t.get(&format!("/v1/logs/{}?from=256", alice.scid()))
            .await
            .body,
    )
    .unwrap();
    assert!(last.vouched);
    assert!(last.note.is_some());
}

/// The same signed #0 sent again and again with a token that never redeems costs the store a
/// replay each time only until the sender's failure bucket is empty: a refused token takes a
/// token from it, and an empty bucket is `429` before any signature is checked.
#[tokio::test]
async fn an_arbitrary_token_repeated_empties_the_failure_bucket_and_then_replays_nothing() {
    let (t, server) = world().await;
    let alice = Person::new("alice");
    for i in 0..30 {
        let reply = post_new(&t, &alice, json!({ "vouch": format!("junk-{i}") })).await;
        assert_vouch_refused(&reply);
    }
    assert_eq!(server.redeems(), 30);
    // The 31st is 429 failure: a broken signature would be 400 had it been replayed, a valid
    // token would have been redeemed.
    let mut broken = alice.json(0);
    broken["proof"][0]["proofValue"] =
        Person::new("alice other").json(0)["proof"][0]["proofValue"].clone();
    let reply = t
        .post(
            &format!("/v1/logs/{}/events", alice.scid()),
            &json!({ "events": [broken], "vouch": "junk-30" }),
        )
        .await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["scope"], "failure");
    server.give("t-alice", "alice@example.org");
    let reply = post_new(&t, &alice, json!({ "vouch": "t-alice" })).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(server.redeems(), 30);
    assert_eq!(t.count("logs"), 0);
    // Another address is not affected.
    let other = "198.51.100.44:1000".parse().unwrap();
    let body = {
        let mut body = TestStore::append_body(&[&alice.events[0]]);
        body["vouch"] = json!("t-alice");
        body
    };
    let reply = t
        .send_from(
            other,
            "POST",
            &format!("/v1/logs/{}/events", alice.scid()),
            Some(serde_json::to_vec(&body).unwrap()),
            None,
            &[],
        )
        .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

/// With no `VOUCH_URL` every token is `503`, and each such request is charged too: a store that
/// cannot redeem is no free replay for a flood.
#[tokio::test]
async fn a_token_at_a_store_with_no_vouching_service_is_charged_and_then_429() {
    let t = TestStore::with_settings(TestStore::brake_settings());
    let alice = Person::new("alice");
    for i in 0..30 {
        let reply = post_new(&t, &alice, json!({ "vouch": format!("junk-{i}") })).await;
        assert_unavailable(&reply);
    }
    let reply = post_new(&t, &alice, json!({ "vouch": "junk-30" })).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["scope"], "failure");
}

/// Waits until a redeem reached the held vouching service.
async fn until_redeeming(server: &VouchServer) {
    while server.redeems() == 0 {
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
}

/// A key revoked while the voucher answers does not fill the identity's one note slot: the
/// standing is checked again in the transaction that stores the note.
#[tokio::test]
async fn a_device_revoked_while_the_voucher_waits_gets_403_and_the_slot_stays_free() {
    let (t, server) = world().await;
    server.give("t-1", "one@example.org");
    server.give("t-2", "two@example.org");
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let phone_id = t.link(&mut alice, &phone, OfferKind::Device, None).await;
    server.hold();
    let revoke = alice.revoke(&[&phone_id]);
    let (reply, revoked) =
        tokio::join!(ask_note(&t, &alice, &phone_id, &phone.key, "t-1"), async {
            until_redeeming(&server).await;
            let reply = t.append(&alice.scid(), &[&revoke]).await;
            server.release();
            reply
        });
    assert_eq!(revoked.status, StatusCode::OK, "{}", revoked.body);
    assert_vouch_refused(&reply);
    assert_eq!(t.count("notes"), 0);
    assert!(page(&t, &alice).await.note.is_none());
    // The slot is free: a key that still stands fills it.
    alice.push(revoke);
    let reply = ask_note(&t, &alice, "d1", &alice.device_key(1), "t-2").await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
}

/// The master a `rotate` retires while the voucher answers is refused the same way.
#[tokio::test]
async fn a_master_retired_while_the_voucher_waits_gets_403_and_the_slot_stays_free() {
    let (t, server) = world().await;
    server.give("t-1", "one@example.org");
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let old_master = alice.current_master();
    let old_id = alice.master_id();
    server.hold();
    let rotate = alice.rotate(&[]);
    let (reply, rotated) = tokio::join!(ask_note(&t, &alice, &old_id, &old_master, "t-1"), async {
        until_redeeming(&server).await;
        let reply = t.append(&alice.scid(), &[&rotate]).await;
        server.release();
        reply
    });
    assert_eq!(rotated.status, StatusCode::OK, "{}", rotated.body);
    assert_vouch_refused(&reply);
    assert_eq!(t.count("notes"), 0);
}
