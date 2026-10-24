//! Logs, heads, the key index, signed requests, limits and agent records, on a real
//! store over a temporary SQLite file.

mod common;

use std::time::{Duration, Instant};

use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{pow, BotScope};
use ad4m_ldk::identity::{KeyIndexEntry, LogPage, SignedHead};
use axum::http::StatusCode;
use identity_store::{Fault, Settings};
use serde_json::{json, Value};

use common::agent::{did_key_record, key, multikey, Device, Person};
use common::{verify_head, TestStore, T0};

fn head_of(body: &Value) -> SignedHead {
    serde_json::from_value(body["head"].clone()).unwrap()
}

fn bot_scope(expires: u64) -> BotScope {
    BotScope {
        can: vec!["read".into()],
        expires,
    }
}

#[tokio::test]
async fn an_append_on_the_head_passes_the_same_seq_again_and_a_gap_get_409_with_the_head() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None, None)
        .await;
    assert_eq!(alice.head_seq(), 1);

    // The same seq again: an event at seq 1 that differs from the stored one.
    let mut fork = Person::new("alice");
    let other = Device::new("alice other");
    let (offer, s) = fork.offer(OfferKind::Device, None, None, t.now() + 300);
    let claim = other.claim(&fork.scid(), &offer.offer.offer_id, s, false);
    let add = fork.add_claimed(&offer.offer, &other.view(&claim, t.now()), None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    let head = head_of(&reply.body);
    assert_eq!(head.seq, 1);
    assert_eq!(head.hash, alice.chain.head().hash);

    // A gap: seq 3 on a head at 1.
    let revoke = alice.revoke(&["d2"]);
    let mut gap = revoke.clone();
    gap.seq = 3;
    let reply = t.append(&alice.scid(), &[&gap]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(head_of(&reply.body).seq, 1);

    // The head continues: 200.
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(head_of(&reply.body).seq, 2);
}

#[tokio::test]
async fn a_key_of_log_a_in_an_incept_of_log_b_is_taken_also_after_a_revoked_it() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let laptop = Device::new("shared laptop");
    let id = t
        .link(&mut alice, &laptop, OfferKind::Device, None, None)
        .await;

    // Bob's #0 lists the laptop's key, co-signed by it.
    let bob_incept = |laptop: &Device| {
        let bob = Person::new("bob");
        let device = ad4m_identity_core::DeviceKey {
            id: "d1".into(),
            sign: laptop.sign_key(),
            enc: common::agent::enc_of(&laptop.key),
            life: ad4m_identity_core::KeyLife::Standing,
        };
        let mut event =
            ad4m_identity_core::build::incept_human(&bob.master(0), bob.next(1), vec![device])
                .unwrap();
        ad4m_identity_core::build::cosign(&mut event, "d1", &laptop.key).unwrap();
        event
    };
    let event = bob_incept(&laptop);
    let reply = t.append(&event.scid, &[&event]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["error"], "key-taken");
    assert_eq!(reply.body["key"], laptop.sign_key());

    let revoke = alice.revoke(&[&id]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );
    let reply = t.append(&event.scid, &[&event]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["error"], "key-taken");
    assert_eq!(
        t.get(&format!("/v1/heads/{}", event.scid)).await.status,
        StatusCode::NOT_FOUND
    );

    // Alice's master key in Bob's #0 is taken too.
    let bob = Person::new("bob");
    let bob_d1 = bob.device_key(1);
    let device = ad4m_identity_core::DeviceKey {
        id: "d1".into(),
        sign: multikey(&bob_d1),
        enc: common::agent::enc_of(&bob_d1),
        life: ad4m_identity_core::KeyLife::Standing,
    };
    let mut event =
        ad4m_identity_core::build::incept_human(&alice.master(0), bob.next(1), vec![device])
            .unwrap();
    ad4m_identity_core::build::cosign(&mut event, "d1", &bob_d1).unwrap();
    let reply = t.append(&event.scid, &[&event]).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["key"], multikey(&alice.master(0)));
}

#[tokio::test]
async fn a_crash_between_the_event_insert_and_the_key_index_leaves_neither() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.store.inject_fault(Fault::AfterEvents);
    let reply = t.append(&alice.scid(), &[&alice.events[0]]).await;
    assert_eq!(reply.status, StatusCode::INTERNAL_SERVER_ERROR);
    for table in [
        "chain_entries",
        "chain_heads",
        "logs",
        "heads",
        "keys",
        "feed",
    ] {
        assert_eq!(t.count(table), 0, "{table} holds a row after the crash");
    }
    let d1 = multikey(&alice.device_key(1));
    assert_eq!(
        t.get(&format!("/v1/keys/{d1}")).await.status,
        StatusCode::NOT_FOUND
    );
    assert_eq!(
        t.get(&format!("/v1/logs/{}", alice.scid())).await.status,
        StatusCode::NOT_FOUND
    );

    t.store.inject_fault(Fault::None);
    t.publish(&alice).await;
    assert_eq!(
        t.get(&format!("/v1/keys/{d1}")).await.status,
        StatusCode::OK
    );
}

#[tokio::test]
async fn every_events_response_carries_a_head_that_verifies_under_the_store_key() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    let reply = t.append(&alice.scid(), &[&alice.events[0]]).await;
    let trusted = [t.store_key()];
    let other = [multikey(&key("another store"))];
    let head = head_of(&reply.body);
    assert!(verify_head(&head, &trusted));
    assert!(!verify_head(&head, &other));
    assert_eq!(head.store, format!("did:key:{}", t.store_key()));

    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None, None)
        .await;
    let page: LogPage = serde_json::from_value(
        t.get(&format!("/v1/logs/{}?from=0", alice.scid()))
            .await
            .body,
    )
    .unwrap();
    assert!(verify_head(&page.head, &trusted));
    assert_eq!(page.head.seq, 1);
    assert_eq!(page.events.len(), 2);
    assert_eq!(page.appended_at, vec![T0, T0]);
    assert_eq!(page.events[1], alice.json(1));
    assert!(!page.vouched);
    assert!(page.note.is_none());
    let from_one: LogPage = serde_json::from_value(
        t.get(&format!("/v1/logs/{}?from=1", alice.scid()))
            .await
            .body,
    )
    .unwrap();
    assert_eq!(from_one.events, vec![alice.json(1)]);

    let head: SignedHead =
        serde_json::from_value(t.get(&format!("/v1/heads/{}", alice.scid())).await.body).unwrap();
    assert!(verify_head(&head, &trusted));
    let mut forged = head.clone();
    forged.seq = 0;
    assert!(!verify_head(&forged, &trusted));
}

#[tokio::test]
async fn the_key_index_answers_revoked_keys_bot_keys_with_scope_and_404_for_unknown_keys() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let scope = bot_scope(T0 + 86_400);
    let bot_id = t
        .link(&mut alice, &bot, OfferKind::Bot, Some(scope.clone()), None)
        .await;
    let revoke = alice.revoke(&["d1"]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );

    let d1 = multikey(&alice.device_key(1));
    let entry: KeyIndexEntry =
        serde_json::from_value(t.get(&format!("/v1/keys/{d1}")).await.body).unwrap();
    assert_eq!(entry.did, alice.did());
    assert_eq!(entry.key_id, "d1");
    assert_eq!(entry.added, 0);
    assert_eq!(entry.revoked, Some(2));
    assert_eq!(entry.head.seq, 2);
    assert!(verify_head(&entry.head, &[t.store_key()]));

    let entry: KeyIndexEntry =
        serde_json::from_value(t.get(&format!("/v1/keys/{}", bot.sign_key())).await.body).unwrap();
    assert_eq!(entry.key_id, bot_id);
    assert_eq!(entry.revoked, None);
    assert_eq!(entry.bot, Some(serde_json::to_value(&scope).unwrap()));
    assert_eq!(entry.expires, Some(scope.expires));
    assert!(!entry.paused);

    let master: KeyIndexEntry = serde_json::from_value(
        t.get(&format!("/v1/keys/{}", multikey(&alice.master(0))))
            .await
            .body,
    )
    .unwrap();
    assert_eq!(master.key_id, "m0");

    let unknown = multikey(&key("nobody"));
    let reply = t.get(&format!("/v1/keys/{unknown}")).await;
    assert_eq!(reply.status, StatusCode::NOT_FOUND);
    assert_eq!(
        t.get(&format!("/v1/keys/{unknown}?wait=9")).await.status,
        StatusCode::BAD_REQUEST
    );
}

#[tokio::test]
async fn an_add_with_expires_in_the_past_or_401_days_ahead_is_refused_and_the_index_answers_expires(
) {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let browser = Device::new("alice browser");
    for expires in [T0 - 1, T0 + 401 * 86_400] {
        let (offer, view) = t
            .claimed_offer(&mut alice, &browser, OfferKind::Device, None)
            .await;
        let add = alice.add_claimed(&offer.offer, &view, Some(expires));
        let reply = t.append(&alice.scid(), &[&add]).await;
        assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
        assert_eq!(reply.body["error"], "expires");
        // Cancel, so the next offer's claim by the same key is not a second claim.
        let path = format!("/v1/offers/{}/{}", alice.scid(), offer.offer.offer_id);
        let header = alice.master_request("DELETE", &path, b"", t.now() + 60);
        assert_eq!(
            t.send("DELETE", &path, None, Some(header)).await.status,
            StatusCode::NO_CONTENT
        );
    }
    let expires = T0 + 30 * 86_400;
    t.link(&mut alice, &browser, OfferKind::Device, None, Some(expires))
        .await;
    let entry: KeyIndexEntry = serde_json::from_value(
        t.get(&format!("/v1/keys/{}", browser.sign_key()))
            .await
            .body,
    )
    .unwrap();
    assert_eq!(entry.expires, Some(expires));
    assert_eq!(entry.bot, None);
    assert!(!entry.paused);
    let page = t.get(&format!("/v1/logs/{}", alice.scid())).await.body;
    assert_eq!(page["pauses"], json!([]));
}

#[tokio::test]
async fn a_key_index_wait_answers_within_one_second_of_the_append() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, view) = t
        .claimed_offer(&mut alice, &phone, OfferKind::Device, None)
        .await;

    let router = t.router.clone();
    let path = format!("/v1/keys/{}?wait=8", phone.sign_key());
    let waiting = tokio::spawn(async move {
        use http_body_util::BodyExt;
        use tower::ServiceExt;
        let request = axum::http::Request::get(path)
            .body(axum::body::Body::empty())
            .unwrap();
        let response = router.oneshot(request).await.unwrap();
        let status = response.status();
        let body = response.into_body().collect().await.unwrap().to_bytes();
        (status, Instant::now(), body)
    });
    tokio::time::sleep(Duration::from_millis(300)).await;
    let add = alice.add_claimed(&offer.offer, &view, None);
    let appended = Instant::now();
    assert_eq!(t.append_push(&mut alice, add).await.status, StatusCode::OK);
    let (status, answered, body) = waiting.await.unwrap();
    assert_eq!(status, StatusCode::OK);
    assert!(answered.duration_since(appended) < Duration::from_secs(1));
    let entry: KeyIndexEntry = serde_json::from_slice(&body).unwrap();
    assert_eq!(entry.key_id, "d2");

    // An unknown key with a wait answers 404 at the timeout.
    let started = Instant::now();
    let reply = t
        .get(&format!("/v1/keys/{}?wait=1", multikey(&key("nobody"))))
        .await;
    assert_eq!(reply.status, StatusCode::NOT_FOUND);
    assert!(started.elapsed() >= Duration::from_millis(900));
}

#[tokio::test]
async fn signed_requests_expired_replayed_wrong_body_bot_key_and_revoked_key_are_refused() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(
            &mut alice,
            &bot,
            OfferKind::Bot,
            Some(bot_scope(T0 + 86_400)),
            None,
        )
        .await;
    let phone = Device::new("alice phone");
    let phone_id = t
        .link(&mut alice, &phone, OfferKind::Device, None, None)
        .await;

    let path = format!("/v1/notes/{}", alice.scid());
    let body = serde_json::to_vec(&json!({ "token": "a-token" })).unwrap();
    let send = |header: String| t.send("POST", &path, Some(body.clone()), Some(header));
    let d1 = alice.device_key(1);
    let later = t.now() + 60;

    // A good request by a device key reaches the vouching step: no vouching service exists yet.
    let good = alice.request("d1", &d1, "POST", &path, &body, later);
    let reply = send(good.clone()).await;
    assert_eq!(
        reply.status,
        StatusCode::SERVICE_UNAVAILABLE,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["error"], "vouch-unavailable");
    // The current master key passes too.
    let by_master = alice.master_request("POST", &path, &body, later);
    assert_eq!(
        send(by_master).await.status,
        StatusCode::SERVICE_UNAVAILABLE
    );

    let refused = |reply: common::Reply| {
        assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
        assert_eq!(reply.body["error"], "signer");
        reply.body["detail"].as_str().unwrap().to_string()
    };
    // Replayed nonce.
    assert!(refused(send(good).await).contains("nonce"));
    // Expired, and too far ahead.
    let expired = alice.request("d1", &d1, "POST", &path, &body, t.now());
    assert!(refused(send(expired).await).contains("expired"));
    let ahead = alice.request("d1", &d1, "POST", &path, &body, t.now() + 301);
    assert!(refused(send(ahead).await).contains("5 minutes"));
    // Wrong body hash.
    let wrong_body = alice.request("d1", &d1, "POST", &path, b"{}", later);
    assert!(refused(send(wrong_body).await).contains("bodyHash"));
    // Another path.
    let other_path = alice.request("d1", &d1, "POST", "/v1/notes/other", &body, later);
    assert!(refused(send(other_path).await).contains("path"));
    // A bot key.
    let by_bot = alice.request(&bot_id, &bot.key, "POST", &path, &body, later);
    assert!(refused(send(by_bot).await).contains("bot"));
    // A key that is not the one `keyId` names.
    let wrong_key = alice.request("d1", &phone.key, "POST", &path, &body, later);
    assert!(refused(send(wrong_key).await).contains("signature"));
    // A key revoked at the head.
    let revoke = alice.revoke(&[&phone_id]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );
    let revoked = alice.request(&phone_id, &phone.key, "POST", &path, &body, later);
    assert!(refused(send(revoked).await).contains("not valid"));
    // An old master after a rotation.
    let rotate = alice.rotate(&[]);
    let old_master = alice.master_request("POST", &path, &body, later);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    assert!(refused(send(old_master).await).contains("not valid"));
    // A position the store does not hold.
    let mut body_at = alice.request_body("d1", "POST", &path, &body, later);
    body_at.kel_seq = 40;
    let unknown = Person::sign_request(body_at, &d1);
    assert!(refused(send(unknown).await).contains("position"));
    // No header at all.
    let reply = t.send("POST", &path, Some(body.clone()), None).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn a_body_over_64_kib_gets_413_and_33_events_get_400() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    let big = vec![b' '; 64 * 1024 + 1];
    let reply = t
        .send(
            "POST",
            &format!("/v1/logs/{}/events", alice.scid()),
            Some(big),
            None,
        )
        .await;
    assert_eq!(reply.status, StatusCode::PAYLOAD_TOO_LARGE);

    let event = alice.json(0);
    let events: Vec<Value> = (0..33).map(|_| event.clone()).collect();
    let reply = t
        .post(
            &format!("/v1/logs/{}/events", alice.scid()),
            &json!({ "events": events }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "too-many-events");
    assert_eq!(t.count("chain_entries"), 0);
}

#[tokio::test]
async fn a_new_log_with_no_note_passes_without_a_proof_and_a_sent_proof_must_meet_the_bits() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    let path = format!("/v1/logs/{}/events", alice.scid());
    let events = json!([alice.json(0)]);

    // Token and proof together: 400.
    let proof = pow::solve(&alice.scid(), 8);
    let both =
        json!({ "events": events, "vouch": "tok", "pow": { "nonce": proof.nonce, "bits": 8 } });
    let reply = t.post(&path, &both).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "vouch-and-pow");
    // A token: no vouching service exists yet, nothing committed.
    let reply = t
        .post(&path, &json!({ "events": events, "vouch": "tok" }))
        .await;
    assert_eq!(reply.status, StatusCode::SERVICE_UNAVAILABLE);
    assert_eq!(reply.body["error"], "vouch-unavailable");
    assert_eq!(t.count("chain_entries"), 0);
    // A proof for another SCID: 403 pow.
    let bob = Person::new("bob");
    let wrong = pow::solve(&bob.scid(), 8);
    let reply = t
        .post(
            &path,
            &json!({ "events": events, "pow": { "nonce": wrong.nonce, "bits": 8 } }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "pow");
    assert_eq!(t.count("logs"), 0);
    // A proof that meets the bits: 200.
    let reply = t
        .post(
            &path,
            &json!({ "events": events, "pow": { "nonce": proof.nonce, "bits": 8 } }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // No proof at all: 200 (the store has no brake yet).
    t.publish(&bob).await;
    let page = t.get(&format!("/v1/logs/{}", bob.scid())).await.body;
    assert_eq!(page["vouched"], false);
    // A proof on a log that exists: 400.
    let reply = t
        .post(
            &path,
            &json!({ "events": events, "pow": { "nonce": proof.nonce, "bits": 8 } }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);

    let pow_params = t.get("/v1/pow").await.body;
    assert_eq!(pow_params["bits"], 8);
    assert_eq!(pow_params["previousBits"], 8);
    assert_eq!(pow_params["changedAt"], T0);
}

#[tokio::test]
async fn the_pow_defaults_to_22_bits() {
    let t = TestStore::with_settings(Settings::default());
    assert_eq!(t.get("/v1/pow").await.body["bits"], 22);
}

#[tokio::test]
async fn a_malformed_event_or_a_wrong_scid_is_400_and_an_unknown_log_is_404() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    let bob = Person::new("bob");
    let reply = t.append(&bob.scid(), &[&alice.events[0]]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "wrong-chain");
    let mut float = alice.json(0);
    float["seq"] = json!(0.5);
    let reply = t
        .post(
            &format!("/v1/logs/{}/events", alice.scid()),
            &json!({ "events": [float] }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    let mut unsigned = alice.json(0);
    unsigned["proof"] = json!([]);
    let reply = t
        .post(
            &format!("/v1/logs/{}/events", alice.scid()),
            &json!({ "events": [unsigned] }),
        )
        .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(
        t.get(&format!("/v1/logs/{}", alice.scid())).await.status,
        StatusCode::NOT_FOUND
    );
    assert_eq!(
        t.get(&format!("/v1/heads/{}", alice.scid())).await.status,
        StatusCode::NOT_FOUND
    );
}

#[tokio::test]
async fn a_did_key_record_signed_by_its_key_is_stored_and_one_signed_by_another_key_is_403() {
    let t = TestStore::new();
    let guest = key("guest");
    let profile = json!({ "perspective": { "links": [] }, "directMessageLanguage": null });
    let record = did_key_record(&guest, profile.clone(), 1);
    let path = format!("/v1/agents/{}", record.did);
    let put = |record: &ad4m_ldk::identity::AgentRecord| {
        t.send(
            "PUT",
            &path,
            Some(serde_json::to_vec(record).unwrap()),
            None,
        )
    };
    assert_eq!(put(&record).await.status, StatusCode::OK);
    let stored = t.get(&path).await;
    assert_eq!(stored.status, StatusCode::OK);
    assert_eq!(stored.body, serde_json::to_value(&record).unwrap());

    let mut forged = did_key_record(&key("someone else"), profile.clone(), 2);
    forged.did = record.did.clone();
    forged.signed_by = record.did.clone();
    let reply = put(&forged).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);

    let older = did_key_record(&guest, profile.clone(), 1);
    let reply = put(&older).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["version"], 1);
    assert_eq!(
        put(&did_key_record(&guest, profile, 2)).await.status,
        StatusCode::OK
    );
    assert_eq!(
        t.get("/v1/agents/did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK")
            .await
            .status,
        StatusCode::NOT_FOUND
    );
}

#[tokio::test]
async fn a_bot_writes_its_own_record_only_while_its_scope_is_in_force() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    t.link(
        &mut alice,
        &bot,
        OfferKind::Bot,
        Some(bot_scope(T0 + 100)),
        None,
    )
    .await;
    let profile = json!({ "name": "bot" });
    let path = format!("/v1/agents/did:key:{}", bot.sign_key());
    let record = did_key_record(&bot.key, profile.clone(), 1);
    let reply = t
        .send(
            "PUT",
            &path,
            Some(serde_json::to_vec(&record).unwrap()),
            None,
        )
        .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    t.set_now(T0 + 100);
    let record = did_key_record(&bot.key, profile, 2);
    let reply = t
        .send(
            "PUT",
            &path,
            Some(serde_json::to_vec(&record).unwrap()),
            None,
        )
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "writer");
}

#[tokio::test]
async fn a_did_scid_record_needs_a_signed_request_and_a_valid_non_bot_key_of_the_log() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let phone_id = t
        .link(&mut alice, &phone, OfferKind::Device, None, None)
        .await;
    let d1 = alice.device_key(1);
    let path = format!("/v1/agents/{}", alice.did());
    let profile = json!({ "perspective": { "links": [] } });
    let later = t.now() + 60;

    let record = alice.agent_record("d1", &d1, profile.clone(), 1);
    let body = serde_json::to_vec(&record).unwrap();
    let header = alice.request("d1", &d1, "PUT", &path, &body, later);
    let reply = t.send("PUT", &path, Some(body.clone()), Some(header)).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(
        t.get(&path).await.body,
        serde_json::to_value(&record).unwrap()
    );

    // No signed request: 403.
    let record2 = alice.agent_record("d1", &d1, profile.clone(), 2);
    let body2 = serde_json::to_vec(&record2).unwrap();
    assert_eq!(
        t.send("PUT", &path, Some(body2), None).await.status,
        StatusCode::FORBIDDEN
    );
    // A record signed by a key from another log: 403.
    let bob = Person::new("bob");
    t.publish(&bob).await;
    let stranger = alice.agent_record("d1", &bob.device_key(1), profile.clone(), 3);
    let body3 = serde_json::to_vec(&stranger).unwrap();
    let header = alice.request("d1", &d1, "PUT", &path, &body3, later);
    assert_eq!(
        t.send("PUT", &path, Some(body3), Some(header)).await.status,
        StatusCode::FORBIDDEN
    );
    // A record signed by a key revoked at the head: 403.
    let revoke = alice.revoke(&[&phone_id]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );
    let revoked = alice.agent_record(&phone_id, &phone.key, profile, 4);
    let body4 = serde_json::to_vec(&revoked).unwrap();
    let header = alice.request("d1", &d1, "PUT", &path, &body4, later);
    assert_eq!(
        t.send("PUT", &path, Some(body4), Some(header)).await.status,
        StatusCode::FORBIDDEN
    );
}

#[tokio::test]
async fn the_feed_answers_401_without_the_mirror_token_and_lists_appends_with_it() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    assert_eq!(t.get("/v1/feed").await.status, StatusCode::UNAUTHORIZED);
    let request = axum::http::Request::get("/v1/feed?after=0")
        .header("authorization", "Bearer wrong")
        .body(axum::body::Body::empty())
        .unwrap();
    use tower::ServiceExt;
    let response = t.router.clone().oneshot(request).await.unwrap();
    assert_eq!(response.status(), StatusCode::UNAUTHORIZED);

    let request = axum::http::Request::get("/v1/feed?after=0")
        .header("authorization", "Bearer mirror-token")
        .body(axum::body::Body::empty())
        .unwrap();
    let response = t.router.clone().oneshot(request).await.unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    use http_body_util::BodyExt;
    let body: Value =
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap();
    assert_eq!(body["items"][0]["scid"], alice.scid());
    assert_eq!(body["items"][0]["seq"], 0);
    assert_eq!(body["next"], 1);
}

#[tokio::test]
async fn the_store_survives_a_restart_on_its_file() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let db = identity_store::db::Db::open(t.dir.path().join("store.sqlite")).unwrap();
    let again = identity_store::Store::new(
        db,
        identity_store::StoreKey::from_secret([7; 32]),
        std::sync::Arc::new(|| T0),
        Settings::default(),
    );
    assert_eq!(again.head(&alice.scid()).unwrap().unwrap().seq, 0);
}
