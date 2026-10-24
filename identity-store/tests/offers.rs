//! AR-02 8.2: offers, claims, the device's state poll and rule A7, on a real store over a
//! temporary SQLite file.

mod common;

use std::time::{Duration, Instant};

use ad4m_identity_core::build;
use ad4m_identity_core::link::{match_code, OfferKind, CONFIRM_GRACE_S};
use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
use axum::http::StatusCode;
use identity_store::Fault;
use serde_json::{json, Value};

use common::agent::{enc_of, Device, Person};
use common::{TestStore, T0};

fn bot_scope() -> BotScope {
    BotScope {
        can: vec!["read".into(), "write".into()],
        expires: T0 + 86_400,
    }
}

async fn list(t: &TestStore, person: &Person, wait: Option<u64>) -> common::Reply {
    let path = format!("/v1/offers/{}", person.scid());
    let header = person.master_request("GET", &path, b"", t.now() + 60);
    let uri = match wait {
        Some(w) => format!("{path}?wait={w}"),
        None => path,
    };
    t.send("GET", &uri, None, Some(header)).await
}

async fn progress(t: &TestStore, scid: &str, offer_id: &str, key: &str) -> common::Reply {
    t.get(&format!("/v1/offers/{scid}/{offer_id}/state?key={key}"))
        .await
}

#[tokio::test]
async fn an_offer_is_claimed_listed_and_spent_once_by_its_add() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let id = offer.offer.offer_id.clone();
    let reply = t.open_offer(&offer).await;
    assert_eq!(reply.status, StatusCode::CREATED);
    assert_eq!(reply.body, json!({ "offerId": id }));
    // The same offer again: 409.
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CONFLICT);

    let listed = list(&t, &alice, None).await;
    assert_eq!(listed.status, StatusCode::OK, "{}", listed.body);
    assert_eq!(listed.body[0]["state"], json!({ "state": "open" }));
    assert_eq!(
        listed.body[0]["offer"],
        serde_json::to_value(&offer).unwrap()
    );

    let claim = phone.claim(&alice.scid(), &id, s, true);
    let reply = t.claim(&alice.scid(), &claim).await;
    assert_eq!(reply.status, StatusCode::CREATED);
    let receipt = json!({ "state": "claimed", "kind": "device", "expires": offer.offer.expires });
    assert_eq!(reply.body, receipt);
    // A byte-for-byte repeat: 200 with the same body.
    let repeat = t.claim(&alice.scid(), &claim).await;
    assert_eq!(repeat.status, StatusCode::OK);
    assert_eq!(repeat.body, receipt);

    let listed = list(&t, &alice, None).await;
    let state = &listed.body[0]["state"];
    assert_eq!(state["state"], "claimed");
    assert_eq!(state["claim"]["deviceKey"], phone.sign_key());
    assert_eq!(state["claim"]["encKey"], enc_of(&phone.key));
    assert_eq!(state["claim"]["canKeepMaster"], true);
    assert!(state["claim"].get("s").is_none());
    // Both sides compute the same match code.
    let view: ad4m_identity_core::link::ClaimView =
        serde_json::from_value(state["claim"].clone()).unwrap();
    assert_eq!(
        match_code(&alice.scid(), &id, &view.device_key),
        match_code(&alice.scid(), &id, &phone.sign_key())
    );

    let progress_reply = progress(&t, &alice.scid(), &id, &phone.sign_key()).await;
    assert_eq!(progress_reply.body, json!({ "state": "claimed" }));

    let add = alice.add_claimed(&offer.offer, &view, None);
    let again = add.clone();
    let reply = t.append_push(&mut alice, add).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let progress_reply = progress(&t, &alice.scid(), &id, &phone.sign_key()).await;
    assert_eq!(progress_reply.body, json!({ "state": "spent", "seq": 1 }));
    assert_eq!(t.count("claims"), 0);
    assert_eq!(list(&t, &alice, None).await.body, json!([]));

    // A second add that names the same offer: 403 claim (a fresh add at the new head).
    let mut second = build::add_unsigned(
        &alice.chain,
        DeviceKey {
            id: alice.chain.state().next_device_id(),
            sign: Device::new("late").sign_key(),
            enc: enc_of(&Device::new("late").key),
            life: KeyLife::Standing,
        },
        &id,
        &view.co_sig,
    )
    .unwrap();
    build::cosign(&mut second, "m0", &alice.current_master()).unwrap();
    let reply = t.append(&alice.scid(), &[&second]).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "claim");
    // The spent add itself again: 409 with the head (it no longer continues it).
    assert_eq!(
        t.append(&alice.scid(), &[&again]).await.status,
        StatusCode::CONFLICT
    );
    // A claim on the spent offer: 410 spent.
    let reply = t.claim(&alice.scid(), &claim).await;
    assert_eq!(reply.status, StatusCode::GONE);
    assert_eq!(reply.body["state"], "spent");
}

#[tokio::test]
async fn an_add_without_a_confirmed_claim_is_refused() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);
    let claim = phone.claim(&alice.scid(), &offer.offer.offer_id, s, false);
    let view = phone.view(&claim, t.now());

    // The offer is open: no claim yet.
    let add = alice.add_claimed(&offer.offer, &view, None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "claim");
    // An add naming no offer of this identity.
    let (unopened, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let add = alice.add_claimed(&unopened.offer, &view, None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "claim");
    assert_eq!(t.count("chain_entries"), 1);
}

#[tokio::test]
async fn rule_a7_fails_on_each_condition_alone() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;

    // 4: the added key is not the claimed one (another key, same co-signature).
    let phone = Device::new("alice phone");
    let (offer, view) = t
        .claimed_offer(&mut alice, &phone, OfferKind::Device, None)
        .await;
    let other = Device::new("swapped");
    let mut swapped = view.clone();
    swapped.device_key = other.sign_key();
    let add = alice.add_claimed(&offer.offer, &swapped, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );
    // 4: another enc key.
    let mut swapped = view.clone();
    swapped.enc_key = enc_of(&other.key);
    let add = alice.add_claimed(&offer.offer, &swapped, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );
    // 4: another co-signature.
    let mut swapped = view.clone();
    swapped.co_sig = other
        .claim(&alice.scid(), &offer.offer.offer_id, [1; 32], false)
        .co_sig;
    let add = alice.add_claimed(&offer.offer, &swapped, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );

    // 5: a device offer added as a bot key.
    let mut bot_offer = offer.offer.clone();
    bot_offer.kind = OfferKind::Bot;
    bot_offer.bot = Some(bot_scope());
    let add = alice.add_claimed(&bot_offer, &view, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );

    // 6 (with A6): a failure after the event insert leaves the offer claimed and the log as it was.
    t.store.inject_fault(Fault::AfterEvents);
    let add = alice.add_claimed(&offer.offer, &view, None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::INTERNAL_SERVER_ERROR);
    t.store.inject_fault(Fault::None);
    let state = progress(&t, &alice.scid(), &offer.offer.offer_id, &phone.sign_key()).await;
    assert_eq!(state.body["state"], "claimed");
    assert_eq!(t.count("chain_entries"), 1);
    assert_eq!(t.count("claims"), 1);
    // The add that fits passes.
    assert_eq!(t.append_push(&mut alice, add).await.status, StatusCode::OK);

    // 5: a bot offer added without the bot mark, or with another scope.
    let bot = Device::new("alice bot");
    let (offer, view) = t
        .claimed_offer(&mut alice, &bot, OfferKind::Bot, Some(bot_scope()))
        .await;
    let mut device_offer = offer.offer.clone();
    device_offer.kind = OfferKind::Device;
    device_offer.bot = None;
    let add = alice.add_claimed(&device_offer, &view, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );
    let mut wider = offer.offer.clone();
    wider.bot = Some(BotScope {
        can: vec!["read".into(), "write".into(), "join".into()],
        expires: T0 + 86_400,
    });
    let add = alice.add_claimed(&wider, &view, None);
    assert_eq!(
        t.append(&alice.scid(), &[&add]).await.body["error"],
        "claim"
    );
    let add = alice.add_claimed(&offer.offer, &view, None);
    assert_eq!(t.append_push(&mut alice, add).await.status, StatusCode::OK);

    // 3: past expires + the confirm grace.
    let late = Device::new("alice late");
    let (offer, view) = t
        .claimed_offer(&mut alice, &late, OfferKind::Device, None)
        .await;
    t.set_now(offer.offer.expires + CONFIRM_GRACE_S);
    let add = alice.add_claimed(&offer.offer, &view, None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "claim");
    let state = progress(&t, &alice.scid(), &offer.offer.offer_id, &late.sign_key()).await;
    assert_eq!(state.body["state"], "expired");

    // 2 and 1: a rotate supersedes the claimed offer; its add then names no claimed offer.
    let tablet = Device::new("alice tablet");
    let (offer, view) = t
        .claimed_offer(&mut alice, &tablet, OfferKind::Device, None)
        .await;
    let rotate = alice.rotate(&[]);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    let state = progress(&t, &alice.scid(), &offer.offer.offer_id, &tablet.sign_key()).await;
    assert_eq!(state.body["state"], "superseded");
    let add = alice.add_claimed(&offer.offer, &view, None);
    let reply = t.append(&alice.scid(), &[&add]).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "claim");
}

#[tokio::test]
async fn offers_by_a_device_key_by_an_old_master_or_at_a_moved_head_are_refused_and_a_sixth_open_offer_is_429(
) {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;

    // Signed by a device key.
    let (mut offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    offer.by.key_id = "d1".into();
    let bytes = ad4m_identity_core::link::offer_signing_bytes(&offer.offer, &offer.by).unwrap();
    offer.sig =
        ad4m_hash_chain::encode_proof_value(&build::Signer::sign(&alice.device_key(1), &bytes));
    let reply = t.open_offer(&offer).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "signer");

    // More than 300 s ahead: 400.
    let (offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 301);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::BAD_REQUEST);
    // A bot offer without its scope: 400.
    let (mut offer, _) = alice.offer(OfferKind::Bot, None, Some(bot_scope()), t.now() + 300);
    offer.offer.bot = None;
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::BAD_REQUEST);

    // At a moved head: 409 with the head.
    let (stale, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None, None)
        .await;
    let reply = t.open_offer(&stale).await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["head"]["seq"], 1);

    // By the old master after a rotation.
    let (old, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let rotate = alice.rotate(&[]);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    let reply = t.open_offer(&old).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "signer");

    // Five open offers pass; the sixth is 429 identity with Retry-After.
    for _ in 0..5 {
        let (offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
        assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);
    }
    let (offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let reply = t.open_offer(&offer).await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(reply.body["scope"], "identity");
    assert!(reply.headers.contains_key("retry-after"));
    // An offer for an identity the store does not hold: 404.
    let bob = Person::new("bob");
    let (mut offer, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    offer.offer.scid = bob.scid();
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn an_open_offer_expires_and_a_claim_after_expires_is_410() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);
    t.set_now(offer.offer.expires);
    let claim = phone.claim(&alice.scid(), &offer.offer.offer_id, s, false);
    let reply = t.claim(&alice.scid(), &claim).await;
    assert_eq!(reply.status, StatusCode::GONE);
    assert_eq!(reply.body["state"], "expired");
    assert_eq!(list(&t, &alice, None).await.body, json!([]));
    // The tombstone goes after 24 hours.
    t.set_now(offer.offer.expires + 86_400);
    let reply = t.claim(&alice.scid(), &claim).await;
    assert_eq!(reply.status, StatusCode::NOT_FOUND);
    assert_eq!(t.count("offers"), 0);
}

#[tokio::test]
async fn claims_with_a_wrong_secret_bound_key_cosig_or_taken_key_are_refused_and_five_wrong_secrets_lock(
) {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let scid = alice.scid();

    // Bound offer: another key gets 403 bound.
    let (bound, s) = alice.offer(
        OfferKind::Device,
        Some(phone.sign_key()),
        None,
        t.now() + 300,
    );
    assert_eq!(t.open_offer(&bound).await.status, StatusCode::CREATED);
    let stranger = Device::new("stranger");
    let reply = t
        .claim(
            &scid,
            &stranger.claim(&scid, &bound.offer.offer_id, s, false),
        )
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "bound");
    // A bad co-signature: canKeepMaster flipped after signing.
    let mut flipped = phone.claim(&scid, &bound.offer.offer_id, s, false);
    flipped.can_keep_master = true;
    let reply = t.claim(&scid, &flipped).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "cosig");
    // The bound key passes.
    let reply = t
        .claim(&scid, &phone.claim(&scid, &bound.offer.offer_id, s, false))
        .await;
    assert_eq!(reply.status, StatusCode::CREATED);

    // A key that sits in a log: 409 key-taken.
    let bob = Person::new("bob");
    t.publish(&bob).await;
    let bob_d1 = Device {
        key: bob.device_key(1),
    };
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);
    let reply = t
        .claim(&scid, &bob_d1.claim(&scid, &offer.offer.offer_id, s, false))
        .await;
    assert_eq!(reply.status, StatusCode::CONFLICT);
    assert_eq!(reply.body["error"], "key-taken");

    // A second valid claim with another key on an unbound offer: contested.
    let first = Device::new("first");
    let second = Device::new("second");
    let reply = t
        .claim(&scid, &first.claim(&scid, &offer.offer.offer_id, s, false))
        .await;
    assert_eq!(reply.status, StatusCode::CREATED);
    let reply = t
        .claim(&scid, &second.claim(&scid, &offer.offer.offer_id, s, false))
        .await;
    assert_eq!(reply.status, StatusCode::GONE);
    assert_eq!(reply.body["state"], "contested");
    let state = progress(&t, &scid, &offer.offer.offer_id, &first.sign_key()).await;
    assert_eq!(state.body["state"], "contested");

    // Five wrong secrets lock an offer; a right one then gets 410 locked.
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);
    let tablet = Device::new("tablet");
    for i in 0..5u8 {
        let reply = t
            .claim(
                &scid,
                &tablet.claim(&scid, &offer.offer.offer_id, [i; 32], false),
            )
            .await;
        assert_eq!(reply.status, StatusCode::FORBIDDEN);
        assert_eq!(reply.body["error"], "secret");
    }
    let reply = t
        .claim(&scid, &tablet.claim(&scid, &offer.offer.offer_id, s, false))
        .await;
    assert_eq!(reply.status, StatusCode::GONE);
    assert_eq!(reply.body["state"], "locked");
    // An unknown offer id: 404.
    let reply = t
        .claim(
            &scid,
            &tablet.claim(&scid, &bs58::encode([9u8; 16]).into_string(), s, false),
        )
        .await;
    assert_eq!(reply.status, StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn the_offer_state_answers_only_the_claiming_key_and_cancel_moves_it_to_cancelled() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, _) = t
        .claimed_offer(&mut alice, &phone, OfferKind::Device, None)
        .await;
    let id = &offer.offer.offer_id;
    let other = Device::new("someone");
    assert_eq!(
        progress(&t, &alice.scid(), id, &other.sign_key())
            .await
            .status,
        StatusCode::NOT_FOUND
    );

    let path = format!("/v1/offers/{}/{id}", alice.scid());
    // Cancel needs the current master key.
    let by_device = alice.request(
        "d1",
        &alice.device_key(1),
        "DELETE",
        &path,
        b"",
        t.now() + 60,
    );
    let reply = t.send("DELETE", &path, None, Some(by_device)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    let header = alice.master_request("DELETE", &path, b"", t.now() + 60);
    assert_eq!(
        t.send("DELETE", &path, None, Some(header)).await.status,
        StatusCode::NO_CONTENT
    );
    let state = progress(&t, &alice.scid(), id, &phone.sign_key()).await;
    assert_eq!(state.body, json!({ "state": "cancelled" }));
    let header = alice.master_request("DELETE", &path, b"", t.now() + 60);
    assert_eq!(
        t.send("DELETE", &path, None, Some(header)).await.status,
        StatusCode::NOT_FOUND
    );
    // Listing needs the master key too.
    let list_path = format!("/v1/offers/{}", alice.scid());
    let by_device = alice.request(
        "d1",
        &alice.device_key(1),
        "GET",
        &list_path,
        b"",
        t.now() + 60,
    );
    assert_eq!(
        t.send("GET", &list_path, None, Some(by_device))
            .await
            .status,
        StatusCode::FORBIDDEN
    );
}

#[tokio::test]
async fn a_listing_with_wait_answers_when_a_claim_arrives() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&offer).await.status, StatusCode::CREATED);

    let path = format!("/v1/offers/{}", alice.scid());
    let header = alice.master_request("GET", &path, b"", t.now() + 60);
    let router = t.router.clone();
    let waiting = tokio::spawn(async move {
        use http_body_util::BodyExt;
        use tower::ServiceExt;
        let request = axum::http::Request::get(format!("{path}?wait=8"))
            .header(identity_store::request::REQUEST_HEADER, header)
            .body(axum::body::Body::empty())
            .unwrap();
        let response = router.oneshot(request).await.unwrap();
        let body = response.into_body().collect().await.unwrap().to_bytes();
        (
            Instant::now(),
            serde_json::from_slice::<Value>(&body).unwrap(),
        )
    });
    tokio::time::sleep(Duration::from_millis(300)).await;
    let phone = Device::new("alice phone");
    let claimed_at = Instant::now();
    let reply = t
        .claim(
            &alice.scid(),
            &phone.claim(&alice.scid(), &offer.offer.offer_id, s, false),
        )
        .await;
    assert_eq!(reply.status, StatusCode::CREATED);
    let (answered, body) = waiting.await.unwrap();
    assert!(answered.duration_since(claimed_at) < Duration::from_secs(1));
    assert_eq!(body[0]["state"]["state"], "claimed");

    // The device's own wait sees the spend.
    let key = phone.sign_key();
    let id = offer.offer.offer_id.clone();
    let scid = alice.scid();
    let router = t.router.clone();
    let waiting = tokio::spawn(async move {
        use http_body_util::BodyExt;
        use tower::ServiceExt;
        let request =
            axum::http::Request::get(format!("/v1/offers/{scid}/{id}/state?key={key}&wait=8"))
                .body(axum::body::Body::empty())
                .unwrap();
        let response = router.oneshot(request).await.unwrap();
        let body = response.into_body().collect().await.unwrap().to_bytes();
        serde_json::from_slice::<Value>(&body).unwrap()
    });
    tokio::time::sleep(Duration::from_millis(300)).await;
    let claim = phone.claim(&alice.scid(), &offer.offer.offer_id, s, false);
    let add = alice.add_claimed(&offer.offer, &phone.view(&claim, t.now()), None);
    assert_eq!(t.append_push(&mut alice, add).await.status, StatusCode::OK);
    assert_eq!(
        waiting.await.unwrap(),
        json!({ "state": "spent", "seq": 1 })
    );
}

#[tokio::test]
async fn no_row_and_no_response_holds_the_offer_secret() {
    let t = TestStore::new();
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let (offer, s) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    let secret = bs58::encode(s).into_string();
    let mut bodies = vec![t.open_offer(&offer).await.raw];
    let claim = phone.claim(&alice.scid(), &offer.offer.offer_id, s, true);
    bodies.push(t.claim(&alice.scid(), &claim).await.raw);
    bodies.push(t.claim(&alice.scid(), &claim).await.raw);
    bodies.push(list(&t, &alice, None).await.raw);
    bodies.push(
        progress(&t, &alice.scid(), &offer.offer.offer_id, &phone.sign_key())
            .await
            .raw,
    );
    let add = alice.add_claimed(&offer.offer, &phone.view(&claim, t.now()), None);
    bodies.push(t.append_push(&mut alice, add).await.raw);
    bodies.push(t.get(&format!("/v1/logs/{}", alice.scid())).await.raw);
    for body in bodies {
        assert!(!String::from_utf8_lossy(&body).contains(&secret));
    }
    assert!(t.all_cells().iter().all(|cell| !cell.contains(&secret)));
}
