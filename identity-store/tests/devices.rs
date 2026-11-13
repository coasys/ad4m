//! The device inbox, the private details record, the request box, the identity bucket and the
//! sweeper, on a real store over a temporary SQLite file.

mod common;

use ad4m_hash_chain::{canonical_json, hash_bytes};
use ad4m_identity_core::inbox::{check_payload, pre_address, Accept, InboxAddress, JOINED_TTL_S};
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::record::{
    self, open_record, verify_receipt, RecordReceipt, RecoveryAttestation, SealedRecord, Writer,
};
use ad4m_identity_core::requests::{
    verify_posted, BoxedRequest, MAX_OPEN_REQUESTS, MAX_OPEN_REQUESTS_PER_KEY, REQUEST_TTL_S,
};
use ad4m_identity_core::{x25519_multikey_of, BotScope, X25519Secret};
use axum::http::StatusCode;
use ed25519_dalek::SigningKey;
use identity_store::Fault;
use serde_json::{json, Value};

use common::agent::{person_enc_secret, Device, Person, RECORD_DEVICE_NAME};
use common::{Reply, TestStore, T0};

// ----- helpers -----

fn body(value: &Value) -> Vec<u8> {
    canonical_json(value).unwrap().into_bytes()
}

/// `PUT /v1/inbox/{scid}/{keyId}` with `sealed`, signed by `signer_id`/`signer`.
async fn put_joined(
    t: &TestStore,
    person: &Person,
    key_id: &str,
    sealed: &str,
    signer: (&str, &ed25519_dalek::SigningKey),
) -> Reply {
    let path = format!("/v1/inbox/{}/{key_id}", person.scid());
    let bytes = body(&json!({ "sealed": sealed }));
    let header = person.request(signer.0, signer.1, "PUT", &path, &bytes, t.now() + 60);
    t.send("PUT", &path, Some(bytes), Some(header)).await
}

async fn master_put_joined(t: &TestStore, person: &Person, key_id: &str, sealed: &str) -> Reply {
    let master = person.current_master();
    put_joined(t, person, key_id, sealed, (&person.master_id(), &master)).await
}

/// A signed `method` on `path` (no body) by `signer_id`/`signer`.
async fn signed(
    t: &TestStore,
    person: &Person,
    method: &str,
    path: &str,
    signer: (&str, &ed25519_dalek::SigningKey),
) -> Reply {
    let header = person.request(signer.0, signer.1, method, path, b"", t.now() + 60);
    t.send(method, path, None, Some(header)).await
}

/// A signed write of `value` (sent as its JCS) by `signer_id`/`signer`.
async fn signed_body(
    t: &TestStore,
    person: &Person,
    method: &str,
    path: &str,
    value: &Value,
    signer: (&str, &ed25519_dalek::SigningKey),
) -> Reply {
    let bytes = body(value);
    let header = person.request(signer.0, signer.1, method, path, &bytes, t.now() + 60);
    t.send(method, path, Some(bytes), Some(header)).await
}

fn bot_scope() -> BotScope {
    BotScope {
        can: vec!["read".into()],
        expires: T0 + 86_400,
    }
}

/// A store with room in the identity bucket, alice published with `d1`, and `d2` linked.
async fn alice_with_phone(t: &TestStore) -> (Person, Device) {
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let id = t.link(&mut alice, &phone, OfferKind::Device, None).await;
    assert_eq!(id, "d2");
    (alice, phone)
}

// ----- the joined inbox -----

#[tokio::test]
async fn the_master_writes_a_joined_inbox_and_only_its_device_reads_and_acknowledges_it() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let d1 = ("d1", &alice.device_key(1));
    let path = format!("/v1/inbox/{}/d1", alice.scid());

    let sealed = alice.seal_master("d1");
    let reply = master_put_joined(&t, &alice, "d1", &sealed).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    assert_eq!(reply.body["expires"], T0 + JOINED_TTL_S);
    let id = reply.body["id"].as_str().unwrap().to_string();
    // The store keeps only ciphertext: the master secret sits in no table.
    let secret = bs58::encode(alice.current_master().to_bytes()).into_string();
    assert!(t.all_cells().iter().all(|c| !c.contains(&secret)));

    // Listing does not delete.
    for _ in 0..2 {
        let reply = signed(&t, &alice, "GET", &path, d1).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        let list = reply.body.as_array().unwrap();
        assert_eq!(list.len(), 1);
        assert_eq!(list[0]["id"], id.as_str());
        assert_eq!(list[0]["sealed"], sealed.as_str());
        assert_eq!(
            list[0]["writer"],
            json!({"kind": "master", "scid": alice.scid(), "keyId": "m0"})
        );
        assert_eq!(list[0]["storedAt"], T0);
    }
    // The device opens it and keeps the master key.
    let payload = ad4m_identity_core::inbox::open(
        &person_enc_secret(&alice, 1),
        &alice.joined("d1"),
        &sealed,
    )
    .unwrap();
    assert_eq!(
        check_payload(
            &payload,
            &alice.chain.state().keys["d1"].sign,
            &alice.chain,
            T0
        ),
        Ok(Accept::KeepMaster { index: 0 })
    );

    // No other key reads or acknowledges it, the master included.
    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None).await;
    let master = alice.current_master();
    for signer in [("d2", &phone.key), ("m0", &master)] {
        assert_eq!(
            signed(&t, &alice, "GET", &path, signer).await.status,
            StatusCode::FORBIDDEN
        );
        assert_eq!(
            signed(&t, &alice, "DELETE", &format!("{path}/{id}"), signer)
                .await
                .status,
            StatusCode::FORBIDDEN
        );
    }
    assert_eq!(t.get(&path).await.status, StatusCode::FORBIDDEN);

    // The acknowledgement deletes it; a second one is 404.
    let ack = format!("{path}/{id}");
    assert_eq!(
        signed(&t, &alice, "DELETE", &ack, d1).await.status,
        StatusCode::NO_CONTENT
    );
    let reply = signed(&t, &alice, "GET", &path, d1).await;
    assert_eq!(reply.body, json!([]));
    assert_eq!(
        signed(&t, &alice, "DELETE", &ack, d1).await.status,
        StatusCode::NOT_FOUND
    );
}

#[tokio::test]
async fn a_joined_write_needs_the_current_master_and_a_valid_device_key_without_the_bot_mark() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    let sealed = alice.seal_master("d1");

    // A device key writes: 403 writer.
    let reply = put_joined(&t, &alice, "d1", &sealed, ("d2", &phone.key)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(reply.body["error"], "writer");

    // To a bot key, an unknown key or a revoked key: 404.
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(&mut alice, &bot, OfferKind::Bot, Some(bot_scope()))
        .await;
    for key in [bot_id.as_str(), "d9", "m0"] {
        assert_eq!(
            master_put_joined(&t, &alice, key, &sealed).await.status,
            StatusCode::NOT_FOUND,
            "{key}"
        );
    }
    let revoke = alice.revoke(&["d2"]);
    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );
    assert_eq!(
        master_put_joined(&t, &alice, "d2", &sealed).await.status,
        StatusCode::NOT_FOUND
    );

    // The old master after a rotation: refused.
    let old = alice.current_master();
    let old_id = alice.master_id();
    let rotate = alice.rotate(&[]);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    let reply = put_joined(&t, &alice, "d1", &sealed, (&old_id, &old)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);

    // Over 8 KiB: 413; not base64url: 400; with a bind: 400.
    let big = "A".repeat(8 * 1024 + 1);
    assert_eq!(
        master_put_joined(&t, &alice, "d1", &big).await.status,
        StatusCode::PAYLOAD_TOO_LARGE
    );
    assert_eq!(
        master_put_joined(&t, &alice, "d1", "not base64!")
            .await
            .status,
        StatusCode::BAD_REQUEST
    );
    let path = format!("/v1/inbox/{}/d1", alice.scid());
    let with_bind = json!({"sealed": "AAAA", "bind": {"scid": alice.scid(), "offerId": "x", "sign": "y", "r": "z"}});
    let master = alice.current_master();
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &with_bind,
        (&alice.master_id(), &master),
    )
    .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);

    // Sixteen messages fit; the seventeenth is 429.
    for _ in 0..16 {
        let reply = master_put_joined(&t, &alice, "d1", "AAAA").await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    }
    let reply = master_put_joined(&t, &alice, "d1", "AAAA").await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(reply.body["scope"], "identity");
    assert!(reply.headers.contains_key("retry-after"));
}

#[tokio::test]
async fn a_joined_message_goes_after_30_days() {
    let t = TestStore::with_identity_burst(100);
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let d1 = ("d1", &alice.device_key(1));
    let path = format!("/v1/inbox/{}/d1", alice.scid());
    master_put_joined(&t, &alice, "d1", &alice.seal_master("d1")).await;

    t.set_now(T0 + JOINED_TTL_S - 1);
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1)
            .await
            .body
            .as_array()
            .unwrap()
            .len(),
        1
    );
    t.set_now(T0 + JOINED_TTL_S);
    // Reads never answer an expired row; the sweeper deletes it.
    assert_eq!(signed(&t, &alice, "GET", &path, d1).await.body, json!([]));
    assert_eq!(t.count("inbox"), 1);
    t.store.sweep().unwrap();
    assert_eq!(t.count("inbox"), 0);
}

#[tokio::test]
async fn a_waiting_inbox_read_answers_within_one_second_of_a_write_to_its_address() {
    use std::time::{Duration, Instant};

    let t = TestStore::with_identity_burst(100);
    let (alice, _phone) = alice_with_phone(&t).await;
    let path = format!("/v1/inbox/{}/d1", alice.scid());
    let header = alice.request("d1", &alice.device_key(1), "GET", &path, b"", t.now() + 60);

    let router = t.router.clone();
    let uri = format!("{path}?wait=8");
    let waiting = tokio::spawn(async move {
        use http_body_util::BodyExt;
        use tower::ServiceExt;
        let request = axum::http::Request::get(uri)
            .header("x-coasys-request", header)
            .body(axum::body::Body::empty())
            .unwrap();
        let response = router.oneshot(request).await.unwrap();
        let status = response.status();
        let body = response.into_body().collect().await.unwrap().to_bytes();
        (status, Instant::now(), body)
    });
    tokio::time::sleep(Duration::from_millis(300)).await;
    // A write to another address does not answer the waiting read.
    let other = master_put_joined(&t, &alice, "d2", &alice.seal_master("d2")).await;
    assert_eq!(other.status, StatusCode::CREATED, "{}", other.body);
    tokio::time::sleep(Duration::from_millis(300)).await;
    assert!(!waiting.is_finished());

    let reply = master_put_joined(&t, &alice, "d1", &alice.seal_master("d1")).await;
    let written = Instant::now();
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    let (status, answered, body) = waiting.await.unwrap();
    assert_eq!(status, StatusCode::OK);
    assert!(answered.duration_since(written) < Duration::from_secs(1));
    let list: Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(list.as_array().unwrap().len(), 1);
    assert_eq!(list[0]["id"], reply.body["id"]);
}

#[tokio::test]
async fn a_revoke_deletes_the_revoked_keys_inbox_in_its_transaction() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    master_put_joined(&t, &alice, "d2", "AAAA").await;
    master_put_joined(&t, &alice, "d1", "BBBB").await;
    let post = json!({"body": {"kind": "send-master"}, "expires": T0 + 60});
    let path = format!("/v1/requests/{}", alice.scid());
    let reply = signed_body(&t, &alice, "POST", &path, &post, ("d2", &phone.key)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);

    // A revoke that fails after its event insert leaves the message and the request.
    let revoke = alice.revoke(&["d2"]);
    t.store.inject_fault(Fault::AfterEvents);
    assert_eq!(
        t.append(&alice.scid(), &[&revoke]).await.status,
        StatusCode::INTERNAL_SERVER_ERROR
    );
    t.store.inject_fault(Fault::None);
    assert_eq!(t.count("inbox"), 2);
    assert_eq!(t.count("requests"), 1);

    assert_eq!(
        t.append_push(&mut alice, revoke).await.status,
        StatusCode::OK
    );
    let conn = t.store.db.lock().unwrap();
    let left: Vec<String> = conn
        .prepare("SELECT key_id FROM inbox")
        .unwrap()
        .query_map([], |r| r.get(0))
        .unwrap()
        .collect::<Result<_, _>>()
        .unwrap();
    assert_eq!(left, ["d1"]);
    drop(conn);
    assert_eq!(t.count("requests"), 0);
}

#[tokio::test]
async fn a_rotate_drops_the_messages_the_replaced_master_wrote() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    master_put_joined(&t, &alice, "d1", &alice.seal_master("d1")).await;
    let rotate = alice.rotate(&[]);
    assert_eq!(
        t.append_push(&mut alice, rotate).await.status,
        StatusCode::OK
    );
    assert_eq!(t.count("inbox"), 0);
    // Master 1 writes again.
    let reply = master_put_joined(&t, &alice, "d1", &alice.seal_master("d1")).await;
    assert_eq!(reply.status, StatusCode::CREATED);
    let path = format!("/v1/inbox/{}/d1", alice.scid());
    let reply = signed(&t, &alice, "GET", &path, ("d1", &alice.device_key(1))).await;
    assert_eq!(reply.body[0]["writer"]["keyId"], "m1");
}

// ----- the pre-join inbox -----

/// Opens an offer bound to `device` and answers it with its secret.
async fn bound_offer(
    t: &TestStore,
    person: &mut Person,
    device: &Device,
) -> (ad4m_identity_core::link::SignedOffer, [u8; 32]) {
    let (offer, s) = person.offer(
        OfferKind::Device,
        Some(device.sign_key()),
        None,
        t.now() + 300,
    );
    let reply = t.open_offer(&offer).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    (offer, s)
}

fn bind(person: &Person, offer_id: &str, device: &Device) -> Value {
    json!({"scid": person.scid(), "offerId": offer_id, "sign": device.sign_key(), "r": device.r()})
}

async fn put_pre(
    t: &TestStore,
    person: &Person,
    address: &str,
    write: &Value,
    signer: (&str, &ed25519_dalek::SigningKey),
) -> Reply {
    signed_body(
        t,
        person,
        "PUT",
        &format!("/v1/inbox/pre/{address}"),
        write,
        signer,
    )
    .await
}

/// A pre-join read of `method path` by `reader`, with a fresh header.
async fn pre_read(t: &TestStore, reader: &Device, method: &str, path: &str) -> Reply {
    let header = reader.pre_auth(method, path, t.now() + 60);
    t.send_with(method, path, None, None, &[("x-coasys-inbox", &header)])
        .await
}

#[tokio::test]
async fn the_holder_that_scanned_sends_a_ticket_the_device_reads_before_it_joins() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let desk = Device::new("alice desk");
    let (offer, s) = bound_offer(&t, &mut alice, &desk).await;
    let address = desk.pre_address();
    let sealed = alice.seal_ticket(&desk, &offer, s);
    let write = json!({"sealed": sealed, "bind": bind(&alice, &offer.offer.offer_id, &desk)});
    let master = alice.current_master();
    let reply = put_pre(&t, &alice, &address, &write, (&alice.master_id(), &master)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    // A pre-join message lives no longer than its offer.
    assert_eq!(reply.body["expires"], offer.offer.expires);
    let id = reply.body["id"].as_str().unwrap().to_string();

    let path = format!("/v1/inbox/pre/{address}");
    let reply = pre_read(&t, &desk, "GET", &path).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let list = reply.body.as_array().unwrap();
    assert_eq!(list.len(), 1);
    assert_eq!(
        list[0]["writer"],
        json!({"kind": "offer", "scid": alice.scid(), "offerId": offer.offer.offer_id})
    );
    let payload = desk.open(
        &InboxAddress::Pre(address.clone()),
        list[0]["sealed"].as_str().unwrap(),
    );
    assert!(matches!(
        check_payload(&payload, &desk.sign_key(), &alice.chain, t.now()),
        Ok(Accept::ClaimOffer(_))
    ));

    // Another device's read of this address is refused, and so is a replayed header.
    let stranger = Device::new("stranger");
    let header = stranger.pre_auth("GET", &path, t.now() + 60);
    let reply = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    let header = desk.pre_auth("GET", &path, t.now() + 60);
    let first = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(first.status, StatusCode::OK);
    let again = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(again.status, StatusCode::FORBIDDEN);
    // A read header used for an acknowledgement, an expired one, or none: refused.
    let ack = format!("{path}/{id}");
    let header = desk.pre_auth("GET", &path, t.now() + 60);
    let reply = t
        .send_with("DELETE", &ack, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    let header = desk.pre_auth("GET", &path, t.now());
    let reply = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(t.get(&path).await.status, StatusCode::FORBIDDEN);

    // The device acknowledges; the message goes.
    let reply = pre_read(&t, &desk, "DELETE", &ack).await;
    assert_eq!(reply.status, StatusCode::NO_CONTENT);
    assert_eq!(pre_read(&t, &desk, "GET", &path).await.body, json!([]));
}

#[tokio::test]
async fn a_pre_join_read_signed_for_another_store_is_refused() {
    let t = TestStore::with_identity_burst(100);
    let other = TestStore::with_key([8; 32]);
    assert_ne!(t.store_key(), other.store_key());
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let desk = Device::new("alice desk");
    let (offer, s) = bound_offer(&t, &mut alice, &desk).await;
    let address = desk.pre_address();
    let write = json!({
        "sealed": alice.seal_ticket(&desk, &offer, s),
        "bind": bind(&alice, &offer.offer.offer_id, &desk),
    });
    let master = alice.current_master();
    let reply = put_pre(&t, &alice, &address, &write, (&alice.master_id(), &master)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    let path = format!("/v1/inbox/pre/{address}");

    // A read the device signed for the other store: 403 store, as a signed request.
    let elsewhere = format!("did:key:{}", other.store_key());
    let header = desk.pre_auth_at(&elsewhere, "GET", &path, t.now() + 60);
    let reply = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "store");
    // An acknowledgement signed for the other store: refused the same way, and the message stays.
    let ack = format!("{path}/{}", reply_id(&t, &desk, &path).await);
    let header = desk.pre_auth_at(&elsewhere, "DELETE", &ack, t.now() + 60);
    let reply = t
        .send_with("DELETE", &ack, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "store");
    // A read signed for this store, replayed to the other: refused there too.
    let header = desk.pre_auth("GET", &path, t.now() + 60);
    let reply = other
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    assert_eq!(reply.body["error"], "store");
    // This store takes the same header.
    let reply = t
        .send_with("GET", &path, None, None, &[("x-coasys-inbox", &header)])
        .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(reply.body.as_array().unwrap().len(), 1);
}

/// The id of the one message at the pre-join inbox `path`, read by `reader` at `t`.
async fn reply_id(t: &TestStore, reader: &Device, path: &str) -> String {
    let reply = pre_read(t, reader, "GET", path).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    reply.body[0]["id"].as_str().unwrap().to_string()
}

#[tokio::test]
async fn a_pre_join_write_needs_the_open_bound_offer_its_r_and_the_master_that_opened_it() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    let desk = Device::new("alice desk");
    let (offer, s) = bound_offer(&t, &mut alice, &desk).await;
    let offer_id = offer.offer.offer_id.clone();
    let sealed = alice.seal_ticket(&desk, &offer, s);
    let address = desk.pre_address();
    let master = alice.current_master();
    let m = (alice.master_id(), master);
    let as_master = (m.0.as_str(), &m.1);
    let refused = |reply: Reply| {
        assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
        assert_eq!(reply.body["error"], "writer");
    };

    // No such offer.
    let mut b = bind(&alice, &offer_id, &desk);
    b["offerId"] = json!(bs58::encode([3u8; 16]).into_string());
    refused(
        put_pre(
            &t,
            &alice,
            &address,
            &json!({"sealed": sealed, "bind": b}),
            as_master,
        )
        .await,
    );
    // The wrong r for the address.
    let mut b = bind(&alice, &offer_id, &desk);
    b["r"] = json!(bs58::encode([4u8; 16]).into_string());
    refused(
        put_pre(
            &t,
            &alice,
            &address,
            &json!({"sealed": sealed, "bind": b}),
            as_master,
        )
        .await,
    );
    // Signed by a device key.
    let write = json!({"sealed": sealed, "bind": bind(&alice, &offer_id, &desk)});
    refused(put_pre(&t, &alice, &address, &write, ("d2", &phone.key)).await);
    // An unbound offer.
    let (unbound, _) = alice.offer(OfferKind::Device, None, None, t.now() + 300);
    assert_eq!(t.open_offer(&unbound).await.status, StatusCode::CREATED);
    let write_unbound =
        json!({"sealed": sealed, "bind": bind(&alice, &unbound.offer.offer_id, &desk)});
    refused(put_pre(&t, &alice, &address, &write_unbound, as_master).await);
    // No bind at all: 400.
    assert_eq!(
        put_pre(&t, &alice, &address, &json!({"sealed": sealed}), as_master)
            .await
            .status,
        StatusCode::BAD_REQUEST
    );

    // Two messages fit; the third is 429.
    for _ in 0..2 {
        assert_eq!(
            put_pre(&t, &alice, &address, &write, as_master)
                .await
                .status,
            StatusCode::CREATED
        );
    }
    let reply = put_pre(&t, &alice, &address, &write, as_master).await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);

    // After the offer's 5 minutes nobody writes and the messages are gone.
    t.set_now(offer.offer.expires);
    refused(put_pre(&t, &alice, &address, &write, as_master).await);
    let path = format!("/v1/inbox/pre/{address}");
    assert_eq!(pre_read(&t, &desk, "GET", &path).await.body, json!([]));
}

#[tokio::test]
async fn a_claim_ends_the_offers_pre_join_messages() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let desk = Device::new("alice desk");
    let (offer, s) = bound_offer(&t, &mut alice, &desk).await;
    let address = desk.pre_address();
    let write = json!({"sealed": alice.seal_ticket(&desk, &offer, s),
                       "bind": bind(&alice, &offer.offer.offer_id, &desk)});
    let master = alice.current_master();
    put_pre(&t, &alice, &address, &write, (&alice.master_id(), &master)).await;
    assert_eq!(t.count("inbox"), 1);
    let claim = desk.claim(&alice.scid(), &offer.offer.offer_id, s, true);
    assert_eq!(
        t.claim(&alice.scid(), &claim).await.status,
        StatusCode::CREATED
    );
    assert_eq!(t.count("inbox"), 0);
}

/// Room at a pre-join address is counted per writing identity: another identity that learned
/// the device string and opened its own offer bound to the device key fills its own room, and
/// the holder that scanned the string still delivers its ticket.
#[tokio::test]
async fn another_identity_filling_its_room_at_a_pre_join_address_leaves_the_holders() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let mut mallory = Person::new("mallory");
    t.publish(&mallory).await;
    let desk = Device::new("alice desk");
    let address = desk.pre_address();

    let (theirs, s) = bound_offer(&t, &mut mallory, &desk).await;
    let write = json!({"sealed": mallory.seal_ticket(&desk, &theirs, s),
                       "bind": bind(&mallory, &theirs.offer.offer_id, &desk)});
    let m = (mallory.master_id(), mallory.current_master());
    for _ in 0..2 {
        let reply = put_pre(&t, &mallory, &address, &write, (&m.0, &m.1)).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    }
    let reply = put_pre(&t, &mallory, &address, &write, (&m.0, &m.1)).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );

    let (ours, s) = bound_offer(&t, &mut alice, &desk).await;
    let write = json!({"sealed": alice.seal_ticket(&desk, &ours, s),
                       "bind": bind(&alice, &ours.offer.offer_id, &desk)});
    let a = (alice.master_id(), alice.current_master());
    let reply = put_pre(&t, &alice, &address, &write, (&a.0, &a.1)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);

    // The device reads both identities' messages and tells them apart by their writer.
    let path = format!("/v1/inbox/pre/{address}");
    let reply = pre_read(&t, &desk, "GET", &path).await;
    let list = reply.body.as_array().unwrap();
    assert_eq!(list.len(), 3);
    let by = |scid: String| list.iter().filter(|m| m["writer"]["scid"] == scid).count();
    assert_eq!((by(mallory.scid()), by(alice.scid())), (2, 1));
}

/// Room for pre-join messages is also counted per offer, over every address: the address
/// depends on the device string's `r` as well as its key, so one offer could otherwise back 2
/// messages at each of many addresses. An identity's second offer has room of its own.
#[tokio::test]
async fn one_offer_holds_at_most_two_pre_join_messages_over_every_address() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let desk = Device::new("alice desk");
    let (offer, s) = bound_offer(&t, &mut alice, &desk).await;
    let sealed = alice.seal_ticket(&desk, &offer, s);
    let a = (alice.master_id(), alice.current_master());
    // The same key with three different `r`: three addresses.
    let writes: Vec<(String, Value)> = [desk.r(), r_of(1), r_of(2)]
        .into_iter()
        .map(|r| {
            let address = pre_address(&desk.sign_key(), &r).unwrap();
            let mut b = bind(&alice, &offer.offer.offer_id, &desk);
            b["r"] = json!(r);
            (address, json!({"sealed": sealed, "bind": b}))
        })
        .collect();
    assert_ne!(writes[0].0, writes[1].0);
    assert_ne!(writes[1].0, writes[2].0);

    for (address, write) in &writes[..2] {
        let reply = put_pre(&t, &alice, address, write, (&a.0, &a.1)).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    }
    // The third message, at an address the offer has not written yet: the offer's room is full.
    let (address, write) = &writes[2];
    let reply = put_pre(&t, &alice, address, write, (&a.0, &a.1)).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["scope"], "identity", "{}", reply.body);
    assert_eq!(t.count("inbox"), 2);

    // A second offer bound to the same key brings 2 more.
    let (second, s) = bound_offer(&t, &mut alice, &desk).await;
    let write = json!({"sealed": alice.seal_ticket(&desk, &second, s),
                       "bind": bind(&alice, &second.offer.offer_id, &desk)});
    let reply = put_pre(&t, &alice, &desk.pre_address(), &write, (&a.0, &a.1)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
}

/// A device string's `r` other than the test device's: 16 bytes of base58btc.
fn r_of(n: u8) -> String {
    bs58::encode([n; 16]).into_string()
}

// ----- the private details record -----

#[tokio::test]
async fn a_record_is_kept_with_rising_versions_and_read_by_the_identitys_keys() {
    let t = TestStore::with_identity_burst(100);
    let (alice, phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1).await.status,
        StatusCode::NOT_FOUND
    );

    // The master installs the recovery key (a device cannot); d1 sends the request.
    let recovery = X25519Secret::from([5u8; 32]);
    let v1 = alice.sealed_record_by(
        1,
        Some(alice.attest_recovery(x25519_multikey_of(&recovery))),
        &alice.current_master(),
    );
    let v1_json = serde_json::to_value(&v1).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &v1_json, d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(reply.body["version"], 1);
    // The store's receipt: the envelope it accepted, at the head it checked it against, by its
    // head key.
    let receipt: RecordReceipt = serde_json::from_value(reply.body["receipt"].clone()).unwrap();
    assert_eq!(
        reply.body.as_object().unwrap().keys().collect::<Vec<_>>(),
        ["receipt", "version"]
    );
    assert_eq!(receipt.seq, alice.chain.head().seq);
    assert_eq!(receipt.store, format!("did:key:{}", t.store_key()));
    let read: SealedRecord = serde_json::from_value(v1_json.clone()).unwrap();
    assert_eq!(
        verify_receipt(&read, &receipt, &alice.chain, &[t.store_key()]),
        Ok(Writer::Master(alice.master_id()))
    );

    // Every device key and the current master read it back as written, with the receipt.
    let master = alice.current_master();
    for signer in [d1, ("d2", &phone.key), (&alice.master_id(), &master)] {
        let reply = signed(&t, &alice, "GET", &path, signer).await;
        assert_eq!(reply.status, StatusCode::OK);
        assert_eq!(reply.body, json!({"record": v1_json, "receipt": receipt}));
    }
    for (entry, secret) in [
        ("d1", person_enc_secret(&alice, 1)),
        ("d2", phone.enc_secret()),
        ("recovery", recovery.clone()),
    ] {
        let record = open_record(
            &read,
            &receipt,
            &alice.chain,
            &[t.store_key()],
            entry,
            &secret,
        )
        .unwrap();
        assert_eq!(record.devices["d1"].name, RECORD_DEVICE_NAME);
    }

    // An equal, lower or skipping version: 409 with the stored one.
    for version in [1, 0, 3] {
        let stale =
            serde_json::to_value(alice.sealed_record(version, Some(x25519_multikey_of(&recovery))))
                .unwrap();
        let reply = signed_body(&t, &alice, "PUT", &path, &stale, d1).await;
        assert_eq!(reply.status, StatusCode::CONFLICT);
        assert_eq!(reply.body["version"], 1);
    }
    // Dropping the recovery key: 400.
    let dropped = serde_json::to_value(alice.sealed_record(2, None)).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &dropped, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "recovery-key");
    let v2 =
        serde_json::to_value(alice.sealed_record(2, Some(x25519_multikey_of(&recovery)))).unwrap();
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &v2, ("d2", &phone.key))
            .await
            .status,
        StatusCode::OK
    );

    // The store never holds the record's plaintext.
    assert!(t
        .all_cells()
        .iter()
        .all(|c| !c.contains(RECORD_DEVICE_NAME)));
}

#[tokio::test]
async fn the_first_record_is_version_one() {
    let t = TestStore::with_identity_burst(100);
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    // An absent record is version 0: a first write at 0, at 2 or at 2^53 − 1 (which would
    // leave no version for a later write) is refused with the stored version 0.
    for version in [0, 2, ad4m_hash_chain::MAX_SAFE_INTEGER] {
        let write = serde_json::to_value(alice.sealed_record(version, None)).unwrap();
        let reply = signed_body(&t, &alice, "PUT", &path, &write, d1).await;
        assert_eq!(
            reply.status,
            StatusCode::CONFLICT,
            "{version}: {}",
            reply.body
        );
        assert_eq!(reply.body["version"], 0, "{version}");
    }
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1).await.status,
        StatusCode::NOT_FOUND
    );
    let v1 = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &v1, d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(reply.body["version"], 1);
}

/// The record a device wrote before its revocation, and the one the master wrote before a
/// rotation, still open for their readers through the store's receipt; the revoked key cannot
/// write again, and a surviving device rewrites the record at the head.
#[tokio::test]
async fn a_stored_record_outlives_its_writers_revocation_or_rotation() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1_key = alice.device_key(1);
    let d1 = ("d1", &d1_key);
    let stores = [t.store_key()];
    let read = |reply: &Reply| -> (SealedRecord, RecordReceipt) {
        (
            serde_json::from_value(reply.body["record"].clone()).unwrap(),
            serde_json::from_value(reply.body["receipt"].clone()).unwrap(),
        )
    };

    // The master installs the recovery key and writes; then it rotates.
    let recovery = X25519Secret::from([5u8; 32]);
    let v1 = alice.sealed_record_by(
        1,
        Some(alice.attest_recovery(x25519_multikey_of(&recovery))),
        &alice.current_master(),
    );
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(&v1).unwrap(),
        d1,
    )
    .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let accepted_at = alice.chain.head().seq;
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let (sealed, receipt) = read(&signed(&t, &alice, "GET", &path, d1).await);
    assert_eq!((sealed.clone(), receipt.seq), (v1, accepted_at));
    for (entry, secret) in [
        ("d1", person_enc_secret(&alice, 1)),
        ("d2", phone.enc_secret()),
        ("recovery", recovery.clone()),
    ] {
        let record = open_record(&sealed, &receipt, &alice.chain, &stores, entry, &secret);
        assert_eq!(
            record.map(|r| r.devices["d1"].name.clone()),
            Ok(RECORD_DEVICE_NAME.to_string()),
            "{entry} after the rotation"
        );
    }
    // The recovery key needs the new master's attestation to count again.
    assert_eq!(record::recovery_of(&sealed, &alice.chain), None);

    // The new master attests it again and rewrites; then the phone writes the latest version
    // and is revoked.
    let v2 = alice.sealed_record_by(
        2,
        Some(alice.attest_recovery(x25519_multikey_of(&recovery))),
        &alice.current_master(),
    );
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(&v2).unwrap(),
        d1,
    )
    .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let kept = record::recovery_of(&v2, &alice.chain);
    assert!(kept.is_some());
    let v3 = alice.sealed_record_by(3, kept.clone(), &phone.key);
    let by_phone = ("d2", &phone.key);
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(&v3).unwrap(),
        by_phone,
    )
    .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // What the phone would write next, sealed while it still could.
    let v4_by_phone = serde_json::to_value(alice.sealed_record_by(4, kept, &phone.key)).unwrap();
    let revoke = alice.revoke(&["d2"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);

    // d1 still opens the phone's record.
    let (sealed, receipt) = read(&signed(&t, &alice, "GET", &path, d1).await);
    assert_eq!(sealed.signer, "d2");
    let record = open_record(
        &sealed,
        &receipt,
        &alice.chain,
        &stores,
        "d1",
        &person_enc_secret(&alice, 1),
    )
    .unwrap();
    assert_eq!(record.devices["d1"].name, RECORD_DEVICE_NAME);

    // The revoked phone's new write is refused, sent by itself or by d1.
    let reply = signed_body(&t, &alice, "PUT", &path, &v4_by_phone, by_phone).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);
    let reply = signed_body(&t, &alice, "PUT", &path, &v4_by_phone, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "signer");

    // d1 rewrites it at the head, wrapped to the readers left.
    let v4 = alice.sealed_record(4, Some(x25519_multikey_of(&recovery)));
    assert_eq!(v4.wrapped.keys().collect::<Vec<_>>(), ["d1", "recovery"]);
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(&v4).unwrap(),
        d1,
    )
    .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let (sealed, receipt) = read(&signed(&t, &alice, "GET", &path, d1).await);
    assert_eq!(sealed, v4);
    assert_eq!(receipt.seq, alice.chain.head().seq);
    assert_eq!(
        verify_receipt(&sealed, &receipt, &alice.chain, &stores),
        Ok(Writer::Device("d1".into()))
    );
}

#[tokio::test]
async fn a_record_no_writer_of_the_identity_signed_is_refused() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    // No signature, or a signature by a key outside the log naming d1: 400 signature, though
    // the request itself is d1's.
    let mut unsigned = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    unsigned["sig"] = json!("");
    let reply = signed_body(&t, &alice, "PUT", &path, &unsigned, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "signature");
    let stranger = ed25519_dalek::SigningKey::from_bytes(&[9u8; 32]);
    let mut forged = alice.sealed_record_by(1, None, &alice.device_key(1));
    let bytes = ad4m_identity_core::record::envelope_signing_bytes(&alice.scid(), &forged).unwrap();
    forged.sig = ad4m_hash_chain::encode_proof_value(
        &ed25519_dalek::Signer::sign(&stranger, &bytes).to_bytes(),
    );
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(&forged).unwrap(),
        d1,
    )
    .await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "signature");
    // A swapped recovery key under the writer's signature over the original.
    let mut swapped = serde_json::to_value(
        alice.sealed_record(1, Some(x25519_multikey_of(&X25519Secret::from([5u8; 32])))),
    )
    .unwrap();
    swapped["recoveryKey"] = json!(x25519_multikey_of(&X25519Secret::from([7u8; 32])));
    let reply = signed_body(&t, &alice, "PUT", &path, &swapped, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "signature");
    // The phone signs version 1; once revoked, its envelope is refused even when d1 sends it.
    let by_phone = serde_json::to_value(alice.sealed_record_by(1, None, &phone.key)).unwrap();
    let revoke = alice.revoke(&["d2"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let reply = signed_body(&t, &alice, "PUT", &path, &by_phone, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "signer");
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1).await.status,
        StatusCode::NOT_FOUND
    );
    // Signed by d1, it is kept.
    let good = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &good, d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

/// `sealed` signed again by `key` under the id `signer`, after a change to its fields.
fn resign(alice: &Person, sealed: &mut SealedRecord, signer: &str, key: &SigningKey) {
    sealed.signer = signer.into();
    let bytes = record::envelope_signing_bytes(&alice.scid(), sealed).unwrap();
    sealed.sig =
        ad4m_hash_chain::encode_proof_value(&ed25519_dalek::Signer::sign(key, &bytes).to_bytes());
}

#[tokio::test]
async fn the_recovery_key_needs_the_current_masters_attestation() {
    let t = TestStore::with_identity_burst(100);
    let (mut alice, phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1_key = alice.device_key(1);
    let d1 = ("d1", &d1_key);
    let put = |sealed: &SealedRecord| serde_json::to_value(sealed).unwrap();
    let key_a = x25519_multikey_of(&X25519Secret::from([5u8; 32]));
    let key_b = x25519_multikey_of(&X25519Secret::from([7u8; 32]));

    // A device installs a recovery key with no attestation: malformed.
    let mut bare = alice.sealed_record(1, Some(key_a.clone()));
    bare.recovery_attestation = None;
    resign(&alice, &mut bare, "d1", &d1_key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&bare), d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "malformed");
    // Attested by d1 itself, under its own id or naming the master: 400 attestation.
    for key_id in ["d1", "m0"] {
        let mut self_attested = alice.sealed_record(1, Some(key_a.clone()));
        let bytes = record::attestation_signing_bytes(&alice.scid(), &key_a, key_id).unwrap();
        self_attested.recovery_attestation = Some(RecoveryAttestation {
            key_id: key_id.into(),
            sig: ad4m_hash_chain::encode_proof_value(
                &ed25519_dalek::Signer::sign(&d1_key, &bytes).to_bytes(),
            ),
        });
        resign(&alice, &mut self_attested, "d1", &d1_key);
        let reply = signed_body(&t, &alice, "PUT", &path, &put(&self_attested), d1).await;
        assert_eq!(
            reply.status,
            StatusCode::BAD_REQUEST,
            "{key_id}: {}",
            reply.body
        );
        assert_eq!(reply.body["error"], "attestation", "{key_id}");
    }
    // The master's attestation of key A carried with key B: 400 attestation.
    let mut other = alice.sealed_record(1, Some(key_b.clone()));
    other.recovery_attestation = Some(alice.attest_recovery(key_a.clone()).attestation);
    resign(&alice, &mut other, "d1", &d1_key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&other), d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "attestation");
    // Even with the master's attestation, a device does not install the recovery key: only a
    // master-signed write does.
    let by_device = alice.sealed_record(1, Some(key_a.clone()));
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&by_device), d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "recovery-key");
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1).await.status,
        StatusCode::NOT_FOUND
    );

    // The master installs key A.
    let master = alice.current_master();
    let attested_a = alice.attest_recovery(key_a.clone());
    let v1 = alice.sealed_record_by(1, Some(attested_a.clone()), &master);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&v1), d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // The phone copies it unchanged.
    let v2 = alice.sealed_record_by(2, Some(attested_a.clone()), &phone.key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&v2), d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // The phone changes it to key B, master-attested or not, or drops it: 400 recovery-key.
    for recovery in [Some(alice.attest_recovery(key_b.clone())), None] {
        let changed = alice.sealed_record_by(3, recovery, &phone.key);
        let reply = signed_body(&t, &alice, "PUT", &path, &put(&changed), d1).await;
        assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
        assert_eq!(reply.body["error"], "recovery-key");
    }
    // The master corrects it to key B.
    let attested_b = alice.attest_recovery(key_b.clone());
    let v3 = alice.sealed_record_by(3, Some(attested_b.clone()), &master);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&v3), d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // The phone cannot put key A back with its old attestation.
    let back = alice.sealed_record_by(4, Some(attested_a), &phone.key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&back), d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "recovery-key");

    // After a rotation m0's attestation no longer counts: a write carrying it is refused, a
    // device write drops the key, and the new master attests it again.
    let rotate = alice.rotate(&[]);
    let reply = t.append_push(&mut alice, rotate).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    // (`seal_record` refuses the stale attestation, so the phone's write carries it by hand)
    let mut stale = alice.sealed_record_by(4, None, &phone.key);
    let wrap = stale.wrapped["d1"].clone();
    stale.wrapped.insert("recovery".into(), wrap);
    stale.recovery_key = Some(attested_b.key.clone());
    stale.recovery_attestation = Some(attested_b.attestation.clone());
    resign(&alice, &mut stale, "d2", &phone.key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&stale), d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "attestation");
    let dropped = alice.sealed_record_by(4, None, &phone.key);
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&dropped), d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(alice.master_id(), "m1");
    let v5 = alice.sealed_record_by(
        5,
        Some(alice.attest_recovery(key_b)),
        &alice.current_master(),
    );
    assert_eq!(v5.recovery_attestation.as_ref().unwrap().key_id, "m1");
    let reply = signed_body(&t, &alice, "PUT", &path, &put(&v5), d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn a_record_not_wrapped_to_exactly_the_logs_readers_or_by_a_bot_is_refused() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    // Sealed before d2 joined: d2 has no wrap.
    let early = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    let phone = Device::new("alice phone");
    t.link(&mut alice, &phone, OfferKind::Device, None).await;
    let reply = signed_body(&t, &alice, "PUT", &path, &early, d1).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "wrapped");
    // A field the record does not carry: 400.
    let mut extra = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    extra["preferences"] = json!({});
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &extra, d1)
            .await
            .status,
        StatusCode::BAD_REQUEST
    );
    // A bot key neither writes nor reads it.
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(&mut alice, &bot, OfferKind::Bot, Some(bot_scope()))
        .await;
    let good = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    assert_eq!(
        good["wrapped"].as_object().unwrap().len(),
        2,
        "d1, d2, no bot"
    );
    let reply = signed_body(&t, &alice, "PUT", &path, &good, (&bot_id, &bot.key)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &good, d1)
            .await
            .status,
        StatusCode::OK
    );
    assert_eq!(
        signed(&t, &alice, "GET", &path, (&bot_id, &bot.key))
            .await
            .status,
        StatusCode::FORBIDDEN
    );
    // Over the 64 KiB body limit: 413.
    let mut huge = good.clone();
    huge["ciphertext"] = json!("A".repeat(64 * 1024));
    huge["version"] = json!(2);
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &huge, d1)
            .await
            .status,
        StatusCode::PAYLOAD_TOO_LARGE
    );
}

// ----- the request box -----

#[tokio::test]
async fn a_device_posts_a_request_that_the_master_reads_and_deletes() {
    let t = TestStore::with_identity_burst(100);
    let (alice, phone) = alice_with_phone(&t).await;
    let path = format!("/v1/requests/{}", alice.scid());
    let d2 = ("d2", &phone.key);
    let master = alice.current_master();
    let as_master = (alice.master_id(), master);
    let as_master = (as_master.0.as_str(), &as_master.1);

    let send = json!({"body": {"kind": "send-master"}, "expires": T0 + 3600});
    let reply = signed_body(&t, &alice, "POST", &path, &send, d2).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    // A second send-master from the same key replaces the first.
    let reply = signed_body(&t, &alice, "POST", &path, &send, d2).await;
    let send_id = reply.body["id"].as_str().unwrap().to_string();
    let revoke = json!({"body": {"kind": "confirm-revoke", "keyIds": ["d1"], "rotate": false},
                        "expires": T0 + 3600});
    assert_eq!(
        signed_body(&t, &alice, "POST", &path, &revoke, d2)
            .await
            .status,
        StatusCode::CREATED
    );

    let reply = signed(&t, &alice, "GET", &path, as_master).await;
    assert_eq!(reply.status, StatusCode::OK);
    let list = reply.body.as_array().unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(list[0]["id"], send_id.as_str());
    assert_eq!(list[0]["body"], json!({"kind": "send-master"}));
    assert_eq!(list[0]["by"], "d2");
    assert_eq!(list[0]["created"], T0);
    // The signed request travels with it: its body hash covers the posted body.
    assert_eq!(list[0]["signed"]["req"]["keyId"], "d2");
    assert_eq!(
        list[0]["signed"]["req"]["bodyHash"],
        hash_bytes(&body(&send))
    );
    // A reader checks each poster against its own copy of the log.
    for entry in list {
        let entry: BoxedRequest = serde_json::from_value(entry.clone()).unwrap();
        assert_eq!(verify_posted(&entry, &alice.chain), Ok(()));
    }
    // A device key reads it too; `since` filters.
    let reply = signed(&t, &alice, "GET", &path, ("d1", &alice.device_key(1))).await;
    assert_eq!(reply.body.as_array().unwrap().len(), 2);
    let later = format!("{path}?since={}", T0 + 1);
    let header = alice.request("d1", &alice.device_key(1), "GET", &path, b"", T0 + 60);
    let reply = t.send("GET", &later, None, Some(header)).await;
    assert_eq!(reply.body, json!([]));

    // Another device does not delete it; the master does.
    let one = format!("{path}/{send_id}");
    let reply = signed(&t, &alice, "DELETE", &one, ("d1", &alice.device_key(1))).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    assert_eq!(
        signed(&t, &alice, "DELETE", &one, as_master).await.status,
        StatusCode::NO_CONTENT
    );
    assert_eq!(
        signed(&t, &alice, "DELETE", &one, d2).await.status,
        StatusCode::NOT_FOUND
    );
}

#[tokio::test]
async fn requests_need_a_device_key_seven_days_at_most_and_room_in_the_box() {
    let t = TestStore::with_identity_burst(100);
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/requests/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let master = alice.current_master();
    let send = json!({"body": {"kind": "send-master"}, "expires": T0 + 60});

    // The master key posts no request; a bot key signs none.
    let reply = signed_body(
        &t,
        &alice,
        "POST",
        &path,
        &send,
        (&alice.master_id(), &master),
    )
    .await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);
    let bot = Device::new("alice bot");
    let bot_id = t
        .link(&mut alice, &bot, OfferKind::Bot, Some(bot_scope()))
        .await;
    let reply = signed_body(&t, &alice, "POST", &path, &send, (&bot_id, &bot.key)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN);

    // More than 7 days ahead, in the past, an unknown kind or key: 400.
    for refused in [
        json!({"body": {"kind": "send-master"}, "expires": T0 + REQUEST_TTL_S + 1}),
        json!({"body": {"kind": "send-master"}, "expires": T0}),
        json!({"body": {"kind": "declined"}, "expires": T0 + 60}),
        json!({"body": {"kind": "confirm-revoke", "keyIds": ["d9"], "rotate": false}, "expires": T0 + 60}),
    ] {
        let reply = signed_body(&t, &alice, "POST", &path, &refused, d1).await;
        assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{refused}");
    }

    // Four open requests per posting key: d1's fifth is 429, and it does not keep the
    // identity's other device keys out of the box.
    let revoke = json!({"body": {"kind": "confirm-revoke", "keyIds": ["d1"], "rotate": true},
                        "expires": T0 + 60});
    for _ in 0..MAX_OPEN_REQUESTS_PER_KEY {
        let reply = signed_body(&t, &alice, "POST", &path, &revoke, d1).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    }
    let reply = signed_body(&t, &alice, "POST", &path, &revoke, d1).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["scope"], "identity");
    let mut others = Vec::new();
    for i in 0..4 {
        let device = Device::new(&format!("alice device {i}"));
        let id = t.link(&mut alice, &device, OfferKind::Device, None).await;
        others.push((id, device));
    }
    let (first_id, first) = &others[0];
    let reply = signed_body(&t, &alice, "POST", &path, &revoke, (first_id, &first.key)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);

    // Sixteen open requests per identity: with four keys at their cap the box is full, and a
    // fifth key's post is 429.
    for (n, (id, device)) in others[..3].iter().enumerate() {
        let left = MAX_OPEN_REQUESTS_PER_KEY - usize::from(n == 0);
        for _ in 0..left {
            let reply = signed_body(&t, &alice, "POST", &path, &revoke, (id, &device.key)).await;
            assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
        }
    }
    assert_eq!(t.count("requests"), MAX_OPEN_REQUESTS as i64);
    let (last_id, last) = &others[3];
    let reply = signed_body(&t, &alice, "POST", &path, &revoke, (last_id, &last.key)).await;
    assert_eq!(
        reply.status,
        StatusCode::TOO_MANY_REQUESTS,
        "{}",
        reply.body
    );
    assert_eq!(reply.body["scope"], "identity");
}

#[tokio::test]
async fn a_post_not_sent_as_its_jcs_is_refused() {
    let t = TestStore::with_identity_burst(100);
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/requests/{}", alice.scid());
    let key = alice.device_key(1);
    // The same post with spaces and its members out of order, and with the hash its own bytes
    // have: the store refuses it, so no reader hashes JCS({body, expires}) to another bodyHash.
    let expires = T0 + 60;
    for text in [
        format!(r#"{{"expires":{expires},"body":{{"kind":"send-master"}}}}"#),
        format!(r#"{{ "body": {{ "kind": "send-master" }}, "expires": {expires} }}"#),
    ] {
        let bytes = text.into_bytes();
        let header = alice.request("d1", &key, "POST", &path, &bytes, t.now() + 60);
        let reply = t.send("POST", &path, Some(bytes), Some(header)).await;
        assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
        assert_eq!(reply.body["error"], "canonical");
    }
    assert_eq!(t.count("requests"), 0);
    // Sent as its JCS, the same post is taken.
    let send = json!({"body": {"kind": "send-master"}, "expires": expires});
    let reply = signed_body(&t, &alice, "POST", &path, &send, ("d1", &key)).await;
    assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
}

#[tokio::test]
async fn a_request_of_each_kind_lives_seven_days() {
    let t = TestStore::with_identity_burst(100);
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/requests/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    for posted in [
        json!({"body": {"kind": "send-master"}, "expires": T0 + REQUEST_TTL_S}),
        json!({"body": {"kind": "confirm-revoke", "keyIds": ["d1"], "rotate": false}, "expires": T0 + REQUEST_TTL_S}),
    ] {
        assert_eq!(
            signed_body(&t, &alice, "POST", &path, &posted, d1)
                .await
                .status,
            StatusCode::CREATED
        );
    }
    t.set_now(T0 + REQUEST_TTL_S - 3600);
    assert_eq!(
        signed(&t, &alice, "GET", &path, d1)
            .await
            .body
            .as_array()
            .unwrap()
            .len(),
        2
    );
    t.set_now(T0 + REQUEST_TTL_S);
    assert_eq!(signed(&t, &alice, "GET", &path, d1).await.body, json!([]));
    t.store.sweep().unwrap();
    assert_eq!(t.count("requests"), 0);
}

// ----- the identity bucket -----

#[tokio::test]
async fn the_identity_bucket_passes_ten_writes_then_one_a_minute_and_never_counts_reads() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let write = |v: u64| serde_json::to_value(alice.sealed_record(v, None)).unwrap();
    for v in 1..=10 {
        assert_eq!(
            signed_body(&t, &alice, "PUT", &path, &write(v), d1)
                .await
                .status,
            StatusCode::OK
        );
    }
    let reply = signed_body(&t, &alice, "PUT", &path, &write(11), d1).await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(reply.body["scope"], "identity");
    assert_eq!(reply.headers["retry-after"], "60");
    // Other kinds of write share the bucket.
    let sealed = alice.seal_master("d1");
    assert_eq!(
        master_put_joined(&t, &alice, "d1", &sealed).await.status,
        StatusCode::TOO_MANY_REQUESTS
    );
    // Reads are never counted against the identity.
    for _ in 0..50 {
        assert_eq!(
            signed(&t, &alice, "GET", &path, d1).await.status,
            StatusCode::OK
        );
    }
    // One write more a minute later; the bucket refills one at a time.
    t.set_now(T0 + 60);
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &write(11), d1)
            .await
            .status,
        StatusCode::OK
    );
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &write(12), d1)
            .await
            .status,
        StatusCode::TOO_MANY_REQUESTS
    );
    // Another identity has its own bucket.
    let bob = Person::new("bob");
    t.publish(&bob).await;
    let bob_path = format!("/v1/private/{}", bob.scid());
    let bob_record = serde_json::to_value(bob.sealed_record(1, None)).unwrap();
    assert_eq!(
        signed_body(
            &t,
            &bob,
            "PUT",
            &bob_path,
            &bob_record,
            ("d1", &bob.device_key(1))
        )
        .await
        .status,
        StatusCode::OK
    );
}

#[tokio::test]
async fn the_bucket_size_and_refill_come_from_the_settings() {
    let t = TestStore::with_settings(identity_store::Settings {
        identity_burst: 2,
        identity_refill_s: 10,
        ..identity_store::Settings::default()
    });
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let write = |v: u64| serde_json::to_value(alice.sealed_record(v, None)).unwrap();
    for v in 1..=2 {
        assert_eq!(
            signed_body(&t, &alice, "PUT", &path, &write(v), d1)
                .await
                .status,
            StatusCode::OK
        );
    }
    let reply = signed_body(&t, &alice, "PUT", &path, &write(3), d1).await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(reply.headers["retry-after"], "10");
    t.set_now(T0 + 10);
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &write(3), d1)
            .await
            .status,
        StatusCode::OK
    );
}

#[tokio::test]
async fn a_refused_write_takes_no_token() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let v1 = serde_json::to_value(alice.sealed_record(1, None)).unwrap();
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &v1, d1).await.status,
        StatusCode::OK
    );
    for _ in 0..10 {
        let reply = signed_body(&t, &alice, "PUT", &path, &v1, d1).await;
        assert_eq!(reply.status, StatusCode::CONFLICT);
    }
    for v in 2..=5 {
        let reply = signed_body(
            &t,
            &alice,
            "PUT",
            &path,
            &serde_json::to_value(alice.sealed_record(v, None)).unwrap(),
            d1,
        )
        .await;
        assert_eq!(reply.status, StatusCode::OK);
    }
}

/// Review coasys/ad4m#1632 (Data's red test): at bc18061d2 the next-version rule ran only when a
/// row existed, so a first write could carry any version. At 2^53 − 1 the next version is past
/// the safe integers and every later write is refused with no way back. The first write must be
/// version 1.
#[tokio::test]
async fn a_first_record_write_must_be_version_one() {
    let t = TestStore::with_identity_burst(100);
    let (alice, _phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let recovery = X25519Secret::from([5u8; 32]);
    for version in [0u64, 2, (1u64 << 53) - 1] {
        let first =
            serde_json::to_value(alice.sealed_record(version, Some(x25519_multikey_of(&recovery))))
                .unwrap();
        let reply = signed_body(&t, &alice, "PUT", &path, &first, d1).await;
        assert_ne!(
            reply.status,
            StatusCode::OK,
            "a first write at version {version} was kept"
        );
        assert_eq!(reply.status, StatusCode::CONFLICT, "{version}");
        assert_eq!(reply.body["version"], 0, "{version}");
    }
}

/// Review coasys/ad4m#1632 (Data's red test): at bc18061d2, with no recovery key stored, any
/// non-bot device key installed one, and from then on no writer, the master included, could
/// change it. Installing or replacing the recovery key needs the current master.
#[tokio::test]
async fn a_device_key_alone_does_not_install_the_recovery_key() {
    let t = TestStore::with_identity_burst(100);
    let (alice, _phone) = alice_with_phone(&t).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let recovery = X25519Secret::from([7u8; 32]);
    let v1 =
        serde_json::to_value(alice.sealed_record(1, Some(x25519_multikey_of(&recovery)))).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &v1, d1).await;
    assert_ne!(
        reply.status,
        StatusCode::OK,
        "a device key installed a recovery key on its own: {}",
        reply.body
    );
    // The helper attaches the current master's attestation; a device-signed write still may
    // not install the key, only copy it.
    assert_eq!(reply.status, StatusCode::BAD_REQUEST);
    assert_eq!(reply.body["error"], "recovery-key");
    // The master installs it.
    let by_master = alice.sealed_record_by(
        1,
        Some(alice.attest_recovery(x25519_multikey_of(&recovery))),
        &alice.current_master(),
    );
    let reply = signed_body(
        &t,
        &alice,
        "PUT",
        &path,
        &serde_json::to_value(by_master).unwrap(),
        d1,
    )
    .await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}
