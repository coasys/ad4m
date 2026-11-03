//! The device inbox, the private details record, the request box, the identity bucket and the
//! sweeper, on a real store over a temporary SQLite file.

mod common;

use ad4m_hash_chain::{canonical_json, hash_bytes};
use ad4m_identity_core::inbox::{check_payload, Accept, InboxAddress, JOINED_TTL_S};
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::record::open_record;
use ad4m_identity_core::requests::REQUEST_TTL_S;
use ad4m_identity_core::{x25519_multikey_of, BotScope, X25519Secret};
use axum::http::StatusCode;
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

    let recovery = X25519Secret::from([5u8; 32]);
    let v1 = alice.sealed_record(1, Some(x25519_multikey_of(&recovery)));
    let v1_json = serde_json::to_value(&v1).unwrap();
    let reply = signed_body(&t, &alice, "PUT", &path, &v1_json, d1).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_eq!(reply.body, json!({"version": 1}));

    // Every device key and the current master read it back as written.
    let master = alice.current_master();
    for signer in [d1, ("d2", &phone.key), (&alice.master_id(), &master)] {
        let reply = signed(&t, &alice, "GET", &path, signer).await;
        assert_eq!(reply.status, StatusCode::OK);
        assert_eq!(reply.body, v1_json);
    }
    let read: ad4m_identity_core::record::SealedRecord =
        serde_json::from_value(v1_json.clone()).unwrap();
    for (entry, secret) in [
        ("d1", person_enc_secret(&alice, 1)),
        ("d2", phone.enc_secret()),
        ("recovery", recovery.clone()),
    ] {
        let record = open_record(&read, &alice.scid(), entry, &secret).unwrap();
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

    // Sixteen open requests; the seventeenth is 429.
    let revoke = json!({"body": {"kind": "confirm-revoke", "keyIds": ["d1"], "rotate": true},
                        "expires": T0 + 60});
    for _ in 0..16 {
        let reply = signed_body(&t, &alice, "POST", &path, &revoke, d1).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
    }
    let reply = signed_body(&t, &alice, "POST", &path, &revoke, d1).await;
    assert_eq!(reply.status, StatusCode::TOO_MANY_REQUESTS);
    assert_eq!(reply.body["scope"], "identity");
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
async fn the_identity_bucket_passes_five_writes_then_one_a_minute_and_never_counts_reads() {
    let t = TestStore::new();
    let alice = Person::new("alice");
    t.publish(&alice).await;
    let path = format!("/v1/private/{}", alice.scid());
    let d1 = ("d1", &alice.device_key(1));
    let write = |v: u64| serde_json::to_value(alice.sealed_record(v, None)).unwrap();
    for v in 1..=5 {
        assert_eq!(
            signed_body(&t, &alice, "PUT", &path, &write(v), d1)
                .await
                .status,
            StatusCode::OK
        );
    }
    let reply = signed_body(&t, &alice, "PUT", &path, &write(6), d1).await;
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
        signed_body(&t, &alice, "PUT", &path, &write(6), d1)
            .await
            .status,
        StatusCode::OK
    );
    assert_eq!(
        signed_body(&t, &alice, "PUT", &path, &write(7), d1)
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
