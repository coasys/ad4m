//! `unlink` (signed by the key itself or the master) and the `since` of `revoke`
//! (bounded by the store's append time), on a real store.

mod common;

use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{build, Op};
use ad4m_ldk::identity::KeyIndexEntry;
use axum::http::StatusCode;

use common::agent::{multikey, Device, Person};
use common::{TestStore, T0};

/// A store with alice and two linked devices (`d2` = phone, `d3` = tablet).
async fn alice_with_two_devices(t: &TestStore) -> (Person, Device, Device) {
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let phone = Device::new("alice phone");
    let tablet = Device::new("alice tablet");
    t.link(&mut alice, &phone, OfferKind::Device, None).await;
    t.link(&mut alice, &tablet, OfferKind::Device, None).await;
    (alice, phone, tablet)
}

async fn index_entry(t: &TestStore, key: &ed25519_dalek::SigningKey) -> KeyIndexEntry {
    serde_json::from_value(t.get(&format!("/v1/keys/{}", multikey(key))).await.body).unwrap()
}

#[tokio::test]
async fn a_device_unlinks_itself_and_the_master_unlinks_another_key() {
    let t = TestStore::new();
    let (mut alice, phone, tablet) = alice_with_two_devices(&t).await;

    let by_phone = alice.unlink_self("d2", &phone.key);
    assert_eq!(by_phone.proof.len(), 1);
    let reply = t.append_push(&mut alice, by_phone).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);

    let by_master = alice.unlink("d3");
    let reply = t.append_push(&mut alice, by_master).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);

    for (id, key) in [("d2", &phone.key), ("d3", &tablet.key)] {
        let entry = index_entry(&t, key).await;
        assert_eq!(entry.key_id, id);
        assert!(entry.unlinked.is_some(), "{id}");
        assert_eq!(entry.revoked, None);
    }
}

#[tokio::test]
async fn a_third_device_cannot_unlink_another_and_a_device_signed_rival_is_no_fork() {
    let t = TestStore::new();
    let (mut alice, _phone, tablet) = alice_with_two_devices(&t).await;

    // The tablet signs the unlink of the phone: refused.
    let mut by_tablet = build::unlink_unsigned(&alice.chain, &"d2".to_string()).unwrap();
    build::cosign(&mut by_tablet, "d3", &tablet.key).unwrap();
    let reply = t.append(&alice.scid(), &[&by_tablet]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "MissingProof", "{}", reply.body);

    // A device-signed event at a seq the log already holds is no fork: the identity stays free.
    let held = alice.events.len() - 1;
    let prefix = ad4m_identity_core::fold(&alice.events[..held]).unwrap();
    let mut rival = build::unlink_unsigned(&prefix, &"d3".to_string()).unwrap();
    build::cosign(&mut rival, "d3", &tablet.key).unwrap();
    let reply = t.append(&alice.scid(), &[&rival]).await;
    assert_ne!(reply.status, StatusCode::OK, "{}", reply.body);
    assert_ne!(reply.body["error"], "forked", "{}", reply.body);
    assert_eq!(t.count("chain_forks"), 0);
    let own = alice.unlink("d2");
    let reply = t.append_push(&mut alice, own).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn an_unlinked_key_signs_no_request_and_the_master_may_revoke_it_after() {
    let t = TestStore::new();
    let (mut alice, phone, _tablet) = alice_with_two_devices(&t).await;
    let unlink = alice.unlink_self("d2", &phone.key);
    assert_eq!(
        t.append_push(&mut alice, unlink).await.status,
        StatusCode::OK
    );

    // The key's second unlink: it is no longer an active key.
    let again = alice.unlink_self("d2", &phone.key);
    let reply = t.append(&alice.scid(), &[&again]).await;
    assert_ne!(reply.status, StatusCode::OK, "{}", reply.body);

    // Its signed request is refused.
    let path = format!("/v1/notes/{}", alice.scid());
    let body = serde_json::to_vec(&serde_json::json!({ "token": "a-token" })).unwrap();
    let request = alice.request("d2", &phone.key, "POST", &path, &body, t.now() + 60);
    let reply = t.send("POST", &path, Some(body), Some(request)).await;
    assert_eq!(reply.status, StatusCode::FORBIDDEN, "{}", reply.body);

    // The master revokes the unlinked key.
    let revoke = alice.revoke(&["d2"]);
    let reply = t.append_push(&mut alice, revoke).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let entry = index_entry(&t, &phone.key).await;
    assert!(entry.unlinked.is_some() && entry.revoked.is_some());
}

#[tokio::test]
async fn a_since_is_held_to_the_stores_append_time() {
    let t = TestStore::new();
    let (alice, _phone, _tablet) = alice_with_two_devices(&t).await;
    let day = 86_400;
    let ms = |s: u64| s * 1000;
    let refused = |reply: common::Reply| {
        assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
        let text = reply.body.to_string();
        assert!(text.contains("since"), "{text}");
    };

    // Later than the append time.
    t.set_now(T0 + day);
    let future = alice.revoke_since(&["d2"], ms(T0 + day + 60));
    refused(t.append(&alice.scid(), &[&future]).await);
    // Before the key's add was appended.
    let before_add = alice.revoke_since(&["d2"], ms(T0 - 1));
    refused(t.append(&alice.scid(), &[&before_add]).await);

    // A year after the add is no refusal: there is no look-back cap.
    t.set_now(T0 + 365 * day);
    let long_ago = alice.revoke_since(&["d2"], ms(T0 + 3_600));
    assert_eq!(
        t.append(&alice.scid(), &[&long_ago]).await.status,
        StatusCode::OK
    );
}

#[tokio::test]
async fn a_since_is_compared_by_the_stores_whole_seconds() {
    let t = TestStore::new();
    let (mut alice, _phone, _tablet) = alice_with_two_devices(&t).await;
    let now = T0 + 86_400;
    t.set_now(now);

    // The first ms of the next second is after the store's time.
    let next_second = alice.revoke_since(&["d2"], (now + 1) * 1000);
    let reply = t.append(&alice.scid(), &[&next_second]).await;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert!(reply.body.to_string().contains("since"), "{}", reply.body);

    // Any ms within the store's second is accepted: a client's own clock reads it.
    let late_in_second = alice.revoke_since(&["d2"], now * 1000 + 999);
    let reply = t.append_push(&mut alice, late_in_second).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
}

#[tokio::test]
async fn a_since_in_range_is_kept_in_the_log() {
    let t = TestStore::new();
    let (mut alice, _phone, _tablet) = alice_with_two_devices(&t).await;
    t.set_now(T0 + 86_400);
    let ms = |s: u64| s * 1000;
    // In range: accepted and kept in the log.
    let good = alice.revoke_since(&["d2"], ms(T0 + 3_600));
    let reply = t.append_push(&mut alice, good).await;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    let kept = alice.events.last().unwrap();
    assert!(matches!(kept.op, Op::Revoke { since: Some(s), .. } if s == ms(T0 + 3_600)));
}
