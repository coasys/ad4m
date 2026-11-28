//! The request box: request kinds, their JSON, the expiry and key checks of a post, and a
//! reader's check of the poster (`verify_posted`).
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::requests::*;
use ad4m_identity_core::KeyLife;
use common::*;
use serde_json::json;

fn post(body: DeviceRequestBody, expires: u64) -> PostedRequest {
    PostedRequest { body, expires }
}

fn revoke(ids: &[&str]) -> DeviceRequestBody {
    DeviceRequestBody::ConfirmRevoke {
        key_ids: ids.iter().map(|s| s.to_string()).collect(),
        rotate: false,
    }
}

#[test]
fn a_request_lives_at_most_seven_days() {
    let log = devices_log();
    let (state, head) = (log.chain.state(), log.seq());
    let now = LINK_NOW;
    let ok = |expires| {
        check_post(
            &post(DeviceRequestBody::SendMaster {}, expires),
            state,
            head,
            now,
        )
    };
    assert_eq!(ok(now + 1), Ok(()));
    assert_eq!(ok(now + REQUEST_TTL_S), Ok(()));
    assert_eq!(ok(now + REQUEST_TTL_S + 1), Err(RequestError::Expires));
    assert_eq!(ok(now), Err(RequestError::Expires));
}

/// The lifetime a poster gives its request ([`REQUEST_POST_TTL_S`]) is accepted by a store
/// whose clock is behind the poster's by up to [`MAX_CLOCK_SKEW_S`]: a phone a moment ahead of
/// its store must not have its asks refused.
#[test]
fn a_posted_lifetime_survives_a_poster_clock_ahead_of_the_store() {
    let log = devices_log();
    let (state, head) = (log.chain.state(), log.seq());
    let store_now = LINK_NOW;
    let poster_now = store_now + MAX_CLOCK_SKEW_S;
    assert_eq!(
        check_post(
            &post(
                DeviceRequestBody::SendMaster {},
                poster_now + REQUEST_POST_TTL_S
            ),
            state,
            head,
            store_now,
        ),
        Ok(())
    );
}

#[test]
fn confirm_revoke_names_distinct_device_keys_valid_at_the_head() {
    let mut log = devices_log();
    log.revoke(&["d2"]);
    let (state, head) = (log.chain.state(), log.seq());
    let check = |ids: &[&str]| check_post(&post(revoke(ids), LINK_NOW + 60), state, head, LINK_NOW);
    assert_eq!(check(&["d1"]), Ok(()));
    assert_eq!(check(&["d1", "d3"]), Ok(()));
    for refused in [&[][..], &["d2"], &["m0"], &["d9"], &["d1", "d1"]] {
        assert!(
            matches!(check(refused), Err(RequestError::Malformed(_))),
            "{refused:?}"
        );
    }
    let many: Vec<String> = (0..=MAX_REVOKE_KEYS).map(|i| format!("d{i}")).collect();
    let many: Vec<&str> = many.iter().map(String::as_str).collect();
    assert!(matches!(check(&many), Err(RequestError::Malformed(_))));
}

#[test]
fn request_kinds_have_their_wire_names_and_refuse_unknown_fields() {
    assert_eq!(
        serde_json::to_value(DeviceRequestBody::SendMaster {}).unwrap(),
        json!({"kind": "send-master"})
    );
    assert_eq!(
        serde_json::to_value(revoke(&["d2"])).unwrap(),
        json!({"kind": "confirm-revoke", "keyIds": ["d2"], "rotate": false})
    );
    for refused in [
        json!({"kind": "declined"}),
        json!({"kind": "send-master", "extra": 1}),
        json!({"kind": "confirm-revoke", "keyIds": ["d2"]}),
    ] {
        assert!(
            serde_json::from_value::<DeviceRequestBody>(refused.clone()).is_err(),
            "{refused}"
        );
    }
    assert!(serde_json::from_value::<PostedRequest>(
        json!({"body": {"kind": "send-master"}, "expires": 1, "by": "d1"})
    )
    .is_err());
}

// ----- a reader's check of the poster: verify_posted -----

/// The box entry `by` posted with `body` at `log`'s head, signed by `key`, as a store lists it.
fn boxed(
    log: &Log,
    by: &str,
    key: &ed25519_dalek::SigningKey,
    body: DeviceRequestBody,
) -> BoxedRequest {
    let expires = LINK_NOW + 3600;
    let bytes = posted_bytes(&post(body.clone(), expires)).unwrap();
    let scid = log.chain.id().clone();
    let head = log.chain.head();
    let req = StoreRequest {
        did: ad4m_identity_core::did_of(&scid),
        store: store_did(),
        key_id: by.to_string(),
        kel_seq: head.seq,
        kel_hash: head.hash.clone(),
        method: "POST".into(),
        path: format!("/v1/requests/{scid}"),
        body_hash: ad4m_hash_chain::hash_bytes(bytes.as_bytes()),
        nonce: bs58::encode([7u8; 16]).into_string(),
        expires: LINK_NOW + 60,
    };
    BoxedRequest {
        id: "r1".into(),
        body,
        by: by.to_string(),
        created: LINK_NOW,
        expires,
        signed: sign_request(req, key),
    }
}

fn sign_request(req: StoreRequest, key: &ed25519_dalek::SigningKey) -> SignedStoreRequest {
    use ed25519_dalek::Signer as _;
    let bytes =
        ad4m_identity_core::signing_bytes(ad4m_identity_core::REQUEST_DOMAIN, &req).unwrap();
    let sig = ad4m_hash_chain::encode_proof_value(&key.sign(&bytes).to_bytes());
    SignedStoreRequest { req, sig }
}

#[test]
fn a_post_is_sent_as_the_jcs_of_body_and_expires() {
    assert_eq!(
        posted_bytes(&post(revoke(&["d2"]), 5)).unwrap(),
        r#"{"body":{"keyIds":["d2"],"kind":"confirm-revoke","rotate":false},"expires":5}"#
    );
}

#[test]
fn verify_posted_accepts_the_devices_own_request() {
    let log = devices_log();
    for body in [DeviceRequestBody::SendMaster {}, revoke(&["d2"])] {
        assert_eq!(
            verify_posted(&boxed(&log, "d1", &dev(1), body), &log.chain),
            Ok(())
        );
    }
    // The store's JSON of an entry reads back into the type.
    let entry = boxed(&log, "d2", &dev(2), DeviceRequestBody::SendMaster {});
    let json = serde_json::to_value(&entry).unwrap();
    assert_eq!(json["signed"]["req"]["keyId"], "d2");
    let back: BoxedRequest = serde_json::from_value(json).unwrap();
    assert_eq!(verify_posted(&back, &log.chain), Ok(()));
}

#[test]
fn verify_posted_refuses_a_changed_body_or_a_request_for_another_post() {
    let log = devices_log();
    let honest = boxed(&log, "d1", &dev(1), revoke(&["d2"]));
    let refused = |change: &dyn Fn(&mut BoxedRequest)| {
        let mut r = honest.clone();
        change(&mut r);
        verify_posted(&r, &log.chain)
    };
    let request = |e: Result<(), RequestError>| matches!(e, Err(RequestError::Request(_)));
    // The body or the expiry the store answers differs from what the device signed.
    assert!(request(refused(&|r| r.body = revoke(&["d1"]))));
    assert!(request(refused(
        &|r| r.body = DeviceRequestBody::SendMaster {}
    )));
    assert!(request(refused(&|r| r.expires += 1)));
    // The entry claims another poster than the key that signed.
    assert!(request(refused(&|r| r.by = "d2".into())));
    // Each field of the signed request that binds it to this post (re-signed, so only the
    // field is wrong).
    let resigned = |change: &dyn Fn(&mut StoreRequest)| {
        let mut r = honest.clone();
        let mut req = r.signed.req.clone();
        change(&mut req);
        r.signed = sign_request(req, &dev(1));
        verify_posted(&r, &log.chain)
    };
    assert!(request(resigned(
        &|q| q.did = ad4m_identity_core::did_of("other")
    )));
    assert!(request(resigned(&|q| q.method = "PUT".into())));
    assert!(request(resigned(&|q| q.path = "/v1/requests/other".into())));
    assert!(request(resigned(
        &|q| q.kel_hash = ad4m_hash_chain::hash_bytes(b"x")
    )));
    // The signature over another request, or a broken one.
    assert_eq!(
        refused(&|r| r.signed.req.nonce = bs58::encode([8u8; 16]).into_string()),
        Err(RequestError::Signature)
    );
    assert_eq!(
        refused(&|r| r.signed.sig = "z1".into()),
        Err(RequestError::Signature)
    );
    // Signed with another key than the log lists for d1.
    let forged = boxed(&log, "d1", &dev(2), revoke(&["d2"]));
    assert_eq!(
        verify_posted(&forged, &log.chain),
        Err(RequestError::Signature)
    );
}

#[test]
fn verify_posted_needs_a_device_key_of_the_log_without_the_bot_mark() {
    let log = devices_log();
    let send = || DeviceRequestBody::SendMaster {};
    // A key the log does not hold, a master key and a bot key (d3) do not post.
    for (by, key) in [("d9", dev(9)), ("m0", master(0)), ("d3", dev(3))] {
        assert_eq!(
            verify_posted(&boxed(&log, by, &key, send()), &log.chain),
            Err(RequestError::Signer(by.into())),
            "{by}"
        );
    }
}

#[test]
fn verify_posted_refuses_a_key_revoked_since_and_waits_for_a_log_behind_the_request() {
    let mut log = devices_log();
    let before = log.chain.clone();
    let posted = boxed(&log, "d2", &dev(2), DeviceRequestBody::SendMaster {});
    assert_eq!(verify_posted(&posted, &log.chain), Ok(()));
    // Revoked after it posted: its request no longer counts at the reader's head.
    log.revoke(&["d2"]);
    assert_eq!(
        verify_posted(&posted, &log.chain),
        Err(RequestError::Signer("d2".into()))
    );
    // d1 posts at the new head; a reader whose log stops before it reads the log first.
    let later = boxed(&log, "d1", &dev(1), DeviceRequestBody::SendMaster {});
    assert_eq!(verify_posted(&later, &log.chain), Ok(()));
    assert_eq!(
        verify_posted(&later, &before),
        Err(RequestError::Behind(log.seq()))
    );
    // A key added after the position the request names was not valid there.
    let mut log = devices_log();
    let mut early = boxed(&log, "d4", &dev(4), DeviceRequestBody::SendMaster {});
    log.add(KeyLife::Standing);
    early.signed = sign_request(early.signed.req.clone(), &dev(4));
    assert_eq!(
        verify_posted(&early, &log.chain),
        Err(RequestError::Signer("d4".into()))
    );
}
