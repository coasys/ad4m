//! The request box: request kinds, their JSON, and the expiry and key checks of a post.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::requests::*;
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
