//! The wasm-bindgen export `MembershipReplay` gives the answers of every membership vector
//! (wasm32 + feature `wasm`): each step's changes or refusal, then the members and the
//! self-joins of the file. `tests/membership.rs` runs the same files natively.
#![cfg(all(target_arch = "wasm32", feature = "wasm"))]

use ad4m_identity_core::wasm::MembershipReplay;
use serde_json::{json, Value};
use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

const VECTORS: &[(&str, &str)] = &[
    (
        "rules-creator-first",
        include_str!("../vectors/membership/rules-creator-first.json"),
    ),
    (
        "rules-without-join",
        include_str!("../vectors/membership/rules-without-join.json"),
    ),
    (
        "join-by-admin",
        include_str!("../vectors/membership/join-by-admin.json"),
    ),
    (
        "self-join-open",
        include_str!("../vectors/membership/self-join-open.json"),
    ),
    (
        "needs-identity",
        include_str!("../vectors/membership/needs-identity.json"),
    ),
    (
        "self-join-invite",
        include_str!("../vectors/membership/self-join-invite.json"),
    ),
    (
        "removed-by",
        include_str!("../vectors/membership/removed-by.json"),
    ),
    (
        "admin-removal-bars-open",
        include_str!("../vectors/membership/admin-removal-bars-open.json"),
    ),
    (
        "self-removal-readmits",
        include_str!("../vectors/membership/self-removal-readmits.json"),
    ),
    (
        "backdated",
        include_str!("../vectors/membership/backdated.json"),
    ),
    ("bot", include_str!("../vectors/membership/bot.json")),
    (
        "not-counted",
        include_str!("../vectors/membership/not-counted.json"),
    ),
    (
        "out-of-order",
        include_str!("../vectors/membership/out-of-order.json"),
    ),
    ("shapes", include_str!("../vectors/membership/shapes.json")),
];

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

/// The refusal a thrown `{ code: "refused", reason }` names.
fn reason(thrown: &JsValue) -> Value {
    let thrown = rust(thrown);
    assert_eq!(thrown["code"], "refused", "{thrown}");
    thrown["reason"].clone()
}

#[wasm_bindgen_test]
fn membership_replay_export_matches_every_vector() {
    for (name, text) in VECTORS {
        let file: Value = serde_json::from_str(text).unwrap();
        let mut replay = MembershipReplay::new(file["creator"].as_str().unwrap().to_string());
        for (i, step) in file["steps"].as_array().unwrap().iter().enumerate() {
            match replay.apply(&step["link"].to_string()) {
                Ok(changes) => {
                    let changes: Value = serde_json::from_str(&changes).unwrap();
                    assert_eq!(changes, step["changes"], "{name}: step {i}");
                }
                Err(thrown) => assert_eq!(reason(&thrown), step["refused"], "{name}: step {i}"),
            }
        }
        let members: Value = serde_json::from_str(&replay.members().unwrap()).unwrap();
        assert_eq!(members, file["members"], "{name}: members");
        for member in file["members"].as_array().unwrap() {
            let person = member["person"].as_str().unwrap();
            assert_eq!(
                replay.is_member(person),
                member["until"].is_null(),
                "{name}: {person}"
            );
        }
        for query in file["admits"].as_array().unwrap() {
            let got = replay.admits(
                query["person"].as_str().map(str::to_string),
                query["byBot"].as_bool().unwrap(),
                query["invite"].as_str().map(str::to_string),
                query["now"].as_u64().unwrap() as f64,
            );
            let got = match got {
                Ok(()) => Value::Null,
                Err(thrown) => reason(&thrown),
            };
            assert_eq!(got, query["refused"], "{name}: {query}");
        }
    }
}

#[wasm_bindgen_test]
fn membership_replay_export_refuses_what_is_not_a_judged_link() {
    let mut replay = MembershipReplay::new("did:scid:coasys:1:Ada".to_string());
    let thrown = rust(&replay.apply("{}").err().unwrap());
    assert_eq!(thrown["code"], "malformed");
    let thrown = rust(&replay.admits(None, false, None, -1.0).err().unwrap());
    assert_eq!(
        thrown,
        json!({"code": "malformed", "reason": "now must be a safe integer"})
    );
}
