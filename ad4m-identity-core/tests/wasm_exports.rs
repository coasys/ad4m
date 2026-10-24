//! The wasm-bindgen exports `KelChain.fold` and `KelChain.import` give the answers of the
//! shared vectors (wasm32 + feature `wasm`).
#![cfg(all(target_arch = "wasm32", feature = "wasm"))]

use ad4m_identity_core::wasm::KelChainJs;
use serde_json::{json, Value};
use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

fn js(value: &Value) -> JsValue {
    js_sys::JSON::parse(&value.to_string()).unwrap()
}

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

fn pinned(code: &Value) -> Value {
    let mut out = serde_json::Map::new();
    for key in ["code", "reason", "format"] {
        if let Some(v) = code.get(key) {
            out.insert(key.into(), v.clone());
        }
    }
    Value::Object(out)
}

#[wasm_bindgen_test]
fn fold_export_matches_the_log_vector() {
    let file: Value = serde_json::from_str(include_str!("../vectors/log.json")).unwrap();
    let handle = KelChainJs::fold(js(&file["events"])).unwrap();
    assert_eq!(handle.id(), file["scid"].as_str().unwrap());
    assert_eq!(json!(handle.hashes()), file["hashes"]);
    assert_eq!(rust(&handle.state().unwrap()), file["state"]);
    let head = rust(&handle.head().unwrap());
    assert_eq!(head["hash"], file["hashes"][6]);
    assert_eq!(
        handle.hash_at(0.0).unwrap(),
        file["hashes"][0].as_str().unwrap()
    );
    assert!(handle.hash_at(1.5).is_none());
}

#[wasm_bindgen_test]
fn fold_export_refuses_every_fold_vector() {
    let file: Value = serde_json::from_str(include_str!("../vectors/fold.json")).unwrap();
    for case in file["cases"].as_array().unwrap() {
        let err = match KelChainJs::fold(js(&case["events"])) {
            Ok(_) => panic!("{} folds", case["name"]),
            Err(e) => rust(&e),
        };
        assert_eq!(pinned(&err), case["error"], "{}", case["name"]);
    }
    assert_eq!(
        rust(&KelChainJs::fold(js(&json!({}))).err().unwrap())["code"],
        "malformed"
    );
}

#[wasm_bindgen_test]
fn import_export_appends_and_reports() {
    let file: Value = serde_json::from_str(include_str!("../vectors/log.json")).unwrap();
    let events = file["events"].as_array().unwrap();
    let mut handle = KelChainJs::fold(js(&json!([events[0].clone()]))).unwrap();
    let gap = rust(&KelChainJs::import(&mut handle, js(&json!([events[3].clone()]))).unwrap());
    assert_eq!(gap, json!({"kind": "gap", "have": 0, "first": 3}));
    let rest: Vec<Value> = events[1..].iter().rev().cloned().collect();
    let appended = rust(&KelChainJs::import(&mut handle, js(&json!(rest))).unwrap());
    assert_eq!(appended["kind"], "appended");
    assert_eq!(appended["count"], 6);
    assert_eq!(json!(handle.hashes()), file["hashes"]);
    let again = rust(&KelChainJs::import(&mut handle, js(&file["events"])).unwrap());
    assert_eq!(again, json!({"kind": "already-have"}));
    // An event that breaks a rule is thrown as a policy code; the handle is unchanged.
    let mut bad = events[1].clone();
    bad["name"] = json!("x");
    let mut fresh = KelChainJs::fold(js(&json!([events[0].clone()]))).unwrap();
    let err = rust(
        &KelChainJs::import(&mut fresh, js(&json!([bad])))
            .err()
            .unwrap(),
    );
    assert_eq!(err["code"], "policy");
    assert_eq!(err["reason"], "Format");
    assert_eq!(fresh.hashes().len(), 1);
}
