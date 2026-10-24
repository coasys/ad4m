//! The wasm-bindgen exports give the answers of the shared vectors (wasm32 + feature `wasm`).
#![cfg(all(target_arch = "wasm32", feature = "wasm"))]

use ad4m_hash_chain::wasm::{compare_js, entry_hash_js};
use serde_json::Value;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

fn js(value: &Value) -> JsValue {
    js_sys::JSON::parse(&value.to_string()).unwrap()
}

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

#[wasm_bindgen_test]
fn entry_hash_export_matches_the_vectors() {
    let file: Value = serde_json::from_str(include_str!("../vectors/entry-hash.json")).unwrap();
    for case in file["cases"].as_array().unwrap() {
        let got = entry_hash_js(js(&case["entry"]));
        match case.get("hash") {
            Some(hash) => assert_eq!(got.unwrap(), hash.as_str().unwrap(), "{}", case["name"]),
            None => {
                let err = rust(&got.unwrap_err());
                assert_eq!(err["code"], "malformed", "{}", case["name"]);
            }
        }
    }
}

#[wasm_bindgen_test]
fn compare_export_matches_the_vectors() {
    let file: Value = serde_json::from_str(include_str!("../vectors/compare.json")).unwrap();
    for case in file["cases"].as_array().unwrap() {
        let list = |v: &Value| -> Vec<String> { serde_json::from_value(v.clone()).unwrap() };
        let got = compare_js(
            list(&case["ours"]),
            list(&case["theirs"]),
            case["from"].as_u64().unwrap() as f64,
            case["chain"].as_str().unwrap().to_string(),
        )
        .unwrap();
        assert_eq!(rust(&got), case["result"], "{}", case["name"]);
    }
    assert!(compare_js(vec![], vec![], 1.5, "c".into()).is_err());
    assert!(compare_js(vec![], vec![], -1.0, "c".into()).is_err());
}
