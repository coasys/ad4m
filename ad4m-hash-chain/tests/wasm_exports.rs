//! The wasm-bindgen exports give the answers of the shared vectors (wasm32 + feature `wasm`).
#![cfg(all(target_arch = "wasm32", feature = "wasm"))]

use ad4m_hash_chain::wasm::{compare_js, entry_hash_js};
use serde_json::{json, Value};
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_test::wasm_bindgen_test;

fn js(value: &Value) -> JsValue {
    js_sys::JSON::parse(&value.to_string()).unwrap()
}

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

fn list(hashes: &[&str]) -> JsValue {
    js(&json!(hashes))
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
        let got = compare_js(
            js(&case["ours"]),
            js(&case["theirs"]),
            case["from"].as_u64().unwrap() as f64,
            case["chain"].as_str().unwrap().to_string(),
        )
        .unwrap();
        assert_eq!(rust(&got), case["result"], "{}", case["name"]);
    }
    assert!(compare_js(list(&[]), list(&[]), 1.5, "c".into()).is_err());
    assert!(compare_js(list(&[]), list(&[]), -1.0, "c".into()).is_err());
}

#[wasm_bindgen_test]
fn compare_export_throws_a_chain_error_code_for_non_string_hashes() {
    for bad in [
        json!(["Qma", 5]),
        json!([null]),
        json!("Qma"),
        json!({"0": "Qma"}),
    ] {
        let err = compare_js(js(&bad), list(&["Qma"]), 0.0, "c".into()).unwrap_err();
        assert_eq!(rust(&err)["code"], "malformed", "{bad}");
        let err = compare_js(list(&["Qma"]), js(&bad), 0.0, "c".into()).unwrap_err();
        assert_eq!(rust(&err)["code"], "malformed", "{bad}");
    }
}

/// What the caller's JavaScript throws while the list is read reaches it as `malformed`.
#[wasm_bindgen_test]
fn compare_export_throws_a_chain_error_code_when_reading_the_list_throws() {
    use js_sys::{Array, Function, Object, Proxy, Reflect, Symbol};
    let thrower = Function::new_no_args("throw new Error('boom')");
    // An array whose iterator throws.
    let iterator = Array::of1(&"Qma".into());
    Reflect::set(&iterator, &Symbol::iterator(), &thrower).unwrap();
    // A Proxy of an array whose `get` trap throws (Array.from reads Symbol.iterator).
    let handler = Object::new();
    Reflect::set(&handler, &"get".into(), &thrower).unwrap();
    let trapped = Proxy::new(&Array::of1(&"Qma".into()), &handler);
    // A revoked Proxy: Array.isArray itself throws.
    let revocable = Proxy::revocable(&Array::new(), &Object::new());
    let revoked = Reflect::get(&revocable, &"proxy".into()).unwrap();
    Reflect::get(&revocable, &"revoke".into())
        .unwrap()
        .unchecked_into::<Function>()
        .call0(&JsValue::UNDEFINED)
        .unwrap();
    for (name, bad) in [
        ("iterator", JsValue::from(iterator)),
        ("trap", JsValue::from(trapped)),
        ("revoked", revoked),
    ] {
        let err = compare_js(bad.clone(), list(&["Qma"]), 0.0, "c".into()).unwrap_err();
        assert_eq!(rust(&err)["code"], "malformed", "{name}");
        let err = compare_js(list(&["Qma"]), bad, 0.0, "c".into()).unwrap_err();
        assert_eq!(rust(&err)["code"], "malformed", "{name}");
    }
}

#[wasm_bindgen_test]
fn compare_export_refuses_seqs_past_max_safe_integer() {
    let max = ad4m_hash_chain::MAX_SAFE_INTEGER;
    // The last hash sits at seq max: the answer names seq max exactly.
    let got = compare_js(
        list(&["Qma"]),
        list(&["Qma", "Qmb"]),
        (max - 1) as f64,
        "c".into(),
    );
    assert_eq!(
        rust(&got.unwrap()),
        json!({"kind": "they-extend", "from": max})
    );
    let got = compare_js(
        list(&["Qma", "Qmb"]),
        list(&["Qma", "Qmc"]),
        (max - 1) as f64,
        "c".into(),
    );
    assert_eq!(rust(&got.unwrap())["fork"]["seq"], json!(max));
    // A hash at seq max + 1: that seq is not a safe integer in JavaScript.
    let err = compare_js(
        list(&["Qma"]),
        list(&["Qma", "Qmb"]),
        max as f64,
        "c".into(),
    );
    assert_eq!(rust(&err.unwrap_err())["code"], "malformed");
    let err = compare_js(
        list(&["Qma", "Qmb"]),
        list(&["Qma", "Qmc"]),
        max as f64,
        "c".into(),
    );
    assert_eq!(rust(&err.unwrap_err())["code"], "malformed");
}
