//! The wasm-bindgen exports `KelChain.fold` and `KelChain.import` give the answers of the
//! shared vectors, and `Pow.solve`, `Pow.solveRange` and `Pow.check` the answers of
//! `ad4m_identity_core::pow` (wasm32 + feature `wasm`).
#![cfg(all(target_arch = "wasm32", feature = "wasm"))]

use ad4m_identity_core::pow;
use ad4m_identity_core::wasm::{KelChainJs, PowJs};
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
fn did_document_export_matches_the_document_vector() {
    let file: Value = serde_json::from_str(include_str!("../vectors/document.json")).unwrap();
    let handle = KelChainJs::fold(js(&file["events"])).unwrap();
    let aka: Vec<String> = serde_json::from_value(file["alsoKnownAs"].clone()).unwrap();
    for case in file["documents"].as_array().unwrap() {
        let version = case["version"].as_u64().map(|v| v as f64);
        let got = rust(&handle.did_document(version, Some(aka.clone())).unwrap());
        assert_eq!(got["didDocument"], case["didDocument"], "{version:?}");
        assert_eq!(got["didDocumentMetadata"], case["didDocumentMetadata"]);
    }
    let past = file["refused"][0]["version"].as_u64().unwrap() as f64;
    let err = rust(&handle.did_document(Some(past), None).err().unwrap());
    assert_eq!(err["code"], "document");
    assert_eq!(err["reason"], file["refused"][0]["error"]);
    let err = rust(&handle.did_document(Some(1.5), None).err().unwrap());
    assert_eq!(err["code"], "malformed");
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

const POW_SCID: &str = "QmZkP3wW4ytZVGAvdUSYQuKgnuqmC1xdqNx1MhLeD6V8Wq";

#[wasm_bindgen_test]
fn pow_exports_give_the_proofs_of_the_core() {
    let solved = rust(&PowJs::solve(POW_SCID, 12).unwrap());
    let native = pow::solve(POW_SCID, 12);
    assert_eq!(solved, json!({ "nonce": native.nonce, "bits": 12 }));
    assert!(PowJs::check(POW_SCID, js(&solved), 12));
    assert!(!PowJs::check("QmAnotherScid", js(&solved), 12));
    assert!(!PowJs::check(POW_SCID, js(&json!({ "nonce": 7 })), 12));
    assert!(!PowJs::check(POW_SCID, JsValue::UNDEFINED, 0));

    // In rounds from 0, the first round that holds the proof gives the same one.
    let mut start = 0.0;
    let found = loop {
        let round = PowJs::solve_range(POW_SCID, 12, start, 1024.0).unwrap();
        if !round.is_undefined() {
            break rust(&round);
        }
        start += 1024.0;
    };
    assert_eq!(found, solved);
    let err = rust(&PowJs::solve_range(POW_SCID, 12, -1.0, 1.0).err().unwrap());
    assert_eq!(err["code"], "malformed");
    let err = rust(&PowJs::solve_range(POW_SCID, 12, 0.0, 0.5).err().unwrap());
    assert_eq!(err["code"], "malformed");
}
