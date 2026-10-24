//! The shared vectors, recomputed from their inputs. Runs on native and on
//! wasm32-unknown-unknown (wasm-bindgen-test under Node).

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const FILES: &[(&str, &str)] = &[
    ("entry-hash", include_str!("../vectors/entry-hash.json")),
    (
        "signing-input",
        include_str!("../vectors/signing-input.json"),
    ),
    ("proof-check", include_str!("../vectors/proof-check.json")),
    ("fold", include_str!("../vectors/fold.json")),
    ("import", include_str!("../vectors/import.json")),
    ("compare", include_str!("../vectors/compare.json")),
];

#[test]
fn every_vector_file_holds() {
    let names: Vec<&str> = FILES.iter().map(|(n, _)| *n).collect();
    assert_eq!(names, common::VECTOR_NAMES);
    for (name, text) in FILES {
        let file: serde_json::Value = serde_json::from_str(text).unwrap();
        common::check_vector_file(name, &file);
    }
}
