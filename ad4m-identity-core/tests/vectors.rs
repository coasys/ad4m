//! The format spec's vectors (AR-01 8.1, R1, R2, R4), recomputed from their inputs. Runs on
//! native and on wasm32-unknown-unknown (wasm-bindgen-test under Node). The words part of
//! `commitment.json` needs the `words` feature.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const FILES: &[(&str, &str)] = &[
    ("log", include_str!("../vectors/log.json")),
    ("fold", include_str!("../vectors/fold.json")),
    ("proofs", include_str!("../vectors/proofs.json")),
    ("commitment", include_str!("../vectors/commitment.json")),
    ("pow", include_str!("../vectors/pow.json")),
    ("domains", include_str!("../vectors/domains.json")),
    ("link", include_str!("../vectors/link.json")),
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
