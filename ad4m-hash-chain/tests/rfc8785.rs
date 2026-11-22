//! `canonical_json` against the RFC 8785 test data in `tests/rfc8785/` (see its README for
//! the source and the float cases left out). Runs on native and on wasm32-unknown-unknown.

use ad4m_hash_chain::canonical_json;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const CASES: &[(&str, &str, &str)] = &[
    (
        "arrays",
        include_str!("rfc8785/input/arrays.json"),
        include_str!("rfc8785/output/arrays.json"),
    ),
    (
        "french",
        include_str!("rfc8785/input/french.json"),
        include_str!("rfc8785/output/french.json"),
    ),
    (
        "structures",
        include_str!("rfc8785/input/structures.json"),
        include_str!("rfc8785/output/structures.json"),
    ),
    (
        "unicode",
        include_str!("rfc8785/input/unicode.json"),
        include_str!("rfc8785/output/unicode.json"),
    ),
    (
        "values",
        include_str!("rfc8785/input/values.json"),
        include_str!("rfc8785/output/values.json"),
    ),
    (
        "weird",
        include_str!("rfc8785/input/weird.json"),
        include_str!("rfc8785/output/weird.json"),
    ),
];

#[test]
fn canonical_json_gives_the_rfc_8785_outputs() {
    for (name, input, output) in CASES {
        let value: serde_json::Value = serde_json::from_str(input).unwrap();
        assert_eq!(canonical_json(&value).unwrap(), *output, "{name}");
    }
}
