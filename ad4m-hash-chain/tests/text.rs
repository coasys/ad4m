//! Entries received as text: `entry_from_json` accepts only the entry's JCS text, so one
//! accepted text has one parse and one hash. Runs on native and on wasm32-unknown-unknown.

mod common;

use ad4m_hash_chain::{entry_from_json, entry_hash, entry_json, CanonError, Entry, Hash, Proof};
use common::*;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

/// An entry type with a free JSON member, so numbers and nested objects reach the check.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct Free {
    seq: u64,
    prev: Option<Hash>,
    body: Value,
}

impl Entry for Free {
    fn seq(&self) -> u64 {
        self.seq
    }
    fn prev(&self) -> Option<&Hash> {
        self.prev.as_ref()
    }
    fn proofs(&self) -> &[Proof] {
        &[]
    }
}

fn refused<E: Entry + std::fmt::Debug>(text: &str) -> CanonError {
    entry_from_json::<E>(text).expect_err(text)
}

#[test]
fn the_canonical_text_parses_back_to_the_entry() {
    for entry in make_chain("c", 1, 3) {
        let text = entry_json(&entry).unwrap();
        assert_eq!(entry_from_json::<TestEntry>(&text).unwrap(), entry);
    }
    let free = r#"{"body":{"a":[1,-2,null]},"prev":null,"seq":0}"#;
    assert_eq!(
        entry_json(&entry_from_json::<Free>(free).unwrap()).unwrap(),
        free
    );
}

#[test]
fn a_null_member_the_type_drops_is_refused() {
    // `key` is a skipped `None`: parsing `"key":null` would drop it from the hash natively,
    // while a JSON object holding it hashes with it.
    let entry = make_chain("c", 1, 2)[1].clone();
    assert_eq!(entry.key, None);
    let text = entry_json(&entry).unwrap();
    let with_null = text.replacen("\"chain\"", "\"key\":null,\"chain\"", 1);
    assert_ne!(with_null, text);
    assert_eq!(refused::<TestEntry>(&with_null), CanonError::NotCanonical);
    let mut value: Value = serde_json::from_str(&with_null).unwrap();
    let hash_with_null = ad4m_hash_chain::entry_hash_json(&value).unwrap();
    assert_ne!(hash_with_null, entry_hash(&entry).unwrap());
    value.as_object_mut().unwrap().remove("key");
    assert_eq!(
        ad4m_hash_chain::entry_hash_json(&value).unwrap(),
        entry_hash(&entry).unwrap()
    );
}

#[test]
fn other_spellings_of_a_number_are_refused() {
    for body in ["1e2", "100.0", "-0", "1E2", "0.5"] {
        let text = format!(r#"{{"body":{body},"prev":null,"seq":0}}"#);
        assert!(
            matches!(
                refused::<Free>(&text),
                CanonError::Float(_) | CanonError::NotCanonical
            ),
            "{text}"
        );
    }
    // In an integer member the parse already fails.
    for seq in ["1e0", "1.0", "-0"] {
        let text = format!(r#"{{"body":1,"prev":null,"seq":{seq}}}"#);
        assert!(entry_from_json::<Free>(&text).is_err(), "{text}");
    }
    assert!(entry_from_json::<Free>(r#"{"body":100,"prev":null,"seq":0}"#).is_ok());
}

#[test]
fn a_duplicate_member_name_is_refused() {
    // In a free member the parser keeps the last one; the text check refuses it.
    let nested = r#"{"body":{"a":1,"a":2},"prev":null,"seq":0}"#;
    assert_eq!(refused::<Free>(nested), CanonError::NotCanonical);
    let top = r#"{"body":1,"body":1,"prev":null,"seq":0}"#;
    assert!(entry_from_json::<Free>(top).is_err());
    let entry = make_chain("c", 1, 1)[0].clone();
    let text = entry_json(&entry).unwrap();
    let twice = text.replacen("\"body\"", "\"body\":\"genesis\",\"body\"", 1);
    assert!(entry_from_json::<TestEntry>(&twice).is_err());
}

#[test]
fn whitespace_and_member_order_are_refused() {
    assert_eq!(
        refused::<Free>(r#"{"body":1, "prev":null,"seq":0}"#),
        CanonError::NotCanonical
    );
    assert_eq!(
        refused::<Free>(r#"{"seq":0,"prev":null,"body":1}"#),
        CanonError::NotCanonical
    );
    assert_eq!(
        refused::<Free>("{\"body\":1,\"prev\":null,\"seq\":0}\n"),
        CanonError::NotCanonical
    );
    assert_eq!(
        // `\u0041` spells "A" another way.
        refused::<Free>("{\"body\":\"\\u0041\",\"prev\":null,\"seq\":0}"),
        CanonError::NotCanonical
    );
}
