//! A link read back from the store must carry the exact target it was signed
//! with, whatever literal encoding the writer used.
//!
//! The store keeps `literal:*` targets as typed RDF literals and renders them
//! back to wire form on read. Anything that republishes read-back links (for
//! example `ensure_public_links_are_shared`, which feeds `get_all_links()` to
//! the link language) hands on that rendering. If it differs by a single byte
//! from what was signed, the receiver's `compute_proof_valid` says `false`.
//!
//! Each test signs and inserts into store A, reads back with `get_all_links()`
//! (and `query_links()`), inserts the read-back copies into store B, and
//! requires B's verdict to be `true` for every link.

use crate::agent::signatures::TestSigner;
use crate::languages::literal::literal_encode;
use crate::perspectives::interpretation_test_support::{
    setup_perspective_no_llm, BELIEF_SDNA, TASK_WITH_RELATION_SDNA,
};
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkStatus};
use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
use serde_json::json;

fn signed(signer: &TestSigner, source: &str, target: &str) -> LinkExpression {
    let data = Link {
        source: source.to_string(),
        predicate: Some("rt://p".to_string()),
        target: target.to_string(),
    };
    let signed = signer.sign(data.normalize());
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(LinkStatus::Shared),
    }
}

/// Copy every link of `a` into a fresh store through `get_all_links()` and
/// return the targets store B did not verify. Also checks the control: A's
/// own verdict, computed on insert from the original bytes, is `true`.
fn not_verified_after_read_back(a: &SparqlStore) -> Vec<String> {
    let read_back = a.get_all_links().unwrap();
    assert!(!read_back.is_empty());
    for l in &read_back {
        assert_eq!(
            l.proof.valid,
            Some(true),
            "control: store A verified {} on insert",
            l.data.target
        );
    }
    let b = SparqlStore::new(None).unwrap();
    for l in read_back {
        b.add_link(&LinkExpression::from(l)).unwrap();
    }
    b.get_all_links()
        .unwrap()
        .into_iter()
        .filter(|l| l.proof.valid != Some(true))
        .map(|l| l.data.target)
        .collect()
}

/// Insert the signed links into A and require both read-back APIs to return
/// each target byte for byte, and B to verify every copy.
fn assert_round_trip(targets: &[String]) {
    let signer = TestSigner::generate();
    let a = SparqlStore::new(None).unwrap();
    for (i, target) in targets.iter().enumerate() {
        a.add_link(&signed(&signer, &format!("rt://s/{i}"), target))
            .unwrap();
    }

    let targets_of = |links: Vec<DecoratedLinkExpression>| {
        let mut t: Vec<(String, String)> = links
            .into_iter()
            .map(|l| (l.data.source, l.data.target))
            .collect();
        t.sort();
        t
    };
    let mut expected: Vec<(String, String)> = targets
        .iter()
        .enumerate()
        .map(|(i, t)| (format!("rt://s/{i}"), t.clone()))
        .collect();
    expected.sort();
    assert_eq!(targets_of(a.get_all_links().unwrap()), expected);
    assert_eq!(
        targets_of(a.query_links(None, None, None, None, None, None).unwrap()),
        expected
    );

    assert_eq!(not_verified_after_read_back(&a), Vec::<String>::new());
}

/// Ported from #1123's ignored
/// `proof_valid_an_unencoded_literal_still_verifies_after_a_read_back`: a raw
/// space and raw JSON, which the SDK would have percent-encoded.
#[test]
fn an_unencoded_literal_verifies_after_a_read_back() {
    assert_round_trip(&[
        "literal:string:Write the guide".to_string(),
        r#"literal:json:{"a":1}"#.to_string(),
        // Whitespace the store's JSON canonicalisation drops.
        r#"literal:json:{"a": 1, "b": "x y"}"#.to_string(),
    ]);
}

/// Over-encoded forms: `NON_ALPHANUMERIC` also escapes `-_.~`, which the
/// store renders bare. That is the form `literal_encode` (behind
/// `expression_create("literal")`) produced before it switched to the SDK's
/// set; links written then are still in the wild.
#[test]
fn an_over_encoded_literal_verifies_after_a_read_back() {
    let over = |s: &str| utf8_percent_encode(s, NON_ALPHANUMERIC).to_string();
    let json = json!({ "predicate": "bots://body", "data": "a-b_c.d~e" }).to_string();
    assert_round_trip(&[
        format!("literal:json:{}", over(&json)),
        format!("literal:string:{}", over("board://status-2")),
        // Whatever `literal_encode` writes today must round-trip too.
        format!("literal:{}", literal_encode(&json!({ "a": "x-y" }))),
    ]);
}

/// Control: canonical SDK forms and non-literal targets already round-trip.
#[test]
fn canonical_and_non_literal_targets_verify_after_a_read_back() {
    assert_round_trip(&[
        "literal:string:Write%20the%20guide".to_string(),
        "literal:json:%7B%22a%22%3A1%7D".to_string(),
        "literal:number:42".to_string(),
        "literal:boolean:true".to_string(),
        "did:key:z6MkExample".to_string(),
        "bots://body".to_string(),
    ]);
}

/// The SHACL links `add_sdna` writes through `parse_shacl_to_links` (setter,
/// adder, constructor, interpretation-hint JSON under `literal:string:`)
/// verify after a read-back.
#[tokio::test]
async fn shacl_add_sdna_links_verify_after_a_read_back() {
    let (perspective, _, _) =
        setup_perspective_no_llm(&[("Belief", BELIEF_SDNA), ("Task", TASK_WITH_RELATION_SDNA)])
            .await;
    let store = &perspective.sparql_store;
    assert!(
        store
            .get_all_links()
            .unwrap()
            .iter()
            .any(|l| l.data.predicate.as_deref() == Some("ad4m://setter")),
        "setup: add_sdna wrote setter links"
    );
    assert_eq!(not_verified_after_read_back(store), Vec::<String>::new());
}
