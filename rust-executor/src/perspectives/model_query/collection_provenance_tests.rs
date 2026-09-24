//! Per-item provenance for collections (#1115, #1046 §6).
//!
//! A `HasMany` hydrates to a plain array of targets, so "who added this member,
//! when, and does their signature hold" cannot be read off the instance. Each
//! member is one link with its own reifier, and `links: ["<relation>"]` already
//! returns that link per member with its author and timestamp (#1112). What it
//! did not carry is the store's signature verdict, so a consumer that must not
//! act on an unverified claim still had to bypass `model_query`.
//!
//! The invariant these tests pin: every `__links` row carries
//! `proof.valid` / `proof.invalid` exactly as the store recorded them at insert
//! time, and the row deserializes as a [`DecoratedLinkExpression`], the same
//! shape `perspective.get` returns. The hydrated collection is untouched.

use super::test_helpers::execute_model_query_from_json;
use super::types::ModelQueryInput;
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkStatus};
use chrono::{DateTime, Utc};
use serde_json::{json, Value};

const TEAM_SHAPE_JSON: &str = r#"{
    "className": "Team",
    "properties": {
        "type": {
            "predicate": "ad4m://type",
            "required": true,
            "flag": true,
            "initial": "team://Team"
        }
    },
    "relations": {
        "members": {
            "predicate": "team://member"
        }
    }
}"#;

const TEAM: &str = "team://1";
const T0: &str = "2026-09-01T10:00:00.000Z";
const T1: &str = "2026-09-01T11:00:00.000Z";
const T2: &str = "2026-09-01T12:00:00.000Z";
const T3: &str = "2026-09-01T13:00:00.000Z";

fn at(ts: &str) -> DateTime<Utc> {
    ts.parse().unwrap()
}

/// A link `signer` really signed, stated at `ts`, stored Shared.
fn signed(signer: &TestSigner, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    let data = Link {
        source: TEAM.to_string(),
        predicate: Some(predicate.to_string()),
        target: target.to_string(),
    };
    let mut link: LinkExpression = signer.sign_at(data, at(ts)).into();
    link.status = Some(LinkStatus::Shared);
    link
}

fn members_rows(result: &super::types::ModelQueryResult) -> Vec<Value> {
    let inst = &result.instances[0];
    inst["__links"]["members"]
        .as_array()
        .unwrap_or_else(|| panic!("per-item rows for `members`: {inst}"))
        .clone()
}

/// Two members added by two different agents at different times, both
/// correctly signed, and a third whose signature does not verify: each row
/// reports its own author, timestamp and verdict.
#[tokio::test]
async fn each_collection_member_carries_its_own_author_timestamp_and_verdict() {
    let store = SparqlStore::new(None).unwrap();
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();

    store
        .add_link(&signed(&alice, "ad4m://type", "team://Team", T0))
        .unwrap();
    store
        .add_link(&signed(&alice, "team://member", "did:key:zCarol", T1))
        .unwrap();
    store
        .add_link(&signed(&bob, "team://member", "did:key:zDave", T2))
        .unwrap();
    // Claims to be Bob's, but the signature is over a different target.
    let mut forged = signed(&bob, "team://member", "did:key:zEve", T3);
    forged.proof.signature = signed(&bob, "team://member", "did:key:zOther", T3)
        .proof
        .signature;
    store.add_link(&forged).unwrap();

    let query = ModelQueryInput {
        links: Some(vec!["members".to_string()]),
        ..Default::default()
    };
    let result = execute_model_query_from_json(&store, "Team", &query, TEAM_SHAPE_JSON)
        .await
        .unwrap();

    // The plain array stays as it is.
    let inst = &result.instances[0];
    let mut members: Vec<&str> = inst["members"]
        .as_array()
        .unwrap()
        .iter()
        .map(|m| m.as_str().unwrap())
        .collect();
    members.sort();
    assert_eq!(
        members,
        vec!["did:key:zCarol", "did:key:zDave", "did:key:zEve"]
    );

    let rows = members_rows(&result);
    let seen: Vec<(&str, &str, &str, &Value, &Value)> = rows
        .iter()
        .map(|r| {
            (
                r["data"]["target"].as_str().unwrap(),
                r["timestamp"].as_str().unwrap(),
                r["author"].as_str().unwrap(),
                &r["proof"]["valid"],
                &r["proof"]["invalid"],
            )
        })
        .collect();
    assert_eq!(
        seen,
        vec![
            (
                "did:key:zCarol",
                T1,
                alice.did.as_str(),
                &json!(true),
                &json!(false)
            ),
            (
                "did:key:zDave",
                T2,
                bob.did.as_str(),
                &json!(true),
                &json!(false)
            ),
            (
                "did:key:zEve",
                T3,
                bob.did.as_str(),
                &json!(false),
                &json!(true)
            ),
        ],
        "each member: its own author, timestamp and stored verdict"
    );

    // The row is the store's decorated link, verdict included.
    for row in &rows {
        let decorated: DecoratedLinkExpression = serde_json::from_value(row.clone())
            .unwrap_or_else(|e| panic!("row is a DecoratedLinkExpression ({e}): {row}"));
        let as_link = LinkExpression {
            author: decorated.author.clone(),
            timestamp: decorated.timestamp.clone(),
            data: decorated.data.clone(),
            proof: crate::types::ExpressionProof {
                key: decorated.proof.key.clone(),
                signature: decorated.proof.signature.clone(),
            },
            status: Some(LinkStatus::Shared),
        };
        assert_eq!(
            decorated.proof.valid,
            Some(as_link.compute_proof_valid()),
            "the stored verdict agrees with the signature: {row}"
        );
    }
}

/// A row whose stored annotation is missing reads `valid: false`, never
/// "unsigned but valid". A link stored without a proof is the reachable case:
/// the store records `false` for it.
#[tokio::test]
async fn an_unsigned_member_reads_as_not_valid() {
    let store = SparqlStore::new(None).unwrap();
    let alice = TestSigner::generate();
    store
        .add_link(&signed(&alice, "ad4m://type", "team://Team", T0))
        .unwrap();
    let mut unsigned = signed(&alice, "team://member", "did:key:zCarol", T1);
    unsigned.proof.key = String::new();
    unsigned.proof.signature = String::new();
    store.add_link(&unsigned).unwrap();

    let query = ModelQueryInput {
        links: Some(vec!["members".to_string()]),
        ..Default::default()
    };
    let result = execute_model_query_from_json(&store, "Team", &query, TEAM_SHAPE_JSON)
        .await
        .unwrap();
    let rows = members_rows(&result);
    assert_eq!(rows.len(), 1);
    assert_eq!(
        rows[0]["proof"],
        json!({ "key": "", "signature": "", "valid": false, "invalid": true })
    );
}
