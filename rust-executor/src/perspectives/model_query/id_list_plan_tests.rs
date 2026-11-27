//! A read by a list of ids seeks each id, however long the list is, when every id is an IRI. An id
//! that is not (a bare string) is matched by its text instead, and that read still scans.
//!
//! The instance query used to bind the shape's predicates with
//! `VALUES ?predicate`. Oxigraph orders joins greedily from the pattern it
//! estimates smallest and estimates a `VALUES` by its row count, so once the
//! id list held more ids than the shape has predicates, it started from the
//! predicates and read every link of the class before joining the ids. Here
//! the shape has three predicates: four ids were enough.
//!
//! Timed against a read of the whole class on the same store, so the bound
//! holds on any machine. That read hydrates every instance; the id read
//! hydrates 40 of 3,000 and, when it scanned, cost about as much.

use super::test_helpers::execute_model_query_from_json;
use super::types::ModelQueryInput;
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};
use std::time::{Duration, Instant};

const SHAPE: &str = r#"{
    "className": "Note",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"idl://Note"},
        "title": {"predicate":"idl://title","required":false},
        "body": {"predicate":"idl://body","required":false}
    }
}"#;

const INSTANCES: u32 = 3000;

fn signed(signer: &TestSigner, source: &str, predicate: &str, target: &str) -> LinkExpression {
    use chrono::TimeZone;
    let at = chrono::Utc
        .with_ymd_and_hms(2026, 11, 25, 12, 0, 0)
        .unwrap();
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        at,
    ));
    l.status = Some(LinkStatus::Shared);
    l
}

/// The fastest of five runs of `query`, each checked to answer the notes numbered `expect`, each
/// with its own title, and no other.
async fn fastest(store: &SparqlStore, query: Value, expect: &[u32]) -> Duration {
    let input: ModelQueryInput = serde_json::from_value(query).unwrap();
    let mut wanted: Vec<(String, String)> = expect
        .iter()
        .map(|i| (format!("idl://note/{i}"), format!("t{i}")))
        .collect();
    wanted.sort();
    let mut best = Duration::MAX;
    for _ in 0..5 {
        let started = Instant::now();
        let got = execute_model_query_from_json(store, "Note", &input, SHAPE)
            .await
            .unwrap();
        best = best.min(started.elapsed());
        let mut answered: Vec<(String, String)> = got
            .instances
            .iter()
            .map(|inst| {
                (
                    inst["id"].as_str().unwrap_or_default().to_string(),
                    inst["title"].as_str().unwrap_or_default().to_string(),
                )
            })
            .collect();
        answered.sort();
        assert_eq!(answered, wanted);
    }
    best
}

#[tokio::test]
async fn test_perf_id_list_read_seeks_each_id() {
    let store = SparqlStore::new(None).unwrap();
    let signer = TestSigner::generate();
    for i in 0..INSTANCES {
        let note = format!("idl://note/{i}");
        for (p, o) in [
            ("ad4m://type", "idl://Note".to_string()),
            ("idl://title", format!("literal:string:t{i}")),
            ("idl://body", format!("literal:string:b{i}")),
        ] {
            store.add_link(&signed(&signer, &note, p, &o)).unwrap();
        }
    }

    let picked: Vec<u32> = (0..40).map(|i| i * 7).collect();
    let ids: Vec<String> = picked.iter().map(|i| format!("idl://note/{i}")).collect();
    let by_ids = fastest(&store, json!({ "where": { "id": ids } }), &picked).await;
    let all: Vec<u32> = (0..INSTANCES).collect();
    let whole = fastest(&store, json!({}), &all).await;
    eprintln!("40 ids: {by_ids:?}, whole class of {INSTANCES}: {whole:?}");
    assert!(
        by_ids * 10 < whole,
        "a read of 40 ids ({by_ids:?}) cost more than a tenth of a read of all \
         {INSTANCES} instances ({whole:?}): the id list is not the plan's start"
    );
}
