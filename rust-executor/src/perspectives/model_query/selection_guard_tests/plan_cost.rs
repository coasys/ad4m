//! What a kind's read costs on a store where other kinds share its predicates.
//!
//! The case the instance query was slow on: every record of every kind carries
//! a flag and a title, so a pattern on a predicate alone, or on the reifier's
//! `proofValid`, matches far more than the kind asked for. Oxigraph plans by how
//! many terms a pattern binds, with no statistics, and in one query holding the
//! membership check, the predicate list and the guard it started from those and
//! joined the ids last. The plan now asks for the ids first and binds the rest
//! as variables (see `annotation_is`, `predicate_in_filter`, `ids_query_of`).

use super::{add, signed};
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::test_helpers::execute_model_query_from_json;
use crate::perspectives::model_query::types::ModelQueryInput;
use crate::perspectives::sparql_store::{EvalStop, SparqlStore};
use serde_json::{json, Value};

const TASK_SHAPE_JSON: &str = r#"{
    "className": "Task",
    "properties": {
        "kind": {"predicate":"pc://flag","required":true,"flag":true,"initial":"pc://task"},
        "title": {"predicate":"pc://title","required":true},
        "status": {"predicate":"pc://status","required":false},
        "priority": {"predicate":"pc://priority","required":false}
    }
}"#;

/// `tasks` tasks, and `notes` records of another kind with the same flag and
/// title predicates, all signed, under one container.
fn seeded(tasks: u32, notes: u32) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    let me = TestSigner::generate();
    for i in 0..tasks {
        let id = format!("pc://task-{i:05}");
        let second = i % 50;
        add(
            &store,
            [
                signed(&me, &id, "pc://flag", "pc://task", second),
                signed(
                    &me,
                    &id,
                    "pc://title",
                    &format!("literal:string:task {i}"),
                    second,
                ),
                signed(&me, &id, "pc://status", "literal:string:todo", second),
                signed(&me, &id, "pc://priority", "literal:string:high", second),
                signed(&me, "pc://root", "ad4m://has_child", &id, second),
            ],
        );
    }
    for i in 0..notes {
        let id = format!("pc://note-{i:05}");
        let second = i % 50;
        add(
            &store,
            [
                signed(&me, &id, "pc://flag", "pc://note", second),
                signed(
                    &me,
                    &id,
                    "pc://title",
                    &format!("literal:string:note {i}"),
                    second,
                ),
                signed(&me, &id, "pc://body", "literal:string:words", second),
            ],
        );
    }
    store
}

async fn tasks(store: &SparqlStore, query: Value) -> Vec<Value> {
    let query: ModelQueryInput = serde_json::from_value(query).unwrap();
    execute_model_query_from_json(store, "Task", &query, TASK_SHAPE_JSON)
        .await
        .unwrap()
        .instances
}

#[tokio::test]
async fn a_kind_read_returns_its_own_records_beside_kinds_sharing_its_predicates() {
    let store = seeded(40, 120);
    for query in [
        json!({}),
        json!({ "order": { "createdAt": "ASC" } }),
        json!({ "order": { "createdAt": "ASC" }, "limit": 10 }),
        json!({ "where": { "status": "todo" } }),
    ] {
        let got = tasks(&store, query.clone()).await;
        let expected = if query.get("limit").is_some() { 10 } else { 40 };
        assert_eq!(got.len(), expected, "{query}");
        for task in &got {
            assert!(
                task["id"].as_str().unwrap().starts_with("pc://task-"),
                "{query}: {task}"
            );
            assert_eq!(task["status"], "todo", "{query}: {task}");
            assert_eq!(task["priority"], "high", "{query}: {task}");
            assert!(
                task["title"].as_str().unwrap().starts_with("task "),
                "{query}: {task}"
            );
        }
    }
}

/// Median time of four reads on 1,500 tasks beside 6,000 notes, and the store queries each one
/// ran. Ignored: a timing is a measurement. Run with
/// `cargo test --release --lib -- --ignored --nocapture plan_cost_by_read`.
#[tokio::test]
#[ignore]
async fn plan_cost_by_read() {
    let store = seeded(1500, 6000);
    for (name, query, rows) in [
        ("unpaged", json!({}), 1500),
        (
            "createdAt page",
            json!({ "order": { "createdAt": "ASC" }, "limit": 50 }),
            50,
        ),
        (
            "where status",
            json!({ "where": { "status": "todo" } }),
            1500,
        ),
    ] {
        measure(&store, name, rows, || async {
            tasks(&store, query.clone()).await.len()
        })
        .await;
    }

    // The id phase a union reads per class: every id, ordered by `createdAt`.
    let (resolver, shape) =
        crate::perspectives::model_query::test_helpers::StaticShapeResolver::from_json(
            "Task",
            TASK_SHAPE_JSON,
        )
        .unwrap();
    let all = ModelQueryInput::default();
    measure(&store, "createdAt ids", 1500, || async {
        crate::perspectives::model_query::query::select_ids(
            &EvalStop::none(),
            &store,
            &shape,
            &all,
            &resolver,
            None,
        )
        .await
        .unwrap()
        .unwrap()
        .len()
    })
    .await;
}

/// Runs `read` seven times, checks it returns `rows` rows, and prints the store queries of the
/// first run and the median time.
async fn measure<F, Fut>(store: &SparqlStore, name: &str, rows: usize, read: F)
where
    F: Fn() -> Fut,
    Fut: std::future::Future<Output = usize>,
{
    let mut runs = Vec::new();
    for run in 0..7 {
        let before = store.queries_run().len();
        let start = std::time::Instant::now();
        assert_eq!(read().await, rows, "{name}");
        runs.push(start.elapsed());
        if run == 0 {
            for q in &store.queries_run()[before..] {
                let shown = q.split_whitespace().collect::<Vec<_>>().join(" ");
                eprintln!("SPARQL [{name}]: {}", &shown[..shown.len().min(1500)]);
            }
        }
    }
    runs.sort();
    eprintln!("MEDIAN [{name}]: {:?}", runs[runs.len() / 2]);
}

/// A record with an optional title only: under a parent scope, nothing but the parent link
/// selects it, so the id query is all that tells an instance from a literal target.
const ITEM_SHAPE_JSON: &str = r#"{
    "className": "Item",
    "properties": {
        "title": {"predicate":"pc://title","required":false}
    }
}"#;

/// A parent holding one record and one literal whose text is the address of another node with a
/// title: the literal is no member, and the node it names is not read in its place, whichever
/// read asks for the ids (the unpaged read, a `createdAt` page, a union's creation-time order).
#[tokio::test]
async fn a_literal_target_is_not_read_as_the_node_its_text_names() {
    let store = SparqlStore::new(None).unwrap();
    let me = TestSigner::generate();
    add(
        &store,
        [
            signed(&me, "pc://parent", "pc://child", "pc://item", 1),
            signed(&me, "pc://item", "pc://title", "literal:string:held", 1),
            signed(
                &me,
                "pc://parent",
                "pc://child",
                "literal:string:pc://other",
                2,
            ),
            signed(
                &me,
                "pc://other",
                "pc://title",
                "literal:string:not held",
                2,
            ),
        ],
    );
    let scope = json!({ "id": "pc://parent", "predicate": "pc://child" });
    for query in [
        json!({ "parent": scope }),
        json!({ "parent": scope, "order": { "createdAt": "ASC" }, "limit": 10 }),
    ] {
        let input: ModelQueryInput = serde_json::from_value(query.clone()).unwrap();
        let got = execute_model_query_from_json(&store, "Item", &input, ITEM_SHAPE_JSON)
            .await
            .unwrap()
            .instances;
        let ids: Vec<&str> = got.iter().filter_map(|i| i["id"].as_str()).collect();
        assert_eq!(ids, ["pc://item"], "{query}: {got:?}");
    }
}

/// The form of the kind read, pinned: the ids are asked for apart from their links, and the
/// links are then bound to the ids found, whether the read is unpaged or a union's creation-time
/// order. The membership check is not repeated beside the links.
#[test]
fn a_kind_read_asks_for_its_ids_apart_from_their_links() {
    use crate::perspectives::model_query::sparql_builder::{
        build_instance_sparql, created_at_query, ids_query,
    };
    use crate::perspectives::model_query::types::InstanceQueryPlan;
    let (_resolver, shape) =
        crate::perspectives::model_query::test_helpers::StaticShapeResolver::from_json(
            "Task",
            TASK_SHAPE_JSON,
        )
        .unwrap();
    let all = ModelQueryInput::default();
    let ids = ids_query(&shape, &all, None).expect("a checked kind has an id query");
    assert!(ids.contains("?source <pc://flag> <pc://task>"), "{ids}");
    assert!(!ids.contains("?predicate"), "no link in the ids: {ids}");
    assert!(ids.contains("FILTER(isIRI(?source))"), "{ids}");

    let InstanceQueryPlan::TwoPhase {
        pagination_subquery,
        ..
    } = build_instance_sparql(&shape, &all, None, None)
    else {
        panic!("an unpaged read of a checked kind is its ids, then their links");
    };
    assert_eq!(pagination_subquery, ids);

    let created_at = created_at_query(&shape, &all, None, Some(&["pc://task-00001".to_string()]));
    assert!(
        created_at.contains("VALUES ?source { <pc://task-00001> }")
            && created_at.contains("?source ?predicate ?target")
            && !created_at.contains("?source <pc://flag> <pc://task>"),
        "the links are bound to the ids found, not the membership check: {created_at}"
    );
}
