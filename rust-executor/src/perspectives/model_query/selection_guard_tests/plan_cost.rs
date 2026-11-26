//! What a kind's read costs on a store where other kinds share its predicates.
//!
//! The case the instance query was slow on: every record of every kind carries
//! a flag and a title, so a pattern on a predicate alone, or on the reifier's
//! `proofValid`, matches far more than the kind asked for. Oxigraph plans by how
//! many terms a pattern binds, with no statistics, and in one query holding the
//! membership check, the predicate list and the guard it started from those and
//! joined the ids last. The plan now asks for the ids first and binds the rest
//! as variables (see `annotation_is`, `predicate_in_filter`,
//! `InstanceQueryPlan::IdsThenLinks`, `IdsFirst`).

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

/// The links of every task, as one query in the form the builder wrote before:
/// the membership check, the predicate list as `VALUES` and the guard as
/// constants, all together.
fn old_single_query() -> String {
    let r = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
    format!(
        "SELECT DISTINCT ?source ?predicate ?target ?author ?timestamp WHERE {{
    ?source <pc://flag> <pc://task> . ?_r1 <{r}> <<( ?source <pc://flag> <pc://task> )>> . ?_r1 <ad4m://ontology/proofValid> \"true\" .
    ?source <pc://title> ?_cf . ?_r2 <{r}> <<( ?source <pc://title> ?_cf )>> . ?_r2 <ad4m://ontology/proofValid> \"true\" .
    VALUES ?predicate {{ <pc://flag> <pc://priority> <pc://status> <pc://title> }}
    ?source ?predicate ?target .
    ?_reifier <{r}> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
    ?_reifier <ad4m://ontology/proofValid> \"true\" .
}}"
    )
}

/// Times a kind's read against the form it replaced, on 1,500 tasks beside
/// 6,000 records of another kind. Ignored: a timing is a measurement, not an
/// assertion CI can hold. Run with
/// `cargo test --release --lib -- --ignored --nocapture plan_cost_against_the_old_form`.
#[tokio::test]
#[ignore]
async fn plan_cost_against_the_old_form() {
    let store = seeded(1500, 6000);
    let stop = EvalStop::none();
    let median = |mut runs: Vec<std::time::Duration>| {
        runs.sort();
        runs[runs.len() / 2]
    };

    let mut old = Vec::new();
    let mut old_rows = 0;
    for _ in 0..5 {
        let start = std::time::Instant::now();
        let rows: Vec<Value> =
            serde_json::from_str(&store.query(&stop, &old_single_query()).unwrap()).unwrap();
        old.push(start.elapsed());
        old_rows = rows.len();
    }
    let mut new = Vec::new();
    for _ in 0..5 {
        let start = std::time::Instant::now();
        let got = tasks(&store, json!({})).await;
        new.push(start.elapsed());
        assert_eq!(got.len(), 1500);
    }
    let (old, new) = (median(old), median(new));
    eprintln!(
        "plan cost, 1,500 tasks beside 6,000 notes: the old single query {old:?} ({old_rows} rows); \
         the whole read now, hydration included, {new:?}"
    );
    assert_eq!(old_rows, 6000, "four links a task");
    assert!(
        new < old,
        "the whole read now should beat the old query alone"
    );
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
/// plan reads the ids (the unpaged ids-then-links, the `createdAt` page's ids first).
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

/// The two plans the kind read now runs, pinned by their form: an unpaged read of a kind whose
/// membership is checked asks for the ids alone and then the links of those ids only; a
/// `createdAt` page asks for the ids first and puts them in place of the membership check.
#[test]
fn a_kind_read_asks_for_its_ids_apart_from_their_links() {
    use crate::perspectives::model_query::sparql_builder::{build_instance_sparql, links_of_ids};
    use crate::perspectives::model_query::types::{
        InstanceQueryPlan, OrderDirection, SortKey, SparqlPagination,
    };
    let (_resolver, shape) =
        crate::perspectives::model_query::test_helpers::StaticShapeResolver::from_json(
            "Task",
            TASK_SHAPE_JSON,
        )
        .unwrap();
    let all = ModelQueryInput::default();

    let InstanceQueryPlan::IdsThenLinks { ids_query, links } =
        build_instance_sparql(&shape, &all, None, None)
    else {
        panic!("an unpaged read of a checked kind reads its ids first");
    };
    assert!(
        !ids_query.contains("?predicate"),
        "the id query fetches no links: {ids_query}"
    );
    assert!(ids_query.contains("FILTER(isIRI(?source))"), "{ids_query}");
    let second = links_of_ids(&["pc://task-00001".to_string()], &links);
    assert!(
        second.contains("<pc://task-00001>") && !second.contains("<pc://flag> <pc://task>"),
        "the links query is bound to the ids found, not the membership check: {second}"
    );

    let page = SparqlPagination {
        sort_key: SortKey::CreatedAt,
        direction: OrderDirection::ASC,
        offset: None,
        limit: Some(10),
        keyset: None,
    };
    let InstanceQueryPlan::TwoPhase {
        pagination_subquery,
        ids_first: Some(first),
        ..
    } = build_instance_sparql(&shape, &all, Some(&page), None)
    else {
        panic!("a createdAt page of a checked kind reads its ids first");
    };
    assert!(
        !first.ids_query.contains("?predicate"),
        "{}",
        first.ids_query
    );
    assert!(
        first.ids_query.contains("FILTER(isIRI(?source))"),
        "{}",
        first.ids_query
    );
    assert!(
        pagination_subquery.contains(&first.replaces),
        "the page query holds the check the ids replace: {pagination_subquery}"
    );
}
