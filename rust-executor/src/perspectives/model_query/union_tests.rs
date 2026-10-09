//! One model query over several classes (#1238): what the union pages on,
//! what `where` and `order` mean for a class that lacks the key, and how a
//! record answered by two classes is returned.

use super::*;
use crate::perspectives::model_query::shape::parse_shape_from_json;
use crate::perspectives::model_query::test_helpers::StaticShapeResolver;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};

fn link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    LinkExpression {
        author: "did:key:test123".to_string(),
        timestamp: ts.to_string(),
        data: Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: ExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
        },
        status: Some(LinkStatus::Shared),
    }
}

fn add(store: &SparqlStore, source: &str, predicate: &str, target: &str, ts: &str) {
    store
        .add_link(&link(source, predicate, target, ts))
        .unwrap();
}

/// `Task { flag, rank, status }` and `Note { flag, rank, body }`: both declare
/// `rank`, only `Task` declares `status`, only `Note` declares `body`.
fn task_and_note() -> (SparqlStore, StaticShapeResolver) {
    let store = SparqlStore::new(None).unwrap();
    for uri in ["app://models/Task", "app://models/Note"] {
        add(&store, uri, "rdf://type", "ad4m://SubjectClass", "1");
    }
    let resolver = StaticShapeResolver::new();
    resolver.register(
        "Task",
        parse_shape_from_json(
            r#"{"className":"Task","properties":{
                "flag":{"predicate":"app://flag","required":true,"flag":true,"initial":"app://task"},
                "rank":{"predicate":"app://rank","resolveLanguage":"literal"},
                "status":{"predicate":"app://status","resolveLanguage":"literal"}
            },"relations":{}}"#,
            "Task",
        )
        .unwrap(),
    );
    resolver.register(
        "Note",
        parse_shape_from_json(
            r#"{"className":"Note","properties":{
                "flag":{"predicate":"app://flag","required":true,"flag":true,"initial":"app://note"},
                "rank":{"predicate":"app://rank","resolveLanguage":"literal"},
                "body":{"predicate":"app://body","resolveLanguage":"literal"}
            },"relations":{}}"#,
            "Note",
        )
        .unwrap(),
    );
    (store, resolver)
}

fn task(store: &SparqlStore, id: &str, rank: Option<i64>, status: &str, ts: &str) {
    add(store, id, "app://flag", "app://task", ts);
    if let Some(r) = rank {
        add(store, id, "app://rank", &format!("literal:number:{r}"), ts);
    }
    add(
        store,
        id,
        "app://status",
        &format!("literal:string:{status}"),
        ts,
    );
}

fn note(store: &SparqlStore, id: &str, rank: Option<i64>, ts: &str) {
    add(store, id, "app://flag", "app://note", ts);
    if let Some(r) = rank {
        add(store, id, "app://rank", &format!("literal:number:{r}"), ts);
    }
    add(store, id, "app://body", "literal:string:text", ts);
}

/// Fixture links carry no verifiable proof, so reads opt in to unverified rows.
fn query(json: serde_json::Value) -> ModelQueryInput {
    let mut q: ModelQueryInput = serde_json::from_value(json).unwrap();
    q.include_unverified = Some(true);
    q
}

fn classes(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

fn ids(result: &ModelQueryResult) -> Vec<&str> {
    result
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap())
        .collect()
}

/// A page of the union is cut after sorting both classes together. With the
/// ranks interleaved, a per-class page (or a page of one class followed by the
/// other) returns different rows.
#[tokio::test]
async fn union_pages_once_over_interleaved_sort_keys() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t1", Some(1), "open", "10");
    note(&store, "app://n2", Some(2), "11");
    task(&store, "app://t3", Some(3), "open", "12");
    note(&store, "app://n4", Some(4), "13");
    task(&store, "app://t5", Some(5), "done", "14");
    note(&store, "app://n6", Some(6), "15");

    let q = query(serde_json::json!({"order": {"rank": "ASC"}, "limit": 3, "offset": 1}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();

    assert_eq!(ids(&result), vec!["app://n2", "app://t3", "app://n4"]);
    assert_eq!(result.total_count, 6, "totalCount is the whole union");
    let class_of: Vec<&str> = result
        .instances
        .iter()
        .map(|i| i["__subjectClass"].as_str().unwrap())
        .collect();
    assert_eq!(class_of, vec!["Note", "Task", "Note"]);
    // Hydrated as its own class: the Note keeps `body`, the Task `status`.
    assert_eq!(result.instances[0]["body"], "text");
    assert_eq!(result.instances[1]["status"], "open");

    let desc = query(serde_json::json!({"order": {"rank": "DESC"}, "limit": 2}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &desc, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://n6", "app://t5"]);
}

/// `where` on a property only one class declares keeps that class's matching
/// rows and excludes the other class, as a per-class query would.
#[tokio::test]
async fn union_where_on_a_property_one_class_declares_excludes_the_other() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t1", Some(1), "open", "10");
    task(&store, "app://t2", Some(2), "done", "11");
    note(&store, "app://n3", Some(3), "12");

    let q = query(serde_json::json!({"where": {"status": "open"}}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://t1"]);
    assert_eq!(result.total_count, 1);
}

/// Rows without the order key — a class that does not declare it, or a record
/// that has no value — sort last in both directions, then by id. That is the
/// store's own `ORDER BY` rule, not `sort_instances`' (which puts them first
/// under `DESC`).
#[tokio::test]
async fn union_order_on_a_missing_property_sorts_nulls_last_both_ways() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t1", Some(1), "open", "10");
    note(&store, "app://n0", None, "11");
    task(&store, "app://t2", Some(2), "open", "12");
    note(&store, "app://n9", None, "13");

    for (dir, expected) in [
        ("ASC", ["app://t1", "app://t2", "app://n0", "app://n9"]),
        ("DESC", ["app://t2", "app://t1", "app://n0", "app://n9"]),
    ] {
        let q = query(serde_json::json!({"order": {"status": dir}}));
        let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
            .await
            .unwrap();
        // `status` ties for both tasks; the id breaks it.
        let got = ids(&result);
        assert_eq!(&got[2..], &expected[2..], "{dir}: Notes last, by id");
        let q = query(serde_json::json!({"order": {"rank": dir}}));
        let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
            .await
            .unwrap();
        assert_eq!(ids(&result), expected.to_vec(), "{dir} on rank");
    }
}

/// A record carrying both classes' required triples is returned once. Without
/// `preferClasses` the tie between unrelated classes breaks alphabetically, as
/// `subject_classes_of` ranks it; with it, the caller's class wins. Either way
/// `__subjectClasses` names both.
#[tokio::test]
async fn union_returns_a_record_of_two_classes_once_with_both_classes() {
    let (store, resolver) = task_and_note();
    // Both flags on one node: a Task and a Note at once.
    task(&store, "app://both", Some(1), "open", "10");
    add(&store, "app://both", "app://flag", "app://note", "10");
    add(
        &store,
        "app://both",
        "app://body",
        "literal:string:text",
        "10",
    );
    note(&store, "app://n2", Some(2), "11");

    let q = query(serde_json::json!({}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://both", "app://n2"]);
    assert_eq!(result.total_count, 2);
    let both = &result.instances[0];
    assert_eq!(both["__subjectClass"], "Note");
    let mut all: Vec<&str> = both["__subjectClasses"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect();
    all.sort();
    assert_eq!(all, vec!["Note", "Task"]);

    let q = query(serde_json::json!({"preferClasses": ["Task"]}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(result.instances[0]["__subjectClass"], "Task");
    assert_eq!(result.instances[0]["status"], "open");
}

/// Paging with no `order` uses creation time, then id — the single-class default.
#[tokio::test]
async fn union_pages_by_timestamp_when_no_order_is_given() {
    let (store, resolver) = task_and_note();
    note(&store, "app://n1", None, "10");
    task(&store, "app://t2", None, "open", "20");
    note(&store, "app://n3", None, "30");

    let q = query(serde_json::json!({"limit": 2}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://n1", "app://t2"]);

    let count = query(serde_json::json!({"limit": 0}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &count, &resolver)
        .await
        .unwrap();
    assert!(result.instances.is_empty());
    assert_eq!(result.total_count, 3);
}

/// What has no single meaning over several classes is refused, not approximated.
#[tokio::test]
async fn union_refuses_per_anchor_slicing_and_relation_order_keys() {
    let (store, resolver) = task_and_note();
    for q in [
        serde_json::json!({"parent": {"ids": ["app://p"], "predicate": "app://has", "limitPerAnchor": 2}}),
        serde_json::json!({"order": {"owner.name": "ASC"}}),
    ] {
        let err = execute_union_query(&store, &classes(&["Task", "Note"]), &query(q), &resolver)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("classNames"), "{err}");
    }
    let err = execute_union_query(&store, &[], &query(serde_json::json!({})), &resolver)
        .await
        .unwrap_err();
    assert!(err.to_string().contains("at least one class"), "{err}");
}

/// A record of two requested classes is kept when one of its readings passes
/// the query, and is read as that class: `where` on a property only `Task`
/// declares keeps a Task-and-Note, even when `preferClasses` names `Note`
/// (it ranks, never excludes). Review R1 / R1b on #1381.
#[tokio::test]
async fn union_where_keeps_a_two_class_record_that_passes_as_one_of_them() {
    let (store, resolver) = task_and_note();
    task(&store, "app://both", Some(1), "open", "10");
    add(&store, "app://both", "app://flag", "app://note", "10");
    note(&store, "app://n2", Some(2), "11");
    for q in [
        serde_json::json!({"where": {"status": "open"}}),
        serde_json::json!({"where": {"status": "open"}, "preferClasses": ["Note"]}),
    ] {
        let result = execute_union_query(&store, &classes(&["Task", "Note"]), &query(q), &resolver)
            .await
            .unwrap();
        assert_eq!(ids(&result), vec!["app://both"]);
        assert_eq!(result.instances[0]["__subjectClass"], "Task");
    }
}

/// A Local flag of a second class does not hide a Shared record from a shared
/// read: the read-as class is chosen among the readings that pass the read's
/// own link filters. Review R2 on #1381.
#[tokio::test]
async fn union_a_local_flag_does_not_hide_a_shared_record_from_a_shared_read() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t1", Some(1), "open", "10");
    let mut l = link("app://t1", "app://flag", "app://note", "11");
    l.status = Some(LinkStatus::Local);
    store.add_link(&l).unwrap();
    let q = query(serde_json::json!({"linkStatus": "shared"}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://t1"]);
    assert_eq!(result.instances[0]["__subjectClass"], "Task");
}

/// Rows that tie on the order key keep id order across classes, so a page
/// boundary between them does not move (#1227). Step 1 returns class by class,
/// Task first, so without the tie-break the Task would come first.
#[tokio::test]
async fn union_breaks_order_ties_by_id_across_classes() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t9", Some(1), "open", "10");
    note(&store, "app://n1", Some(1), "11");
    let q = query(serde_json::json!({"order": {"rank": "ASC"}}));
    let result = execute_union_query(&store, &classes(&["Task", "Note"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://n1", "app://t9"]);
}

/// A class named twice is read once: rows are not doubled and the total counts
/// each record once.
#[tokio::test]
async fn union_reads_a_class_named_twice_once() {
    let (store, resolver) = task_and_note();
    task(&store, "app://t1", Some(1), "open", "10");
    note(&store, "app://n2", Some(2), "11");
    let q = query(serde_json::json!({}));
    let result = execute_union_query(&store, &classes(&["Task", "Note", "Task"]), &q, &resolver)
        .await
        .unwrap();
    assert_eq!(ids(&result), vec!["app://t1", "app://n2"]);
    assert_eq!(result.total_count, 2);
}
