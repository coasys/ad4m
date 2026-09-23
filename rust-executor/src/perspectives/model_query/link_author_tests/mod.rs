//! `author` in a where clause, end to end through `execute_model_query` (#1114).
//!
//! The invariant these suites prove: a per-link `author` is a condition on the
//! **link that carries the property value**, not on the instance's earliest
//! link. See [`super::link_author`] for the three forms.
//!
//! This is the role-gating case from #1046 §1. A role query
//! `{ agent: <candidate>, author: <admin> }` used to be matched after
//! hydration, against the synthetic `author` field, which is the author of the
//! instance's *earliest* link. Admin created the role instance, anyone added
//! `agent -> did:mallory`, and the query accepted Mallory as a member admin
//! appointed. That fails open.
//!
//! - [`nested`]: `{ agent: { eq: X, author: A } }` and its operator forms.
//! - [`side_by_side`]: `{ agent: X, author: A }`, both halves, and the bare form.
//! - [`refusals`]: what the store cannot answer is an `Err`, never a guess.
//!
//! Every positive and negative runs through the single-phase, the paginated
//! and the COUNT plan and asserts they agree. Each negative that stands for the
//! forgery makes the forged link the **sole** link on its predicate, so no
//! ordering can pass it by luck.

mod nested;
mod refusals;
mod side_by_side;

use super::query::execute_model_query;
use super::shape::parse_shape_from_json;
use super::test_helpers::StaticShapeResolver;
use super::types::{ModelQueryInput, ModelQueryResult, ModelShape};
use super::utils::literal_percent_encode;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
use deno_core::anyhow::Error;
use serde_json::{json, Value};

const ADMIN: &str = "did:key:zAdmin";
const LEAD: &str = "did:key:zLead";
const MALLORY: &str = "did:key:zMallory";
const ALICE: &str = "did:key:zAlice";
const BOB: &str = "did:key:zBob";

/// `agent` as a literal property (a TS `@Property` holding a DID) and
/// `member` as a relation to the DID itself (a TS `@HasOne`/`@HasMany`).
/// A role query can name either one as its `didProperty`.
const REVIEWER_SHAPE_JSON: &str = r#"{
    "className": "Reviewer",
    "properties": {
        "role": { "predicate": "ns://role", "required": true, "flag": true, "initial": "ns://reviewer" },
        "agent": { "predicate": "ns://agent", "required": false, "resolveLanguage": "literal" },
        "note": { "predicate": "ns://note", "required": false, "resolveLanguage": "literal" }
    },
    "relations": {
        "member": { "predicate": "ns://member" }
    }
}"#;

const T0: &str = "2026-01-01T00:00:00.000Z";

fn link(author: &str, source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    LinkExpression {
        author: author.to_string(),
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

fn lit(value: &str) -> String {
    format!("literal:string:{}", literal_percent_encode(value))
}

/// Write one role instance: `creator` writes the class flag first (the earliest
/// link, so `creator` is the instance's hydrated `author`), then each
/// `(author, predicate, target)` follows one second apart.
fn role_instance(store: &SparqlStore, id: &str, creator: &str, links: &[(&str, &str, String)]) {
    store
        .add_link(&link(creator, id, "ns://role", "ns://reviewer", T0))
        .unwrap();
    for (i, (author, predicate, target)) in links.iter().enumerate() {
        let ts = format!("2026-01-01T00:00:{:02}.000Z", i + 1);
        store
            .add_link(&link(author, id, predicate, target, &ts))
            .unwrap();
    }
}

fn input(query: Value) -> ModelQueryInput {
    serde_json::from_value(query).expect("valid ModelQueryInput")
}

fn reviewer() -> (StaticShapeResolver, ModelShape) {
    let (resolver, shape) =
        StaticShapeResolver::from_json("Reviewer", REVIEWER_SHAPE_JSON).unwrap();
    (resolver, (*shape).clone())
}

/// The Reviewer shape plus `computed`, a getter-backed property. A getter
/// property has no predicate, so it is not a link condition. Shape JSON drops
/// predicate-less properties, so this one is added by hand, the way
/// `load_shape` builds it from SHACL.
fn reviewer_with_getter() -> (StaticShapeResolver, ModelShape) {
    let (_, mut shape) = reviewer();
    let mut computed = shape
        .properties
        .iter()
        .find(|p| p.name == "note")
        .unwrap()
        .clone();
    computed.name = "computed".to_string();
    computed.predicate = String::new();
    computed.getter = Some("SELECT ?target WHERE { <Base> <ns://note> ?target }".to_string());
    shape.properties.push(computed);
    let resolver = StaticShapeResolver::new();
    resolver.register("Reviewer", shape.clone());
    (resolver, shape)
}

async fn try_run(
    store: &SparqlStore,
    shape: &ModelShape,
    resolver: &StaticShapeResolver,
    query: Value,
) -> Result<ModelQueryResult, Error> {
    execute_model_query(store, shape, &input(query), resolver).await
}

async fn run(store: &SparqlStore, query: Value) -> ModelQueryResult {
    let (resolver, shape) = reviewer();
    try_run(store, &shape, &resolver, query)
        .await
        .expect("query should execute")
}

fn ids(result: &ModelQueryResult) -> Vec<String> {
    let mut ids: Vec<String> = result
        .instances
        .iter()
        .filter_map(|i| i["id"].as_str().map(str::to_string))
        .collect();
    ids.sort();
    ids
}

/// Run `where` on the Reviewer shape through every plan that answers it.
async fn ids_on_every_plan(store: &SparqlStore, where_clause: Value) -> Vec<String> {
    let (resolver, shape) = reviewer();
    ids_on_every_plan_of(store, &shape, &resolver, where_clause).await
}

/// Run `where` through every plan that answers it: the single-phase instance
/// query, the paginated two-phase plan, and the COUNT fast path. Each builds its
/// own SPARQL, and a condition enforced by only one of them is still laundered
/// by the others.
async fn ids_on_every_plan_of(
    store: &SparqlStore,
    shape: &ModelShape,
    resolver: &StaticShapeResolver,
    where_clause: Value,
) -> Vec<String> {
    let run = |query: Value| async move {
        try_run(store, shape, resolver, query)
            .await
            .expect("query should execute")
    };
    let single = run(json!({ "where": where_clause })).await;
    let paged = run(json!({ "where": where_clause, "limit": 10 })).await;
    let count = run(json!({ "where": where_clause, "limit": 0 })).await;
    assert_eq!(
        ids(&single),
        ids(&paged),
        "single-phase and paginated plans disagree on {where_clause}"
    );
    assert_eq!(
        count.total_count,
        ids(&single).len(),
        "COUNT disagrees with the rows for {where_clause}"
    );
    assert_eq!(
        single.total_count,
        ids(&single).len(),
        "totalCount disagrees with the rows for {where_clause}"
    );
    ids(&single)
}

/// Assert `where` is refused on every plan, and return the single-phase error.
async fn refused_on_every_plan_of(
    store: &SparqlStore,
    shape: &ModelShape,
    resolver: &StaticShapeResolver,
    where_clause: Value,
) -> String {
    let mut first = None;
    for extra in [json!({}), json!({ "limit": 10 }), json!({ "limit": 0 })] {
        let mut query = extra;
        query["where"] = where_clause.clone();
        let err = try_run(store, shape, resolver, query.clone())
            .await
            .expect_err(&format!("{query} must be refused"));
        first.get_or_insert(err.to_string());
    }
    first.unwrap()
}

async fn refused_on_every_plan(store: &SparqlStore, where_clause: Value) -> String {
    let (resolver, shape) = reviewer();
    refused_on_every_plan_of(store, &shape, &resolver, where_clause).await
}

/// `ns://r/both`: admin and the lead each wrote `agent -> alice`.
/// `ns://r/lead`: only the lead did. Admin created both instances.
fn agent_written_twice(store: &SparqlStore) {
    role_instance(
        store,
        "ns://r/both",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (LEAD, "ns://agent", lit(ALICE)),
        ],
    );
    role_instance(
        store,
        "ns://r/lead",
        ADMIN,
        &[(LEAD, "ns://agent", lit(ALICE))],
    );
}

/// Two tasks, each with one review. Admin wrote everything except the
/// `verdict` link: on `ns://t/forged`'s review Mallory wrote it.
fn task_review_fixture() -> (SparqlStore, ModelShape, StaticShapeResolver) {
    let store = SparqlStore::new(None).unwrap();
    let t1 = "2026-01-01T00:00:01.000Z";
    for (task, review, verdict_author) in [
        ("ns://t/honest", "ns://rv/honest", ADMIN),
        ("ns://t/forged", "ns://rv/forged", MALLORY),
    ] {
        for l in [
            link(ADMIN, task, "ns://type", "ns://task", T0),
            link(ADMIN, task, "ns://review", review, T0),
            link(ADMIN, review, "ns://type", "ns://review", T0),
            link(verdict_author, review, "ns://verdict", &lit("approved"), t1),
        ] {
            store.add_link(&l).unwrap();
        }
    }

    let task_shape = parse_shape_from_json(
        r#"{
            "className": "Task",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://task" }
            },
            "relations": {
                "reviews": { "predicate": "ns://review", "targetClassName": "Review" }
            }
        }"#,
        "Task",
    )
    .unwrap();
    let review_shape = parse_shape_from_json(
        r#"{
            "className": "Review",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://review" },
                "verdict": { "predicate": "ns://verdict", "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#,
        "Review",
    )
    .unwrap();
    let resolver = StaticShapeResolver::new();
    resolver.register("Task", task_shape.clone());
    resolver.register("Review", review_shape);
    (store, task_shape, resolver)
}
