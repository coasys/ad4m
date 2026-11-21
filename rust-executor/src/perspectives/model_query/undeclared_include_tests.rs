//! An include can read a link pointing at an instance whose class does not
//! declare it, by naming the predicate (`through`) and the kind.
//!
//! The case it is for: a conversation links to an action item it produced
//! (`we://extracted`), and the action item's class, written by some other app,
//! says nothing about conversations. Without `through` the include is skipped
//! and the action item reads as coming from nowhere.

use super::*;
use crate::perspectives::model_query::shape::parse_shape_from_json;
use crate::perspectives::model_query::test_helpers::StaticShapeResolver;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};
use std::sync::Arc;

fn make_link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
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

/// Fixture links carry no verifiable proof, so reads opt in to unverified rows.
async fn fixture_query(
    store: &SparqlStore,
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: &dyn super::types::ShapeResolver,
) -> Result<ModelQueryResult, deno_core::anyhow::Error> {
    let mut query = query.clone();
    query.include_unverified = Some(true);
    super::query::execute_model_query(store, shape, &query, resolver).await
}

/// `ActionItem` declares no relations at all; `TaskBlock` declares
/// `extractedFrom` the way a class that knows about conversations would.
fn shapes() -> (StaticShapeResolver, Arc<ModelShape>) {
    let (resolver, item) = StaticShapeResolver::from_json(
        "ActionItem",
        r#"{"className":"ActionItem","properties":{
             "flag":{"predicate":"bot://flag","required":true,"flag":true,"initial":"bot://action_item"}
           },"relations":{}}"#,
    )
    .unwrap();
    resolver.register(
        "TaskBlock",
        parse_shape_from_json(
            r#"{"className":"TaskBlock","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://task_block"}
               },"relations":{
                 "extractedFrom":{"predicate":"we://extracted","kind":"belongsToOne","direction":"reverse","targetClassName":"","polymorphic":true}
               }}"#,
            "TaskBlock",
        )
        .unwrap(),
    );
    resolver.register(
        "CollectionBlock",
        parse_shape_from_json(
            r#"{"className":"CollectionBlock","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://collection_block"},
                 "name":{"predicate":"we://name","required":false}
               },"relations":{}}"#,
            "CollectionBlock",
        )
        .unwrap(),
    );
    (resolver, item)
}

/// Two action items and a task; one call extracted the first item and the
/// task, another recognised the first item too. The second item was made by
/// hand: nothing links to it.
fn world() -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for uri in [
        "we://models/ActionItem",
        "we://models/TaskBlock",
        "we://models/CollectionBlock",
    ] {
        store
            .add_link(&make_link(uri, "rdf://type", "ad4m://SubjectClass", "1"))
            .unwrap();
    }
    for (s, p, t) in [
        ("bot://item/1", "bot://flag", "bot://action_item"),
        ("bot://item/2", "bot://flag", "bot://action_item"),
        ("we://task/1", "we://flag", "we://task_block"),
        ("we://c/1", "we://flag", "we://collection_block"),
        ("we://c/1", "we://name", "literal://string:Monday"),
        ("we://c/1", "we://extracted", "bot://item/1"),
        ("we://c/1", "we://extracted", "we://task/1"),
        ("we://c/2", "we://flag", "we://collection_block"),
        ("we://c/2", "we://recognised", "bot://item/1"),
    ] {
        store.add_link(&make_link(s, p, t, "2")).unwrap();
    }
    store
}

fn q(v: Value) -> ModelQueryInput {
    serde_json::from_value(v).unwrap()
}

fn by_id<'a>(result: &'a ModelQueryResult, id: &str) -> &'a Value {
    result
        .instances
        .iter()
        .find(|r| r["id"] == id)
        .unwrap_or_else(|| panic!("no {id} in {:?}", result.instances))
}

#[tokio::test]
async fn an_undeclared_reverse_link_is_read_through_the_predicate_it_names() {
    let (resolver, item) = shapes();
    let store = world();
    let query = q(json!({ "include": { "extractedFrom": {
        "through": "we://extracted", "kind": "belongsToOne" } } }));
    let result = fixture_query(&store, item.as_ref(), &query, &resolver)
        .await
        .unwrap();
    let from = &by_id(&result, "bot://item/1")["extractedFrom"];
    assert_eq!(from["id"], "we://c/1", "{from}");
    // Hydrated as the class it is, since the reader names none.
    assert_eq!(from["__subjectClass"], "we://models/CollectionBlock");
    // With the fields of that class, which the reading class knows nothing about.
    assert!(
        from["name"]
            .as_str()
            .is_some_and(|name| name.ends_with("Monday")),
        "{from}"
    );
    // Nothing links to it: an absent link reads as none, not as a missing key.
    assert_eq!(by_id(&result, "bot://item/2")["extractedFrom"], Value::Null);
}

#[tokio::test]
async fn without_through_an_undeclared_include_is_still_skipped() {
    let (resolver, item) = shapes();
    let store = world();
    let query = q(json!({ "include": { "extractedFrom": true } }));
    let result = fixture_query(&store, item.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert!(by_id(&result, "bot://item/1")
        .get("extractedFrom")
        .is_none());
}

#[tokio::test]
async fn belongs_to_many_reads_every_source() {
    let (resolver, item) = shapes();
    let store = world();
    let query = q(json!({ "include": { "seenIn": {
        "through": "we://recognised", "kind": "belongsToMany" } } }));
    let result = fixture_query(&store, item.as_ref(), &query, &resolver)
        .await
        .unwrap();
    let seen = by_id(&result, "bot://item/1")["seenIn"].as_array().unwrap();
    assert_eq!(seen.len(), 1);
    assert_eq!(seen[0]["id"], "we://c/2");
    assert_eq!(by_id(&result, "bot://item/2")["seenIn"], json!([]));
}

#[tokio::test]
async fn through_reads_the_same_link_in_a_union_over_both_kinds_of_class() {
    let (resolver, _) = shapes();
    let store = world();
    let mut query = q(json!({ "include": { "extractedFrom": {
        "through": "we://extracted", "kind": "belongsToOne" } } }));
    query.include_unverified = Some(true);
    let classes = vec!["ActionItem".to_string(), "TaskBlock".to_string()];
    let result = super::union::execute_union_query(&store, &classes, &query, &resolver)
        .await
        .unwrap();
    assert_eq!(
        by_id(&result, "bot://item/1")["extractedFrom"]["id"],
        "we://c/1"
    );
    assert_eq!(
        by_id(&result, "we://task/1")["extractedFrom"]["id"],
        "we://c/1"
    );
}

#[tokio::test]
async fn a_declared_relation_wins_and_a_different_predicate_is_refused() {
    let (resolver, _) = shapes();
    let store = world();
    let task = resolver.get_shape("TaskBlock").unwrap();
    let same = q(json!({ "include": { "extractedFrom": {
        "through": "we://extracted", "kind": "belongsToOne" } } }));
    let result = fixture_query(&store, task.as_ref(), &same, &resolver)
        .await
        .unwrap();
    assert_eq!(
        by_id(&result, "we://task/1")["extractedFrom"]["id"],
        "we://c/1"
    );

    let other = q(json!({ "include": { "extractedFrom": {
        "through": "we://recognised", "kind": "belongsToOne" } } }));
    let err = fixture_query(&store, task.as_ref(), &other, &resolver)
        .await
        .expect_err("two readings of one name");
    assert!(err.to_string().contains("we://extracted"), "{err}");
}

#[tokio::test]
async fn through_needs_a_reverse_kind() {
    let (resolver, item) = shapes();
    let store = world();
    for (kind, says) in [
        (None, "needs a `kind`"),
        (Some("hasMany"), "`links`"),
        (Some("sideways"), "must be"),
    ] {
        let mut include = json!({ "through": "we://extracted" });
        if let Some(kind) = kind {
            include["kind"] = json!(kind);
        }
        let query = q(json!({ "include": { "extractedFrom": include } }));
        let err = fixture_query(&store, item.as_ref(), &query, &resolver)
            .await
            .expect_err("refused");
        assert!(err.to_string().contains(says), "{kind:?}: {err}");
    }
}

#[tokio::test]
async fn through_must_be_a_predicate_iri() {
    let (resolver, item) = shapes();
    let store = world();
    let query = q(json!({ "include": { "extractedFrom": {
        "through": "we://extracted> } UNION { ?s ?p ?o", "kind": "belongsToOne" } } }));
    let err = fixture_query(&store, item.as_ref(), &query, &resolver)
        .await
        .expect_err("not an IRI");
    assert!(err.to_string().contains("predicate IRI"), "{err}");
}
