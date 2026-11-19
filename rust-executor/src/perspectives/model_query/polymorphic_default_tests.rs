//! A relation's declared `polymorphic` is carried in its SHACL shape, so the
//! executor applies it as the default of an include at every depth, and an
//! untyped include without `polymorphic` is refused whatever the data holds.

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

/// `Placement.node` and `TaskBlock.extractedFrom` are untyped; `node_poly` and
/// `extracted_poly` say whether each declares `polymorphic`.
fn shapes(node_poly: bool, extracted_poly: bool) -> (StaticShapeResolver, Arc<ModelShape>) {
    let (resolver, placement) = StaticShapeResolver::from_json(
        "Placement",
        &format!(
            r#"{{"className":"Placement","properties":{{
                 "flag":{{"predicate":"we://flag","required":true,"flag":true,"initial":"we://placement"}}
               }},"relations":{{
                 "node":{{"predicate":"we://placed_node","kind":"hasOne","maxCount":1,"targetClassName":"","polymorphic":{node_poly}}}
               }}}}"#
        ),
    )
    .unwrap();
    resolver.register(
        "TaskBlock",
        parse_shape_from_json(
            &format!(
                r#"{{"className":"TaskBlock","properties":{{
                     "flag":{{"predicate":"we://flag","required":true,"flag":true,"initial":"we://task_block"}}
                   }},"relations":{{
                     "extractedFrom":{{"predicate":"we://extracted","kind":"belongsToOne","direction":"reverse","targetClassName":"","polymorphic":{extracted_poly}}}
                   }}}}"#
            ),
            "TaskBlock",
        )
        .unwrap(),
    );
    resolver.register(
        "CollectionBlock",
        parse_shape_from_json(
            r#"{"className":"CollectionBlock","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://collection_block"}
               },"relations":{}}"#,
            "CollectionBlock",
        )
        .unwrap(),
    );
    resolver.register(
        "EdgeRoute",
        parse_shape_from_json(
            r#"{"className":"EdgeRoute","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://edge_route"}
               },"relations":{}}"#,
            "EdgeRoute",
        )
        .unwrap(),
    );
    (resolver, placement)
}

/// One placement of a task; `extracted` says whether a collection was extracted
/// from the task.
fn world(extracted: bool) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for uri in [
        "we://models/Placement",
        "we://models/TaskBlock",
        "we://models/CollectionBlock",
        "we://models/EdgeRoute",
    ] {
        store
            .add_link(&make_link(uri, "rdf://type", "ad4m://SubjectClass", "1"))
            .unwrap();
    }
    for (s, p, t) in [
        ("we://p/1", "we://flag", "we://placement"),
        ("we://p/1", "we://placed_node", "we://task/1"),
        ("we://task/1", "we://flag", "we://task_block"),
    ] {
        store.add_link(&make_link(s, p, t, "2")).unwrap();
    }
    if extracted {
        store
            .add_link(&make_link(
                "we://c/1",
                "we://flag",
                "we://collection_block",
                "3",
            ))
            .unwrap();
        store
            .add_link(&make_link("we://c/1", "we://extracted", "we://task/1", "3"))
            .unwrap();
    }
    store
}

fn q(v: Value) -> ModelQueryInput {
    serde_json::from_value(v).unwrap()
}

fn node(result: &ModelQueryResult) -> &Value {
    &result.instances[0]["node"]
}

fn assert_collection(task: &Value) {
    let from = &task["extractedFrom"];
    assert_eq!(from["id"], "we://c/1", "extractedFrom hydrated: {task}");
    assert_eq!(from["__subjectClass"], "CollectionBlock");
}

#[tokio::test]
async fn declared_polymorphic_applies_below_a_polymorphic_include() {
    let (resolver, placement) = shapes(true, true);
    let store = world(true);
    let query = q(json!({ "include": { "node": { "polymorphic": true,
        "include": { "extractedFrom": true } } } }));
    let result = fixture_query(&store, placement.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert_collection(node(&result));
}

#[tokio::test]
async fn declared_polymorphic_applies_to_the_include_itself() {
    let (resolver, placement) = shapes(true, true);
    let store = world(true);
    let query = q(json!({ "include": { "node": { "include": { "extractedFrom": true } } } }));
    let result = fixture_query(&store, placement.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert_collection(node(&result));
}

#[tokio::test]
async fn declared_polymorphic_applies_through_a_union() {
    for node_wire in [
        json!({ "polymorphic": true, "include": { "extractedFrom": true } }),
        json!({ "include": { "extractedFrom": true } }),
    ] {
        let (resolver, _) = shapes(true, true);
        let store = world(true);
        let mut query = q(json!({ "include": { "node": node_wire } }));
        query.include_unverified = Some(true);
        let classes = vec!["Placement".to_string(), "EdgeRoute".to_string()];
        let result = super::union::execute_union_query(&store, &classes, &query, &resolver)
            .await
            .unwrap();
        let row = result
            .instances
            .iter()
            .find(|r| r["id"] == "we://p/1")
            .unwrap();
        assert_collection(&row["node"]);
    }
}

#[tokio::test]
async fn untyped_include_is_refused_with_or_without_data() {
    for extracted in [true, false] {
        let (resolver, placement) = shapes(true, false);
        let store = world(extracted);
        let query = q(json!({ "include": { "node": { "polymorphic": true,
            "include": { "extractedFrom": true } } } }));
        let err = fixture_query(&store, placement.as_ref(), &query, &resolver)
            .await
            .expect_err("untyped, not polymorphic: refused");
        let msg = err.to_string();
        assert!(
            msg.contains("extractedFrom") && msg.contains("polymorphic"),
            "{msg}"
        );
    }
}

#[tokio::test]
async fn untyped_include_is_refused_without_targets_directly() {
    // No placement links at all: the relation holds no targets.
    let (resolver, placement) = shapes(false, false);
    let store = SparqlStore::new(None).unwrap();
    store
        .add_link(&make_link("we://p/9", "we://flag", "we://placement", "2"))
        .unwrap();
    let query = q(json!({ "include": { "node": true } }));
    let err = fixture_query(&store, placement.as_ref(), &query, &resolver)
        .await
        .expect_err("refused on an empty relation too");
    assert!(err.to_string().contains("polymorphic"), "{err}");
}

#[tokio::test]
async fn explicit_false_beats_the_declared_default() {
    let (resolver, placement) = shapes(true, true);
    let store = world(true);
    let query = q(json!({ "include": { "node": { "polymorphic": false } } }));
    let err = fixture_query(&store, placement.as_ref(), &query, &resolver)
        .await
        .expect_err("explicit false wins");
    assert!(err.to_string().contains("polymorphic"), "{err}");
}

#[tokio::test]
async fn declared_polymorphic_refuses_limit_without_the_caller_writing_it() {
    let (resolver, placement) = shapes(true, true);
    let store = world(true);
    let query = q(json!({ "include": { "node": { "limit": 1 } } }));
    let err = fixture_query(&store, placement.as_ref(), &query, &resolver)
        .await
        .expect_err("limit on a polymorphic include");
    assert!(err.to_string().contains("limit"), "{err}");
}

#[test]
fn shape_relation_reads_declared_polymorphic() {
    let (_, placement) = shapes(true, false);
    assert!(placement.include_relations[0].polymorphic);
    let (_, placement) = shapes(false, false);
    assert!(!placement.include_relations[0].polymorphic);
}
