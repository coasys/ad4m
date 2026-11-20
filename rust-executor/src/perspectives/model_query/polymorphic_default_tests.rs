//! A relation's declared `polymorphic` is carried in its SHACL shape, so the
//! executor applies it as the default of an include at every depth, and an
//! untyped include without `polymorphic` is refused whatever the data holds.

use super::*;
use crate::perspectives::model_query::shape::parse_shape_from_json;
use crate::perspectives::model_query::test_helpers::StaticShapeResolver;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{LinkExpression, LinkStatus};
use serde_json::{json, Value};
use std::sync::Arc;

/// Signed for real: the classifier counts only verified links.
fn make_link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    crate::perspectives::model_query::test_helpers::signed_link(source, predicate, target, ts)
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

/// `Holder.items` is untyped and declared polymorphic; `Shelf.holder` is typed,
/// so `Shelf → holder → items` reaches the polymorphic include one level down.
/// `Special` requires everything `Base` does and a `we://kind` flag, so a target
/// carrying both ranks `Special` first.
fn status_shapes() -> (StaticShapeResolver, Arc<ModelShape>, Arc<ModelShape>) {
    let (resolver, holder) = StaticShapeResolver::from_json(
        "Holder",
        r#"{"className":"Holder","properties":{
             "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://holder"}
           },"relations":{
             "items":{"predicate":"we://holds","kind":"hasMany","targetClassName":"","polymorphic":true}
           }}"#,
    )
    .unwrap();
    for (name, json) in [
        (
            "Shelf",
            r#"{"className":"Shelf","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://shelf"}
               },"relations":{
                 "holder":{"predicate":"we://on","kind":"hasOne","maxCount":1,"targetClassName":"Holder"}
               }}"#,
        ),
        (
            "Base",
            r#"{"className":"Base","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"}
               },"relations":{}}"#,
        ),
        (
            "Special",
            r#"{"className":"Special","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special"}
               },"relations":{}}"#,
        ),
    ] {
        resolver.register(name, parse_shape_from_json(json, name).unwrap());
    }
    let shelf = super::types::ShapeResolver::get_shape(&resolver, "Shelf").unwrap();
    (resolver, holder, shelf)
}

/// Shelf `s/1` holds holder `h/1`, which holds three targets, each `Base`
/// through a signed Shared link. `t/1` is `Special` only through a Local
/// `we://kind` link, `t/2` through a Shared one, `t/3` through a Shared one
/// whose signature does not verify.
fn status_world() -> SparqlStore {
    use super::link_status_tests::ls_link;
    let signer = crate::agent::signatures::TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    let shared = |s: &str, p: &str, t: &str| ls_link(&signer, s, p, t, 1, LinkStatus::Shared);
    let mut links: Vec<LinkExpression> = ["Holder", "Shelf", "Base", "Special"]
        .iter()
        .map(|c| {
            shared(
                &format!("we://models/{c}"),
                "rdf://type",
                "ad4m://SubjectClass",
            )
        })
        .collect();
    links.extend([
        shared("we://s/1", "we://flag", "we://shelf"),
        shared("we://s/1", "we://on", "we://h/1"),
        shared("we://h/1", "we://flag", "we://holder"),
        shared("we://h/1", "we://holds", "we://t/1"),
        shared("we://h/1", "we://holds", "we://t/2"),
        shared("we://h/1", "we://holds", "we://t/3"),
        shared("we://t/1", "we://flag", "we://base"),
        shared("we://t/2", "we://flag", "we://base"),
        shared("we://t/3", "we://flag", "we://base"),
        ls_link(
            &signer,
            "we://t/1",
            "we://kind",
            "we://special",
            2,
            LinkStatus::Local,
        ),
        shared("we://t/2", "we://kind", "we://special"),
    ]);
    let mut forged = shared("we://t/3", "we://kind", "we://signed-something-else");
    forged.data.target = "we://special".to_string();
    assert!(!forged.compute_proof_valid(), "the fixture must not verify");
    links.push(forged);
    for l in links {
        store.add_link(&l).unwrap();
    }
    store
}

/// `(id, __subjectClass)` of each item, by id: the three links share a
/// timestamp, so their order is not what is under test.
fn item_classes(items: &Value) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = items
        .as_array()
        .unwrap_or_else(|| panic!("items is a list: {items}"))
        .iter()
        .map(|i| {
            (
                i["id"].as_str().unwrap_or_default().to_string(),
                i["__subjectClass"].as_str().unwrap_or_default().to_string(),
            )
        })
        .collect();
    out.sort();
    out
}

fn pairs(v: &[(&str, &str)]) -> Vec<(String, String)> {
    v.iter()
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

/// What the Shared, verified-only read of `status_world` supports: `t/1` and
/// `t/3` are `Special` only through a link that read does not admit.
fn shared_reading() -> Vec<(String, String)> {
    pairs(&[
        ("we://t/1", "Base"),
        ("we://t/2", "Special"),
        ("we://t/3", "Base"),
    ])
}

/// A target classified as a more specific class through a link the read does
/// not admit (Local under `linkStatus: SHARED`, unverified without
/// `includeUnverified`) is not dropped: its group query does not return it, so
/// it is read as the next class it conforms to, the one the admitted links
/// support.
#[tokio::test]
async fn polymorphic_target_typed_only_by_an_unadmitted_link_falls_back() {
    let (resolver, holder, _) = status_shapes();
    let store = status_world();
    let query = q(json!({ "linkStatus": "SHARED", "include": { "items": true } }));
    let result = super::query::execute_model_query(&store, holder.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert_eq!(
        item_classes(&result.instances[0]["items"]),
        shared_reading(),
        "{:?}",
        result.instances
    );
}

/// The same one include down, where the declared polymorphic applies.
#[tokio::test]
async fn nested_polymorphic_target_typed_only_by_an_unadmitted_link_falls_back() {
    let (resolver, _, shelf) = status_shapes();
    let store = status_world();
    let query = q(json!({ "linkStatus": "SHARED",
        "include": { "holder": { "include": { "items": true } } } }));
    let result = super::query::execute_model_query(&store, shelf.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert_eq!(
        item_classes(&result.instances[0]["holder"]["items"]),
        shared_reading(),
        "{:?}",
        result.instances
    );
}

/// The read that admits every link still reads each target as its most
/// specific class.
#[tokio::test]
async fn polymorphic_target_admitted_by_the_read_keeps_its_specific_class() {
    let (resolver, holder, shelf) = status_shapes();
    let store = status_world();
    let expected = pairs(&[
        ("we://t/1", "Special"),
        ("we://t/2", "Special"),
        // t/3's `Special` link does not verify. The classifier counts only
        // verified links whatever `includeUnverified` says (it decides what a
        // node *is*), so t/3 stays `Base` here.
        ("we://t/3", "Base"),
    ]);

    let top = q(json!({ "include": { "items": true } }));
    let result = fixture_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .unwrap();
    assert_eq!(item_classes(&result.instances[0]["items"]), expected);

    let nested = q(json!({ "include": { "holder": { "include": { "items": true } } } }));
    let result = fixture_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .unwrap();
    assert_eq!(
        item_classes(&result.instances[0]["holder"]["items"]),
        expected
    );
}
