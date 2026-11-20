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
        ("we://t/3", "Special"),
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

/// `items` read under `linkStatus: SHARED` with `where` on `kind`, which only
/// `Special` declares, as `(top level, nested)` results.
async fn items_where(cond: Value) -> (Vec<(String, String)>, Vec<(String, String)>) {
    let (resolver, holder, shelf) = status_shapes();
    let store = status_world();
    let items = json!({ "where": { "kind": cond } });
    let top = q(json!({ "linkStatus": "SHARED", "include": { "items": items } }));
    let top = super::query::execute_model_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .unwrap();
    let nested = q(json!({ "linkStatus": "SHARED",
        "include": { "holder": { "include": { "items": items } } } }));
    let nested = super::query::execute_model_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .unwrap();
    (
        item_classes(&top.instances[0]["items"]),
        item_classes(&nested.instances[0]["holder"]["items"]),
    )
}

/// A target the include's own `where` excludes under its class stays excluded:
/// it is not read again as a less specific class, where `kind` is not a
/// property and a negated condition would pass. `t/2` is `Special` through
/// admitted links and fails `not`; `t/1` and `t/3` are `Base` under this read,
/// so the condition holds for them.
#[tokio::test]
async fn polymorphic_target_excluded_by_where_is_not_read_as_another_class() {
    let expected = pairs(&[("we://t/1", "Base"), ("we://t/3", "Base")]);
    let (top, nested) = items_where(json!({ "not": "we://special" })).await;
    assert_eq!(
        (top, nested),
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// Control: equality on the same property keeps the one target whose admitted
/// links satisfy it, as its specific class.
#[tokio::test]
async fn polymorphic_where_equality_keeps_the_matching_target() {
    let expected = pairs(&[("we://t/2", "Special")]);
    let (top, nested) = items_where(json!("we://special")).await;
    assert_eq!(
        (top, nested),
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// `(id, __subjectClass, __subjectClasses)` of each row, by id.
fn subject_classes(rows: &Value) -> Vec<(String, String, Vec<String>)> {
    let mut out: Vec<(String, String, Vec<String>)> = rows
        .as_array()
        .unwrap_or_else(|| panic!("a list: {rows}"))
        .iter()
        .map(|r| {
            (
                r["id"].as_str().unwrap_or_default().to_string(),
                r["__subjectClass"].as_str().unwrap_or_default().to_string(),
                serde_json::from_value(r["__subjectClasses"].clone()).unwrap_or_default(),
            )
        })
        .collect();
    out.sort();
    out
}

/// What the Shared, verified-only read of `status_world` supports, with every
/// class it supports for each target.
fn shared_classes() -> Vec<(String, String, Vec<String>)> {
    let row = |id: &str, class: &str, all: &[&str]| {
        (
            id.to_string(),
            class.to_string(),
            all.iter().map(|c| c.to_string()).collect::<Vec<_>>(),
        )
    };
    vec![
        row("we://t/1", "Base", &["Base"]),
        row("we://t/2", "Special", &["Special", "Base"]),
        row("we://t/3", "Base", &["Base"]),
    ]
}

/// Classification reads the links the read admits: `__subjectClasses` does
/// not name a class a target is only through a Local or unverified link.
/// Top level and nested.
#[tokio::test]
async fn polymorphic_subject_classes_name_only_what_the_read_admits() {
    let (resolver, holder, shelf) = status_shapes();
    let store = status_world();
    let top = q(json!({ "linkStatus": "SHARED", "include": { "items": true } }));
    let top = super::query::execute_model_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .unwrap();
    let nested = q(json!({ "linkStatus": "SHARED",
        "include": { "holder": { "include": { "items": true } } } }));
    let nested = super::query::execute_model_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .unwrap();
    assert_eq!(
        (
            subject_classes(&top.instances[0]["items"]),
            subject_classes(&nested.instances[0]["holder"]["items"])
        ),
        (shared_classes(), shared_classes()),
        "(top level, nested)"
    );
}

/// The same for a union read, which classifies with the same function.
#[tokio::test]
async fn union_subject_classes_name_only_what_the_read_admits() {
    let (resolver, _, _) = status_shapes();
    let store = status_world();
    let query = q(json!({ "linkStatus": "SHARED" }));
    let classes = vec!["Base".to_string(), "Special".to_string()];
    let result = super::union::execute_union_query(&store, &classes, &query, &resolver)
        .await
        .unwrap();
    assert_eq!(
        subject_classes(&Value::Array(result.instances)),
        shared_classes()
    );
}

/// `Holder`/`Shelf` as in [`status_shapes`], and three classes that each add a
/// `we://kind` flag to `Base` and one more requirement the group query's
/// conformance makes: `SpecialG` a required property read through a getter,
/// `SpecialF` a required flag with no fixed value, and `SpecialL` both its
/// flags declared `local`.
fn membership_shapes() -> (StaticShapeResolver, Arc<ModelShape>, Arc<ModelShape>) {
    let (resolver, holder, shelf) = status_shapes();
    for (name, json) in [
        (
            "SpecialG",
            r#"{"className":"SpecialG","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_g"},
                 "extra":{"predicate":"we://extra","required":true,
                          "getter":"SELECT ?value WHERE { <Base> <we://extra> ?value . }"}
               },"relations":{}}"#,
        ),
        (
            "SpecialF",
            r#"{"className":"SpecialF","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_f"},
                 "tag":{"predicate":"we://tag","required":true,"flag":true}
               },"relations":{}}"#,
        ),
        (
            "SpecialL",
            r#"{"className":"SpecialL","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base","local":true},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_l","local":true}
               },"relations":{}}"#,
        ),
    ] {
        resolver.register(name, parse_shape_from_json(json, name).unwrap());
    }
    (resolver, holder, shelf)
}

/// Shelf `s/1` holds holder `h/1`, which holds `u/1`..`u/3`, all through
/// signed Shared links. Each is `Base`, and carries the `we://kind` of
/// `SpecialG`, `SpecialF` and `SpecialL` respectively, but not the rest of
/// that class: `u/1` has no `we://extra`, `u/2` no `we://tag`, and `u/3`'s
/// links on `SpecialL`'s local predicates are Shared.
fn membership_world() -> SparqlStore {
    use super::link_status_tests::ls_link;
    let signer = crate::agent::signatures::TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    let shared = |s: &str, p: &str, t: &str| ls_link(&signer, s, p, t, 1, LinkStatus::Shared);
    let mut links: Vec<LinkExpression> = [
        "Holder", "Shelf", "Base", "Special", "SpecialG", "SpecialF", "SpecialL",
    ]
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
    ]);
    for (u, kind) in [
        ("we://u/1", "we://special_g"),
        ("we://u/2", "we://special_f"),
        ("we://u/3", "we://special_l"),
    ] {
        links.extend([
            shared("we://h/1", "we://holds", u),
            shared(u, "we://flag", "we://base"),
            shared(u, "we://kind", kind),
        ]);
    }
    for l in links {
        store.add_link(&l).unwrap();
    }
    store
}

/// The items of `h/1` among `ids`, top level and nested, read with no
/// `linkStatus`.
async fn membership_items(ids: &[&str]) -> (Vec<(String, String)>, Vec<(String, String)>) {
    let (resolver, holder, shelf) = membership_shapes();
    let store = membership_world();
    let keep = |v: Vec<(String, String)>| -> Vec<(String, String)> {
        v.into_iter()
            .filter(|(id, _)| ids.contains(&id.as_str()))
            .collect()
    };
    let top = q(json!({ "include": { "items": true } }));
    let top = super::query::execute_model_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .unwrap();
    let nested = q(json!({ "include": { "holder": { "include": { "items": true } } } }));
    let nested = super::query::execute_model_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .unwrap();
    (
        keep(item_classes(&top.instances[0]["items"])),
        keep(item_classes(&nested.instances[0]["holder"]["items"])),
    )
}

/// A class whose group query requires what the target lacks (a required
/// property read through a getter, a required flag with no fixed value) is
/// not the target's class: it is read as `Base`, not lost.
#[tokio::test]
async fn polymorphic_target_lacking_a_getter_or_valueless_requirement_is_read_as_base() {
    let expected = pairs(&[("we://u/1", "Base"), ("we://u/2", "Base")]);
    assert_eq!(
        membership_items(&["we://u/1", "we://u/2"]).await,
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// A class's `local` predicates count only through Local links, as loading
/// reads them: a target holding `SpecialL`'s flags only as Shared links is
/// read as `Base`, not lost.
#[tokio::test]
async fn polymorphic_target_with_shared_links_on_local_predicates_is_read_as_base() {
    let expected = pairs(&[("we://u/3", "Base")]);
    assert_eq!(
        membership_items(&["we://u/3"]).await,
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// [`membership_shapes`] and two more classes. `SpecialC` requires two
/// collections read through getters, `we://p` and `we://q` (a SHACL
/// collection with `minCount` ≥ 1 and a getter; the JSON shape format has no
/// required relation, so the parsed property is marked a collection here),
/// and has an optional scalar `we://r`: its query loads `we://r` only.
/// `SpecialI` adds the IRI flag `we://kind = we://special_i` to `Base`.
fn hydration_shapes() -> (StaticShapeResolver, Arc<ModelShape>, Arc<ModelShape>) {
    let (resolver, holder, shelf) = membership_shapes();
    let getter = |p: &str| format!("SELECT ?target WHERE {{ <Base> <{p}> ?target . }}");
    let mut special_c = parse_shape_from_json(
        &json!({
            "className": "SpecialC",
            "properties": {
                "p": { "predicate": "we://p", "required": true, "getter": getter("we://p") },
                "q": { "predicate": "we://q", "required": true, "getter": getter("we://q") },
                "r": { "predicate": "we://r" }
            },
            "relations": {}
        })
        .to_string(),
        "SpecialC",
    )
    .unwrap();
    for p in special_c.properties.iter_mut() {
        if p.name != "r" {
            p.is_collection = true;
        }
    }
    resolver.register("SpecialC", special_c);
    resolver.register(
        "SpecialI",
        parse_shape_from_json(
            r#"{"className":"SpecialI","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_i"}
               },"relations":{}}"#,
            "SpecialI",
        )
        .unwrap(),
    );
    (resolver, holder, shelf)
}

/// Shelf `s/1` holds holder `h/1`, which holds `u/4`..`u/6`, all `Base`
/// through signed Shared links. `u/4` also links `we://p` and `we://q` but has
/// no `we://r`; `u/5` has all three; `u/6`'s `we://kind` is the *literal*
/// "we://special_i", not the IRI `SpecialI` requires.
fn hydration_world() -> SparqlStore {
    use super::link_status_tests::ls_link;
    let signer = crate::agent::signatures::TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    let shared = |s: &str, p: &str, t: &str| ls_link(&signer, s, p, t, 1, LinkStatus::Shared);
    let mut links: Vec<LinkExpression> = [
        "Holder", "Shelf", "Base", "Special", "SpecialG", "SpecialF", "SpecialL", "SpecialC",
        "SpecialI",
    ]
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
    ]);
    for u in ["we://u/4", "we://u/5", "we://u/6"] {
        links.extend([
            shared("we://h/1", "we://holds", u),
            shared(u, "we://flag", "we://base"),
        ]);
    }
    for u in ["we://u/4", "we://u/5"] {
        links.extend([
            shared(u, "we://p", "we://p/1"),
            shared(u, "we://q", "we://q/1"),
        ]);
    }
    links.extend([
        shared("we://u/5", "we://r", "literal:string:r"),
        shared("we://u/6", "we://kind", "literal:string:we://special_i"),
    ]);
    for l in links {
        store.add_link(&l).unwrap();
    }
    store
}

/// The items of `h/1` in [`hydration_world`] among `ids`, top level and
/// nested, read with no `linkStatus`.
async fn hydration_items(ids: &[&str]) -> (Vec<(String, String)>, Vec<(String, String)>) {
    let (resolver, holder, shelf) = hydration_shapes();
    let store = hydration_world();
    let keep = |v: Vec<(String, String)>| -> Vec<(String, String)> {
        v.into_iter()
            .filter(|(id, _)| ids.contains(&id.as_str()))
            .collect()
    };
    let top = q(json!({ "include": { "items": true } }));
    let top = super::query::execute_model_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .unwrap();
    let nested = q(json!({ "include": { "holder": { "include": { "items": true } } } }));
    let nested = super::query::execute_model_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .unwrap();
    (
        keep(item_classes(&top.instances[0]["items"])),
        keep(item_classes(&nested.instances[0]["holder"]["items"])),
    )
}

/// A class whose requirements hold but whose query loads none of the
/// target's links (it requires only getter-backed collections, which loading
/// does not read) returns no row for it, so it is not the target's class:
/// `u/4` is read as `Base`, not lost. `u/5` carries `we://r`, which
/// `SpecialC`'s query loads, so it is read as `SpecialC`.
#[tokio::test]
async fn polymorphic_target_whose_class_query_loads_none_of_its_links_is_read_as_base() {
    let expected = pairs(&[("we://u/4", "Base"), ("we://u/5", "SpecialC")]);
    assert_eq!(
        hydration_items(&["we://u/4", "we://u/5"]).await,
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// An IRI flag value is not matched by a literal with the same text, as the
/// class's query (`?source <pred> <iri>`) does not match it: `u/6` is read as
/// `Base`, not lost.
#[tokio::test]
async fn polymorphic_target_with_a_literal_lookalike_of_an_iri_flag_is_read_as_base() {
    let expected = pairs(&[("we://u/6", "Base")]);
    assert_eq!(
        hydration_items(&["we://u/6"]).await,
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// [`status_shapes`] and three classes whose predicates are not all
/// emittable as `<…>`, or whose names collide once sanitised.
/// `SpecialBad` has an optional predicate `literal://string:xyz` (stored as
/// a term, but not a parseable IRI) and `SpecialRel` an optional relative
/// predicate `title`, each besides a `we://kind` flag on `Base`. `SpecialD`
/// requires two IRI flags, `we://p-a` and `we://p_a`, with the same value.
fn emit_shapes() -> (StaticShapeResolver, Arc<ModelShape>, Arc<ModelShape>) {
    let (resolver, holder, shelf) = status_shapes();
    for (name, json) in [
        (
            "SpecialBad",
            r#"{"className":"SpecialBad","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_bad"},
                 "odd":{"predicate":"literal://string:xyz"}
               },"relations":{}}"#,
        ),
        (
            "SpecialRel",
            r#"{"className":"SpecialRel","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_rel"},
                 "title":{"predicate":"title"}
               },"relations":{}}"#,
        ),
        (
            "SpecialD",
            r#"{"className":"SpecialD","properties":{
                 "dash":{"predicate":"we://p-a","required":true,"flag":true,"initial":"we://v"},
                 "under":{"predicate":"we://p_a","required":true,"flag":true,"initial":"we://v"}
               },"relations":{}}"#,
        ),
    ] {
        resolver.register(name, parse_shape_from_json(json, name).unwrap());
    }
    (resolver, holder, shelf)
}

/// Shelf `s/1` holds holder `h/1`, which holds `u/7`..`u/9`, all `Base`
/// through signed Shared links, carrying the `we://kind` of `SpecialBad`,
/// `Special` and `SpecialRel` respectively. `u/10` carries both of
/// `SpecialD`'s flags.
fn emit_world() -> SparqlStore {
    use super::link_status_tests::ls_link;
    let signer = crate::agent::signatures::TestSigner::generate();
    let store = SparqlStore::new(None).unwrap();
    let shared = |s: &str, p: &str, t: &str| ls_link(&signer, s, p, t, 1, LinkStatus::Shared);
    let mut links: Vec<LinkExpression> = [
        "Holder",
        "Shelf",
        "Base",
        "Special",
        "SpecialBad",
        "SpecialRel",
        "SpecialD",
    ]
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
    ]);
    for (u, kind) in [
        ("we://u/7", "we://special_bad"),
        ("we://u/8", "we://special"),
        ("we://u/9", "we://special_rel"),
    ] {
        links.extend([
            shared("we://h/1", "we://holds", u),
            shared(u, "we://flag", "we://base"),
            shared(u, "we://kind", kind),
        ]);
    }
    links.extend([
        shared("we://u/10", "we://p-a", "we://v"),
        shared("we://u/10", "we://p_a", "we://v"),
    ]);
    for l in links {
        store.add_link(&l).unwrap();
    }
    store
}

/// A class one of whose predicates cannot be emitted as `<…>` cannot run its
/// own query, so classification skips it rather than fail for every class
/// in the batch: the read succeeds, `u/8` is `Special`, and `u/7` and `u/9`
/// fall back to `Base`. Top level and nested.
#[tokio::test]
async fn polymorphic_read_skips_a_class_with_an_unemittable_predicate() {
    let (resolver, holder, shelf) = emit_shapes();
    let store = emit_world();
    let top = q(json!({ "include": { "items": true } }));
    let top = super::query::execute_model_query(&store, holder.as_ref(), &top, &resolver)
        .await
        .expect("top level read");
    let nested = q(json!({ "include": { "holder": { "include": { "items": true } } } }));
    let nested = super::query::execute_model_query(&store, shelf.as_ref(), &nested, &resolver)
        .await
        .expect("nested read");
    let expected = pairs(&[
        ("we://u/7", "Base"),
        ("we://u/8", "Special"),
        ("we://u/9", "Base"),
    ]);
    assert_eq!(
        (
            item_classes(&top.instances[0]["items"]),
            item_classes(&nested.instances[0]["holder"]["items"])
        ),
        (expected.clone(), expected),
        "(top level, nested)"
    );
}

/// Two IRI flags whose predicates differ only where sanitising makes them
/// equal (`we://p-a`, `we://p_a`), with one value, each check their own
/// link: a node carrying both triples conforms.
#[tokio::test]
async fn iri_flags_whose_names_sanitise_alike_each_check_their_own_link() {
    let (resolver, _, _) = emit_shapes();
    let store = emit_world();
    let special_d = super::types::ShapeResolver::get_shape(&resolver, "SpecialD").unwrap();
    let result =
        super::query::execute_model_query(&store, special_d.as_ref(), &q(json!({})), &resolver)
            .await
            .unwrap();
    let ids: Vec<&str> = result
        .instances
        .iter()
        .filter_map(|i| i["id"].as_str())
        .collect();
    assert_eq!(ids, vec!["we://u/10"]);
}

/// [`emit_shapes`] and two more. `SpecialV` requires two valued properties
/// whose names sanitise alike (`a-b` on `we://va`, `a_b` on `we://vb`).
/// `SpecialL` is `Special`'s `we://kind` with the value `we://special_local`
/// and a `local` getter-backed collection whose predicate
/// `literal://string:abc` is not a parseable IRI: loading does not read it,
/// but the class's status filter names it.
fn collide_shapes() -> (StaticShapeResolver, Arc<ModelShape>, Arc<ModelShape>) {
    let (resolver, holder, shelf) = emit_shapes();
    for (name, json) in [
        (
            "SpecialV",
            r#"{"className":"SpecialV","properties":{
                 "a-b":{"predicate":"we://va","required":true},
                 "a_b":{"predicate":"we://vb","required":true}
               },"relations":{}}"#,
        ),
        (
            "SpecialL",
            r#"{"className":"SpecialL","properties":{
                 "flag":{"predicate":"we://flag","required":true,"flag":true,"initial":"we://base"},
                 "kind":{"predicate":"we://kind","required":true,"flag":true,"initial":"we://special_local"}
               },"relations":{
                 "odd":{"predicate":"literal://string:abc","kind":"hasMany","getter":"?source <we://none> ?target","local":true}
               }}"#,
        ),
    ] {
        resolver.register(name, parse_shape_from_json(json, name).unwrap());
    }
    (resolver, holder, shelf)
}

/// [`emit_world`], with `SpecialV` and `SpecialL` registered. `v/1` carries
/// `we://va` and `we://vb` with different values, `v/2` with the same one.
/// Holder `h/2` holds `u/13`, `Base` with the `we://kind` of `SpecialL`.
fn collide_world() -> SparqlStore {
    use super::link_status_tests::ls_link;
    let signer = crate::agent::signatures::TestSigner::generate();
    let store = emit_world();
    let shared = |s: &str, p: &str, t: &str| ls_link(&signer, s, p, t, 1, LinkStatus::Shared);
    for l in [
        shared("we://models/SpecialV", "rdf://type", "ad4m://SubjectClass"),
        shared("we://models/SpecialL", "rdf://type", "ad4m://SubjectClass"),
        shared("we://v/1", "we://va", "literal://string:one"),
        shared("we://v/1", "we://vb", "literal://string:two"),
        shared("we://v/2", "we://va", "literal://string:same"),
        shared("we://v/2", "we://vb", "literal://string:same"),
        shared("we://h/2", "we://flag", "we://holder"),
        shared("we://h/2", "we://holds", "we://u/13"),
        shared("we://u/13", "we://flag", "we://base"),
        shared("we://u/13", "we://kind", "we://special_local"),
    ] {
        store.add_link(&l).unwrap();
    }
    store
}

/// Two required valued properties whose names sanitise alike (`a-b`, `a_b`)
/// each check their own link, whether their values differ or are equal.
#[tokio::test]
async fn valued_properties_whose_names_sanitise_alike_each_check_their_own_link() {
    let (resolver, _, _) = collide_shapes();
    let store = collide_world();
    let special_v = super::types::ShapeResolver::get_shape(&resolver, "SpecialV").unwrap();
    let result =
        super::query::execute_model_query(&store, special_v.as_ref(), &q(json!({})), &resolver)
            .await
            .unwrap();
    let mut ids: Vec<&str> = result
        .instances
        .iter()
        .filter_map(|i| i["id"].as_str())
        .collect();
    ids.sort();
    assert_eq!(ids, vec!["we://v/1", "we://v/2"]);
}

/// A class whose status filter names a `local` predicate that cannot be
/// emitted as `<…>` cannot run its own query, so classification skips it:
/// the read succeeds and `u/13` falls back to `Base`.
#[tokio::test]
async fn polymorphic_read_skips_a_class_with_an_unemittable_local_predicate() {
    let (resolver, holder, _) = collide_shapes();
    let store = collide_world();
    let query = q(json!({ "where": { "id": "we://h/2" }, "include": { "items": true } }));
    let result = super::query::execute_model_query(&store, holder.as_ref(), &query, &resolver)
        .await
        .expect("the read succeeds");
    assert_eq!(
        item_classes(&result.instances[0]["items"]),
        pairs(&[("we://u/13", "Base")])
    );
}
