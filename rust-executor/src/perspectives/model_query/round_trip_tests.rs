//! Round-trip tests for the SHACL writer → store → loader pipeline.
//!
//! These tests gate the spec's R11 requirement: every field that the
//! SHACL JSON-LD transport carries must be parsed by `parse_shacl_to_links`,
//! written as RDF triples to the store, then read back by `load_shape`
//! into a `ModelShape` with no information loss.
//!
//! The Rust side of the round-trip is exercised end-to-end here; the
//! TypeScript side (decorators → SHACL JSON-LD) is covered by
//! `core/src/shacl/SHACLShape.test.ts` and the model tests.

use super::shape::load_shape;
use super::types::ModelShape;
use crate::perspectives::shacl_parser::parse_shacl_to_links;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};

fn make_link_for_round_trip(link: Link) -> DecoratedLinkExpression {
    DecoratedLinkExpression {
        author: "did:key:test_writer".to_string(),
        timestamp: "1700000000000".to_string(),
        data: link,
        proof: DecoratedExpressionProof {
            key: "k".to_string(),
            signature: "s".to_string(),
            valid: Some(true),
            invalid: Some(false),
        },
        status: None,
    }
}

/// Helper: parse the SHACL JSON, fan it out into links, ingest those links
/// into a fresh in-memory store, then ask the loader to rebuild a shape.
fn round_trip(class_name: &str, shacl_json: &str) -> ModelShape {
    let store = SparqlStore::new(None).unwrap();
    let links = parse_shacl_to_links(shacl_json, class_name).expect("parse SHACL");
    // The shape <ad4m://shape> URI link is normally emitted by add_sdna_inner;
    // synthesize the SubjectClass-pointing-at-shape pair load_shape relies on.
    let target_class_uri = format!("ns://{class_name}");
    let shape_uri = format!("ns://{class_name}Shape");
    let mut all_links = vec![
        Link {
            source: target_class_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://SubjectClass".to_string(),
        },
        Link {
            source: target_class_uri.clone(),
            predicate: Some("ad4m://shape".to_string()),
            target: shape_uri.clone(),
        },
    ];
    all_links.extend(links);
    for link in all_links {
        store.add_link(&make_link_for_round_trip(link)).unwrap();
    }
    load_shape(&store, class_name).expect("load shape")
}

#[test]
fn round_trip_preserves_basic_property() {
    let shacl_json = r#"{
        "target_class": "ns://Recipe",
        "properties": [
            { "path": "ns://name", "name": "name", "datatype": "xsd://string", "min_count": 1, "max_count": 1, "resolve_language": "literal" }
        ]
    }"#;
    let shape = round_trip("Recipe", shacl_json);
    let name = shape
        .properties
        .iter()
        .find(|p| p.name == "name")
        .expect("name property");
    assert!(name.is_required, "name should be required");
    assert!(!name.is_collection, "name should be scalar");
    assert_eq!(name.datatype.as_deref(), Some("xsd://string"));
    assert_eq!(name.resolve_language.as_deref(), Some("literal"));
}

#[test]
fn round_trip_preserves_getter() {
    let shacl_json = r#"{
        "target_class": "ns://Message",
        "properties": [
            {
                "path": "ns://reply",
                "name": "replyingTo",
                "getter": "SELECT ?target WHERE { <Base> <ns://reply> ?target . } LIMIT 1"
            }
        ]
    }"#;
    let shape = round_trip("Message", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "replyingTo")
        .expect("replyingTo property");
    assert_eq!(
        prop.getter.as_deref(),
        Some("SELECT ?target WHERE { <Base> <ns://reply> ?target . } LIMIT 1")
    );
}

#[test]
fn round_trip_preserves_flag_property() {
    let shacl_json = r#"{
        "target_class": "ns://Channel",
        "properties": [
            {
                "path": "ns://type",
                "name": "type",
                "min_count": 1,
                "max_count": 1
            }
        ],
        "constructor_actions": [
            { "action": "addLink", "source": "this", "predicate": "ns://type", "target": "ns://Channel" }
        ]
    }"#;
    // Even without sh:hasValue, the writer's constructor action carries the
    // initial value the loader can reconstruct.  When sh:hasValue is added,
    // it also marks the property as a flag.
    let shape = round_trip("Channel", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "type")
        .expect("type property");
    assert_eq!(prop.initial_value.as_deref(), Some("ns://Channel"));
}

#[test]
fn round_trip_preserves_hasvalue_flag() {
    let shacl_json = r#"{
        "target_class": "ns://Item",
        "properties": [
            {
                "path": "ns://kind",
                "name": "kind",
                "min_count": 1,
                "max_count": 1,
                "has_value": "ns://Special"
            }
        ]
    }"#;
    let shape = round_trip("Item", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "kind")
        .expect("kind property");
    assert!(prop.is_flag, "sh:hasValue + minCount>=1 marks a flag");
    assert_eq!(prop.initial_value.as_deref(), Some("ns://Special"));
}

#[test]
fn round_trip_preserves_relation_kind_and_target() {
    let shacl_json = r#"{
        "target_class": "ns://Recipe",
        "properties": [
            {
                "path": "ns://ingredient",
                "name": "ingredients",
                "node_kind": "IRI",
                "relation_kind": "hasMany",
                "target_class_name": "Ingredient"
            }
        ]
    }"#;
    let shape = round_trip("Recipe", shacl_json);
    let rel = shape
        .include_relations
        .iter()
        .find(|r| r.name == "ingredients")
        .expect("ingredients relation");
    assert_eq!(rel.kind, "hasMany");
    assert_eq!(rel.direction, "forward");
    assert_eq!(rel.target_class_name, "Ingredient");
    // hasMany is a collection.
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "ingredients")
        .expect("ingredients in properties");
    assert!(prop.is_collection);
    assert!(!prop.is_scalar_relation);
}

#[test]
fn round_trip_scalar_relation_is_rendered_scalar() {
    let shacl_json = r#"{
        "target_class": "ns://Post",
        "properties": [
            {
                "path": "ns://author",
                "name": "author",
                "node_kind": "IRI",
                "relation_kind": "hasOne",
                "max_count": 1,
                "target_class_name": "User"
            }
        ]
    }"#;
    let shape = round_trip("Post", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "author")
        .expect("author");
    // Scalar relations stay `is_collection: true` so the hydration pipeline
    // accumulates link targets; the renderer uses `is_scalar_relation` to
    // unwrap the array down to a single value.
    assert!(prop.is_scalar_relation);
    assert!(prop.is_collection);
}

#[test]
fn round_trip_belongs_to_relation_is_reverse() {
    let shacl_json = r#"{
        "target_class": "ns://Comment",
        "properties": [
            {
                "path": "ns://author",
                "name": "author",
                "node_kind": "IRI",
                "relation_kind": "belongsToOne",
                "target_class_name": "User"
            }
        ]
    }"#;
    let shape = round_trip("Comment", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "author")
        .expect("author");
    assert_eq!(prop.direction.as_deref(), Some("reverse"));
    assert!(prop.is_scalar_relation);
}

#[test]
fn round_trip_preserves_where_filter_and_predicates() {
    let shacl_json = r#"{
        "target_class": "ns://Board",
        "properties": [
            {
                "path": "ns://has_task",
                "name": "tasks",
                "node_kind": "IRI",
                "relation_kind": "hasMany",
                "target_class_name": "Task",
                "where_filter": { "status": "active" },
                "where_predicates": { "status": "ns://status" }
            }
        ]
    }"#;
    let shape = round_trip("Board", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "tasks")
        .expect("tasks");
    let wf = prop.where_filter.as_ref().expect("where_filter present");
    let cond = wf.get("status").expect("status condition");
    if let super::types::WhereCondition::String(s) = cond {
        assert_eq!(s, "active");
    } else {
        panic!("expected String condition, got {:?}", cond);
    }
    let wp = prop.where_predicates.as_ref().expect("where_predicates");
    assert_eq!(wp.get("status").unwrap(), "ns://status");
}

#[test]
fn round_trip_carries_filter_disabled_flag() {
    // filter=false is the only value emitted by the writer for the filter field;
    // omitted means the executor defaults to true.
    let shacl_json = r#"{
        "target_class": "ns://Doc",
        "properties": [
            {
                "path": "ns://tag",
                "name": "tags",
                "node_kind": "IRI",
                "relation_kind": "hasMany",
                "target_class_name": "Tag",
                "filter": false
            }
        ]
    }"#;
    // load_shape currently consumes the `ad4m://filter` triple but does not
    // expose it on ShapeProperty (no consumer needs it yet); confirm load
    // does not error on its presence and the relation otherwise survives.
    let shape = round_trip("Doc", shacl_json);
    assert!(
        shape.include_relations.iter().any(|r| r.name == "tags"),
        "tags relation should be present despite filter=false"
    );
}

#[test]
fn round_trip_picks_up_max_count_for_relations() {
    let shacl_json = r#"{
        "target_class": "ns://Article",
        "properties": [
            {
                "path": "ns://has_co_author",
                "name": "coAuthors",
                "node_kind": "IRI",
                "relation_kind": "hasMany",
                "target_class_name": "User",
                "max_count": 3
            }
        ]
    }"#;
    let shape = round_trip("Article", shacl_json);
    let rel = shape
        .include_relations
        .iter()
        .find(|r| r.name == "coAuthors")
        .expect("coAuthors");
    assert_eq!(rel.max_count, Some(3));
}

#[test]
fn round_trip_target_class_name_falls_back_to_sh_class_suffix() {
    let shacl_json = r#"{
        "target_class": "ns://Post",
        "properties": [
            {
                "path": "ns://author",
                "name": "author",
                "node_kind": "IRI",
                "relation_kind": "hasOne",
                "class": "ns://UserShape"
            }
        ]
    }"#;
    let shape = round_trip("Post", shacl_json);
    let rel = shape
        .include_relations
        .iter()
        .find(|r| r.name == "author")
        .expect("author relation");
    // Without an explicit target_class_name link the loader falls back to
    // extracting the local-name from the sh:class URI.  A trailing `Shape`
    // suffix is normalised away so the cache/resolver lookup uses the bare
    // class name (`User`), matching the keys the writer emits.
    assert_eq!(rel.target_class_name, "User");
    assert_eq!(rel.target_class_uri, "ns://UserShape");
}

#[test]
fn round_trip_preserves_transform_expression() {
    let shacl_json = r#"{
        "target_class": "ns://ImagePost",
        "properties": [
            {
                "path": "image://data",
                "name": "image",
                "datatype": "xsd://string",
                "resolve_language": "literal",
                "transform": {
                    "type": "concat",
                    "args": [
                        { "type": "literal", "value": "data:image/png;base64," },
                        { "type": "focus" }
                    ]
                }
            }
        ]
    }"#;
    let shape = round_trip("ImagePost", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "image")
        .expect("image property");

    // Deep-compare the structure — `is_some()` would still pass if a future
    // regression mangled the expression body during serialize → store → parse.
    let expected = super::types::TransformExpression::Concat {
        args: vec![
            super::types::TransformExpression::Literal {
                value: serde_json::Value::String("data:image/png;base64,".to_string()),
            },
            super::types::TransformExpression::Focus,
        ],
    };
    assert_eq!(
        prop.transform.as_ref(),
        Some(&expected),
        "transform structure should survive SHACL round-trip without information loss",
    );
}

/// Regression guard for the empty-string preservation half of the
/// transform-round-trip fix.  Before that fix, `resolveLanguage: ""` was
/// dropped on the floor by JS truthiness in both `buildSHACL` and
/// `SHACLShape.toLinks()`.  The Rust pipeline must also preserve it
/// end-to-end so language-expression resolution can be opted into without
/// hard-coding a language address.
#[test]
fn round_trip_preserves_empty_resolve_language() {
    let shacl_json = r#"{
        "target_class": "ns://ImagePost",
        "properties": [
            {
                "path": "image://data",
                "name": "image",
                "datatype": "xsd://string",
                "resolve_language": ""
            }
        ]
    }"#;
    let shape = round_trip("ImagePost", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "image")
        .expect("image property");
    assert_eq!(
        prop.resolve_language.as_deref(),
        Some(""),
        "empty-string resolveLanguage must survive SHACL round-trip (not be coerced to None)",
    );
}

/// A custom (non-"literal") resolveLanguage round-trips and is never treated
/// as deterministic-literal storage.
#[test]
fn round_trip_preserves_custom_resolve_language() {
    let shacl_json = r#"{
        "target_class": "ns://ImagePost",
        "properties": [
            {
                "path": "image://data",
                "name": "image",
                "resolve_language": "lang://custom"
            }
        ]
    }"#;
    let shape = round_trip("ImagePost", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "image")
        .expect("image property");
    assert_eq!(prop.resolve_language.as_deref(), Some("lang://custom"));
    assert!(
        !prop.is_deterministic_literal(),
        "custom resolveLanguage produces signed envelopes, never deterministic literals",
    );
}

/// An explicit `resolveLanguage: "literal"` selects the signed-envelope path
/// (NOT deterministic) — the Flux message body case. Storage mode is now
/// derived entirely from `resolveLanguage`.
#[test]
fn round_trip_explicit_literal_is_envelope() {
    let shacl_json = r#"{
        "target_class": "ns://Message",
        "properties": [
            { "path": "flux://body", "name": "body", "resolve_language": "literal" }
        ]
    }"#;
    let shape = round_trip("Message", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "body")
        .expect("body property");
    assert_eq!(prop.resolve_language.as_deref(), Some("literal"));
    assert!(
        !prop.is_deterministic_literal(),
        "explicit resolveLanguage:\"literal\" means the signed-envelope path",
    );
}

/// A property with no `resolveLanguage` defaults to deterministic typed-literal
/// storage (the perf default — what a plain `@Property()` gets).
#[test]
fn round_trip_no_resolve_language_is_deterministic() {
    let shacl_json = r#"{
        "target_class": "ns://Channel",
        "properties": [
            { "path": "flux://channel_name", "name": "name", "datatype": "xsd://string" }
        ]
    }"#;
    let shape = round_trip("Channel", shacl_json);
    let prop = shape
        .properties
        .iter()
        .find(|p| p.name == "name")
        .expect("name property");
    assert_eq!(prop.resolve_language, None);
    assert!(
        prop.is_deterministic_literal(),
        "no resolveLanguage → deterministic literal storage (perf default)",
    );
}

/// End-to-end test mimicking the JS integration test pipeline:
/// SHACL shape → store → load shape → add data → model query with operators.
#[tokio::test]
async fn e2e_shacl_shape_with_where_ops() {
    use super::query::execute_model_query;
    use super::test_helpers::StaticShapeResolver;
    use super::types::{ModelQueryInput, WhereCondition, WhereOps};
    use std::collections::BTreeMap;
    use std::sync::Arc;

    let store = SparqlStore::new(None).unwrap();

    let shacl_json = r#"{
        "target_class": "ns://TestPost",
        "properties": [
            {
                "path": "test://post_type",
                "name": "type",
                "min_count": 1,
                "max_count": 1,
                "has_value": "test://post",
                "resolve_language": null
            },
            {
                "path": "test://title",
                "name": "title",
                "datatype": "xsd://string",
                "resolve_language": "literal"
            },
            {
                "path": "test://view_count",
                "name": "viewCount",
                "datatype": "xsd://integer",
                "resolve_language": "literal"
            }
        ]
    }"#;

    let class_name = "TestPost";
    let target_class_uri = "ns://TestPost";
    let shape_uri = "ns://TestPostShape";

    let shacl_links = parse_shacl_to_links(shacl_json, class_name).expect("parse SHACL");
    let mut all_links = vec![
        Link {
            source: target_class_uri.to_string(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://SubjectClass".to_string(),
        },
        Link {
            source: target_class_uri.to_string(),
            predicate: Some("ad4m://shape".to_string()),
            target: shape_uri.to_string(),
        },
    ];
    all_links.extend(shacl_links);
    for link in all_links {
        store.add_link(&make_link_for_round_trip(link)).unwrap();
    }

    let shape = load_shape(&store, class_name).expect("load shape");
    eprintln!("[e2e] Loaded shape properties:");
    for p in &shape.properties {
        eprintln!(
            "  {} predicate={} resolve_language={:?} is_flag={} is_required={} initial={:?}",
            p.name, p.predicate, p.resolve_language, p.is_flag, p.is_required, p.initial_value
        );
    }

    let ts = "1700000000000";
    let items = [
        "test://post-1",
        "test://post-2",
        "test://post-3",
        "test://post-4",
        "test://post-5",
    ];
    let titles = ["Alpha", "Beta", "Gamma", "Delta", "Epsilon"];
    let counts: [f64; 5] = [10.0, 20.0, 30.0, 40.0, 50.0];

    for (i, item) in items.iter().enumerate() {
        store
            .add_link(&make_link_for_round_trip(Link {
                source: item.to_string(),
                predicate: Some("test://post_type".to_string()),
                target: "test://post".to_string(),
            }))
            .unwrap();
        store
            .add_link(&make_link_for_round_trip(Link {
                source: item.to_string(),
                predicate: Some("test://title".to_string()),
                target: format!(
                    "literal:string:{}",
                    percent_encoding::utf8_percent_encode(
                        titles[i],
                        percent_encoding::NON_ALPHANUMERIC
                    )
                ),
            }))
            .unwrap();
        let count_target = if counts[i].fract() == 0.0 {
            format!("literal:number:{}", counts[i] as i64)
        } else {
            format!("literal:number:{}", counts[i])
        };
        store
            .add_link(&make_link_for_round_trip(Link {
                source: item.to_string(),
                predicate: Some("test://view_count".to_string()),
                target: count_target,
            }))
            .unwrap();
    }

    let shape_arc = Arc::new(shape);
    let resolver = StaticShapeResolver::new();
    resolver.register_arc(class_name, shape_arc.clone());

    // Test: gt numeric
    let mut wc = BTreeMap::new();
    wc.insert(
        "viewCount".to_string(),
        WhereCondition::Ops(WhereOps {
            gt: Some(30.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        shape_arc.as_ref(),
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        &resolver,
    )
    .await
    .unwrap();
    assert_eq!(
        result.instances.len(),
        2,
        "e2e: gt:30 should match 40 and 50, got {} instances: {:?}",
        result.instances.len(),
        result.instances
    );

    // Test: not string
    let mut wc = BTreeMap::new();
    wc.insert(
        "title".to_string(),
        WhereCondition::Ops(WhereOps {
            not: Some(serde_json::Value::String("Alpha".to_string())),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        shape_arc.as_ref(),
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        &resolver,
    )
    .await
    .unwrap();
    assert_eq!(
        result.instances.len(),
        4,
        "e2e: not:Alpha should match 4, got {} instances: {:?}",
        result.instances.len(),
        result.instances
    );
}
