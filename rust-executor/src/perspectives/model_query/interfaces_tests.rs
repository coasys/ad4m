//! Interface types (#1435): which classes implement an interface, and a union
//! query over them. Classes are written through the real SHACL writer and read
//! back through the real shape loader, so the rule is tested on the shapes a
//! perspective actually holds.

use super::*;
use crate::perspectives::model_query::shape::load_shape_from_store;
use crate::perspectives::model_query::types::ModelQueryInput;
use crate::perspectives::model_query::{execute_union_query, ModelQueryResult};
use crate::perspectives::shacl_parser::{parse_interface_shacl_to_links, parse_shacl_to_links};
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
use std::sync::Arc;

/// We-like: `text` through `we://text`, typed `xsd:string`; children through
/// the base containment predicate.
const TEXT_BLOCK: &str = r#"{
  "target_class":"we://TextBlock",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"we://type","target":"we://text_block"}],
  "properties":[
    {"path":"we://type","name":"type","has_value":"we://text_block","min_count":1,"max_count":1},
    {"path":"we://text","name":"text","datatype":"xsd:string","max_count":1},
    {"path":"ad4m://has_child","name":"children","relation_kind":"hasMany"}
  ]
}"#;

/// Another app's message: `text` through a different predicate and no
/// datatype; it sits *under* a parent through `has_child` (reverse).
const POST: &str = r#"{
  "target_class":"app://Post",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://post"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://post","min_count":1,"max_count":1},
    {"path":"app://content","name":"text","max_count":1},
    {"path":"ad4m://has_child","name":"parent","relation_kind":"belongsToOne","max_count":1}
  ]
}"#;

/// No `text`.
const POLL: &str = r#"{
  "target_class":"app://Poll",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://poll"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://poll","min_count":1,"max_count":1},
    {"path":"app://title","name":"title","max_count":1}
  ]
}"#;

/// `text` is a getter, not a stored predicate.
const SUMMARY: &str = r#"{
  "target_class":"app://Summary",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://summary"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://summary","min_count":1,"max_count":1},
    {"path":"app://summary_text","name":"text","max_count":1,
     "getter":"SELECT ?value WHERE { <Base> <app://summary_text> ?value }"}
  ]
}"#;

/// `text` is a relation here (to `Paragraph` records), not a property.
const ARTICLE: &str = r#"{
  "target_class":"app://Article",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://article"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://article","min_count":1,"max_count":1},
    {"path":"app://paragraphs","name":"text","relation_kind":"hasMany","target_class_name":"Paragraph"}
  ]
}"#;

fn link_expression(link: Link, ts: &str) -> LinkExpression {
    LinkExpression {
        author: "did:key:test123".to_string(),
        timestamp: ts.to_string(),
        data: link,
        proof: ExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
        },
        status: Some(LinkStatus::Shared),
    }
}

fn add(store: &SparqlStore, source: &str, predicate: &str, target: &str, ts: &str) {
    store
        .add_link(&link_expression(
            Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            ts,
        ))
        .unwrap();
}

fn install_class(store: &SparqlStore, name: &str, shacl: &str) {
    for link in parse_shacl_to_links(shacl, name).unwrap() {
        store.add_link(&link_expression(link, "0")).unwrap();
    }
}

fn install_interface(store: &SparqlStore, name: &str, shacl: &str) {
    for link in parse_interface_shacl_to_links(shacl, name).unwrap() {
        store.add_link(&link_expression(link, "0")).unwrap();
    }
}

/// Shapes read straight from the store, as the perspective's cache-backed
/// resolver reads them on a miss.
struct StoreResolver<'a>(&'a SparqlStore);

impl ShapeResolver for StoreResolver<'_> {
    fn get_shape(&self, class_name: &str) -> Result<Arc<ModelShape>, Error> {
        load_shape_from_store(self.0, class_name).map(Arc::new)
    }
}

fn store_with(classes: &[(&str, &str)]) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for (name, shacl) in classes {
        install_class(&store, name, shacl);
    }
    store
}

fn all_classes() -> SparqlStore {
    store_with(&[
        ("TextBlock", TEXT_BLOCK),
        ("Post", POST),
        ("Poll", POLL),
        ("Summary", SUMMARY),
        ("Article", ARTICLE),
    ])
}

fn spec(json: serde_json::Value) -> InterfaceSpec {
    let input: ImplementsInput = serde_json::from_value(json).unwrap();
    InterfaceSpec::try_from(input).unwrap()
}

fn resolve(store: &SparqlStore, json: serde_json::Value) -> Vec<String> {
    classes_implementing(store, &StoreResolver(store), &spec(json)).unwrap()
}

/// A property named alone matches every class with a property of that name,
/// whatever predicate it is stored under — including a getter-backed one. A
/// relation of that name is not a property, and a class without it is out.
#[test]
fn a_property_by_name_matches_loosely() {
    let store = all_classes();
    assert_eq!(
        resolve(&store, serde_json::json!({"properties": ["text"]})),
        vec!["Post", "Summary", "TextBlock"]
    );
}

/// Every part must match: a class with `text` but not `title` is out.
#[test]
fn every_part_of_the_interface_must_match() {
    let store = all_classes();
    assert!(resolve(&store, serde_json::json!({"properties": ["text", "title"]})).is_empty());
    assert_eq!(
        resolve(&store, serde_json::json!({"properties": ["title"]})),
        vec!["Poll"]
    );
}

/// `through` pins the predicate: a class whose `text` is stored elsewhere is
/// excluded.
#[test]
fn through_pins_the_predicate() {
    let store = all_classes();
    assert_eq!(
        resolve(
            &store,
            serde_json::json!({"properties": [{"name": "text", "through": "we://text"}]})
        ),
        vec!["TextBlock"]
    );
    assert_eq!(
        resolve(
            &store,
            serde_json::json!({"properties": [{"name": "text", "through": "app://content"}]})
        ),
        vec!["Post"]
    );
    assert!(resolve(
        &store,
        serde_json::json!({"properties": [{"name": "text", "through": "flux://body"}]})
    )
    .is_empty());
}

/// `datatype` pins the datatype, in any of its spellings; a property that
/// declares none does not match a pinned datatype.
#[test]
fn datatype_pins_the_datatype() {
    let store = all_classes();
    for spelling in [
        "xsd:string",
        "xsd://string",
        "http://www.w3.org/2001/XMLSchema#string",
    ] {
        assert_eq!(
            resolve(
                &store,
                serde_json::json!({"properties": [{"name": "text", "datatype": spelling}]})
            ),
            vec!["TextBlock"],
            "{spelling}"
        );
    }
    assert!(resolve(
        &store,
        serde_json::json!({"properties": [{"name": "text", "datatype": "xsd:integer"}]})
    )
    .is_empty());
}

/// A relation matches by predicate and direction, never by name: `children`
/// and `items` both find `TextBlock`'s forward `has_child`; `Post`'s
/// `has_child` points the other way.
#[test]
fn a_relation_matches_by_predicate_and_direction() {
    let store = all_classes();
    for name in ["children", "items"] {
        assert_eq!(
            resolve(
                &store,
                serde_json::json!({"relations": [{"through": "ad4m://has_child", "name": name}]})
            ),
            vec!["TextBlock"]
        );
    }
    assert_eq!(
        resolve(
            &store,
            serde_json::json!({"relations": [{"through": "ad4m://has_child", "direction": "reverse"}]})
        ),
        vec!["Post"]
    );
    assert!(resolve(
        &store,
        serde_json::json!({"relations": [{"through": "we://child"}]})
    )
    .is_empty());
}

/// A relation is not a property: asking for a property `children` (the name
/// of `TextBlock`'s relation) matches nothing, and `Article`, whose `text` is
/// a relation, is not a `text` holder.
#[test]
fn a_relation_does_not_satisfy_a_property() {
    let store = all_classes();
    assert!(resolve(&store, serde_json::json!({"properties": ["children"]})).is_empty());
    assert!(
        !resolve(&store, serde_json::json!({"properties": ["text"]}))
            .contains(&"Article".to_string())
    );
}

/// The ad-hoc form is checked: nothing asked for, a nameless property, a
/// relation without a predicate, an unknown key and an unknown direction are
/// all refused.
#[test]
fn the_ad_hoc_form_refuses_what_has_no_meaning() {
    let refused = |json: serde_json::Value| {
        serde_json::from_value::<ImplementsInput>(json)
            .map_err(Error::from)
            .and_then(InterfaceSpec::try_from)
            .is_err()
    };
    assert!(refused(serde_json::json!({})));
    assert!(refused(serde_json::json!({"properties": []})));
    assert!(refused(serde_json::json!({"properties": [""]})));
    assert!(refused(serde_json::json!({"relations": [{"through": ""}]})));
    assert!(refused(
        serde_json::json!({"relations": [{"predicate": "ad4m://has_child"}]})
    ));
    assert!(refused(
        serde_json::json!({"relations": [{"through": "ad4m://has_child", "direction": "up"}]})
    ));
    assert!(refused(serde_json::json!({"props": ["text"]})));
}

/// Two specs asking for the same thing in another order are one cache key.
#[test]
fn equal_specs_are_equal_whatever_their_order() {
    let a = spec(serde_json::json!({"properties": ["text", "title", "text"]}));
    let b = spec(serde_json::json!({"properties": ["title", {"name": "text"}]}));
    assert_eq!(a, b);
}

const MESSAGE_INTERFACE: &str = r#"{
  "target_class":"ad4m://interface/Message",
  "properties":[
    {"path":"","name":"text"},
    {"path":"ad4m://has_child","name":"children","relation_kind":"hasMany"}
  ]
}"#;

/// A named interface is written by the SHACL writer and read back as the same
/// rule. It is not a class: nothing lists, loads or classifies it as one.
#[test]
fn a_named_interface_round_trips_through_shacl_and_is_not_a_class() {
    let store = all_classes();
    install_interface(&store, "Message", MESSAGE_INTERFACE);

    let loaded = load_interface(&store, "Message").unwrap();
    assert_eq!(
        loaded,
        InterfaceSpec::new(
            vec![InterfaceProperty {
                name: "text".into(),
                through: None,
                datatype: None,
            }],
            vec![InterfaceRelation {
                through: "ad4m://has_child".into(),
                direction: RelationDirection::Forward,
                name: Some("children".into()),
            }],
        )
        .unwrap()
    );
    assert_eq!(
        classes_implementing(&store, &StoreResolver(&store), &loaded).unwrap(),
        vec!["TextBlock"]
    );

    assert!(!all_subject_class_names(&store)
        .unwrap()
        .contains(&"Message".to_string()));
    assert!(load_shape_from_store(&store, "Message").is_err());
    let missing = load_interface(&store, "Messag").unwrap_err();
    assert!(
        missing.downcast_ref::<MissingInterface>().is_some(),
        "{missing}"
    );
}

/// A pinned property in a named interface keeps its pin through SHACL.
#[test]
fn a_named_interface_keeps_its_pins() {
    let store = all_classes();
    install_interface(
        &store,
        "WeText",
        r#"{"target_class":"ad4m://interface/WeText",
            "properties":[{"path":"we://text","name":"text","datatype":"xsd:string"}]}"#,
    );
    let loaded = load_interface(&store, "WeText").unwrap();
    assert_eq!(loaded.properties[0].through.as_deref(), Some("we://text"));
    assert_eq!(loaded.properties[0].datatype.as_deref(), Some("xsd:string"));
    assert_eq!(
        classes_implementing(&store, &StoreResolver(&store), &loaded).unwrap(),
        vec!["TextBlock"]
    );
}

/// The interface writer refuses whatever would make it a class or write data.
#[test]
fn the_interface_writer_refuses_class_parts() {
    for (why, shacl) in [
        (
            "constructor",
            r#"{"target_class":"ad4m://interface/I","properties":[{"path":"","name":"text"}],
                "constructor_actions":[{"action":"addLink","source":"this","predicate":"a://b","target":"a://c"}]}"#,
        ),
        (
            "flag",
            r#"{"target_class":"ad4m://interface/I","properties":[{"path":"a://t","name":"t","has_value":"a://x","min_count":1}]}"#,
        ),
        (
            "setter",
            r#"{"target_class":"ad4m://interface/I","properties":[{"path":"a://t","name":"t",
                "setter":[{"action":"setSingleTarget","source":"this","predicate":"a://t","target":"value"}]}]}"#,
        ),
        (
            "relation without a predicate",
            r#"{"target_class":"ad4m://interface/I","properties":[{"path":"","name":"kids","relation_kind":"hasMany"}]}"#,
        ),
        (
            "nothing",
            r#"{"target_class":"ad4m://interface/I","properties":[]}"#,
        ),
    ] {
        assert!(
            parse_interface_shacl_to_links(shacl, "I").is_err(),
            "an interface with a {why} must be refused"
        );
    }
}

fn text_block(store: &SparqlStore, id: &str, text: &str, ts: &str) {
    add(store, id, "we://type", "we://text_block", ts);
    add(
        store,
        id,
        "we://text",
        &format!("literal:string:{text}"),
        ts,
    );
}

fn post(store: &SparqlStore, id: &str, text: &str, ts: &str) {
    add(store, id, "app://kind", "app://post", ts);
    add(
        store,
        id,
        "app://content",
        &format!("literal:string:{text}"),
        ts,
    );
}

fn poll(store: &SparqlStore, id: &str, title: &str, ts: &str) {
    add(store, id, "app://kind", "app://poll", ts);
    add(
        store,
        id,
        "app://title",
        &format!("literal:string:{title}"),
        ts,
    );
}

/// Fixture links carry no verifiable proof, so reads opt in to unverified rows.
fn query(json: serde_json::Value) -> ModelQueryInput {
    let mut q: ModelQueryInput = serde_json::from_value(json).unwrap();
    q.include_unverified = Some(true);
    q
}

fn rows(result: &ModelQueryResult) -> Vec<(String, String, String)> {
    result
        .instances
        .iter()
        .map(|r| {
            (
                r["id"].as_str().unwrap().to_string(),
                r["__subjectClass"].as_str().unwrap().to_string(),
                r["text"].as_str().unwrap_or("").to_string(),
            )
        })
        .collect()
}

/// An interface query is the union query over the classes that implement it:
/// rows of both `text` classes, each hydrated as its own class with
/// `__subjectClass` set, the `Poll` left out, and `where` / `order` /
/// `limit` / `offset` applied once over the union.
#[tokio::test]
async fn an_interface_query_reads_every_implementing_class_as_one_union() {
    let store = store_with(&[("TextBlock", TEXT_BLOCK), ("Post", POST), ("Poll", POLL)]);
    text_block(&store, "we://tb1", "hello", "1");
    post(&store, "app://p1", "hi @data", "2");
    poll(&store, "app://q1", "lunch?", "3");
    text_block(&store, "we://tb2", "@data look", "4");
    let resolver = StoreResolver(&store);

    let classes = classes_implementing(
        &store,
        &resolver,
        &spec(serde_json::json!({"properties": ["text"]})),
    )
    .unwrap();
    assert_eq!(classes, vec!["Post", "TextBlock"]);

    let all = execute_union_query(&store, &classes, &query(serde_json::json!({})), &resolver)
        .await
        .unwrap();
    assert_eq!(
        rows(&all),
        vec![
            ("we://tb1".into(), "TextBlock".into(), "hello".into()),
            ("app://p1".into(), "Post".into(), "hi @data".into()),
            ("we://tb2".into(), "TextBlock".into(), "@data look".into()),
        ]
    );
    assert_eq!(all.total_count, 3);

    let page = execute_union_query(
        &store,
        &classes,
        &query(serde_json::json!({"order": {"text": "DESC"}, "limit": 2, "offset": 1})),
        &resolver,
    )
    .await
    .unwrap();
    assert_eq!(
        rows(&page)
            .into_iter()
            .map(|(id, ..)| id)
            .collect::<Vec<_>>(),
        vec!["we://tb1", "we://tb2"],
        "text DESC is hi @data, hello, @data look; the page skips one"
    );
    assert_eq!(page.total_count, 3);

    let one = execute_union_query(
        &store,
        &classes,
        &query(serde_json::json!({"where": {"id": "app://p1"}})),
        &resolver,
    )
    .await
    .unwrap();
    assert_eq!(
        rows(&one),
        vec![("app://p1".into(), "Post".into(), "hi @data".into())]
    );
}
