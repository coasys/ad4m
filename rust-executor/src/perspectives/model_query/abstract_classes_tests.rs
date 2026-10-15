//! Abstract classes (#1435): which concrete classes one reads, its views, and
//! a union query over them, on shapes written by the real SHACL writer and
//! read back by the real loader. The links the writer emits for the marker
//! and an alias are pinned by `fixtures/shacl_writer_golden.json`.

use super::*;
use crate::perspectives::model_query::shape::load_shape_from_store;
use crate::perspectives::model_query::types::ModelQueryInput;
use crate::perspectives::model_query::{execute_union_query_with_views, ModelQueryResult};
use crate::perspectives::shacl_parser::parse_shacl_to_links;
use crate::perspectives::subject_classes_of::subject_classes_of;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};

/// The abstract class: anything with `text`. Its path is never compared.
const TEXT_CONTENT: &str = r#"{"target_class":"app://TextContent","abstract":true,"properties":[
  {"path":"app://text","name":"text","datatype":"xsd://string","min_count":1,"max_count":1}]}"#;

/// A second abstract class with `text`: abstract classes never conform to
/// one another.
const READABLE: &str = r#"{"target_class":"app://Readable","abstract":true,"properties":[
  {"path":"app://readable_text","name":"text","min_count":1,"max_count":1}]}"#;

/// Relations by name: a container has `children`.
const CONTAINER: &str = r#"{"target_class":"app://Container","abstract":true,"properties":[
  {"path":"ad4m://has_child","name":"children","relation_kind":"hasMany"}]}"#;

/// Flux-like: `body`, aliased `text` for TextContent.
const MESSAGE: &str = r#"{
  "target_class":"flux://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"flux://entry_type","target":"flux://has_message"}],
  "properties":[
    {"path":"flux://entry_type","name":"type","has_value":"flux://has_message","min_count":1,"max_count":1},
    {"path":"flux://body","name":"body","max_count":1,"alias":{"TextContent":"text"}},
    {"path":"flux://has_reply","name":"replies","relation_kind":"hasMany","target_class_name":"Message"}
  ]
}"#;

/// `app://<Class>`, flagged `app://kind` → `app://<class>`, with `property`.
fn concrete_shacl(class: &str, property: &str) -> String {
    let kind = format!("app://{}", class.to_lowercase());
    format!(
        r#"{{"target_class":"app://{class}",
        "constructor_actions":[{{"action":"addLink","source":"this","predicate":"app://kind","target":"{kind}"}}],
        "properties":[{{"path":"app://kind","name":"kind","has_value":"{kind}","min_count":1,"max_count":1}},{property}]}}"#
    )
}

fn all_classes() -> SparqlStore {
    let concrete = [
        // Has `text`, through its own predicate.
        (
            "Post",
            r#"{"path":"app://content","name":"text","max_count":1}"#,
        ),
        // `body` aliased for another abstract class only.
        (
            "Note",
            r#"{"path":"app://body","name":"body","max_count":1,"alias":{"Readable":"text"}}"#,
        ),
        // No `text`.
        (
            "Poll",
            r#"{"path":"app://title","name":"title","max_count":1}"#,
        ),
        // `text` is a relation here, not a property.
        (
            "Article",
            r#"{"path":"app://paragraphs","name":"text","relation_kind":"hasMany","target_class_name":"Paragraph"}"#,
        ),
        // `children` through its own predicate, in the other direction even.
        (
            "Folder",
            r#"{"path":"app://contains","name":"children","relation_kind":"belongsToMany"}"#,
        ),
        // `children` as a plain property: not a relation.
        (
            "List",
            r#"{"path":"app://items","name":"children","max_count":1}"#,
        ),
    ];
    let store = SparqlStore::new(None).unwrap();
    for (name, shacl) in [
        ("TextContent", TEXT_CONTENT),
        ("Readable", READABLE),
        ("Container", CONTAINER),
        ("Message", MESSAGE),
    ] {
        install_class(&store, name, shacl);
    }
    for (name, property) in concrete {
        install_class(&store, name, &concrete_shacl(name, property));
    }
    store
}

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

/// Raw links, past the SHACL writer's checks; timestamps in order.
fn add(store: &SparqlStore, links: &[(&str, &str, &str)]) {
    for (i, (source, predicate, target)) in links.iter().enumerate() {
        let link = Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        };
        store
            .add_link(&link_expression(link, &(i + 1).to_string()))
            .unwrap();
    }
}

fn install_class(store: &SparqlStore, name: &str, shacl: &str) {
    for link in parse_shacl_to_links(shacl, name).unwrap() {
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

fn resolved(store: &SparqlStore, abstract_class: &str) -> Resolution {
    let index = AbstractIndex::load(store).unwrap();
    resolve(store, &StoreResolver(store), &index, abstract_class).unwrap()
}

#[test]
fn the_index_reads_abstract_markers_and_aliases() {
    let store = all_classes();
    let index = AbstractIndex::load(&store).unwrap();
    for name in ["TextContent", "Readable", "Container"] {
        assert!(index.is_abstract(name), "{name} is abstract");
    }
    for name in [
        "Post", "Message", "Note", "Poll", "Article", "Folder", "List",
    ] {
        assert!(!index.is_abstract(name), "{name} is concrete");
    }
    assert_eq!(index.name_through("Message", "body", "TextContent"), "text");
    assert_eq!(index.name_through("Message", "body", "Readable"), "body");
    assert_eq!(index.name_through("Note", "body", "Readable"), "text");
    assert_eq!(index.name_through("Post", "text", "TextContent"), "text");
    // And back, for a write through the abstract class.
    let message = load_shape_from_store(&store, "Message").unwrap();
    let own = |name| index.own_name("Message", &message, name, "TextContent");
    assert_eq!((own("text"), own("body")), (Some("body"), None));
}

/// By name: Post has `text`; Message answers to `text` through its alias.
/// Note's alias is for another abstract class, Poll has no `text`, Article's
/// `text` is a relation, and Readable is abstract.
#[test]
fn an_abstract_class_reads_every_concrete_class_with_its_names() {
    let store = all_classes();
    assert_eq!(
        resolved(&store, "TextContent").classes,
        vec!["Message", "Post"]
    );
    assert_eq!(resolved(&store, "Readable").classes, vec!["Note", "Post"]);
}

/// An abstract shape with no names reads nothing, not everything. The
/// writer refuses one; here the marker arrived without the property links,
/// as a sync can deliver it.
#[test]
fn an_abstract_shape_without_names_reads_no_class() {
    let store = all_classes();
    let links = parse_shacl_to_links(
        r#"{"target_class":"app://Bare","abstract":true,
            "properties":[{"path":"app://bare","name":"bare","max_count":1}]}"#,
        "Bare",
    )
    .unwrap();
    let property_nodes: Vec<String> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some("sh://property"))
        .map(|l| l.target.clone())
        .collect();
    assert!(!property_nodes.is_empty());
    for link in links {
        let property_link = link.predicate.as_deref() == Some("sh://property")
            || property_nodes.contains(&link.source);
        if !property_link {
            store.add_link(&link_expression(link, "0")).unwrap();
        }
    }
    let index = AbstractIndex::load(&store).unwrap();
    assert!(index.is_abstract("Bare"));
    let shape = StoreResolver(&store).get_shape("Bare").unwrap();
    assert!(shape.properties.is_empty(), "{shape:?}");
    assert_eq!(resolved(&store, "Bare").classes, Vec::<String>::new());
    let post = StoreResolver(&store).get_shape("Post").unwrap();
    assert!(view_through(&index, "Bare", &shape, "Post", &post).is_none());
}

/// Relations match relations by name, whatever their predicate or direction;
/// a property of that name does not.
#[test]
fn relations_match_by_name_only() {
    let store = all_classes();
    assert_eq!(resolved(&store, "Container").classes, vec!["Folder"]);
}

/// The view renames the aliased property and nothing else: same predicate,
/// same relations. The class's own shape keeps its name.
#[test]
fn the_view_carries_the_abstract_names_through_the_concrete_predicates() {
    let store = all_classes();
    let resolution = resolved(&store, "TextContent");
    let view = &resolution.views["Message"];
    let text = view
        .properties
        .iter()
        .find(|p| p.name == "text")
        .expect("body is read as text");
    assert_eq!(text.predicate, "flux://body");
    assert!(view.properties.iter().all(|p| p.name != "body"));
    assert!(view.include_relations.iter().any(|r| r.name == "replies"));

    let own = load_shape_from_store(&store, "Message").unwrap();
    assert!(own.properties.iter().any(|p| p.name == "body"));
    assert!(own.properties.iter().all(|p| p.name != "text"));
}

/// A shape that reached the store without the writer's checks, with a real
/// `text` beside an alias `text`, is left out rather than read wrongly.
#[test]
fn a_class_with_two_properties_answering_to_one_name_is_left_out() {
    let store = all_classes();
    let alias = format!(
        "literal:string:{}",
        urlencoding::encode(r#"{"TextContent":"text"}"#)
    );
    add(
        &store,
        &[
            ("app://PostShape", "sh://property", "app://Post.body"),
            ("app://Post.body", "sh://path", "app://body"),
            ("app://Post.body", "rdf://type", "sh://PropertyShape"),
            ("app://Post.body", ALIAS_PREDICATE, &alias),
        ],
    );
    assert_eq!(resolved(&store, "TextContent").classes, vec!["Message"]);
}

/// The classifier lists concrete classes only. TextContent requires `text`,
/// so without that rule a record with `app://text` would be classified as
/// it.
#[test]
fn an_abstract_class_is_never_a_records_class() {
    let store = all_classes();
    add(
        &store,
        &[
            ("app://r1", "app://text", "literal:string:hello"),
            ("app://r2", "app://kind", "app://post"),
        ],
    );
    let classes = subject_classes_of(
        &store,
        &StoreResolver(&store),
        &["app://r1".to_string(), "app://r2".to_string()],
    )
    .unwrap();
    assert!(!classes.contains_key("app://r1"), "{classes:?}");
    assert_eq!(classes["app://r2"], vec!["Post".to_string()]);
}

fn ids_and_texts(result: &ModelQueryResult) -> Vec<(String, String, String)> {
    let mut rows: Vec<_> = result
        .instances
        .iter()
        .map(|r| {
            (
                r["id"].as_str().unwrap().to_string(),
                r["__subjectClass"].as_str().unwrap_or("").to_string(),
                r["text"].as_str().unwrap_or("").to_string(),
            )
        })
        .collect();
    rows.sort();
    rows
}

async fn run(store: &SparqlStore, resolution: &Resolution, query: &str) -> ModelQueryResult {
    let query: ModelQueryInput = serde_json::from_str(query).unwrap();
    execute_union_query_with_views(
        store,
        &resolution.classes,
        &query,
        &StoreResolver(store),
        &resolution.views,
    )
    .await
    .unwrap()
}

/// A union query through the views: every row carries `text` and its own
/// class, and `where` on `text` filters every class.
#[tokio::test]
async fn a_query_through_the_abstract_class_reads_aliases_as_its_names() {
    let store = all_classes();
    add(
        &store,
        &[
            ("app://p1", "app://kind", "app://post"),
            ("app://p1", "app://content", "literal:string:post%20one"),
            ("flux://m1", "flux://entry_type", "flux://has_message"),
            ("flux://m1", "flux://body", "literal:string:hello%20data"),
            ("app://n1", "app://kind", "app://note"),
            ("app://n1", "app://body", "literal:string:a%20note"),
        ],
    );

    let resolution = resolved(&store, "TextContent");
    let all = run(&store, &resolution, r#"{"includeUnverified":true}"#).await;
    assert_eq!(
        ids_and_texts(&all),
        vec![
            ("app://p1".into(), "Post".into(), "post one".into()),
            ("flux://m1".into(), "Message".into(), "hello data".into()),
        ]
    );
    assert!(all.instances.iter().all(|r| r.get("body").is_none()));

    let filtered = run(
        &store,
        &resolution,
        r#"{"includeUnverified":true,"where":{"text":"hello data"}}"#,
    )
    .await;
    assert_eq!(
        ids_and_texts(&filtered),
        vec![("flux://m1".into(), "Message".into(), "hello data".into())]
    );
}

#[test]
fn an_alias_that_clashes_is_refused_at_registration() {
    let p = |a: &str, b: &str| format!(r#"{{"path":"app://{a}","name":"{a}","alias":{b}}}"#);
    for (properties, error) in [
        (
            // A real `text` and an alias `text`.
            format!(
                r#"{{"path":"app://text","name":"text"}},{}"#,
                p("body", r#"{"TextContent":"text"}"#)
            ),
            "already has a property named 'text'",
        ),
        (
            format!(
                "{},{}",
                p("a", r#"{"TextContent":"text"}"#),
                p("b", r#"{"TextContent":"text"}"#)
            ),
            "both have alias 'text'",
        ),
        (p("a", r#"{"TextContent":"author"}"#), "alias 'author'"),
        (p("a", r#"{"TextContent":""}"#), "empty alias"),
    ] {
        let json = format!(r#"{{"target_class":"app://Clash","properties":[{properties}]}}"#);
        let err = parse_shacl_to_links(&json, "Clash")
            .unwrap_err()
            .to_string();
        assert!(err.contains(error), "{err}");
    }
    let err = parse_shacl_to_links(
        r#"{"target_class":"app://Clash","abstract":true,"properties":[]}"#,
        "Clash",
    )
    .unwrap_err();
    assert!(err.to_string().contains("would match every class"), "{err}");

    // The same alias for two different abstract classes, and a property
    // aliased to its own name, are fine.
    parse_shacl_to_links(
        r#"{"target_class":"app://Fine","properties":[
            {"path":"app://a","name":"a","alias":{"TextContent":"text"}},
            {"path":"app://b","name":"b","alias":{"Readable":"text"}},
            {"path":"app://c","name":"c","alias":{"Other":"c"}}]}"#,
        "Fine",
    )
    .expect("no clash");
}
