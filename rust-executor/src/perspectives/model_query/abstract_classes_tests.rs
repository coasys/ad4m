//! Abstract classes (#1435): which concrete classes one reads, the views it
//! reads them through, and a union query over them. Classes are written
//! through the real SHACL writer and read back through the real shape loader,
//! so the rule is tested on the shapes a perspective actually holds.

use super::*;
use crate::perspectives::model_query::shape::load_shape_from_store;
use crate::perspectives::model_query::types::ModelQueryInput;
use crate::perspectives::model_query::{execute_union_query_with_views, ModelQueryResult};
use crate::perspectives::shacl_parser::parse_shacl_to_links;
use crate::perspectives::subject_classes_of::subject_classes_of;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};

/// The abstract class: anything with `text`. Its path is never compared.
const TEXT_CONTENT: &str = r#"{
  "target_class":"app://TextContent",
  "abstract":true,
  "properties":[
    {"path":"app://text","name":"text","datatype":"xsd://string","min_count":1,"max_count":1}
  ]
}"#;

/// Has `text`, through its own predicate.
const POST: &str = r#"{
  "target_class":"app://Post",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://post"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://post","min_count":1,"max_count":1},
    {"path":"app://content","name":"text","max_count":1}
  ]
}"#;

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

/// `body` aliased for another abstract class only.
const NOTE: &str = r#"{
  "target_class":"app://Note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://note"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://note","min_count":1,"max_count":1},
    {"path":"app://body","name":"body","max_count":1,"alias":{"Readable":"text"}}
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

/// `text` is a relation here, not a property.
const ARTICLE: &str = r#"{
  "target_class":"app://Article",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://article"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://article","min_count":1,"max_count":1},
    {"path":"app://paragraphs","name":"text","relation_kind":"hasMany","target_class_name":"Paragraph"}
  ]
}"#;

/// A second abstract class with `text`: abstract classes never conform to
/// one another.
const READABLE: &str = r#"{
  "target_class":"app://Readable",
  "abstract":true,
  "properties":[{"path":"app://readable_text","name":"text","min_count":1,"max_count":1}]
}"#;

/// Relations by name: a container has `children`.
const CONTAINER: &str = r#"{
  "target_class":"app://Container",
  "abstract":true,
  "properties":[{"path":"ad4m://has_child","name":"children","relation_kind":"hasMany"}]
}"#;

/// `children` through its own predicate, in the other direction even.
const FOLDER: &str = r#"{
  "target_class":"app://Folder",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://folder"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://folder","min_count":1,"max_count":1},
    {"path":"app://contains","name":"children","relation_kind":"belongsToMany"}
  ]
}"#;

/// `children` as a plain property: not a relation.
const LIST: &str = r#"{
  "target_class":"app://List",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"app://kind","target":"app://list"}],
  "properties":[
    {"path":"app://kind","name":"kind","has_value":"app://list","min_count":1,"max_count":1},
    {"path":"app://items","name":"children","max_count":1}
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
        ("TextContent", TEXT_CONTENT),
        ("Readable", READABLE),
        ("Post", POST),
        ("Message", MESSAGE),
        ("Note", NOTE),
        ("Poll", POLL),
        ("Article", ARTICLE),
        ("Container", CONTAINER),
        ("Folder", FOLDER),
        ("List", LIST),
    ])
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
    // Raw links, past `parse_shacl_to_links`'s clash check.
    add(
        &store,
        "app://PostShape",
        "sh://property",
        "app://Post.body",
        "0",
    );
    add(&store, "app://Post.body", "sh://path", "app://body", "0");
    add(
        &store,
        "app://Post.body",
        "rdf://type",
        "sh://PropertyShape",
        "0",
    );
    add(
        &store,
        "app://Post.body",
        ALIAS_PREDICATE,
        &format!(
            "literal:string:{}",
            urlencoding::encode(r#"{"TextContent":"text"}"#)
        ),
        "0",
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
        "app://r1",
        "app://text",
        "literal:string:hello",
        "1",
    );
    add(&store, "app://r2", "app://kind", "app://post", "1");
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
    add(&store, "app://p1", "app://kind", "app://post", "1");
    add(
        &store,
        "app://p1",
        "app://content",
        "literal:string:post%20one",
        "2",
    );
    add(
        &store,
        "flux://m1",
        "flux://entry_type",
        "flux://has_message",
        "3",
    );
    add(
        &store,
        "flux://m1",
        "flux://body",
        "literal:string:hello%20data",
        "4",
    );
    add(&store, "app://n1", "app://kind", "app://note", "5");
    add(
        &store,
        "app://n1",
        "app://body",
        "literal:string:a%20note",
        "6",
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

fn shacl_error(json: &str) -> String {
    parse_shacl_to_links(json, "Clash").unwrap_err().to_string()
}

#[test]
fn an_alias_that_clashes_is_refused_at_registration() {
    // A real `text` and an alias `text`.
    let err = shacl_error(
        r#"{"target_class":"app://Clash","properties":[
            {"path":"app://text","name":"text"},
            {"path":"app://body","name":"body","alias":{"TextContent":"text"}}]}"#,
    );
    assert!(err.contains("already has a property named 'text'"), "{err}");

    // Two aliases `text` for the same abstract class.
    let err = shacl_error(
        r#"{"target_class":"app://Clash","properties":[
            {"path":"app://a","name":"a","alias":{"TextContent":"text"}},
            {"path":"app://b","name":"b","alias":{"TextContent":"text"}}]}"#,
    );
    assert!(err.contains("both have alias 'text'"), "{err}");

    // A reserved name.
    let err = shacl_error(
        r#"{"target_class":"app://Clash","properties":[
            {"path":"app://a","name":"a","alias":{"TextContent":"author"}}]}"#,
    );
    assert!(err.contains("alias 'author'"), "{err}");

    // An empty alias.
    let err = shacl_error(
        r#"{"target_class":"app://Clash","properties":[
            {"path":"app://a","name":"a","alias":{"TextContent":""}}]}"#,
    );
    assert!(err.contains("empty alias"), "{err}");

    // An abstract class with nothing to match on.
    let err = shacl_error(r#"{"target_class":"app://Clash","abstract":true,"properties":[]}"#);
    assert!(err.contains("would match every class"), "{err}");

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

/// One marker on the shape node, one alias link on the property shape.
#[test]
fn the_writer_emits_one_marker_and_one_alias_link() {
    let links = parse_shacl_to_links(TEXT_CONTENT, "TextContent").unwrap();
    let markers: Vec<_> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some(ABSTRACT_PREDICATE))
        .collect();
    assert_eq!(markers.len(), 1);
    assert_eq!(markers[0].source, "app://TextContentShape");

    let links = parse_shacl_to_links(MESSAGE, "Message").unwrap();
    let aliases: Vec<_> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some(ALIAS_PREDICATE))
        .collect();
    assert_eq!(aliases.len(), 1);
    assert_eq!(aliases[0].source, "flux://Message.body");
    assert!(links
        .iter()
        .all(|l| l.predicate.as_deref() != Some(ABSTRACT_PREDICATE)));
}
