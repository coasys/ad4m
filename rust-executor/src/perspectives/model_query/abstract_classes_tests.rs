//! Abstract classes (#1435): which concrete classes one reads, its views, and
//! a union query over them, on shapes written by the real SHACL writer and
//! read back by the real loader. The links the writer emits for the marker
//! and an alias are pinned by `fixtures/shacl_writer_golden.json`.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::shape::load_shape_from_store;
use crate::perspectives::model_query::types::ModelQueryInput;
use crate::perspectives::model_query::{execute_union_query_with_views, ModelQueryResult};
use crate::perspectives::shacl_parser::{
    parse_shacl_to_links, ABSTRACT_PREDICATE, ALIAS_PREDICATE,
};
use crate::perspectives::subject_classes_of::{
    all_subject_class_names, concrete_subject_class_names, subject_classes_of,
};
use crate::types::{Link, LinkExpression, LinkStatus};

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

fn link_expression(link: Link) -> LinkExpression {
    // Signed for real: a link whose signature does not hold is judged
    // `invalid` and never counts, and every schema read now joins the triple
    // to a counting reifier.
    let mut le = LinkExpression::from(TestSigner::generate().sign(link.normalize()));
    le.status = Some(LinkStatus::Shared);
    le
}

/// Raw links, past the SHACL writer's checks.
fn add(store: &SparqlStore, links: &[(&str, &str, &str)]) {
    for (source, predicate, target) in links.iter() {
        let link = Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        };
        store.add_link(&link_expression(link)).unwrap();
    }
}

fn install_class(store: &SparqlStore, name: &str, shacl: &str) {
    // SHACL is read through `load_shape`, which joins every definition
    // triple to a *counting* reifier — a link whose signature does not hold
    // never counts, so schema fixtures are signed for real.
    for link in parse_shacl_to_links(shacl, name).unwrap() {
        store.add_link(&link_expression(link)).unwrap();
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
    let shape = load_shape_from_store(store, abstract_class).unwrap();
    resolve(store, &StoreResolver(store), &index, abstract_class, &shape).unwrap()
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

/// A marker or an alias a peer sent whose stored verdict does not count must not make a
/// class abstract (which would block its create) or pick the property an abstract write
/// targets. Same cut as the shape loader: every definition triple joins a counting reifier.
#[test]
fn the_index_ignores_markers_and_aliases_that_do_not_count() {
    let store = all_classes();
    let one = |q: &str, var: &str| -> String {
        let rows: Vec<serde_json::Value> =
            serde_json::from_str(&store.query(&EvalStop::none(), q).unwrap()).unwrap();
        rows[0][var].as_str().unwrap().to_string()
    };
    let shape = one(
        "SELECT ?shape WHERE { <app://Post> <ad4m://shape> ?shape }",
        "shape",
    );
    let prop = one(
        &format!(
            r#"SELECT ?prop WHERE {{ <{shape}> <sh://property> ?prop FILTER(STRENDS(STR(?prop), ".text")) }}"#
        ),
        "prop",
    );
    // A real alias value, as the SHACL writer stores one (Message.body's).
    let alias = one(
        &format!(
            r#"SELECT ?alias WHERE {{ <flux://Message> <ad4m://shape> ?s . ?s <sh://property> ?p . ?p <{ALIAS_PREDICATE}> ?alias FILTER(STRENDS(STR(?p), ".body")) }}"#
        ),
        "alias",
    );
    let forged = |source: &str, predicate: &str, target: &str| {
        let mut le = link_expression(Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        });
        le.proof.signature = "ab".repeat(64);
        store.add_link(&le).unwrap();
    };
    forged(&shape, ABSTRACT_PREDICATE, "literal://string:true");
    forged(&prop, ALIAS_PREDICATE, &alias);

    let index = AbstractIndex::load(&store).unwrap();
    assert!(
        !index.is_abstract("Post"),
        "a marker that does not count marks nothing"
    );
    assert_eq!(
        index.name_through("Post", "text", "TextContent"),
        "text",
        "an alias that does not count renames nothing"
    );
    // The same links, counting, do both.
    add(
        &store,
        &[(&shape, ABSTRACT_PREDICATE, "literal://string:true")],
    );
    let index = AbstractIndex::load(&store).unwrap();
    assert!(index.is_abstract("Post"));
}

/// The classifier's own reads make the same cut as the index's: a marker that
/// does not count keeps a class concrete, a shape link that does not count
/// marries it to no marked shape, a registration that does not count registers
/// nothing, and a record reached only through links that do not count conforms
/// to nothing. Same rule on the negative pattern too: the enumeration's `FILTER
/// NOT EXISTS` tests for a *counting* marker.
#[test]
fn the_classifier_ignores_triples_that_do_not_count() {
    let store = all_classes();
    let one = |q: &str, var: &str| -> String {
        let rows: Vec<serde_json::Value> =
            serde_json::from_str(&store.query(&EvalStop::none(), q).unwrap()).unwrap();
        rows[0][var].as_str().unwrap().to_string()
    };
    let post_shape = one(
        "SELECT ?shape WHERE { <app://Post> <ad4m://shape> ?shape }",
        "shape",
    );
    // Readable's shape carries a counting marker; marrying Post to it takes a
    // non-counting shape link.
    let readable_shape = one(
        "SELECT ?shape WHERE { <app://Readable> <ad4m://shape> ?shape }",
        "shape",
    );
    let forged = |source: &str, predicate: &str, target: &str| {
        let mut le = link_expression(Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        });
        le.proof.signature = "ab".repeat(64);
        store.add_link(&le).unwrap();
    };
    forged(&post_shape, ABSTRACT_PREDICATE, "literal://string:true");
    forged("app://Post", "ad4m://shape", &readable_shape);
    forged("app://Ghost", "rdf://type", "ad4m://SubjectClass");
    forged("app://r2", "app://kind", "app://post");

    let names = concrete_subject_class_names(&EvalStop::none(), &store).unwrap();
    assert!(names.contains(&"Post".to_string()), "{names:?}");
    assert!(
        !names.contains(&"Readable".to_string()),
        "a counting marker still marks: {names:?}"
    );
    assert!(
        !all_subject_class_names(&EvalStop::none(), &store)
            .unwrap()
            .contains(&"Ghost".to_string()),
        "a registration that does not count registers nothing"
    );
    assert!(
        resolved(&store, "TextContent")
            .classes
            .contains(&"Post".to_string()),
        "Post is still a concrete class TextContent reads"
    );
    let classes = subject_classes_of(
        &EvalStop::none(),
        &store,
        &StoreResolver(&store),
        &["app://r2".to_string()],
    )
    .unwrap();
    assert!(!classes.contains_key("app://r2"), "{classes:?}");

    // The same links, counting, do all of it.
    add(&store, &[("app://r2", "app://kind", "app://post")]);
    let classes = subject_classes_of(
        &EvalStop::none(),
        &store,
        &StoreResolver(&store),
        &["app://r2".to_string()],
    )
    .unwrap();
    assert_eq!(classes["app://r2"], vec!["Post".to_string()]);

    add(
        &store,
        &[(&post_shape, ABSTRACT_PREDICATE, "literal://string:true")],
    );
    assert!(
        !concrete_subject_class_names(&EvalStop::none(), &store)
            .unwrap()
            .contains(&"Post".to_string()),
        "a counting marker makes Post abstract"
    );
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
            store.add_link(&link_expression(link)).unwrap();
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
        &crate::perspectives::sparql_store::EvalStop::none(),
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
        &crate::perspectives::sparql_store::EvalStop::none(),
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

/// Two signers asserting one definition triple must not multiply its rows:
/// every guarded triple `AbstractIndex::load` and `load_shape` join yields
/// one row per binding, however many counting reifiers assert it. Installing
/// the classes twice writes a second reifier (a second signer) over every
/// definition triple; before `join_once`, each row of the definition reads
/// duplicated by its reifier count.
#[test]
fn guarded_definition_queries_return_one_row_per_triple() {
    use serde_json::Value;

    let store = SparqlStore::new(None).unwrap();
    install_class(&store, "TextContent", TEXT_CONTENT);
    install_class(&store, "Message", MESSAGE);
    install_class(&store, "TextContent", TEXT_CONTENT);
    install_class(&store, "Message", MESSAGE);

    let guard = crate::perspectives::model_query::sparql_builder::LinkGuard::default();
    let marked: Vec<Value> = serde_json::from_str(
        &store
            .query(&EvalStop::none(), &AbstractIndex::marked_query(&guard))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(marked.len(), 1, "{marked:?}");

    let aliased: Vec<Value> = serde_json::from_str(
        &store
            .query(&EvalStop::none(), &AbstractIndex::aliased_query(&guard))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(aliased.len(), 1, "{aliased:?}");

    // `load_shape`'s property read is bound by the shape URI on record.
    let shape_uri = load_shape_from_store(&store, "Message").unwrap().shape_uri;
    let props =
        crate::perspectives::model_query::shape::property_rows(&store, &shape_uri, &guard).unwrap();
    assert_eq!(
        props.len(),
        3,
        "Message's three properties, one row each: {props:?}"
    );
}

/// Lal's P3 on #1597: two signers give one property field conflicting
/// counting values. The row carries one of them, and which one must not
/// depend on the order the links reached the store (or the query returned
/// them): the smallest value, whichever arrived first.
///
/// Red while `property_rows` takes the field's first value in query order.
#[test]
fn a_field_two_signers_disagree_on_reads_the_same_whatever_the_insert_order() {
    use crate::perspectives::model_query::shape::property_rows;
    use crate::perspectives::model_query::sparql_builder::LinkGuard;
    use serde_json::Value;

    const OWN: &str = "literal:1^^xsd:integer";
    let guard = LinkGuard::default();
    let body_row = |store: &SparqlStore| -> Value {
        let shape_uri = load_shape_from_store(store, "Message").unwrap().shape_uri;
        let rows = property_rows(store, &shape_uri, &guard).unwrap();
        let body: Vec<Value> = rows
            .into_iter()
            .filter(|row| row["path"] == "flux://body")
            .collect();
        assert_eq!(body.len(), 1, "{body:?}");
        body[0].clone()
    };
    let scratch = SparqlStore::new(None).unwrap();
    install_class(&scratch, "Message", MESSAGE);
    let body_prop = body_row(&scratch)["propUri"].as_str().unwrap().to_string();
    assert_eq!(
        body_row(&scratch)["maxCount"],
        OWN,
        "the writer's own value"
    );

    for conflicting in ["literal:0^^xsd:integer", "literal:7^^xsd:integer"] {
        let expected = OWN.min(conflicting);
        let other = Link {
            source: body_prop.clone(),
            predicate: Some("sh://maxCount".into()),
            target: conflicting.into(),
        };

        let definition_first = SparqlStore::new(None).unwrap();
        install_class(&definition_first, "Message", MESSAGE);
        definition_first
            .add_link(&link_expression(other.clone()))
            .unwrap();

        let conflict_first = SparqlStore::new(None).unwrap();
        conflict_first.add_link(&link_expression(other)).unwrap();
        install_class(&conflict_first, "Message", MESSAGE);

        for (order, store) in [
            ("definition first", &definition_first),
            ("conflict first", &conflict_first),
        ] {
            assert_eq!(
                body_row(store)["maxCount"],
                expected,
                "{order}, against {conflicting}"
            );
        }
    }
}

/// A cold read of every class's definition — what a model query with
/// `include` makes first, through `subject_classes_of_with_pending`, and what
/// a waker's subscribe handshake waits on — must not cost a scan of every
/// counting reifier in the perspective per property field. With a reifier
/// join inside each of `load_shape`'s per-field `OPTIONAL`s and inside
/// `concrete_subject_class_names`' `FILTER NOT EXISTS`, Oxigraph scanned
/// every reifier per row: about a second per class in a WE space of a few
/// thousand links, so the first `include` and the waker's handshake timed out.
#[test]
fn definition_reads_do_not_scale_with_the_perspectives_links() {
    let store = all_classes();
    for i in 0..30 {
        let name = format!("Kind{i}");
        let property = format!(r#"{{"path":"app://field{i}","name":"field","max_count":1}}"#);
        install_class(&store, &name, &concrete_shacl(&name, &property));
    }
    // Records: every link its own counting reifier.
    let signer = TestSigner::generate();
    for i in 0..6000u32 {
        let link = Link {
            source: format!("app://record-{}", i / 3),
            predicate: Some(format!("app://field{}", i % 30)),
            target: format!("literal:string:r{i}"),
        };
        let mut le = LinkExpression::from(signer.sign(link.normalize()));
        le.status = Some(LinkStatus::Shared);
        store.add_link(&le).unwrap();
    }

    let start = std::time::Instant::now();
    let concrete = concrete_subject_class_names(&EvalStop::none(), &store).unwrap();
    let classes_took = start.elapsed();
    for name in &concrete {
        let shape = load_shape_from_store(&store, name).unwrap();
        assert!(
            !shape.properties.is_empty() || !shape.include_relations.is_empty(),
            "{name}"
        );
    }
    let elapsed = start.elapsed();
    eprintln!(
        "{} concrete classes over 6000 signed links: class list {classes_took:?}, with every shape {elapsed:?}",
        concrete.len()
    );
    assert_eq!(concrete.len(), 37, "{concrete:?}");
    // Linear in the records before the fix: ~9 s here in a release build; ~0.4 s after.
    // The bound sits at 3 s, well under the old query's ~9 s and far above the ~0.4 s it
    // takes now, so a loaded runner does not fail it while a return of the old query does.
    assert!(elapsed.as_millis() < 3000, "{elapsed:?}");
}
