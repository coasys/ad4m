//! `register_shapes` and `shapes` on a perspective: what a registration
//! writes, what it leaves alone, and what the registry answers.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::perspective_instance::SubjectClassOption;
use crate::types::LinkExpression;
use std::collections::BTreeSet;

/// A note with a title and a body.
const NOTE: &str = r#"{
  "target_class":"ns://Note",
  "interpretation_hint":"A short note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://kind","target":"ns://note"}],
  "destructor_actions":[{"action":"removeLink","source":"this","predicate":"ns://kind","target":"*"}],
  "properties":[
    {"path":"ns://kind","name":"kind","has_value":"ns://note","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","datatype":"xsd:string","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://body","name":"body","datatype":"xsd:string","max_count":1,"resolve_language":"literal",
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]}
  ]
}"#;

fn note() -> SHACLShape {
    serde_json::from_str(NOTE).unwrap()
}

/// [`NOTE`] with `title` an integer.
fn note_with_integer_title() -> SHACLShape {
    let mut shape = note();
    let title = shape
        .properties
        .iter_mut()
        .find(|p| p.name.as_deref() == Some("title"))
        .unwrap();
    title.datatype = Some("xsd:integer".into());
    shape
}

/// The shape `register_shapes` would store for `shape`, as `shapes` reads it.
fn stored_form(shape: &SHACLShape) -> SHACLShape {
    let links = shape_links(shape.clone(), &class_name_of(&shape.target_class)).unwrap();
    shape_from_links(&shape.target_class, &links).unwrap()
}

/// Every link in the store as (source, predicate, target, author, timestamp).
fn all_links(
    perspective: &PerspectiveInstance,
) -> BTreeSet<(String, String, String, String, String)> {
    perspective
        .sparql_store
        .get_all_links()
        .unwrap()
        .into_iter()
        .map(|l| {
            (
                l.data.source,
                l.data.predicate.unwrap_or_default(),
                l.data.target,
                l.author,
                l.timestamp,
            )
        })
        .collect()
}

fn count(perspective: &PerspectiveInstance, source: &str, predicate: &str, target: &str) -> usize {
    perspective
        .sparql_store
        .query_links(
            Some(source),
            Some(predicate),
            Some(target),
            None,
            None,
            None,
        )
        .unwrap()
        .len()
}

/// The URI of the property node `name` of a class in `ns://`.
fn property_node(name: &str) -> String {
    format!("ns://Note.{name}")
}

/// A copy of `link` signed by another agent, as a peer's registration
/// arrives through sync.
fn peer_copy(link: &Link) -> LinkExpression {
    let signer = TestSigner::generate();
    let mut copy = LinkExpression::from(signer.sign_at(link.clone(), chrono::Utc::now()));
    copy.status = Some(LinkStatus::Shared);
    copy
}

#[tokio::test]
async fn register_twice_writes_no_link() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let first = perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();
    assert_eq!(first.written, vec!["Note".to_string()]);
    assert!(first.unchanged.is_empty());
    let before = all_links(&perspective);

    let second = perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();

    assert_eq!(second.unchanged, vec!["Note".to_string()]);
    assert!(second.written.is_empty());
    assert_eq!(second.generation, first.generation);
    assert_eq!(all_links(&perspective), before, "links and timestamps stay");
}

#[tokio::test]
async fn refresh_keeps_foreign_annotations() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();
    let title = property_node("title");
    // Another app's annotations, a type among them: only the types the
    // writer gives are the class's own.
    let annotations = [
        ("app://label", "literal:string:mine"),
        ("rdf://type", "app://Tagged"),
    ];
    for source in ["ns://Note", "ns://NoteShape", title.as_str()] {
        for (predicate, target) in annotations {
            perspective
                .add_link(
                    Link {
                        source: source.into(),
                        predicate: Some(predicate.into()),
                        target: target.into(),
                    },
                    LinkStatus::Shared,
                    None,
                    &ctx,
                )
                .await
                .unwrap();
        }
    }

    let result = perspective
        .register_shapes(vec![note_with_integer_title()], &ctx)
        .await
        .unwrap();

    assert_eq!(result.written, vec!["Note".to_string()]);
    for source in ["ns://Note", "ns://NoteShape", title.as_str()] {
        for (predicate, target) in annotations {
            assert_eq!(
                count(&perspective, source, predicate, target),
                1,
                "the annotation {predicate} {target} on {source} stays"
            );
        }
    }
    assert_eq!(
        count(&perspective, &title, "sh://datatype", "xsd:string"),
        0
    );
    assert_eq!(
        count(&perspective, &title, "sh://datatype", "xsd:integer"),
        1
    );
    let stored = perspective.shapes(None).unwrap();
    assert_eq!(stored[0].shape, stored_form(&note_with_integer_title()));
}

/// A type link other apps put on a class's nodes is an annotation too:
/// only the types the writer gives are the class's own.
#[tokio::test]
async fn refresh_keeps_foreign_type_links() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();
    let title = property_node("title");
    for source in ["ns://Note", "ns://NoteShape", title.as_str()] {
        perspective
            .add_link(
                Link {
                    source: source.into(),
                    predicate: Some("rdf://type".into()),
                    target: "app://Tagged".into(),
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .unwrap();
    }

    let result = perspective
        .register_shapes(vec![note_with_integer_title()], &ctx)
        .await
        .unwrap();

    assert_eq!(result.written, vec!["Note".to_string()]);
    for source in ["ns://Note", "ns://NoteShape", title.as_str()] {
        assert_eq!(
            count(&perspective, source, "rdf://type", "app://Tagged"),
            1,
            "the type link on {source} stays"
        );
    }
}

#[tokio::test]
async fn shapes_equals_the_stored_shape() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let generation = perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap()
        .generation;

    let summaries = perspective.shapes(None).unwrap();

    assert_eq!(summaries.len(), 1);
    let summary = &summaries[0];
    assert_eq!(summary.name, "Note");
    assert_eq!(summary.shape, stored_form(&note()));
    assert_eq!(summary.hash, shape_hash(&summary.shape));
    assert_eq!(summary.hash.len(), 64);
    assert_eq!(summary.shape.target_class, "ns://Note");
    assert_eq!(
        summary.shape.interpretation_hint.as_deref(),
        Some("A short note")
    );
    assert_eq!(
        summary.shape.constructor_actions,
        note().constructor_actions
    );
    assert_eq!(summary.shape.destructor_actions, note().destructor_actions);
    let names: Vec<_> = summary
        .shape
        .properties
        .iter()
        .map(|p| p.name.clone().unwrap())
        .collect();
    assert_eq!(names, ["body", "kind", "title"]);

    // What `shapes` answers registers as itself.
    let again = perspective
        .register_shapes(vec![summary.shape.clone()], &ctx)
        .await
        .unwrap();
    assert_eq!(again.unchanged, vec!["Note".to_string()]);
    assert_eq!(again.generation, generation);
    assert!(perspective
        .shapes(Some(vec!["Other".into()]))
        .unwrap()
        .is_empty());
}

#[tokio::test]
async fn register_answers_after_the_cache_holds_the_shape() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;

    let result = perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();

    {
        let cache = perspective.abstract_cache.read().unwrap();
        let registry = cache.registry.as_ref().expect("the registry is warm");
        assert!(registry.iter().any(|s| s.name == "Note"));
    }
    assert!(perspective.shape_cache.read().unwrap().contains_key("Note"));
    assert_eq!(result.generation, perspective.shapes_generation());
}

#[tokio::test]
async fn old_index_links_are_ignored() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let old = [
        (
            "ad4m://self",
            "ad4m://has_subject_class",
            "literal:string:Legacy",
        ),
        (
            "literal:string:Legacy",
            "ad4m://sdna",
            "literal:string:subject_class(%22Legacy%22)",
        ),
        ("ad4m://self", "ad4m://has_shacl", "literal:string:Legacy"),
        (
            "literal:string:Legacy",
            "ad4m://shacl_shape_uri",
            "ns://LegacyShape",
        ),
    ];
    for (source, predicate, target) in old {
        perspective
            .add_link(
                Link {
                    source: source.into(),
                    predicate: Some(predicate.into()),
                    target: target.into(),
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .unwrap();
    }

    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();

    assert_eq!(perspective.class_names().unwrap(), vec!["Note".to_string()]);
    assert!(perspective
        .shapes(Some(vec!["Legacy".into()]))
        .unwrap()
        .is_empty());
    for (source, predicate, target) in old {
        assert_eq!(count(&perspective, source, predicate, target), 1);
    }
}

#[tokio::test]
async fn registration_writes_exactly_two_index_links() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let self_links = perspective
        .sparql_store
        .get_links_by_source("ad4m://self")
        .unwrap()
        .len();

    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();

    let index: BTreeSet<(String, String)> = perspective
        .sparql_store
        .get_links_by_source("ns://Note")
        .unwrap()
        .into_iter()
        .map(|l| (l.data.predicate.unwrap_or_default(), l.data.target))
        .collect();
    assert_eq!(
        index,
        BTreeSet::from([
            ("rdf://type".to_string(), SUBJECT_CLASS.to_string()),
            (SHAPE_PREDICATE.to_string(), "ns://NoteShape".to_string()),
        ])
    );
    assert_eq!(
        perspective
            .sparql_store
            .get_links_by_source("ad4m://self")
            .unwrap()
            .len(),
        self_links,
        "nothing indexed on ad4m://self"
    );
}

#[tokio::test]
async fn same_local_name_in_two_namespaces_is_refused() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let task = |ns: &str| -> SHACLShape {
        serde_json::from_value(serde_json::json!({
            "target_class": format!("{ns}Task"),
            "properties": [{"path": format!("{ns}title"), "name": "title", "max_count": 1}]
        }))
        .unwrap()
    };
    let before = all_links(&perspective);

    let err = perspective
        .register_shapes(vec![task("a://"), task("b://")], &ctx)
        .await
        .unwrap_err();

    assert!(
        err.to_string()
            .contains("Classes 'a://Task' and 'b://Task' in one call are both named 'Task'"),
        "{err}"
    );
    assert_eq!(all_links(&perspective), before);
}

#[tokio::test]
async fn concurrent_duplicate_triple_counts_as_unchanged() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();
    let title = property_node("title");
    let copies = [
        Link {
            source: "ns://Note".into(),
            predicate: Some("rdf://type".into()),
            target: SUBJECT_CLASS.into(),
        },
        Link {
            source: title.clone(),
            predicate: Some("sh://datatype".into()),
            target: "xsd:string".into(),
        },
    ];
    for link in &copies {
        perspective.sparql_store.add_link(&peer_copy(link)).unwrap();
    }

    let result = perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();

    assert_eq!(result.unchanged, vec!["Note".to_string()]);
    for link in &copies {
        assert_eq!(
            count(
                &perspective,
                &link.source,
                link.predicate.as_deref().unwrap(),
                &link.target
            ),
            2,
            "both copies of {link:?} stay"
        );
    }

    // A change removes every copy of a stale triple.
    perspective
        .register_shapes(vec![note_with_integer_title()], &ctx)
        .await
        .unwrap();
    assert_eq!(
        count(&perspective, &title, "sh://datatype", "xsd:string"),
        0
    );
    assert_eq!(
        count(&perspective, "ns://Note", "rdf://type", SUBJECT_CLASS),
        2
    );
}

#[tokio::test]
async fn shape_moved_to_a_new_uri_moves_its_mapping() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    perspective
        .register_shapes(vec![note()], &ctx)
        .await
        .unwrap();
    let mut moved = note();
    moved.node_shape_uri = Some("ns://v2/NoteShape".into());

    let result = perspective
        .register_shapes(vec![moved.clone()], &ctx)
        .await
        .unwrap();

    assert_eq!(result.written, vec!["Note".to_string()]);
    let shapes: Vec<String> = perspective
        .sparql_store
        .query_links(
            Some("ns://Note"),
            Some(SHAPE_PREDICATE),
            None,
            None,
            None,
            None,
        )
        .unwrap()
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    assert_eq!(shapes, ["ns://v2/NoteShape"]);
    assert!(
        perspective
            .sparql_store
            .get_links_by_source("ns://NoteShape")
            .unwrap()
            .is_empty(),
        "the old shape node's own links go"
    );
    let stored = perspective.shapes(None).unwrap();
    assert_eq!(
        stored[0].shape.node_shape_uri.as_deref(),
        Some("ns://v2/NoteShape")
    );
    assert_eq!(stored[0].shape, stored_form(&moved));
}

#[tokio::test]
async fn a_shape_uri_another_class_holds_is_refused() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let memo = |ns: &str| -> SHACLShape {
        serde_json::from_value(serde_json::json!({
            "target_class": format!("{ns}Memo"),
            "properties": [{"path": format!("{ns}text"), "name": "text"}]
        }))
        .unwrap()
    };
    perspective
        .register_shapes(vec![memo("other://")], &ctx)
        .await
        .unwrap();
    let mut thief = memo("ns://");
    thief.node_shape_uri = Some("other://MemoShape".into());
    let before = all_links(&perspective);

    let err = perspective
        .register_shapes(vec![thief], &ctx)
        .await
        .unwrap_err();

    assert!(
        err.to_string()
            .contains("already belongs to class 'other://Memo'"),
        "{err}"
    );
    assert_eq!(all_links(&perspective), before);
}

#[tokio::test]
async fn one_call_refuses_a_class_twice_and_a_shared_shape_uri() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let before = all_links(&perspective);

    let twice = perspective
        .register_shapes(vec![note(), note_with_integer_title()], &ctx)
        .await
        .unwrap_err();
    assert!(
        twice
            .to_string()
            .contains("Two shapes for class 'ns://Note'"),
        "{twice}"
    );

    let class = |target_class: &str| -> SHACLShape {
        serde_json::from_value(serde_json::json!({
            "target_class": target_class,
            // Ends with `TaskShape` and with `XTaskShape`.
            "node_shape_uri": "a://XTaskShape",
            "properties": [{"path": "a://title", "name": "title"}]
        }))
        .unwrap()
    };
    let shared = perspective
        .register_shapes(vec![class("a://Task"), class("b://XTask")], &ctx)
        .await
        .unwrap_err();
    assert!(shared.to_string().contains("share a shape URI"), "{shared}");

    assert_eq!(all_links(&perspective), before);
}

/// A task class in `ns`, with a title and a done flag, made by a
/// constructor that marks it with `{ns}kind`.
fn task(ns: &str) -> SHACLShape {
    serde_json::from_value(serde_json::json!({
        "target_class": format!("{ns}Task"),
        "constructor_actions": [
            {"action": "addLink", "source": "this", "predicate": format!("{ns}kind"), "target": format!("{ns}task")}
        ],
        "properties": [
            {"path": format!("{ns}kind"), "name": "kind", "has_value": format!("{ns}task"),
             "min_count": 1, "max_count": 1},
            {"path": format!("{ns}title"), "name": "title", "datatype": "xsd:string",
             "max_count": 1, "writable": true,
             "setter": [{"action": "setSingleTarget", "source": "this",
                         "predicate": format!("{ns}title"), "target": "value"}]}
        ]
    }))
    .unwrap()
}

#[tokio::test]
async fn a_class_name_another_class_has_is_refused() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    perspective
        .register_shapes(vec![task("a://")], &ctx)
        .await
        .unwrap();
    let before = all_links(&perspective);
    let generation = perspective.shapes_generation();

    // Stored `a://Task` holds the name: `b://Task` is refused, and so is
    // the whole call it came in, the class that was fine with it too.
    let err = perspective
        .register_shapes(vec![note(), task("b://")], &ctx)
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("Class 'a://Task' is already registered under the name 'Task'"),
        "{err}"
    );
    assert_eq!(all_links(&perspective), before, "nothing is written");
    assert_eq!(perspective.shapes_generation(), generation);
    let names: Vec<_> = perspective
        .shapes(None)
        .unwrap()
        .into_iter()
        .map(|s| s.shape.target_class)
        .collect();
    assert_eq!(names, ["a://Task"]);

    // Two classes of one name in one call.
    let (mut fresh, _, ctx) = setup_perspective_no_llm(&[]).await;
    let both = fresh
        .register_shapes(vec![task("a://"), task("b://")], &ctx)
        .await
        .unwrap_err();
    assert!(
        both.to_string()
            .contains("Classes 'a://Task' and 'b://Task' in one call are both named 'Task'"),
        "{both}"
    );
    assert!(fresh.shapes(None).unwrap().is_empty());
}

#[tokio::test]
async fn a_query_and_a_create_by_name_use_the_registered_class() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    // `MyTask`'s shape `a://MyTaskShape` and its `a://MyTask.title` end
    // like `Task`'s: a lookup by URI suffix could take its actions.
    let mut my_task = task("a://");
    my_task.target_class = "a://MyTask".into();
    my_task.constructor_actions[0].predicate = "a://mykind".into();
    my_task.properties[1].setter[0].predicate = "a://mytitle".into();
    perspective
        .register_shapes(vec![my_task], &ctx)
        .await
        .unwrap();
    perspective
        .register_shapes(vec![task("a://")], &ctx)
        .await
        .unwrap();
    // Refused: `a://Task` stays the one class named `Task`.
    perspective
        .register_shapes(vec![task("b://")], &ctx)
        .await
        .unwrap_err();

    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Task".into()),
                query: None,
            },
            "expr://t1".into(),
            Some(serde_json::json!({"title": "Write tests"})),
            None,
            &ctx,
        )
        .await
        .unwrap();

    // The create ran `a://Task`'s constructor and setter, nothing of `b://`.
    assert_eq!(count(&perspective, "expr://t1", "a://kind", "a://task"), 1);
    let titles: Vec<_> = perspective
        .sparql_store
        .query_links(Some("expr://t1"), Some("a://title"), None, None, None, None)
        .unwrap();
    assert_eq!(titles.len(), 1);
    assert!(perspective
        .sparql_store
        .get_links_by_source("expr://t1")
        .unwrap()
        .iter()
        .all(|l| {
            let p = l.data.predicate.as_deref().unwrap_or("");
            !p.starts_with("b://") && p != "a://mykind" && p != "a://mytitle"
        }));

    // A query by the name reads `a://Task`'s shape: it finds the instance
    // by `a://kind` and reads its title through `a://title`.
    let shape = perspective.get_shape("Task").unwrap();
    assert_eq!(shape.target_class, "a://Task");
    let rows = crate::perspectives::interpretation_test_support::model_instances(
        &perspective,
        "Task",
        &["title"],
    )
    .await;
    assert_eq!(rows.len(), 1, "{rows:?}");
    assert_eq!(rows[0]["id"], "expr://t1", "{rows:?}");
    assert_eq!(rows[0]["title"], "Write tests", "{rows:?}");
}

/// A class is found by its exact name: `Message` is not `ChatMessage`,
/// though `ChatMessageShape` ends with `MessageShape`.
#[tokio::test]
async fn a_name_finds_only_the_class_of_exactly_that_name() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let chat_message: SHACLShape = serde_json::from_value(serde_json::json!({
        "target_class": "chat://ChatMessage",
        "constructor_actions": [{"action": "addLink", "source": "this",
                                 "predicate": "chat://kind", "target": "chat://message"}],
        "properties": [{"path": "chat://body", "name": "body", "max_count": 1,
            "setter": [{"action": "setSingleTarget", "source": "this",
                        "predicate": "chat://body", "target": "value"}]}]
    }))
    .unwrap();
    perspective
        .register_shapes(vec![chat_message], &ctx)
        .await
        .unwrap();
    let before = all_links(&perspective);

    let created = perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Message".into()),
                query: None,
            },
            "expr://m1".into(),
            Some(serde_json::json!({"body": "hi"})),
            None,
            &ctx,
        )
        .await;

    assert!(created.is_err(), "{created:?}");
    assert_eq!(all_links(&perspective), before, "nothing written");
    assert!(perspective.get_shape("Message").is_err());
    assert!(perspective.get_shape("ChatMessage").is_ok());
    assert!(crate::perspectives::model_query::shape::find_class(
        &perspective.sparql_store,
        "Message"
    )
    .unwrap()
    .is_none());
}

/// A task in `old://`, as an app registered it before this revision.
const OLD_TASK: &str = r#"{
  "target_class":"old://Task",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"old://kind","target":"old://task"}],
  "properties":[
    {"path":"old://title","name":"title","datatype":"xsd:string","max_count":1,
     "where_predicates":{"title":"old://title","done":"old://done"},
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"old://title","target":"value"}]}
  ]
}"#;

/// The links the previous writer (`add_sdna` with SHACL JSON on
/// perf/sdk-round-trips: `parse_shacl_to_links` and `add_sdna_inner`) stored
/// for [`OLD_TASK`]: the name-index links, then the class, shape and property
/// links with action lists serialised in struct field order and maps
/// (`where_predicates`) in whatever key order its map yielded.
const OLD_WRITER_LINKS: [(&str, &str, &str); 17] = [
    (
        "ad4m://self",
        "ad4m://has_subject_class",
        "literal:string:Task",
    ),
    (
        "literal:string:Task",
        "ad4m://sdna",
        "literal:string:%2F%2F%20SHACL",
    ),
    (
        "ad4m://self",
        "ad4m://has_shacl",
        "literal:string:shacl://Task",
    ),
    (
        "literal:string:shacl://Task",
        "ad4m://shacl_shape_uri",
        "old://TaskShape",
    ),
    ("old://Task", "rdf://type", "ad4m://SubjectClass"),
    ("old://Task", "ad4m://shape", "old://TaskShape"),
    ("old://TaskShape", "rdf://type", "sh://NodeShape"),
    ("old://TaskShape", "sh://targetClass", "old://Task"),
    (
        "old://TaskShape",
        "ad4m://constructor",
        r#"literal:string:[{"action":"addLink","source":"this","predicate":"old://kind","target":"old://task"}]"#,
    ),
    ("old://TaskShape", "ad4m://destructor", "literal:string:[]"),
    ("old://TaskShape", "sh://property", "old://Task.title"),
    ("old://Task.title", "rdf://type", "sh://PropertyShape"),
    ("old://Task.title", "sh://path", "old://title"),
    ("old://Task.title", "sh://datatype", "xsd:string"),
    (
        "old://Task.title",
        "sh://maxCount",
        "literal:1^^xsd:integer",
    ),
    (
        "old://Task.title",
        "ad4m://setter",
        r#"literal:string:[{"action":"setSingleTarget","source":"this","predicate":"old://title","target":"value"}]"#,
    ),
    (
        "old://Task.title",
        "ad4m://wherePredicates",
        r#"literal:string:{"title":"old://title","done":"old://done"}"#,
    ),
];

#[tokio::test]
async fn classes_the_previous_writer_stored_are_read_and_rewritten_once() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    for (source, predicate, target) in OLD_WRITER_LINKS {
        perspective
            .add_link(
                Link {
                    source: source.into(),
                    predicate: Some(predicate.into()),
                    target: target.into(),
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .unwrap();
    }
    let old_task: SHACLShape = serde_json::from_str(OLD_TASK).unwrap();

    // Read as stored: the same shape the current writer would store.
    let read = perspective.shapes(Some(vec!["Task".into()])).unwrap();
    assert_eq!(read.len(), 1, "{read:?}");
    assert_eq!(read[0].shape, stored_form(&old_task));
    assert_eq!(
        perspective.get_shape("Task").unwrap().target_class,
        "old://Task"
    );

    assert_eq!(
        read[0].shape.properties[0]
            .where_predicates
            .as_ref()
            .map(|m| m
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>()),
        Some(vec![("done", "old://done"), ("title", "old://title")])
    );

    // The previous writer wrote action lists in struct field order and maps
    // in its map's key order, the current one in canonical JSON: the first
    // registration rewrites those three literals, and only them.
    let before = all_links(&perspective);
    let result = perspective
        .register_shapes(vec![old_task.clone()], &ctx)
        .await
        .unwrap();
    assert_eq!(result.written, vec!["Task".to_string()]);
    let after = all_links(&perspective);
    let changed = |a: &BTreeSet<_>, b: &BTreeSet<_>| -> BTreeSet<(String, String)> {
        a.difference(b)
            .map(|l: &(String, String, String, String, String)| (l.0.clone(), l.1.clone()))
            .collect()
    };
    let rewritten: BTreeSet<(String, String)> = [
        ("old://TaskShape", "ad4m://constructor"),
        ("old://Task.title", "ad4m://setter"),
        ("old://Task.title", "ad4m://wherePredicates"),
    ]
    .into_iter()
    .map(|(s, p)| (s.to_string(), p.to_string()))
    .collect();
    assert_eq!(changed(&before, &after), rewritten, "removed");
    assert_eq!(changed(&after, &before), rewritten, "added");
    // The old name-index links are not the class's own: they stay.
    for (source, predicate, target) in &OLD_WRITER_LINKS[..4] {
        assert_eq!(count(&perspective, source, predicate, target), 1);
    }
    assert_eq!(
        perspective.shapes(Some(vec!["Task".into()])).unwrap()[0].shape,
        stored_form(&old_task)
    );

    let again = perspective
        .register_shapes(vec![old_task], &ctx)
        .await
        .unwrap();
    assert_eq!(again.unchanged, vec!["Task".to_string()]);
}

#[tokio::test]
async fn two_concurrent_registrations_of_one_shape_write_once() {
    let (perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let (mut first, mut second) = (perspective.clone(), perspective.clone());
    let (a, b) = tokio::join!(
        first.register_shapes(vec![note()], &ctx),
        second.register_shapes(vec![note()], &ctx)
    );
    let (a, b) = (a.unwrap(), b.unwrap());

    // One wrote, the other found the shape stored.
    let mut outcomes = [
        (a.written.clone(), a.unchanged.clone()),
        (b.written.clone(), b.unchanged.clone()),
    ];
    outcomes.sort();
    let note_name = vec!["Note".to_string()];
    assert_eq!(
        outcomes,
        [(vec![], note_name.clone()), (note_name, vec![])],
        "{a:?} {b:?}"
    );
    assert_eq!(a.generation, b.generation);

    // Every own link once.
    let expected = shape_links(note(), "Note").unwrap();
    let nodes: BTreeSet<&str> = expected.iter().map(|l| l.source.as_str()).collect();
    let triples: Vec<Triple> = perspective
        .sparql_store
        .get_all_links()
        .unwrap()
        .iter()
        .filter(|l| nodes.contains(l.data.source.as_str()))
        .map(|l| triple(&l.data))
        .collect();
    let distinct: BTreeSet<&Triple> = triples.iter().collect();
    assert_eq!(triples.len(), distinct.len(), "a triple stored twice");
    assert_eq!(triples.len(), expected.len());
    assert_eq!(
        count(&perspective, "ns://Note", "rdf://type", SUBJECT_CLASS),
        1
    );
    for link in &expected {
        assert!(distinct.contains(&triple(link)), "missing {link:?}");
    }
    assert_eq!(
        perspective.shapes(None).unwrap(),
        first.shapes(None).unwrap()
    );
    assert_eq!(
        perspective.shapes(None).unwrap()[0].shape,
        stored_form(&note())
    );
}

/// Two peers register a class of one name, each before it saw the other's,
/// and sync brings each the other's links. `find_class` reads the class of
/// the least IRI, `a://Task`, on both: registering it again answers, so an
/// app that registers its classes on every start still starts. Registering
/// `b://Task` is refused, and so is a new class that would take the name
/// from the stored one, though its IRI sorts first.
#[tokio::test]
async fn a_class_name_two_peers_registered_answers_for_the_class_readers_find() {
    let (mut a_side, _, a_ctx) = setup_perspective_no_llm(&[]).await;
    let (mut b_side, _, b_ctx) = setup_perspective_no_llm(&[]).await;
    a_side
        .register_shapes(vec![task("a://")], &a_ctx)
        .await
        .unwrap();
    b_side
        .register_shapes(vec![task("b://")], &b_ctx)
        .await
        .unwrap();
    // Sync: each side receives the other peer's registration.
    for (side, ns) in [(&a_side, "b://"), (&b_side, "a://")] {
        side.diff_from_link_language(crate::types::PerspectiveDiff {
            additions: shape_links(task(ns), "Task")
                .unwrap()
                .iter()
                .map(peer_copy)
                .collect(),
            removals: vec![],
        })
        .await
        .unwrap();
    }

    for (side, ctx) in [(&mut a_side, &a_ctx), (&mut b_side, &b_ctx)] {
        assert_eq!(side.get_shape("Task").unwrap().target_class, "a://Task");
        let before = all_links(side);

        let again = side.register_shapes(vec![task("a://")], ctx).await.unwrap();
        assert_eq!(again.unchanged, vec!["Task".to_string()]);

        let err = side
            .register_shapes(vec![task("b://")], ctx)
            .await
            .unwrap_err();
        assert!(
            err.to_string()
                .contains("Class 'a://Task' is already registered under the name 'Task'"),
            "{err}"
        );
        assert_eq!(all_links(side), before, "nothing is written");
    }

    // Stored `b://Task` holds the name: `a://Task` sorts first, but taking
    // the name would move every reader of `Task` to another class.
    let (mut fresh, _, ctx) = setup_perspective_no_llm(&[]).await;
    fresh
        .register_shapes(vec![task("b://")], &ctx)
        .await
        .unwrap();
    let err = fresh
        .register_shapes(vec![task("a://")], &ctx)
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("Class 'b://Task' is already registered under the name 'Task'"),
        "{err}"
    );
    assert_eq!(fresh.get_shape("Task").unwrap().target_class, "b://Task");
}
