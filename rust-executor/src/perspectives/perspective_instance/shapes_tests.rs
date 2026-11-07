//! `register_shapes` and `shapes` on a perspective: what a registration
//! writes, what it leaves alone, and what the registry answers.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
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
    for source in ["ns://Note", "ns://NoteShape", title.as_str()] {
        perspective
            .add_link(
                Link {
                    source: source.into(),
                    predicate: Some("app://label".into()),
                    target: "literal:string:mine".into(),
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
            count(&perspective, source, "app://label", "literal:string:mine"),
            1,
            "the annotation on {source} stays"
        );
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
async fn same_local_name_in_two_namespaces() {
    let (mut perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    let task = |ns: &str| -> SHACLShape {
        serde_json::from_value(serde_json::json!({
            "target_class": format!("{ns}Task"),
            "properties": [{"path": format!("{ns}title"), "name": "title", "max_count": 1}]
        }))
        .unwrap()
    };

    let result = perspective
        .register_shapes(vec![task("a://"), task("b://")], &ctx)
        .await
        .unwrap();
    assert_eq!(result.written, vec!["Task".to_string(), "Task".to_string()]);
    // Registering one again leaves the other alone.
    let again = perspective
        .register_shapes(vec![task("b://")], &ctx)
        .await
        .unwrap();
    assert_eq!(again.unchanged, vec!["Task".to_string()]);

    let targets: Vec<String> = perspective
        .shapes(Some(vec!["Task".into()]))
        .unwrap()
        .into_iter()
        .map(|s| s.shape.target_class)
        .collect();
    assert_eq!(targets, ["a://Task", "b://Task"]);
    assert_eq!(
        count(&perspective, "a://Task", SHAPE_PREDICATE, "a://TaskShape"),
        1
    );
    assert_eq!(
        count(&perspective, "b://Task", SHAPE_PREDICATE, "b://TaskShape"),
        1
    );
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

    let task = |ns: &str| -> SHACLShape {
        serde_json::from_value(serde_json::json!({
            "target_class": format!("{ns}Task"),
            "node_shape_uri": "a://TaskShape",
            "properties": [{"path": format!("{ns}title"), "name": "title"}]
        }))
        .unwrap()
    };
    let shared = perspective
        .register_shapes(vec![task("a://"), task("b://")], &ctx)
        .await
        .unwrap_err();
    assert!(shared.to_string().contains("share a shape URI"), "{shared}");

    assert_eq!(all_links(&perspective), before);
}
