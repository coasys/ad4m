//! A write plan against a real perspective: all of it lands, or none of it.

use super::plan::{apply, PlanError, PlannedCreated, PlannedRecord, PlannedWrite};
use super::write::{create, InstanceCreate};
use super::InstanceError;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::LinkQuery;
use serde_json::{json, Map, Value};

/// A required title, and a collection of other records.
const NOTE_SDNA: &str = r#"{
  "target_class": "ns://Note",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Note"}
  ],
  "properties": [
    {"path":"ns://note_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://note_title","target":"value"}]},
    {"path":"ns://item","name":"items","node_kind":"IRI","collection":true,"writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://item","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://item","target":"value"}]}
  ]
}"#;

async fn targets(p: &PerspectiveInstance, source: &str, predicate: &str) -> Vec<String> {
    let mut targets: Vec<String> = p
        .get_links(&LinkQuery {
            source: Some(source.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    targets.sort();
    targets
}

async fn a_note(p: &mut PerspectiveInstance, ctx: &crate::agent::AgentContext) -> String {
    create(
        p,
        ctx,
        InstanceCreate {
            class_name: "Note".into(),
            properties: Some(Map::from_iter([("title".to_string(), json!("existing"))])),
            ..Default::default()
        },
    )
    .await
    .expect("create")
    .base_uri
}

fn note(
    reference: &str,
    title: Option<&str>,
    parent: Option<PlannedRecord>,
    extra: Value,
) -> PlannedWrite {
    let mut properties = Map::new();
    if let Some(title) = title {
        properties.insert("title".into(), json!(title));
    }
    if let Value::Object(extra) = extra {
        properties.extend(extra);
    }
    PlannedWrite::Create {
        reference: reference.into(),
        class_name: "Note".into(),
        properties: Some(properties),
        parent,
    }
}

fn by_ref(reference: &str) -> PlannedRecord {
    PlannedRecord::Created {
        reference: reference.into(),
    }
}

fn id<'a>(created: &'a [PlannedCreated], reference: &str) -> &'a str {
    &created
        .iter()
        .find(|c| c.reference == reference)
        .unwrap_or_else(|| panic!("no create {reference}"))
        .base_uri
}

/// A record with two parts under it, a property naming a part created after it, and the new
/// record added to an existing one's collection: all of it lands.
#[tokio::test(flavor = "multi_thread")]
async fn a_plan_writes_every_record_and_link_it_names() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
    let existing = a_note(&mut p, &ctx).await;

    let created = apply(
        &mut p,
        &ctx,
        vec![
            note(
                "root",
                Some("root"),
                None,
                json!({ "items": [{ "ref": "second" }] }),
            ),
            note("first", Some("first"), Some(by_ref("root")), json!({})),
            note("second", Some("second"), Some(by_ref("root")), json!({})),
            PlannedWrite::Add {
                class_name: "Note".into(),
                base_uri: existing.clone(),
                collection: "items".into(),
                item: by_ref("root"),
            },
        ],
    )
    .await
    .unwrap_or_else(|e| panic!("{e:?}"));

    assert_eq!(
        created
            .iter()
            .map(|c| c.reference.as_str())
            .collect::<Vec<_>>(),
        ["root", "first", "second"]
    );
    let (root, first, second) = (
        id(&created, "root"),
        id(&created, "first"),
        id(&created, "second"),
    );
    let mut parts = vec![first.to_string(), second.to_string()];
    parts.sort();
    assert_eq!(targets(&p, root, "ad4m://has_child").await, parts);
    assert_eq!(
        targets(&p, root, "ns://item").await,
        vec![second.to_string()]
    );
    assert_eq!(
        targets(&p, &existing, "ns://item").await,
        vec![root.to_string()]
    );
    assert_eq!(
        targets(&p, first, "ns://note_title").await,
        vec!["literal:string:first"]
    );
}

/// The third write breaks its class's shape (no title). The two before it were already staged,
/// and none of them land.
#[tokio::test(flavor = "multi_thread")]
async fn a_plan_with_a_refused_write_writes_nothing() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
    let existing = a_note(&mut p, &ctx).await;
    let before = p
        .get_links(&LinkQuery::default())
        .await
        .expect("get_links")
        .len();

    let refused = apply(
        &mut p,
        &ctx,
        vec![
            note("root", Some("root"), None, json!({})),
            PlannedWrite::Add {
                class_name: "Note".into(),
                base_uri: existing.clone(),
                collection: "items".into(),
                item: by_ref("root"),
            },
            note("untitled", None, Some(by_ref("root")), json!({})),
        ],
    )
    .await;

    match refused {
        Err(PlanError::Write {
            index: 2,
            error: InstanceError::Validation { .. },
        }) => {}
        other => panic!("expected the third write refused by its shape, got {other:?}"),
    }
    let after = p
        .get_links(&LinkQuery::default())
        .await
        .expect("get_links")
        .len();
    assert_eq!(after, before, "a refused plan left links behind");
    assert!(targets(&p, &existing, "ns://item").await.is_empty());
}

/// A ref no create has, and two creates with one ref, are refused before anything is written.
#[tokio::test(flavor = "multi_thread")]
async fn a_plan_naming_a_record_it_does_not_create_writes_nothing() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
    let before = p.get_links(&LinkQuery::default()).await.unwrap().len();

    let unknown = apply(
        &mut p,
        &ctx,
        vec![
            note("root", Some("root"), None, json!({})),
            note("part", Some("part"), Some(by_ref("nowhere")), json!({})),
        ],
    )
    .await;
    assert!(
        matches!(unknown, Err(PlanError::UnknownRef { index: 1, ref reference }) if reference == "nowhere"),
        "{unknown:?}"
    );

    let twice = apply(
        &mut p,
        &ctx,
        vec![
            note("a", Some("one"), None, json!({})),
            note("a", Some("two"), None, json!({})),
        ],
    )
    .await;
    assert!(
        matches!(twice, Err(PlanError::DuplicateRef { index: 1, .. })),
        "{twice:?}"
    );

    assert!(matches!(
        apply(&mut p, &ctx, vec![]).await,
        Err(PlanError::Empty)
    ));
    assert_eq!(
        p.get_links(&LinkQuery::default()).await.unwrap().len(),
        before
    );
}

/// The plan as a bundle answers it, in JSON.
#[test]
fn a_plan_reads_from_the_json_a_tool_answers_with() {
    let writes: Vec<PlannedWrite> = serde_json::from_value(json!([
        { "create": { "ref": "post", "className": "Note", "properties": { "title": "t" } } },
        { "create": { "ref": "text", "className": "Note", "parent": { "ref": "post" } } },
        { "create": { "ref": "under", "className": "Note", "parent": "ns://existing" } },
        { "add": { "className": "Note", "baseUri": "ns://subject", "collection": "items", "item": { "ref": "post" } } },
    ]))
    .expect("the JSON form parses");
    assert_eq!(writes.len(), 4);
    assert!(
        matches!(&writes[1], PlannedWrite::Create { parent: Some(PlannedRecord::Created { reference }), .. } if reference == "post")
    );
    assert!(
        matches!(&writes[2], PlannedWrite::Create { parent: Some(PlannedRecord::Existing(id)), .. } if id == "ns://existing")
    );
}
