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

/// A record that holds its parts through a relation of its own, which answers to `Container`'s
/// `children`.
const HOLDER_SDNA: &str = r#"{
  "target_class": "ns://Holder",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Holder"}
  ],
  "properties": [
    {"path":"rdf://type","name":"type","has_value":"ns://Holder","min_count":1,"max_count":1},
    {"path":"ns://part","name":"parts","node_kind":"IRI","relation_kind":"hasMany","collection":true,"alias":{"Container":"children"},
     "adder":[{"action":"addLink","source":"this","predicate":"ns://part","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://part","target":"value"}]}
  ]
}"#;

/// A class with a property named `ref`.
const TAG_SDNA: &str = r#"{
  "target_class": "ns://Tag",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Tag"}
  ],
  "properties": [
    {"path":"ns://tag_ref","name":"ref","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://tag_ref","target":"value"}]}
  ]
}"#;

/// A part put under a record the same plan creates goes through that record's own `children`
/// relation, as it would under a committed one, not `ad4m://has_child`: the parent is only
/// staged when the part is written. A parent created after its part is the same.
#[tokio::test(flavor = "multi_thread")]
async fn a_part_under_a_record_of_the_plan_goes_through_its_class_s_relation() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("Note", NOTE_SDNA), ("Holder", HOLDER_SDNA)]).await;
    let holder = |reference: &str| PlannedWrite::Create {
        reference: reference.into(),
        class_name: "Holder".into(),
        properties: None,
        parent: None,
    };
    let created = apply(
        &mut p,
        &ctx,
        vec![
            holder("before"),
            note("first", Some("first"), Some(by_ref("before")), json!({})),
            note("second", Some("second"), Some(by_ref("after")), json!({})),
            holder("after"),
        ],
    )
    .await
    .unwrap_or_else(|e| panic!("{e:?}"));

    for (holder, part) in [("before", "first"), ("after", "second")] {
        let holder = id(&created, holder);
        assert_eq!(
            targets(&p, holder, "ns://part").await,
            vec![id(&created, part).to_string()]
        );
        assert!(targets(&p, holder, "ad4m://has_child").await.is_empty());
    }
}

/// A properties map whose one property is named `ref` is a value, not a ref to a record of the
/// plan.
#[tokio::test(flavor = "multi_thread")]
async fn a_property_named_ref_is_written_as_a_value() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Tag", TAG_SDNA)]).await;
    let created = apply(
        &mut p,
        &ctx,
        vec![PlannedWrite::Create {
            reference: "tag".into(),
            class_name: "Tag".into(),
            properties: Some(Map::from_iter([("ref".to_string(), json!("v1.2"))])),
            parent: None,
        }],
    )
    .await
    .unwrap_or_else(|e| panic!("{e:?}"));
    assert_eq!(
        targets(&p, id(&created, "tag"), "ns://tag_ref").await,
        vec!["literal:string:v1.2"]
    );
}

/// A create of one of the executor's own classes, not registered yet, is refused before anything
/// is written: registering it is a write the plan's batch could not take back.
#[tokio::test(flavor = "multi_thread")]
async fn a_plan_creating_an_unregistered_executor_class_writes_nothing() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
    let mut before: Vec<String> = p
        .get_links(&LinkQuery::default())
        .await
        .unwrap()
        .into_iter()
        .map(|l| format!("{:?}", l.data))
        .collect();
    before.sort();

    let refused = apply(
        &mut p,
        &ctx,
        vec![
            note("root", Some("root"), None, json!({})),
            PlannedWrite::Create {
                reference: "run".into(),
                class_name: "ad4m://FlowInstance".into(),
                properties: None,
                parent: None,
            },
        ],
    )
    .await;
    assert!(
        matches!(&refused, Err(PlanError::UnregisteredClass { index: 1, class_name }) if class_name == "ad4m://FlowInstance"),
        "{refused:?}"
    );

    let mut after: Vec<String> = p
        .get_links(&LinkQuery::default())
        .await
        .unwrap()
        .into_iter()
        .map(|l| format!("{:?}", l.data))
        .collect();
    after.sort();
    assert_eq!(after, before, "a refused plan changed the links");
    assert!(
        crate::perspectives::hardwired_class::unstored(&p, "ad4m://FlowInstance")
            .unwrap()
            .is_some(),
        "the refused plan registered the class"
    );
}

/// A photo stored through a language of its own, and a caption on the literal language.
const PHOTO_SDNA: &str = r#"{
  "target_class": "ns://Photo",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Photo"}
  ],
  "properties": [
    {"path":"ns://image","name":"image","datatype":"xsd:string","max_count":1,"writable":true,"resolve_language":"QmPhotoLanguageAddress",
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://image","target":"value"}]},
    {"path":"ns://caption","name":"caption","datatype":"xsd:string","max_count":1,"writable":true,"resolve_language":"literal",
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://caption","target":"value"}]}
  ]
}"#;

/// A value for a property stored through a language of its own is refused before anything is
/// written: the language would keep it though the plan were refused later. One on the literal
/// language is written.
#[tokio::test(flavor = "multi_thread")]
async fn a_plan_setting_a_language_stored_property_writes_nothing() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("Note", NOTE_SDNA), ("Photo", PHOTO_SDNA)]).await;
    let before = p.get_links(&LinkQuery::default()).await.unwrap().len();
    let photo = |properties: Value| PlannedWrite::Create {
        reference: "photo".into(),
        class_name: "Photo".into(),
        properties: properties.as_object().cloned(),
        parent: None,
    };

    let refused = apply(
        &mut p,
        &ctx,
        vec![
            note("root", Some("root"), None, json!({})),
            photo(json!({ "image": "data:image/png;base64,AAAA", "caption": "hi" })),
        ],
    )
    .await;
    assert!(
        matches!(&refused, Err(PlanError::LanguageProperty { index: 1, property, .. }) if property == "image"),
        "{refused:?}"
    );
    assert_eq!(
        p.get_links(&LinkQuery::default()).await.unwrap().len(),
        before
    );

    apply(&mut p, &ctx, vec![photo(json!({ "caption": "hi" }))])
        .await
        .unwrap_or_else(|e| panic!("a literal-language property is written: {e:?}"));
}

/// The check before the batch reads the shape once. A shape registered or synced after it can
/// make a property the plan sets language-stored; the write refuses the value then, and nothing of
/// the plan is written. The check is skipped here, as such a shape change would leave it.
#[tokio::test(flavor = "multi_thread")]
async fn a_language_value_is_refused_at_the_write_when_the_check_before_missed_it() {
    use super::plan::SKIP_LANGUAGE_CHECK;
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("Note", NOTE_SDNA), ("Photo", PHOTO_SDNA)]).await;
    let before = p.get_links(&LinkQuery::default()).await.unwrap().len();

    let refused = SKIP_LANGUAGE_CHECK
        .scope(
            (),
            apply(
                &mut p,
                &ctx,
                vec![
                    note("root", Some("root"), None, json!({})),
                    PlannedWrite::Create {
                        reference: "photo".into(),
                        class_name: "Photo".into(),
                        properties: json!({ "image": "data:image/png;base64,AAAA" })
                            .as_object()
                            .cloned(),
                        parent: None,
                    },
                ],
            ),
        )
        .await;
    match &refused {
        Err(PlanError::Write { index: 1, error }) => {
            let text = format!("{error:?}");
            assert!(text.contains("stored through the language"), "{text}");
        }
        other => panic!("expected the photo refused at its write, got {other:?}"),
    }
    assert_eq!(
        p.get_links(&LinkQuery::default()).await.unwrap().len(),
        before
    );

    // Outside a plan the same value is written as before.
    super::write::create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "Photo".into(),
            properties: json!({ "image": "data:image/png;base64,AAAA" })
                .as_object()
                .cloned(),
            ..Default::default()
        },
    )
    .await
    .expect("instance_create still writes a language value");
}
