//! The validated writes against a real perspective.

use super::write::{
    add_to_collection, create, remove_from_collection, InstanceAddToCollection, InstanceCreate,
    InstanceRemoveFromCollection,
};
use crate::agent::AgentContext;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::LinkQuery;
use serde_json::{json, Map};

/// A required title (an instance is found by it), a boolean and an integer collection, stored as plain typed literals.
const COUNTED_SDNA: &str = r#"{
  "target_class": "ns://Counted",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Counted"}
  ],
  "properties": [
    {"path":"ns://counted_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://counted_title","target":"value"}]},
    {"path":"ns://flag","name":"flags","datatype":"xsd:boolean","collection":true,"writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://flag","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://flag","target":"value"}]},
    {"path":"ns://count","name":"counts","datatype":"xsd:integer","collection":true,"writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://count","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://count","target":"value"}]}
  ]
}"#;

async fn stored_targets(p: &PerspectiveInstance, source: &str, predicate: &str) -> Vec<String> {
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

async fn add(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    base: &str,
    collection: &str,
    item: &str,
) -> (usize, bool) {
    let added = add_to_collection(
        p,
        ctx,
        InstanceAddToCollection {
            class_name: "Counted".into(),
            base_uri: base.into(),
            collection: collection.into(),
            item_uri: item.into(),
            batch_id: None,
        },
    )
    .await
    .unwrap_or_else(|e| panic!("add {item:?} to {collection}: {e:?}"));
    (added.links_added, added.already_member)
}

async fn remove(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    base: &str,
    collection: &str,
    item: &str,
) -> usize {
    remove_from_collection(
        p,
        ctx,
        InstanceRemoveFromCollection {
            class_name: "Counted".into(),
            base_uri: base.into(),
            collection: collection.into(),
            item_uri: item.into(),
            batch_id: None,
        },
    )
    .await
    .unwrap_or_else(|e| panic!("remove {item:?} from {collection}: {e:?}"))
    .links_removed
}

/// The item is stored as the value validation coerced it to, so another spelling of the same
/// value (`TRUE` for `true`, `+005` for `5`) is already a member, and removes it.
#[tokio::test(flavor = "multi_thread")]
async fn collection_items_are_found_by_their_coerced_value() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Counted", COUNTED_SDNA)]).await;
    let base = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "Counted".into(),
            properties: Some(Map::from_iter([("title".to_string(), json!("t"))])),
            ..Default::default()
        },
    )
    .await
    .expect("create")
    .base_uri;

    assert_eq!(add(&mut p, &ctx, &base, "flags", "true").await, (1, false));
    assert_eq!(add(&mut p, &ctx, &base, "flags", "TRUE").await, (0, true));
    assert_eq!(
        stored_targets(&p, &base, "ns://flag").await,
        vec!["literal:boolean:true"]
    );

    assert_eq!(add(&mut p, &ctx, &base, "counts", "5").await, (1, false));
    assert_eq!(add(&mut p, &ctx, &base, "counts", "+005").await, (0, true));
    assert_eq!(
        stored_targets(&p, &base, "ns://count").await,
        vec!["literal:number:5"]
    );

    assert_eq!(remove(&mut p, &ctx, &base, "flags", "TRUE").await, 1);
    assert!(stored_targets(&p, &base, "ns://flag").await.is_empty());
    assert_eq!(remove(&mut p, &ctx, &base, "counts", "+005").await, 1);
    assert!(stored_targets(&p, &base, "ns://count").await.is_empty());
}

/// `resolve_class` reads the shape registry only: every key `class_keys`
/// gives resolves to its one class, a name two classes share to neither.
mod resolve_class {
    use super::*;
    use crate::perspectives::instances::resolve_class;
    use crate::types::{Link, LinkStatus};

    /// Two classes of one name, `title` required on each.
    const TASK_A_SDNA: &str = r#"{
  "target_class": "a://Task",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"a://Task"}
  ],
  "properties": [
    {"path":"a://task_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"a://task_title","target":"value"}]}
  ]
}"#;

    const TASK_B_SDNA: &str = r#"{
  "target_class": "b://Task",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"b://Task"}
  ],
  "properties": [
    {"path":"b://task_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"b://task_title","target":"value"}]}
  ]
}"#;

    /// `a://Task` and `b://Task` share the name `Task`; `Counted` is unique.
    async fn shared_name_perspective() -> (PerspectiveInstance, AgentContext) {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        for sdna in [TASK_A_SDNA, TASK_B_SDNA, COUNTED_SDNA] {
            p.register_shape_json(sdna, &ctx).await.expect("register");
        }
        (p, ctx)
    }

    /// Every key `class_keys()` gives resolves: a unique name, or a target
    /// class IRI when the name is shared.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_target_class_iri_resolves_to_its_class() {
        let (p, _ctx) = shared_name_perspective().await;
        let (key, shape) = resolve_class(&p, "b://Task")
            .await
            .expect("an IRI key resolves");
        assert_eq!(key, "b://Task");
        assert_eq!(shape.target_class, "b://Task");
    }

    /// A bare name two registered classes share resolves to neither — the
    /// refusal names both target class IRIs.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_name_two_classes_share_is_refused_as_ambiguous() {
        let (p, _ctx) = shared_name_perspective().await;
        let error = resolve_class(&p, "Task")
            .await
            .expect_err("a shared name resolves to neither class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "ambiguousClass", "{data}");
        assert_eq!(
            data["candidates"],
            json!(["a://Task", "b://Task"]),
            "{data}"
        );
    }

    /// The case-insensitive match is held to the same rule: onto a shared
    /// name it is refused, not resolved to one of them.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_case_insensitive_match_onto_a_shared_name_is_ambiguous() {
        let (p, _ctx) = shared_name_perspective().await;
        let error = resolve_class(&p, "task")
            .await
            .expect_err("a shared name resolves to neither class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "ambiguousClass", "{data}");
        assert_eq!(
            data["candidates"],
            json!(["a://Task", "b://Task"]),
            "{data}"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_case_insensitive_unique_name_still_resolves() {
        let (p, _ctx) = shared_name_perspective().await;
        let (key, shape) = resolve_class(&p, "cOuNtEd")
            .await
            .expect("a unique name resolves case-insensitively");
        assert_eq!(key, "Counted");
        assert_eq!(shape.target_class, "ns://Counted");
    }

    /// A class that exists only as an `rdf://type ad4m://SubjectClass` link
    /// (no registered shape) is not resolved.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_subject_class_marker_without_a_shape_is_not_a_class() {
        let (mut p, ctx) = shared_name_perspective().await;
        p.add_link(
            Link {
                source: "ns://Orphan".to_string(),
                predicate: Some("rdf://type".to_string()),
                target: "ad4m://SubjectClass".to_string(),
            },
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("marker link");

        let error = resolve_class(&p, "Orphan")
            .await
            .expect_err("a marker without a shape is not a class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "unknownClass", "{data}");
        let error = resolve_class(&p, "ns://Orphan")
            .await
            .expect_err("a marker without a shape is not a class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "unknownClass", "{data}");
    }

    /// `UnknownClass` lists the class keys — an IRI for each class whose name
    /// is shared.
    #[tokio::test(flavor = "multi_thread")]
    async fn an_unknown_class_lists_the_class_keys() {
        let (p, _ctx) = shared_name_perspective().await;
        let error = resolve_class(&p, "Nope").await.expect_err("unknown class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "unknownClass", "{data}");
        assert_eq!(
            data["registered"],
            json!(["Counted", "a://Task", "b://Task"]),
            "{data}"
        );
    }

    /// The rule end to end: `Task` is refused, `b://Task` creates against
    /// `b://Task`'s shape.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_shared_name_create_is_refused_but_the_iri_creates() {
        let (mut p, ctx) = shared_name_perspective().await;
        let properties = || Some(Map::from_iter([("title".to_string(), json!("t"))]));
        let error = create(
            &mut p,
            &ctx,
            InstanceCreate {
                class_name: "Task".into(),
                properties: properties(),
                ..Default::default()
            },
        )
        .await
        .expect_err("a shared name resolves to neither class");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "ambiguousClass", "{data}");

        let created = create(
            &mut p,
            &ctx,
            InstanceCreate {
                class_name: "b://Task".into(),
                properties: properties(),
                ..Default::default()
            },
        )
        .await
        .expect("an IRI key resolves");
        assert_eq!(created.class_name, "b://Task");
    }
}

mod decoded_literal {
    use super::super::decoded_literal;
    use serde_json::{json, Value};

    fn json_literal(value: &Value) -> String {
        format!("literal:json:{}", urlencoding::encode(&value.to_string()))
    }

    fn decoded_json(target: &str) -> Value {
        let text = decoded_literal(target);
        serde_json::from_str(&text).unwrap_or(Value::String(text))
    }

    /// A signed envelope decodes to its payload once: a payload with a `data` key of its own,
    /// or a plain JSON literal with one, stays whole.
    #[test]
    fn a_payloads_own_data_key_is_not_unwrapped() {
        let payload = json!({"data": "inner", "note": "x"});
        assert_eq!(decoded_json(&json_literal(&payload)), payload);

        let envelope = json!({
            "author": "did:key:z6Mk",
            "timestamp": "2026-11-17T00:00:00Z",
            "data": payload,
            "proof": {"signature": "s", "key": "k"},
        });
        assert_eq!(decoded_json(&json_literal(&envelope)), payload);
    }

    #[test]
    fn a_signed_envelope_decodes_to_its_payload() {
        let envelope = json!({
            "author": "did:key:z6Mk",
            "timestamp": "2026-11-17T00:00:00Z",
            "data": "hello",
            "proof": {"signature": "s", "key": "k"},
        });
        assert_eq!(decoded_literal(&json_literal(&envelope)), "hello");
        assert_eq!(decoded_literal("literal:string:a%20b"), "a b");
        assert_eq!(decoded_literal("literal:boolean:true"), "true");
        assert_eq!(decoded_literal("ns://item"), "ns://item");
    }
}
