//! The validated writes against a real perspective.

use super::write::{
    add_to_collection, create, remove_from_collection, update, InstanceAddToCollection,
    InstanceCreate, InstanceRemoveFromCollection, InstanceUpdate,
};
use super::{InstanceError, ValidationProblem};
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

/// A hard-wired class is the executor's to register: the first `create` of
/// one on a perspective that has never stored it registers it and writes.
#[tokio::test(flavor = "multi_thread")]
async fn the_first_create_of_a_hard_wired_class_registers_it() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let created = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "FlowInstance".into(),
            properties: Some(Map::from_iter([
                ("flowUri".to_string(), json!("ns://flow")),
                ("subject".to_string(), json!("ns://subject")),
            ])),
            ..Default::default()
        },
    )
    .await
    .expect("a hard-wired class needs no registration");
    assert_eq!(
        stored_targets(&p, &created.base_uri, "ad4m://flow/base").await,
        vec!["ns://subject"]
    );
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

    /// A class with one required `title`, registered at `target_class`.
    fn titled_sdna(target_class: &str) -> String {
        format!(
            r#"{{
  "target_class": "{target_class}",
  "constructor_actions": [
    {{"action":"addLink","source":"this","predicate":"rdf://type","target":"{target_class}"}}
  ],
  "properties": [
    {{"path":"{target_class}.title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{{"action":"setSingleTarget","source":"this","predicate":"{target_class}.title","target":"value"}}]}}
  ]
}}"#
        )
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

    /// The fold is Unicode, not ASCII: `ns://Ärger` answers to `ärger`.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_non_ascii_name_resolves_through_its_lowercase_fold() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        p.register_shape_json(&titled_sdna("ns://Ärger"), &ctx)
            .await
            .expect("register");
        let (key, shape) = resolve_class(&p, "ärger")
            .await
            .expect("a unique name resolves through its lowercase fold");
        assert_eq!(key, "Ärger");
        assert_eq!(shape.target_class, "ns://Ärger");
    }

    /// `a://Ärger` and `b://Ärger` fold to one non-ASCII spelling: asked by
    /// it, the refusal names both IRIs rather than resolving to one.
    #[tokio::test(flavor = "multi_thread")]
    async fn two_names_folding_to_one_non_ascii_spelling_are_ambiguous() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        for iri in ["a://Ärger", "b://Ärger"] {
            p.register_shape_json(&titled_sdna(iri), &ctx)
                .await
                .expect("register");
        }
        let error = resolve_class(&p, "ärger")
            .await
            .expect_err("two classes folding to one name resolves to neither");
        let data = serde_json::to_value(&error).expect("serialize");
        assert_eq!(data["kind"], "ambiguousClass", "{data}");
        assert_eq!(
            data["candidates"],
            json!(["a://Ärger", "b://Ärger"]),
            "{data}"
        );
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

/// `Counted` with `counts` written through a declared `collectionSetter`.
const SET_COUNTED_SDNA: &str = r#"{
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
     "setter":[{"action":"collectionSetter","source":"this","predicate":"ns://count","target":"value"}],
     "adder":[{"action":"addLink","source":"this","predicate":"ns://count","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://count","target":"value"}]}
  ]
}"#;

/// An `update` patch names the full member list of a collection with a
/// declared `ad4m://setter` — members not in the list leave, `[]` clears. A
/// collection with no setter is refused (`collectionNotSettable`): its
/// members change through `add_to_collection` / `remove_from_collection`.
#[tokio::test(flavor = "multi_thread")]
async fn update_replaces_a_collections_member_list() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("Counted", SET_COUNTED_SDNA)]).await;
    let base = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "Counted".into(),
            properties: Some(Map::from_iter([
                ("title".to_string(), json!("t")),
                ("counts".to_string(), json!([1, 2])),
                ("flags".to_string(), json!([true])),
            ])),
            ..Default::default()
        },
    )
    .await
    .expect("create")
    .base_uri;

    assert_eq!(
        stored_targets(&p, &base, "ns://count").await,
        vec!["literal:number:1", "literal:number:2"]
    );

    update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "Counted".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("counts".to_string(), json!([2, 3]))]),
            batch_id: None,
        },
    )
    .await
    .expect("update");
    assert_eq!(
        stored_targets(&p, &base, "ns://count").await,
        vec!["literal:number:2", "literal:number:3"],
        "the member list is replaced, not appended"
    );

    update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "Counted".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("counts".to_string(), json!([]))]),
            batch_id: None,
        },
    )
    .await
    .expect("update");
    assert!(
        stored_targets(&p, &base, "ns://count").await.is_empty(),
        "an empty list clears"
    );
    // The untouched collection stays.
    assert_eq!(
        stored_targets(&p, &base, "ns://flag").await,
        vec!["literal:boolean:true"]
    );

    let refused = update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "Counted".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("flags".to_string(), json!([]))]),
            batch_id: None,
        },
    )
    .await;
    match refused {
        Err(InstanceError::Validation { errors, .. }) => {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert_eq!(errors[0].property, "flags");
            assert!(
                matches!(errors[0].problem, ValidationProblem::CollectionNotSettable),
                "{errors:?}"
            );
        }
        other => panic!("a setter-less collection must be refused on update: {other:?}"),
    }
    assert_eq!(
        stored_targets(&p, &base, "ns://flag").await,
        vec!["literal:boolean:true"],
        "the refused update wrote nothing"
    );
}

/// A scalar `hasOne` relation: `adder`/`remover`, no `ad4m://setter`.
const PINNED_SDNA: &str = r#"{
  "target_class": "ns://PinPost",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://PinPost"}
  ],
  "properties": [
    {"path":"ns://pin_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://pin_title","target":"value"}]},
    {"path":"ns://pinned","name":"pinned","relation_kind":"hasOne","target_class_name":"Comment",
     "class":"ns://CommentShape","node_kind":"IRI","max_count":1,"writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://pinned","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://pinned","target":"value"}]}
  ]
}"#;

/// A `hasOne` relation writes through its `adder`/`remover` pair: a scalar in
/// the patch replaces the one member, so the old pin leaves when a new one
/// is set.
#[tokio::test(flavor = "multi_thread")]
async fn update_replaces_a_scalar_relation_member() {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[("PinPost", PINNED_SDNA)]).await;
    let base = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "PinPost".into(),
            properties: Some(Map::from_iter([("title".to_string(), json!("t"))])),
            ..Default::default()
        },
    )
    .await
    .expect("create")
    .base_uri;

    update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "PinPost".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("pinned".to_string(), json!("ns://c1"))]),
            batch_id: None,
        },
    )
    .await
    .expect("update");
    assert_eq!(
        stored_targets(&p, &base, "ns://pinned").await,
        vec!["ns://c1"]
    );

    update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "PinPost".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("pinned".to_string(), json!("ns://c2"))]),
            batch_id: None,
        },
    )
    .await
    .expect("update");
    assert_eq!(
        stored_targets(&p, &base, "ns://pinned").await,
        vec!["ns://c2"],
        "the scalar relation replaces its one member"
    );
}

/// [`PINNED_SDNA`] with a `call` adder and remover on `pinned`: a language
/// writes the pin.
const CALL_PINNED_SDNA: &str = r#"{
  "target_class": "ns://CallPinPost",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://CallPinPost"}
  ],
  "properties": [
    {"path":"ns://pin_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://pin_title","target":"value"}]},
    {"path":"ns://pinned","name":"pinned","relation_kind":"hasOne","target_class_name":"Comment",
     "class":"ns://CommentShape","node_kind":"IRI","max_count":1,"writable":true,
     "adder":[{"action":"call","source":"this","predicate":"pin","target":"{}"}],
     "remover":[{"action":"call","source":"this","predicate":"unpin","target":"{}"}]}
  ]
}"#;

/// A scalar whose adder and remover are `call`s is not written as raw
/// links: the one-member replace through a synthesized `collectionSetter`
/// would never reach the language. Create and update refuse it before
/// they write anything.
#[tokio::test(flavor = "multi_thread")]
async fn a_scalar_with_a_call_adder_and_remover_is_not_written_as_links() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("CallPinPost", CALL_PINNED_SDNA)]).await;
    let refused_create = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "CallPinPost".into(),
            properties: Some(Map::from_iter([
                ("title".to_string(), json!("t")),
                ("pinned".to_string(), json!("ns://c1")),
            ])),
            ..Default::default()
        },
    )
    .await;
    assert!(
        matches!(refused_create, Err(InstanceError::Validation { .. })),
        "{refused_create:?}"
    );

    let base = create(
        &mut p,
        &ctx,
        InstanceCreate {
            class_name: "CallPinPost".into(),
            properties: Some(Map::from_iter([("title".to_string(), json!("t"))])),
            ..Default::default()
        },
    )
    .await
    .expect("create")
    .base_uri;
    let refused = update(
        &mut p,
        &ctx,
        InstanceUpdate {
            class_name: "CallPinPost".into(),
            base_uri: base.clone(),
            properties: Map::from_iter([("pinned".to_string(), json!("ns://c1"))]),
            batch_id: None,
        },
    )
    .await;
    match refused {
        Err(InstanceError::Validation { errors, .. }) => {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert_eq!(errors[0].property, "pinned");
        }
        other => panic!("a call-pair scalar must be refused: {other:?}"),
    }
    let pinned = p
        .get_links(&LinkQuery {
            predicate: Some("ns://pinned".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert!(pinned.is_empty(), "a raw pin link was written: {pinned:?}");
}
