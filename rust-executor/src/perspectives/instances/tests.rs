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

/// A class with a destructor (it removes the type link only), an ordered relation and a plain
/// one.
const NOTE_SDNA: &str = r#"{
  "target_class": "ns://Note",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Note"}
  ],
  "destructor_actions": [
    {"action":"removeLink","source":"this","predicate":"rdf://type","target":"ns://Note"}
  ],
  "properties": [
    {"path":"ns://note_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://note_title","target":"value"}]},
    {"path":"ns://note_body","name":"body","datatype":"xsd:string","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://note_body","target":"value"}]},
    {"path":"ns://note_tag","name":"tags","collection":true,"writable":true,
     "setter":[{"action":"collectionSetter","source":"this","predicate":"ns://note_tag","target":"value"}],
     "adder":[{"action":"addLink","source":"this","predicate":"ns://note_tag","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://note_tag","target":"value"}]},
    {"path":"ns://note_child","name":"children","collection":true,"writable":true,"ordering":"linkedList",
     "setter":[{"action":"collectionSetter","source":"this","predicate":"ns://note_child","target":"value"}],
     "adder":[{"action":"addLink","source":"this","predicate":"ns://note_child","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://note_child","target":"value"}]}
  ]
}"#;

/// No destructor: a delete removes the links of the declared predicates.
const MEMO_SDNA: &str = r#"{
  "target_class": "ns://Memo",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Memo"}
  ],
  "properties": [
    {"path":"ns://memo_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://memo_title","target":"value"}]}
  ]
}"#;

async fn run(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    ops: serde_json::Value,
) -> Result<Vec<super::mutate::Mutated>, super::MutateError> {
    super::mutate(p, ctx, serde_json::from_value(ops).expect("ops"), None).await
}

async fn link(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    source: &str,
    predicate: &str,
    target: &str,
) {
    p.add_link(
        crate::types::Link {
            source: source.into(),
            predicate: Some(predicate.into()),
            target: target.into(),
        },
        crate::types::LinkStatus::Shared,
        None,
        ctx,
    )
    .await
    .expect("add_link");
}

mod mutate_rules {
    use super::*;

    async fn note(
        p: &mut PerspectiveInstance,
        ctx: &AgentContext,
        id: &str,
        values: serde_json::Value,
    ) {
        run(
            p,
            ctx,
            json!([{"op":"create","class":"Note","id":id,"values":values}]),
        )
        .await
        .unwrap_or_else(|e| panic!("create {id}: {:?}", e.error));
    }

    /// `null` and `""` clear a single-valued property on update.
    #[tokio::test(flavor = "multi_thread")]
    async fn null_and_empty_string_clear_a_property_on_update() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(&mut p, &ctx, "ns://n1", json!({"title":"t","body":"b"})).await;
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_body").await.len(),
            1
        );

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1","set":{"body":null}}]),
        )
        .await
        .expect("null clears");
        assert!(stored_targets(&p, "ns://n1", "ns://note_body")
            .await
            .is_empty());

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1","set":{"body":"again"}}]),
        )
        .await
        .expect("set");
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_body").await.len(),
            1
        );
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1","set":{"body":""}}]),
        )
        .await
        .expect("empty string clears");
        assert!(stored_targets(&p, "ns://n1", "ns://note_body")
            .await
            .is_empty());
    }

    /// A required property is not cleared, and the call writes nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_required_property_is_not_cleared() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(&mut p, &ctx, "ns://n1", json!({"title":"t"})).await;
        let refused = run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1","set":{"title":null}}]),
        )
        .await
        .err()
        .expect("required");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::Validation { .. }
            ),
            "{:?}",
            refused.error
        );
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_title").await.len(),
            1
        );
    }

    /// `null` and `""` on create write nothing for the property.
    #[tokio::test(flavor = "multi_thread")]
    async fn null_and_empty_string_on_create_write_nothing() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(
            &mut p,
            &ctx,
            "ns://n1",
            json!({"title":"t","body":null,"tags":null}),
        )
        .await;
        note(&mut p, &ctx, "ns://n2", json!({"title":"t","body":""})).await;
        for id in ["ns://n1", "ns://n2"] {
            assert!(
                stored_targets(&p, id, "ns://note_body").await.is_empty(),
                "{id}"
            );
            assert!(
                stored_targets(&p, id, "ns://note_tag").await.is_empty(),
                "{id}"
            );
        }
    }

    /// An array on a relation sets its members: missing ones are added, extra ones removed;
    /// `[]` empties it.
    #[tokio::test(flavor = "multi_thread")]
    async fn an_array_sets_the_members_of_a_relation() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(
            &mut p,
            &ctx,
            "ns://n1",
            json!({"title":"t","tags":["ns://a","ns://b"]}),
        )
        .await;
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_tag").await,
            vec!["ns://a", "ns://b"]
        );

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1",
                    "set":{"tags":["ns://b","ns://c"]}}]),
        )
        .await
        .expect("set members");
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_tag").await,
            vec!["ns://b", "ns://c"]
        );

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n1","set":{"tags":[]}}]),
        )
        .await
        .expect("empty");
        assert!(stored_targets(&p, "ns://n1", "ns://note_tag")
            .await
            .is_empty());
    }

    /// How many ordering entries `source` holds, and the order the strategy reads its members
    /// in.
    async fn ordered(p: &PerspectiveInstance, source: &str) -> (usize, Vec<String>) {
        let members: Vec<(String, String)> = p
            .get_links(&LinkQuery {
                source: Some(source.into()),
                predicate: Some("ns://note_child".into()),
                ..Default::default()
            })
            .await
            .expect("members")
            .into_iter()
            .map(|l| (l.data.target, l.timestamp))
            .collect();
        let targets: Vec<String> = p
            .get_links(&LinkQuery {
                source: Some(source.into()),
                predicate: Some(crate::perspectives::ordering::COLLECTION_ORDER_PREDICATE.into()),
                ..Default::default()
            })
            .await
            .expect("entries")
            .into_iter()
            .map(|l| l.data.target)
            .collect();
        let entries =
            crate::perspectives::ordering::parse_ordering_entries(&targets, "ns://note_child");
        let strategy =
            crate::perspectives::ordering::create_strategy("linkedList").expect("strategy");
        (entries.len(), strategy.reconstruct(&members, &entries))
    }

    /// A create with a relation parent, and an add, write the ordering entries; the members
    /// read back in the order they were put in, through the ordering entries. The parent's
    /// class comes from the parent instance.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_relation_parent_and_an_add_write_ordering_entries() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(&mut p, &ctx, "ns://folder", json!({"title":"folder"})).await;
        let parent = json!({"id":"ns://folder","relation":"children"});
        run(
            &mut p,
            &ctx,
            json!([
                {"op":"create","class":"Note","id":"ns://c1","values":{"title":"1"},"parent":parent},
                {"op":"create","class":"Note","id":"ns://c2","values":{"title":"2"},"parent":parent},
                {"op":"create","class":"Note","id":"ns://c3","values":{"title":"3"},"parent":parent}
            ]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        let (entries, order) = ordered(&p, "ns://folder").await;
        assert!(entries > 0, "the create wrote no ordering entries");
        assert_eq!(order, vec!["ns://c1", "ns://c2", "ns://c3"]);

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://folder",
                    "add":{"children":["ns://c4"]}}]),
        )
        .await
        .expect("add");
        assert_eq!(
            ordered(&p, "ns://folder").await.1,
            vec!["ns://c1", "ns://c2", "ns://c3", "ns://c4"]
        );
    }

    /// A parent relation without `model` is refused when no class of the parent declares it.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_relation_parent_names_a_class_that_declares_the_relation() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(&mut p, &ctx, "ns://folder", json!({"title":"folder"})).await;
        let refused = run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Note","values":{"title":"x"},
                    "parent":{"id":"ns://folder","relation":"nope"}}]),
        )
        .await
        .err()
        .expect("no class holds it");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::ParentClassUnresolved { .. }
            ),
            "{:?}",
            refused.error
        );
    }

    /// A delete runs the class's destructor (here it removes the type link only: the title
    /// stays) and removes the links that point at the instance.
    #[tokio::test(flavor = "multi_thread")]
    async fn delete_runs_the_destructor_and_removes_incoming_links() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        note(&mut p, &ctx, "ns://n1", json!({"title":"t"})).await;
        link(&mut p, &ctx, "ns://other", "ns://points_at", "ns://n1").await;

        run(
            &mut p,
            &ctx,
            json!([{"op":"delete","class":"Note","id":"ns://n1"}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert!(stored_targets(&p, "ns://n1", "rdf://type").await.is_empty());
        assert_eq!(
            stored_targets(&p, "ns://n1", "ns://note_title").await.len(),
            1
        );
        assert!(stored_targets(&p, "ns://other", "ns://points_at")
            .await
            .is_empty());
    }

    /// Without a destructor a delete removes the links of the declared predicates; a link of
    /// no declared predicate stays. What points at the instance goes.
    #[tokio::test(flavor = "multi_thread")]
    async fn delete_without_a_destructor_removes_the_declared_predicates() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Memo", MEMO_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Memo","id":"ns://m1","values":{"title":"t"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        link(&mut p, &ctx, "ns://m1", "ns://undeclared", "ns://x").await;
        link(&mut p, &ctx, "ns://other", "ns://points_at", "ns://m1").await;

        run(
            &mut p,
            &ctx,
            json!([{"op":"delete","class":"Memo","id":"ns://m1"}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert!(stored_targets(&p, "ns://m1", "ns://memo_title")
            .await
            .is_empty());
        assert_eq!(
            stored_targets(&p, "ns://m1", "ns://undeclared").await,
            vec!["ns://x"]
        );
        assert!(stored_targets(&p, "ns://other", "ns://points_at")
            .await
            .is_empty());
    }
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
