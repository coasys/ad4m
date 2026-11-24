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

/// A required collection.
const LABELLED_SDNA: &str = r#"{
  "target_class": "ns://Labelled",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Labelled"}
  ],
  "properties": [
    {"path":"ns://label","name":"labels","collection":true,"min_count":1,"writable":true,
     "setter":[{"action":"collectionSetter","source":"this","predicate":"ns://label","target":"value"}],
     "adder":[{"action":"addLink","source":"this","predicate":"ns://label","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://label","target":"value"}]}
  ]
}"#;

/// A single-valued relation (what `@HasOne` registers): one target, no datatype.
const OWNED_SDNA: &str = r#"{
  "target_class": "ns://Owned",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Owned"}
  ],
  "properties": [
    {"path":"ns://owner","name":"owner","node_kind":"IRI","relation_kind":"hasOne","max_count":1,
     "target_class_name":"Owned","writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://owner","target":"value"}]}
  ]
}"#;

/// A `@HasOne` as the SDK registers it: an adder and a remover, no setter.
const OWNED_ADDER_SDNA: &str = r#"{
  "target_class": "ns://OwnedByAdder",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://OwnedByAdder"}
  ],
  "properties": [
    {"path":"ns://owner","name":"owner","node_kind":"IRI","relation_kind":"hasOne","max_count":1,
     "target_class_name":"OwnedByAdder","writable":true,
     "adder":[{"action":"addLink","source":"this","predicate":"ns://owner","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://owner","target":"value"}]}
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

    /// A parent created by an earlier op of the same call is found without a `model`: its
    /// class is the one the call created it as.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_parent_created_in_the_same_call_needs_no_model() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([
                {"op":"create","class":"Note","id":"ns://folder","values":{"title":"f"}},
                {"op":"create","class":"Note","id":"ns://child","values":{"title":"c"},
                 "parent":{"id":"ns://folder","relation":"children"}}
            ]),
        )
        .await
        .unwrap_or_else(|e| panic!("op {:?}: {:?}", e.op_index, e.error));
        assert_eq!(
            stored_targets(&p, "ns://folder", "ns://note_child").await,
            vec!["ns://child"]
        );
    }

    /// `remove` names the one member of a single-valued relation (the SDK's `removeOwner(id)`):
    /// it drops that link and leaves the relation empty; `add` on it is refused, the SDK sends a
    /// `set` for it.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_single_valued_relation_member_is_removed_and_not_added() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Owned", OWNED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Owned","id":"ns://o1","values":{"owner":"ns://a"}}]),
        )
        .await
        .expect("create");
        let refused = run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Owned","id":"ns://o1","add":{"owner":["ns://b"]}}]),
        )
        .await
        .err()
        .expect("an add on a single-valued relation is refused");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::Validation { .. }
            ),
            "{:?}",
            refused.error
        );
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Owned","id":"ns://o1","remove":{"owner":["ns://a"]}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert!(stored_targets(&p, "ns://o1", "ns://owner").await.is_empty());
    }

    /// A single-valued relation takes one id on create, a new id on update and `null` to
    /// clear, as the SDK sends them; an array is refused, so the SDK must not send one.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_single_valued_relation_takes_one_id_not_an_array() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Owned", OWNED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Owned","id":"ns://o1","values":{"owner":"ns://a"}}]),
        )
        .await
        .expect("create with one id");
        assert_eq!(
            stored_targets(&p, "ns://o1", "ns://owner").await,
            vec!["ns://a"]
        );

        let refused = run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Owned","id":"ns://o1","set":{"owner":["ns://b"]}}]),
        )
        .await
        .err()
        .expect("an array is refused");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::Validation { .. }
            ),
            "{:?}",
            refused.error
        );

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Owned","id":"ns://o1","set":{"owner":"ns://b"}}]),
        )
        .await
        .expect("replace with one id");
        assert_eq!(
            stored_targets(&p, "ns://o1", "ns://owner").await,
            vec!["ns://b"]
        );

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Owned","id":"ns://o1","set":{"owner":null}}]),
        )
        .await
        .expect("null clears");
        assert!(stored_targets(&p, "ns://o1", "ns://owner").await.is_empty());
    }

    /// The same writes on a `@HasOne` that has an adder and no setter: `set` with one id replaces
    /// it and `set` with `null` clears it (the SDK's `relationWrite`), as `null` on a derived
    /// collection setter would be refused.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_hasone_without_a_setter_is_replaced_and_cleared_by_set() {
        let (mut p, _s, ctx) =
            setup_perspective_no_llm(&[("OwnedByAdder", OWNED_ADDER_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"OwnedByAdder","id":"ns://o1","values":{"owner":"ns://a"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"OwnedByAdder","id":"ns://o1","set":{"owner":"ns://b"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://o1", "ns://owner").await,
            vec!["ns://b"]
        );
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"OwnedByAdder","id":"ns://o1","set":{"owner":null}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert!(stored_targets(&p, "ns://o1", "ns://owner").await.is_empty());
    }

    /// A required collection is not emptied on update, whichever way the empty set is spelled;
    /// a non-empty set is taken.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_required_collection_is_not_emptied() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Labelled", LABELLED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Labelled","id":"ns://l1","values":{"labels":["ns://a"]}}]),
        )
        .await
        .expect("create");
        for empty in [json!(null), json!(""), json!([])] {
            let refused = run(
                &mut p,
                &ctx,
                json!([{"op":"update","class":"Labelled","id":"ns://l1","set":{"labels":empty}}]),
            )
            .await
            .err()
            .unwrap_or_else(|| panic!("{empty} emptied a required collection"));
            assert!(
                matches!(
                    refused.error,
                    super::super::InstanceError::Validation { .. }
                ),
                "{:?}",
                refused.error
            );
            assert_eq!(
                stored_targets(&p, "ns://l1", "ns://label").await,
                vec!["ns://a"]
            );
        }
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Labelled","id":"ns://l1","set":{"labels":["ns://b"]}}]),
        )
        .await
        .expect("a non-empty set");
        assert_eq!(
            stored_targets(&p, "ns://l1", "ns://label").await,
            vec!["ns://b"]
        );
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

    /// A create whose parent names a single-valued relation (`@HasOne`) sets that relation to the
    /// new instance, replacing the one it had.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_relation_parent_on_a_single_valued_relation_sets_it() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Owned", OWNED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Owned","id":"ns://o1","values":{"owner":"ns://a"}}]),
        )
        .await
        .expect("create");
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Owned","id":"ns://o2","values":{},
                        "parent":{"id":"ns://o1","relation":"owner","model":"Owned"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://o1", "ns://owner").await,
            vec!["ns://o2"]
        );
    }

    ///

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

    /// What the SDK's relation decorators register: an adder and a remover, no setter. An
    /// array on such a relation sets its members on create and on update, as on one with a
    /// `collectionSetter`.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_relation_with_an_adder_and_no_setter_is_writable() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Counted", COUNTED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Counted","id":"ns://c1",
                    "values":{"title":"t","counts":[1,2]}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("create: {:?}", e.error));
        assert_eq!(stored_targets(&p, "ns://c1", "ns://count").await.len(), 2);

        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Counted","id":"ns://c1","set":{"counts":[2,3]}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("update: {:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://c1", "ns://count").await,
            vec!["literal:number:2", "literal:number:3"]
        );
    }

    /// A collection whose own setter is only addLink actions: setting it replaces the members,
    /// so an update removes those not named, in the batch's effective graph too.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_collection_setter_made_of_add_links_replaces_the_members() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Listed", LISTED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Listed","id":"ns://l1","values":{"items":[1,2]}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("create: {:?}", e.error));
        run(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Listed","id":"ns://l1","set":{"items":[2,3]}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("update: {:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://l1", "ns://item").await,
            vec!["literal:number:2", "literal:number:3"]
        );
        // The same call that stages members and replaces them leaves only the last set.
        run(
            &mut p,
            &ctx,
            json!([
                {"op":"update","class":"Listed","id":"ns://l1","set":{"items":[7]}},
                {"op":"update","class":"Listed","id":"ns://l1","set":{"items":[8,9]}}
            ]),
        )
        .await
        .unwrap_or_else(|e| panic!("two updates: {:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://l1", "ns://item").await,
            vec!["literal:number:8", "literal:number:9"]
        );
    }

    /// One call may create the same id as two classes; each later op finds it as either, and
    /// names a class the way it likes (local name or IRI).
    #[tokio::test(flavor = "multi_thread")]
    async fn a_later_op_finds_what_an_earlier_op_created_under_any_name_of_its_class() {
        let (mut p, _s, ctx) =
            setup_perspective_no_llm(&[("Counted", COUNTED_SDNA), ("Listed", LISTED_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([
                {"op":"create","class":"Counted","id":"ns://both","values":{"title":"t"}},
                {"op":"create","class":"Listed","id":"ns://both","values":{}},
                {"op":"update","class":"Counted","id":"ns://both","set":{"title":"u"}},
                {"op":"update","class":"ns://Listed","id":"ns://both","set":{"items":[1]}},
                {"op":"update","class":"ns://Counted","id":"ns://both","set":{"title":"v"}}
            ]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://both", "ns://counted_title").await,
            vec!["literal:string:v"]
        );
        assert_eq!(
            stored_targets(&p, "ns://both", "ns://item").await,
            vec!["literal:number:1"]
        );
    }

    /// A required property the class constructor writes an initial value for is held on a
    /// create that gives none; one without an initial value is still missing.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_required_property_with_an_initial_value_needs_no_value_on_create() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Initial", INITIAL_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Initial","id":"ns://i1","values":{"name":"n"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("create: {:?}", e.error));
        assert_eq!(
            stored_targets(&p, "ns://i1", "ns://initial_note")
                .await
                .len(),
            1
        );

        let refused = run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Initial","id":"ns://i2","values":{"note":"n"}}]),
        )
        .await
        .err()
        .expect("`name` has no initial value");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::Validation { .. }
            ),
            "{:?}",
            refused.error
        );
    }

    /// A string property holds a JSON object (the SDK stores an object-typed field as one),
    /// and a property signed into the literal language holds any JSON value.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_json_object_is_a_value_of_a_string_or_literal_property() {
        let (p, _s, _ctx) = setup_perspective_no_llm(&[("Doc", DOC_SDNA)]).await;
        let shape = p.get_shape("Doc").expect("shape");
        let validate = |values: serde_json::Value, json_objects: bool| {
            crate::perspectives::instances::validate_properties(
                &shape,
                values.as_object().expect("object"),
                crate::perspectives::instances::WriteMode::Create,
                json_objects,
            )
        };
        let validated = validate(
            json!({"meta":{"a":1},"content":{"b":[1,2]},"list":[1,"x"]}),
            true,
        )
        .unwrap_or_else(|e| panic!("{e:?}"));
        assert_eq!(validated.scalars["meta"], json!({"a":1}));
        assert_eq!(validated.scalars["content"], json!({"b":[1,2]}));
        assert_eq!(validated.scalars["list"], json!([1, "x"]));
        // A number property still refuses an object.
        let errors =
            validate(json!({"size":{"a":1}}), true).expect_err("an object is not a number");
        assert_eq!(
            errors[0].problem,
            crate::perspectives::instances::ValidationProblem::WrongType
        );
        // A write that does not take objects (the MCP tools) refuses one for a string property,
        // and still takes one for the literal-language property.
        let errors = validate(json!({"meta":{"a":1}}), false).expect_err("prose for a string");
        assert_eq!(
            errors[0].problem,
            crate::perspectives::instances::ValidationProblem::WrongType
        );
        validate(json!({"content":{"b":1}}), false).expect("the literal language takes any JSON");
    }
}

/// A collection with a setter of its own that only appends links.
const LISTED_SDNA: &str = r#"{
  "target_class": "ns://Listed",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Listed"}
  ],
  "properties": [
    {"path":"ns://item","name":"items","datatype":"xsd:integer","collection":true,"writable":true,
     "setter":[{"action":"addLink","source":"this","predicate":"ns://item","target":"value"}],
     "adder":[{"action":"addLink","source":"this","predicate":"ns://item","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ns://item","target":"value"}]}
  ]
}"#;

/// A required property the constructor gives an initial value, and one it does not.
const INITIAL_SDNA: &str = r#"{
  "target_class": "ns://Initial",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Initial"},
    {"action":"addLink","source":"this","predicate":"ns://initial_note","target":"literal:string:none"}
  ],
  "properties": [
    {"path":"ns://initial_note","name":"note","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://initial_note","target":"value"}]},
    {"path":"ns://initial_name","name":"name","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://initial_name","target":"value"}]}
  ]
}"#;

/// A string property, a literal-language property and a number.
const DOC_SDNA: &str = r#"{
  "target_class": "ns://Doc",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Doc"}
  ],
  "properties": [
    {"path":"ns://doc_meta","name":"meta","datatype":"xsd:string","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://doc_meta","target":"value"}]},
    {"path":"ns://doc_content","name":"content","datatype":"xsd:string","resolve_language":"literal","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://doc_content","target":"value"}]},
    {"path":"ns://doc_list","name":"list","datatype":"xsd:string","resolve_language":"literal","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://doc_list","target":"value"}]},
    {"path":"ns://doc_size","name":"size","datatype":"xsd:decimal","max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://doc_size","target":"value"}]}
  ]
}"#;

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

/// A `mutate` call into a caller's batch is all or none against everything that touches the
/// batch beside it, not only against other `mutate` calls. The hook runs after op 0 of the call.
mod caller_batch {
    use super::*;
    use crate::types::{Link, LinkStatus};

    /// Create `ns://kept`, then update an instance that does not exist: the call fails at op 1.
    fn failing_ops() -> serde_json::Value {
        json!([
            {"op":"create","class":"Note","id":"ns://kept","values":{"title":"t"}},
            {"op":"update","class":"Note","id":"ns://missing","set":{"title":"x"}}
        ])
    }

    async fn call(
        p: &mut PerspectiveInstance,
        ctx: &AgentContext,
        ops: serde_json::Value,
        batch: &str,
    ) -> Result<Vec<super::super::mutate::Mutated>, super::super::MutateError> {
        super::super::mutate(
            p,
            ctx,
            serde_json::from_value(ops).expect("ops"),
            Some(batch.to_string()),
        )
        .await
    }

    /// The caller commits the batch between op 0 and the failing op 1: op 0 is not published.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_commit_of_the_batch_in_flight_does_not_publish_a_failed_call() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                other.commit_batch(id, &other_ctx).await.expect("commit");
            })
        }));

        let failed = call(&mut p, &ctx, failing_ops(), &batch)
            .await
            .err()
            .expect("op 1 fails");
        assert_eq!(failed.op_index, Some(1), "{:?}", failed.error);
        assert!(
            stored_targets(&p, "ns://kept", "rdf://type")
                .await
                .is_empty(),
            "op 0 of the failed call was published by the commit"
        );
    }

    /// The caller commits the batch while a call that would succeed is in flight: the call
    /// fails as a whole (the batch is gone) and publishes nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_commit_of_the_batch_in_flight_fails_the_call_whole() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                other.commit_batch(id, &other_ctx).await.expect("commit");
            })
        }));

        let failed = call(
            &mut p,
            &ctx,
            json!([
                {"op":"create","class":"Note","id":"ns://kept","values":{"title":"t"}},
                {"op":"create","class":"Note","id":"ns://second","values":{"title":"t"}}
            ]),
            &batch,
        )
        .await
        .err()
        .expect("the batch was committed under the call");
        assert!(
            matches!(
                failed.error,
                super::super::InstanceError::BatchNotFound { .. }
            ),
            "{:?}",
            failed.error
        );
        for id in ["ns://kept", "ns://second"] {
            assert!(
                stored_targets(&p, id, "rdf://type").await.is_empty(),
                "{id}"
            );
        }
    }

    /// A raw link staged into the batch while a call into it fails is still staged after.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_raw_add_in_flight_survives_a_failed_call() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                other
                    .add_link(
                        Link {
                            source: "ns://raw".into(),
                            predicate: Some("ns://p".into()),
                            target: "ns://o".into(),
                        },
                        LinkStatus::Shared,
                        Some(id),
                        &other_ctx,
                    )
                    .await
                    .expect("raw add");
            })
        }));

        call(&mut p, &ctx, failing_ops(), &batch)
            .await
            .err()
            .expect("op 1 fails");
        p.commit_batch(batch, &ctx).await.expect("commit");
        assert_eq!(
            stored_targets(&p, "ns://raw", "ns://p").await,
            vec!["ns://o"],
            "the restore overwrote the raw add"
        );
        assert!(stored_targets(&p, "ns://kept", "rdf://type")
            .await
            .is_empty());
    }

    /// A successful call merges into what the batch holds now: a raw add staged meanwhile stays.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_call_is_refused_when_a_raw_add_changed_the_batch_meanwhile() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                other
                    .add_link(
                        Link {
                            source: "ns://raw".into(),
                            predicate: Some("ns://p".into()),
                            target: "ns://o".into(),
                        },
                        LinkStatus::Shared,
                        Some(id),
                        &other_ctx,
                    )
                    .await
                    .expect("raw add");
            })
        }));

        let refused = call(
            &mut p,
            &ctx,
            json!([
                {"op":"create","class":"Note","id":"ns://kept","values":{"title":"t"}},
                {"op":"update","class":"Note","id":"ns://kept","set":{"body":"b"}}
            ]),
            &batch,
        )
        .await
        .err()
        .expect("the batch changed under the call");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::BatchConflict { .. }
            ),
            "{:?}",
            refused.error
        );
        p.commit_batch(batch, &ctx).await.expect("commit");
        // The raw add stays staged; the refused call wrote nothing.
        assert_eq!(stored_targets(&p, "ns://raw", "ns://p").await.len(), 1);
        assert!(stored_targets(&p, "ns://kept", "rdf://type")
            .await
            .is_empty());
    }

    /// Two calls into one batch replace the same single-valued property: the one that merges
    /// second is refused, so the batch holds one title, not both.
    #[tokio::test(flavor = "multi_thread")]
    async fn two_calls_replacing_the_same_property_do_not_both_merge() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Note","id":"ns://n","values":{"title":"A"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                call(
                    &mut other,
                    &other_ctx,
                    json!([{"op":"update","class":"Note","id":"ns://n","set":{"title":"C"}}]),
                    &id,
                )
                .await
                .unwrap_or_else(|e| panic!("the call that merges first: {:?}", e.error));
            })
        }));

        let refused = call(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n","set":{"title":"B"}}]),
            &batch,
        )
        .await
        .err()
        .expect("the second to merge is refused");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::BatchConflict { .. }
            ),
            "{:?}",
            refused.error
        );
        p.commit_batch(batch, &ctx).await.expect("commit");
        assert_eq!(
            stored_targets(&p, "ns://n", "ns://note_title").await,
            vec!["literal:string:C"]
        );
    }

    /// A call the client cancels (its future dropped between two ops) leaves the caller's batch
    /// as it was and no private batch open.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_cancelled_call_leaves_the_callers_batch_and_no_private_batch() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        let batch = p.create_batch().await;
        let (reached_tx, reached_rx) = tokio::sync::oneshot::channel::<()>();
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                let _ = reached_tx.send(());
                std::future::pending::<()>().await;
            })
        }));
        let ops = serde_json::from_value(json!([
            {"op":"create","class":"Note","id":"ns://a","values":{"title":"a"}},
            {"op":"create","class":"Note","id":"ns://b","values":{"title":"b"}}
        ]))
        .expect("ops");
        {
            let mut call_p = p.clone();
            let call = super::super::mutate(&mut call_p, &ctx, ops, Some(batch.clone()));
            tokio::select! {
                _ = call => panic!("the call ends only by being dropped"),
                _ = reached_rx => {}
            }
        }
        // The guard discards the private batch on a task: wait for the one open batch.
        while p.open_batch_count().await != 1 {
            tokio::task::yield_now().await;
        }
        p.commit_batch(batch, &ctx).await.expect("commit");
        assert!(stored_targets(&p, "ns://a", "rdf://type").await.is_empty());
    }

    /// A raw update of the link a call replaces, made while the call runs: the call is refused
    /// and the raw update is what the batch holds.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_setter_racing_a_raw_update_is_refused() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[("Note", NOTE_SDNA)]).await;
        run(
            &mut p,
            &ctx,
            json!([{"op":"create","class":"Note","id":"ns://n","values":{"title":"A"}}]),
        )
        .await
        .unwrap_or_else(|e| panic!("{:?}", e.error));
        let batch = p.create_batch().await;
        let (mut other, other_ctx, id) = (p.clone(), ctx.clone(), batch.clone());
        *p.mutate_gap_hook.lock().unwrap() = Some(Box::new(move || {
            Box::pin(async move {
                let old = other
                    .get_links(&LinkQuery {
                        source: Some("ns://n".into()),
                        predicate: Some("ns://note_title".into()),
                        ..Default::default()
                    })
                    .await
                    .expect("links")
                    .remove(0);
                other
                    .update_link(
                        old.into(),
                        Link {
                            source: "ns://n".into(),
                            predicate: Some("ns://note_title".into()),
                            target: "literal:string:C".into(),
                        },
                        Some(id),
                        &other_ctx,
                    )
                    .await
                    .expect("raw update");
            })
        }));

        let refused = call(
            &mut p,
            &ctx,
            json!([{"op":"update","class":"Note","id":"ns://n","set":{"title":"B"}}]),
            &batch,
        )
        .await
        .err()
        .expect("the batch changed under the call");
        assert!(
            matches!(
                refused.error,
                super::super::InstanceError::BatchConflict { .. }
            ),
            "{:?}",
            refused.error
        );
        p.commit_batch(batch, &ctx).await.expect("commit");
        assert_eq!(
            stored_targets(&p, "ns://n", "ns://note_title").await,
            vec!["literal:string:C"]
        );
    }
}
