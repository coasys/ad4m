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
