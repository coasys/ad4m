//! `call` actions on a shape (design §3): the constructor, a property's
//! setter and the destructor call into a language instead of writing
//! links. `mount-test` stands in for Acorn's language; its `project`
//! address records every call it served, which these tests read back.

use super::*;
use crate::languages::mount_tests::{interact, load_mount_test_language, mount_thing_shape};
use serde_json::{json, Value};

fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

/// The `MountThing` shape, plus a `fail` property on the same `update`
/// call as `done`: `{"fail": "boom"}` makes the language throw
/// `update: boom`.
async fn thing_shape(lang: &str) -> String {
    let mut shape: Value = serde_json::from_str(&mount_thing_shape(lang).await).unwrap();
    let update = shape["properties"][1]["setter"].clone();
    shape["properties"].as_array_mut().unwrap().push(json!({
        "path": "mounttest://fail",
        "name": "fail",
        "max_count": 1,
        "writable": true,
        "setter": update,
    }));
    shape.to_string()
}

/// A private perspective with `shape` registered as `class_name`'s SDNA.
async fn mounted(class_name: &str, shape: String) -> (PerspectiveInstance, AgentContext) {
    let (mut p, _, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_sdna(
        class_name.to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shape),
        &ctx,
    )
    .await
    .unwrap();
    (p, ctx)
}

/// Every call the language has served, in order: `{address, name, params}`.
async fn calls(lang: &str) -> Vec<Value> {
    interact(&format!("{lang}://project"), "calls", json!({}))
        .await
        .unwrap()
        .as_array()
        .unwrap()
        .clone()
}

/// A MountThing with a `{"title": "a"}` record the language knows as `t1`.
async fn created_thing(lang: &str, ctx: &AgentContext, p: &mut PerspectiveInstance) -> String {
    p.create_subject(
        class("MountThing"),
        "ignored".to_string(),
        Some(json!({"title": "a"})),
        None,
        ctx,
    )
    .await
    .unwrap();
    format!("{lang}://t1")
}

async fn link_count(p: &PerspectiveInstance) -> usize {
    p.get_links(&LinkQuery::default()).await.unwrap().len()
}

#[tokio::test]
async fn create_through_a_call_returns_the_new_url() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;

    let url = p
        .create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "a"})),
            None,
            &ctx,
        )
        .await
        .unwrap();
    assert_eq!(url, Some(format!("{lang}://t1")));

    let calls = calls(&lang).await;
    assert_eq!(
        calls,
        vec![json!({"address": "class/MountThing", "name": "create", "params": {"title": "a"}})]
    );

    // The call itself wrote no links: neither the caller's address nor
    // the new one appears in the perspective.
    for source in ["ignored".to_string(), format!("{lang}://t1")] {
        assert!(
            p.get_links(&LinkQuery {
                source: Some(source.clone()),
                ..Default::default()
            })
            .await
            .unwrap()
            .is_empty(),
            "a link with source {source} exists"
        );
    }
}

#[tokio::test]
async fn two_properties_make_one_update_call() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.update_subject(
        class("MountThing"),
        base,
        json!({"title": "b", "done": true}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(
        updates.len(),
        1,
        "the two setters must group into one update call: {calls:?}"
    );
    assert_eq!(updates[0]["address"], json!("t1"));
    assert_eq!(updates[0]["params"], json!({"title": "b", "done": true}));
}

#[tokio::test]
async fn remove_calls_delete() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.remove_subject(class("MountThing"), base, None, &ctx)
        .await
        .unwrap();

    let calls = calls(&lang).await;
    assert_eq!(
        calls.last().unwrap(),
        &json!({"address": "t1", "name": "delete", "params": {}})
    );
}

#[tokio::test]
async fn a_throwing_call_fails_with_its_text_and_writes_nothing() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;
    let before = link_count(&p).await;

    let err = p
        .update_subject(
            class("MountThing"),
            base,
            json!({"fail": "boom"}),
            None,
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("update: boom"), "{err}");
    assert_eq!(link_count(&p).await, before);
}

#[tokio::test]
async fn a_call_inside_a_caller_batch_is_refused() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    let batch = p.create_batch().await;
    let err = p
        .update_subject(
            class("MountThing"),
            base,
            json!({"done": true}),
            Some(batch.clone()),
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(
        err.to_string().contains("cannot run inside a batch"),
        "{err}"
    );
    assert_eq!(calls(&lang).await.len(), 1, "the update never ran");
    p.discard_batch(&batch).await;
}

#[tokio::test]
async fn a_constructor_must_return_an_address() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    shape["target_class"] = json!("mounttest://Bad");
    // `calls` exists on `project` but answers an array, not a delta: no
    // address to name the new instance.
    shape["constructor_actions"] = json!([{
        "action": "call",
        "source": format!("{lang}://project"),
        "predicate": "calls",
        "target": "{}"
    }]);
    let (mut p, ctx) = mounted("MountBad", shape.to_string()).await;

    let err = p
        .create_subject(class("MountBad"), "ignored".to_string(), None, None, &ctx)
        .await
        .unwrap_err();
    // An array is a malformed result: the call ran, so the error says so.
    let err = err.to_string();
    assert!(
        err.contains("the call succeeded but its result was malformed"),
        "{err}"
    );
    assert!(err.contains("names no address"), "{err}");
}

#[tokio::test]
async fn a_malformed_create_result_keeps_its_address() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;

    // `malformed` makes `create` answer a `changed` entry without a
    // `class`: not a `MountDelta`, but its `address` names the record.
    let url = p
        .create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "a", "malformed": true})),
            None,
            &ctx,
        )
        .await
        .unwrap();
    assert_eq!(url, Some(format!("{lang}://t1")));
    assert_eq!(calls(&lang).await.len(), 1);
}

#[tokio::test]
async fn a_managed_user_may_not_make_a_call() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;
    let alice = AgentContext::managed("did:key:z6MkAliceCallActionUser".to_string());
    let before = link_count(&p).await;

    let err = p
        .create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "b"})),
            None,
            &alice,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("main agent"), "{err}");
    let err = p
        .update_subject(
            class("MountThing"),
            base.clone(),
            json!({"done": true}),
            None,
            &alice,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("main agent"), "{err}");
    let err = p
        .remove_subject(class("MountThing"), base, None, &alice)
        .await
        .unwrap_err();
    assert!(err.to_string().contains("main agent"), "{err}");

    assert_eq!(
        calls(&lang).await.len(),
        1,
        "only the main agent's create ran"
    );
    assert_eq!(link_count(&p).await, before);
}

#[tokio::test]
async fn a_request_without_expression_update_may_not_make_a_call() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;

    let err = with_interact_grant(
        false,
        p.create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "a"})),
            None,
            &ctx,
        ),
    )
    .await
    .unwrap_err();
    assert!(err.to_string().contains("expression:update"), "{err}");
    assert!(calls(&lang).await.is_empty());

    let url = with_interact_grant(
        true,
        p.create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "a"})),
            None,
            &ctx,
        ),
    )
    .await
    .unwrap();
    assert_eq!(url, Some(format!("{lang}://t1")));
}

#[test]
fn a_call_result_parses_or_says_why_not() {
    use super::call_actions::parse_call_result;

    let none = parse_call_result(None);
    assert_eq!(
        none.delta,
        crate::languages::mount_types::MountDelta::default()
    );
    assert!(none.malformed.is_none());
    assert!(parse_call_result(Some("null")).malformed.is_none());

    let good = parse_call_result(Some(
        r#"{"address":"a","changed":[{"class":"C","address":"a","data":{}}]}"#,
    ));
    assert!(good.malformed.is_none());
    assert_eq!(good.delta.changed.len(), 1);

    // A bad `changed` entry drops the delta but keeps the address.
    let bad_changed = parse_call_result(Some(r#"{"address":"a","changed":[{"address":"a"}]}"#));
    assert!(bad_changed.malformed.is_some());
    assert_eq!(bad_changed.delta.address.as_deref(), Some("a"));
    assert!(bad_changed.delta.changed.is_empty());

    // A bad `address` is lost, and said so.
    let bad_address = parse_call_result(Some(r#"{"address":42}"#));
    assert!(bad_address.malformed.is_some());
    assert_eq!(bad_address.delta.address, None);

    assert!(parse_call_result(Some("[1]")).malformed.is_some());
    assert!(parse_call_result(Some("not json")).malformed.is_some());
}

#[tokio::test]
async fn a_mixed_action_list_is_refused() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    shape["properties"][1]["setter"]
        .as_array_mut()
        .unwrap()
        .push(json!({
            "action": "setSingleTarget",
            "source": "this",
            "predicate": "mounttest://title",
            "target": "value"
        }));
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    let err = p
        .update_subject(class("MountThing"), base, json!({"title": "b"}), None, &ctx)
        .await
        .unwrap_err();
    assert!(
        err.to_string().contains("mixes call and link actions"),
        "{err}"
    );
}

#[tokio::test]
async fn a_call_target_is_not_parameter_substituted() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // The call's own params are its `target` object; the caller's values
    // merge over them and are never substituted inside the text.
    shape["properties"][2]["setter"][0]["target"] = json!("{\"title\":\"fixed\"}");
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    p.update_subject(class("MountThing"), base, json!({"done": true}), None, &ctx)
        .await
        .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 1);
    assert_eq!(
        updates[0]["params"],
        json!({"title": "fixed", "done": true})
    );
}

/// two setters sharing one `(source, interaction)` group merge
/// each action's `target` defaults into the group together with the
/// caller's values. A later property's default for an earlier property
/// must not overwrite the caller's explicit value: the group's defaults
/// merge first, the caller values last — so the caller wins regardless of
/// which property's contribution lands last.
#[tokio::test]
async fn grouped_calls_merge_defaults_before_caller_values() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `done`'s setter defaults `title`; `title`'s setter defaults `done`
    // and `tags`; `tags`' setter defaults `title`. Every setter shares the
    // `update` interaction, so all three land in one call group.
    shape["properties"][2]["setter"][0]["target"] = json!("{\"title\":\"from-done-default\"}");
    shape["properties"][1]["setter"][0]["target"] =
        json!("{\"done\":\"from-title-default\",\"tags\":\"from-title-tags\"}");
    shape["properties"][3]["setter"][0]["target"] = json!("{\"title\":\"from-tags-default\"}");
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    // done < title: `done` merges first, and `title`'s default for `done`
    // would overwrite the caller's `true`. `title`'s `tags` default is not
    // caller-supplied, so it must survive.
    p.update_subject(
        class("MountThing"),
        base.clone(),
        json!({"done": true, "title": "x"}),
        None,
        &ctx,
    )
    .await
    .unwrap();
    // tags < title: `tags` merges first, and `title`'s default for `tags`
    // would overwrite the caller's `["g"]`. `title`'s `done` default is
    // not caller-supplied here, so it must survive.
    p.update_subject(
        class("MountThing"),
        base,
        json!({"tags": ["g"], "title": "y"}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 2, "one grouped update per write: {calls:?}");
    assert_eq!(
        updates[0]["params"],
        json!({"title": "x", "done": true, "tags": "from-title-tags"}),
        "a later property's default overwrote a caller value: {calls:?}"
    );
    assert_eq!(
        updates[1]["params"],
        json!({"title": "y", "tags": ["g"], "done": "from-title-default"}),
        "caller values must win over every default, defaults over nothing: {calls:?}"
    );
}

/// an adder/remover on a collection reached through an abstract
/// class grouped each member under the same `(source, interaction)` key
/// as `{own: member}`, so the call carried only the last member. The call
/// must carry the whole member list as the collection payload — one
/// `update` with `{children: [..]}`, which is what the mount-test
/// language's record merge can express (a per-member call would leave
/// only the last member on the record).
#[tokio::test]
async fn an_abstract_collection_call_carries_every_member() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `children` gets call adder/remover sharing the `update` interaction.
    let update = shape["properties"][1]["setter"].clone();
    shape["properties"][4]["adder"] = update.clone();
    shape["properties"][4]["remover"] = update;
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    // The mounted record's class flag — the delta apply would write it;
    // until then the test writes it by hand (as
    // `abstract_update_groups_by_concrete_name` does).
    p.add_link(
        Link {
            source: base.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "mounttest://Thing".to_string(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();

    // Add two members through `Container.children`, then remove two.
    p.update_subject(
        class("ad4m://Container"),
        base.clone(),
        json!({"children": {"action": "adder", "value": [format!("{lang}://k1"), format!("{lang}://k2")]}}),
        None,
        &ctx,
    )
    .await
    .unwrap();
    p.update_subject(
        class("ad4m://Container"),
        base.clone(),
        json!({"children": {"action": "remover", "value": [format!("{lang}://k1"), format!("{lang}://k3")]}}),
        None,
        &ctx,
    )
    .await
    .unwrap();
    // A single member is a one-element payload, not a scalar.
    p.update_subject(
        class("ad4m://Container"),
        base.clone(),
        json!({"children": {"action": "adder", "value": format!("{lang}://k4")}}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let log = calls(&lang).await;
    let updates: Vec<&Value> = log.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 3, "one call per member op: {log:?}");
    assert_eq!(
        updates[0]["params"],
        json!({"children": [format!("{lang}://k1"), format!("{lang}://k2")]}),
        "the add kept only the last member: {log:?}"
    );
    assert_eq!(
        updates[1]["params"],
        json!({"children": [format!("{lang}://k1"), format!("{lang}://k3")]}),
        "the remove kept only the last member: {log:?}"
    );
    assert_eq!(
        updates[2]["params"],
        json!({"children": [format!("{lang}://k4")]}),
        "a single member stays an array payload: {log:?}"
    );

    // An empty member list calls nothing: there is nothing to add or
    // remove, as for link actions which write no commands for it.
    for action in ["adder", "remover"] {
        p.update_subject(
            class("ad4m://Container"),
            base.clone(),
            json!({"children": {"action": action, "value": []}}),
            None,
            &ctx,
        )
        .await
        .unwrap();
    }
    let after = calls(&lang).await;
    assert_eq!(after, log, "an empty member op made a call: {after:?}");
}

#[tokio::test]
async fn abstract_update_groups_by_concrete_name() {
    let lang = load_mount_test_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `title` answers to the abstract `TextContent.text` through this
    // alias; the call's params carry the concrete name, `title`.
    shape["properties"][1]["alias"] = json!({"TextContent": "text"});
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;

    // The mounted record's class flag is a link — the A4 delta apply
    // writes it; until then the test writes it by hand.
    p.add_link(
        Link {
            source: base.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "mounttest://Thing".to_string(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();

    p.update_subject(
        class("ad4m://TextContent"),
        base,
        json!({"text": "z"}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 1, "one grouped update: {calls:?}");
    assert_eq!(updates[0]["params"], json!({"title": "z"}));
}

// --- `perspective.instance*` on a mounted class -----------------------------
//
// The validated writes run the same lifecycle actions `*_subject` runs — on a
// mounted class those are `call`s, so `instanceCreate`/`instanceUpdate`/
// `instanceRemove` reach the language and the returned delta lands in the
// perspective.

mod instance_writes {
    use super::*;
    use crate::perspectives::instances::{
        add_to_collection as instance_add_to_collection, create as instance_create,
        remove as instance_remove, remove_from_collection as instance_remove_from_collection,
        update as instance_update, InstanceAddToCollection, InstanceCreate, InstanceError,
        InstanceRemove, InstanceRemoveFromCollection, InstanceUpdate,
    };
    use crate::perspectives::perspective_instance::mounts_tests::{
        create as create_in_language, mounted, things,
    };
    use serde_json::Map;

    /// A link constructor must not bypass a declared call adder by writing
    /// initial membership links. Refuse before creating or staging anything.
    #[tokio::test(flavor = "multi_thread")]
    async fn create_refuses_a_call_adder_without_a_setter() {
        use crate::perspectives::instances::{ReadOnlyReason, ValidationProblem};
        let lang = load_mount_test_language().await;
        let shape = json!({
            "target_class": "mounttest://Mixed",
            "constructor_actions": [{"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "mounttest://Mixed"}],
            "properties": [
                {"path": "rdf://type", "name": "type", "has_value": "mounttest://Mixed", "min_count": 1, "max_count": 1},
                {"path": "mounttest://tag", "name": "tags", "collection": true, "datatype": "xsd:string", "writable": true,
                 "adder": [{"action": "call", "source": "this", "predicate": "addTags", "target": "{}"}]}
            ]
        });
        let (mut p, ctx) = super::mounted("Mixed", shape.to_string()).await;
        let before = link_count(&p).await;
        for batched in [false, true] {
            let batch = if batched {
                Some(p.create_batch().await)
            } else {
                None
            };
            let result = instance_create(
                &mut p,
                &ctx,
                InstanceCreate {
                    class_name: "Mixed".into(),
                    base_uri: Some(format!("{lang}://t1")),
                    properties: Some(Map::from_iter([("tags".into(), json!(["x"]))])),
                    batch_id: batch.clone(),
                    ..Default::default()
                },
            )
            .await;
            match result {
                Err(InstanceError::Validation { errors, .. }) => {
                    assert_eq!(errors.len(), 1);
                    assert_eq!(errors[0].property, "tags");
                    assert_eq!(
                        errors[0].problem,
                        ValidationProblem::ReadOnly {
                            reason: ReadOnlyReason::NoSetter
                        }
                    );
                }
                other => panic!("expected typed refusal, got {other:?}"),
            }
            if let Some(batch) = batch {
                p.commit_batch(batch, &ctx).await.unwrap();
            }
            assert_eq!(
                link_count(&p).await,
                before,
                "refusal wrote or staged links"
            );
            assert!(calls(&lang).await.is_empty());
        }
    }

    /// `instanceCreate` on a mounted class calls the language's constructor;
    /// its minted address is the instance's id, every supplied value —
    /// collections too — is a constructor parameter, and the record reads
    /// back as an instance at once.
    #[tokio::test(flavor = "multi_thread")]
    async fn create_calls_the_constructor_and_returns_its_address() {
        let lang = load_mount_test_language().await;
        let (mut p, ctx) = mounted(&lang).await;

        let created = instance_create(
            &mut p,
            &ctx,
            InstanceCreate {
                class_name: "MountThing".into(),
                properties: Some(Map::from_iter([
                    ("title".to_string(), json!("a")),
                    ("tags".to_string(), json!(["x", "y"])),
                ])),
                ..Default::default()
            },
        )
        .await
        .expect("instance create");

        assert_eq!(created.base_uri, format!("{lang}://t1"));
        assert_eq!(
            calls(&lang).await,
            vec![json!({"address": "class/MountThing", "name": "create",
                        "params": {"title": "a", "tags": ["x", "y"]}})]
        );
        let found = things(&p).await;
        assert_eq!(
            found.len(),
            1,
            "the minted record is an instance: {found:?}"
        );
        assert_eq!(found[0]["title"], "a");
        assert_eq!(found[0]["tags"], json!(["x", "y"]));
    }

    /// `instanceUpdate` sends the patch to the record's address as the
    /// grouped setter call — scalars and the collection replace (`tags`) in
    /// one `update` — and the delta it answers lands in the perspective. An
    /// empty string clears through the setter like any value.
    #[tokio::test(flavor = "multi_thread")]
    async fn update_calls_the_setters_and_applies_the_delta() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a", "tags": ["x", "y"]})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let base = format!("{lang}://t1");

        instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "MountThing".into(),
                base_uri: base.clone(),
                properties: Map::from_iter([
                    ("done".to_string(), json!(true)),
                    ("tags".to_string(), json!(["b"])),
                    ("title".to_string(), json!("")),
                ]),
                batch_id: None,
            },
        )
        .await
        .expect("instance update");

        let updates: Vec<Value> = calls(&lang)
            .await
            .into_iter()
            .filter(|c| c["name"] == "update")
            .collect();
        assert_eq!(updates.len(), 1, "one grouped update call: {updates:?}");
        assert_eq!(updates[0]["address"], "t1");
        // `done` declares no datatype: it is a string-typed property, so
        // validation coerces `true` to "true" before the call sees it.
        assert_eq!(
            updates[0]["params"],
            json!({"done": "true", "tags": ["b"], "title": ""})
        );

        let found = things(&p).await;
        assert_eq!(found[0]["title"], "");
        assert_eq!(found[0]["done"], "true");
        assert_eq!(found[0]["tags"], json!(["b"]));
    }

    /// `instanceRemove` runs the `call` destructor at the record's address
    /// before the link cascade: the language sees `delete`, its record goes,
    /// and the mirrored links leave the perspective.
    #[tokio::test(flavor = "multi_thread")]
    async fn remove_calls_the_destructor() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let base = format!("{lang}://t1");

        instance_remove(
            &mut p,
            &ctx,
            InstanceRemove {
                class_name: "MountThing".into(),
                base_uri: base,
                batch_id: None,
            },
        )
        .await
        .expect("instance remove");

        let deletes: Vec<Value> = calls(&lang)
            .await
            .into_iter()
            .filter(|c| c["name"] == "delete")
            .collect();
        assert_eq!(deletes.len(), 1);
        assert_eq!(deletes[0]["address"], "t1");
        assert!(things(&p).await.is_empty(), "the record is gone");
    }

    /// A write that calls into a language cannot honour a caller's batch:
    /// discarding the batch cannot undo the call. `instanceCreate` with an
    /// open batch on a class with a `call` constructor is refused before
    /// the call — the language sees nothing and the batch stages nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn create_refuses_a_caller_batch_before_calling() {
        let lang = load_mount_test_language().await;
        let (mut p, ctx) = mounted(&lang).await;
        let batch = p.create_batch().await;

        let result = instance_create(
            &mut p,
            &ctx,
            InstanceCreate {
                class_name: "MountThing".into(),
                properties: Some(Map::from_iter([("title".to_string(), json!("a"))])),
                batch_id: Some(batch.clone()),
                ..Default::default()
            },
        )
        .await;

        assert!(
            matches!(result, Err(InstanceError::CallsInBatch { .. })),
            "a create with a call constructor ran in the caller's batch: {result:?}"
        );
        assert!(calls(&lang).await.is_empty(), "the language was called");
        assert!(things(&p).await.is_empty());
        assert!(p.has_batch(&batch).await, "the caller's batch stays open");
    }

    /// `instanceUpdate` with an open batch on a `call` setter is refused
    /// before the call.
    #[tokio::test(flavor = "multi_thread")]
    async fn update_refuses_a_caller_batch_before_calling() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let batch = p.create_batch().await;
        let before = calls(&lang).await;

        let result = instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "MountThing".into(),
                base_uri: format!("{lang}://t1"),
                properties: Map::from_iter([("title".to_string(), json!("b"))]),
                batch_id: Some(batch),
            },
        )
        .await;

        assert!(
            matches!(result, Err(InstanceError::CallsInBatch { .. })),
            "an update with a call setter ran in the caller's batch: {result:?}"
        );
        assert_eq!(calls(&lang).await, before, "the language was called");
        assert_eq!(things(&p).await[0]["title"], "a");
    }

    /// `instanceRemove` with an open batch on a `call` destructor is
    /// refused before the destructor and before the cascade.
    #[tokio::test(flavor = "multi_thread")]
    async fn remove_refuses_a_caller_batch_before_calling() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let batch = p.create_batch().await;
        let before = calls(&lang).await;

        let result = instance_remove(
            &mut p,
            &ctx,
            InstanceRemove {
                class_name: "MountThing".into(),
                base_uri: format!("{lang}://t1"),
                batch_id: Some(batch),
            },
        )
        .await;

        assert!(
            matches!(result, Err(InstanceError::CallsInBatch { .. })),
            "a remove with a call destructor ran in the caller's batch: {result:?}"
        );
        assert_eq!(calls(&lang).await, before, "the language was called");
        assert_eq!(things(&p).await.len(), 1, "the record is still there");
    }

    /// An app's abstract class the mount's `TextContent` alias answers to
    /// (the built-in `ad4m://TextContent` is not an instance* class).
    async fn register_text_content(p: &mut PerspectiveInstance, ctx: &AgentContext) {
        p.add_sdna(
            "TextContent".to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(
                json!({
                    "target_class": "app://TextContent",
                    "abstract": true,
                    "properties": [{
                        "path": "app://text", "name": "text", "datatype": "xsd:string",
                        "max_count": 1, "writable": true,
                        "setter": [{"action": "setSingleTarget", "source": "this",
                                    "predicate": "app://text", "target": "value"}],
                    }],
                })
                .to_string(),
            ),
            ctx,
        )
        .await
        .unwrap();
    }

    /// An abstract `instanceRemove` whose record's destructor calls a
    /// language refuses a caller's batch before the call, like a concrete one.
    #[tokio::test(flavor = "multi_thread")]
    async fn abstract_remove_refuses_a_caller_batch_before_calling() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "hello"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        register_text_content(&mut p, &ctx).await;
        let batch = p.create_batch().await;
        let before = calls(&lang).await;

        let result = instance_remove(
            &mut p,
            &ctx,
            InstanceRemove {
                class_name: "app://TextContent".into(),
                base_uri: format!("{lang}://t1"),
                batch_id: Some(batch),
            },
        )
        .await;

        assert!(
            matches!(result, Err(InstanceError::CallsInBatch { .. })),
            "{result:?}"
        );
        assert_eq!(calls(&lang).await, before, "the language was called");
    }

    /// `instanceUpdate` through an abstract class reaches a mounted
    /// record's `call` setter unbatched — the write core must not open a
    /// batch of its own around it (`run_call_actions` refuses one).
    #[tokio::test(flavor = "multi_thread")]
    async fn abstract_update_calls_without_a_batch() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "hello"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        register_text_content(&mut p, &ctx).await;

        instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "app://TextContent".into(),
                base_uri: format!("{lang}://t1"),
                properties: Map::from_iter([("text".to_string(), json!("bye"))]),
                batch_id: None,
            },
        )
        .await
        .expect("abstract instance update");

        let updates: Vec<Value> = calls(&lang)
            .await
            .into_iter()
            .filter(|c| c["name"] == "update")
            .collect();
        assert_eq!(updates.len(), 1, "{updates:?}");
        assert_eq!(updates[0]["params"], json!({"title": "bye"}));
        assert_eq!(things(&p).await[0]["title"], "bye");
    }

    /// Round 6 (We's Acorn "Untag"): the frame We sends, by the mount's alias
    /// class IRI (`literal:string:shacl://<Name>`), `tags: []` on a record
    /// that has a tag, reaches the language's setter as `{tags: []}` and the
    /// record reads back with no tags. Pins the executor half; the live
    /// failure was the SDK sending no frame at all (`cdd52af8c`).
    #[tokio::test(flavor = "multi_thread")]
    async fn untag_by_the_alias_iri_clears_the_collection_in_the_language() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a", "tags": ["x"]})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let before = calls(&lang).await.len();

        instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "literal:string:shacl://MountThing".into(),
                base_uri: format!("{lang}://t1"),
                properties: Map::from_iter([("tags".to_string(), json!([]))]),
                batch_id: None,
            },
        )
        .await
        .expect("untag");

        assert_eq!(
            calls(&lang).await[before..].to_vec(),
            vec![json!({"address": "t1", "name": "update", "params": {"tags": []}})]
        );
        let tags = things(&p).await[0]["tags"].clone();
        assert!(tags.as_array().is_none_or(|t| t.is_empty()), "{tags}");
    }

    /// a mounted collection whose shape declares a `call` adder
    /// and remover and no setter. `instanceAddToCollection` /
    /// `instanceRemoveFromCollection` run the language's adder / remover
    /// with the member list (writing no membership link of their own),
    /// and refuse a caller's batch before calling.
    #[tokio::test(flavor = "multi_thread")]
    async fn collection_writes_run_a_call_adder_and_remover() {
        let lang = load_mount_test_language().await;
        interact(
            &format!("{lang}://project"),
            "setShapes",
            json!({"shapes": [{"name": "MountThing", "target": "mounttest://Thing", "tags": {
                "setter": null,
                "adder": [{"action": "call", "source": "this", "predicate": "addTags", "target": "{}"}],
                "remover": [{"action": "call", "source": "this", "predicate": "removeTags", "target": "{}"}],
            }}]}),
        )
        .await
        .unwrap();
        create_in_language(&lang, json!({"title": "a"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        let base = format!("{lang}://t1");
        let add = |batch_id: Option<String>| InstanceAddToCollection {
            class_name: "MountThing".into(),
            base_uri: base.clone(),
            collection: "tags".into(),
            item_uri: "x".into(),
            batch_id,
        };
        let batch = p.create_batch().await;
        let before = calls(&lang).await;

        let refused = instance_add_to_collection(&mut p, &ctx, add(Some(batch))).await;
        assert!(
            matches!(refused, Err(InstanceError::CallsInBatch { .. })),
            "{refused:?}"
        );
        assert_eq!(calls(&lang).await, before, "nothing was called");

        instance_add_to_collection(&mut p, &ctx, add(None))
            .await
            .expect("add through the call adder");
        assert_eq!(things(&p).await[0]["tags"], json!(["x"]));
        instance_remove_from_collection(
            &mut p,
            &ctx,
            InstanceRemoveFromCollection {
                class_name: "MountThing".into(),
                base_uri: base.clone(),
                collection: "tags".into(),
                item_uri: "x".into(),
                batch_id: None,
            },
        )
        .await
        .expect("remove through the call remover");
        // No member links left: the raw model query leaves the key out.
        let tags = things(&p).await[0]["tags"].clone();
        assert!(tags.as_array().is_none_or(|t| t.is_empty()), "{tags}");

        assert_eq!(
            calls(&lang).await[before.len()..].to_vec(),
            vec![
                json!({"address": "t1", "name": "addTags", "params": {"tags": ["x"]}}),
                json!({"address": "t1", "name": "removeTags", "params": {"tags": ["x"]}}),
            ]
        );
    }

    /// `app://Note`: a link class read through `app://TextContent` whose
    /// `text` is stored through `lang` — every value written to it is an
    /// `expressionCreate` on the language. A node is a Note when it has the
    /// `app://is_note` flag link (`make_note`).
    async fn register_note(p: &mut PerspectiveInstance, ctx: &AgentContext, lang: &str) {
        p.add_sdna(
            "Note".to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(
                json!({
                    "target_class": "app://Note",
                    "properties": [
                        {"path": "app://is_note", "name": "isNote", "has_value": "app://Note",
                         "min_count": 1, "max_count": 1},
                        {"path": "app://note_text", "name": "text", "datatype": "xsd:string",
                         "max_count": 1, "writable": true, "resolve_language": lang,
                         "setter": [{"action": "setSingleTarget", "source": "this",
                                     "predicate": "app://note_text", "target": "value"}]},
                    ],
                })
                .to_string(),
            ),
            ctx,
        )
        .await
        .unwrap();
    }

    async fn make_note(p: &mut PerspectiveInstance, ctx: &AgentContext, base: &str) {
        p.add_link(
            Link {
                source: base.to_string(),
                predicate: Some("app://is_note".to_string()),
                target: "app://Note".to_string(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .unwrap();
    }

    async fn expression_creates(lang: &str) -> usize {
        calls(lang)
            .await
            .iter()
            .filter(|c| c["name"] == "expressionCreate")
            .count()
    }

    /// an abstract `instanceUpdate` that reaches only a link
    /// setter stored through a language makes one expression. Choosing the
    /// batch must not resolve the values: a dry run of the write made one
    /// expression, and the write itself a second.
    #[tokio::test(flavor = "multi_thread")]
    async fn abstract_update_creates_each_expression_once() {
        let lang = load_mount_test_language().await;
        let (mut p, ctx) = mounted(&lang).await;
        register_text_content(&mut p, &ctx).await;
        register_note(&mut p, &ctx, &lang).await;
        make_note(&mut p, &ctx, "app://n1").await;

        instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "app://TextContent".into(),
                base_uri: "app://n1".into(),
                properties: Map::from_iter([("text".to_string(), json!("hi"))]),
                batch_id: None,
            },
        )
        .await
        .expect("abstract instance update");

        assert_eq!(
            expression_creates(&lang).await,
            1,
            "{:?}",
            calls(&lang).await
        );
    }

    /// a record that is both a mounted Thing (`call` setter) and a
    /// Note (a link setter stored through the language). Through the
    /// abstract class, with no batch, the call runs once and one expression
    /// is made; in a caller's batch the update is refused before either.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_mixed_abstract_update_creates_no_expression_when_refused() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "hello"})).await;
        let (mut p, ctx) = mounted(&lang).await;
        register_text_content(&mut p, &ctx).await;
        register_note(&mut p, &ctx, &lang).await;
        let base = format!("{lang}://t1");
        make_note(&mut p, &ctx, &base).await;
        let batch = p.create_batch().await;
        let before = calls(&lang).await;

        let refused = instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "app://TextContent".into(),
                base_uri: base.clone(),
                properties: Map::from_iter([("text".to_string(), json!("bye"))]),
                batch_id: Some(batch),
            },
        )
        .await;
        assert!(
            matches!(refused, Err(InstanceError::CallsInBatch { .. })),
            "{refused:?}"
        );
        assert_eq!(
            calls(&lang).await,
            before,
            "nothing reached the language before the refusal"
        );

        instance_update(
            &mut p,
            &ctx,
            InstanceUpdate {
                class_name: "app://TextContent".into(),
                base_uri: base,
                properties: Map::from_iter([("text".to_string(), json!("bye"))]),
                batch_id: None,
            },
        )
        .await
        .expect("abstract instance update");
        let after: Vec<Value> = calls(&lang).await[before.len()..].to_vec();
        let named = |name: &str| after.iter().filter(|c| c["name"] == name).count();
        assert_eq!(named("expressionCreate"), 1, "{after:?}");
        assert_eq!(named("update"), 1, "{after:?}");
    }

    /// a link constructor with a `call` setter on one property must
    /// still write a setter-less collection's item links on create. Whether
    /// the constructor calls — not whether any action calls — decides if
    /// the collection goes to `create_subject`, which skips it.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_call_setter_does_not_swallow_a_plain_collection_on_create() {
        let lang = load_mount_test_language().await;
        create_in_language(&lang, json!({"title": "a"})).await;
        let update =
            json!([{"action": "call", "source": "this", "predicate": "update", "target": "{}"}]);
        let shape = json!({
            "target_class": "mounttest://Mixed",
            "constructor_actions": [{"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "mounttest://Mixed"}],
            "destructor_actions": [{"action": "removeLink", "source": "this", "predicate": "rdf://type", "target": "mounttest://Mixed"}],
            "properties": [
                {"path": "rdf://type", "name": "type", "has_value": "mounttest://Mixed", "min_count": 1, "max_count": 1},
                {"path": "mounttest://title", "name": "title", "max_count": 1, "writable": true, "setter": update},
                {"path": "mounttest://ref", "name": "refs", "collection": true, "node_kind": "IRI", "writable": true},
            ],
        });
        let (mut p, ctx) = super::mounted("Mixed", shape.to_string()).await;
        let base = format!("{lang}://t1");

        instance_create(
            &mut p,
            &ctx,
            InstanceCreate {
                class_name: "Mixed".into(),
                base_uri: Some(base.clone()),
                properties: Some(Map::from_iter([
                    ("title".to_string(), json!("b")),
                    ("refs".to_string(), json!(["ad4m://r1", "ad4m://r2"])),
                ])),
                ..Default::default()
            },
        )
        .await
        .expect("mixed create");

        let refs = p
            .get_links(&LinkQuery {
                source: Some(base),
                predicate: Some("mounttest://ref".to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(refs.len(), 2, "the plain collection was skipped: {refs:?}");
        let updates: Vec<Value> = calls(&lang)
            .await
            .into_iter()
            .filter(|c| c["name"] == "update")
            .collect();
        assert_eq!(updates.len(), 1, "{updates:?}");
        assert_eq!(updates[0]["params"], json!({"title": "b"}));
    }
}

/// A language whose `expressionCreate` counts its calls; `count` on its
/// `project` address reads the count back.
async fn load_counting_resolve_language() -> String {
    let source = format!(
        "export const name = \"counting-resolve\";\n\
         let count = 0;\n\
         export async function init() {{}}\n\
         export function isPublic() {{ return false; }}\n\
         export async function expressionCreate(content) {{ count++; return \"counted://\" + count; }}\n\
         export function interactions(a) {{ return a === \"project\" ? [{{ name: \"count\", label: \"count\", parameters: [], execute: async () => count }}] : []; }}\n\
         // instance {}\n",
        uuid::Uuid::new_v4()
    );
    let controller = crate::languages::LanguageController::global_instance();
    let (_, path) = controller.save_language_bundle(&source, None).unwrap();
    controller.load_language(path, false).await.unwrap()
}

#[tokio::test]
async fn an_abstract_call_adder_never_runs_the_link_resolver() {
    let lang = load_mount_test_language().await;
    let resolver = load_counting_resolve_language().await;
    let mut shape: Value = serde_json::from_str(&thing_shape(&lang).await).unwrap();
    // `children` gets call adder/remover, and a resolve language a link
    // adder would create its members on.
    let update = shape["properties"][1]["setter"].clone();
    shape["properties"][4]["adder"] = update.clone();
    shape["properties"][4]["remover"] = update;
    shape["properties"][4]["resolve_language"] = json!(resolver);
    let (mut p, ctx) = mounted("MountThing", shape.to_string()).await;
    let base = created_thing(&lang, &ctx, &mut p).await;
    p.add_link(
        Link {
            source: base.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "mounttest://Thing".to_string(),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();
    let resolved = || async {
        interact(&format!("{resolver}://project"), "count", json!({}))
            .await
            .unwrap()
    };
    let children = json!({"children": {"action": "adder", "value": [format!("{lang}://k1")]}});

    // Denied: a managed user may not make the call.
    let alice = AgentContext::managed("did:key:z6MkAliceCallActionUser".to_string());
    let err = p
        .update_subject(
            class("ad4m://Container"),
            base.clone(),
            children.clone(),
            None,
            &alice,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("main agent"), "{err}");
    assert_eq!(resolved().await, json!(0), "a denied call ran the resolver");

    // Batched: the call is refused inside a caller batch.
    let batch = p.create_batch().await;
    let err = p
        .update_subject(
            class("ad4m://Container"),
            base.clone(),
            children.clone(),
            Some(batch.clone()),
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(
        err.to_string().contains("cannot run inside a batch"),
        "{err}"
    );
    p.discard_batch(&batch).await;
    assert_eq!(
        resolved().await,
        json!(0),
        "a batched call ran the resolver"
    );

    // Allowed: the call runs, and still nothing is resolved.
    p.update_subject(class("ad4m://Container"), base, children, None, &ctx)
        .await
        .unwrap();
    assert_eq!(resolved().await, json!(0), "a call adder ran the resolver");
}

#[tokio::test]
async fn execute_commands_refuses_a_call_before_writing_anything() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let before = link_count(&p).await;
    let commands = || {
        vec![
            Command {
                action: Action::AddLink,
                source: Some("this".to_string()),
                predicate: Some("test://p".to_string()),
                target: Some("test://t".to_string()),
                local: None,
            },
            Command {
                action: Action::Call,
                source: Some(format!("{lang}://class/MountThing")),
                predicate: Some("create".to_string()),
                target: Some("{}".to_string()),
                local: None,
            },
        ]
    };

    let err = p
        .execute_commands(commands(), "test://base".to_string(), vec![], None, &ctx)
        .await
        .unwrap_err();
    assert!(err.to_string().contains("call"), "{err}");
    assert_eq!(
        link_count(&p).await,
        before,
        "the addLink before the call was written"
    );

    let batch = p.create_batch().await;
    let err = p
        .execute_commands(
            commands(),
            "test://base".to_string(),
            vec![],
            Some(batch.clone()),
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("call"), "{err}");
    p.commit_batch(batch, &ctx).await.unwrap();
    assert_eq!(link_count(&p).await, before, "the batch staged the addLink");
    assert!(calls(&lang).await.is_empty(), "the call ran");
}

/// How many of `LocalNote`'s links are on `base`.
async fn sibling_links(p: &PerspectiveInstance, base: &str) -> usize {
    p.get_links(&LinkQuery {
        source: Some(base.to_string()),
        ..Default::default()
    })
    .await
    .unwrap()
    .into_iter()
    .filter(|l| {
        l.data.predicate.as_deref() == Some("local://text") || l.data.target == "local://LocalNote"
    })
    .count()
}

/// `remove_subject` of a call-backed class, by its target class IRI (as
/// the SDK's `delete()` sends it): the destructor's `delete` call runs,
/// and a local model on the same node keeps its links.
#[tokio::test]
async fn removing_a_call_backed_record_keeps_a_sibling_model() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted("MountThing", thing_shape(&lang).await).await;
    let base = created_thing(&lang, &ctx, &mut p).await;
    p.add_sdna(
        "LocalNote".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(
            json!({
                "target_class": "local://LocalNote",
                "constructor_actions": [
                    {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "local://LocalNote"}
                ],
                "destructor_actions": [
                    {"action": "removeLink", "source": "this", "predicate": "rdf://type", "target": "local://LocalNote"}
                ],
                "properties": [
                    {"path": "rdf://type", "name": "type", "has_value": "local://LocalNote", "min_count": 1, "max_count": 1},
                    {"path": "local://text", "name": "text", "max_count": 1, "writable": true,
                     "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "local://text", "target": "value"}]}
                ]
            })
            .to_string(),
        ),
        &ctx,
    )
    .await
    .unwrap();
    p.create_subject(
        class("LocalNote"),
        base.clone(),
        Some(json!({"text": "mine"})),
        None,
        &ctx,
    )
    .await
    .unwrap();
    assert_eq!(sibling_links(&p, &base).await, 2);

    p.remove_subject(class("mounttest://Thing"), base.clone(), None, &ctx)
        .await
        .unwrap();

    let log = calls(&lang).await;
    assert!(
        log.iter().any(|c| c["name"] == "delete"),
        "the destructor did not call: {log:?}"
    );
    assert_eq!(
        sibling_links(&p, &base).await,
        2,
        "the sibling model lost its links"
    );
}
