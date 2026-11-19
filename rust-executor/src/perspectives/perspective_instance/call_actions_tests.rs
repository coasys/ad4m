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
        create as instance_create, remove as instance_remove, update as instance_update,
        InstanceCreate, InstanceRemove, InstanceUpdate,
    };
    use crate::perspectives::perspective_instance::mounts_tests::{
        create as create_in_language, mounted, things,
    };
    use serde_json::Map;

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
}
