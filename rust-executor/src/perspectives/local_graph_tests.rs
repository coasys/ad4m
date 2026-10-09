//! Local graphs: per-agent named graphs that never sync and that only their
//! agent reads. Each test pins one rule of the design agreed on #812.

use super::perspective_instance::{
    Command, Parameter, PerspectiveInstance, SdnaType, SubjectClassOption,
};
use super::sparql_store::{local_graph_iri, resolve_graph_for, LOCAL_GRAPH_ALIAS};
use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::languages::language::recording;
use crate::prolog_service::init_prolog_service;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC};
use crate::test_utils::setup_wallet;
use crate::types::{
    Link, LinkExpression, LinkQuery, LinkStatus, PerspectiveDiff, PerspectiveHandle,
    PerspectiveState,
};
use serde_json::Value;
use uuid::Uuid;

const SHARED_GRAPH: &str = "ad4m://graph/channel-1";

/// The wallet and the main agent, which every signature and user key needs.
fn init() {
    setup_wallet();
    AgentService::init_global_test_instance();
}

async fn setup(owners: Option<Vec<String>>) -> PerspectiveInstance {
    init();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    init_prolog_service().await;
    PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("Local graphs".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: PerspectiveState::Private,
            owners,
        },
        None,
    )
}

/// A managed user with a key, as signup leaves one.
pub(crate) fn user(label: &str) -> (AgentContext, String) {
    init();
    let email = format!("{label}.{}@example.org", Uuid::new_v4());
    AgentService::ensure_user_key_exists(&email).unwrap();
    let did = AgentService::get_user_did_by_email(&email).unwrap();
    (AgentContext::for_user_email(email), did)
}

fn link(name: &str) -> Link {
    Link {
        source: format!("ad4m://s/{name}"),
        predicate: Some("ad4m://p".to_string()),
        target: format!("ad4m://t/{name}"),
    }
}

fn sources(links: &[crate::types::DecoratedLinkExpression]) -> Vec<String> {
    let mut s: Vec<String> = links.iter().map(|l| l.data.source.clone()).collect();
    s.sort();
    s
}

/// Alice and Bob each write one link into their Local graph; the main agent
/// writes one into a shared named graph and one into the default graph.
pub(crate) async fn two_users_and_shared_links() -> (PerspectiveInstance, String, String) {
    let (alice, alice_did) = user("alice");
    let (bob, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    let local = Some(LOCAL_GRAPH_ALIAS.to_string());
    p.add_link(
        link("alice"),
        LinkStatus::Shared,
        None,
        &alice,
        local.clone(),
    )
    .await
    .unwrap();
    p.add_link(link("bob"), LinkStatus::Shared, None, &bob, local)
        .await
        .unwrap();
    p.add_link(
        link("shared"),
        LinkStatus::Shared,
        None,
        &main,
        Some(SHARED_GRAPH.to_string()),
    )
    .await
    .unwrap();
    p.add_link(link("default"), LinkStatus::Shared, None, &main, None)
        .await
        .unwrap();
    (p, alice_did, bob_did)
}

#[test]
fn the_alias_resolves_per_agent_and_another_agents_local_graph_is_refused() {
    assert_eq!(
        resolve_graph_for(LOCAL_GRAPH_ALIAS, Some("did:a")).unwrap(),
        local_graph_iri("did:a")
    );
    assert_eq!(
        resolve_graph_for(&local_graph_iri("did:a"), Some("did:a")).unwrap(),
        local_graph_iri("did:a")
    );
    assert!(resolve_graph_for(&local_graph_iri("did:b"), Some("did:a")).is_err());
    assert_eq!(
        resolve_graph_for(SHARED_GRAPH, Some("did:a")).unwrap(),
        SHARED_GRAPH
    );
    // The executor itself names graphs as stored, with no alias to resolve.
    assert!(resolve_graph_for(LOCAL_GRAPH_ALIAS, None).is_err());
    assert!(resolve_graph_for(&local_graph_iri("did:b"), None).is_ok());
}

#[tokio::test]
async fn a_write_to_the_alias_lands_in_the_writers_local_graph_as_local() {
    let (alice, alice_did) = user("alice");
    let mut p = setup(None).await;
    let written = p
        .add_link(
            link("a"),
            LinkStatus::Shared,
            None,
            &alice,
            Some(LOCAL_GRAPH_ALIAS.to_string()),
        )
        .await
        .unwrap();
    assert_eq!(written.graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(written.status, Some(LinkStatus::Local));

    let stored = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery::default())
        .await
        .unwrap();
    assert_eq!(stored.len(), 1);
    assert_eq!(stored[0].graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(stored[0].status, Some(LinkStatus::Local));
}

/// A signed link in `graph`, marked `Shared` whatever the graph.
fn shared_in(name: &str, graph: &str) -> LinkExpression {
    let mut l = LinkExpression::from(
        crate::agent::create_signed_expression(link(name).normalize(), &AgentContext::main_agent())
            .unwrap(),
    );
    l.graph = Some(graph.to_string());
    l.status = Some(LinkStatus::Shared);
    l
}

/// Each path into the link language is the last guard before it: whatever
/// status a write path set, a link in a Local graph never passes.
#[tokio::test]
async fn no_path_into_the_link_language_carries_a_local_graph_link() {
    let p = setup(None).await;
    let address = format!("test://recording/{}", Uuid::new_v4());
    p.set_link_language_for_test(recording::language(&address))
        .await;
    let mut handle = p.persisted.lock().await.clone();
    handle.neighbourhood = Some(Default::default());
    p.update_from_handle(handle).await;

    let private = shared_in("private", &local_graph_iri("did:a"));
    let public = shared_in("public", SHARED_GRAPH);
    let diff = PerspectiveDiff {
        additions: vec![private.clone(), public.clone()],
        removals: vec![private.clone()],
    };

    // 1. A commit.
    p.commit(&diff).await.unwrap();
    // 2. A queued pending diff.
    Ad4mDb::with_global_instance(|db| db.add_pending_diff(&p.uuid, &diff)).unwrap();
    p.commit_pending_diffs().await.unwrap();
    // 3. The fallback sync of stored shared links.
    p.sparql_store.add_link(&private).unwrap();
    p.sparql_store.add_link(&public).unwrap();
    assert!(p.ensure_public_links_are_shared().await);

    let commits = recording::commits(&address);
    assert_eq!(commits.len(), 3, "{commits:?}");
    for commit in &commits {
        assert_eq!(commit.additions.len(), 1, "{commit:?}");
        assert_eq!(commit.additions[0].data.source, "ad4m://s/public");
        assert!(commit.removals.is_empty(), "{commit:?}");
    }
}

#[tokio::test]
async fn a_write_into_another_agents_local_graph_is_refused() {
    let (alice, _) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(None).await;
    let bobs = Some(local_graph_iri(&bob_did));
    assert!(p
        .add_link(link("x"), LinkStatus::Shared, None, &alice, bobs.clone())
        .await
        .is_err());
    assert!(p
        .add_links(vec![link("x")], LinkStatus::Shared, None, &alice, bobs)
        .await
        .is_err());
    // Bob would read a write that landed in his graph.
    assert!(p
        .clone()
        .for_viewer(bob_did)
        .get_links(&LinkQuery::default())
        .await
        .unwrap()
        .is_empty());
}

/// A batch re-signs its links on commit. That used to drop the graph, so every
/// batched write — `Ad4mModel.save()` and `create()` included — landed in the
/// default graph.
#[tokio::test]
async fn a_batched_write_keeps_its_graph() {
    let (alice, alice_did) = user("alice");
    let mut p = setup(None).await;
    let batch = p.create_batch().await;
    p.add_links(
        vec![link("shared")],
        LinkStatus::Shared,
        Some(batch.clone()),
        &alice,
        Some(SHARED_GRAPH.to_string()),
    )
    .await
    .unwrap();
    p.add_links(
        vec![link("local")],
        LinkStatus::Shared,
        Some(batch.clone()),
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    p.commit_batch(batch, &alice).await.unwrap();

    let links = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery::default())
        .await
        .unwrap();
    let graph_of = |name: &str| {
        let l = links
            .iter()
            .find(|l| l.data.source == format!("ad4m://s/{name}"))
            .unwrap();
        (l.graph.clone(), l.status.clone())
    };
    assert_eq!(
        graph_of("shared"),
        (Some(SHARED_GRAPH.to_string()), Some(LinkStatus::Shared))
    );
    assert_eq!(
        graph_of("local"),
        (Some(local_graph_iri(&alice_did)), Some(LinkStatus::Local))
    );
}

#[tokio::test]
async fn other_agents_local_graphs_leave_link_and_sparql_reads() {
    let (p, alice_did, bob_did) = two_users_and_shared_links().await;
    let alice_view = p.clone().for_viewer(alice_did);

    let seen = sources(&alice_view.get_links(&LinkQuery::default()).await.unwrap());
    assert_eq!(
        seen,
        vec!["ad4m://s/alice", "ad4m://s/default", "ad4m://s/shared"]
    );
    // A page counts only links the viewer sees.
    let page = alice_view
        .get_links(&LinkQuery {
            limit: Some(3),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(page.len(), 3);

    let rows = |json: String| -> Vec<String> {
        let mut s: Vec<String> = serde_json::from_str::<Vec<Value>>(&json)
            .unwrap()
            .iter()
            .filter_map(|r| r["s"].as_str().map(str::to_string))
            .collect();
        s.sort();
        s
    };
    let all = "SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string();
    assert_eq!(
        rows(alice_view.sparql_query(all.clone()).unwrap()),
        vec!["ad4m://s/alice", "ad4m://s/default", "ad4m://s/shared"]
    );
    // `GRAPH ?g` ranges over the viewer's graphs only.
    let by_graph = "SELECT ?s WHERE { GRAPH ?g { ?s <ad4m://p> ?o } }".to_string();
    assert_eq!(
        rows(alice_view.sparql_query(by_graph).unwrap()),
        vec!["ad4m://s/alice", "ad4m://s/shared"]
    );
    // Outside `GRAPH`, such a query still reads the store's default graph only:
    // the named graphs do not show up a second time.
    let both = "SELECT ?s WHERE { { ?s <ad4m://p> ?o } UNION { GRAPH ?g { ?s <ad4m://p> ?o } } }"
        .to_string();
    assert_eq!(
        rows(alice_view.sparql_query(both).unwrap()),
        vec!["ad4m://s/alice", "ad4m://s/default", "ad4m://s/shared"]
    );
    // However the query spells its `GRAPH` pattern, another agent's Local
    // graph stays out (found by review: `$g`, a comment, a prefixed name).
    let bob_graph = local_graph_iri(&bob_did);
    for spelled in [
        "SELECT ?s WHERE { GRAPH $g { ?s <ad4m://p> ?o } }".to_string(),
        "SELECT ?s WHERE { GRAPH #c\n?g { ?s <ad4m://p> ?o } }".to_string(),
        format!(
            "PREFIX l: <ad4m://local/> SELECT ?s WHERE {{ GRAPH l:{bob_did} {{ ?s <ad4m://p> ?o }} }}"
        ),
    ] {
        let seen = alice_view.sparql_query(spelled.clone()).map(rows);
        assert!(
            !seen.as_ref().is_ok_and(|s| s.iter().any(|s| s == "ad4m://s/bob")),
            "{spelled} read Bob's Local graph: {seen:?}"
        );
    }
    // Naming another agent's Local graph fails, in the query or in the scope.
    assert!(alice_view
        .sparql_query(format!("SELECT ?s FROM <{bob_graph}> WHERE {{ ?s ?p ?o }}"))
        .is_err());
    assert!(alice_view
        .sparql_query_with_graphs(all.clone(), Some(&[bob_graph]))
        .is_err());
    // The alias names the viewer's own Local graph.
    assert_eq!(
        rows(
            alice_view
                .sparql_query_with_graphs(all.clone(), Some(&[LOCAL_GRAPH_ALIAS.to_string()]))
                .unwrap()
        ),
        vec!["ad4m://s/alice"]
    );
    // The executor's shared instance reads the shared graphs only (#1358).
    assert_eq!(
        rows(p.sparql_query(all).unwrap()),
        vec!["ad4m://s/default", "ad4m://s/shared"]
    );
}

#[tokio::test]
async fn model_queries_leave_out_other_agents_local_graphs() {
    let (alice, alice_did) = user("alice");
    let (bob, _) = user("bob");
    let mut p = setup(None).await;
    let main = AgentContext::main_agent();
    let shacl = r#"{
        "target_class": "t://Note",
        "constructor_actions": [
            {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "t://Note"}
        ],
        "destructor_actions": [],
        "properties": [
            {
                "path": "t://text", "name": "text", "datatype": "xsd://string",
                "min_count": 0, "max_count": 1, "writable": true,
                "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "t://text", "target": "value"}]
            }
        ]
    }"#;
    p.add_sdna(
        "Note".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        &main,
    )
    .await
    .unwrap();
    let note = || SubjectClassOption {
        class_name: Some("Note".to_string()),
        query: None,
    };
    let local = Some(LOCAL_GRAPH_ALIAS.to_string());
    for (id, ctx, graph) in [
        ("t://note/alice", &alice, local.clone()),
        ("t://note/bob", &bob, local),
        ("t://note/shared", &main, None),
    ] {
        p.create_subject(
            note(),
            id.to_string(),
            Some(serde_json::json!({ "text": id })),
            None,
            ctx,
            graph,
        )
        .await
        .unwrap();
    }

    let ids = |json: String| -> Vec<String> {
        let v: Value = serde_json::from_str(&json).unwrap();
        let mut ids: Vec<String> = v["instances"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|i| i["id"].as_str().map(str::to_string))
            .collect();
        ids.sort();
        ids
    };
    let alice_view = p.clone().for_viewer(alice_did);
    assert_eq!(
        ids(alice_view.model_query("Note", "{}", None).await.unwrap()),
        vec!["t://note/alice", "t://note/shared"]
    );
    assert_eq!(
        ids(p.model_query("Note", "{}", None).await.unwrap()),
        vec!["t://note/shared"]
    );
}

/// An app that loads an instance and edits it names no graph. The edit must
/// stay where the instance lives, or a Local note's new text would sync.
#[tokio::test]
async fn editing_an_instance_without_naming_a_graph_writes_where_it_lives() {
    let (alice, alice_did) = user("alice");
    let mut p = setup(None).await;
    let main = AgentContext::main_agent();
    let shacl = r#"{
        "target_class": "t://Note",
        "constructor_actions": [
            {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "t://Note"}
        ],
        "destructor_actions": [],
        "properties": [
            {
                "path": "t://text", "name": "text", "datatype": "xsd://string",
                "min_count": 0, "max_count": 1, "writable": true,
                "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "t://text", "target": "value"}]
            }
        ]
    }"#;
    p.add_sdna(
        "Note".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        &main,
    )
    .await
    .unwrap();
    let note = || SubjectClassOption {
        class_name: Some("Note".to_string()),
        query: None,
    };
    p.create_subject(
        note(),
        "t://note/private".to_string(),
        Some(serde_json::json!({ "text": "draft" })),
        None,
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    p.update_subject(
        note(),
        "t://note/private".to_string(),
        serde_json::json!({ "text": "edited" }),
        None,
        &alice,
    )
    .await
    .unwrap();

    let links = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery {
            source: Some("t://note/private".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert!(
        links.iter().any(|l| l.data.target.contains("edited")),
        "{links:?}"
    );
    assert!(
        links
            .iter()
            .all(|l| l.graph == Some(local_graph_iri(&alice_did))
                && l.status == Some(LinkStatus::Local)),
        "an edit left the Local graph: {links:?}"
    );

    // A client that loaded the note without a scope names the model's default
    // graph. The write still stays in the Local graph.
    let commands: Vec<Command> = serde_json::from_value(serde_json::json!([{
        "source": "this", "predicate": "t://tag", "target": "value", "action": "addLink"
    }]))
    .unwrap();
    let params: Vec<Parameter> =
        serde_json::from_value(serde_json::json!([{ "name": "value", "value": "t://tag/1" }]))
            .unwrap();
    p.execute_commands(
        commands,
        "t://note/private".to_string(),
        params,
        None,
        &alice,
        Some(SHARED_GRAPH.to_string()),
    )
    .await
    .unwrap();
    let tag = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery {
            predicate: Some("t://tag".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(tag.len(), 1);
    assert_eq!(tag[0].graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(tag[0].status, Some(LinkStatus::Local));
}

#[tokio::test]
async fn updating_a_link_keeps_its_graph() {
    let (alice, alice_did) = user("alice");
    let mut p = setup(None).await;
    let written = p
        .add_link(
            link("old"),
            LinkStatus::Shared,
            None,
            &alice,
            Some(LOCAL_GRAPH_ALIAS.to_string()),
        )
        .await
        .unwrap();
    let updated = p
        .update_link(LinkExpression::from(written), link("new"), None, &alice)
        .await
        .unwrap();
    assert_eq!(updated.graph, Some(local_graph_iri(&alice_did)));
    let stored = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery::default())
        .await
        .unwrap();
    assert_eq!(sources(&stored), vec!["ad4m://s/new"]);
    assert_eq!(stored[0].graph, Some(local_graph_iri(&alice_did)));
}

/// Local graphs never sync, so a peer's link that names one is forged or
/// misrouted, and must not reach anyone's private view.
#[tokio::test]
async fn synced_links_that_name_a_local_graph_are_dropped() {
    let (_, alice_did) = user("alice");
    let p = setup(None).await;
    let mut forged = LinkExpression::from(
        crate::agent::create_signed_expression(
            link("forged").normalize(),
            &AgentContext::main_agent(),
        )
        .unwrap(),
    );
    forged.graph = Some(local_graph_iri(&alice_did));
    let mut honest = forged.clone();
    honest.graph = Some(SHARED_GRAPH.to_string());
    honest.data.source = "ad4m://s/honest".to_string();
    p.diff_from_link_language(PerspectiveDiff {
        additions: vec![forged, honest],
        removals: vec![],
    })
    .await
    .unwrap();
    assert_eq!(
        sources(
            &p.clone()
                .for_viewer(alice_did)
                .get_links(&LinkQuery::default())
                .await
                .unwrap()
        ),
        vec!["ad4m://s/honest"]
    );
}

/// A link-added event goes to each owner once; a Local-graph link only to its agent.
#[tokio::test]
async fn a_local_graph_link_event_reaches_only_its_agent() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let mut events = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_LINK_ADDED_TOPIC)
        .await;
    p.add_link(
        link("private"),
        LinkStatus::Shared,
        None,
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    p.add_link(link("public"), LinkStatus::Shared, None, &alice, None)
        .await
        .unwrap();

    let mut owners_by_link: Vec<(String, String)> = Vec::new();
    while let Ok(Ok(msg)) =
        tokio::time::timeout(std::time::Duration::from_millis(300), events.recv()).await
    {
        let v: Value = serde_json::from_str(&msg).unwrap();
        if v["perspectiveUuid"].as_str() != Some(p.uuid.as_str()) {
            continue;
        }
        let source = v["link"]["data"]["source"].as_str().unwrap().to_string();
        owners_by_link.push((source, v["owner"].as_str().unwrap().to_string()));
    }
    owners_by_link.sort();
    assert_eq!(
        owners_by_link,
        vec![
            ("ad4m://s/private".to_string(), alice_did.clone()),
            (
                "ad4m://s/public".to_string(),
                std::cmp::min(alice_did.clone(), bob_did.clone())
            ),
            (
                "ad4m://s/public".to_string(),
                std::cmp::max(alice_did, bob_did)
            ),
        ]
    );
}

/// A subscription's update reaches its subscriber's session only. The result
/// is computed as the subscriber reads, so it carries that agent's Local rows;
/// a co-owner of the perspective must not get it.
#[tokio::test]
async fn a_subscription_update_reaches_only_its_subscriber() {
    use crate::api::events_ws::matches_query_subscription_owner;
    use crate::pubsub::PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC;

    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    // The session filter checks ownership through the global registry.
    super::register_perspective(p.uuid.clone(), p.clone());

    let (subscription_id, initial) = p
        .subscribe_and_query(
            "SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string(),
            alice.user_email.clone(),
        )
        .await
        .unwrap();
    assert!(!initial.contains("ad4m://s/secret"));

    let mut events = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;
    p.add_link(
        link("secret"),
        LinkStatus::Shared,
        None,
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        while !p.subscription_check_pending() {
            tokio::task::yield_now().await;
        }
    })
    .await
    .expect("the write was not recorded for the subscription check");
    p.check_subscriptions_now().await;

    let update = loop {
        let msg = tokio::time::timeout(std::time::Duration::from_secs(5), events.recv())
            .await
            .expect("no subscription update was published")
            .unwrap();
        let v: Value = serde_json::from_str(&msg).unwrap();
        if v["subscriptionId"].as_str() == Some(subscription_id.as_str()) {
            break msg;
        }
    };
    let v: Value = serde_json::from_str(&update).unwrap();
    assert!(
        v["result"].as_str().unwrap().contains("ad4m://s/secret"),
        "the update carries Alice's Local row: {update}"
    );
    assert!(
        matches_query_subscription_owner(&update, Some(&alice_did), false),
        "Alice's session gets her update"
    );
    assert!(
        !matches_query_subscription_owner(&update, Some(&bob_did), false),
        "Bob co-owns the perspective but didn't subscribe: {update}"
    );
    assert!(
        !matches_query_subscription_owner(&update, None, false),
        "a session whose DID hasn't resolved gets nothing"
    );
    super::unregister_perspective(&p.uuid);
}

/// A signed link from a client carries its own graph. It gets the same rules
/// as a plain write.
#[tokio::test]
async fn a_signed_link_names_its_graph_like_any_write() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(None).await.for_viewer(alice_did.clone());
    let signed = |name: &str, graph: &str| {
        let mut l = LinkExpression::from(
            crate::agent::create_signed_expression(link(name).normalize(), &alice).unwrap(),
        );
        l.graph = Some(graph.to_string());
        l
    };
    assert!(p
        .add_link_expression(
            signed("x", &local_graph_iri(&bob_did)),
            LinkStatus::Shared,
            None
        )
        .await
        .is_err());
    let written = p
        .add_link_expression(signed("y", LOCAL_GRAPH_ALIAS), LinkStatus::Shared, None)
        .await
        .unwrap();
    assert_eq!(written.graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(written.status, Some(LinkStatus::Local));
}

/// A removal acts on the stored link, whatever graph the caller's copy names.
#[tokio::test]
async fn a_removal_finds_the_stored_link_and_its_graph() {
    let (alice, alice_did) = user("alice");
    let (bob, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let local = Some(LOCAL_GRAPH_ALIAS.to_string());
    let a = p
        .add_link(link("a"), LinkStatus::Shared, None, &alice, local.clone())
        .await
        .unwrap();
    let b = p
        .add_link(link("b"), LinkStatus::Shared, None, &alice, local)
        .await
        .unwrap();
    let mutation = |l: &crate::types::DecoratedLinkExpression, graph: Option<&str>| {
        let mut l = LinkExpression::from(l.clone());
        l.graph = graph.map(str::to_string);
        l.status = None;
        crate::types::LinkMutations {
            additions: vec![],
            removals: vec![serde_json::from_value(serde_json::to_value(l).unwrap()).unwrap()],
        }
    };

    // Bob cannot remove Alice's Local link, even by naming it exactly.
    assert!(p
        .link_mutations(mutation(&a, None), LinkStatus::Shared, &bob, None)
        .await
        .is_err());

    // Alice removes hers with the alias, and with no graph at all.
    let out = p
        .link_mutations(
            mutation(&a, Some(LOCAL_GRAPH_ALIAS)),
            LinkStatus::Shared,
            &alice,
            None,
        )
        .await
        .unwrap();
    assert_eq!(out.removals[0].graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(out.removals[0].status, Some(LinkStatus::Local));
    p.link_mutations(mutation(&b, None), LinkStatus::Shared, &alice, None)
        .await
        .unwrap();
    assert!(p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery::default())
        .await
        .unwrap()
        .is_empty());

    // On a viewer's instance, a single removal of a foreign Local link fails.
    let c = p
        .add_link(
            link("c"),
            LinkStatus::Shared,
            None,
            &alice,
            Some(LOCAL_GRAPH_ALIAS.to_string()),
        )
        .await
        .unwrap();
    let mut bob_view = p.clone().for_viewer(bob_did);
    assert!(bob_view
        .remove_link(LinkExpression::from(c.clone()), None)
        .await
        .is_err());
    assert!(bob_view
        .remove_links(vec![LinkExpression::from(c)], None)
        .await
        .unwrap()
        .is_empty());
    assert_eq!(
        p.clone()
            .for_viewer(alice_did)
            .get_links(&LinkQuery::default())
            .await
            .unwrap()
            .len(),
        1
    );
}

/// A child written into the own graph of a graph-rooted parent that lives in
/// the writer's Local graph lands in that Local graph: it never syncs.
#[tokio::test]
async fn children_of_a_local_instance_stay_local() {
    let (alice, alice_did) = user("alice");
    let mut p = setup(None).await;
    p.add_link(
        Link {
            source: "t://channel/1".to_string(),
            predicate: Some("t://name".to_string()),
            target: "literal://string:mine".to_string(),
        },
        LinkStatus::Shared,
        None,
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    let child = p
        .add_link(
            Link {
                source: "t://channel/1".to_string(),
                predicate: Some("t://note".to_string()),
                target: "t://note/1".to_string(),
            },
            LinkStatus::Shared,
            None,
            &alice,
            Some("ad4m://graph/t://channel/1".to_string()),
        )
        .await
        .unwrap();
    assert_eq!(child.graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(child.status, Some(LinkStatus::Local));
}

/// A notification trigger reads as the agent the notification belongs to.
#[tokio::test]
async fn a_notification_trigger_does_not_read_another_agents_local_graph() {
    let (p, _, _) = two_users_and_shared_links().await;
    let bob_email = format!("bob.{}@example.org", Uuid::new_v4());
    AgentService::ensure_user_key_exists(&bob_email).unwrap();
    Ad4mDb::with_global_instance(|db| {
        db.add_notification(
            crate::types::NotificationInput {
                description: "all".to_string(),
                app_name: "t".to_string(),
                app_url: "t".to_string(),
                app_icon_path: "t".to_string(),
                trigger: "SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string(),
                perspective_ids: vec![p.uuid.clone()],
                webhook_url: String::new(),
                webhook_auth: String::new(),
            },
            Some(bob_email),
        )
    })
    .unwrap();
    let matches = p.calc_notification_trigger_matches().await.unwrap();
    let (_, rows) = matches.into_iter().next().unwrap();
    let mut seen: Vec<&str> = rows.iter().filter_map(|r| r["s"].as_str()).collect();
    seen.sort();
    assert_eq!(seen, vec!["ad4m://s/default", "ad4m://s/shared"]);
}

/// The auto-processor reads as the agent it runs for: Bob's Local draft never
/// reaches Alice's pass, so it cannot go to the LLM or into a shared graph.
#[tokio::test]
async fn the_auto_processor_does_not_gather_another_agents_local_graph() {
    use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
    use crate::perspectives::auto_processor::watcher::WatcherState;
    use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

    let (mut p, _shapes, main) = setup_perspective_no_llm(&[]).await;
    let (alice, _) = user("alice");
    let (bob, _) = user("bob");
    p.add_link(
        Link {
            source: "msg://draft".to_string(),
            predicate: Some("ns://body".to_string()),
            target: "literal:string:secret".to_string(),
        },
        LinkStatus::Shared,
        None,
        &bob,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    let cfg = AutoProcessorConfig {
        processor_id: "local-graph".into(),
        source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms: 60_000,
        batch_min: 1,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        ..Default::default()
    };
    write_processor(&mut p, &cfg, Some(false), &main)
        .await
        .unwrap();
    let now_ms = chrono::Utc::now().timestamp_millis();
    let gathered = |watcher: &WatcherState| {
        watcher
            .pending_for("local-graph")
            .map(|e| e.items.len())
            .unwrap_or(0)
    };

    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &alice)
        .await;
    assert_eq!(gathered(&watcher), 0, "Alice's pass gathered Bob's draft");

    // Control: the same message, shared, is gathered. Bob's own pass leaving
    // his draft out is pinned by
    // `the_auto_processor_does_not_gather_its_runners_own_local_graph`.
    p.add_link(
        Link {
            source: "msg://shared".to_string(),
            predicate: Some("ns://body".to_string()),
            target: "literal:string:public".to_string(),
        },
        LinkStatus::Shared,
        None,
        &bob,
        None,
    )
    .await
    .unwrap();
    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &alice)
        .await;
    assert_eq!(gathered(&watcher), 1);
}

/// A pass writes what it extracts as Shared, so it reads shared graphs only:
/// Bob's own tick leaves his Local draft out. It never reaches a batch, a
/// claim, the LLM or a shared graph.
#[tokio::test]
async fn the_auto_processor_does_not_gather_its_runners_own_local_graph() {
    use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
    use crate::perspectives::auto_processor::events::{
        subscribe, AutoProcessorEvent, AutoProcessorStep,
    };
    use crate::perspectives::auto_processor::watcher::WatcherState;
    use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::perspectives::sparql_store::is_local_graph;

    let (mut p, _shapes, main) = setup_perspective_no_llm(&[]).await;
    let (bob, _) = user("bob");
    let body = |source: &str, text: &str| Link {
        source: source.to_string(),
        predicate: Some("ns://body".to_string()),
        target: format!("literal:string:{text}"),
    };
    p.add_link(
        body("msg://draft", "secret"),
        LinkStatus::Shared,
        None,
        &bob,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    let debounce_ms = 50;
    let cfg = AutoProcessorConfig {
        processor_id: "own-local".into(),
        source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms,
        batch_min: 1,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        ..Default::default()
    };
    write_processor(&mut p, &cfg, Some(false), &main)
        .await
        .unwrap();
    let gathered = |watcher: &WatcherState| {
        watcher
            .pending_for("own-local")
            .map(|e| e.items.len())
            .unwrap_or(0)
    };
    let shared_links = |links: Vec<crate::types::DecoratedLinkExpression>| {
        links
            .into_iter()
            .filter(|l| !l.graph.as_deref().is_some_and(is_local_graph))
            .count()
    };
    let shared_before = shared_links(p.get_links(&LinkQuery::default()).await.unwrap());
    let uuid = p.uuid.clone();
    let mut rx = subscribe().await;

    // Tick 1 gathers; tick 2 is past the debounce, so a gathered batch runs.
    let now_ms = chrono::Utc::now().timestamp_millis();
    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &bob).await;
    assert_eq!(
        gathered(&watcher),
        0,
        "Bob's pass gathered his own Local draft"
    );
    p.run_auto_processor_tick(&mut watcher, now_ms + debounce_ms + 1, &bob)
        .await;

    // A tick emits its events before it returns.
    let mut started = Vec::new();
    while let Ok(json) = rx.try_recv() {
        if let Ok(ev) = serde_json::from_str::<AutoProcessorEvent>(&json) {
            if ev.perspective_uuid == uuid && ev.step == AutoProcessorStep::BatchReady {
                started.push(ev.item_ids);
            }
        }
    }
    assert_eq!(
        started,
        Vec::<Vec<String>>::new(),
        "a pass ran on Bob's draft"
    );
    assert_eq!(
        shared_links(p.get_links(&LinkQuery::default()).await.unwrap()),
        shared_before,
        "Bob's pass wrote into a shared graph"
    );

    // Control: the same message, shared, is gathered.
    p.add_link(
        body("msg://shared", "public"),
        LinkStatus::Shared,
        None,
        &bob,
        None,
    )
    .await
    .unwrap();
    let mut watcher = WatcherState::new();
    p.run_auto_processor_tick(&mut watcher, now_ms, &bob).await;
    assert_eq!(gathered(&watcher), 1);
}

/// "Children of a Local instance stay Local" holds when someone else creates
/// the subject's own graph, and inside one batch.
#[tokio::test]
async fn a_local_subjects_own_graph_cannot_pull_its_children_out() {
    let (alice, alice_did) = user("alice");
    let (bob, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did])).await;
    let local = Some(LOCAL_GRAPH_ALIAS.to_string());
    let channel_link = |s: &str, p: &str, t: &str| Link {
        source: s.to_string(),
        predicate: Some(p.to_string()),
        target: t.to_string(),
    };
    p.add_link(
        channel_link("t://channel/1", "t://name", "literal:string:mine"),
        LinkStatus::Shared,
        None,
        &alice,
        local.clone(),
    )
    .await
    .unwrap();

    // Bob may not create the own graph of Alice's Local channel.
    assert!(p
        .add_link(
            channel_link("t://x", "t://p", "t://y"),
            LinkStatus::Shared,
            None,
            &bob,
            Some("ad4m://graph/t://channel/1".to_string()),
        )
        .await
        .is_err());
    // Alice's child still follows her channel.
    let child = p
        .add_link(
            channel_link("t://channel/1", "t://note", "t://note/1"),
            LinkStatus::Shared,
            None,
            &alice,
            Some("ad4m://graph/t://channel/1".to_string()),
        )
        .await
        .unwrap();
    assert_eq!(child.graph, Some(local_graph_iri(&alice_did)));

    // Inside one batch: the channel and its child commit together.
    let batch = p.create_batch().await;
    p.add_link(
        channel_link("t://channel/2", "t://name", "literal:string:draft"),
        LinkStatus::Shared,
        Some(batch.clone()),
        &alice,
        local,
    )
    .await
    .unwrap();
    p.add_link(
        channel_link("t://channel/2", "t://note", "t://note/2"),
        LinkStatus::Shared,
        Some(batch.clone()),
        &alice,
        Some("ad4m://graph/t://channel/2".to_string()),
    )
    .await
    .unwrap();
    p.commit_batch(batch, &alice).await.unwrap();
    let stored = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery {
            source: Some("t://channel/2".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(stored.len(), 2);
    assert!(
        stored
            .iter()
            .all(|l| l.graph == Some(local_graph_iri(&alice_did))
                && l.status == Some(LinkStatus::Local)),
        "{stored:?}"
    );
}

/// A Local write that names no graph lands in its writer's Local graph (#1357).
/// Apps write Local links this way (`addLink(uuid, link, "local")`), and so
/// does the flow engine's `currentState` cache. In the default graph every
/// co-owner of the perspective would read them.
#[tokio::test]
async fn a_local_write_without_a_graph_lands_in_its_writers_local_graph() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;

    let one = p
        .add_link(link("one"), LinkStatus::Local, None, &alice, None)
        .await
        .unwrap();
    assert_eq!(
        (one.graph, one.status),
        (Some(local_graph_iri(&alice_did)), Some(LinkStatus::Local))
    );
    p.add_links(vec![link("two")], LinkStatus::Local, None, &alice, None)
        .await
        .unwrap();
    p.link_mutations(
        crate::types::LinkMutations {
            additions: vec![{
                let l = link("three");
                crate::types::LinkInput {
                    source: l.source,
                    predicate: l.predicate,
                    target: l.target,
                }
            }],
            removals: vec![],
        },
        LinkStatus::Local,
        &alice,
        None,
    )
    .await
    .unwrap();
    let batch = p.create_batch().await;
    p.add_link(
        link("four"),
        LinkStatus::Local,
        Some(batch.clone()),
        &alice,
        None,
    )
    .await
    .unwrap();
    p.commit_batch(batch, &alice).await.unwrap();

    let mine = p
        .clone()
        .for_viewer(alice_did.clone())
        .get_links(&LinkQuery::default())
        .await
        .unwrap();
    assert_eq!(
        sources(&mine),
        vec![
            "ad4m://s/four",
            "ad4m://s/one",
            "ad4m://s/three",
            "ad4m://s/two"
        ]
    );
    assert!(
        mine.iter()
            .all(|l| l.graph == Some(local_graph_iri(&alice_did))
                && l.status == Some(LinkStatus::Local)),
        "{mine:?}"
    );

    let bob_view = p.clone().for_viewer(bob_did);
    assert!(
        bob_view
            .get_links(&LinkQuery::default())
            .await
            .unwrap()
            .is_empty(),
        "Bob co-owns the perspective but reads Alice's Local links"
    );
    let rows: Vec<Value> = serde_json::from_str(
        &bob_view
            .sparql_query("SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string())
            .unwrap(),
    )
    .unwrap();
    assert!(
        rows.is_empty(),
        "Bob's SPARQL read Alice's Local links: {rows:?}"
    );
}

/// A Local command on a subject that lives in a shared graph: the link is
/// still Alice's alone. A Local link always lives in its writer's Local graph.
#[tokio::test]
async fn a_local_command_on_a_shared_subject_lands_in_the_writers_local_graph() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    p.add_link(
        Link {
            source: "t://channel".to_string(),
            predicate: Some("t://name".to_string()),
            target: "literal:string:general".to_string(),
        },
        LinkStatus::Shared,
        None,
        &main,
        Some(SHARED_GRAPH.to_string()),
    )
    .await
    .unwrap();

    let commands: Vec<Command> = serde_json::from_value(serde_json::json!([{
        "source": "this", "predicate": "t://read", "target": "value",
        "action": "addLink", "local": true
    }]))
    .unwrap();
    let params: Vec<Parameter> =
        serde_json::from_value(serde_json::json!([{ "name": "value", "value": "t://marker" }]))
            .unwrap();
    for graph in [None, Some(SHARED_GRAPH.to_string())] {
        p.execute_commands(
            commands.clone(),
            "t://channel".to_string(),
            params.clone(),
            None,
            &alice,
            graph,
        )
        .await
        .unwrap();
    }

    let read = |viewer: String| {
        let view = p.clone().for_viewer(viewer);
        async move {
            view.get_links(&LinkQuery {
                predicate: Some("t://read".to_string()),
                ..Default::default()
            })
            .await
            .unwrap()
        }
    };
    let alices = read(alice_did.clone()).await;
    assert!(!alices.is_empty());
    assert!(
        alices
            .iter()
            .all(|l| l.graph == Some(local_graph_iri(&alice_did))
                && l.status == Some(LinkStatus::Local)),
        "{alices:?}"
    );
    assert!(read(bob_did).await.is_empty());
}

/// Editing a Local link stored in the default graph (written before #812)
/// moves it into its writer's Local graph.
#[tokio::test]
async fn updating_a_local_link_in_the_default_graph_moves_it_into_the_writers_local_graph() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    // As a pre-#812 executor stored it: Local, no graph.
    let mut legacy: LinkExpression = crate::agent::create_signed_expression(link("old"), &alice)
        .unwrap()
        .into();
    legacy.status = Some(LinkStatus::Local);
    p.sparql_store.add_link(&legacy).unwrap();

    let updated = p
        .update_link(legacy, link("new"), None, &alice)
        .await
        .unwrap();
    assert_eq!(updated.graph, Some(local_graph_iri(&alice_did)));
    assert_eq!(updated.status, Some(LinkStatus::Local));
    assert!(p
        .clone()
        .for_viewer(bob_did)
        .get_links(&LinkQuery::default())
        .await
        .unwrap()
        .is_empty());
}

/// A read with no viewer sees the shared graphs only (#1358). Request paths
/// stamp a viewer, and background work that writes shared output reads
/// shared-only; any other path that forgets to scope itself must not read
/// every user's Local graph.
#[tokio::test]
async fn an_unscoped_read_sees_no_local_graph() {
    let (p, alice_did, _) = two_users_and_shared_links().await;
    let shared = vec!["ad4m://s/default", "ad4m://s/shared"];

    assert_eq!(
        sources(&p.get_links(&LinkQuery::default()).await.unwrap()),
        shared
    );
    let rows = |json: String| -> Vec<String> {
        let mut s: Vec<String> = serde_json::from_str::<Vec<Value>>(&json)
            .unwrap()
            .iter()
            .filter_map(|r| r["s"].as_str().map(str::to_string))
            .collect();
        s.sort();
        s
    };
    assert_eq!(
        rows(
            p.sparql_query("SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string())
                .unwrap()
        ),
        shared
    );
    assert_eq!(
        rows(
            p.sparql_query("SELECT ?s WHERE { GRAPH ?g { ?s <ad4m://p> ?o } }".to_string())
                .unwrap()
        ),
        vec!["ad4m://s/shared"]
    );
    // Naming a Local graph does not widen it.
    assert!(!p
        .sparql_query_with_graphs(
            "SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string(),
            Some(&[local_graph_iri(&alice_did)]),
        )
        .map(rows)
        .is_ok_and(|s| s.iter().any(|s| s == "ad4m://s/alice")));
    // The viewer's own Local graph is still there for the viewer.
    assert_eq!(
        sources(
            &p.clone()
                .for_viewer(alice_did)
                .get_links(&LinkQuery::default())
                .await
                .unwrap()
        ),
        vec!["ad4m://s/alice", "ad4m://s/default", "ad4m://s/shared"]
    );
}

/// `subject_classes_of` reads the store directly. Model queries use it to pick
/// the class of a polymorphic relation target, so it must not classify a URI
/// by another agent's Local links (#1358, found by CodeRabbit on #1058).
#[tokio::test]
async fn subject_classes_of_does_not_read_another_agents_local_graph() {
    let (bob, bob_did) = user("bob");
    let (_, alice_did) = user("alice");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    let shacl = r#"{
        "target_class": "t://Note",
        "constructor_actions": [
            {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "t://Note"}
        ],
        "destructor_actions": [],
        "properties": [
            {
                "path": "t://text", "name": "text", "datatype": "xsd://string",
                "min_count": 1, "max_count": 1, "writable": true,
                "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "t://text", "target": "value"}]
            }
        ]
    }"#;
    p.add_sdna(
        "Note".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        &main,
    )
    .await
    .unwrap();
    // Required `text`, so the class can be tested; written Local by Bob.
    p.add_links(
        vec![
            Link {
                source: "t://note/bob".to_string(),
                predicate: Some("rdf://type".to_string()),
                target: "t://Note".to_string(),
            },
            Link {
                source: "t://note/bob".to_string(),
                predicate: Some("t://text".to_string()),
                target: "literal:string:mine".to_string(),
            },
        ],
        LinkStatus::Local,
        None,
        &bob,
        None,
    )
    .await
    .unwrap();
    let uris = ["t://note/bob".to_string()];

    for (who, view) in [
        ("the executor", p.clone()),
        ("Alice", p.clone().for_viewer(alice_did)),
    ] {
        let classes = view.subject_classes_of(&uris).unwrap();
        assert!(
            classes.get("t://note/bob").is_none(),
            "{who} classified Bob's Local note: {classes:?}"
        );
    }
    let bobs = p
        .clone()
        .for_viewer(bob_did)
        .subject_classes_of(&uris)
        .unwrap();
    assert_eq!(bobs.get("t://note/bob"), Some(&vec!["Note".to_string()]));
}

/// A class acts for every agent, so the class list ignores a declaration in a
/// Local graph, its own author's included. The list reads raw store rows, so
/// the viewer scope does not do this for it.
#[tokio::test]
async fn a_class_declared_in_a_local_graph_is_not_listed() {
    let (bob, bob_did) = user("bob");
    let (_, alice_did) = user("alice");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    let declare = |class: &str| Link {
        source: format!("t://{class}"),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://SubjectClass".to_string(),
    };
    p.add_link(declare("Public"), LinkStatus::Shared, None, &main, None)
        .await
        .unwrap();
    p.add_link(
        declare("Secret"),
        LinkStatus::Shared,
        None,
        &bob,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();

    for (who, view) in [
        ("the executor", p.clone()),
        ("Alice", p.clone().for_viewer(alice_did)),
        ("Bob", p.clone().for_viewer(bob_did)),
    ] {
        let classes = view.get_subject_classes_from_shacl().await.unwrap();
        assert_eq!(classes, vec!["Public".to_string()], "{who}'s class list");
    }
}

/// Each co-owner keeps their flow `currentState` cache in their own Local
/// graph (#1360), and a local vote runs the pass for the voter only. On a
/// co-owned perspective a local flow write therefore queues the pass a synced
/// one would, which sweeps once per owner on this node; on a perspective with
/// one owner it does not, as before.
#[tokio::test]
async fn a_local_vote_on_a_co_owned_perspective_queues_every_owners_pass() {
    use super::flow_instance::atom::ACCEPTED_BY_PREDICATE;
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let vote = || Link {
        source: "ad4m://proposal/1".to_string(),
        predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
        target: "literal:string:yes".to_string(),
    };
    let queued = |p: &PerspectiveInstance| !p.flow_pass_queue.lock().unwrap().is_idle();

    let mut alone = setup(Some(vec![alice_did.clone()])).await;
    alone
        .add_link(vote(), LinkStatus::Shared, None, &alice, None)
        .await
        .unwrap();
    assert!(
        !queued(&alone),
        "a single owner's own vote already ran its pass"
    );

    let mut shared = setup(Some(vec![alice_did, bob_did])).await;
    shared
        .add_link(vote(), LinkStatus::Shared, None, &alice, None)
        .await
        .unwrap();
    assert!(
        queued(&shared),
        "Bob's pass was not queued after Alice's vote"
    );
    shared.settle_flow_passes().await;
}

/// A signed `Local` link from a client that names no graph lands in the Local
/// graph of the agent the request reads as (#1357), like a plain write.
#[tokio::test]
async fn a_signed_local_link_without_a_graph_lands_in_the_viewers_local_graph() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let shared = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let mut p = shared.clone().for_viewer(alice_did.clone());
    let signed = LinkExpression::from(
        crate::agent::create_signed_expression(link("signed").normalize(), &alice).unwrap(),
    );
    assert_eq!(signed.graph, None);

    let written = p
        .add_link_expression(signed, LinkStatus::Local, None)
        .await
        .unwrap();
    assert_eq!(
        (written.graph, written.status),
        (Some(local_graph_iri(&alice_did)), Some(LinkStatus::Local))
    );
    assert!(
        shared
            .clone()
            .for_viewer(bob_did)
            .get_links(&LinkQuery::default())
            .await
            .unwrap()
            .is_empty(),
        "Bob co-owns the perspective but reads Alice's signed Local link"
    );
}

/// A subscription's first result is computed as its subscriber reads: their
/// own Local rows in, other agents' out. The shared instance reads no Local
/// graph (#1358), so a subscription that ran its first query there would miss
/// the subscriber's own data until the next change.
#[tokio::test]
async fn a_subscriptions_first_result_holds_its_subscribers_local_rows() {
    let (alice, alice_did) = user("alice");
    let (bob, bob_did) = user("bob");
    let mut p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    let shacl = r#"{
        "target_class": "t://Note",
        "constructor_actions": [
            {"action": "addLink", "source": "this", "predicate": "rdf://type", "target": "t://Note"}
        ],
        "destructor_actions": [],
        "properties": [
            {
                "path": "t://text", "name": "text", "datatype": "xsd://string",
                "min_count": 1, "max_count": 1, "writable": true,
                "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "t://text", "target": "value"}]
            }
        ]
    }"#;
    p.add_sdna(
        "Note".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        &main,
    )
    .await
    .unwrap();
    let local = Some(LOCAL_GRAPH_ALIAS.to_string());
    for (id, ctx, graph) in [
        ("t://note/alice", &alice, local.clone()),
        ("t://note/bob", &bob, local),
        ("t://note/shared", &main, None),
    ] {
        p.create_subject(
            SubjectClassOption {
                class_name: Some("Note".to_string()),
                query: None,
            },
            id.to_string(),
            Some(serde_json::json!({ "text": id })),
            None,
            ctx,
            graph,
        )
        .await
        .unwrap();
    }

    let (_, sparql) = p
        .subscribe_and_query(
            "SELECT ?s WHERE { ?s <t://text> ?o }".to_string(),
            alice.user_email.clone(),
        )
        .await
        .unwrap();
    let rows: Vec<Value> = serde_json::from_str(&sparql).unwrap();
    let mut subjects: Vec<&str> = rows.iter().filter_map(|r| r["s"].as_str()).collect();
    subjects.sort();
    assert_eq!(
        subjects,
        vec!["t://note/alice", "t://note/shared"],
        "SPARQL subscription's first result: {sparql}"
    );

    let (_, model) = p
        .model_subscribe_and_query(
            "Note".to_string(),
            "{}".to_string(),
            alice.user_email.clone(),
            None,
        )
        .await
        .unwrap();
    let v: Value = serde_json::from_str(&model).unwrap();
    let mut ids: Vec<&str> = v["instances"]
        .as_array()
        .unwrap_or_else(|| panic!("no instances in {model}"))
        .iter()
        .filter_map(|i| i["id"].as_str())
        .collect();
    ids.sort();
    assert_eq!(
        ids,
        vec!["t://note/alice", "t://note/shared"],
        "model subscription's first result: {model}"
    );
}

// ---------------------------------------------------------------------------
// Marvin's review of 9eb5c58fd (#812)
// ---------------------------------------------------------------------------

/// A notification trigger reads its owner's Local graph, so the event that
/// carries its match reaches that owner's sessions only, not every co-owner
/// of the perspective (review item 2, the rule #1324 set for subscriptions).
#[tokio::test]
async fn a_triggered_notification_reaches_only_its_owner() {
    use crate::api::events_ws::matches_notification_owner;
    use crate::pubsub::RUNTIME_NOTIFICATION_TRIGGERED_TOPIC;

    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    // The session filter checks ownership through the global registry.
    super::register_perspective(p.uuid.clone(), p.clone());
    let notification = crate::types::Notification {
        id: Uuid::new_v4().to_string(),
        granted: true,
        description: "mine".to_string(),
        app_name: "t".to_string(),
        app_url: "t".to_string(),
        app_icon_path: "t".to_string(),
        trigger: "SELECT ?s WHERE { ?s <ad4m://p> ?o }".to_string(),
        perspective_ids: vec![p.uuid.clone()],
        webhook_url: String::new(),
        webhook_auth: String::new(),
        user_email: alice.user_email.clone(),
    };

    let mut events = get_global_pubsub()
        .await
        .subscribe(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC)
        .await;
    PerspectiveInstance::publish_notification_matches(
        p.uuid.clone(),
        std::collections::BTreeMap::from([(
            notification,
            vec![serde_json::json!({ "s": "ad4m://s/alice-private" })],
        )]),
    )
    .await;
    let msg = loop {
        let msg = tokio::time::timeout(std::time::Duration::from_secs(5), events.recv())
            .await
            .expect("no notification was published")
            .unwrap();
        let v: Value = serde_json::from_str(&msg).unwrap();
        if v["perspectiveUuid"].as_str() == Some(p.uuid.as_str()) {
            break msg;
        }
    };
    assert!(
        matches_notification_owner(&msg, Some(&alice_did), false),
        "Alice's session gets her notification: {msg}"
    );
    assert!(
        !matches_notification_owner(&msg, Some(&bob_did), false),
        "Bob co-owns the perspective but the notification is Alice's: {msg}"
    );
    assert!(
        !matches_notification_owner(&msg, None, false),
        "a session whose DID hasn't resolved gets nothing"
    );
    super::unregister_perspective(&p.uuid);
}

/// Alice's link `name`, stored twice: in the default graph, then re-added
/// as the same signed expression with status Local, which puts the copy in
/// her Local graph. The reifier IRI leaves the graph out, so both copies
/// match one author + timestamp + triple.
async fn one_expression_in_two_graphs(
    p: &PerspectiveInstance,
    alice: &AgentContext,
    alice_did: &str,
    name: &str,
) -> LinkExpression {
    let mut view = p.clone().for_viewer(alice_did.to_string());
    let shared = view
        .add_link(link(name), LinkStatus::Shared, None, alice, None)
        .await
        .unwrap();
    let expression = LinkExpression::from(shared);
    let mut again = expression.clone();
    again.graph = None;
    view.add_link_expression(again, LinkStatus::Local, None)
        .await
        .unwrap();
    assert_eq!(
        graphs_of(p, alice_did, name).await,
        vec![None, Some(local_graph_iri(alice_did))]
    );
    expression
}

/// The graphs `viewer` reads link `name` in, sorted (`None` = default graph).
async fn graphs_of(p: &PerspectiveInstance, viewer: &str, name: &str) -> Vec<Option<String>> {
    let mut graphs: Vec<Option<String>> = p
        .clone()
        .for_viewer(viewer.to_string())
        .get_links(&LinkQuery {
            source: Some(format!("ad4m://s/{name}")),
            ..Default::default()
        })
        .await
        .unwrap()
        .into_iter()
        .map(|l| l.graph)
        .collect();
    graphs.sort();
    graphs
}

/// With one expression in two graphs, a removal or an update acts on the
/// copy in the graph the request names (none = the default graph), never on
/// whichever copy the store returns first. A copy the caller cannot see never
/// hides one it can (review item 3).
#[tokio::test]
async fn an_expression_in_two_graphs_is_removed_and_updated_where_the_request_names() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let p = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let local = local_graph_iri(&alice_did);
    let mut alice_view = p.clone().for_viewer(alice_did.clone());

    // Remove the Local copy: the shared one stays.
    let expr = one_expression_in_two_graphs(&p, &alice, &alice_did, "a").await;
    let mut named = expr.clone();
    named.graph = Some(LOCAL_GRAPH_ALIAS.to_string());
    let removed = alice_view.remove_link(named, None).await.unwrap();
    assert_eq!(removed.graph, Some(local.clone()));
    assert_eq!(graphs_of(&p, &alice_did, "a").await, vec![None]);

    // Remove naming no graph: the default-graph copy goes, the Local one stays.
    let expr = one_expression_in_two_graphs(&p, &alice, &alice_did, "b").await;
    let removed = alice_view.remove_link(expr, None).await.unwrap();
    assert_eq!(removed.graph, None);
    assert_eq!(graphs_of(&p, &alice_did, "b").await, vec![Some(local.clone())]);

    // Update the Local copy: the shared one is untouched.
    let expr = one_expression_in_two_graphs(&p, &alice, &alice_did, "c").await;
    let mut named = expr.clone();
    named.graph = Some(local.clone());
    let updated = alice_view
        .update_link(named, link("c-new"), None, &alice)
        .await
        .unwrap();
    assert_eq!(updated.graph, Some(local.clone()));
    assert_eq!(graphs_of(&p, &alice_did, "c").await, vec![None]);
    assert_eq!(graphs_of(&p, &alice_did, "c-new").await, vec![Some(local.clone())]);

    // Bob reads only the shared copy, so his removal acts on it, even when
    // Alice's Local copy is the one the store holds first.
    let expr = one_expression_in_two_graphs(&p, &alice, &alice_did, "d").await;
    let removed = p
        .clone()
        .for_viewer(bob_did.clone())
        .remove_links(vec![expr], None)
        .await
        .unwrap();
    assert_eq!(removed.len(), 1, "Bob's removal found nothing");
    assert_eq!(removed[0].graph, None);
    assert_eq!(graphs_of(&p, &alice_did, "d").await, vec![Some(local)]);
}

/// A shape acts for every agent, so a Local graph can't give its property an
/// initial value either: Bob's Local constructor on a shared shape is not
/// read (review item 4).
#[tokio::test]
async fn a_local_constructor_does_not_set_a_shared_shapes_initial_value() {
    use super::model_query::shape::load_shape_from_store;
    let (bob, bob_did) = user("bob");
    let mut p = setup(Some(vec![bob_did.clone()])).await;
    let main = AgentContext::main_agent();
    let shacl = r#"{
        "target_class": "t://Note",
        "constructor_actions": [],
        "destructor_actions": [],
        "properties": [
            {
                "path": "t://text", "name": "text", "datatype": "xsd://string",
                "min_count": 0, "max_count": 1, "writable": true,
                "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "t://text", "target": "value"}]
            }
        ]
    }"#;
    p.add_sdna(
        "Note".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        &main,
    )
    .await
    .unwrap();
    // Drop the shared (empty) constructor, so Bob's Local one is the only
    // constructor in the store.
    let ctor = p
        .get_links(&LinkQuery {
            predicate: Some("ad4m://constructor".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(ctor.len(), 1, "{ctor:?}");
    let shape_uri = ctor[0].data.source.clone();
    p.remove_link(LinkExpression::from(ctor[0].clone()), None)
        .await
        .unwrap();
    p.add_link(
        Link {
            source: shape_uri,
            predicate: Some("ad4m://constructor".to_string()),
            target: r#"literal:string:[{"action":"addLink","source":"this","predicate":"t://text","target":"literal:string:planted"}]"#
                .to_string(),
        },
        LinkStatus::Shared,
        None,
        &bob,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();

    let shape = load_shape_from_store(&p.sparql_store, "Note").unwrap();
    let text = shape
        .properties
        .iter()
        .find(|prop| prop.name == "text")
        .expect("the shape has `text`");
    assert_eq!(text.initial_value, None, "Bob's Local constructor was read");
}

/// A `link-updated` event names its links `oldLink` and `newLink`. A session
/// whose DID hasn't resolved gets no update of a Local link (review item 5).
#[test]
fn a_local_link_update_does_not_reach_a_session_without_a_did() {
    use crate::api::events_ws::matches_owner;
    let local = serde_json::json!({ "graph": local_graph_iri("did:key:alice") });
    let shared = serde_json::json!({ "graph": SHARED_GRAPH });
    let update = |old: &Value, new: &Value| {
        serde_json::json!({
            "perspectiveUuid": "p",
            "owner": "did:key:alice",
            "oldLink": old,
            "newLink": new,
        })
        .to_string()
    };
    assert!(!matches_owner(&update(&local, &local), None));
    assert!(!matches_owner(&update(&local, &shared), None));
    assert!(!matches_owner(&update(&shared, &local), None));
    assert!(matches_owner(&update(&shared, &shared), None));
    assert!(matches_owner(
        &update(&local, &local),
        Some("did:key:alice")
    ));
}

/// A signed `Local` link with no viewer to write for lands in the main
/// agent's Local graph, as `add_link` puts it: a Local link always lives in a
/// Local graph (review item 6).
#[tokio::test]
async fn a_local_link_expression_without_a_viewer_lands_in_the_main_agents_local_graph() {
    let mut p = setup(None).await;
    let main = AgentContext::main_agent();
    let main_did = crate::agent::did_for_context(&main).unwrap();
    let signed = LinkExpression::from(
        crate::agent::create_signed_expression(link("unstamped").normalize(), &main).unwrap(),
    );
    let written = p
        .add_link_expression(signed, LinkStatus::Local, None)
        .await
        .unwrap();
    assert_eq!(
        (written.graph, written.status),
        (Some(local_graph_iri(&main_did)), Some(LinkStatus::Local))
    );
}

/// A signed link that names the own graph of a Local subject goes to that
/// Local graph, as `add_link` redirects it; it must not create the shared
/// graph `ad4m://graph/<S>` and sync (review item 6).
#[tokio::test]
async fn a_signed_link_into_a_local_subjects_own_graph_stays_local() {
    let (alice, alice_did) = user("alice");
    let (_, bob_did) = user("bob");
    let shared = setup(Some(vec![alice_did.clone(), bob_did.clone()])).await;
    let mut p = shared.clone().for_viewer(alice_did.clone());
    p.add_link(
        Link {
            source: "ad4m://obj/draft".to_string(),
            predicate: Some("ad4m://title".to_string()),
            target: "literal:string:mine".to_string(),
        },
        LinkStatus::Shared,
        None,
        &alice,
        Some(LOCAL_GRAPH_ALIAS.to_string()),
    )
    .await
    .unwrap();
    let mut signed = LinkExpression::from(
        crate::agent::create_signed_expression(
            Link {
                source: "ad4m://obj/draft".to_string(),
                predicate: Some("ad4m://has_child".to_string()),
                target: "ad4m://obj/child".to_string(),
            }
            .normalize(),
            &alice,
        )
        .unwrap(),
    );
    signed.graph = Some("ad4m://graph/ad4m://obj/draft".to_string());

    let written = p
        .add_link_expression(signed, LinkStatus::Shared, None)
        .await
        .unwrap();
    assert_eq!(
        (written.graph, written.status),
        (Some(local_graph_iri(&alice_did)), Some(LinkStatus::Local))
    );
    assert!(
        !shared
            .sparql_store
            .contains_named_graph("ad4m://graph/ad4m://obj/draft"),
        "the Local subject's own graph was created"
    );
}

/// A Local link written before #1357 sits in the default graph. It never
/// reaches the link language either, so a Shared removal can't send its
/// triple (review note 7).
#[test]
fn a_legacy_local_link_in_the_default_graph_is_not_shareable() {
    init();
    let mut legacy = LinkExpression::from(
        crate::agent::create_signed_expression(
            link("legacy").normalize(),
            &AgentContext::main_agent(),
        )
        .unwrap(),
    );
    legacy.status = Some(LinkStatus::Local);
    let public = shared_in("public", SHARED_GRAPH);
    let out = super::perspective_instance::shareable(&PerspectiveDiff {
        additions: vec![legacy.clone(), public],
        removals: vec![legacy],
    });
    let added: Vec<&str> = out.additions.iter().map(|l| l.data.source.as_str()).collect();
    assert_eq!(added, vec!["ad4m://s/public"]);
    assert!(out.removals.is_empty(), "{:?}", out.removals);
}

/// An `EXISTS` inside an expression can't reach a Local graph a read leaves
/// out: the scope is set on the dataset, not by rewriting the query. Eight
/// places an `EXISTS` can hide, each with a plain and a `GRAPH ?g` probe, on
/// a viewer (Bob's Local graph) and on a shared-reads view (its own viewer's
/// Local graph). The control probe on a shared link changes every result, so
/// each query would show a leak (review note 8b).
#[tokio::test]
async fn an_exists_in_any_expression_reads_no_hidden_local_graph() {
    let (p, alice_did, bob_did) = two_users_and_shared_links().await;
    const SHAPES: [(&str, &str); 8] = [
        ("BIND/IF", "SELECT ?s ?r WHERE { ?s <ad4m://p> ?o . BIND(IF(PROBE, 1, 0) AS ?r) }"),
        ("FILTER NOT EXISTS", "SELECT ?s WHERE { ?s <ad4m://p> ?o . FILTER(NOT PROBE) }"),
        ("ORDER BY", "SELECT ?s WHERE { { ?s <ad4m://p> ?o } UNION { GRAPH ?h { ?s <ad4m://p> ?o } } } ORDER BY (IF(PROBE, 0 - STRLEN(STR(?s)), STRLEN(STR(?s))))"),
        ("HAVING", "SELECT ?s WHERE { ?s <ad4m://p> ?o } GROUP BY ?s HAVING (PROBE)"),
        ("aggregate", "SELECT (SUM(IF(PROBE, 1, 0)) AS ?n) WHERE { ?s <ad4m://p> ?o }"),
        ("sub-select", "SELECT ?s ?r WHERE { { SELECT ?s (IF(PROBE, 1, 0) AS ?r) WHERE { ?s <ad4m://p> ?o } } }"),
        ("OPTIONAL filter", "SELECT ?s ?m WHERE { ?s <ad4m://p> ?o . OPTIONAL { ?m <ad4m://p> ?o2 . FILTER(PROBE) } }"),
        ("nested EXISTS + VALUES", "SELECT ?s ?r WHERE { ?s <ad4m://p> ?o . BIND(IF(EXISTS { VALUES ?n { 1 } FILTER(PROBE) }, 1, 0) AS ?r) }"),
    ];
    let probe = |subject: &str, graph_var: bool| {
        if graph_var {
            format!("EXISTS {{ GRAPH ?g {{ <ad4m://s/{subject}> ?pp ?x }} }}")
        } else {
            format!("EXISTS {{ <ad4m://s/{subject}> ?pp ?x }}")
        }
    };
    let views = [
        ("Alice", p.clone().for_viewer(alice_did)),
        ("Bob, shared reads", p.clone().for_viewer(bob_did).for_shared_reads()),
    ];
    for (who, view) in &views {
        for (shape, template) in SHAPES {
            for graph_var in [false, true] {
                let run = |subject: &str| {
                    let query = template.replace("PROBE", &probe(subject, graph_var));
                    let json = view
                        .sparql_query(query.clone())
                        .unwrap_or_else(|e| panic!("{shape}: {e:#}\n{query}"));
                    let mut rows: Vec<String> = serde_json::from_str::<Vec<Value>>(&json)
                        .unwrap()
                        .iter()
                        .map(Value::to_string)
                        .collect();
                    if shape != "ORDER BY" {
                        rows.sort();
                    }
                    rows
                };
                let absent = run("nothing-here");
                assert_eq!(
                    run("bob"),
                    absent,
                    "{who}: {shape} (GRAPH ?g: {graph_var}) read Bob's Local link"
                );
                assert_ne!(
                    run("shared"),
                    absent,
                    "{who}: {shape} (GRAPH ?g: {graph_var}) can't tell a link apart"
                );
            }
        }
    }
}
