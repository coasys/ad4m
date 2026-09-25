//! #1024 on top of #1120: another agent's `Local` link does not select, count
//! or scope an instance for a viewer, and visibility sits on the same reifier
//! as the verdict and the status.
//!
//! Alice and Bob co-own one perspective. Every test reads as Bob, as Alice,
//! and in executor scope, so none of them can pass because the Local link
//! never mattered.

use super::super::test_helpers::execute_model_query_from_json_for_viewer;
use super::super::types::{ModelQueryInput, ModelQueryResult};
use super::{add, forged, ids, opted_in, sg_link, signed, SG_SHAPE_JSON};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkStatus;
use serde_json::{json, Value};

async fn run_as(store: &SparqlStore, query: Value, viewer: &str) -> ModelQueryResult {
    let query: ModelQueryInput = serde_json::from_value(query.clone())
        .unwrap_or_else(|e| panic!("query {query} does not parse: {e}"));
    execute_model_query_from_json_for_viewer(store, "Grant", &query, SG_SHAPE_JSON, Some(viewer))
        .await
        .unwrap()
}

async fn run_in_executor_scope(store: &SparqlStore, query: Value) -> ModelQueryResult {
    let query: ModelQueryInput = serde_json::from_value(query).unwrap();
    execute_model_query_from_json_for_viewer(store, "Grant", &query, SG_SHAPE_JSON, None)
        .await
        .unwrap()
}

/// A `where` on a value Bob cannot read does not select for Bob, under the
/// default proof filter, with `includeUnverified`, and in `count`.
///
/// `g` is a Grant by its Shared flag, which Bob may see, and its only `agent`
/// is Alice's verified Local link. Before the viewer sat in the guard, Bob's
/// `where { agent }` matched that link and returned `g` with no `agent`: the
/// row set showed Bob which value Alice had stored.
///
/// The second half is the one-reifier rule. `h` carries Alice's verified Local
/// `agent` and a forged Shared link on the same triple. Under the default Bob
/// may see the forged link but it did not verify, and the verified link is
/// Alice's, so no single link passes and `h` is not selected. Checked on two
/// reifiers, the forged link would pass visibility and Alice's the verdict.
/// With `includeUnverified` the forged link alone passes, and Bob gets `h`.
#[tokio::test]
async fn another_users_local_value_does_not_select_for_a_viewer() {
    let store = SparqlStore::new(None).unwrap();
    let alice = TestSigner::generate();
    let carol = TestSigner::generate();
    let bob = TestSigner::generate();
    let alice_local =
        |s: &str, p: &str, t: &str, sec: u32| sg_link(&alice, s, p, t, sec, LinkStatus::Local);
    add(
        &store,
        [
            signed(&alice, "sg://g/g", "ad4m://type", "sg://Grant", 1),
            alice_local("sg://g/g", "sg://agent", "literal:string:secret", 2),
            signed(&alice, "sg://g/h", "ad4m://type", "sg://Grant", 3),
            alice_local("sg://g/h", "sg://agent", "literal:string:both", 4),
            forged(&carol, "sg://g/h", "sg://agent", "literal:string:both", 5),
        ],
    );
    let secret = json!({ "where": { "agent": "secret" }, "limit": 10 });
    let both = json!({ "where": { "agent": "both" }, "limit": 10 });

    let as_bob = run_as(&store, secret.clone(), &bob.did).await;
    assert_eq!(ids(&as_bob), Vec::<String>::new(), "Alice's Local value");
    assert_eq!(as_bob.total_count, 0, "totalCount");
    let count = run_as(
        &store,
        json!({ "where": { "agent": "secret" }, "limit": 0 }),
        &bob.did,
    )
    .await;
    assert_eq!(count.total_count, 0, "count()");
    let as_bob = run_as(&store, opted_in(secret.clone()), &bob.did).await;
    assert_eq!(
        ids(&as_bob),
        Vec::<String>::new(),
        "includeUnverified does not widen visibility"
    );

    let as_bob = run_as(&store, both.clone(), &bob.did).await;
    assert_eq!(
        ids(&as_bob),
        Vec::<String>::new(),
        "a visible forged link and an invisible verified one on one triple"
    );
    let as_bob = run_as(&store, opted_in(both.clone()), &bob.did).await;
    assert_eq!(
        ids(&as_bob),
        vec!["sg://g/h"],
        "with includeUnverified the forged Shared link selects"
    );

    // Alice's own Local value selects for her, and executor scope reads all.
    let as_alice = run_as(&store, secret.clone(), &alice.did).await;
    assert_eq!(ids(&as_alice), vec!["sg://g/g"], "Alice");
    assert_eq!(as_alice.instances[0]["agent"], "secret");
    let as_alice = run_as(&store, both, &alice.did).await;
    assert_eq!(ids(&as_alice), vec!["sg://g/h"], "Alice, same triple");
    let executor = run_in_executor_scope(&store, secret).await;
    assert_eq!(ids(&executor), vec!["sg://g/g"], "executor scope");
}

/// `linkStatus: 'local'` for a viewer means the viewer's own Local links: Bob
/// does not read Alice's Local links through it, and they do not select for
/// him.
///
/// `a` is typed by Alice's Local flag only, `b` by Bob's. `b`'s `agent` is
/// Alice's Local link. So under `local` Bob gets `b` alone and `where { agent }`
/// selects nothing for him; Alice gets `a` and cannot see `b`; executor scope
/// gets both, and `b` by `agent`.
#[tokio::test]
async fn link_status_local_is_the_viewers_own_local_links() {
    let store = SparqlStore::new(None).unwrap();
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    add(
        &store,
        [
            sg_link(
                &alice,
                "sg://g/a",
                "ad4m://type",
                "sg://Grant",
                1,
                LinkStatus::Local,
            ),
            sg_link(
                &bob,
                "sg://g/b",
                "ad4m://type",
                "sg://Grant",
                2,
                LinkStatus::Local,
            ),
            sg_link(
                &alice,
                "sg://g/b",
                "sg://agent",
                "literal:string:x",
                3,
                LinkStatus::Local,
            ),
        ],
    );
    let local = |mut q: Value| {
        q["linkStatus"] = json!("local");
        q
    };
    let all = local(json!({ "limit": 10 }));
    let by_agent = local(json!({ "where": { "agent": "x" }, "limit": 10 }));

    let as_bob = run_as(&store, all.clone(), &bob.did).await;
    assert_eq!(ids(&as_bob), vec!["sg://g/b"], "Bob's own Local flag");
    assert_eq!(as_bob.total_count, 1, "totalCount");
    assert!(
        as_bob.instances[0]["agent"].is_null(),
        "Alice's Local agent does not hydrate for Bob: {}",
        as_bob.instances[0]
    );
    let as_bob = run_as(&store, by_agent.clone(), &bob.did).await;
    assert_eq!(
        ids(&as_bob),
        Vec::<String>::new(),
        "Alice's Local agent selects b for Bob"
    );

    let as_alice = run_as(&store, all.clone(), &alice.did).await;
    assert_eq!(ids(&as_alice), vec!["sg://g/a"], "Alice's own Local flag");
    let as_alice = run_as(&store, by_agent.clone(), &alice.did).await;
    assert_eq!(
        ids(&as_alice),
        Vec::<String>::new(),
        "b is typed by Bob's Local flag, which Alice cannot see"
    );

    let executor = run_in_executor_scope(&store, all).await;
    assert_eq!(
        ids(&executor),
        vec!["sg://g/a", "sg://g/b"],
        "executor scope"
    );
    let executor = run_in_executor_scope(&store, by_agent).await;
    assert_eq!(ids(&executor), vec!["sg://g/b"], "executor scope, by agent");
}
