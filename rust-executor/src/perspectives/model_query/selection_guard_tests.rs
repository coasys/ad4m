//! #1120: a link that is withheld from hydration must not select an instance
//! either.
//!
//! #1113 withholds unverified links from the rows that hydrate an instance, and
//! #1116's `linkStatus` withholds links of the other status. The patterns that
//! decide *which* instances match (pushed `where`, the class's flags, `COUNT` /
//! `totalCount`, the two-phase plan's id phase and order keys, `$` projections)
//! must read the same links. Otherwise a forged `agent` link with an admin as
//! its claimed author satisfies `{ agent: { eq: X, author: admin } }`, which is
//! the role check #1103 and #1063 build on.
//!
//! Every fixture signs with real [`TestSigner`] keys. A forged link claims a
//! signer as its author and carries that signer's signature over a different
//! target, which is what a peer without the key can gossip. Each test also runs
//! the query with `includeUnverified` (or without `linkStatus`) and asserts the
//! old answer, so none of them can pass because the withheld link never
//! mattered.

use super::test_helpers::execute_model_query_from_json;
use super::types::{ModelQueryInput, ModelQueryResult};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};

const SG_SHAPE_JSON: &str = r#"{
    "className": "Grant",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Grant"},
        "agent": {"predicate":"sg://agent","required":false},
        "name": {"predicate":"sg://name","required":false}
    },
    "relations": {
        "members": { "predicate": "sg://member", "kind": "hasMany", "targetClassName": "" },
        "replies": { "predicate": "sg://reply", "kind": "hasMany", "targetClassName": "" }
    }
}"#;

fn sg_at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 9, 25, 12, 0, second)
        .unwrap()
}

fn sg_link(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
    status: LinkStatus,
) -> LinkExpression {
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        sg_at(second),
    ));
    l.status = Some(status);
    l
}

/// A Shared link signed by `signer`.
fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    sg_link(
        signer,
        source,
        predicate,
        target,
        second,
        LinkStatus::Shared,
    )
}

/// A Shared link claiming `signer` as its author, carrying the signer's
/// signature over another target.
fn forged(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    let mut l = signed(
        signer,
        source,
        predicate,
        "literal:string:what-was-signed",
        second,
    );
    l.data.target = target.to_string();
    assert!(!l.compute_proof_valid(), "the fixture must not verify");
    l
}

fn add(store: &SparqlStore, links: impl IntoIterator<Item = LinkExpression>) {
    for l in links {
        store.add_link(&l).unwrap();
    }
}

async fn run(store: &SparqlStore, query: Value) -> ModelQueryResult {
    let query: ModelQueryInput = serde_json::from_value(query.clone())
        .unwrap_or_else(|e| panic!("query {query} does not parse: {e}"));
    execute_model_query_from_json(store, "Grant", &query, SG_SHAPE_JSON)
        .await
        .unwrap()
}

fn ids(result: &ModelQueryResult) -> Vec<String> {
    result
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap().to_string())
        .collect()
}

/// `query` with `includeUnverified: true` added.
fn opted_in(mut query: Value) -> Value {
    query["includeUnverified"] = json!(true);
    query
}

/// The role check #1103 builds on: "admin wrote an `agent -> mallory` link".
/// Mallory has no admin key and forges one on a grant admin created. It must
/// not satisfy the nested `author`, a membership condition on a relation, or
/// the side-by-side form, on either instance-query plan.
///
/// The side-by-side case is the instance-level half: hydration takes the
/// instance's `author` from its earliest *verified* link, so a forged earlier
/// link claiming admin must not make the instance's author admin in the filter
/// while the row shows someone else.
#[tokio::test]
async fn selection_a_forged_admin_link_does_not_satisfy_an_author_condition() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    let bob = TestSigner::generate();
    add(
        &store,
        [
            // g1: admin's grant, with a forged agent and a forged member.
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            forged(
                &admin,
                "sg://g/1",
                "sg://agent",
                "literal:string:mallory",
                2,
            ),
            forged(&admin, "sg://g/1", "sg://member", "sg://a/mallory", 2),
            // g2: the genuine control.
            signed(&admin, "sg://g/2", "ad4m://type", "sg://Grant", 3),
            signed(&admin, "sg://g/2", "sg://agent", "literal:string:carol", 4),
            signed(&admin, "sg://g/2", "sg://member", "sg://a/carol", 4),
            // g3: bob's grant, admin genuinely wrote `agent -> dave`, and a
            // forged earlier link claims admin as the instance's first author.
            forged(&admin, "sg://g/3", "sg://name", "literal:string:early", 0),
            signed(&bob, "sg://g/3", "ad4m://type", "sg://Grant", 5),
            signed(&admin, "sg://g/3", "sg://agent", "literal:string:dave", 6),
        ],
    );

    let cases = [
        (
            json!({ "agent": { "eq": "mallory", "author": admin.did } }),
            vec![],
            vec!["sg://g/1"],
        ),
        (
            json!({ "agent": { "eq": "carol", "author": admin.did } }),
            vec!["sg://g/2"],
            vec!["sg://g/2"],
        ),
        (
            json!({ "members": "sg://a/mallory" }),
            vec![],
            vec!["sg://g/1"],
        ),
        (
            json!({ "members": { "eq": "sg://a/mallory", "author": admin.did } }),
            vec![],
            vec!["sg://g/1"],
        ),
        (
            json!({ "members": "sg://a/carol" }),
            vec!["sg://g/2"],
            vec!["sg://g/2"],
        ),
        (
            json!({ "agent": "dave", "author": admin.did }),
            vec![],
            vec!["sg://g/3"],
        ),
    ];
    for (clause, expected, expected_opted_in) in cases {
        for limit in [json!(null), json!(10)] {
            let query = json!({ "where": clause, "limit": limit });
            let got = run(&store, query.clone()).await;
            assert_eq!(ids(&got), expected, "{query}");
            assert_eq!(got.total_count, expected.len(), "{query}: totalCount");
            let got = run(&store, opted_in(query.clone())).await;
            assert_eq!(ids(&got), expected_opted_in, "{query} with the opt-in");
        }
    }
}

/// A forged flag must not make a node an instance of the class: not as a row,
/// not in a `limit: 0` count, and not in `totalCount`.
#[tokio::test]
async fn selection_a_forged_flag_does_not_make_a_node_conform() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            // n: a genuine node of no class, re-typed by a forged flag.
            signed(&admin, "sg://n/1", "sg://name", "literal:string:n", 2),
            forged(&admin, "sg://n/1", "ad4m://type", "sg://Grant", 3),
        ],
    );

    for limit in [json!(null), json!(10)] {
        let query = json!({ "limit": limit });
        let got = run(&store, query.clone()).await;
        assert_eq!(ids(&got), vec!["sg://g/1"], "{query}");
        assert_eq!(got.total_count, 1, "{query}: totalCount");
        let got = run(&store, opted_in(query.clone())).await;
        assert_eq!(got.total_count, 2, "{query} with the opt-in");
    }
    let count = run(&store, json!({ "limit": 0 })).await;
    assert_eq!(count.total_count, 1, "a forged flag was counted");
    let count = run(&store, opted_in(json!({ "limit": 0 }))).await;
    assert_eq!(count.total_count, 2, "the opt-in counts it");
}

/// Grants `a`, `b`, `c`, typed in that order... except that `c`'s type link is
/// the earliest, so `c` sorts first by timestamp. `a` and `b` are named `x`;
/// `c` is named `y` and carries a forged `x`.
fn seed_page(store: &SparqlStore, admin: &TestSigner) {
    add(
        store,
        [
            signed(admin, "sg://g/c", "ad4m://type", "sg://Grant", 0),
            signed(admin, "sg://g/a", "ad4m://type", "sg://Grant", 1),
            signed(admin, "sg://g/b", "ad4m://type", "sg://Grant", 2),
            signed(admin, "sg://g/a", "sg://name", "literal:string:x", 3),
            signed(admin, "sg://g/b", "sg://name", "literal:string:x", 3),
            signed(admin, "sg://g/c", "sg://name", "literal:string:y", 3),
            forged(admin, "sg://g/c", "sg://name", "literal:string:x", 4),
        ],
    );
}

/// A forged value must not select an instance into `totalCount` or a count,
/// and must not exclude one through `NOT` either.
#[tokio::test]
async fn selection_a_forged_value_is_not_counted() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    seed_page(&store, &admin);

    let by_x = json!({ "where": { "name": "x" } });
    for limit in [json!(null), json!(2), json!(0)] {
        let mut query = by_x.clone();
        query["limit"] = limit;
        let got = run(&store, query.clone()).await;
        assert_eq!(got.total_count, 2, "{query}: a forged value was counted");
        let got = run(&store, opted_in(query.clone())).await;
        assert_eq!(got.total_count, 3, "{query}: the opt-in counts it");
    }

    let not_x = json!({ "where": { "NOT": { "name": "x" } } });
    let got = run(&store, not_x.clone()).await;
    assert_eq!(
        ids(&got),
        vec!["sg://g/c"],
        "a forged value must not exclude"
    );
    let got = run(&store, opted_in(not_x)).await;
    assert!(ids(&got).is_empty(), "the opt-in lets it exclude");
}

/// A forged value must not select an instance into a page. `c` sorts first,
/// so if its forged `x` selected it, it would take a slot on page 1 and push
/// `b` onto page 2.
#[tokio::test]
async fn selection_a_forged_value_does_not_move_an_instance_across_a_page() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    seed_page(&store, &admin);

    let page = |offset: usize| json!({ "where": { "name": "x" }, "limit": 2, "offset": offset });
    let first = run(&store, page(0)).await;
    assert_eq!(ids(&first), vec!["sg://g/a", "sg://g/b"], "page 1");
    assert_eq!(first.total_count, 2, "page 1: totalCount");
    let second = run(&store, page(2)).await;
    assert!(ids(&second).is_empty(), "page 2: {:?}", ids(&second));

    let first = run(&store, opted_in(page(0))).await;
    assert_eq!(
        ids(&first),
        vec!["sg://g/c", "sg://g/a"],
        "the opt-in selects c onto page 1"
    );
}

/// A transitive `$` projection walks the relation to any depth. A forged link
/// on the path must not add what lies behind it to the count or the list.
///
/// `g/1 -reply-> r/1` is genuine, `r/1 -reply-> r/2` is forged, and
/// `r/2 -reply-> r/3` is genuine. Only `r/1` is reachable over verified links.
#[tokio::test]
async fn selection_a_forged_link_in_a_transitive_projection_is_not_counted() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/1", "sg://reply", "sg://r/1", 2),
            forged(&admin, "sg://r/1", "sg://reply", "sg://r/2", 3),
            signed(&admin, "sg://r/2", "sg://reply", "sg://r/3", 4),
        ],
    );
    let query = json!({
        "projections": {
            "$n": { "from": "replies", "count": true, "transitive": true },
            "$all": { "from": "replies", "transitive": true, "order": { "id": "ASC" } },
        }
    });

    let got = run(&store, query.clone()).await;
    assert_eq!(got.instances[0]["$n"], json!(1), "{}", got.instances[0]);
    assert_eq!(
        got.instances[0]["$all"],
        json!(["sg://r/1"]),
        "{}",
        got.instances[0]
    );

    let got = run(&store, opted_in(query)).await;
    assert_eq!(got.instances[0]["$n"], json!(3), "the opt-in walks it");
    assert_eq!(
        got.instances[0]["$all"],
        json!(["sg://r/1", "sg://r/2", "sg://r/3"])
    );
}

/// `linkStatus` restricts selection the same way: a Local flag does not make a
/// node conform under `shared`, a Local value does not select, and a Local
/// name that sorts first does not reorder a Shared-only page.
///
/// The one-reifier rule from #1124 holds for selection too: a verified Local
/// link and a forged Shared link on the same triple do not jointly satisfy a
/// Shared, verified-only `where`.
#[tokio::test]
async fn selection_link_status_restricts_selection_and_order() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    let local =
        |s: &str, p: &str, t: &str, sec: u32| sg_link(&admin, s, p, t, sec, LinkStatus::Local);
    add(
        &store,
        [
            signed(&admin, "sg://g/a", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/b", "ad4m://type", "sg://Grant", 2),
            signed(&admin, "sg://g/a", "sg://name", "literal:string:m", 3),
            // b's only name is Local, and sorts before a's.
            local("sg://g/b", "sg://name", "literal:string:a", 4),
            // l: typed by a Local flag only.
            local("sg://g/l", "ad4m://type", "sg://Grant", 5),
            // A Local agent on a, and on b the same triple Local-valid plus
            // Shared-forged.
            local("sg://g/a", "sg://agent", "literal:string:secret", 6),
            local("sg://g/b", "sg://agent", "literal:string:both", 6),
            forged(&admin, "sg://g/b", "sg://agent", "literal:string:both", 7),
        ],
    );

    let shared = |mut q: Value| {
        q["linkStatus"] = json!("shared");
        q
    };

    let all = run(&store, shared(json!({ "limit": 10 }))).await;
    assert_eq!(
        ids(&all),
        vec!["sg://g/a", "sg://g/b"],
        "a Local flag conformed"
    );
    assert_eq!(all.total_count, 2, "totalCount");
    let count = run(&store, shared(json!({ "limit": 0 }))).await;
    assert_eq!(count.total_count, 2, "a Local flag was counted");
    let unrestricted = run(&store, json!({ "limit": 0 })).await;
    assert_eq!(unrestricted.total_count, 3, "without linkStatus it counts");

    for clause in [
        json!({ "agent": "secret" }),
        json!({ "agent": { "eq": "secret", "author": admin.did } }),
        json!({ "agent": "both" }),
    ] {
        for limit in [json!(null), json!(10)] {
            let query = shared(json!({ "where": clause, "limit": limit }));
            let got = run(&store, query.clone()).await;
            assert!(ids(&got).is_empty(), "{query}: {:?}", ids(&got));
            assert_eq!(got.total_count, 0, "{query}: totalCount");
        }
    }
    let got = run(&store, json!({ "where": { "agent": "secret" } })).await;
    assert_eq!(ids(&got), vec!["sg://g/a"], "without linkStatus it selects");

    let by_name = |q: Value| {
        let mut q = q;
        q["order"] = json!({ "name": "ASC" });
        q["limit"] = json!(1);
        q
    };
    let first = run(&store, shared(by_name(json!({})))).await;
    assert_eq!(
        ids(&first),
        vec!["sg://g/a"],
        "a Local name reordered the page"
    );
    let first = run(&store, by_name(json!({}))).await;
    assert_eq!(
        ids(&first),
        vec!["sg://g/b"],
        "without linkStatus the Local name sorts first"
    );
}
