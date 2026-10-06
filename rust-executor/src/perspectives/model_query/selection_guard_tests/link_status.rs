//! #1120 under `linkStatus`: a link of the other status does not select, count
//! or order an instance, and status and verdict are read off one link.

use super::super::test_helpers::execute_model_query_from_json;
use super::super::types::ModelQueryInput;
use super::{add, forged, ids, run, sg_link, signed};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkStatus;
use serde_json::{json, Value};

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

/// A typed relation's `where` (`@HasMany(() => T, { through, where })`) reads
/// each target's values under the query's `linkStatus` too: a Local `status`
/// must not let a target through a Shared-only read.
#[tokio::test]
async fn selection_link_status_applies_to_a_relation_where() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/1", "sg://member", "sg://m/shared", 2),
            signed(&admin, "sg://g/1", "sg://member", "sg://m/local", 2),
            signed(
                &admin,
                "sg://m/shared",
                "sg://state",
                "literal:string:ok",
                3,
            ),
            sg_link(
                &admin,
                "sg://m/local",
                "sg://state",
                "literal:string:ok",
                3,
                LinkStatus::Local,
            ),
        ],
    );
    let shape_json = json!({
        "className": "Grant",
        "properties": {
            "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Grant"}
        },
        "relations": {
            "members": {
                "predicate": "sg://member",
                "kind": "hasMany",
                "targetClassName": "",
                "getter": "SELECT ?target WHERE { <Base> <sg://member> ?target . }",
                "whereFilter": {"state": "ok"},
                "wherePredicates": {"state": "sg://state"}
            }
        }
    })
    .to_string();
    let members = |link_status: Option<&'static str>| {
        let (store, shape_json) = (&store, &shape_json);
        async move {
            let mut out = Vec::new();
            for limit in [json!(null), json!(10)] {
                let query: ModelQueryInput =
                    serde_json::from_value(json!({ "limit": limit, "linkStatus": link_status }))
                        .unwrap();
                let got = execute_model_query_from_json(store, "Grant", &query, shape_json)
                    .await
                    .unwrap();
                let mut m: Vec<String> = got.instances[0]["members"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| v.as_str().unwrap().to_string())
                    .collect();
                m.sort();
                out.push(m);
            }
            assert_eq!(out[0], out[1], "both plans agree");
            out.remove(0)
        }
    };
    assert_eq!(members(Some("shared")).await, vec!["sg://m/shared"]);
    assert_eq!(members(None).await, vec!["sg://m/local", "sg://m/shared"]);
}

/// `linkStatus: "local"` reads the instance as it exists in Local links, its
/// flag included: a card flagged only in a Shared link is not a Local
/// instance, even with a Local note on it. On #1124 it was returned, with the
/// note hydrated. Without `linkStatus` the note stays readable.
#[tokio::test]
async fn selection_link_status_local_needs_a_local_flag() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    let local =
        |s: &str, p: &str, t: &str, sec: u32| sg_link(&admin, s, p, t, sec, LinkStatus::Local);
    add(
        &store,
        [
            // s: flagged only in a Shared link, with a Local note.
            signed(&admin, "sg://g/s", "ad4m://type", "sg://Grant", 1),
            local("sg://g/s", "sg://agent", "literal:string:secret", 2),
            // l: flagged in a Local link, so `local` is not empty.
            local("sg://g/l", "ad4m://type", "sg://Grant", 3),
            local("sg://g/l", "sg://agent", "literal:string:mine", 4),
        ],
    );

    let local_q = |mut q: Value| {
        q["linkStatus"] = json!("local");
        q
    };
    for limit in [json!(null), json!(10)] {
        let query = local_q(json!({ "limit": limit }));
        let got = run(&store, query.clone()).await;
        assert_eq!(
            ids(&got),
            vec!["sg://g/l"],
            "{query}: a Shared flag conformed"
        );
        assert_eq!(got.total_count, 1, "{query}: totalCount");

        let query = local_q(json!({ "where": { "agent": "secret" }, "limit": limit }));
        let got = run(&store, query.clone()).await;
        assert!(ids(&got).is_empty(), "{query}: {:?}", ids(&got));
        assert_eq!(got.total_count, 0, "{query}: totalCount");
    }
    let count = run(
        &store,
        local_q(json!({ "where": { "agent": "secret" }, "limit": 0 })),
    )
    .await;
    assert_eq!(
        count.total_count, 0,
        "limit 0 counted the Shared-flagged card"
    );
    let count = run(&store, local_q(json!({ "limit": 0 }))).await;
    assert_eq!(
        count.total_count, 1,
        "limit 0 counted the Shared-flagged card"
    );

    // Without linkStatus: my private note on a shared card.
    for limit in [json!(null), json!(10)] {
        let query = json!({ "where": { "agent": "secret" }, "limit": limit });
        let got = run(&store, query.clone()).await;
        assert_eq!(ids(&got), vec!["sg://g/s"], "{query}");
        assert_eq!(
            got.instances[0]["agent"],
            json!("secret"),
            "{query}: the note"
        );
        assert_eq!(got.total_count, 1, "{query}: totalCount");
    }
}
