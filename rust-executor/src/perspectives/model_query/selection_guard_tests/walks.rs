//! #1120 on transitive paths: a property path has no link per hop to check, so
//! a guarded walk is done one step at a time (`query::guarded_reach`).

use super::super::types::ModelQueryResult;
use super::{add, forged, ids, opted_in, run, signed};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use serde_json::json;

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

/// A transitive `parent` scope (`Traverse` with `transitive: true`) walks a
/// property path, which has no link per hop to check. The executor walks it
/// one guarded step at a time instead, so a forged link in the chain must not
/// bring in what lies behind it, in either direction, as rows or as a count.
///
/// `root -> a` and `b -> c` are genuine, `a -> b` is forged. `c` has an id
/// that cannot be written as `<…>` (a legacy Flux form), which takes the
/// string-matching fallback.
#[tokio::test]
async fn selection_a_forged_link_does_not_extend_a_transitive_scope() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    let c = "literal://string:legacyc";
    add(
        &store,
        [
            signed(&admin, "sg://g/a", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/b", "ad4m://type", "sg://Grant", 2),
            signed(&admin, c, "ad4m://type", "sg://Grant", 3),
            signed(&admin, "sg://root", "sg://reply", "sg://g/a", 4),
            forged(&admin, "sg://g/a", "sg://reply", "sg://g/b", 5),
            signed(&admin, "sg://g/b", "sg://reply", c, 6),
        ],
    );
    let walk = |anchor: &str, direction: &str| {
        json!({ "parent": {
            "ids": [anchor], "predicate": "sg://reply",
            "transitive": true, "direction": direction } })
    };
    let sorted = |r: &ModelQueryResult| {
        let mut v = ids(r);
        v.sort();
        v
    };

    for (anchor, direction, expected, expected_opted_in) in [
        (
            "sg://root",
            "out",
            vec!["sg://g/a"],
            vec!["literal://string:legacyc", "sg://g/a", "sg://g/b"],
        ),
        (c, "in", vec!["sg://g/b"], vec!["sg://g/a", "sg://g/b"]),
    ] {
        for limit in [json!(null), json!(10)] {
            let mut query = walk(anchor, direction);
            query["limit"] = limit;
            let got = run(&store, query.clone()).await;
            assert_eq!(sorted(&got), expected, "{query}");
            assert_eq!(got.total_count, expected.len(), "{query}: totalCount");
            let got = run(&store, opted_in(query.clone())).await;
            assert_eq!(sorted(&got), expected_opted_in, "{query} with the opt-in");
        }
        let mut count = walk(anchor, direction);
        count["limit"] = json!(0);
        assert_eq!(
            run(&store, count.clone()).await.total_count,
            expected.len(),
            "{count}"
        );
    }
}

/// The cost of the guard on selection. `test_perf_large_dataset_paginated_query`
/// reads through the opt-in, because its fixtures do not verify, so it never
/// runs the guard. This is its query over signed links: 3 channels of 1000
/// messages, one channel's page of 50 by timestamp, a `where` on the channel
/// and a count. Timed with the default (every selection triple guarded) and
/// with `includeUnverified` (the pre-#1120 selection SPARQL), on one store.
#[tokio::test]
async fn test_perf_guarded_selection_paginated_query() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    for ch in 0..3u32 {
        let channel = format!("sg://channel-{ch}");
        for i in 0..1000u32 {
            let msg = format!("sg://msg-{ch}-{i}");
            let second = (i % 50) + 1;
            add(
                &store,
                [
                    signed(&admin, &msg, "ad4m://type", "sg://Grant", second),
                    signed(&admin, &msg, "sg://member", &channel, second),
                    signed(
                        &admin,
                        &msg,
                        "sg://name",
                        &format!("literal:string:m{i}"),
                        second,
                    ),
                ],
            );
        }
    }
    let by_time = json!({
        "where": { "members": "sg://channel-1" },
        "limit": 50,
        "order": { "timestamp": "DESC" },
    });
    let mut by_name = by_time.clone();
    by_name["order"] = json!({ "name": "ASC" });
    let mut timings = Vec::new();
    for q in [
        by_time.clone(),
        opted_in(by_time.clone()),
        by_name.clone(),
        opted_in(by_name.clone()),
    ] {
        let start = std::time::Instant::now();
        let got = run(&store, q.clone()).await;
        let elapsed = start.elapsed();
        assert_eq!(got.instances.len(), 50, "{q}");
        assert_eq!(got.total_count, 1000, "{q}");
        timings.push(elapsed);
    }
    // The same two queries read as an agent: the guard then also checks that
    // each link is not another agent's Local link (#1024).
    let viewer = TestSigner::generate();
    for q in [by_time, by_name] {
        let query: super::super::types::ModelQueryInput =
            serde_json::from_value(q.clone()).unwrap();
        let start = std::time::Instant::now();
        let got = super::super::test_helpers::execute_model_query_from_json_for_viewer(
            &store,
            "Grant",
            &query,
            super::SG_SHAPE_JSON,
            Some(&viewer.did),
        )
        .await
        .unwrap();
        let elapsed = start.elapsed();
        assert_eq!(got.instances.len(), 50, "{q} as a viewer");
        assert_eq!(got.total_count, 1000, "{q} as a viewer");
        timings.push(elapsed);
    }
    eprintln!(
        "guarded selection over 3000 signed instances (1000 matching), page of 50: \
         by timestamp default {:?} / includeUnverified {:?} / as a viewer {:?}, \
         by name default {:?} / includeUnverified {:?} / as a viewer {:?}",
        timings[0], timings[1], timings[4], timings[2], timings[3], timings[5]
    );
    assert!(timings[0].as_secs() < 5, "{:?}", timings[0]);
    assert!(timings[2].as_secs() < 5, "{:?}", timings[2]);
    assert!(timings[4].as_secs() < 5, "{:?}", timings[4]);
    assert!(timings[5].as_secs() < 5, "{:?}", timings[5]);
}
