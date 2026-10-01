//! #1120 without `linkStatus`: a link whose signature does not verify does not
//! select, count, page or exclude an instance.

use super::super::test_helpers::execute_model_query_from_json;
use super::super::types::{ModelQueryInput, ModelQueryResult};
use super::{add, forged, ids, opted_in, run, signed, SG_SHAPE_JSON};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use serde_json::{json, Value};

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

/// The other selection patterns: a parent scope's link, and a `$` projection's
/// `where` on the counted records' values. A forged link must not put an
/// instance under a parent, or make a target pass a projection's `where`.
#[tokio::test]
async fn selection_a_forged_link_does_not_pass_a_scope_or_a_projection_where() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/2", "ad4m://type", "sg://Grant", 1),
            // Parent scope: g/1 genuinely under the board, g/2 by a forged link.
            signed(&admin, "sg://board", "sg://holds", "sg://g/1", 2),
            forged(&admin, "sg://board", "sg://holds", "sg://g/2", 2),
            // Projection targets: r/1 genuinely named `x`, r/2 by a forged link.
            signed(&admin, "sg://g/1", "sg://reply", "sg://r/1", 3),
            signed(&admin, "sg://g/1", "sg://reply", "sg://r/2", 3),
            signed(&admin, "sg://r/1", "sg://name", "literal:string:x", 4),
            forged(&admin, "sg://r/2", "sg://name", "literal:string:x", 4),
        ],
    );
    let (resolver, _) =
        super::super::test_helpers::StaticShapeResolver::from_json("Grant", SG_SHAPE_JSON).unwrap();
    resolver.register(
        "Reply",
        super::super::shape::parse_shape_from_json(
            r#"{ "className": "Reply", "properties": {
                "name": {"predicate":"sg://name","required":false} }, "relations": {} }"#,
            "Reply",
        )
        .unwrap(),
    );

    let scoped = json!({ "parent": { "id": "sg://board", "predicate": "sg://holds" } });
    for limit in [json!(null), json!(10)] {
        let mut query = scoped.clone();
        query["limit"] = limit;
        let got = run(&store, query.clone()).await;
        assert_eq!(ids(&got), vec!["sg://g/1"], "{query}");
        assert_eq!(got.total_count, 1, "{query}: totalCount");
        let got = run(&store, opted_in(query.clone())).await;
        assert_eq!(
            ids(&got),
            vec!["sg://g/1", "sg://g/2"],
            "{query} with the opt-in"
        );
    }

    let projected = |include_unverified: bool| {
        let (store, resolver) = (&store, &resolver);
        async move {
            let query: ModelQueryInput = serde_json::from_value(json!({
                "where": { "id": "sg://g/1" },
                "includeUnverified": include_unverified,
                "projections": { "$x": {
                    "from": "replies", "count": true,
                    "targetClassName": "Reply", "where": { "name": "x" } } },
            }))
            .unwrap();
            let shape = resolver.get_shape("Grant").unwrap();
            let got = super::super::query::execute_model_query(store, &shape, &query, resolver)
                .await
                .unwrap();
            got.instances[0]["$x"].clone()
        }
    };
    use super::super::types::ShapeResolver;
    assert_eq!(projected(false).await, json!(1), "a forged value passed");
    assert_eq!(projected(true).await, json!(2), "the opt-in counts it");
}

/// A relation quantifier's link, its target's conformance and its nested
/// clause, and a class conformed to only structurally (no flag, no required
/// property), must read verified links only too.
#[tokio::test]
async fn selection_a_forged_link_does_not_pass_a_quantifier_or_structural_conformance() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/2", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/3", "ad4m://type", "sg://Grant", 1),
            // g/1 -> v/1 genuinely, and v/1 is genuinely `approved`.
            signed(&admin, "sg://g/1", "sg://vote", "sg://v/1", 2),
            signed(&admin, "sg://v/1", "ad4m://type", "sg://Vote", 2),
            signed(
                &admin,
                "sg://v/1",
                "sg://verdict",
                "literal:string:approved",
                2,
            ),
            // g/2 -> v/2 genuinely, but v/2's `approved` is forged.
            signed(&admin, "sg://g/2", "sg://vote", "sg://v/2", 3),
            signed(&admin, "sg://v/2", "ad4m://type", "sg://Vote", 3),
            forged(
                &admin,
                "sg://v/2",
                "sg://verdict",
                "literal:string:approved",
                3,
            ),
            // g/3 -> v/3 by a forged link, and v/3 is genuinely `approved`.
            forged(&admin, "sg://g/3", "sg://vote", "sg://v/3", 4),
            signed(&admin, "sg://v/3", "ad4m://type", "sg://Vote", 4),
            signed(
                &admin,
                "sg://v/3",
                "sg://verdict",
                "literal:string:approved",
                4,
            ),
            // g/4 -> v/4 genuinely, and v/4 is genuinely `approved`, but
            // v/4's Vote flag is forged.
            signed(&admin, "sg://g/4", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/4", "sg://vote", "sg://v/4", 6),
            forged(&admin, "sg://v/4", "ad4m://type", "sg://Vote", 6),
            signed(
                &admin,
                "sg://v/4",
                "sg://verdict",
                "literal:string:approved",
                6,
            ),
            // A Note has no flag and no required property. n/1 is one by a
            // signed `text`, n/2 by a forged one only.
            signed(&admin, "sg://n/1", "sg://text", "literal:string:a", 5),
            forged(&admin, "sg://n/2", "sg://text", "literal:string:b", 5),
        ],
    );
    let (resolver, _) = super::super::test_helpers::StaticShapeResolver::from_json(
        "Grant",
        r#"{ "className": "Grant", "properties": {
            "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Grant"} },
            "relations": { "votes": { "predicate": "sg://vote", "kind": "hasMany", "targetClassName": "Vote" } } }"#,
    )
    .unwrap();
    for (class, json) in [
        (
            "Vote",
            r#"{ "className": "Vote", "properties": {
                "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Vote"},
                "verdict": {"predicate":"sg://verdict","required":false} }, "relations": {} }"#,
        ),
        (
            "Note",
            r#"{ "className": "Note", "properties": {
                "text": {"predicate":"sg://text","required":false} }, "relations": {} }"#,
        ),
    ] {
        resolver.register(
            class,
            super::super::shape::parse_shape_from_json(json, class).unwrap(),
        );
    }
    use super::super::types::ShapeResolver;
    let run_class = |class: &'static str, query: Value| {
        let (store, resolver) = (&store, &resolver);
        async move {
            let query: ModelQueryInput = serde_json::from_value(query).unwrap();
            let shape = resolver.get_shape(class).unwrap();
            super::super::query::execute_model_query(store, &shape, &query, resolver)
                .await
                .unwrap()
        }
    };

    let sorted = |r: ModelQueryResult| {
        let mut v = ids(&r);
        v.sort();
        v
    };
    let approved = json!({ "where": { "votes": { "some": { "verdict": "approved" } } } });
    assert_eq!(
        sorted(run_class("Grant", approved.clone()).await),
        vec!["sg://g/1"]
    );
    assert_eq!(
        sorted(run_class("Grant", opted_in(approved)).await),
        vec!["sg://g/1", "sg://g/2", "sg://g/3", "sg://g/4"],
        "with the opt-in"
    );

    // A node matched only by a forged link hydrates no rows, so the leak
    // would show in the count and the page, not in the rows.
    for limit in [json!(0), json!(10)] {
        let query = json!({ "limit": limit });
        let got = run_class("Note", query.clone()).await;
        assert_eq!(
            got.total_count, 1,
            "{query}: a forged link conformed a Note"
        );
        let got = run_class("Note", opted_in(query.clone())).await;
        assert_eq!(got.total_count, 2, "{query} with the opt-in");
    }
    assert_eq!(sorted(run_class("Note", json!({})).await), vec!["sg://n/1"]);
}

/// A typed relation (`@HasMany(() => Vote, { through })`) is filled by the
/// conformance getter the SDK generates, whose triples after the relation's
/// own are the target class's flags. A forged flag must not make a genuinely
/// linked node a target.
#[tokio::test]
async fn selection_a_forged_flag_does_not_make_a_typed_relation_target() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&admin, "sg://g/1", "sg://vote", "sg://v/real", 2),
            signed(&admin, "sg://g/1", "sg://vote", "sg://v/retyped", 2),
            signed(&admin, "sg://v/real", "ad4m://type", "sg://Vote", 3),
            forged(&admin, "sg://v/retyped", "ad4m://type", "sg://Vote", 3),
        ],
    );
    let shape_json = json!({
        "className": "Grant",
        "properties": {
            "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Grant"}
        },
        "relations": {
            "votes": {
                "predicate": "sg://vote",
                "kind": "hasMany",
                "targetClassName": "Vote",
                "getter": "SELECT ?target WHERE { <Base> <sg://vote> ?target . ?target <ad4m://type> <sg://Vote> . }"
            }
        }
    })
    .to_string();
    let votes = |include_unverified: bool| {
        let (store, shape_json) = (&store, &shape_json);
        async move {
            let mut out = Vec::new();
            for limit in [json!(null), json!(10)] {
                let query: ModelQueryInput = serde_json::from_value(
                    json!({ "limit": limit, "includeUnverified": include_unverified }),
                )
                .unwrap();
                let got = execute_model_query_from_json(store, "Grant", &query, shape_json)
                    .await
                    .unwrap();
                let mut v: Vec<String> = got.instances[0]["votes"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| v.as_str().unwrap().to_string())
                    .collect();
                v.sort();
                out.push(v);
            }
            assert_eq!(out[0], out[1], "both plans agree");
            out.remove(0)
        }
    };
    assert_eq!(votes(false).await, vec!["sg://v/real"]);
    assert_eq!(votes(true).await, vec!["sg://v/real", "sg://v/retyped"]);
}

/// The selection guard joins a reifier per triple, so two links that both pass
/// and assert the same triple match it twice. Every consumer deduplicates:
/// the instance comes back once, with the same values and count as the
/// unguarded read, on both plans and in the id phase.
#[tokio::test]
async fn selection_two_passing_links_on_one_triple_do_not_duplicate_an_instance() {
    let store = SparqlStore::new(None).unwrap();
    let admin = TestSigner::generate();
    let bob = TestSigner::generate();
    add(
        &store,
        [
            signed(&admin, "sg://g/1", "ad4m://type", "sg://Grant", 1),
            signed(&bob, "sg://g/1", "ad4m://type", "sg://Grant", 2),
            signed(&admin, "sg://g/1", "sg://name", "literal:string:x", 3),
            signed(&bob, "sg://g/1", "sg://name", "literal:string:x", 4),
            signed(&admin, "sg://g/1", "sg://member", "sg://a/carol", 5),
            signed(&bob, "sg://g/1", "sg://member", "sg://a/carol", 6),
        ],
    );
    for query in [
        json!({ "where": { "name": "x", "members": "sg://a/carol" } }),
        json!({ "where": { "name": "x" }, "limit": 10 }),
        json!({ "where": { "name": "x" }, "limit": 10, "order": { "name": "ASC" } }),
        json!({ "limit": 10, "projections": { "$n": { "from": "members", "count": true } } }),
    ] {
        let guarded = run(&store, query.clone()).await;
        let open = run(&store, opted_in(query.clone())).await;
        assert_eq!(
            guarded.instances.len(),
            1,
            "{query}: {:?}",
            guarded.instances
        );
        assert_eq!(guarded.total_count, 1, "{query}: totalCount");
        assert_eq!(
            guarded.instances, open.instances,
            "{query}: same as unguarded"
        );
    }

    // Hydration folds identical rows, so the instances above cannot show the
    // fan-out. The single plan's rows can: one per link (two each on `type`,
    // `name` and `members`), not one per combination of passing links
    // (2 x 2 x 2 per link without `DISTINCT`).
    let (_, shape) =
        super::super::test_helpers::StaticShapeResolver::from_json("Grant", SG_SHAPE_JSON).unwrap();
    let query: ModelQueryInput =
        serde_json::from_value(json!({ "where": { "name": "x", "members": "sg://a/carol" } }))
            .unwrap();
    let super::super::types::InstanceQueryPlan::Single(sparql) =
        super::super::sparql_builder::build_instance_sparql(&shape, &query, None, None)
    else {
        panic!("expected the single plan");
    };
    let rows: Vec<Value> = serde_json::from_str(&store.query(&sparql).unwrap()).unwrap();
    assert_eq!(rows.len(), 6, "one row per link: {rows:?}");
}
