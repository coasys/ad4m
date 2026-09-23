//! #1113: links whose signature does not verify are withheld by default.
//!
//! These tests use real signatures ([`TestSigner`]) rather than the
//! `key`/`sig` fixtures of `integration_tests.rs`, which never verify and so
//! read through `include_unverified` there.

use super::shape::parse_shape_from_json;
use super::test_helpers::{execute_model_query_from_json, StaticShapeResolver};
use super::types::{ModelQueryInput, OrderDirection, WhereCondition};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression};
use serde_json::json;
use std::collections::{BTreeMap, HashMap};

const PV_SHAPE_JSON: &str = r#"{
    "className": "Recipe",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"pv://Recipe"},
        "name": {"predicate":"pv://name","required":false}
    },
    "relations": {
        "comments": { "predicate": "pv://comment", "kind": "hasMany", "targetClassName": "Comment" },
        "markedBy": { "predicate": "pv://marks", "kind": "belongsToMany", "targetClassName": "", "direction": "reverse" }
    }
}"#;

const PV_COMMENT_SHAPE_JSON: &str = r#"{
    "className": "Comment",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"pv://Comment"},
        "body": {"predicate":"pv://body","required":false}
    },
    "relations": {}
}"#;

fn pv_at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 9, 23, 12, 0, second)
        .unwrap()
}

/// A link signed by `signer`, as the wallet path produces it.
fn pv_signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        pv_at(second),
    ));
    l.status = Some(crate::types::LinkStatus::Shared);
    l
}

/// A link that claims `signer` as its author but whose signature does not
/// cover it: the signer's signature over one target, carried on another. This
/// is what a peer without the key can gossip.
fn pv_forged(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    let mut l = pv_signed(
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

/// Recipe `pv://r/1`, typed and named "real" by a signed link at :01, then a
/// forged later link naming it "forged" at :02. Returns the signer.
fn pv_seed(store: &SparqlStore) -> TestSigner {
    let signer = TestSigner::generate();
    let r = "pv://r/1";
    store
        .add_link(&pv_signed(&signer, r, "ad4m://type", "pv://Recipe", 0))
        .unwrap();
    store
        .add_link(&pv_signed(
            &signer,
            r,
            "pv://name",
            "literal:string:real",
            1,
        ))
        .unwrap();
    store
        .add_link(&pv_forged(
            &signer,
            r,
            "pv://name",
            "literal:string:forged",
            2,
        ))
        .unwrap();
    signer
}

/// #1113: by default a link whose signature does not verify does not hydrate,
/// on both instance-query plans. The forged value is the *later* one, so under
/// last-write-wins it would become the property's value — which it does on
/// `dev`, and does again here with `include_unverified`. The signed value is
/// present both ways.
#[tokio::test]
async fn proof_valid_a_forged_later_value_does_not_hydrate_by_default() {
    let store = SparqlStore::new(None).unwrap();
    pv_seed(&store);

    // `limit` switches to the two-phase plan, whose property query is built
    // separately in `query.rs`.
    for limit in [None, Some(10)] {
        let default = execute_model_query_from_json(
            &store,
            "Recipe",
            &ModelQueryInput {
                limit,
                ..Default::default()
            },
            PV_SHAPE_JSON,
        )
        .await
        .unwrap();
        assert_eq!(default.instances.len(), 1, "limit {limit:?}");
        assert_eq!(
            default.instances[0]["name"],
            json!("real"),
            "limit {limit:?}: the forged later link must not become the value"
        );
        assert_eq!(
            default.instances[0]["updatedAt"],
            json!(pv_at(1).to_rfc3339_opts(chrono::SecondsFormat::Millis, true)),
            "limit {limit:?}: nor move `updatedAt`"
        );

        let opted_in = execute_model_query_from_json(
            &store,
            "Recipe",
            &ModelQueryInput {
                limit,
                include_unverified: Some(true),
                ..Default::default()
            },
            PV_SHAPE_JSON,
        )
        .await
        .unwrap();
        assert_eq!(
            opted_in.instances[0]["name"],
            json!("forged"),
            "limit {limit:?}: the opt-in returns the unverified link"
        );
    }
}

/// #1113 on the other hydration paths: `include` sub-queries inherit the
/// caller's opt-in (an explicit `false` in the sub-query still wins), and a
/// reverse relation (`belongsToMany`) is read under the same rule.
#[tokio::test]
async fn proof_valid_applies_to_includes_and_reverse_relations() {
    let store = SparqlStore::new(None).unwrap();
    let signer = pv_seed(&store);
    let c = "pv://c/1";
    for l in [
        pv_signed(&signer, "pv://r/1", "pv://comment", c, 3),
        pv_signed(&signer, c, "ad4m://type", "pv://Comment", 3),
        pv_signed(&signer, c, "pv://body", "literal:string:real", 4),
        pv_forged(&signer, c, "pv://body", "literal:string:forged", 5),
        // Reverse relation: one verified mark, one forged.
        pv_signed(&signer, "pv://m/good", "pv://marks", "pv://r/1", 6),
        pv_forged(&signer, "pv://m/bad", "pv://marks", "pv://r/1", 7),
    ] {
        store.add_link(&l).unwrap();
    }
    let (resolver, shape) = StaticShapeResolver::from_json("Recipe", PV_SHAPE_JSON).unwrap();
    resolver.register(
        "Comment",
        parse_shape_from_json(PV_COMMENT_SHAPE_JSON, "Comment").unwrap(),
    );
    let run = |top: Option<bool>, sub: Option<bool>| {
        let query = ModelQueryInput {
            include_unverified: top,
            include: Some(HashMap::from([(
                "comments".to_string(),
                super::types::IncludeValue::SubQuery(Box::new(ModelQueryInput {
                    include_unverified: sub,
                    ..Default::default()
                })),
            )])),
            ..Default::default()
        };
        let store = &store;
        let shape = shape.clone();
        let resolver = &resolver;
        async move {
            super::query::execute_model_query(store, shape.as_ref(), &query, resolver)
                .await
                .unwrap()
                .instances[0]
                .clone()
        }
    };

    let default = run(None, None).await;
    assert_eq!(default["comments"][0]["body"], json!("real"), "{default}");
    assert_eq!(default["markedBy"], json!(["pv://m/good"]), "{default}");

    let opted_in = run(Some(true), None).await;
    assert_eq!(
        opted_in["comments"][0]["body"],
        json!("forged"),
        "inherited: {opted_in}"
    );
    let mut marks: Vec<String> = serde_json::from_value(opted_in["markedBy"].clone()).unwrap();
    marks.sort();
    assert_eq!(marks, vec!["pv://m/bad", "pv://m/good"]);

    let overridden = run(Some(true), Some(false)).await;
    assert_eq!(
        overridden["comments"][0]["body"],
        json!("real"),
        "an explicit `false` on the sub-query wins: {overridden}"
    );
}

/// Instance *selection* still matches unverified links — #1120.
///
/// #1113 filters the rows that hydrate an instance, not the patterns that
/// select it. `where: {name: "forged"}` is pushed into SPARQL and matches the
/// bare triple, which the forged link asserts, so the query returns the recipe
/// — hydrated as `name: "real"`, contradicting the condition it was selected
/// by — and a `limit: 0` count reports it. Remove the `#[ignore]` with the fix.
#[tokio::test]
#[ignore = "#1120: where/conformance/count still match unverified links"]
async fn proof_valid_where_does_not_select_on_a_forged_value() {
    let store = SparqlStore::new(None).unwrap();
    pv_seed(&store);
    let by_forged_name = || ModelQueryInput {
        where_clause: Some(BTreeMap::from([(
            "name".to_string(),
            WhereCondition::String("forged".to_string()),
        )])),
        ..Default::default()
    };

    let rows = execute_model_query_from_json(&store, "Recipe", &by_forged_name(), PV_SHAPE_JSON)
        .await
        .unwrap();
    assert!(
        rows.instances.is_empty(),
        "selected by a forged value: {:?}",
        rows.instances
    );

    let count = execute_model_query_from_json(
        &store,
        "Recipe",
        &ModelQueryInput {
            limit: Some(0),
            ..by_forged_name()
        },
        PV_SHAPE_JSON,
    )
    .await
    .unwrap();
    assert_eq!(count.total_count, 0, "counted by a forged value");
}

/// Genuine recipes `a`, `b`, `c` (typed at :10, :11, :12) with every order key
/// the two-phase plan sorts on: `a` is named "banana" and has two comments,
/// `b` is named "cherry" and has one, `c` has neither. Comment bodies sort
/// `a` before `b`.
fn pv_seed_page(store: &SparqlStore, signer: &TestSigner) {
    for l in [
        pv_signed(signer, "pv://r/a", "ad4m://type", "pv://Recipe", 10),
        pv_signed(signer, "pv://r/b", "ad4m://type", "pv://Recipe", 11),
        pv_signed(signer, "pv://r/c", "ad4m://type", "pv://Recipe", 12),
        pv_signed(signer, "pv://r/a", "pv://name", "literal:string:banana", 13),
        pv_signed(signer, "pv://r/b", "pv://name", "literal:string:cherry", 13),
        pv_signed(signer, "pv://r/a", "pv://comment", "pv://c/a1", 14),
        pv_signed(signer, "pv://r/a", "pv://comment", "pv://c/a2", 14),
        pv_signed(signer, "pv://r/b", "pv://comment", "pv://c/b1", 14),
        pv_signed(signer, "pv://c/a1", "pv://body", "literal:string:m1", 15),
        pv_signed(signer, "pv://c/a2", "pv://body", "literal:string:m2", 15),
        pv_signed(signer, "pv://c/b1", "pv://body", "literal:string:n1", 15),
        // Comments nobody attached to a recipe, with bodies that sort first.
        pv_signed(signer, "pv://c/x1", "pv://body", "literal:string:a1", 15),
        pv_signed(signer, "pv://c/x2", "pv://body", "literal:string:a2", 15),
        pv_signed(signer, "pv://c/x3", "pv://body", "literal:string:a3", 15),
    ] {
        store.add_link(&l).unwrap();
    }
}

/// Unverified links on `c` that would move it to the front under every order
/// key: an earlier type link, a name sorting first, three comments (more than
/// `a` has) whose bodies sort first.
fn pv_forge_page(store: &SparqlStore, signer: &TestSigner) {
    for l in [
        pv_forged(signer, "pv://r/c", "ad4m://type", "pv://Recipe", 1),
        pv_forged(signer, "pv://r/c", "pv://name", "literal:string:aaa", 20),
        pv_forged(signer, "pv://r/c", "pv://comment", "pv://c/x1", 20),
        pv_forged(signer, "pv://r/c", "pv://comment", "pv://c/x2", 20),
        pv_forged(signer, "pv://r/c", "pv://comment", "pv://c/x3", 20),
    ] {
        store.add_link(&l).unwrap();
    }
}

async fn pv_page(
    store: &SparqlStore,
    shape: &super::types::ModelShape,
    resolver: &StaticShapeResolver,
    query: ModelQueryInput,
) -> super::types::ModelQueryResult {
    super::query::execute_model_query(store, shape, &query, resolver)
        .await
        .unwrap()
}

/// #1113 in the two-phase plan's pagination subquery: an unverified link on
/// one instance must not change which instances land on a page, or in which
/// order, under any order key the subquery sorts on — the default timestamp,
/// a property, a `$` projection count, and a relation's property. Page 1
/// (`limit: 2`) and page 2 (`offset: 2`) are compared against the same
/// perspective without the unverified links. `totalCount` must agree with the
/// unpaginated row count, and a `$` count shown on the page with the one it
/// was sorted by.
///
/// Each order key is also run with `include_unverified`, which must see the
/// reordering, so the comparison cannot pass because the unverified links
/// never mattered.
#[tokio::test]
async fn proof_valid_unverified_links_do_not_reorder_a_page() {
    let signer = TestSigner::generate();
    let clean = SparqlStore::new(None).unwrap();
    let dirty = SparqlStore::new(None).unwrap();
    pv_seed_page(&clean, &signer);
    pv_seed_page(&dirty, &signer);
    pv_forge_page(&dirty, &signer);

    let (resolver, shape) = StaticShapeResolver::from_json("Recipe", PV_SHAPE_JSON).unwrap();
    resolver.register(
        "Comment",
        parse_shape_from_json(PV_COMMENT_SHAPE_JSON, "Comment").unwrap(),
    );
    let projections = HashMap::from([(
        "$n".to_string(),
        super::types::ProjectionInput {
            from: "comments".to_string(),
            count: true,
            transitive: false,
            target_class_name: None,
            where_clause: None,
            limit: None,
            order: None,
        },
    )]);
    let query = |order: Option<(&str, OrderDirection)>,
                 limit: Option<usize>,
                 offset: Option<usize>,
                 include_unverified: Option<bool>| ModelQueryInput {
        order: order.map(|(k, d)| vec![(k.to_string(), d)]),
        limit,
        offset,
        include_unverified,
        projections: Some(projections.clone()),
        ..Default::default()
    };
    let ids = |r: &super::types::ModelQueryResult| -> Vec<String> {
        r.instances
            .iter()
            .map(|i| i["id"].as_str().unwrap().to_string())
            .collect()
    };
    let counts = |r: &super::types::ModelQueryResult| -> Vec<serde_json::Value> {
        r.instances.iter().map(|i| i["$n"].clone()).collect()
    };

    for order in [
        None,
        Some(("name", OrderDirection::ASC)),
        Some(("$n", OrderDirection::DESC)),
        Some(("comments.body", OrderDirection::ASC)),
    ] {
        let expected_first =
            pv_page(&clean, &shape, &resolver, query(order, Some(2), None, None)).await;
        assert_eq!(
            ids(&expected_first),
            vec!["pv://r/a", "pv://r/b"],
            "order {order:?}: fixture"
        );

        let first = pv_page(&dirty, &shape, &resolver, query(order, Some(2), None, None)).await;
        assert_eq!(
            ids(&first),
            ids(&expected_first),
            "order {order:?}: an unverified link changed page 1"
        );
        assert_eq!(
            counts(&first),
            counts(&expected_first),
            "order {order:?}: `$n` on page 1"
        );
        let second = pv_page(
            &dirty,
            &shape,
            &resolver,
            query(order, Some(2), Some(2), None),
        )
        .await;
        assert_eq!(
            ids(&second),
            vec!["pv://r/c"],
            "order {order:?}: an unverified link changed page 2"
        );

        let all = pv_page(&dirty, &shape, &resolver, query(order, None, None, None)).await;
        assert_eq!(all.instances.len(), 3, "order {order:?}");
        assert_eq!(
            first.total_count,
            all.instances.len(),
            "order {order:?}: totalCount disagrees with the rows"
        );

        let opted_in = pv_page(
            &dirty,
            &shape,
            &resolver,
            query(order, Some(2), None, Some(true)),
        )
        .await;
        assert_eq!(
            ids(&opted_in)[0],
            "pv://r/c",
            "order {order:?}: the opt-in sees the unverified links"
        );
    }
}
