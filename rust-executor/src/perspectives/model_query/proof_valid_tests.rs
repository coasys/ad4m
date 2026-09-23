//! #1113: links whose signature does not verify are withheld by default.
//!
//! These tests use real signatures ([`TestSigner`]) rather than the
//! `key`/`sig` fixtures of `integration_tests.rs`, which never verify and so
//! read through `include_unverified` there.

use super::shape::parse_shape_from_json;
use super::test_helpers::{execute_model_query_from_json, StaticShapeResolver};
use super::types::{ModelQueryInput, WhereCondition};
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
