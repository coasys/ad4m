//! #1116: `linkStatus` reads an instance from links of one status only, for
//! every predicate.
//!
//! #1028's `local_status_filter` constrains only predicates a class declares
//! `local: true`. Here the class declares nothing local. `note` is an ordinary
//! property that happens to have been written Local, which is the case a
//! multi-user read (#1024) must not leak to another user.
//!
//! The links carry real signatures ([`TestSigner`]), so these tests do not
//! depend on whether the store's signature verdict is honoured by the read.

use super::shape::parse_shape_from_json;
use super::test_helpers::{execute_model_query_from_json, StaticShapeResolver};
use super::types::{IncludeValue, ModelQueryInput, WhereCondition};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap};

const LS_SHAPE_JSON: &str = r#"{
    "className": "Card",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"ls://Card"},
        "title": {"predicate":"ls://title","required":false},
        "note": {"predicate":"ls://note","required":false}
    },
    "relations": {
        "comments": { "predicate": "ls://comment", "kind": "hasMany", "targetClassName": "Remark" },
        "markedBy": { "predicate": "ls://marks", "kind": "belongsToMany", "targetClassName": "", "direction": "reverse" }
    }
}"#;

const LS_REMARK_SHAPE_JSON: &str = r#"{
    "className": "Remark",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"ls://Remark"},
        "body": {"predicate":"ls://body","required":false},
        "aside": {"predicate":"ls://aside","required":false}
    },
    "relations": {}
}"#;

fn ls_at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 9, 24, 12, 0, second)
        .unwrap()
}

fn ls_link(
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
        ls_at(second),
    ));
    l.status = Some(status);
    l
}

/// Card `ls://c/1`: its type and `title` are Shared, its `note` is Local.
fn ls_seed(store: &SparqlStore) -> TestSigner {
    let signer = TestSigner::generate();
    let c = "ls://c/1";
    for l in [
        ls_link(
            &signer,
            c,
            "ad4m://type",
            "ls://Card",
            0,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            c,
            "ls://title",
            "literal:string:shared",
            1,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            c,
            "ls://note",
            "literal:string:local",
            2,
            LinkStatus::Local,
        ),
    ] {
        store.add_link(&l).unwrap();
    }
    signer
}

/// `true` when `key` did not hydrate: absent or null.
fn unset(instance: &Value, key: &str) -> bool {
    instance.get(key).map_or(true, Value::is_null)
}

/// #1116's "done when": an instance with one Local and one Shared property
/// link, where the Shared-only read returns only the Shared one. Also
/// `linkStatus: local` (the converse) and the default (both), on both
/// instance-query plans.
#[tokio::test]
async fn link_status_shared_reads_only_the_shared_property() {
    let store = SparqlStore::new(None).unwrap();
    ls_seed(&store);

    // `limit` switches to the two-phase plan, whose property query is built
    // separately in `query.rs`.
    for limit in [None, Some(10)] {
        let read = |link_status: Option<LinkStatus>| {
            let store = &store;
            async move {
                let result = execute_model_query_from_json(
                    store,
                    "Card",
                    &ModelQueryInput {
                        limit,
                        link_status,
                        ..Default::default()
                    },
                    LS_SHAPE_JSON,
                )
                .await
                .unwrap();
                assert_eq!(result.instances.len(), 1, "limit {limit:?}");
                result.instances[0].clone()
            }
        };

        let both = read(None).await;
        assert_eq!(both["title"], json!("shared"), "limit {limit:?}: {both}");
        assert_eq!(both["note"], json!("local"), "limit {limit:?}: {both}");

        let shared = read(Some(LinkStatus::Shared)).await;
        assert_eq!(
            shared["title"],
            json!("shared"),
            "limit {limit:?}: {shared}"
        );
        assert!(
            unset(&shared, "note"),
            "limit {limit:?}: a Shared-only read must not return the Local note: {shared}"
        );
        assert_eq!(
            shared["updatedAt"],
            json!(ls_at(1).to_rfc3339_opts(chrono::SecondsFormat::Millis, true)),
            "limit {limit:?}: nor let it move `updatedAt`"
        );

        let local = read(Some(LinkStatus::Local)).await;
        assert_eq!(local["note"], json!("local"), "limit {limit:?}: {local}");
        assert!(unset(&local, "title"), "limit {limit:?}: {local}");
    }
}

/// The same rule on the other hydration paths: an `include` sub-query inherits
/// the caller's `linkStatus` (its own setting wins), and a reverse relation
/// (`belongsToMany`) reads only links of that status.
#[tokio::test]
async fn link_status_applies_to_includes_and_reverse_relations() {
    let store = SparqlStore::new(None).unwrap();
    let signer = ls_seed(&store);
    let r = "ls://r/1";
    for l in [
        ls_link(
            &signer,
            "ls://c/1",
            "ls://comment",
            r,
            3,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            r,
            "ad4m://type",
            "ls://Remark",
            3,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            r,
            "ls://body",
            "literal:string:shared",
            4,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            r,
            "ls://aside",
            "literal:string:local",
            5,
            LinkStatus::Local,
        ),
        // Reverse relation: one Shared mark, one Local.
        ls_link(
            &signer,
            "ls://m/shared",
            "ls://marks",
            "ls://c/1",
            6,
            LinkStatus::Shared,
        ),
        ls_link(
            &signer,
            "ls://m/local",
            "ls://marks",
            "ls://c/1",
            7,
            LinkStatus::Local,
        ),
    ] {
        store.add_link(&l).unwrap();
    }
    let (resolver, shape) = StaticShapeResolver::from_json("Card", LS_SHAPE_JSON).unwrap();
    resolver.register(
        "Remark",
        parse_shape_from_json(LS_REMARK_SHAPE_JSON, "Remark").unwrap(),
    );
    let run = |top: Option<LinkStatus>, sub: Option<LinkStatus>| {
        let query = ModelQueryInput {
            link_status: top,
            include: Some(HashMap::from([(
                "comments".to_string(),
                IncludeValue::SubQuery(Box::new(ModelQueryInput {
                    link_status: sub,
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

    let marks = |v: &Value| -> Vec<String> {
        let mut m: Vec<String> = serde_json::from_value(v["markedBy"].clone()).unwrap();
        m.sort();
        m
    };

    let both = run(None, None).await;
    assert_eq!(both["comments"][0]["aside"], json!("local"), "{both}");
    assert_eq!(marks(&both), vec!["ls://m/local", "ls://m/shared"]);

    let shared = run(Some(LinkStatus::Shared), None).await;
    assert_eq!(shared["comments"][0]["body"], json!("shared"), "{shared}");
    assert!(
        unset(&shared["comments"][0], "aside"),
        "inherited by the include: {shared}"
    );
    assert_eq!(marks(&shared), vec!["ls://m/shared"], "{shared}");

    let overridden = run(Some(LinkStatus::Shared), Some(LinkStatus::Local)).await;
    assert_eq!(
        overridden["comments"][0]["aside"],
        json!("local"),
        "the sub-query's own linkStatus wins: {overridden}"
    );
}

/// The `__links` rows (#1117's `links` option) are read by their own query
/// after hydration, so they need the same restriction. Otherwise a Shared-only
/// read that asks for `links: ["note"]` returns the Local note link there,
/// while `note` itself is withheld. Both plans, and an absolute IRI entry as
/// well as a property name.
#[tokio::test]
async fn link_status_restricts_links_rows() {
    let store = SparqlStore::new(None).unwrap();
    ls_seed(&store);
    let targets = |inst: &Value, key: &str| -> Vec<String> {
        inst["__links"][key]
            .as_array()
            .unwrap_or_else(|| panic!("`__links.{key}` missing: {inst}"))
            .iter()
            .map(|r| r["data"]["target"].as_str().unwrap().to_string())
            .collect()
    };
    for limit in [None, Some(10)] {
        let read = |link_status: Option<LinkStatus>| {
            let store = &store;
            async move {
                execute_model_query_from_json(
                    store,
                    "Card",
                    &ModelQueryInput {
                        limit,
                        link_status,
                        links: Some(vec!["title".to_string(), "ls://note".to_string()]),
                        ..Default::default()
                    },
                    LS_SHAPE_JSON,
                )
                .await
                .unwrap()
                .instances[0]
                    .clone()
            }
        };

        let both = read(None).await;
        assert_eq!(targets(&both, "title"), vec!["literal:string:shared"]);
        assert_eq!(targets(&both, "ls://note"), vec!["literal:string:local"]);

        let shared = read(Some(LinkStatus::Shared)).await;
        assert_eq!(targets(&shared, "title"), vec!["literal:string:shared"]);
        assert!(
            targets(&shared, "ls://note").is_empty(),
            "limit {limit:?}: a Shared-only read must not return the Local note link: {shared}"
        );

        let local = read(Some(LinkStatus::Local)).await;
        assert!(targets(&local, "title").is_empty(), "{local}");
        assert_eq!(targets(&local, "ls://note"), vec!["literal:string:local"]);
    }
}

/// Instance *selection* is not restricted by `linkStatus` — #1120.
///
/// `where: {note: "local"}` is pushed into SPARQL and matches the bare triple,
/// which the Local link asserts. A Shared-only read therefore returns the card,
/// with `note` withheld, so a condition on a Local value reveals which
/// instances carry it. A `limit: 0` count reports it too. Remove the `#[ignore]`
/// with the #1120 fix.
#[tokio::test]
#[ignore = "#1120: where/conformance/count are not restricted by linkStatus"]
async fn link_status_shared_does_not_select_on_a_local_value() {
    let store = SparqlStore::new(None).unwrap();
    ls_seed(&store);
    let by_local_note = || ModelQueryInput {
        where_clause: Some(BTreeMap::from([(
            "note".to_string(),
            WhereCondition::String("local".to_string()),
        )])),
        link_status: Some(LinkStatus::Shared),
        ..Default::default()
    };

    let rows = execute_model_query_from_json(&store, "Card", &by_local_note(), LS_SHAPE_JSON)
        .await
        .unwrap();
    assert!(
        rows.instances.is_empty(),
        "selected by a Local value: {:?}",
        rows.instances
    );

    let count = execute_model_query_from_json(
        &store,
        "Card",
        &ModelQueryInput {
            limit: Some(0),
            ..by_local_note()
        },
        LS_SHAPE_JSON,
    )
    .await
    .unwrap();
    assert_eq!(count.total_count, 0, "counted by a Local value");
}

/// Merge guard for #1123 (the default invalid-proof filter), which adds its own
/// `FILTER EXISTS` to the reverse-relation reads next to this PR's.
///
/// Two `FILTER EXISTS` clauses over two reifier variables mean "a Shared link
/// exists and a verified link exists", not "one link is both". Here the mark
/// has a valid Local link and a forged Shared link on the same triple, so
/// together they would pass a Shared (and, after #1123, verified) read, and a
/// Local value would reach the Shared-only caller.
///
/// On this branch alone the forged Shared link is read because nothing checks
/// signatures yet, so this test is red by design. Whichever of #1123 / #1116
/// lands second folds the two clauses into one `FILTER EXISTS` over a single
/// reifier and removes the `#[ignore]`.
#[tokio::test]
#[ignore = "needs #1123: fold the proofValid and status FILTER EXISTS into one reifier"]
async fn link_status_shared_does_not_combine_a_local_link_with_a_forged_shared_one() {
    let store = SparqlStore::new(None).unwrap();
    let signer = ls_seed(&store);
    let valid_local = ls_link(
        &signer,
        "ls://m/secret",
        "ls://marks",
        "ls://c/1",
        8,
        LinkStatus::Local,
    );
    // The same triple, Shared, carrying a signature over a different target.
    let mut forged_shared = ls_link(
        &signer,
        "ls://m/secret",
        "ls://marks",
        "ls://c/other",
        9,
        LinkStatus::Shared,
    );
    forged_shared.data.target = "ls://c/1".to_string();
    assert!(
        !forged_shared.compute_proof_valid(),
        "the fixture must not verify"
    );
    store.add_link(&valid_local).unwrap();
    store.add_link(&forged_shared).unwrap();

    let result = execute_model_query_from_json(
        &store,
        "Card",
        &ModelQueryInput {
            link_status: Some(LinkStatus::Shared),
            ..Default::default()
        },
        LS_SHAPE_JSON,
    )
    .await
    .unwrap();
    let marks = &result.instances[0]["markedBy"];
    assert!(
        marks.is_null() || marks == &json!([]),
        "no single link is both Shared and verified: {}",
        result.instances[0]
    );
}
