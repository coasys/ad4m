//! #1116 on the reads that fill relations and projections, rather than the
//! instance's own property rows (`link_status_tests.rs`): a typed relation's
//! generated getter, a reverse relation read through `include`, and the
//! instances a `$` projection hydrates for its target class.
//!
//! Each fixture has the same relation twice over, once through a Shared link
//! and once through a Local one, or has a Local link as the relation's only
//! link, so a read that ignores `linkStatus` returns the Local value.

use super::link_status_tests::{
    ls_link, ls_local_type, ls_seed, unset, LS_REMARK_SHAPE_JSON, LS_SHAPE_JSON,
};
use super::shape::parse_shape_from_json;
use super::test_helpers::StaticShapeResolver;
use super::types::{IncludeValue, ModelQueryInput, ProjectionInput};
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkStatus;
use serde_json::{json, Value};
use std::collections::HashMap;

/// Remark `ls://r/1`, typed by a Shared and a Local link, with a Shared
/// `body` and a Local `aside`. Not yet related to the card.
fn ls_seed_remark(store: &SparqlStore, signer: &crate::agent::signatures::TestSigner) {
    let r = "ls://r/1";
    for l in [
        ls_link(
            signer,
            r,
            "ad4m://type",
            "ls://Remark",
            3,
            LinkStatus::Shared,
        ),
        ls_local_type(signer, r, "ls://Remark", 4),
        ls_link(
            signer,
            r,
            "ls://body",
            "literal:string:shared",
            4,
            LinkStatus::Shared,
        ),
        ls_link(
            signer,
            r,
            "ls://aside",
            "literal:string:local",
            5,
            LinkStatus::Local,
        ),
    ] {
        store.add_link(&l).unwrap();
    }
}

/// The ids in a relation value: a list of ids, or of hydrated instances.
fn ids(v: &Value) -> Vec<String> {
    let mut out: Vec<String> = v
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|e| e.as_str().or_else(|| e["id"].as_str()))
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default();
    out.sort();
    out
}

/// A typed `@HasMany(() => Remark, { through })` ships the conformance getter
/// the SDK generates (`buildConformanceFilter` in `core/src/model/decorators.ts`),
/// and a relation with a getter is filled by `evaluate_getters`, not by the
/// status-filtered hydration read. Here the card's only `comment` link is
/// Local, so a Shared-only read must not list the remark, nor hydrate it
/// through `include`. Both instance-query plans.
///
/// A hand-written getter is the model author's SPARQL and runs as written, the
/// same as for `includeUnverified`. That direction is pinned too.
#[tokio::test]
async fn link_status_applies_to_getter_backed_relations() {
    let store = SparqlStore::new(None).unwrap();
    let signer = ls_seed(&store);
    ls_seed_remark(&store, &signer);
    store
        .add_link(&ls_link(
            &signer,
            "ls://c/1",
            "ls://comment",
            "ls://r/1",
            6,
            LinkStatus::Local,
        ))
        .unwrap();

    // `note` is Local, so the card still has a row to hydrate under Local.
    let shape_with = |getter: &str| {
        json!({
            "className": "Card",
            "properties": {
                "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"ls://Card"},
                "note": {"predicate":"ls://note","required":false}
            },
            "relations": {
                "comments": {
                    "predicate": "ls://comment",
                    "kind": "hasMany",
                    "targetClassName": "Remark",
                    "getter": getter
                }
            }
        })
        .to_string()
    };
    let comments = |shape_json: String, link_status: Option<LinkStatus>, include: bool| {
        let store = &store;
        async move {
            let (resolver, shape) = StaticShapeResolver::from_json("Card", &shape_json).unwrap();
            resolver.register(
                "Remark",
                parse_shape_from_json(LS_REMARK_SHAPE_JSON, "Remark").unwrap(),
            );
            let mut out = Vec::new();
            // `limit` switches to the two-phase plan.
            for limit in [None, Some(10)] {
                let query = ModelQueryInput {
                    limit,
                    link_status: link_status.clone(),
                    include: include.then(|| {
                        HashMap::from([("comments".to_string(), IncludeValue::Bool(true))])
                    }),
                    ..Default::default()
                };
                let result = super::query::execute_model_query(
                    store,
                    shape.as_ref(),
                    &query,
                    &resolver,
                    None,
                )
                .await
                .unwrap();
                assert_eq!(result.instances.len(), 1, "limit {limit:?}");
                out.push(ids(&result.instances[0]["comments"]));
            }
            assert_eq!(out[0], out[1], "both plans agree");
            out.remove(0)
        }
    };

    let generated = shape_with(
        "SELECT ?target WHERE { <Base> <ls://comment> ?target . ?target <ad4m://type> <ls://Remark> . }",
    );
    for include in [false, true] {
        assert_eq!(
            comments(generated.clone(), None, include).await,
            vec!["ls://r/1"],
            "include {include}: both statuses by default"
        );
        assert_eq!(
            comments(generated.clone(), Some(LinkStatus::Local), include).await,
            vec!["ls://r/1"],
            "include {include}"
        );
        assert!(
            comments(generated.clone(), Some(LinkStatus::Shared), include)
                .await
                .is_empty(),
            "include {include}: a Local relation link must not fill a typed relation under Shared"
        );
    }

    let hand_written = shape_with(
        "SELECT ?target WHERE { ?target <ad4m://type> <ls://Remark> . <Base> <ls://comment> ?target . }",
    );
    assert_eq!(
        comments(hand_written, Some(LinkStatus::Shared), false).await,
        vec!["ls://r/1"],
        "the executor does not rewrite a hand-written getter"
    );
}

/// A reverse relation read through `include` runs its own triple query
/// (`resolve_reverse_include`), separate from the plain reverse read. Under
/// Shared, of one Shared and one Local mark, only the Shared mark is hydrated.
#[tokio::test]
async fn link_status_applies_to_a_reverse_include() {
    let store = SparqlStore::new(None).unwrap();
    let signer = ls_seed(&store);
    for (second, mark, status) in [
        (6, "ls://m/shared", LinkStatus::Shared),
        (7, "ls://m/local", LinkStatus::Local),
    ] {
        // Both marks are Shared instances; only the link to the card differs.
        store
            .add_link(&ls_link(
                &signer,
                mark,
                "ad4m://type",
                "ls://Mark",
                second,
                LinkStatus::Shared,
            ))
            .unwrap();
        store
            .add_link(&ls_link(
                &signer,
                mark,
                "ls://marks",
                "ls://c/1",
                second,
                status,
            ))
            .unwrap();
    }
    let card_json = LS_SHAPE_JSON.replace(
        r#""targetClassName": "", "direction": "reverse""#,
        r#""targetClassName": "Mark", "direction": "reverse""#,
    );
    assert_ne!(card_json, LS_SHAPE_JSON, "the fixture names a target class");
    let (resolver, shape) = StaticShapeResolver::from_json("Card", &card_json).unwrap();
    resolver.register(
        "Mark",
        parse_shape_from_json(
            r#"{
                "className": "Mark",
                "properties": {
                    "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"ls://Mark"}
                },
                "relations": {}
            }"#,
            "Mark",
        )
        .unwrap(),
    );
    let marked_by = |link_status: Option<LinkStatus>| {
        let query = ModelQueryInput {
            link_status,
            include: Some(HashMap::from([(
                "markedBy".to_string(),
                IncludeValue::Bool(true),
            )])),
            ..Default::default()
        };
        let store = &store;
        let shape = shape.clone();
        let resolver = &resolver;
        async move {
            let inst =
                super::query::execute_model_query(store, shape.as_ref(), &query, resolver, None)
                    .await
                    .unwrap()
                    .instances[0]
                    .clone();
            let marks = &inst["markedBy"];
            assert!(
                marks
                    .as_array()
                    .is_some_and(|a| a.iter().all(|m| m.is_object())),
                "hydrated by the include: {inst}"
            );
            ids(marks)
        }
    };

    assert_eq!(marked_by(None).await, vec!["ls://m/local", "ls://m/shared"]);
    assert_eq!(
        marked_by(Some(LinkStatus::Shared)).await,
        vec!["ls://m/shared"],
        "the Local mark must not be hydrated under Shared"
    );
}

/// A `$` projection with a target class hydrates each target in a sub-query.
/// That sub-query inherits the caller's `linkStatus`, so the target's Local
/// `aside` is withheld under Shared. The relation link here is Shared, so the
/// target is listed; which targets are listed follows `linkStatus` too
/// (#1120).
#[tokio::test]
async fn link_status_applies_to_a_projection_target() {
    let store = SparqlStore::new(None).unwrap();
    let signer = ls_seed(&store);
    ls_seed_remark(&store, &signer);
    store
        .add_link(&ls_link(
            &signer,
            "ls://c/1",
            "ls://comment",
            "ls://r/1",
            6,
            LinkStatus::Shared,
        ))
        .unwrap();
    let (resolver, shape) = StaticShapeResolver::from_json("Card", LS_SHAPE_JSON).unwrap();
    resolver.register(
        "Remark",
        parse_shape_from_json(LS_REMARK_SHAPE_JSON, "Remark").unwrap(),
    );
    let remark = |link_status: Option<LinkStatus>| {
        let query = ModelQueryInput {
            link_status,
            projections: Some(HashMap::from([(
                "$remark".to_string(),
                ProjectionInput {
                    from: "comments".to_string(),
                    count: false,
                    transitive: false,
                    target_class_name: Some("Remark".to_string()),
                    where_clause: None,
                    limit: Some(1),
                    order: None,
                },
            )])),
            ..Default::default()
        };
        let store = &store;
        let shape = shape.clone();
        let resolver = &resolver;
        async move {
            let inst =
                super::query::execute_model_query(store, shape.as_ref(), &query, resolver, None)
                    .await
                    .unwrap()
                    .instances[0]
                    .clone();
            let remark = inst["$remark"].clone();
            assert_eq!(remark["id"], json!("ls://r/1"), "hydrated: {inst}");
            remark
        }
    };

    let both = remark(None).await;
    assert_eq!(both["aside"], json!("local"), "{both}");

    let shared = remark(Some(LinkStatus::Shared)).await;
    assert_eq!(shared["body"], json!("shared"), "{shared}");
    assert!(
        unset(&shared, "aside"),
        "the projection target's Local property must be withheld under Shared: {shared}"
    );
}
