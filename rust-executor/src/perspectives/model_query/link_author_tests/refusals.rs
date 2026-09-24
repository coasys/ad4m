//! What the store cannot answer is an `Err`. After hydration an instance has
//! one `author`, its earliest link's, so a per-link `author` that would reach
//! the post-hydration filter would be answered against the wrong author.

use super::*;

/// A per-link `author`, nested or side by side, beside anything the store
/// cannot evaluate is refused on every plan.
#[tokio::test]
async fn a_per_link_author_beside_an_unpushable_condition_is_refused() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    for where_clause in [
        // `timestamp` is instance metadata, matched after hydration.
        json!({ "agent": { "eq": MALLORY, "author": ADMIN }, "timestamp": T0 }),
        json!({ "agent": MALLORY, "author": ADMIN, "timestamp": T0 }),
        // An OR whose other arm cannot be pushed is evaluated after hydration
        // whole, the per-link arm with it.
        json!({ "OR": [
            { "agent": { "eq": MALLORY, "author": ADMIN } },
            { "timestamp": { "gt": 0 } }
        ] }),
        // A relation takes no `not`/`contains` in the store.
        json!({ "member": { "not": ALICE, "author": ADMIN } }),
    ] {
        let err = refused_on_every_plan(&store, where_clause).await;
        assert!(err.contains("per-link `author`"), "{err}");
    }
}

/// Beside a getter the side-by-side form scopes the link-backed siblings, so
/// the unpushable getter makes it a refusal, not a bare fallback.
#[tokio::test]
async fn side_by_side_beside_a_getter_and_a_link_is_refused() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    let (resolver, shape) = reviewer_with_getter();

    let err = refused_on_every_plan_of(
        &store,
        &shape,
        &resolver,
        json!({ "agent": MALLORY, "computed": { "not": "x" }, "author": ADMIN }),
    )
    .await;
    assert!(err.contains("per-link `author`"), "{err}");
}

/// A per-link `author` inside a quantifier's nested clause, where the nested
/// clause cannot be answered in the store, is refused like the top-level case.
/// The flag is carried up out of the quantifier to the refusal. Without that,
/// the query would return no rows only because the post-hydration filter
/// happens to fail closed on every quantifier. Red when that propagation is
/// removed.
#[tokio::test]
async fn a_per_link_author_inside_an_unpushable_quantifier_is_refused() {
    let (store, task_shape, resolver) = task_review_fixture();
    let nested = |author: bool| {
        let verdict = if author {
            json!({ "eq": "approved", "author": ADMIN })
        } else {
            json!("approved")
        };
        json!({ "reviews": { "some": { "verdict": verdict, "timestamp": "2026-01-01T00:00:01.000Z" } } })
    };

    let err = refused_on_every_plan_of(&store, &task_shape, &resolver, nested(true)).await;
    assert!(err.contains("per-link `author`"), "{err}");

    // The control: without the `author` the same declined quantifier is not
    // refused. It returns no rows, because the post-hydration filter cannot
    // evaluate a quantifier and fails closed.
    assert!(
        ids_on_every_plan_of(&store, &task_shape, &resolver, nested(false))
            .await
            .is_empty()
    );
}

/// `author` nested under a getter property inside a quantifier's nested clause
/// is the malformed-`author` error, the same as at the top level. The nested
/// clause's error is carried up out of the quantifier to the refusal. Without
/// that, nothing is refused, and the query returns no rows only because the
/// post-hydration filter fails closed on every quantifier. Red when that
/// propagation is removed.
#[tokio::test]
async fn a_nested_author_under_a_getter_inside_a_quantifier_is_an_error() {
    use super::super::types::ShapeResolver;

    let (store, task_shape, resolver) = task_review_fixture();
    // The Review shape plus `computed`, a getter-backed property, as in
    // `reviewer_with_getter`.
    let mut review_shape = (*resolver.get_shape("Review").unwrap()).clone();
    let mut computed = review_shape
        .properties
        .iter()
        .find(|p| p.name == "verdict")
        .unwrap()
        .clone();
    computed.name = "computed".to_string();
    computed.predicate = String::new();
    computed.getter = Some("SELECT ?target WHERE { <Base> <ns://verdict> ?target }".to_string());
    review_shape.properties.push(computed);
    resolver.register("Review", review_shape);

    let nested = |computed: Value| json!({ "reviews": { "some": { "computed": computed } } });

    let err = refused_on_every_plan_of(
        &store,
        &task_shape,
        &resolver,
        nested(json!({ "eq": "approved", "author": ADMIN })),
    )
    .await;
    assert!(err.contains("not a property stored as a link"), "{err}");
    assert!(!err.contains("per-link `author`"), "{err}");

    // The control: without the `author` the same clause is not an error.
    ids_on_every_plan_of(&store, &task_shape, &resolver, nested(json!("approved"))).await;
}

/// `author` nested under something with no link to check is malformed.
#[tokio::test]
async fn a_nested_author_without_a_link_is_an_error() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    let (resolver, shape) = reviewer_with_getter();

    for where_clause in [
        json!({ "computed": { "eq": "x", "author": ADMIN } }),
        json!({ "timestamp": { "author": ADMIN } }),
        json!({ "id": { "eq": "ns://r/forged", "author": ADMIN } }),
        json!({ "OR": [{ "timestamp": { "gt": 0, "author": ADMIN } }] }),
    ] {
        let err = refused_on_every_plan_of(&store, &shape, &resolver, where_clause).await;
        assert!(err.contains("not a property stored as a link"), "{err}");
    }
}

/// `eq` beside another value operator, and an author value that is not a DID
/// form, are malformed and say why.
#[tokio::test]
async fn malformed_eq_and_author_values_are_errors() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(&store, "ns://r/a", ADMIN, &[]);

    for (where_clause, reason) in [
        (
            json!({ "agent": { "eq": "x", "not": "y" } }),
            "`eq` cannot be combined",
        ),
        (
            json!({ "agent": { "eq": { "gt": 1 } } }),
            "`eq` takes a value",
        ),
        (
            json!({ "agent": { "author": { "gt": 1 } } }),
            "`author` must be a DID",
        ),
        (
            json!({ "agent": "x", "author": { "gt": 1 } }),
            "`author` must be a DID",
        ),
    ] {
        let err = refused_on_every_plan(&store, where_clause.clone()).await;
        assert!(err.contains(reason), "{where_clause}: {err}");
    }
}
