//! Top-level `author`. Beside link-backed conditions in the same object it
//! means both: the instance's author is A, and A wrote the links that satisfy
//! those conditions. With none beside it, it is bare: the instance's author.

use super::*;

/// #1046 §1, the acceptance case, in the form role queries were written in.
/// Admin created the instance, Mallory wrote the `agent` link. Red on `dev`,
/// which matched `author` against the earliest link only.
#[tokio::test]
async fn author_is_not_laundered_through_the_instances_earliest_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert!(
        ids_on_every_plan(&store, json!({ "agent": MALLORY, "author": ADMIN }))
            .await
            .is_empty(),
        "Mallory wrote the agent link herself; admin never appointed her"
    );
}

/// The same forgery through a relation, in its single and array forms.
#[tokio::test]
async fn author_is_not_laundered_through_a_relation_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://member", MALLORY.to_string())],
    );

    assert!(
        ids_on_every_plan(&store, json!({ "member": MALLORY, "author": ADMIN }))
            .await
            .is_empty()
    );
    assert!(ids_on_every_plan(
        &store,
        json!({ "member": [MALLORY, ALICE], "author": ADMIN })
    )
    .await
    .is_empty());
}

/// Both halves must hold. On a single-author instance that is `dev`'s answer.
/// Where the creator and the appointer differ, neither author satisfies both,
/// which is the behaviour change: `dev` matched the creator, the per-link
/// reading alone would match the appointer.
#[tokio::test]
async fn side_by_side_requires_the_instance_author_and_the_link_author() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/single",
        ADMIN,
        &[(ADMIN, "ns://agent", lit(BOB))],
    );
    role_instance(
        &store,
        "ns://r/split",
        ALICE,
        &[(ADMIN, "ns://agent", lit(BOB))],
    );

    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": BOB, "author": ADMIN })).await,
        vec!["ns://r/single"],
        "on ns://r/split admin wrote the link but did not create the instance"
    );
    assert!(
        ids_on_every_plan(&store, json!({ "agent": BOB, "author": ALICE }))
            .await
            .is_empty(),
        "Alice created ns://r/split but did not write its agent link"
    );
    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": { "eq": BOB, "author": ADMIN } })).await,
        vec!["ns://r/single", "ns://r/split"],
        "the nested form asks only about the link"
    );
}

/// Every link-backed sibling in the object is scoped, not just the first.
#[tokio::test]
async fn side_by_side_scopes_every_link_backed_sibling() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/mixed",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (MALLORY, "ns://note", lit("trusted")),
        ],
    );

    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": ALICE, "author": ADMIN })).await,
        vec!["ns://r/mixed"]
    );
    assert!(
        ids_on_every_plan(
            &store,
            json!({ "agent": ALICE, "note": "trusted", "author": ADMIN })
        )
        .await
        .is_empty(),
        "admin did not write the note"
    );
}

/// `author: { not: A }` side by side negates both halves: the instance's
/// author is not A, and the link was written by someone other than A.
#[tokio::test]
async fn side_by_side_not_negates_both_halves() {
    let store = SparqlStore::new(None).unwrap();
    // Admin created it, Mallory wrote the link.
    role_instance(
        &store,
        "ns://r/admin-made",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    // The lead created it, Mallory wrote the link.
    role_instance(
        &store,
        "ns://r/lead-made",
        LEAD,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    // The lead created it, admin wrote the link.
    role_instance(
        &store,
        "ns://r/admin-link",
        LEAD,
        &[(ADMIN, "ns://agent", lit(MALLORY))],
    );

    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": MALLORY, "author": { "not": ADMIN } })
        )
        .await,
        vec!["ns://r/lead-made"]
    );
}

/// When several links share the earliest timestamp, hydration shows whichever
/// it met first. The pushed instance-level half requires all of them to meet
/// the condition, so a tie can only exclude.
#[tokio::test]
async fn a_tie_for_the_earliest_link_fails_closed() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/tied",
        ADMIN,
        &[(ADMIN, "ns://agent", lit(BOB))],
    );
    store
        .add_link(&link(
            MALLORY,
            "ns://r/tied",
            "ns://note",
            &lit("me too"),
            T0,
        ))
        .unwrap();

    assert!(
        ids_on_every_plan(&store, json!({ "agent": BOB, "author": ADMIN }))
            .await
            .is_empty()
    );
    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": BOB, "author": { "not": LEAD } })).await,
        vec!["ns://r/tied"],
        "neither tied author is the lead"
    );
}

/// A `none` quantifier sibling is not scoped. "A wrote no member link" would
/// be wider than "no member link", and side by side must only narrow.
#[tokio::test]
async fn side_by_side_does_not_scope_a_none_quantifier() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://member", MALLORY.to_string())],
    );

    assert!(
        ids_on_every_plan(&store, json!({ "member": { "none": {} }, "author": ADMIN }))
            .await
            .is_empty()
    );
}

/// With no link-backed condition beside it, `author` is bare: the instance's
/// author, the earliest link's, the value the instance JSON shows. Mute lists
/// (`{ not: [...] }`) are bare too.
#[tokio::test]
async fn a_bare_author_keeps_its_instance_level_meaning() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert_eq!(
        ids_on_every_plan(&store, json!({ "author": ADMIN })).await,
        vec!["ns://r/forged"]
    );
    assert!(ids_on_every_plan(&store, json!({ "author": MALLORY }))
        .await
        .is_empty());
    assert_eq!(
        ids_on_every_plan(&store, json!({ "author": { "not": [MALLORY, LEAD] } })).await,
        vec!["ns://r/forged"],
        "Mallory wrote a link, but the instance is admin's"
    );
}

/// A getter property has no link, so beside only a getter `author` is bare.
#[tokio::test]
async fn author_beside_only_a_getter_property_is_bare() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    let (resolver, shape) = reviewer_with_getter();

    // `not` on a value no instance has, so the getter condition holds for
    // every instance and the `author` alone decides.
    let getter = json!({ "not": "no-such-value" });
    assert_eq!(
        ids_on_every_plan_of(
            &store,
            &shape,
            &resolver,
            json!({ "computed": getter, "author": ADMIN })
        )
        .await,
        vec!["ns://r/forged"],
        "admin wrote the earliest link"
    );
    assert!(
        ids_on_every_plan_of(
            &store,
            &shape,
            &resolver,
            json!({ "computed": getter, "author": MALLORY })
        )
        .await
        .is_empty(),
        "Mallory wrote a link on the instance, but not its earliest one"
    );
}

/// `timestamp` is instance metadata, not a link. Beside it alone, `author` is
/// bare, the same as the getter case.
#[tokio::test]
async fn author_beside_only_a_timestamp_is_bare() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert_eq!(
        ids_on_every_plan(&store, json!({ "timestamp": T0, "author": ADMIN })).await,
        vec!["ns://r/forged"]
    );
    assert!(
        ids_on_every_plan(&store, json!({ "timestamp": T0, "author": MALLORY }))
            .await
            .is_empty(),
        "Mallory wrote a link on the instance, but not its earliest one"
    );
}
