//! Nested `author`: `{ agent: { eq: X, author: A } }` holds when A wrote an
//! `agent -> X` link. The author and the value condition hold on one link.

use super::*;

/// #1046 §1 in the nested form. Admin created the instance, Mallory wrote the
/// `agent` link. Admin never wrote `agent -> mallory`.
#[tokio::test]
async fn a_nested_author_is_not_laundered_through_the_earliest_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert!(ids_on_every_plan(
        &store,
        json!({ "agent": { "eq": MALLORY, "author": ADMIN } })
    )
    .await
    .is_empty());
    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": { "eq": MALLORY, "author": MALLORY } })
        )
        .await,
        vec!["ns://r/forged"],
        "Mallory did write it"
    );

    // The instance JSON is unchanged: `author` is still the earliest link's.
    let all = run(&store, json!({})).await;
    assert_eq!(all.instances.len(), 1);
    assert_eq!(all.instances[0]["author"], ADMIN);
    assert_eq!(all.instances[0]["agent"], MALLORY);
}

/// The same forgery through a relation (`@HasOne`/`@HasMany` to the DID),
/// including the any-of array form of `eq`.
#[tokio::test]
async fn a_nested_author_is_not_laundered_through_a_relation_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://member", MALLORY.to_string())],
    );

    for value in [json!(MALLORY), json!([MALLORY, ALICE])] {
        assert!(ids_on_every_plan(
            &store,
            json!({ "member": { "eq": value, "author": ADMIN } })
        )
        .await
        .is_empty());
    }
}

/// The positive side: who wrote the property link counts, not who created the
/// instance. Alice created it, admin appointed Bob.
#[tokio::test]
async fn a_nested_author_matches_the_link_the_author_wrote() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/appointed",
        ALICE,
        &[
            (ADMIN, "ns://agent", lit(BOB)),
            (ADMIN, "ns://member", BOB.to_string()),
        ],
    );

    for where_clause in [
        json!({ "agent": { "eq": BOB, "author": ADMIN } }),
        json!({ "member": { "eq": BOB, "author": ADMIN } }),
        json!({ "member": { "eq": [BOB, MALLORY], "author": ADMIN } }),
        json!({ "agent": { "eq": BOB, "author": [LEAD, ADMIN] } }),
    ] {
        assert_eq!(
            ids_on_every_plan(&store, where_clause.clone()).await,
            vec!["ns://r/appointed"],
            "{where_clause}"
        );
    }
    assert!(
        ids_on_every_plan(&store, json!({ "agent": { "eq": BOB, "author": ALICE } }))
            .await
            .is_empty(),
        "Alice created the instance but did not write the agent link"
    );
}

/// Several links on one predicate: the one that satisfies the value condition
/// is the one whose author counts. Admin appointed Alice; Mallory added
/// herself later on the same predicate.
#[tokio::test]
async fn the_author_of_the_matching_value_counts_not_any_link_on_the_predicate() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/shared",
        ADMIN,
        &[
            (ADMIN, "ns://member", ALICE.to_string()),
            (MALLORY, "ns://member", MALLORY.to_string()),
        ],
    );

    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "member": { "eq": ALICE, "author": ADMIN } })
        )
        .await,
        vec!["ns://r/shared"]
    );
    assert!(ids_on_every_plan(
        &store,
        json!({ "member": { "eq": MALLORY, "author": ADMIN } })
    )
    .await
    .is_empty());
}

/// `{ agent: { author: A } }` with no value operator: A wrote some `agent`
/// link, whatever its value. The same on a relation.
#[tokio::test]
async fn author_alone_means_some_link_on_the_property_by_that_author() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[
            (MALLORY, "ns://agent", lit(MALLORY)),
            (MALLORY, "ns://member", MALLORY.to_string()),
        ],
    );

    for prop in ["agent", "member"] {
        assert!(
            ids_on_every_plan(&store, json!({ prop: { "author": ADMIN } }))
                .await
                .is_empty(),
            "{prop}: admin wrote no {prop} link"
        );
        assert_eq!(
            ids_on_every_plan(&store, json!({ prop: { "author": MALLORY } })).await,
            vec!["ns://r/forged"],
            "{prop}"
        );
    }
}

/// Every author value form, nested: a DID array (any of), `{ not }` with one
/// DID or several, and `{ contains }`.
#[tokio::test]
async fn every_author_value_form_is_per_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    let matches = |author: Value| json!({ "agent": { "eq": MALLORY, "author": author } });
    for (author, expected) in [
        (json!([ADMIN, LEAD]), false),
        (json!([ADMIN, MALLORY]), true),
        (json!({ "not": ADMIN }), true),
        (json!({ "not": [MALLORY, LEAD] }), false),
        (json!({ "contains": "mallory" }), true),
        (json!({ "contains": "admin" }), false),
    ] {
        let got = ids_on_every_plan(&store, matches(author.clone())).await;
        assert_eq!(!got.is_empty(), expected, "author: {author}");
    }
}

/// `author` beside another value operator: the author and the operator must
/// hold on the same link. Admin wrote "hello", Mallory wrote "trusted".
#[tokio::test]
async fn author_and_a_value_operator_hold_on_the_same_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/notes",
        ADMIN,
        &[
            (ADMIN, "ns://note", lit("hello")),
            (MALLORY, "ns://note", lit("trusted")),
        ],
    );

    for (where_clause, expected) in [
        // Only Mallory's note contains "trust".
        (
            json!({ "note": { "contains": "trust", "author": ADMIN } }),
            false,
        ),
        (
            json!({ "note": { "contains": "trust", "author": MALLORY } }),
            true,
        ),
        // Admin's only note is "hello".
        (
            json!({ "note": { "not": "hello", "author": ADMIN } }),
            false,
        ),
        (
            json!({ "note": { "not": "hello", "author": MALLORY } }),
            true,
        ),
        (
            json!({ "note": { "eq": ["trusted", "x"], "author": ADMIN } }),
            false,
        ),
    ] {
        let got = ids_on_every_plan(&store, where_clause.clone()).await;
        assert_eq!(!got.is_empty(), expected, "{where_clause}");
    }
}

/// `eq` alone is the bare value, on properties, relations and `id`.
#[tokio::test]
async fn eq_alone_is_the_bare_value() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/a",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (ADMIN, "ns://member", ALICE.to_string()),
        ],
    );
    role_instance(
        &store,
        "ns://r/b",
        ADMIN,
        &[(ADMIN, "ns://agent", lit(BOB))],
    );

    for (bare, eq) in [
        (
            json!({ "agent": ALICE }),
            json!({ "agent": { "eq": ALICE } }),
        ),
        (
            json!({ "agent": [ALICE, LEAD] }),
            json!({ "agent": { "eq": [ALICE, LEAD] } }),
        ),
        (
            json!({ "member": ALICE }),
            json!({ "member": { "eq": ALICE } }),
        ),
        (
            json!({ "id": "ns://r/b" }),
            json!({ "id": { "eq": "ns://r/b" } }),
        ),
    ] {
        let expected = ids_on_every_plan(&store, bare.clone()).await;
        assert_eq!(expected.len(), 1, "{bare}");
        assert_eq!(
            ids_on_every_plan(&store, eq.clone()).await,
            expected,
            "{eq}"
        );
    }
}

/// The two negations mean different things, and both are per link.
/// `NOT: { agent: { eq: X, author: A } }`: A wrote no `agent -> X` link.
/// `agent: { eq: X, author: { not: A } }`: someone other than A wrote one.
/// `ns://r/both` has admin's and the lead's link, so it fails the first and
/// passes the second.
#[tokio::test]
async fn not_around_a_nested_author_and_not_inside_it_differ() {
    let store = SparqlStore::new(None).unwrap();
    agent_written_twice(&store);

    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": ALICE, "NOT": { "agent": { "eq": ALICE, "author": ADMIN } } })
        )
        .await,
        vec!["ns://r/lead"]
    );
    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": { "eq": ALICE, "author": { "not": ADMIN } } })
        )
        .await,
        vec!["ns://r/both", "ns://r/lead"]
    );
}

/// A top-level `author` does not reach into a sub-clause, and a sub-clause's
/// does not reach out. `{ agent: X, OR: [{ author: A }] }` is a bare `agent`
/// and an instance-level `author`: the forged instance matches it, while the
/// nested form, which the flow translator now emits, does not.
#[tokio::test]
async fn a_sub_clause_author_does_not_reach_out_of_its_object() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": MALLORY, "OR": [{ "author": ADMIN }, { "author": LEAD }] })
        )
        .await,
        vec!["ns://r/forged"],
        "the OR's authors are bare: admin created the instance"
    );
    assert!(ids_on_every_plan(
        &store,
        json!({ "agent": { "eq": MALLORY, "author": [ADMIN, LEAD] } })
    )
    .await
    .is_empty());
}

/// And the other direction: a side-by-side `author` scopes its own object's
/// siblings, not the conditions inside an `AND` beside them. Admin created the
/// instance and wrote `agent`; Mallory wrote the note.
#[tokio::test]
async fn a_side_by_side_author_does_not_reach_into_a_sub_clause() {
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

    assert!(
        ids_on_every_plan(
            &store,
            json!({ "agent": ALICE, "note": "trusted", "author": ADMIN })
        )
        .await
        .is_empty(),
        "side by side, the note is scoped too, and admin did not write it"
    );
    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": ALICE, "author": ADMIN, "AND": [{ "note": "trusted" }] })
        )
        .await,
        vec!["ns://r/mixed"],
        "inside the AND the note is its own object's, unscoped"
    );
}

/// A bare `author` inside `NOT` still means the instance's author. `NOT` with
/// only operator-named keys deserialises as an operator object, and must be
/// read back as the clause it is.
#[tokio::test]
async fn not_around_a_bare_author_is_still_instance_level() {
    let store = SparqlStore::new(None).unwrap();
    agent_written_twice(&store);
    role_instance(&store, "ns://r/lead-made", LEAD, &[]);

    assert_eq!(
        ids_on_every_plan(&store, json!({ "NOT": { "author": ADMIN } })).await,
        vec!["ns://r/lead-made"]
    );
}

/// Relation quantifiers and a nested `author`. Inside the quantifier's
/// clause, `verdict: { eq, author }` scopes the linked review's own link. The
/// clause is rebased onto the quantifier's variable, and that rebase has to
/// rewrite `?source` inside the reifier's `<<( … )>>` too.
#[tokio::test]
async fn a_nested_author_inside_a_quantifier_scopes_the_linked_records_link() {
    let (store, task_shape, resolver) = task_review_fixture();

    assert_eq!(
        ids_on_every_plan_of(
            &store,
            &task_shape,
            &resolver,
            json!({ "reviews": { "some": { "verdict": { "eq": "approved", "author": ADMIN } } } })
        )
        .await,
        vec!["ns://t/honest"]
    );
    // Side by side in the nested clause: the review's author (admin, for both)
    // and the verdict link's author.
    assert_eq!(
        ids_on_every_plan_of(
            &store,
            &task_shape,
            &resolver,
            json!({ "reviews": { "some": { "verdict": "approved", "author": ADMIN } } })
        )
        .await,
        vec!["ns://t/honest"]
    );
}

/// `author` beside a quantifier scopes the relation link itself: `some` is
/// "A wrote a link to a record satisfying the clause", `none` is "A wrote no
/// such link". Admin wrote both `reviews` links.
#[tokio::test]
async fn a_nested_author_beside_a_quantifier_scopes_the_relation_link() {
    let (store, task_shape, resolver) = task_review_fixture();
    let both = vec!["ns://t/forged", "ns://t/honest"];

    for (where_clause, expected) in [
        (
            json!({ "reviews": { "some": { "verdict": "approved" }, "author": ADMIN } }),
            both.clone(),
        ),
        (
            json!({ "reviews": { "some": { "verdict": "approved" }, "author": MALLORY } }),
            vec![],
        ),
        (
            json!({ "reviews": { "none": {}, "author": MALLORY } }),
            both.clone(),
        ),
        (
            json!({ "reviews": { "none": {}, "author": ADMIN } }),
            vec![],
        ),
        (json!({ "reviews": { "author": ADMIN } }), both.clone()),
    ] {
        assert_eq!(
            ids_on_every_plan_of(&store, &task_shape, &resolver, where_clause.clone()).await,
            expected,
            "{where_clause}"
        );
    }
}
