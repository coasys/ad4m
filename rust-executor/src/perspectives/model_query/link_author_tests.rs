//! `where: { author }` beside property conditions is a per-link condition (#1114).
//!
//! The invariant these tests prove: when a where clause names an `author` and a
//! property, the **link that carries the property value** must have been written
//! by that author. It is not enough that the instance's earliest link was.
//!
//! This is the role-gating case from #1046 §1. A role query
//! `{ agent: <candidate>, author: <admin> }` used to be matched after hydration,
//! against the synthetic `author` field. That field is the author of the
//! instance's *earliest* link. So admin created the role instance, anyone added
//! `agent -> did:mallory`, and the query accepted Mallory as a member admin
//! appointed. That fails open.
//!
//! Each negative case below makes the forged link the **sole** link on its
//! predicate, so no ordering can pass it by luck.

use super::test_helpers::execute_model_query_from_json;
use super::types::{ModelQueryInput, ModelQueryResult};
use super::utils::literal_percent_encode;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};

const ADMIN: &str = "did:key:zAdmin";
const LEAD: &str = "did:key:zLead";
const MALLORY: &str = "did:key:zMallory";
const ALICE: &str = "did:key:zAlice";

/// `agent` as a literal property (a TS `@Property` holding a DID) and
/// `member` as a relation to the DID itself (a TS `@HasOne`/`@HasMany`).
/// A role query can name either one as its `didProperty`.
const REVIEWER_SHAPE_JSON: &str = r#"{
    "className": "Reviewer",
    "properties": {
        "role": { "predicate": "ns://role", "required": true, "flag": true, "initial": "ns://reviewer" },
        "agent": { "predicate": "ns://agent", "required": false, "resolveLanguage": "literal" },
        "note": { "predicate": "ns://note", "required": false, "resolveLanguage": "literal" }
    },
    "relations": {
        "member": { "predicate": "ns://member" }
    }
}"#;

fn link(author: &str, source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    LinkExpression {
        author: author.to_string(),
        timestamp: ts.to_string(),
        data: Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: ExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
        },
        status: Some(LinkStatus::Shared),
    }
}

fn lit(value: &str) -> String {
    format!("literal:string:{}", literal_percent_encode(value))
}

/// Write one role instance: `creator` writes the class flag first (the earliest
/// link, so `creator` is the instance's hydrated `author`), then each
/// `(author, predicate, target)` follows one second apart.
fn role_instance(store: &SparqlStore, id: &str, creator: &str, links: &[(&str, &str, String)]) {
    store
        .add_link(&link(
            creator,
            id,
            "ns://role",
            "ns://reviewer",
            "2026-01-01T00:00:00.000Z",
        ))
        .unwrap();
    for (i, (author, predicate, target)) in links.iter().enumerate() {
        let ts = format!("2026-01-01T00:00:{:02}.000Z", i + 1);
        store
            .add_link(&link(author, id, predicate, target, &ts))
            .unwrap();
    }
}

fn input(query: Value) -> ModelQueryInput {
    serde_json::from_value(query).expect("valid ModelQueryInput")
}

async fn run(store: &SparqlStore, query: Value) -> ModelQueryResult {
    execute_model_query_from_json(store, "Reviewer", &input(query), REVIEWER_SHAPE_JSON)
        .await
        .expect("query should execute")
}

fn ids(result: &ModelQueryResult) -> Vec<String> {
    let mut ids: Vec<String> = result
        .instances
        .iter()
        .filter_map(|i| i["id"].as_str().map(str::to_string))
        .collect();
    ids.sort();
    ids
}

/// Run `where` through every plan that answers it: the single-phase instance
/// query, the paginated two-phase plan, and the COUNT fast path. Each builds its
/// own SPARQL, and a condition enforced by only one of them is still laundered
/// by the others.
async fn ids_on_every_plan(store: &SparqlStore, where_clause: Value) -> Vec<String> {
    let single = run(store, json!({ "where": where_clause })).await;
    let paged = run(store, json!({ "where": where_clause, "limit": 10 })).await;
    let count = run(store, json!({ "where": where_clause, "limit": 0 })).await;
    assert_eq!(
        ids(&single),
        ids(&paged),
        "single-phase and paginated plans disagree on {where_clause}"
    );
    assert_eq!(
        count.total_count,
        ids(&single).len(),
        "COUNT disagrees with the rows for {where_clause}"
    );
    assert_eq!(
        single.total_count,
        ids(&single).len(),
        "totalCount disagrees with the rows for {where_clause}"
    );
    ids(&single)
}

/// #1046 §1, the acceptance case. Admin created the instance, Mallory wrote the
/// `agent` link. `{ agent: mallory, author: admin }` must not match.
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

    // The hydrated instance JSON is unchanged: `author` is still the earliest
    // link's author. Only the condition's meaning changed.
    let all = run(&store, json!({})).await;
    assert_eq!(all.instances.len(), 1);
    assert_eq!(all.instances[0]["author"], ADMIN);
    assert_eq!(all.instances[0]["agent"], MALLORY);
}

/// The same forgery through a relation (`@HasOne`/`@HasMany` to the DID).
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
    assert!(
        ids_on_every_plan(
            &store,
            json!({ "member": [MALLORY, ALICE], "author": ADMIN })
        )
        .await
        .is_empty(),
        "the IN form of a relation condition is scoped the same way"
    );
}

/// The positive side: the condition is about who wrote the property link, not
/// who created the instance. Alice created it and admin appointed Bob, so
/// `{ agent: bob, author: admin }` matches and `author: alice` does not.
#[tokio::test]
async fn author_matches_the_property_link_the_author_wrote() {
    let store = SparqlStore::new(None).unwrap();
    let bob = "did:key:zBob";
    role_instance(
        &store,
        "ns://r/appointed",
        ALICE,
        &[
            (ADMIN, "ns://agent", lit(bob)),
            (ADMIN, "ns://member", bob.to_string()),
        ],
    );

    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": bob, "author": ADMIN })).await,
        vec!["ns://r/appointed"]
    );
    assert_eq!(
        ids_on_every_plan(&store, json!({ "member": bob, "author": ADMIN })).await,
        vec!["ns://r/appointed"]
    );
    assert!(
        ids_on_every_plan(&store, json!({ "agent": bob, "author": ALICE }))
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
        ids_on_every_plan(&store, json!({ "member": ALICE, "author": ADMIN })).await,
        vec!["ns://r/shared"]
    );
    assert!(
        ids_on_every_plan(&store, json!({ "member": MALLORY, "author": ADMIN }))
            .await
            .is_empty()
    );
}

/// The shape the flow translator emits for `or` branches that each name a
/// granter: `{ agent: <did>, OR: [{ author: admin }, { author: lead }] }`.
/// Each branch's `author` scopes the enclosing `agent` condition, the same as
/// if it were written into every branch.
#[tokio::test]
async fn author_in_or_branches_scopes_the_enclosing_property_condition() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );
    role_instance(
        &store,
        "ns://r/lead",
        ADMIN,
        &[(LEAD, "ns://agent", lit(ALICE))],
    );

    let granted_by = json!([{ "author": ADMIN }, { "author": LEAD }]);
    assert!(
        ids_on_every_plan(&store, json!({ "agent": MALLORY, "OR": granted_by }))
            .await
            .is_empty(),
        "neither branch's granter wrote Mallory's agent link"
    );
    assert_eq!(
        ids_on_every_plan(&store, json!({ "agent": ALICE, "OR": granted_by })).await,
        vec!["ns://r/lead"]
    );
}

/// The array (`in`) and `not` forms of the author condition are per-link too.
#[tokio::test]
async fn author_array_and_not_forms_are_per_link() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    assert!(
        ids_on_every_plan(&store, json!({ "agent": MALLORY, "author": [ADMIN, LEAD] }))
            .await
            .is_empty()
    );
    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": MALLORY, "author": { "not": ADMIN } })
        )
        .await,
        vec!["ns://r/forged"],
        "the agent link is not admin's, whoever created the instance"
    );
    assert!(ids_on_every_plan(
        &store,
        json!({ "agent": MALLORY, "author": { "not": [MALLORY, LEAD] } })
    )
    .await
    .is_empty());
}

/// Every property condition in the clause is scoped, not just the first.
#[tokio::test]
async fn author_scopes_every_property_condition_in_the_clause() {
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

/// With no property condition beside it there is no link to scope, so a bare
/// `author` keeps its instance-level meaning: the earliest link's author, the
/// same value the hydrated `author` field shows.
#[tokio::test]
async fn a_bare_author_condition_keeps_its_instance_level_meaning() {
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
}

/// A scoped `author` can only be answered in the store, where the links still
/// carry their own authors. If part of the clause has to be evaluated after
/// hydration, the per-link answer is gone, and the fallback would compare
/// against the earliest link's author, which is the laundering this fixes.
/// Such a query is refused rather than answered wrongly.
#[tokio::test]
async fn a_scoped_author_beside_an_unpushable_condition_is_refused() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/forged",
        ADMIN,
        &[(MALLORY, "ns://agent", lit(MALLORY))],
    );

    for where_clause in [
        // `timestamp` is instance-level metadata, matched after hydration.
        json!({ "agent": MALLORY, "author": ADMIN, "timestamp": "2026-01-01T00:00:00.000Z" }),
        // An OR whose other arm cannot be pushed is evaluated after hydration whole.
        json!({ "agent": MALLORY, "OR": [{ "author": ADMIN }, { "timestamp": { "gt": 0 } }] }),
    ] {
        let err = execute_model_query_from_json(
            &store,
            "Reviewer",
            &input(json!({ "where": where_clause })),
            REVIEWER_SHAPE_JSON,
        )
        .await
        .expect_err("a per-link author the store cannot answer must be refused");
        assert!(
            err.to_string().contains("author"),
            "the refusal should say why: {err}"
        );
    }
}

/// Inside a relation quantifier the nested clause describes the *linked*
/// record, so its `author` scopes the linked record's links. The compiled
/// clause is rebased onto the quantifier's variable. That rebase has to rewrite
/// `?source` inside the reifier's `<<( … )>>` triple term too. Otherwise the
/// join would silently name the outer record's link and match nothing.
#[tokio::test]
async fn author_inside_a_relation_quantifier_scopes_the_linked_records_links() {
    use super::shape::parse_shape_from_json;
    use super::test_helpers::StaticShapeResolver;

    let store = SparqlStore::new(None).unwrap();
    let t0 = "2026-01-01T00:00:00.000Z";
    let t1 = "2026-01-01T00:00:01.000Z";
    for (task, review, verdict_author) in [
        ("ns://t/honest", "ns://rv/honest", ADMIN),
        ("ns://t/forged", "ns://rv/forged", MALLORY),
    ] {
        for l in [
            link(ADMIN, task, "ns://type", "ns://task", t0),
            link(ADMIN, task, "ns://review", review, t0),
            link(ADMIN, review, "ns://type", "ns://review", t0),
            link(verdict_author, review, "ns://verdict", &lit("approved"), t1),
        ] {
            store.add_link(&l).unwrap();
        }
    }

    let task_shape = parse_shape_from_json(
        r#"{
            "className": "Task",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://task" }
            },
            "relations": {
                "reviews": { "predicate": "ns://review", "targetClassName": "Review" }
            }
        }"#,
        "Task",
    )
    .unwrap();
    let review_shape = parse_shape_from_json(
        r#"{
            "className": "Review",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://review" },
                "verdict": { "predicate": "ns://verdict", "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#,
        "Review",
    )
    .unwrap();
    let resolver = StaticShapeResolver::new();
    resolver.register("Task", task_shape.clone());
    resolver.register("Review", review_shape);

    let query = input(json!({
        "where": { "reviews": { "some": { "verdict": "approved", "author": ADMIN } } }
    }));
    let result = super::query::execute_model_query(&store, &task_shape, &query, &resolver)
        .await
        .expect("query should execute");
    assert_eq!(
        ids(&result),
        vec!["ns://t/honest"],
        "only the review whose verdict admin wrote counts"
    );
}
