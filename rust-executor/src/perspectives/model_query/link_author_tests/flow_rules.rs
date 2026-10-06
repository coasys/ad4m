//! Social-DNA role rules, translated by the flow engine
//! (`flow_evaluator::requires_query_input`) and run through the store.
//!
//! A role rule's `author` names who may grant the role, so every link the rule
//! filters on at that level must be one that author wrote: the DID property's
//! link and every other field beside it. A translation table can only mirror
//! the translator; these tests pin what the translated query *matches*.

use super::*;
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_evaluator::requires_query_input;
use crate::perspectives::shacl_parser::ModelQuery;

/// The flow instance a role rule is evaluated for; `$flow.base` is `task`.
fn record(task: &str) -> FlowInstanceRecord {
    FlowInstanceRecord {
        flow_uri: "ns://ReviewFlow".into(),
        instance_uri: "ns://flow/1".into(),
        subject: task.into(),
        current_state: "open".into(),
        created_at: None,
    }
}

/// Translate `rule` for `candidate` on `task`, then run its `where` on every plan.
async fn eligible(store: &SparqlStore, rule: &Value, task: &str, candidate: &str) -> Vec<String> {
    let rule: ModelQuery = serde_json::from_value(rule.clone()).unwrap();
    let translated = requires_query_input(&rule, &record(task), candidate).unwrap();
    ids_on_every_plan(store, translated["where"].clone()).await
}

/// The design doc's reviewer rule
/// (`docs/flow-interpretation-hints-design.md`): admin appoints a reviewer
/// *for a task*. Admin appointed Mallory for T1; Mallory then added her own
/// `forTask -> T2` link to that instance. Admin wrote the `agent` link, but not
/// the `forTask` one, so she is a reviewer of T1 and not of T2.
#[tokio::test]
async fn a_granted_members_own_field_link_does_not_extend_the_grant() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/m",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(MALLORY)),
            (ADMIN, "ns://forTask", lit("T1")),
            (MALLORY, "ns://forTask", lit("T2")),
        ],
    );
    let rule = json!({ "className": "Reviewer", "didProperty": "agent",
                       "where": { "forTask": "$flow.base", "author": ADMIN } });

    assert_eq!(
        eligible(&store, &rule, "T1", MALLORY).await,
        vec!["ns://r/m"],
        "control: admin appointed her for T1"
    );
    assert!(
        eligible(&store, &rule, "T2", MALLORY).await.is_empty(),
        "Mallory's own `forTask -> T2` link must not make her a reviewer of T2"
    );
}

/// The same hole inside an `or` branch: a member whose `agent` link admin
/// wrote adds `rank -> lead` herself. The branch `{ rank: lead, author: admin }`
/// must need admin's `rank` link too, or any member can promote herself.
#[tokio::test]
async fn a_member_cannot_promote_herself_through_an_or_branch() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/self",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(MALLORY)),
            (MALLORY, "ns://rank", lit("lead")),
        ],
    );
    role_instance(
        &store,
        "ns://r/lead",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (ADMIN, "ns://rank", lit("lead")),
        ],
    );
    let plain = json!({ "className": "Reviewer", "didProperty": "agent",
                        "where": { "rank": "lead", "author": ADMIN } });
    let branched = json!({ "className": "Reviewer", "didProperty": "agent",
                           "or": [ { "className": "Reviewer", "where": { "rank": "lead", "author": ADMIN } },
                                   { "className": "Reviewer", "where": { "author": LEAD } } ] });

    for rule in [&plain, &branched] {
        assert!(
            eligible(&store, rule, "T1", MALLORY).await.is_empty(),
            "self-promotion through {rule}"
        );
        assert_eq!(
            eligible(&store, rule, "T1", ALICE).await,
            vec!["ns://r/lead"],
            "control: admin made Alice lead, {rule}"
        );
    }
}

/// The same self-promotion with the `author` one level up: the level's
/// `where` and its `or` are ANDed, so the arms' fields are links the rule
/// filters on too, and an arm without an `author` of its own needs the
/// level's. Admin appointed Mallory, who then wrote `rank -> lead` herself.
#[tokio::test]
async fn a_levels_author_reaches_or_arms_without_their_own() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/self",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(MALLORY)),
            (MALLORY, "ns://rank", lit("lead")),
        ],
    );
    role_instance(
        &store,
        "ns://r/lead",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (ADMIN, "ns://rank", lit("lead")),
        ],
    );
    let rule = json!({ "className": "Reviewer", "didProperty": "agent", "where": { "author": ADMIN },
                       "or": [ { "className": "Reviewer", "where": { "rank": "lead" } },
                               { "className": "Reviewer", "where": { "rank": "senior" } } ] });

    assert!(
        eligible(&store, &rule, "T1", MALLORY).await.is_empty(),
        "Mallory's own `rank -> lead` must not satisfy an arm under admin's `author`"
    );
    assert_eq!(
        eligible(&store, &rule, "T1", ALICE).await,
        vec!["ns://r/lead"],
        "control: admin made Alice lead"
    );
}

/// The granter collapse (`or` arms that only name a granter fold into one
/// author list) must not replace an author the level inherits. Admin's rule
/// has an arm `{ didProperty: agent, where: { rank: lead }, or: [{ author: LEAD }] }`
/// with no `author` of its own, so its `rank` link needs admin, and Lead's
/// sub-arm scopes only the `agent` link. Lead wrote `rank -> lead` for Mallory:
/// that must not satisfy the arm.
#[tokio::test]
async fn an_inherited_author_survives_the_granter_collapse_in_an_arm() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/lead-ranked",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(MALLORY)),
            (LEAD, "ns://agent", lit(MALLORY)),
            (LEAD, "ns://rank", lit("lead")),
        ],
    );
    role_instance(
        &store,
        "ns://r/admin-ranked",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (LEAD, "ns://agent", lit(ALICE)),
            (ADMIN, "ns://rank", lit("lead")),
        ],
    );
    let rule = json!({ "className": "Reviewer", "didProperty": "agent", "where": { "author": ADMIN },
                       "or": [ { "className": "Reviewer", "didProperty": "agent", "where": { "rank": "lead" },
                                 "or": [ { "className": "Reviewer", "where": { "author": LEAD } } ] } ] });

    assert!(
        eligible(&store, &rule, "T1", MALLORY).await.is_empty(),
        "Lead's `rank -> lead` must not stand in for admin's in an arm that inherits admin"
    );
    assert_eq!(
        eligible(&store, &rule, "T1", ALICE).await,
        vec!["ns://r/admin-ranked"],
        "control: admin ranked Alice, Lead wrote her `agent` link"
    );
}

/// A positive `OR` whose arms each carry nested authors, the shape the
/// translator emits for a branched rule. It has to be pushed into SPARQL (a
/// per-link author in a declined clause is refused), so a result at all means
/// the UNION of reifier joins ran. One instance per arm, and one whose links
/// Mallory wrote, which matches neither.
#[tokio::test]
async fn a_pushed_or_with_nested_authors_in_each_arm_matches_per_arm() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/by-admin",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(ALICE)),
            (ADMIN, "ns://rank", lit("lead")),
        ],
    );
    role_instance(
        &store,
        "ns://r/by-lead",
        LEAD,
        &[(LEAD, "ns://agent", lit(ALICE))],
    );
    role_instance(
        &store,
        "ns://r/by-mallory",
        ADMIN,
        &[
            (MALLORY, "ns://agent", lit(ALICE)),
            (MALLORY, "ns://rank", lit("lead")),
        ],
    );
    let expected = vec!["ns://r/by-admin", "ns://r/by-lead"];

    let rule = json!({ "className": "Reviewer", "didProperty": "agent",
                       "or": [ { "className": "Reviewer", "where": { "rank": "lead", "author": ADMIN } },
                               { "className": "Reviewer", "where": { "author": LEAD } } ] });
    assert_eq!(eligible(&store, &rule, "T1", ALICE).await, expected);

    // The same shape written out, so it stays covered whatever the translator emits.
    assert_eq!(
        ids_on_every_plan(
            &store,
            json!({ "agent": ALICE, "OR": [
                { "rank": { "eq": "lead", "author": ADMIN }, "agent": { "eq": ALICE, "author": ADMIN } },
                { "agent": { "eq": ALICE, "author": LEAD } },
            ] })
        )
        .await,
        expected
    );
}

/// Translate `rule` like [`eligible`], but a refused translation is `None`: a
/// rule that does not translate grants nobody.
async fn eligible_unless_refused(
    store: &SparqlStore,
    rule: &Value,
    task: &str,
    candidate: &str,
) -> Option<Vec<String>> {
    let rule: ModelQuery = serde_json::from_value(rule.clone()).unwrap();
    let translated = requires_query_input(&rule, &record(task), candidate).ok()?;
    Some(ids_on_every_plan(store, translated["where"].clone()).await)
}

/// The design doc's reviewer rule with the granters moved into `or` arms that
/// do not all collapse (Lead also needs `rank: senior`). The level has no
/// `author`, so nothing may leave its `forTask` bare while an arm names one:
/// admin appointed Mallory for T1, she wrote `forTask -> T2` herself, and arm 1
/// matches on admin's `agent` link. She must not be a reviewer of T2.
///
/// The same holds one level down (an arm with fields and no `author`, whose own
/// arms name one), and for a wrapper arm with no `where` whose own arm names
/// one. The translator refuses all three. Controls: the collapsing rule
/// (without `rank: senior`), and the non-collapsing rule with `forTask` written
/// into each arm, both of which make Mallory a reviewer of T1 and not of T2.
#[tokio::test]
async fn a_level_without_an_author_does_not_leave_its_fields_bare_beside_author_arms() {
    let store = SparqlStore::new(None).unwrap();
    role_instance(
        &store,
        "ns://r/m",
        ADMIN,
        &[
            (ADMIN, "ns://agent", lit(MALLORY)),
            (ADMIN, "ns://forTask", lit("T1")),
            (MALLORY, "ns://forTask", lit("T2")),
        ],
    );
    let arms = json!([ { "className": "Reviewer", "where": { "author": ADMIN } },
                       { "className": "Reviewer", "where": { "author": LEAD, "rank": "senior" } } ]);
    let level = json!({ "className": "Reviewer", "didProperty": "agent",
                        "where": { "forTask": "$flow.base" }, "or": arms });
    let nested = json!({ "className": "Reviewer", "didProperty": "agent",
                         "or": [ { "className": "Reviewer", "where": { "forTask": "$flow.base" },
                                   "or": [ { "className": "Reviewer", "where": { "author": ADMIN } } ] } ] });
    let wrapped = json!({ "className": "Reviewer", "didProperty": "agent",
                          "where": { "forTask": "$flow.base" },
                          "or": [ { "className": "Reviewer",
                                    "or": [ { "className": "Reviewer", "where": { "author": ADMIN } } ] } ] });
    for rule in [&level, &nested, &wrapped] {
        let t2 = eligible_unless_refused(&store, rule, "T2", MALLORY).await;
        assert!(
            t2.as_ref().is_none_or(|ids| ids.is_empty()),
            "Mallory's own `forTask -> T2` link must not make her a reviewer of T2: {rule} gave {t2:?}"
        );
    }

    let collapsing = json!({ "className": "Reviewer", "didProperty": "agent",
                             "where": { "forTask": "$flow.base" },
                             "or": [ { "className": "Reviewer", "where": { "author": ADMIN } },
                                     { "className": "Reviewer", "where": { "author": LEAD } } ] });
    let distributed = json!({ "className": "Reviewer", "didProperty": "agent",
                              "or": [ { "className": "Reviewer", "where": { "author": ADMIN, "forTask": "$flow.base" } },
                                      { "className": "Reviewer",
                                        "where": { "author": LEAD, "rank": "senior", "forTask": "$flow.base" } } ] });
    for rule in [&collapsing, &distributed] {
        assert_eq!(
            eligible(&store, rule, "T1", MALLORY).await,
            vec!["ns://r/m"],
            "control: admin appointed her for T1, {rule}"
        );
        assert!(
            eligible(&store, rule, "T2", MALLORY).await.is_empty(),
            "control: not for T2, {rule}"
        );
    }
}
