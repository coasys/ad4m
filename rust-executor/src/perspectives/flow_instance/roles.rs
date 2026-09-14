//! Who may vote: resolving a rule's `fromRole` gate to a set of DIDs.
//!
//! This is the only step of a state read that touches the store, which is
//! why it happens in the loader and not in [`fold`](super::fold). It fails
//! closed in both directions: a store error aborts the read rather than
//! mis-counting a vote, and a role query that cannot tell one DID from
//! another is an error rather than "everybody passes".
//!
//! **Roles are re-derived live, against the current graph.** There is no
//! honest way to snapshot them on today's platform — a snapshot written at
//! vote time is a link any member could forge, which is exactly the
//! authority this engine removes, and `model_query` has no as-of filter. The
//! consequence, pinned by a test: revoking someone's role later can un-settle
//! an edge their vote once settled. What makes that survivable is that the
//! verdicts land in the [`ReadSet`](super::ReadSet) as [`RoleGrant`]s, each
//! naming the rows it relied on — so a token minted from a flow records
//! which role rows its verdict rested on, rather than merely asserting the
//! voter was eligible.

use super::atom::{TransitionAtom, Vote};
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, requires_query_input, run_query, RequiresQueryable,
};
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery};
use serde::{Deserialize, Serialize};

/// One `fromRole` membership verdict, with the role rows that produced it.
///
/// Part of the read-set, so a verdict is auditable after the fact: "Bob
/// counted toward `approved` because these rows said he was a Reviewer".
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrant {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    pub eligible: bool,
    /// IDs of the rows the role query matched for this DID.
    pub rows: Vec<String>,
}

/// Ask a `fromRole` gate about each candidate and record what it answered.
///
/// Both role-query shapes collapse into one per-candidate membership check,
/// because the fold only ever intersects the role set with the DIDs that
/// actually voted:
///
/// - a `$did`-templated query substitutes the candidate's DID directly;
/// - a `didProperty` query becomes `where.<prop> = candidate`, the
///   membership form of "extract the DIDs from the role rows".
///
/// A query that references the DID in neither way cannot discriminate
/// between candidates, and "I cannot determine membership" must never
/// degrade to "everyone is a member" — so it is an `Err`, as is any store or
/// translation failure. The caller then abandons the read.
pub async fn resolve_role_grants<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    to_state: &str,
    role: &ModelQuery,
    record: &FlowInstanceRecord,
    candidates: &[String],
) -> anyhow::Result<Vec<RoleGrant>> {
    // `unwrap_or(false)` routes a serde failure into the same `Err` below —
    // "couldn't even inspect the rule" is the purest can't-determine case.
    let did_dependent = role.did_property.is_some()
        || serde_json::to_string(role)
            .map(|s| s.contains("$did"))
            .unwrap_or(false);
    if !did_dependent {
        anyhow::bail!(
            "resolve_role_grants: fromRole query on `{}` references neither `didProperty` nor `$did` — it cannot discriminate between DIDs, so membership is undeterminable; refusing to gate (fail-closed)",
            role.class_name
        );
    }

    let mut grants = Vec::with_capacity(candidates.len());
    for did in candidates {
        let input = requires_query_input(role, record, did)?;
        let matched = run_query(perspective, &role.class_name, &input).await?;
        grants.push(RoleGrant {
            to_state: to_state.to_string(),
            role_class: role.class_name.clone(),
            did: did.clone(),
            eligible: cardinality_satisfied(role.count.as_ref(), matched.len()),
            rows: matched.into_iter().map(|m| m.id).collect(),
        });
    }
    Ok(grants)
}

/// The votes on `atom` that its rule admits — pure, so the fold's inputs can
/// be rebuilt from a serialised read-set. A rule without a `fromRole` admits
/// every vote; with one, a vote counts only when a grant for that atom's
/// target state says its voter is eligible.
pub fn eligible_votes(
    atom: &TransitionAtom,
    rule: &ConsensusRule,
    grants: &[RoleGrant],
) -> Vec<Vote> {
    if rule.from_role.is_none() {
        return atom.votes.clone();
    }
    atom.votes
        .iter()
        .filter(|vote| {
            grants
                .iter()
                .any(|g| g.eligible && g.to_state == atom.to_state && g.did == vote.did)
        })
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use serde_json::{json, Value};
    use std::sync::Mutex;

    fn record() -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: "delivery://DeliveryFlow".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            subject: "ad4m://task/onboarding".into(),
            current_state: "review".into(),
            created_at: None,
        }
    }

    fn role(v: Value) -> ModelQuery {
        serde_json::from_value(v).expect("role query deserializes")
    }

    fn dids(names: &[&str]) -> Vec<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    fn eligible_of(grants: &[RoleGrant]) -> Vec<&str> {
        grants
            .iter()
            .filter(|g| g.eligible)
            .map(|g| g.did.as_str())
            .collect()
    }

    /// Query-aware stub: a call whose JSON mentions one of `member_dids`
    /// returns `rows_per_match` instances; `unconditional_rows` (for
    /// DID-independent queries) wins over matching when set; `error` fails
    /// every call.
    #[derive(Default)]
    struct RoleStub {
        member_dids: Vec<String>,
        rows_per_match: usize,
        unconditional_rows: Option<usize>,
        error: Option<String>,
        calls: Mutex<Vec<String>>,
    }

    #[async_trait]
    impl RequiresQueryable for RoleStub {
        async fn model_query(&self, _class: &str, query_json: &str) -> anyhow::Result<String> {
            self.calls.lock().unwrap().push(query_json.to_string());
            if let Some(msg) = &self.error {
                return Err(anyhow::anyhow!(msg.clone()));
            }
            let n = self.unconditional_rows.unwrap_or_else(|| {
                if self
                    .member_dids
                    .iter()
                    .any(|d| query_json.contains(d.as_str()))
                {
                    self.rows_per_match
                } else {
                    0
                }
            });
            let rows: Vec<Value> = (0..n).map(|i| json!({ "id": format!("r{i}") })).collect();
            Ok(json!({ "instances": rows, "totalCount": n }).to_string())
        }
    }

    /// `(name, role query, member DIDs, candidates, expected eligible set,
    /// expected query count)`.
    #[tokio::test]
    #[rustfmt::skip]
    async fn both_role_query_shapes_resolve_per_candidate() {
        let cases: Vec<(&str, Value, &[&str], &[&str], &[&str], Option<usize>)> = vec![
            ("shape 1: didProperty filters candidates",
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
             &["did:key:alice"], &["did:key:alice", "did:key:bob"], &["did:key:alice"], Some(2)),
            ("shape 2: $did token substitutes per candidate",
             json!({ "className": "ns://Member", "where": { "member": "$did" } }),
             &["did:key:bob"], &["did:key:alice", "did:key:bob"], &["did:key:bob"], None),
            ("one role row does not satisfy count.min = 2",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "count": { "min": 2 } }),
             &["did:key:alice"], &["did:key:alice"], &[], None),
        ];

        for (name, role_json, member_dids, candidates, expected, expect_calls) in cases {
            let stub = RoleStub {
                member_dids: dids(member_dids), rows_per_match: 1, ..Default::default()
            };
            let grants =
                resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(candidates))
                    .await
                    .unwrap();
            assert_eq!(eligible_of(&grants), expected.to_vec(), "{name}");
            assert_eq!(grants.len(), candidates.len(), "{name}: one verdict per candidate");
            for g in &grants {
                assert_eq!(g.to_state, "approved", "{name}: verdicts are per target state");
                if g.eligible {
                    assert!(!g.rows.is_empty(), "{name}: an eligible verdict names its rows");
                }
            }

            if let Some(n) = expect_calls {
                let calls = stub.calls.lock().unwrap();
                assert_eq!(calls.len(), n, "{name}: one membership query per candidate");
                for (call, candidate) in calls.iter().zip(candidates) {
                    assert!(call.contains(candidate),
                            "{name}: each query carries its candidate's DID: {calls:?}");
                }
            }
        }
    }

    /// All three failure modes fail CLOSED — error out, never degrade to
    /// "everyone passes". A `fromRole` is a security rule, and a
    /// misconfigured security rule admits nobody.
    #[tokio::test]
    #[rustfmt::skip]
    async fn every_role_failure_is_an_error_not_allow_all() {
        let cases: Vec<(&str, RoleStub, Value, &str)> = vec![
            ("a query that cannot discriminate between DIDs",
             RoleStub { unconditional_rows: Some(1), ..Default::default() },
             json!({ "className": "ns://Quorum", "where": { "open": true } }), "cannot discriminate"),
            ("a store error",
             RoleStub { error: Some("store down".into()), ..Default::default() },
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }), "store down"),
            ("a role query model_query cannot express",
             RoleStub::default(),
             json!({ "className": "ns://Reviewer", "didProperty": "agent",
                     "where": { "status": { "matches": ".*" } } }), ""),
        ];

        for (name, stub, role_json, expect_contains) in cases {
            let err = resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(&["did:key:alice"]))
                .await
                .expect_err(name);
            assert!(err.to_string().contains(expect_contains), "{name}: got {err:#}");
        }
        // And the undeterminable rule never even runs a query.
        let stub = RoleStub { unconditional_rows: Some(1), ..Default::default() };
        let _ = resolve_role_grants(
            &stub, "approved",
            &role(json!({ "className": "ns://Quorum", "where": { "open": true } })),
            &record(), &dids(&["did:key:alice"]),
        ).await;
        assert!(stub.calls.lock().unwrap().is_empty(), "no query may run for an undeterminable rule");
    }

    #[test]
    fn a_rule_without_a_role_admits_every_vote_and_one_with_a_role_admits_only_grantees() {
        let atom = TransitionAtom {
            uri: "p1".into(),
            from_state: "review".into(),
            to_state: "approved".into(),
            proposer: "did:key:alice".into(),
            proposed_at: "t1".into(),
            evidence_hash: "seal".into(),
            votes: vec![
                Vote {
                    did: "did:key:alice".into(),
                    at: "t1".into(),
                },
                Vote {
                    did: "did:key:bob".into(),
                    at: "t2".into(),
                },
            ],
        };
        let open = ConsensusRule {
            n: 1,
            from_role: None,
        };
        assert_eq!(eligible_votes(&atom, &open, &[]).len(), 2);

        let gated = ConsensusRule {
            n: 1,
            from_role: Some(role(
                json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
            )),
        };
        assert!(
            eligible_votes(&atom, &gated, &[]).is_empty(),
            "no grant, no vote — an unresolved gate admits nobody"
        );
        let grants = vec![
            RoleGrant {
                to_state: "approved".into(),
                role_class: "ns://Reviewer".into(),
                did: "did:key:bob".into(),
                eligible: true,
                rows: vec!["r0".into()],
            },
            RoleGrant {
                to_state: "approved".into(),
                role_class: "ns://Reviewer".into(),
                did: "did:key:alice".into(),
                eligible: false,
                rows: vec![],
            },
            RoleGrant {
                to_state: "shipped".into(),
                role_class: "ns://Reviewer".into(),
                did: "did:key:alice".into(),
                eligible: true,
                rows: vec!["r1".into()],
            },
        ];
        assert_eq!(
            eligible_votes(&atom, &gated, &grants)
                .into_iter()
                .map(|v| v.did)
                .collect::<Vec<_>>(),
            vec!["did:key:bob"],
            "a grant for another target state is not a grant for this one"
        );
    }
}
