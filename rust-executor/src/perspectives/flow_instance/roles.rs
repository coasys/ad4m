//! Who may vote: resolving a rule's `fromRole` gate to a set of DIDs,
//! evaluated *as of each vote's own timestamp*.
//!
//! ## Tombstone revocation
//!
//! Role revocation is an explicit signed link (`ad4m://flow/role_grant_revoked`)
//! on the grant row, never a deletion. This keeps the full membership history
//! in the replicated graph so every replica — including ones that join after a
//! revocation — derives the same flow state.
//!
//! ## As-of vote-time gating
//!
//! A vote counts only when, at the moment the voter signed it:
//!
//! 1. A matching grant row existed (`granted_at <= vote.at`), and
//! 2. No revocation tombstone existed (`revoked_at > vote.at` for all tombstones).
//!
//! This makes eligibility a deterministic function of graph *content*, so two
//! replicas holding the same links derive the same state regardless of when
//! they first folded. Settlement is permanent once reached: a later revocation
//! cannot un-settle an edge whose votes were all cast before it.
//!
//! ## Backdating caveat (documented, not engineered around)
//!
//! Link timestamps are author-asserted, so an admin could backdate a
//! revocation to retroactively un-settle an edge. This is an escalation of
//! *timing*, not of *authority* — the admin already controls role membership,
//! and a backdated revocation achieves nothing a genuinely earlier revocation
//! would not have. As flows become the source of roles (recursive composition),
//! the residual shrinks to the admin-authored base case. Worth documenting; not
//! worth engineering around in v1.
//!
//! ## Fallback (backward compatibility)
//!
//! When the perspective does not return timestamp info for a grant (the
//! default for test stubs and legacy code paths), [`eligible_votes`] falls back
//! to the pre-computed `eligible: bool` on the [`RoleGrant`] — the behaviour
//! that existed before tombstone revocation.

use super::atom::{TransitionAtom, Vote};
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, requires_query_input, run_query, RequiresQueryable, RoleGrantTimestamps,
};
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery};
use serde::{Deserialize, Serialize};

/// Temporal validity of one matched role row for a specific DID.
///
/// Serialisable so the full history rides in the read-set and a minted token's
/// backing is a resolvable audit trail — nothing referenced is ever deleted.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrantWindow {
    /// The row that matched the role query for this DID.
    pub row_id: String,
    /// Timestamp of the earliest grant link for this row/DID pair.
    /// `None` means "unknown"; treated as −∞ (grant pre-dates everything).
    pub granted_at: Option<String>,
    /// Timestamps of every revocation tombstone on this row for this DID.
    pub revoked_at: Vec<String>,
}

impl RoleGrantWindow {
    /// Whether this grant window was open at `vote_at`.
    pub fn open_at(&self, vote_at: &str) -> bool {
        // Grant must have existed before the vote (or timestamp is unknown).
        let grant_ok = self.granted_at.as_deref().map_or(true, |g| g <= vote_at);
        // No revocation must have preceded the vote.
        let not_revoked = self.revoked_at.iter().all(|r| r.as_str() > vote_at);
        grant_ok && not_revoked
    }
}

/// One `fromRole` membership verdict, with the role rows that produced it.
///
/// Part of the read-set, so a verdict is auditable after the fact: "Bob
/// counted toward `approved` because these rows said he was a Reviewer".
/// Under tombstone revocation the rows are never deleted, so every ID in
/// `rows` (and every `window` entry) remains resolvable in the graph.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrant {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    /// Current eligibility (live-graph view: does a non-revoked grant exist
    /// right now?). Used by [`eligible_votes`] only when `windows` is empty,
    /// which preserves pre-tombstone behaviour for stubs and legacy callers.
    pub eligible: bool,
    /// IDs of the rows the role query matched for this DID.
    pub rows: Vec<String>,
    /// As-of eligibility windows, one per matched row. Empty when the
    /// perspective does not return timestamp info (default for test stubs).
    #[serde(default)]
    pub windows: Vec<RoleGrantWindow>,
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
///
/// For each matched row, [`RequiresQueryable::role_grant_timestamps`] is
/// called to populate the [`RoleGrantWindow`]s used by the as-of eligibility
/// check in [`eligible_votes`]. The timestamps are loaded once per read here
/// and never queried again.
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

    let grant_predicate = role.did_property.as_deref();
    let mut grants = Vec::with_capacity(candidates.len());
    for did in candidates {
        let input = requires_query_input(role, record, did)?;
        let matched = run_query(perspective, &role.class_name, &input).await?;
        let eligible = cardinality_satisfied(role.count.as_ref(), matched.len());

        // Load as-of windows for each matched row. A failure here means we
        // cannot determine temporal eligibility, but we still have the live
        // `eligible` flag — propagate the error so the caller can decide
        // whether to abort or accept the fallback.
        let mut windows = Vec::with_capacity(matched.len());
        for item in &matched {
            let RoleGrantTimestamps {
                granted_at,
                revoked_at,
            } = perspective
                .role_grant_timestamps(&item.id, grant_predicate, did)
                .await?;
            windows.push(RoleGrantWindow {
                row_id: item.id.clone(),
                granted_at,
                revoked_at,
            });
        }

        grants.push(RoleGrant {
            to_state: to_state.to_string(),
            role_class: role.class_name.clone(),
            did: did.clone(),
            eligible,
            rows: matched.into_iter().map(|m| m.id).collect(),
            windows,
        });
    }
    Ok(grants)
}

/// The votes on `atom` that its rule admits — pure, so the fold's inputs can
/// be rebuilt from a serialised read-set.
///
/// A rule without a `fromRole` admits every vote. With one:
///
/// - If the relevant [`RoleGrant`] has non-empty `windows` (tombstone path):
///   a vote counts only when at least one of the DID's grant windows was open
///   at `vote.at` — i.e. the grant pre-dated the vote AND no revocation had
///   occurred yet at that time.
/// - If `windows` is empty (fallback / backward-compat path): a vote counts
///   when the grant's `eligible` flag is `true` (live-graph semantics, same as
///   the behaviour before tombstone revocation was introduced).
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
            grants.iter().any(|g| {
                if g.to_state != atom.to_state || g.did != vote.did {
                    return false;
                }
                if g.windows.is_empty() {
                    // Backward-compat: no temporal info, use current eligibility.
                    g.eligible
                } else {
                    // As-of gating: at least one window open at vote time.
                    g.windows.iter().any(|w| w.open_at(&vote.at))
                }
            })
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
        /// Per-call timestamp overrides indexed by DID (for as-of tests).
        timestamps: std::collections::HashMap<String, RoleGrantTimestamps>,
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

        async fn role_grant_timestamps(
            &self,
            _row_id: &str,
            _grant_predicate: Option<&str>,
            did: &str,
        ) -> anyhow::Result<RoleGrantTimestamps> {
            Ok(self.timestamps.get(did).cloned().unwrap_or_default())
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
                windows: vec![],
            },
            RoleGrant {
                to_state: "approved".into(),
                role_class: "ns://Reviewer".into(),
                did: "did:key:alice".into(),
                eligible: false,
                rows: vec![],
                windows: vec![],
            },
            RoleGrant {
                to_state: "shipped".into(),
                role_class: "ns://Reviewer".into(),
                did: "did:key:alice".into(),
                eligible: true,
                rows: vec!["r1".into()],
                windows: vec![],
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

    // -------------------------------------------------------------------------
    // As-of gating (tombstone path)
    // -------------------------------------------------------------------------

    fn atom_with_votes(votes: &[(&str, &str)]) -> TransitionAtom {
        let votes: Vec<Vote> = votes
            .iter()
            .map(|(did, at)| Vote {
                did: did.to_string(),
                at: at.to_string(),
            })
            .collect();
        TransitionAtom {
            uri: "p1".into(),
            from_state: "review".into(),
            to_state: "approved".into(),
            proposer: votes.first().map(|v| v.did.clone()).unwrap_or_default(),
            proposed_at: votes.first().map(|v| v.at.clone()).unwrap_or_default(),
            evidence_hash: "seal".into(),
            votes: votes.clone(),
        }
    }

    fn gated_rule() -> ConsensusRule {
        ConsensusRule {
            n: 1,
            from_role: Some(role(
                json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
            )),
        }
    }

    fn grant_with_window(did: &str, granted_at: Option<&str>, revoked_at: Vec<&str>) -> RoleGrant {
        let revoked_at = revoked_at.iter().map(|s| s.to_string()).collect();
        RoleGrant {
            to_state: "approved".into(),
            role_class: "ns://Reviewer".into(),
            did: did.to_string(),
            eligible: granted_at.is_some() && revoked_at == Vec::<String>::new(),
            rows: vec!["r0".into()],
            windows: vec![RoleGrantWindow {
                row_id: "r0".into(),
                granted_at: granted_at.map(|s| s.to_string()),
                revoked_at,
            }],
        }
    }

    /// A vote cast after the grant and before any revocation is eligible.
    #[test]
    fn vote_after_grant_before_revocation_is_eligible() {
        let atom = atom_with_votes(&[("did:key:alice", "T2")]);
        let grants = vec![grant_with_window("did:key:alice", Some("T1"), vec![])];
        assert_eq!(
            eligible_votes(&atom, &gated_rule(), &grants).len(),
            1,
            "vote at T2, grant at T1, no revocation → eligible"
        );
    }

    /// A vote cast before the grant existed does NOT count.
    #[test]
    fn vote_before_grant_is_not_eligible() {
        // Vote at T1, grant only at T2 — voter was not in the role yet.
        let atom = atom_with_votes(&[("did:key:alice", "T1")]);
        let grants = vec![grant_with_window("did:key:alice", Some("T2"), vec![])];
        assert!(
            eligible_votes(&atom, &gated_rule(), &grants).is_empty(),
            "vote at T1, grant at T2 → not eligible (grant hadn't arrived yet)"
        );
    }

    /// A vote cast after a revocation does NOT count, but a vote cast BEFORE
    /// the revocation on the SAME atom still does. This verifies that the
    /// revocation gate is per-vote, not per-atom.
    #[test]
    fn revocation_gates_only_votes_cast_after_it() {
        // Two votes on the same atom: Alice voted at T2 (before T3 revocation)
        // and at T4 (after T3 revocation). Only the T2 vote should count.
        let atom = atom_with_votes(&[("did:key:alice", "T2"), ("did:key:alice", "T4")]);
        let grants = vec![grant_with_window("did:key:alice", Some("T1"), vec!["T3"])];
        let eligible = eligible_votes(&atom, &gated_rule(), &grants);
        assert_eq!(eligible.len(), 1);
        assert_eq!(eligible[0].at, "T2", "only the pre-revocation vote counts");
    }

    /// An already-settled edge must survive a later revocation: the fold is
    /// called with the same votes and the same windows, and only votes-after
    /// are filtered. Votes-before keep the edge settled. This is the core
    /// convergence guarantee.
    #[test]
    fn settled_edge_survives_revocation_of_voter() {
        // Alice voted at T2 to settle an edge (n=1). Role revoked at T3.
        let atom = atom_with_votes(&[("did:key:alice", "T2")]);
        let grants_before = vec![grant_with_window("did:key:alice", Some("T1"), vec![])];
        let grants_after = vec![grant_with_window("did:key:alice", Some("T1"), vec!["T3"])];
        // Before revocation: vote counts.
        assert_eq!(
            eligible_votes(&atom, &gated_rule(), &grants_before).len(),
            1
        );
        // After revocation: the SAME vote at T2 still counts because T2 < T3.
        assert_eq!(
            eligible_votes(&atom, &gated_rule(), &grants_after).len(),
            1,
            "a vote cast before the revocation remains eligible even after revocation lands"
        );
    }

    /// When `windows` is empty (no temporal info from the perspective), the
    /// fallback is the pre-tombstone `eligible: bool` — backward compat.
    #[test]
    fn empty_windows_fall_back_to_eligible_flag() {
        let atom = atom_with_votes(&[("did:key:alice", "T1")]);
        // eligible=true, windows=[] → fallback admits the vote
        let eligible_grant = RoleGrant {
            to_state: "approved".into(),
            role_class: "ns://Reviewer".into(),
            did: "did:key:alice".into(),
            eligible: true,
            rows: vec!["r0".into()],
            windows: vec![],
        };
        assert_eq!(
            eligible_votes(&atom, &gated_rule(), &[eligible_grant]).len(),
            1
        );

        // eligible=false, windows=[] → fallback denies the vote
        let ineligible_grant = RoleGrant {
            to_state: "approved".into(),
            role_class: "ns://Reviewer".into(),
            did: "did:key:alice".into(),
            eligible: false,
            rows: vec![],
            windows: vec![],
        };
        assert!(eligible_votes(&atom, &gated_rule(), &[ineligible_grant]).is_empty());
    }

    /// `resolve_role_grants` populates windows from the stub's per-DID
    /// timestamp overrides.
    #[tokio::test]
    async fn resolve_role_grants_populates_windows_from_timestamps() {
        let mut timestamps = std::collections::HashMap::new();
        timestamps.insert(
            "did:key:alice".to_string(),
            RoleGrantTimestamps {
                granted_at: Some("T1".into()),
                revoked_at: vec!["T3".into()],
            },
        );
        let stub = RoleStub {
            member_dids: dids(&["did:key:alice"]),
            rows_per_match: 1,
            timestamps,
            ..Default::default()
        };
        let grants = resolve_role_grants(
            &stub,
            "approved",
            &role(json!({ "className": "ns://Reviewer", "didProperty": "agent" })),
            &record(),
            &dids(&["did:key:alice"]),
        )
        .await
        .unwrap();
        assert_eq!(grants.len(), 1);
        let g = &grants[0];
        assert_eq!(g.windows.len(), 1);
        assert_eq!(g.windows[0].granted_at.as_deref(), Some("T1"));
        assert_eq!(g.windows[0].revoked_at, vec!["T3"]);
    }
}
