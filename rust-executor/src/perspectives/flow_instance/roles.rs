//! Who may vote: resolving a rule's `fromRole` gate, **as of each vote's own
//! timestamp** (#1027).
//!
//! This is the only step of a state read that touches the store, which is
//! why it happens in the loader and not in [`fold`](super::fold). It fails
//! closed in every direction: a store error aborts the read rather than
//! mis-counting a vote; a role query that cannot tell one DID from another
//! is an error rather than "everybody passes"; a role instance that cannot be
//! placed in time is an error rather than "granted since forever".
//!
//! ## What a grant is, and what a revocation is
//!
//! A grant is a role instance — whatever class the flow author chose — that the
//! rule's query matches for a DID. A revocation is a signed tombstone link
//! on that instance, [`ROLE_GRANT_REVOKED_PREDICATE`](super::atom::ROLE_GRANT_REVOKED_PREDICATE)
//! naming the revoked DID,
//! **never a deletion**. Role instances stay in the graph for good; the tombstone
//! ends the instance's validity for that DID from the tombstone's own timestamp.
//! Both are Shared links on the same trust substrate as the votes.
//!
//! This puts one convention on social DNA authors: **role instances are add-only,
//! and membership ends only through tombstones.** The role query runs against
//! the *current* graph — only the instance's existence is windowed in time — so a
//! query keyed on a mutable property (`where: { active: true }`) reopens the
//! deletion problem through the side door: flipping the property makes the
//! instance vanish from historical verdicts too, un-settling edges its votes once
//! settled. State that changes belongs in new instances, not in edited ones.
//!
//! ## As-of gating
//!
//! Every vote is gated against the graph *as it stood at the vote's
//! timestamp*: an instance counts toward the rule's `count` for a vote at `t` iff
//! it was granted at or before `t` and no authorised revocation of it
//! existed at or before `t` — `granted_at <= t < revoked_at`. Both bounds
//! are deliberate: a grant written in the same instant as the vote counts,
//! and a revocation written in the same instant as the vote already gates
//! it ([`RoleGrantWindow::open_at`]). Eligibility is therefore a function of
//! graph *content* alone. A newcomer replica deriving from scratch reaches
//! the state everyone else holds, an edge settled by a then-eligible voter
//! stays settled after the voter is revoked, and a revocation that syncs in
//! late heals exactly like a late vote — the fold re-runs and re-derives.
//!
//! ## Where the timestamps come from
//!
//! - `granted_at` is the earliest `instance --didProperty--> did` link, when the
//!   query names a `didProperty` and that link exists — there the grant is
//!   dated from the assignment itself; otherwise the instance's own timestamp
//!   (its earliest link, as `model_query` reports it). Only that fallback is
//!   coarse: in it, membership acquired on a pre-existing instance dates from the
//!   instance, not the acquisition. Neither branch is ever "unknown, so always":
//!   an instance with no timestamp at all is an error.
//! - `revoked_at` is the tombstone's link timestamp.
//!
//! Timestamps are compared as **parsed instants** ([`super::time`]), never
//! as strings: they are client-asserted RFC 3339 and clients disagree on
//! flavour (`+00:00` vs `Z`, fractional digits, non-UTC offsets), so string
//! order diverges from instant order inside a second (#1000). Anything that
//! cannot be placed in time fails closed — see [`RoleGrantWindow::open_at`]
//! and [`fold`](super::fold) § *Ordering and time*.
//!
//! ## Authority
//!
//! A tombstone counts only when its signature verifies **and** its author
//! is one the grant's own rule would accept as a granter: the role query's
//! `author` condition (top level, and any `or` branch) is applied to the
//! tombstone's author through `model_query`'s own condition evaluator
//! ([`revocation_authorised`]). The condition is read from the *translated*
//! query — `$did` is already substituted to the candidate whose membership
//! is being tested, and never stands for anyone's author — so
//! `where: { author: "did:…admin" }` makes grants *and* revocations
//! admin-only, while `author: "$did"` requires the author to be that
//! candidate: a role that can only be self-granted is therefore also only
//! self-revoked. A rule with no author condition lets anyone grant — and,
//! symmetrically, anyone revoke — which is exactly the authority that social
//! DNA already declares.
//!
//! ## Accepted caveat: author-asserted timestamps
//!
//! A tombstone's timestamp is asserted by its author, so an admin could
//! back-date a revocation and retroactively un-settle an edge. That is an
//! escalation of *timing* only, within an authority the social DNA already
//! grants: the admin controls membership, and a back-dated revocation
//! achieves nothing a genuinely earlier one would not have. Documented, not
//! engineered around, in v1 (#1027). Roles granted as flow outputs — the
//! planned recursive composition — will carry a quorum-fixed time no single
//! party can back-date, shrinking the residual to the admin-authored base
//! case.
//!
//! ## History
//!
//! Before #1027 roles were re-derived live against the current graph and
//! revocation meant deleting the instance, so revoking someone later un-settled
//! an edge their vote had settled and replicas disagreed depending on when
//! they first derived. The [`RoleGrant`]s in the read-set now carry each
//! instance's [`RoleGrantWindow`] — grant time and the tombstones honoured — so a
//! minted token's backing names a fully resolvable history.

use super::atom::{TransitionAtom, Vote};
use super::time::parse_link_timestamp;
use crate::perspectives::flow_context::FlowInstanceRecord;
pub use crate::perspectives::flow_evaluator::RoleRevocation;
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, requires_query_input, run_query, EvidenceItem, RequiresQueryable,
};
use crate::perspectives::model_query::{matches_condition, WhereCondition};
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery, ModelQueryCount};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

/// One matched role instance's history for one DID: when it started to count and
/// every authorised tombstone that ended it.
///
/// Serialisable so the history rides in the read-set: a minted token's
/// backing names the instances *and* the tombstones its verdict rested on, and
/// nothing referenced is ever deleted.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrantWindow {
    /// The instance the role query matched for this DID.
    pub instance_id: String,
    /// When the instance started to count — see the module doc.
    pub granted_at: String,
    /// The authorised, signed tombstones on this instance naming this DID,
    /// earliest first. Usually none or one.
    pub revocations: Vec<RoleRevocation>,
}

impl RoleGrantWindow {
    /// The moment the instance stopped counting, if it has: the earliest
    /// revocation **by parsed instant** — string `min()` would pick by
    /// client format inside a sub-second collision (#1000).
    ///
    /// Fail-closed like [`Self::open_at`]: a revocation that cannot be
    /// placed in time sorts *first*, never last. `open_at` treats such a
    /// tombstone as closing the window outright, so any surface rendering
    /// this answer must show that tombstone — a later, parseable one
    /// presenting itself as the earliest (or, with no other revocations,
    /// the instance reading as "not revoked") would be the "revocation ignored"
    /// direction this module cannot afford.
    pub fn revoked_at(&self) -> Option<&str> {
        self.revocations
            .iter()
            .min_by_key(|r| {
                let parsed = parse_link_timestamp(&r.at);
                (
                    parsed.is_some(),
                    parsed.unwrap_or(chrono::DateTime::<chrono::Utc>::MIN_UTC),
                    r.at.as_str(),
                )
            })
            .map(|r| r.at.as_str())
    }

    /// Whether the instance counted for a vote cast at `at`:
    /// `granted_at <= at < revoked_at`, compared as parsed instants —
    /// timestamps are client-asserted and clients disagree on RFC 3339
    /// flavour, so string comparison would gate same-second votes by client
    /// library (#1000).
    ///
    /// **Anything that cannot be placed in time fails closed.** An
    /// unparseable `at` or `granted_at` means the window never opens for
    /// that question; an unparseable revocation timestamp closes the window
    /// outright — an authorised tombstone exists, so the safe reading is
    /// "revoked", never "revocation ignored". A revocation stamped with the
    /// vote's own timestamp already gates it.
    pub fn open_at(&self, at: &str) -> bool {
        let (Some(at), Some(granted)) = (
            parse_link_timestamp(at),
            parse_link_timestamp(&self.granted_at),
        ) else {
            return false;
        };
        if granted > at {
            return false;
        }
        let mut revoked: Option<chrono::DateTime<chrono::Utc>> = None;
        for r in &self.revocations {
            match parse_link_timestamp(&r.at) {
                None => return false,
                Some(instant) => revoked = Some(revoked.map_or(instant, |m| m.min(instant))),
            }
        }
        revoked.is_none_or(|revoked| at < revoked)
    }
}

/// The role instances behind one `(target state, DID)` pair, with their history.
///
/// Part of the read-set, so a verdict is auditable after the fact: "Bob
/// counted toward `approved` at 10:02 because instance r1 said he was a Reviewer
/// from 09:00 and its tombstone is from 11:00".
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrant {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    /// IDs of the instances the role query matched for this DID (`windows`, by
    /// ID).
    pub instances: Vec<String>,
    /// One history per matched instance, sorted by `(granted_at, instance_id)`.
    pub windows: Vec<RoleGrantWindow>,
}

impl RoleGrant {
    /// How many of the matched instances counted at `at`.
    pub fn instances_open_at(&self, at: &str) -> usize {
        self.windows.iter().filter(|w| w.open_at(at)).count()
    }

    /// Whether this DID satisfied the rule's `count` at `at` — the number of
    /// instances open then, under the same cardinality rule a `requires` guard
    /// uses (`None` = at least one).
    pub fn eligible_at(&self, at: &str, count: Option<&ModelQueryCount>) -> bool {
        cardinality_satisfied(count, self.instances_open_at(at))
    }
}

/// Whether `author` could have written a grant the translated role query
/// accepts — and may therefore revoke one.
///
/// Reads only the `author` conditions of the translated `where` (the top
/// level, and each `OR` branch, of which at least one must accept), and
/// evaluates each with `model_query`'s own [`matches_condition`], so the
/// condition means exactly what it means for the instances. No author condition
/// anywhere accepts everyone, as the query itself does. A condition that
/// cannot be read is a refusal, never a pass.
///
/// Top-level keys plus `OR` is not a simplification: SHACL `ModelQuery`
/// grammar has exactly a flat `where` and an `OR` of flat branches — no
/// `AND`, no `NOT` — and the translator emits the same uppercase `OR` key
/// `matches_where` reads. A reader who knows `matches_where` also handles
/// `AND`/`NOT` on other paths should not wait for them here: the role-query
/// translator cannot produce them.
pub fn revocation_authorised(translated_query: &Value, author: &str) -> bool {
    fn accepted(where_clause: &Map<String, Value>, author: &str) -> bool {
        let own = match where_clause.get("author") {
            None => true,
            Some(cond) => serde_json::from_value::<WhereCondition>(cond.clone())
                .map(|c| matches_condition(&Value::String(author.to_string()), &c))
                .unwrap_or(false),
        };
        let branches = match where_clause.get("OR") {
            None => true,
            Some(Value::Array(alts)) => alts
                .iter()
                .any(|alt| alt.as_object().is_some_and(|w| accepted(w, author))),
            Some(_) => false,
        };
        own && branches
    }
    match translated_query.get("where") {
        None => true,
        Some(Value::Object(w)) => accepted(w, author),
        Some(_) => false,
    }
}

/// The `timestamp` `model_query` reports for a hydrated instance: its earliest
/// link. Absent only for an instance that hydration could not date.
fn instance_timestamp(item: &EvidenceItem) -> Option<String> {
    serde_json::from_str::<Value>(&item.content)
        .ok()?
        .get("timestamp")?
        .as_str()
        .filter(|s| !s.is_empty())
        .map(str::to_string)
}

/// Ask a `fromRole` gate about each candidate and record each matched instance's
/// history, so the fold can gate every vote as of its own timestamp.
///
/// Both role-query shapes collapse into one per-candidate membership check,
/// because the fold only ever intersects the role set with the DIDs that
/// actually voted:
///
/// - a `$did`-templated query substitutes the candidate's DID directly;
/// - a `didProperty` query becomes `where.<prop> = candidate`, the
///   membership form of "extract the DIDs from the role instances".
///
/// A query that references the DID in neither way cannot discriminate
/// between candidates, and "I cannot determine membership" must never
/// degrade to "everyone is a member" — so it is an `Err`, as is any store or
/// translation failure, and a matched instance that carries no timestamp at all.
/// The caller then abandons the read.
///
/// Per matched instance there is one
/// [`RequiresQueryable::role_grant_timestamps`] call for the grant link and
/// the signed tombstones — two `get_links` queries under the live impl, so
/// the store fan-out is candidates × instances × 2; the tombstones are then
/// filtered by [`revocation_authorised`] against this very query. Nothing
/// here is queried again by the fold.
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

        let mut windows = Vec::with_capacity(matched.len());
        for item in &matched {
            let history = perspective
                .role_grant_timestamps(&item.id, grant_predicate, did)
                .await?;
            let Some(granted_at) = history
                .granted_at
                .or_else(|| instance_timestamp(item))
                .filter(|t| parse_link_timestamp(t).is_some())
            else {
                anyhow::bail!(
                    "resolve_role_grants: role instance `{}` (`{}`) carries no RFC 3339-parseable timestamp, so the grant cannot be placed in time; refusing to gate (fail-closed)",
                    item.id,
                    role.class_name
                );
            };
            // Sorted here, not by the store: the read-set must not depend on
            // the order a store happens to return links in.
            let mut revocations: Vec<RoleRevocation> = history
                .revocations
                .into_iter()
                .filter(|r| revocation_authorised(&input, &r.by))
                .collect();
            revocations.sort_by(|a, b| {
                (parse_link_timestamp(&a.at), &a.at, &a.by).cmp(&(
                    parse_link_timestamp(&b.at),
                    &b.at,
                    &b.by,
                ))
            });
            revocations.dedup();
            windows.push(RoleGrantWindow {
                instance_id: item.id.clone(),
                granted_at,
                revocations,
            });
        }
        windows.sort_by(|a, b| {
            (
                parse_link_timestamp(&a.granted_at),
                &a.granted_at,
                &a.instance_id,
            )
                .cmp(&(
                    parse_link_timestamp(&b.granted_at),
                    &b.granted_at,
                    &b.instance_id,
                ))
        });

        grants.push(RoleGrant {
            to_state: to_state.to_string(),
            role_class: role.class_name.clone(),
            did: did.clone(),
            instances: windows.iter().map(|w| w.instance_id.clone()).collect(),
            windows,
        });
    }
    Ok(grants)
}

/// The votes on `atom` that its rule admits — pure, so the fold's inputs can
/// be rebuilt from a serialised read-set. A rule without a `fromRole` admits
/// every vote; with one, a vote counts only when the grant for that atom's
/// target state and that voter satisfied the rule's `count` **at the vote's
/// own timestamp** ([`RoleGrant::eligible_at`]). No grant, no vote.
pub fn eligible_votes(
    atom: &TransitionAtom,
    rule: &ConsensusRule,
    grants: &[RoleGrant],
) -> Vec<Vote> {
    let Some(role) = rule.from_role.as_ref() else {
        return atom.votes.clone();
    };
    atom.votes
        .iter()
        .filter(|vote| {
            grants.iter().any(|g| {
                g.to_state == atom.to_state
                    && g.did == vote.did
                    && g.eligible_at(&vote.at, role.count.as_ref())
            })
        })
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_evaluator::RoleGrantTimestamps;
    use async_trait::async_trait;
    use serde_json::json;
    use std::collections::HashMap;
    use std::sync::Mutex;

    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";
    const ADMIN: &str = "did:key:admin";
    const LEAD: &str = "did:key:lead";
    const MALLORY: &str = "did:key:mallory";
    const T0: &str = "2026-01-01T00:00:00.000Z";
    const T1: &str = "2026-01-02T00:00:00.000Z";
    const T2: &str = "2026-01-03T00:00:00.000Z";
    const T3: &str = "2026-01-04T00:00:00.000Z";
    const T4: &str = "2026-01-05T00:00:00.000Z";
    /// Later than any grant or tombstone a test writes: "eligible now".
    const NOW: &str = "2030-01-01T00:00:00.000Z";

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

    fn revocation(by: &str, at: &str) -> RoleRevocation {
        RoleRevocation {
            by: by.into(),
            at: at.into(),
        }
    }

    fn history(granted_at: Option<&str>, revocations: &[(&str, &str)]) -> RoleGrantTimestamps {
        RoleGrantTimestamps {
            granted_at: granted_at.map(str::to_string),
            revocations: revocations
                .iter()
                .map(|(by, at)| revocation(by, at))
                .collect(),
        }
    }

    /// The DIDs whose grants satisfy `count` at [`NOW`].
    fn eligible_now<'g>(grants: &'g [RoleGrant], count: Option<&ModelQueryCount>) -> Vec<&'g str> {
        grants
            .iter()
            .filter(|g| g.eligible_at(NOW, count))
            .map(|g| g.did.as_str())
            .collect()
    }

    /// Query-aware stub: a call whose JSON mentions one of `member_dids`
    /// returns `rows_per_match` instances (`r0`, `r1`, …, each dated [`T0`] unless
    /// `undated_instances`); `unconditional_instances` (for DID-independent queries)
    /// wins over matching when set; `error` fails every call. `histories`
    /// is what the store says about each DID's instances.
    #[derive(Default)]
    struct RoleStub {
        member_dids: Vec<String>,
        rows_per_match: usize,
        unconditional_instances: Option<usize>,
        undated_instances: bool,
        error: Option<String>,
        calls: Mutex<Vec<String>>,
        histories: HashMap<String, RoleGrantTimestamps>,
    }

    #[async_trait]
    impl RequiresQueryable for RoleStub {
        async fn model_query(&self, _class: &str, query_json: &str) -> anyhow::Result<String> {
            self.calls.lock().unwrap().push(query_json.to_string());
            if let Some(msg) = &self.error {
                return Err(anyhow::anyhow!(msg.clone()));
            }
            let n = self.unconditional_instances.unwrap_or_else(|| {
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
            let instances: Vec<Value> = (0..n)
                .map(|i| {
                    if self.undated_instances {
                        json!({ "id": format!("r{i}") })
                    } else {
                        json!({ "id": format!("r{i}"), "timestamp": T0, "author": ADMIN })
                    }
                })
                .collect();
            Ok(json!({ "instances": instances, "totalCount": n }).to_string())
        }

        async fn role_grant_timestamps(
            &self,
            _row_id: &str,
            _grant_predicate: Option<&str>,
            did: &str,
        ) -> anyhow::Result<RoleGrantTimestamps> {
            Ok(self.histories.get(did).cloned().unwrap_or_default())
        }
    }

    fn members(member_dids: &[&str]) -> RoleStub {
        RoleStub {
            member_dids: dids(member_dids),
            rows_per_match: 1,
            ..Default::default()
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
             &[ALICE], &[ALICE, BOB], &[ALICE], Some(2)),
            ("shape 2: $did token substitutes per candidate",
             json!({ "className": "ns://Member", "where": { "member": "$did" } }),
             &[BOB], &[ALICE, BOB], &[BOB], None),
            ("one role instance does not satisfy count.min = 2",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "count": { "min": 2 } }),
             &[ALICE], &[ALICE], &[], None),
        ];

        for (name, role_json, member_dids, candidates, expected, expect_calls) in cases {
            let stub = members(member_dids);
            let role = role(role_json);
            let grants =
                resolve_role_grants(&stub, "approved", &role, &record(), &dids(candidates))
                    .await
                    .unwrap();
            assert_eq!(eligible_now(&grants, role.count.as_ref()), expected.to_vec(), "{name}");
            assert_eq!(grants.len(), candidates.len(), "{name}: one verdict per candidate");
            for g in &grants {
                assert_eq!(g.to_state, "approved", "{name}: verdicts are per target state");
                assert_eq!(g.instances, g.windows.iter().map(|w| w.instance_id.clone()).collect::<Vec<_>>(),
                           "{name}: `instances` is the windows' IDs");
                for w in &g.windows {
                    assert_eq!(w.granted_at, T0, "{name}: an undated grant link dates from the instance");
                    assert!(w.revocations.is_empty(), "{name}: no tombstones were reported");
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

    /// Every failure mode fails CLOSED — error out, never degrade to
    /// "everyone passes". A `fromRole` is a security rule, and a
    /// misconfigured security rule admits nobody.
    #[tokio::test]
    #[rustfmt::skip]
    async fn every_role_failure_is_an_error_not_allow_all() {
        let cases: Vec<(&str, RoleStub, Value, &str)> = vec![
            ("a query that cannot discriminate between DIDs",
             RoleStub { unconditional_instances: Some(1), ..Default::default() },
             json!({ "className": "ns://Quorum", "where": { "open": true } }), "cannot discriminate"),
            ("a store error",
             RoleStub { error: Some("store down".into()), ..Default::default() },
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }), "store down"),
            ("a role query model_query cannot express",
             RoleStub::default(),
             json!({ "className": "ns://Reviewer", "didProperty": "agent",
                     "where": { "status": { "matches": ".*" } } }), ""),
            ("a role instance that cannot be placed in time",
             RoleStub { undated_instances: true, ..members(&[ALICE]) },
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }), "cannot be placed in time"),
        ];

        for (name, stub, role_json, expect_contains) in cases {
            let err = resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(&[ALICE]))
                .await
                .expect_err(name);
            assert!(err.to_string().contains(expect_contains), "{name}: got {err:#}");
        }
        // And the undeterminable rule never even runs a query.
        let stub = RoleStub { unconditional_instances: Some(1), ..Default::default() };
        let _ = resolve_role_grants(
            &stub, "approved",
            &role(json!({ "className": "ns://Quorum", "where": { "open": true } })),
            &record(), &dids(&[ALICE]),
        ).await;
        assert!(stub.calls.lock().unwrap().is_empty(), "no query may run for an undeterminable rule");
    }

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
            votes,
        }
    }

    fn gated_rule(count: Option<Value>) -> ConsensusRule {
        let mut q = json!({ "className": "ns://Reviewer", "didProperty": "agent" });
        if let Some(c) = count {
            q["count"] = c;
        }
        ConsensusRule {
            n: 1,
            from_role: Some(role(q)),
        }
    }

    fn window(instance_id: &str, granted_at: &str, revocations: &[(&str, &str)]) -> RoleGrantWindow {
        RoleGrantWindow {
            instance_id: instance_id.into(),
            granted_at: granted_at.into(),
            revocations: revocations
                .iter()
                .map(|(by, at)| revocation(by, at))
                .collect(),
        }
    }

    fn grant(to_state: &str, did: &str, windows: Vec<RoleGrantWindow>) -> RoleGrant {
        RoleGrant {
            to_state: to_state.into(),
            role_class: "ns://Reviewer".into(),
            did: did.into(),
            instances: windows.iter().map(|w| w.instance_id.clone()).collect(),
            windows,
        }
    }

    #[test]
    fn a_rule_without_a_role_admits_every_vote_and_one_with_a_role_admits_only_grantees() {
        let atom = atom_with_votes(&[(ALICE, T1), (BOB, T2)]);
        let open = ConsensusRule {
            n: 1,
            from_role: None,
        };
        assert_eq!(eligible_votes(&atom, &open, &[]).len(), 2);

        let gated = gated_rule(None);
        assert!(
            eligible_votes(&atom, &gated, &[]).is_empty(),
            "no grant, no vote — an unresolved gate admits nobody"
        );
        let grants = vec![
            grant("approved", BOB, vec![window("r0", T0, &[])]),
            grant("approved", ALICE, vec![]),
            grant("shipped", ALICE, vec![window("r1", T0, &[])]),
        ];
        assert_eq!(
            eligible_votes(&atom, &gated, &grants)
                .into_iter()
                .map(|v| v.did)
                .collect::<Vec<_>>(),
            vec![BOB],
            "a grant with no instances admits nobody, and a grant for another target state is not a grant for this one"
        );
    }

    /// The as-of rule, including both boundaries: an instance counts for a vote at
    /// `t` iff `granted_at <= t < revoked_at`. `(name, vote at, granted at,
    /// revoked at, counts?)`.
    #[test]
    #[rustfmt::skip]
    fn a_vote_counts_only_while_the_grant_was_open_at_its_own_timestamp() {
        let cases: Vec<(&str, &str, &str, Option<&str>, bool)> = vec![
            ("vote after grant, never revoked",             T2, T1, None,     true),
            ("vote before the grant existed",               T1, T2, None,     false),
            ("vote at exactly the grant's timestamp",       T1, T1, None,     true),
            ("vote before the revocation",                  T2, T1, Some(T3), true),
            ("vote after the revocation",                   T4, T1, Some(T3), false),
            ("vote at exactly the revocation's timestamp",  T3, T1, Some(T3), false),
            ("revocation back-dated before the grant",      T2, T1, Some(T0), false),
        ];
        for (name, vote_at, granted_at, revoked_at, counts) in cases {
            let revocations: Vec<(&str, &str)> = revoked_at.map(|at| (ADMIN, at)).into_iter().collect();
            let grants = vec![grant("approved", ALICE, vec![window("r0", granted_at, &revocations)])];
            let atom = atom_with_votes(&[(ALICE, vote_at)]);
            assert_eq!(eligible_votes(&atom, &gated_rule(None), &grants).len(), usize::from(counts), "{name}");
        }
    }

    /// The gate is per vote, not per atom or per DID: on one atom, the same
    /// agent's vote before the tombstone counts and their vote after it does
    /// not — which is what keeps an edge they settled settled.
    #[test]
    fn a_revocation_gates_only_votes_cast_after_it() {
        let atom = atom_with_votes(&[(ALICE, T2), (ALICE, T4)]);
        let before = vec![grant("approved", ALICE, vec![window("r0", T1, &[])])];
        let after = vec![grant(
            "approved",
            ALICE,
            vec![window("r0", T1, &[(ADMIN, T3)])],
        )];
        assert_eq!(eligible_votes(&atom, &gated_rule(None), &before).len(), 2);
        let eligible = eligible_votes(&atom, &gated_rule(None), &after);
        assert_eq!(
            eligible.iter().map(|v| v.at.as_str()).collect::<Vec<_>>(),
            vec![T2],
            "the pre-revocation vote survives the revocation; the later one is gated"
        );
    }

    /// `count` is evaluated as of the vote too: with `min: 2`, a DID whose
    /// second instance was revoked at T3 still had two instances at T2 and one at T4.
    #[test]
    fn the_rules_count_is_evaluated_as_of_the_vote() {
        let grants = vec![grant(
            "approved",
            ALICE,
            vec![window("r0", T0, &[]), window("r1", T1, &[(ADMIN, T3)])],
        )];
        let rule = gated_rule(Some(json!({ "min": 2 })));
        assert_eq!(
            eligible_votes(&atom_with_votes(&[(ALICE, T2)]), &rule, &grants).len(),
            1
        );
        assert!(eligible_votes(&atom_with_votes(&[(ALICE, T4)]), &rule, &grants).is_empty());
    }

    /// A tombstone counts only from an author the grant's own rule accepts:
    /// the role query's `author` condition is applied to the tombstone's
    /// author. `(name, role query, accepted revokers, rejected revokers)` —
    /// every revoker writes a tombstone at [`T2`] and the window keeps only
    /// the accepted ones.
    #[tokio::test]
    #[rustfmt::skip]
    async fn a_tombstone_counts_only_from_an_author_the_grants_rule_accepts() {
        let cases: Vec<(&str, Value, &[&str], &[&str])> = vec![
            ("no author condition: anyone may grant, so anyone may revoke",
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
             &[ADMIN, MALLORY, ALICE], &[]),
            ("admin-only grants are admin-only revocations",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": ADMIN } }),
             &[ADMIN], &[MALLORY, ALICE]),
            ("a set of authorised granters",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": { "in": [ADMIN, LEAD] } } }),
             &[ADMIN, LEAD], &[MALLORY, ALICE]),
            ("`or` branches: a revoker any branch would accept as granter",
             json!({ "className": "ns://Reviewer", "didProperty": "agent",
                     "or": [ { "className": "ns://Reviewer", "where": { "author": ADMIN } },
                             { "className": "ns://Reviewer", "where": { "author": LEAD } } ] }),
             &[ADMIN, LEAD], &[MALLORY, ALICE]),
            ("`author: $did`: a self-granted role is self-revoked",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": "$did" } }),
             &[ALICE], &[ADMIN, MALLORY]),
        ];
        for (name, role_json, accepted, rejected) in cases {
            let mut stub = members(&[ALICE]);
            let revokers: Vec<(&str, &str)> = accepted.iter().chain(rejected).map(|by| (*by, T2)).collect();
            stub.histories.insert(ALICE.into(), history(Some(T1), &revokers));
            let grants = resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(&[ALICE]))
                .await
                .unwrap();
            let kept: Vec<&str> = grants[0].windows[0].revocations.iter().map(|r| r.by.as_str()).collect();
            let mut expected: Vec<&str> = accepted.to_vec();
            expected.sort();
            assert_eq!(kept, expected, "{name}");
            assert_eq!(grants[0].eligible_at(T3, None), accepted.is_empty(), "{name}: revoked iff someone authorised did it");
            assert!(grants[0].eligible_at(T1, None), "{name}: the vote before any tombstone still counts");
        }
    }

    /// The window records what the store said, in a deterministic order: the
    /// grant link's timestamp when there is one, else the instance's own; the
    /// authorised tombstones earliest first.
    #[tokio::test]
    async fn windows_carry_the_history_the_store_reported() {
        let mut stub = RoleStub {
            rows_per_match: 2,
            ..members(&[ALICE, BOB])
        };
        stub.histories.insert(
            ALICE.into(),
            history(Some(T1), &[(MALLORY, T3), (ADMIN, T2)]),
        );
        // Bob's instances have no `didProperty` link the store could date: they
        // date from the instances themselves (T0), and nothing revoked them.
        let grants = resolve_role_grants(
            &stub,
            "approved",
            &role(json!({ "className": "ns://Reviewer", "didProperty": "agent" })),
            &record(),
            &dids(&[ALICE, BOB]),
        )
        .await
        .unwrap();
        let alice = &grants[0];
        assert_eq!(alice.did, ALICE);
        assert_eq!(alice.instances, vec!["r0", "r1"]);
        for w in &alice.windows {
            assert_eq!(w.granted_at, T1);
            assert_eq!(
                w.revocations,
                vec![revocation(ADMIN, T2), revocation(MALLORY, T3)],
                "no author condition, so both tombstones count, earliest first"
            );
            assert_eq!(w.revoked_at(), Some(T2));
        }
        let bob = &grants[1];
        assert!(bob
            .windows
            .iter()
            .all(|w| w.granted_at == T0 && w.revocations.is_empty()));
        assert!(bob.eligible_at(NOW, None));
        assert!(!alice.eligible_at(NOW, None));
    }

    /// `revoked_at` must share `open_at`'s fail direction: a revocation that
    /// cannot be placed in time closes the window (`open_at`), so it is also
    /// the one `revoked_at` surfaces — never out-ranked by a later,
    /// parseable tombstone, and never dropped into "not revoked".
    #[test]
    fn an_unparseable_revocation_is_surfaced_never_ignored() {
        let w = RoleGrantWindow {
            row_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![revocation(ADMIN, "not-a-timestamp"), revocation(ADMIN, T2)],
        };
        assert_eq!(w.revoked_at(), Some("not-a-timestamp"));
        assert!(!w.open_at(NOW), "unparseable revocation closes the window");

        let only_garbage = RoleGrantWindow {
            row_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![revocation(ADMIN, "not-a-timestamp")],
        };
        assert_eq!(only_garbage.revoked_at(), Some("not-a-timestamp"));
    }
}
