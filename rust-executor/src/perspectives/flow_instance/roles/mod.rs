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
//! - `granted_at` is the earliest `instance --didProperty--> did` link, when
//!   the query names a `didProperty` and that link exists — there the grant is
//!   dated from the assignment itself; otherwise the instance's own timestamp
//!   (its earliest link, as `model_query` reports it). Only that fallback is
//!   coarse: in it, membership acquired on a pre-existing instance dates from the
//!   instance, not the acquisition. Neither branch is ever "unknown, so always":
//!   an instance with no timestamp at all is an error.
//!
//!   **This paragraph described the intent from #1027 until #1065; it did not
//!   describe the code.** The `didProperty` branch had never once executed:
//!   the role's `didProperty` is a property *name* and the lookup passed it to
//!   `get_links` as an RDF *predicate*, so it matched nothing and every
//!   `didProperty` role fell through to the fallback — which is normally
//!   EARLIER than the assignment link, making the branch advertised as the
//!   precise one the widest one available. Every such grant was therefore
//!   retroactive to the instance's creation, and votes cast between then and
//!   the DID actually being assigned counted toward quorum. `didProperty` is
//!   resolved through the class shape at the store boundary since #1065
//!   (`flow_evaluator::PerspectiveInstance::did_property_predicate`); the
//!   sentence above is true from that commit forward and from no earlier one.
//!   Receipts minted before it date their grants from the instance.
//! - `granted_at` is **the granting run's quorum time** when the role query
//!   declares `producedByFlow`, and then nothing else may date it — not the
//!   assignment link, not the instance timestamp, not even as a fallback. See
//!   [`grant`](super::grant).
//! - `revoked_at` is the tombstone's link timestamp, for both kinds.
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
//! engineered around, in v1 (#1027).
//!
//! The half of this that *is* engineered around is the **grant** side, for
//! roles granted as flow outputs: a `producedByFlow` grant is dated from
//! [`SettledEdge::settled_at`](super::fold::SettledEdge) — the moment the
//! n-th distinct eligible voter signed — which no single party picks and
//! nobody can back-date without producing a different quorum. The replica
//! that collects the evidence checks that against the receipt in its own
//! graph; a reader of a serialised read-set trusts the carried date (see
//! [`grant`](super::grant) § *What a receipt of a gated flow proves*).
//!
//! ## History
//!
//! Before #1027 roles were re-derived live against the current graph and
//! revocation meant deleting the instance, so revoking someone later un-settled
//! an edge their vote had settled and replicas disagreed depending on when
//! they first derived. Windows fixed that; they were then *asserted* by the
//! replica that read them. Now the read-set carries the
//! [`RoleGrantEvidence`] — the grant links and tombstones themselves — and
//! [`RoleGrantEvidence::resolve`] recomputes the [`RoleGrantWindow`]s from
//! them, so a minted token's backing states no chronology of its own: it
//! hands over the signed material and lets the reader do the arithmetic.
//!
//! ## Same filter at collection and at read
//!
//! Whatever decides that a link counts toward a DID's membership must run
//! identically at the store boundary (when evidence is collected) and in
//! [`RoleGrantEvidence::resolve`] (when it is read) — otherwise the minter
//! and the verifier disagree, which either mints receipts that fail their own
//! verification or lets material the minter dropped widen a window. Hence one
//! predicate per link kind, exported from `flow_evaluator` and called from
//! both sites: [`grant_link_names_did`] and [`revocation_link_counts_for_did`].
//!
//! The two kinds are filtered *differently*, unchanged from pre-#1027:
//! tombstones must carry a verified signature, grant links need only name the
//! DID. Making grant links signature-filtered too is a real hole but not a
//! one-line one — `granted_at` falls back to the instance timestamp, which is
//! normally earlier than the assignment link, so dropping links can widen the
//! window rather than narrow it, and the bigger half of the hole is a missing
//! author filter. See <https://github.com/coasys/ad4m/issues/1063>, which
//! also waits on the `proof.valid` tri-state (#1046).

pub mod evidence;
#[cfg(test)]
mod test_support;
pub mod window;

use super::atom::{TransitionAtom, Vote};
use crate::perspectives::flow_context::FlowInstanceRecord;
pub use crate::perspectives::flow_evaluator::RoleRevocation;
use crate::perspectives::flow_evaluator::{requires_query_input, run_query, RequiresQueryable};
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery};
use evidence::instance_timestamp;
pub use evidence::{RoleGrantEvidence, RoleInstanceHistory};
pub use window::{RoleGrant, RoleGrantWindow};

/// Ask a `fromRole` gate about each candidate and collect the links behind
/// each matched instance's history, so the fold can gate every vote as of its
/// own timestamp — and so a reader can re-derive that gating from the links
/// rather than from our summary of them.
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
/// translation failure. Timing is **not** decided here any more: an instance
/// that cannot be placed in time fails closed in [`RoleGrantEvidence::resolve`],
/// on both sides of the wire, rather than only on this one.
///
/// Per matched instance there is one [`RequiresQueryable::role_grant_links`]
/// call for the grant links and the signed tombstones — two `get_links`
/// queries under the live impl, so the store fan-out is
/// candidates × instances × 2. Authority is deliberately **not** applied
/// here: the tombstones travel unfiltered and the reader applies
/// [`revocation_authorised`] itself, so a minter cannot silently mis-apply
/// the rule. Nothing here is queried again by the fold.
pub async fn resolve_role_grants<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    to_state: &str,
    role: &ModelQuery,
    record: &FlowInstanceRecord,
    candidates: &[String],
) -> anyhow::Result<Vec<RoleGrantEvidence>> {
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

    // A property NAME, not a predicate. The store boundary resolves it
    // through the class's shape before querying links — see
    // `flow_evaluator::PerspectiveInstance::did_property_predicate`.
    let did_property = role.did_property.as_deref();

    // A `producedByFlow` gate is decided here, against this replica's own
    // graph, once for the whole role: every receipt fully verified, bound to
    // `(role.class_name, id)`. Over budget, or a flow this replica does not
    // hold, is an ERROR that propagates — see `grant`. Every other role pays
    // nothing.
    let produced = match &role.produced_by_flow {
        Some(spec) => {
            Some(super::grant::produced_at_by_instance(perspective, &role.class_name, spec).await?)
        }
        None => None,
    };

    let mut evidence = Vec::with_capacity(candidates.len());
    for did in candidates {
        let input = requires_query_input(role, record, did)?;
        let matched = run_query(perspective, &role.class_name, &input).await?;

        let mut instances = Vec::with_capacity(matched.len());
        for item in &matched {
            let links = perspective
                .role_grant_links(&role.class_name, &item.id, did_property, did)
                .await?;
            instances.push(RoleInstanceHistory {
                instance_id: item.id.clone(),
                grant_links: links.grant_links,
                revocation_links: links.revocation_links,
                asserted_instance_timestamp: instance_timestamp(item),
                produced_at: produced
                    .as_ref()
                    .and_then(|by_id| by_id.get(&item.id).cloned()),
            });
        }
        // Stable by instance URI: the read-set must not depend on the order a
        // store happens to return matches in. The *view*'s order is
        // `resolve`'s business (by grant time); this is only the carrier.
        instances.sort_by(|a, b| a.instance_id.cmp(&b.instance_id));

        evidence.push(RoleGrantEvidence {
            to_state: to_state.to_string(),
            role_class: role.class_name.clone(),
            did: did.clone(),
            instances,
        });
    }
    Ok(evidence)
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
    use super::test_support::*;
    use super::*;
    use serde_json::{json, Value};
    /// `(name, role query, member DIDs, candidates, expected eligible set,
    /// expected query count)`.
    #[tokio::test]
    #[rustfmt::skip]
    async fn both_role_query_shapes_resolve_per_candidate() {
        let cases: Vec<(&str, Value, Vec<&str>, Vec<&str>, Vec<&str>, Option<usize>)> = vec![
            ("shape 1: didProperty filters candidates",
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
             vec![ALICE()], vec![ALICE(), BOB()], vec![ALICE()], Some(2)),
            ("shape 2: $did token substitutes per candidate",
             json!({ "className": "ns://Member", "where": { "member": "$did" } }),
             vec![BOB()], vec![ALICE(), BOB()], vec![BOB()], None),
            ("one role instance does not satisfy count.min = 2",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "count": { "min": 2 } }),
             vec![ALICE()], vec![ALICE()], vec![], None),
        ];

        for (name, role_json, member_dids, candidates, expected, expect_calls) in cases {
            let stub = members(&member_dids);
            let role = role(role_json);
            let evidence =
                resolve_role_grants(&stub, "approved", &role, &record(), &dids(&candidates))
                    .await
                    .unwrap();
            let grants = views(&evidence, &role);
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
        ];

        for (name, stub, role_json, expect_contains) in cases {
            let err = resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(&[ALICE()]))
                .await
                .expect_err(name);
            assert!(err.to_string().contains(expect_contains), "{name}: got {err:#}");
        }

        // Timing fails closed one step later, in the pure resolve, so it fails
        // the same way for a reader off-perspective: an instance with no signed
        // grant link and no instance timestamp cannot be placed in time, and
        // an unplaceable grant gates nothing rather than gating everything.
        let stub = RoleStub { undated_instances: true, ..members(&[ALICE()]) };
        let undated = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let evidence = resolve_role_grants(&stub, "approved", &undated, &record(), &dids(&[ALICE()]))
            .await
            .expect("collecting the links themselves cannot fail on timing");
        let err = evidence[0].resolve(&translated(&undated, ALICE()), &undated).expect_err("undated instance");
        assert!(err.to_string().contains("cannot be placed in time"), "got {err:#}");

        // And the undeterminable rule never even runs a query.
        let stub = RoleStub { unconditional_instances: Some(1), ..Default::default() };
        let _ = resolve_role_grants(
            &stub, "approved",
            &role(json!({ "className": "ns://Quorum", "where": { "open": true } })),
            &record(), &dids(&[ALICE()]),
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
            outputs_hash: None,
            outputs: Vec::new(),
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

    fn window(
        instance_id: &str,
        granted_at: &str,
        revocations: &[(&str, &str)],
    ) -> RoleGrantWindow {
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
        let atom = atom_with_votes(&[(ALICE(), T1), (BOB(), T2)]);
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
            grant("approved", BOB(), vec![window("r0", T0, &[])]),
            grant("approved", ALICE(), vec![]),
            grant("shipped", ALICE(), vec![window("r1", T0, &[])]),
        ];
        assert_eq!(
            eligible_votes(&atom, &gated, &grants)
                .into_iter()
                .map(|v| v.did)
                .collect::<Vec<_>>(),
            vec![BOB()],
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
            let revocations: Vec<(&str, &str)> = revoked_at.map(|at| (ADMIN(), at)).into_iter().collect();
            let grants = vec![grant("approved", ALICE(), vec![window("r0", granted_at, &revocations)])];
            let atom = atom_with_votes(&[(ALICE(), vote_at)]);
            assert_eq!(eligible_votes(&atom, &gated_rule(None), &grants).len(), usize::from(counts), "{name}");
        }
    }

    /// The gate is per vote, not per atom or per DID: on one atom, the same
    /// agent's vote before the tombstone counts and their vote after it does
    /// not — which is what keeps an edge they settled settled.
    #[test]
    fn a_revocation_gates_only_votes_cast_after_it() {
        let atom = atom_with_votes(&[(ALICE(), T2), (ALICE(), T4)]);
        let before = vec![grant("approved", ALICE(), vec![window("r0", T1, &[])])];
        let after = vec![grant(
            "approved",
            ALICE(),
            vec![window("r0", T1, &[(ADMIN(), T3)])],
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
            ALICE(),
            vec![window("r0", T0, &[]), window("r1", T1, &[(ADMIN(), T3)])],
        )];
        let rule = gated_rule(Some(json!({ "min": 2 })));
        assert_eq!(
            eligible_votes(&atom_with_votes(&[(ALICE(), T2)]), &rule, &grants).len(),
            1
        );
        assert!(eligible_votes(&atom_with_votes(&[(ALICE(), T4)]), &rule, &grants).is_empty());
    }
}
