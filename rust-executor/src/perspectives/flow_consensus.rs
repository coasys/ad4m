//! Consensus counting + transition firing for `FlowTransitionProposal`s —
//! slice 10.6 of the flow arc, the pass that CONSUMES what
//! [`super::flow_evaluator::run_engine_proposal_pass`] mints.
//!
//! Design authority: `planning/flow-firing-engine-design-2026-09-04.md` and
//! design doc §5.4 step 6 / §7. Ported from the pre-restructure
//! `feature/flow-mutations` branch (its `aggregate_flow_votes` /
//! `select_fire_candidate` / `fire_flow_consensus` core survives verbatim
//! where the semantics didn't change) and adapted to the current stack:
//!
//! - `proposed_at` is Ad4mModel's synthesised `createdAt` (the proposal SDNA
//!   deliberately has no `proposedAt` property).
//! - A DID qualifies toward consensus when it **proposed OR accepted**
//!   (`ad4m://acceptedBy` links, design §7.2) — the old core counted
//!   proposers only.
//! - Fired proposals are **kept and marked** (`ad4m://flow/resolved_as` →
//!   `"fired"`), not deleted: they are the co-signed flow-atoms Synergy
//!   mints from later. This deliberately deviates from design §5.4's
//!   "delete resolved proposals" — flagged to Nico 2026-09-04; flipping to
//!   spec-literal delete is one small change in [`resolve_proposals_fired`].
//!   Rejection stays delete, per §4.2.
//!
//! # What this module owns
//!
//! - [`FlowTransitionProposalRecord`] — data mirror of the on-graph proposal.
//! - [`aggregate_flow_votes`] — pure aggregation: bucket by
//!   `(from_state, to_state)`, count distinct qualifying DIDs, apply the
//!   `ConsensusRule` threshold.
//! - [`select_fire_candidate`] — pure stale-`from_state` guard.
//! - [`fire_flow_consensus`] — async composition: preconditions →
//!   [`super::flow_classes::advance_flow_instance_state`] → [`FireOutcome`].
//! - [`load_flow_transition_proposals`] — `model_query` loader for the live
//!   (unresolved) proposals targeting one `FlowInstance`.
//! - [`resolve_proposals_fired`] — the keep-and-mark write.
//!
//! # Non-goals here (owned by the pass orchestrator / later commits)
//!
//! - `fromRole` resolution — caller passes the pre-resolved eligible-DID
//!   set. When the rule has `from_role` the caller MUST supply it; omitting
//!   it errors, because a silent "all DIDs eligible" would misreport
//!   consensus.
//! - Evidence-hash re-verification (pass orchestrator, before firing).
//! - Weighted / delegation / time-decay consensus (v1.5+).

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::{
    advance_flow_instance_state, FLOW_TRANSITION_PROPOSAL_CLASS,
};
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::ConsensusRule;
use crate::types::{Link, LinkQuery, LinkStatus};
use std::collections::{BTreeMap, HashSet};

/// Predicate marking a proposal as consumed by a firing. Absence = live.
pub const RESOLVED_AS_PREDICATE: &str = "ad4m://flow/resolved_as";
/// Predicate for acceptance links: proposal URI → accepting DID (§4.2).
pub const ACCEPTED_BY_PREDICATE: &str = "ad4m://acceptedBy";

/// Data mirror of one on-graph `FlowTransitionProposal`, as the consensus
/// pass consumes it.
///
/// `proposed_at` is the hydrated `createdAt` (earliest link timestamp on the
/// proposal URI — RFC3339, Z-suffix, lex-sortable). `acceptors` are the DIDs
/// on `ad4m://acceptedBy` links; they qualify toward consensus exactly like
/// the proposer. `evidence_hash` is carried through for the orchestrator's
/// pre-fire re-verification; the aggregator itself never reads it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowTransitionProposalRecord {
    pub uri: String,
    pub from_state: String,
    pub to_state: String,
    pub proposer: String,
    pub proposed_at: String,
    pub acceptors: Vec<String>,
    pub evidence_hash: String,
}

impl FlowTransitionProposalRecord {
    /// Every DID vouching for this proposal: proposer + acceptors.
    fn qualifying_dids(&self) -> impl Iterator<Item = &String> {
        std::iter::once(&self.proposer).chain(self.acceptors.iter())
    }
}

/// Per-target result: one row per `(from_state, to_state)` that appears in
/// the input bag, whether it fires or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowVoteTally {
    pub from_state: String,
    pub to_state: String,
    /// Distinct DIDs that proposed or accepted this target, before any
    /// `from_role` gate. Sorted lexicographically.
    pub distinct_proposers: Vec<String>,
    /// Distinct DIDs surviving the `from_role` gate. Equal to
    /// `distinct_proposers` when the rule has no `from_role`. Sorted.
    pub eligible_proposers: Vec<String>,
    /// `consensus_rule.n`, copied onto every tally for logging/rendering.
    pub required_count: u32,
    /// `eligible_proposers.len() as u32 >= required_count`.
    pub consensus_reached: bool,
    /// The subset of input proposals targeting this pair, input order.
    pub contributing: Vec<FlowTransitionProposalRecord>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateFlowVotesResult {
    /// All tallies, sorted lexicographically by `(from_state, to_state)`.
    pub tallies: Vec<FlowVoteTally>,
    /// The tally that should fire next, if any: consensus-reached tallies
    /// only, earliest contributing `proposed_at` first, ties broken by lex
    /// `(from_state, to_state)`.
    pub fires: Option<FlowVoteTally>,
}

/// Pure aggregation entry point.
///
/// - `proposals`: every live record for one `FlowInstance`; grouped by
///   `(from_state, to_state)` internally, no pre-sort required.
/// - `consensus_rule`: `None` defaults to `{ n: 1 }` (§7.1).
/// - `eligible_dids`: pre-resolved `from_role` result set. Required when
///   the rule carries `from_role`.
pub fn aggregate_flow_votes(
    proposals: &[FlowTransitionProposalRecord],
    consensus_rule: Option<&ConsensusRule>,
    eligible_dids: Option<&HashSet<String>>,
) -> anyhow::Result<AggregateFlowVotesResult> {
    let default_rule;
    let rule: &ConsensusRule = match consensus_rule {
        Some(r) => r,
        None => {
            default_rule = ConsensusRule {
                n: 1,
                from_role: None,
            };
            &default_rule
        }
    };
    if rule.n == 0 {
        return Err(anyhow::anyhow!(
            "aggregate_flow_votes: consensus_rule.n must be a positive integer, got 0"
        ));
    }
    if rule.from_role.is_some() && eligible_dids.is_none() {
        return Err(anyhow::anyhow!(
            "aggregate_flow_votes: consensus_rule.from_role is set — caller must resolve it and pass eligible_dids (silent default would misreport consensus)"
        ));
    }

    // BTreeMap so bucket iteration is already lex-sorted by key.
    let mut buckets: BTreeMap<(String, String), Vec<FlowTransitionProposalRecord>> =
        BTreeMap::new();
    for p in proposals {
        buckets
            .entry((p.from_state.clone(), p.to_state.clone()))
            .or_default()
            .push(p.clone());
    }

    let mut tallies: Vec<FlowVoteTally> = Vec::with_capacity(buckets.len());
    for ((from_state, to_state), bucket) in buckets {
        let mut distinct: Vec<String> = {
            let set: HashSet<&String> = bucket.iter().flat_map(|p| p.qualifying_dids()).collect();
            set.into_iter().cloned().collect()
        };
        distinct.sort();
        let eligible: Vec<String> = match eligible_dids {
            Some(set) => distinct
                .iter()
                .filter(|d| set.contains(d.as_str()))
                .cloned()
                .collect(),
            None => distinct.clone(),
        };
        let consensus_reached = (eligible.len() as u32) >= rule.n;
        tallies.push(FlowVoteTally {
            from_state,
            to_state,
            distinct_proposers: distinct,
            eligible_proposers: eligible,
            required_count: rule.n,
            consensus_reached,
            contributing: bucket,
        });
    }

    let mut fires: Option<FlowVoteTally> = None;
    let mut fires_earliest: Option<String> = None;
    for t in &tallies {
        if !t.consensus_reached {
            continue;
        }
        let earliest = earliest_proposed_at(&t.contributing);
        let take = match (&fires, &fires_earliest) {
            (None, _) => true,
            (Some(f), Some(fe)) => match earliest.cmp(fe) {
                std::cmp::Ordering::Less => true,
                std::cmp::Ordering::Equal => match t.from_state.cmp(&f.from_state) {
                    std::cmp::Ordering::Less => true,
                    std::cmp::Ordering::Equal => t.to_state < f.to_state,
                    std::cmp::Ordering::Greater => false,
                },
                std::cmp::Ordering::Greater => false,
            },
            _ => false,
        };
        if take {
            fires = Some(t.clone());
            fires_earliest = Some(earliest);
        }
    }

    Ok(AggregateFlowVotesResult { tallies, fires })
}

/// Snapshot of what [`fire_flow_consensus`] wrote.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FireOutcome {
    /// The `FlowInstance` URI whose `currentState` was advanced.
    pub instance_uri: String,
    /// `currentState` before the advance.
    pub from_state: String,
    /// `currentState` after the advance.
    pub to_state: String,
    /// Distinct DIDs counted toward this consensus, sorted.
    pub fired_by_proposers: Vec<String>,
    /// Contributing proposal URIs, input order.
    pub contributing_proposal_uris: Vec<String>,
}

/// Choose which tally (if any) is safe to fire against a `FlowInstance`
/// snapshot right now.
///
/// Returns `None` when the aggregate has no `fires` tally, or when its
/// `from_state` differs from `current_state` — a firing pass has already
/// advanced the flow, so the votes are stale. That second guard is the
/// reason this exists as a separate function:
/// [`AggregateFlowVotesResult::fires`] is computed without reference to
/// the live instance, so callers MUST re-check against the current
/// snapshot before firing.
pub fn select_fire_candidate<'a>(
    current_state: &str,
    aggregate: &'a AggregateFlowVotesResult,
) -> Option<&'a FlowVoteTally> {
    let candidate = aggregate.fires.as_ref()?;
    if candidate.from_state != current_state {
        return None;
    }
    Some(candidate)
}

/// Advance an on-graph `FlowInstance` to `fired_tally.to_state`.
///
/// Preconditions (all enforced — a violation returns `Err` before touching
/// the perspective): consensus reached, `from_state` matches the live
/// snapshot, and the transition is not a no-op. The caller owns id-gen /
/// batching so the fire can bundle with follow-on writes (proposal
/// resolution, a future audit event) into one commit.
pub async fn fire_flow_consensus(
    perspective: &mut PerspectiveInstance,
    instance: &FlowInstanceRecord,
    fired_tally: &FlowVoteTally,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<FireOutcome> {
    if !fired_tally.consensus_reached {
        return Err(anyhow::anyhow!(
            "fire_flow_consensus: refusing to fire — tally has not reached consensus ({}/{} for {} → {})",
            fired_tally.eligible_proposers.len(),
            fired_tally.required_count,
            fired_tally.from_state,
            fired_tally.to_state,
        ));
    }
    if fired_tally.from_state != instance.current_state {
        return Err(anyhow::anyhow!(
            "fire_flow_consensus: stale tally — fromState={} does not match instance.currentState={} (flow already advanced?)",
            fired_tally.from_state,
            instance.current_state,
        ));
    }
    if fired_tally.to_state == instance.current_state {
        return Err(anyhow::anyhow!(
            "fire_flow_consensus: refusing to fire a no-op — toState={} equals instance.currentState",
            fired_tally.to_state,
        ));
    }

    let from_state = instance.current_state.clone();
    let to_state = fired_tally.to_state.clone();

    advance_flow_instance_state(
        perspective,
        &instance.instance_uri,
        &to_state,
        batch_id,
        context,
    )
    .await?;

    Ok(FireOutcome {
        instance_uri: instance.instance_uri.clone(),
        from_state,
        to_state,
        fired_by_proposers: fired_tally.eligible_proposers.clone(),
        contributing_proposal_uris: fired_tally
            .contributing
            .iter()
            .map(|p| p.uri.clone())
            .collect(),
    })
}

/// Mark every contributing proposal of a fired tally as consumed:
/// `ad4m://flow/resolved_as` → `"fired"`. The loader filters marked
/// proposals out, so a fired transition's votes can never count twice.
///
/// Keep-and-mark rather than delete (the design-note deviation from §5.4):
/// a fired proposal is a co-signed flow-atom — the record Synergy's
/// Proof-of-Flow minting reads later. To flip to spec-literal delete,
/// replace the `add_link` below with proposal deletion; the loader and
/// aggregator need no change either way.
pub async fn resolve_proposals_fired(
    perspective: &mut PerspectiveInstance,
    proposal_uris: &[String],
    context: &AgentContext,
) -> anyhow::Result<()> {
    for uri in proposal_uris {
        perspective
            .add_link(
                Link {
                    source: uri.clone(),
                    predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                    target: format!("literal:string:{}", urlencoding::encode("fired")),
                },
                LinkStatus::Shared,
                None,
                context,
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!("resolve_proposals_fired: add_link({uri}) failed: {e:#}")
            })?;
    }
    Ok(())
}

/// Load every **live** `FlowTransitionProposal` targeting a
/// `FlowInstance` URI: hydrated via `model_query`, then joined with
/// `ad4m://acceptedBy` acceptance links and filtered of
/// already-resolved proposals.
///
/// Silently returns `Ok(vec![])` when the proposal class hasn't been
/// registered on this perspective yet — a fresh perspective has no
/// proposals, and erroring would break the consensus pass on every call
/// before the first proposal lands. Same policy (and same message match)
/// as [`super::flow_context::load_flow_instances`].
pub async fn load_flow_transition_proposals(
    perspective: &PerspectiveInstance,
    flow_instance_uri: &str,
) -> anyhow::Result<Vec<FlowTransitionProposalRecord>> {
    if flow_instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "load_flow_transition_proposals: flow_instance_uri must not be empty (raw model_query would return every proposal on the perspective)"
        ));
    }
    let query = serde_json::json!({ "where": { "flowInstance": flow_instance_uri } });
    let json = match perspective
        .model_query(FLOW_TRANSITION_PROPOSAL_CLASS, &query.to_string())
        .await
    {
        Ok(j) => j,
        Err(e) => {
            let msg = format!("{e:#}");
            if msg.to_lowercase().contains("no shacl shape stored") {
                return Ok(vec![]);
            }
            return Err(anyhow::anyhow!(
                "load_flow_transition_proposals: model_query failed: {msg}"
            ));
        }
    };
    let parsed: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("load_flow_transition_proposals: response not JSON: {e:#}"))?;
    let instances = parsed
        .get("instances")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    let mut records = Vec::new();
    for v in &instances {
        let Some(mut record) = parse_flow_transition_proposal_from_hydrated(v) else {
            continue;
        };
        // Resolution + acceptance ride as raw links, not model properties —
        // one bounded link query per proposal for each.
        let resolved = perspective
            .get_links(&LinkQuery {
                source: Some(record.uri.clone()),
                predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                ..Default::default()
            })
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "load_flow_transition_proposals: resolved-as lookup on {} failed: {e:#}",
                    record.uri
                )
            })?;
        if !resolved.is_empty() {
            continue;
        }
        let acceptances = perspective
            .get_links(&LinkQuery {
                source: Some(record.uri.clone()),
                predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                ..Default::default()
            })
            .await
            .map_err(|e| {
                anyhow::anyhow!(
                    "load_flow_transition_proposals: acceptedBy lookup on {} failed: {e:#}",
                    record.uri
                )
            })?;
        let mut acceptors: Vec<String> =
            acceptances.iter().map(|l| l.data.target.clone()).collect();
        acceptors.sort();
        acceptors.dedup();
        record.acceptors = acceptors;
        records.push(record);
    }
    Ok(records)
}

/// Parse one hydrated `FlowTransitionProposal` JSON object into a record.
///
/// Returns `None` when `id` / `fromState` / `toState` / `proposer` /
/// `createdAt` is missing — an untyped or half-written proposal is
/// silently skipped rather than failing the whole pass. `evidenceHashes`
/// defaults to empty (the orchestrator treats a hash-less proposal as
/// unverifiable and skips it, fail-closed, at fire time). `acceptors`
/// start empty; [`load_flow_transition_proposals`] fills them from links.
pub fn parse_flow_transition_proposal_from_hydrated(
    v: &serde_json::Value,
) -> Option<FlowTransitionProposalRecord> {
    let uri = v.get("id").and_then(|x| x.as_str())?.to_string();
    let from_state = v.get("fromState").and_then(|x| x.as_str())?.to_string();
    let to_state = v.get("toState").and_then(|x| x.as_str())?.to_string();
    let proposer = v.get("proposer").and_then(|x| x.as_str())?.to_string();
    let proposed_at = v.get("createdAt").and_then(|x| x.as_str())?.to_string();
    let evidence_hash = v
        .get("evidenceHashes")
        .and_then(|x| x.as_str())
        .unwrap_or_default()
        .to_string();
    Some(FlowTransitionProposalRecord {
        uri,
        from_state,
        to_state,
        proposer,
        proposed_at,
        acceptors: Vec::new(),
        evidence_hash,
    })
}

/// Smallest `proposed_at` in a non-empty bucket. RFC3339 Z-suffix
/// timestamps sort lexicographically. An empty string sorts before any
/// real value — a de-facto tie-breaker winner, the safest failure mode
/// (fires first, exposes the bug).
fn earliest_proposed_at(bucket: &[FlowTransitionProposalRecord]) -> String {
    let mut earliest = bucket
        .first()
        .map(|p| p.proposed_at.clone())
        .unwrap_or_default();
    for p in bucket.iter().skip(1) {
        if p.proposed_at < earliest {
            earliest = p.proposed_at.clone();
        }
    }
    earliest
}

#[cfg(test)]
mod tests {
    use super::*;

    fn proposal(
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> FlowTransitionProposalRecord {
        proposal_with_uri("ad4m://flow/proposal/p", from_state, to_state, proposer, proposed_at)
    }

    fn proposal_with_uri(
        uri: &str,
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> FlowTransitionProposalRecord {
        FlowTransitionProposalRecord {
            uri: uri.to_string(),
            from_state: from_state.to_string(),
            to_state: to_state.to_string(),
            proposer: proposer.to_string(),
            proposed_at: proposed_at.to_string(),
            acceptors: Vec::new(),
            evidence_hash: "hash".to_string(),
        }
    }

    fn rule(n: u32) -> ConsensusRule {
        ConsensusRule {
            n,
            from_role: None,
        }
    }

    fn from_role_rule(n: u32) -> ConsensusRule {
        ConsensusRule {
            n,
            from_role: Some(
                serde_json::from_value(serde_json::json!({
                    "className": "ns://Reviewer",
                    "didProperty": "agent"
                }))
                .expect("role query deserializes"),
            ),
        }
    }

    fn dids(list: &[&str]) -> HashSet<String> {
        list.iter().map(|s| s.to_string()).collect()
    }

    // ---- aggregate_flow_votes -------------------------------------------

    #[test]
    fn errors_when_n_is_zero() {
        let err = aggregate_flow_votes(&[], Some(&rule(0)), None).unwrap_err();
        assert!(err.to_string().contains("positive integer"), "{err}");
    }

    #[test]
    fn errors_when_from_role_set_and_eligible_dids_missing() {
        let err = aggregate_flow_votes(&[], Some(&from_role_rule(1)), None).unwrap_err();
        assert!(err.to_string().contains("eligible_dids"), "{err}");
    }

    #[test]
    fn accepts_from_role_plus_empty_eligible_set_no_fire() {
        let props = vec![proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z")];
        let out =
            aggregate_flow_votes(&props, Some(&from_role_rule(1)), Some(&dids(&[]))).unwrap();
        assert_eq!(out.tallies.len(), 1);
        assert!(!out.tallies[0].consensus_reached);
        assert!(out.fires.is_none());
    }

    #[test]
    fn empty_proposal_list_returns_empty_tallies_no_fire() {
        let out = aggregate_flow_votes(&[], Some(&rule(1)), None).unwrap();
        assert!(out.tallies.is_empty());
        assert!(out.fires.is_none());
    }

    #[test]
    fn defaults_to_n_1_when_rule_omitted() {
        let props = vec![proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z")];
        let out = aggregate_flow_votes(&props, None, None).unwrap();
        assert_eq!(out.tallies[0].required_count, 1);
        assert!(out.tallies[0].consensus_reached);
        assert!(out.fires.is_some());
    }

    #[test]
    fn same_did_twice_for_one_target_counts_once() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(2)), None).unwrap();
        assert_eq!(out.tallies[0].distinct_proposers, vec!["did:alice"]);
        assert!(!out.tallies[0].consensus_reached);
    }

    #[test]
    fn n_2_threshold_two_distinct_meet_one_does_not() {
        let one = vec![proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z")];
        let out = aggregate_flow_votes(&one, Some(&rule(2)), None).unwrap();
        assert!(!out.tallies[0].consensus_reached);

        let two = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:bob", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&two, Some(&rule(2)), None).unwrap();
        assert!(out.tallies[0].consensus_reached);
    }

    #[test]
    fn an_acceptor_counts_like_a_second_proposer() {
        // Design §7.2: a DID qualifies iff it has proposed OR accepted.
        // One proposal from Alice, accepted by Bob → n=2 is met without a
        // second proposal — the accept-link path.
        let mut p = proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z");
        p.acceptors = vec!["did:bob".to_string()];
        let out = aggregate_flow_votes(&[p], Some(&rule(2)), None).unwrap();
        assert_eq!(
            out.tallies[0].distinct_proposers,
            vec!["did:alice", "did:bob"]
        );
        assert!(out.tallies[0].consensus_reached);
        assert!(out.fires.is_some());
    }

    #[test]
    fn proposer_accepting_their_own_proposal_still_counts_once() {
        let mut p = proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z");
        p.acceptors = vec!["did:alice".to_string()];
        let out = aggregate_flow_votes(&[p], Some(&rule(2)), None).unwrap();
        assert_eq!(out.tallies[0].distinct_proposers, vec!["did:alice"]);
        assert!(!out.tallies[0].consensus_reached);
    }

    #[test]
    fn acceptors_pass_through_the_from_role_gate() {
        // Bob (in role) accepts Alice's (not in role) proposal: only Bob
        // counts, so n=1-with-role fires on the acceptor alone.
        let mut p = proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z");
        p.acceptors = vec!["did:bob".to_string()];
        let out =
            aggregate_flow_votes(&[p], Some(&from_role_rule(1)), Some(&dids(&["did:bob"])))
                .unwrap();
        assert_eq!(out.tallies[0].eligible_proposers, vec!["did:bob"]);
        assert!(out.tallies[0].consensus_reached);
    }

    #[test]
    fn separates_buckets_by_from_and_to_state() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "c", "did:bob", "2026-01-02T00:00:00Z"),
            proposal("b", "c", "did:carol", "2026-01-03T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        assert_eq!(out.tallies.len(), 3);
        let keys: Vec<(String, String)> = out
            .tallies
            .iter()
            .map(|t| (t.from_state.clone(), t.to_state.clone()))
            .collect();
        assert_eq!(
            keys,
            vec![
                ("a".to_string(), "b".to_string()),
                ("a".to_string(), "c".to_string()),
                ("b".to_string(), "c".to_string()),
            ],
            "tallies sorted lex by (from, to)"
        );
    }

    #[test]
    fn distinct_proposers_sorted_lex() {
        let props = vec![
            proposal("a", "b", "did:zed", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        assert_eq!(
            out.tallies[0].distinct_proposers,
            vec!["did:alice", "did:zed"]
        );
    }

    #[test]
    fn contributing_preserves_input_ordering() {
        let props = vec![
            proposal_with_uri("ad4m://p2", "a", "b", "did:bob", "2026-01-02T00:00:00Z"),
            proposal_with_uri("ad4m://p1", "a", "b", "did:alice", "2026-01-01T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        let uris: Vec<&str> = out.tallies[0]
            .contributing
            .iter()
            .map(|p| p.uri.as_str())
            .collect();
        assert_eq!(uris, vec!["ad4m://p2", "ad4m://p1"]);
    }

    #[test]
    fn only_eligible_dids_contribute_to_consensus() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:bob", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(
            &props,
            Some(&from_role_rule(2)),
            Some(&dids(&["did:alice"])),
        )
        .unwrap();
        assert_eq!(
            out.tallies[0].distinct_proposers,
            vec!["did:alice", "did:bob"]
        );
        assert_eq!(out.tallies[0].eligible_proposers, vec!["did:alice"]);
        assert!(!out.tallies[0].consensus_reached);
    }

    #[test]
    fn consensus_fails_when_only_non_role_did_proposed() {
        let props = vec![proposal("a", "b", "did:mallory", "2026-01-01T00:00:00Z")];
        let out = aggregate_flow_votes(
            &props,
            Some(&from_role_rule(1)),
            Some(&dids(&["did:alice"])),
        )
        .unwrap();
        assert!(!out.tallies[0].consensus_reached);
        assert!(out.fires.is_none());
    }

    #[test]
    fn fires_selects_earliest_proposed_at_across_tallies() {
        let props = vec![
            proposal("a", "c", "did:bob", "2026-01-05T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        let fired = out.fires.expect("one tally fires");
        assert_eq!((fired.from_state.as_str(), fired.to_state.as_str()), ("a", "b"));
    }

    #[test]
    fn fires_breaks_ties_by_lex_from_then_to() {
        let props = vec![
            proposal("a", "c", "did:bob", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        let fired = out.fires.expect("one tally fires");
        assert_eq!((fired.from_state.as_str(), fired.to_state.as_str()), ("a", "b"));
    }

    #[test]
    fn fires_none_when_no_bucket_clears_the_bar() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "c", "did:bob", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(2)), None).unwrap();
        assert!(out.fires.is_none());
    }

    #[test]
    fn does_not_mutate_input_slice_or_records() {
        let props = vec![
            proposal("a", "b", "did:zed", "2026-01-02T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
        ];
        let before = props.clone();
        let _ = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        assert_eq!(props, before);
    }

    #[test]
    fn required_count_copied_onto_every_tally() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("b", "c", "did:bob", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(3)), None).unwrap();
        assert!(out.tallies.iter().all(|t| t.required_count == 3));
    }

    #[test]
    fn without_from_role_eligible_equals_distinct() {
        let props = vec![
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:bob", "2026-01-02T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        assert_eq!(
            out.tallies[0].distinct_proposers,
            out.tallies[0].eligible_proposers
        );
    }

    // ---- select_fire_candidate ------------------------------------------

    fn aggregate_firing(from: &str, to: &str) -> AggregateFlowVotesResult {
        let props = vec![proposal(from, to, "did:alice", "2026-01-01T00:00:00Z")];
        aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap()
    }

    #[test]
    fn select_fire_candidate_returns_none_when_aggregate_has_no_fires() {
        let out = aggregate_flow_votes(&[], Some(&rule(1)), None).unwrap();
        assert!(select_fire_candidate("a", &out).is_none());
    }

    #[test]
    fn select_fire_candidate_returns_none_when_from_state_stale() {
        let out = aggregate_firing("a", "b");
        assert!(
            select_fire_candidate("b", &out).is_none(),
            "instance already advanced past `a` — votes are stale"
        );
    }

    #[test]
    fn select_fire_candidate_returns_the_tally_when_from_state_matches() {
        let out = aggregate_firing("a", "b");
        let t = select_fire_candidate("a", &out).expect("candidate");
        assert_eq!((t.from_state.as_str(), t.to_state.as_str()), ("a", "b"));
    }

    // ---- parse_flow_transition_proposal_from_hydrated -------------------

    #[test]
    fn parses_hydrated_proposal_reading_created_at_as_proposed_at() {
        // The proposal SDNA has no `proposedAt` — Ad4mModel's synthesised
        // `createdAt` is the propose time. This is the one field-mapping
        // difference from the pre-restructure port source.
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "identified",
            "toState": "scoped",
            "proposer": "did:key:alice",
            "createdAt": "2026-09-04T00:00:00Z",
            "evidenceHashes": "abc123",
        });
        let r = parse_flow_transition_proposal_from_hydrated(&v).expect("parses");
        assert_eq!(r.proposed_at, "2026-09-04T00:00:00Z");
        assert_eq!(r.evidence_hash, "abc123");
        assert!(r.acceptors.is_empty(), "acceptors come from links, not hydration");
    }

    #[test]
    fn skips_half_written_proposal_missing_created_at() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "identified",
            "toState": "scoped",
            "proposer": "did:key:alice",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    #[test]
    fn missing_evidence_hashes_defaults_to_empty_not_skip() {
        // Unverifiable ≠ unparseable: the orchestrator decides what to do
        // with a hash-less proposal (skip at fire time, fail-closed).
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "a",
            "toState": "b",
            "proposer": "did:key:alice",
            "createdAt": "2026-09-04T00:00:00Z",
        });
        let r = parse_flow_transition_proposal_from_hydrated(&v).expect("parses");
        assert_eq!(r.evidence_hash, "");
    }
}
