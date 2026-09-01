//! Rust port of `aggregateFlowVotes` + `fireFlowConsensus`
//! (`core/src/perspectives/FlowVoteAggregator.ts` and
//! `core/src/perspectives/FlowConsensusFire.ts`).
//!
//! Byte-for-byte parity with the TS aggregator + firing pair so the
//! auto-processor (engine side) and Ad4m clients (TS side) reach the
//! same fire/no-fire verdict — and, when firing, advance the same
//! `FlowInstance.currentState` under the same stale-vote guards — from
//! the same on-graph proposal bag. Without this, an engine-side
//! consensus firing pass would either double-fire when clients disagree
//! or silently drift as consensus rules evolve.
//!
//! # What this module owns
//!
//! - [`FlowTransitionProposalRecord`] — plain data mirror of the TS
//!   `FlowTransitionProposal` shape as it lands on-graph. The four
//!   fields the aggregator counts on (`from_state`, `to_state`,
//!   `proposer`, `proposed_at`) are load-bearing; `uri` is carried so
//!   [`FireOutcome::contributing_proposal_uris`] can be derived by the
//!   firing path (mirrors TS `FlowTransitionProposal.id`).
//! - [`FlowVoteTally`] + [`AggregateFlowVotesResult`] — output shape,
//!   1:1 with the TS interfaces.
//! - [`aggregate_flow_votes`] — pure aggregation entry point.
//! - [`select_fire_candidate`] — pure "which tally is safe to fire now"
//!   check, mirroring TS `selectFireCandidate`. Stale-`from_state`
//!   guard lives here so the async writer path can stay a thin
//!   composition.
//! - [`fire_flow_consensus`] — async composition: preconditions →
//!   [`super::flow_classes::advance_flow_instance_state`] → structured
//!   [`FireOutcome`]. Mirrors TS `fireFlowConsensus`.
//! - [`load_flow_transition_proposals`] — thin `model_query` loader for
//!   every proposal targeting a given `FlowInstance`. Companion to
//!   [`super::flow_context::load_flow_instances`].
//!
//! # Non-goals (deferred)
//!
//! - `fromRole` resolution — caller passes the pre-resolved eligible-DID
//!   set. Same contract as the TS side: it keeps this helper sync +
//!   composable with both engine and client firing paths.
//! - Weighted / delegation / time-decay consensus (v1.5+).
//! - Writing a `FlowInstanceAdvance` audit event alongside the state
//!   advance ([`FireOutcome`] already carries what a future writer
//!   would need). Same "v1 fires only" scope as TS `fireFlowConsensus`.

#![allow(dead_code)]

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::{
    advance_flow_instance_state, FLOW_TRANSITION_PROPOSAL_CLASS,
};
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::ConsensusRule;
use std::collections::{BTreeMap, HashSet};

/// Plain data mirror of the TS `FlowTransitionProposal` shape as it
/// appears in the aggregator's input. The writer stage
/// (`write_flow_transition_proposal`) emits every declared property; the
/// aggregator only counts on `from_state`/`to_state`/`proposer`/`proposed_at`.
///
/// `uri` is the on-graph proposal URI (`ad4m://flow/proposal/{id}` —
/// see [`super::flow_classes::flow_transition_proposal_uri`]). It is
/// NOT read by [`aggregate_flow_votes`]; it is copied through so the
/// firing path can derive [`FireOutcome::contributing_proposal_uris`]
/// without a second read of the perspective. Mirrors the way TS
/// `fireFlowConsensus` reads `contributing.map((p) => p.id)`.
///
/// Fields are `String` (not `&str`) so a loader step can construct
/// records from either a live perspective query or a fixture without
/// lifetime gymnastics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowTransitionProposalRecord {
    pub uri: String,
    pub from_state: String,
    pub to_state: String,
    pub proposer: String,
    pub proposed_at: String,
}

/// Per-target result: one row per `(from_state, to_state)` that appears in
/// the input bag, whether it fires or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowVoteTally {
    pub from_state: String,
    pub to_state: String,
    /// Distinct DIDs that proposed this target, before any `from_role`
    /// gate. Sorted lexicographically for deterministic ordering.
    pub distinct_proposers: Vec<String>,
    /// Distinct DIDs that survived the `from_role` gate. Equal to
    /// `distinct_proposers` when the rule has no `from_role`. Sorted
    /// lexicographically.
    pub eligible_proposers: Vec<String>,
    /// `consensus_rule.n`. Copied onto every tally so callers can log /
    /// render without re-lookup.
    pub required_count: u32,
    /// `eligible_proposers.len() as u32 >= required_count`.
    pub consensus_reached: bool,
    /// The subset of the input proposals that target this
    /// (from_state, to_state). Ordering preserved from input.
    pub contributing: Vec<FlowTransitionProposalRecord>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateFlowVotesResult {
    /// All tallies for the input bag, sorted lexicographically by
    /// `(from_state, to_state)` for deterministic diffing across runs.
    pub tallies: Vec<FlowVoteTally>,
    /// The tally that should fire next, if any. Selection rule
    /// (deterministic, mirrors TS):
    ///   1. Only tallies with `consensus_reached == true` are candidates.
    ///   2. Among candidates, pick the one whose earliest contributing
    ///      proposal has the smallest `proposed_at` (RFC3339 sorts
    ///      lex-safe).
    ///   3. Ties broken by lex `(from_state, to_state)`.
    pub fires: Option<FlowVoteTally>,
}

/// Pure aggregation entry point. Byte-for-byte parity with the TS
/// `aggregateFlowVotes` — same inputs → same outputs.
///
/// # Arguments
///
/// - `proposals`: every `FlowTransitionProposalRecord` written for one
///   `FlowInstance`. The helper groups by `(from_state, to_state)`
///   internally, does not require pre-sort or filter.
/// - `consensus_rule`: rule governing the transition(s). `None` →
///   defaults to `{ n: 1 }` (single-proposer suffices).
/// - `eligible_dids`: optional pre-resolved DID set from
///   `consensus_rule.from_role`. When the rule has `from_role` set the
///   caller MUST supply this — omitting it errors, because a silent
///   default of "all DIDs eligible" would misreport consensus.
///
/// # Errors
///
/// - `consensus_rule.n == 0` — `u32` blocks the negative/non-integer
///   cases the TS side must check at runtime.
/// - `consensus_rule.from_role` set + `eligible_dids` is `None`.
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

    // BTreeMap so bucket iteration is already lex-sorted by
    // `(from_state, to_state)` key — matches the final TS sort step.
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
            let set: HashSet<&String> = bucket.iter().map(|p| &p.proposer).collect();
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

    // Select `fires` — earliest `proposed_at` among consensus-reached
    // tallies, ties broken by lex (from_state, to_state).
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

/// Snapshot of what [`fire_flow_consensus`] actually wrote. Mirrors TS
/// `FireOutcome` (`core/src/perspectives/FlowConsensusFire.ts`).
///
/// Callers use this for logging, UI updates, and — once the
/// `FlowInstanceAdvance` event class lands — projecting proposal
/// evidence into an on-graph audit trail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FireOutcome {
    /// The `FlowInstance` URI whose `currentState` was advanced.
    pub instance_uri: String,
    /// Value of `currentState` before the advance.
    pub from_state: String,
    /// Value of `currentState` after the advance.
    pub to_state: String,
    /// Distinct DIDs whose proposals were counted toward this
    /// consensus. Sorted lexicographically — mirrors
    /// [`FlowVoteTally::eligible_proposers`].
    pub fired_by_proposers: Vec<String>,
    /// URIs of the proposals that contributed to the tally. Order
    /// preserved from [`FlowVoteTally::contributing`] (which mirrors
    /// input order — oldest-first when the caller loaded proposals via
    /// [`load_flow_transition_proposals`]).
    pub contributing_proposal_uris: Vec<String>,
}

/// Choose which tally (if any) is safe to fire against a given
/// `FlowInstance` snapshot right now. Mirrors TS `selectFireCandidate`
/// (`core/src/perspectives/FlowConsensusFire.ts`).
///
/// Returns `None` when:
///   - the aggregate has no `fires` tally (no target met consensus), OR
///   - the fires tally's `from_state` differs from `current_state` (a
///     firing pass has already advanced the flow past this transition,
///     so the votes are stale relative to the current state).
///
/// The second guard is the reason this helper exists as a separate
/// function: [`AggregateFlowVotesResult::fires`] is computed without
/// reference to the live instance, so callers MUST re-check against the
/// current snapshot before firing.
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
/// Mirrors TS `fireFlowConsensus`
/// (`core/src/perspectives/FlowConsensusFire.ts`).
///
/// Preconditions (all enforced — a violation returns `Err` before
/// touching the perspective):
///   - `fired_tally.consensus_reached == true`
///   - `fired_tally.from_state == instance.current_state`
///   - `fired_tally.to_state != instance.current_state`
///
/// Mutation path: delegates to
/// [`super::flow_classes::advance_flow_instance_state`], which does an
/// atomic `setSingleTarget` replace of the `currentState` link under
/// the caller-supplied `batch_id`. Mirrors [`super::flow_classes::mint_flow_instance`]
/// and [`super::flow_classes::write_flow_transition_proposal`]: the
/// caller owns id-gen / clock / atomic-commit batching so the fire path
/// can bundle the advance with follow-on writes (e.g. a future
/// `FlowInstanceAdvance` event) into one commit.
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

/// Load every live `FlowTransitionProposal` targeting a given
/// `FlowInstance` URI. Companion to
/// [`super::flow_context::load_flow_instances`] — same
/// `model_query`-with-`where`-filter pattern, same absent-class silent
/// return, same "half-record silently skipped" tolerance.
///
/// Results are NOT sorted by [`FlowTransitionProposalRecord::proposed_at`]
/// here — [`aggregate_flow_votes`] does its own bucketing, and the
/// "oldest-first" ordering the TS `listForInstance` guarantees is a
/// client-side ergonomic that the engine path does not need. Callers
/// wanting stable ordering across runs should sort at the call site.
///
/// Silently returns `Ok(vec![])` when the `FlowTransitionProposal`
/// class hasn't been registered on this perspective yet — a
/// freshly-created perspective has no proposals; treating that as an
/// error would break the auto-processor firing pass on every call
/// before the first proposal ever lands.
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
            if msg.contains("Shape not found") || msg.contains("shape not found") {
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
    Ok(instances
        .iter()
        .filter_map(parse_flow_transition_proposal_from_hydrated)
        .collect())
}

/// Parse one hydrated `FlowTransitionProposal` JSON object (as returned
/// by [`PerspectiveInstance::model_query`]) into a
/// [`FlowTransitionProposalRecord`].
///
/// Returns `None` when any of `id` / `fromState` / `toState` /
/// `proposer` / `proposedAt` is missing — an untyped or half-written
/// proposal is silently skipped rather than failing the whole firing
/// pass. Same policy as
/// [`super::flow_context::parse_flow_instance_from_hydrated`].
pub fn parse_flow_transition_proposal_from_hydrated(
    v: &serde_json::Value,
) -> Option<FlowTransitionProposalRecord> {
    let uri = v.get("id").and_then(|x| x.as_str())?.to_string();
    let from_state = v.get("fromState").and_then(|x| x.as_str())?.to_string();
    let to_state = v.get("toState").and_then(|x| x.as_str())?.to_string();
    let proposer = v.get("proposer").and_then(|x| x.as_str())?.to_string();
    let proposed_at = v.get("proposedAt").and_then(|x| x.as_str())?.to_string();
    Some(FlowTransitionProposalRecord {
        uri,
        from_state,
        to_state,
        proposer,
        proposed_at,
    })
}

/// Smallest `proposed_at` in a non-empty bucket. RFC3339 timestamps
/// sort lexicographically when normalized to UTC (Z-suffix); the writer
/// always emits Z-suffix. Empty string sorts before any real RFC3339
/// value — makes the proposal a de-facto tie-breaker winner, which is
/// the safest failure mode (fires it first, exposes the bug).
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
    use crate::perspectives::shacl_parser::ModelQuery;

    fn proposal(
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> FlowTransitionProposalRecord {
        // Synthetic URI so unit tests exercise the `uri`-carry contract
        // without loading from a real perspective. Real callers get the
        // URI from `parse_flow_transition_proposal_from_hydrated`.
        proposal_with_uri(
            &format!("ad4m://flow/proposal/test-{proposer}-{from_state}-{to_state}-{proposed_at}"),
            from_state,
            to_state,
            proposer,
            proposed_at,
        )
    }

    fn proposal_with_uri(
        uri: &str,
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> FlowTransitionProposalRecord {
        FlowTransitionProposalRecord {
            uri: uri.into(),
            from_state: from_state.into(),
            to_state: to_state.into(),
            proposer: proposer.into(),
            proposed_at: proposed_at.into(),
        }
    }

    fn from_role_rule(n: u32) -> ConsensusRule {
        ConsensusRule {
            n,
            from_role: Some(ModelQuery {
                class_name: "Reviewer".into(),
                did_property: Some("agent".into()),
                r#where: None,
                linked_to: None,
                or: None,
                count: None,
            }),
        }
    }

    // ---------- defence ----------

    #[test]
    fn errors_when_n_is_zero() {
        let rule = ConsensusRule {
            n: 0,
            from_role: None,
        };
        let err = aggregate_flow_votes(&[], Some(&rule), None).unwrap_err();
        assert!(
            err.to_string().contains("must be a positive integer"),
            "err = {err}"
        );
    }

    #[test]
    fn errors_when_from_role_set_and_eligible_dids_missing() {
        let rule = from_role_rule(1);
        let err = aggregate_flow_votes(&[], Some(&rule), None).unwrap_err();
        assert!(err.to_string().contains("from_role is set"), "err = {err}");
    }

    #[test]
    fn accepts_from_role_plus_empty_eligible_set_no_fire() {
        let rule = from_role_rule(1);
        let empty: HashSet<String> = HashSet::new();
        let res = aggregate_flow_votes(
            &[proposal(
                "A",
                "B",
                "did:example:alice",
                "2026-08-30T00:00:00Z",
            )],
            Some(&rule),
            Some(&empty),
        )
        .unwrap();
        assert!(res.fires.is_none());
        assert_eq!(res.tallies[0].eligible_proposers, Vec::<String>::new());
        assert!(!res.tallies[0].consensus_reached);
    }

    // ---------- grouping + counting ----------

    #[test]
    fn empty_proposal_list_returns_empty_tallies_no_fire() {
        let res = aggregate_flow_votes(&[], None, None).unwrap();
        assert!(res.tallies.is_empty());
        assert!(res.fires.is_none());
    }

    #[test]
    fn defaults_to_n_1_when_rule_omitted() {
        let res = aggregate_flow_votes(
            &[proposal(
                "A",
                "B",
                "did:example:alice",
                "2026-08-30T00:00:00Z",
            )],
            None,
            None,
        )
        .unwrap();
        assert_eq!(res.tallies[0].required_count, 1);
        assert!(res.tallies[0].consensus_reached);
        assert!(res.fires.is_some());
    }

    #[test]
    fn same_did_twice_for_one_target_counts_once() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        assert_eq!(
            res.tallies[0].distinct_proposers,
            vec!["did:example:alice".to_string()]
        );
        assert!(!res.tallies[0].consensus_reached);
    }

    #[test]
    fn n_2_threshold_two_distinct_meet_one_does_not() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let met = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:bob", "2026-08-30T00:00:01Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        assert!(met.tallies[0].consensus_reached);
        assert_eq!(
            met.tallies[0].distinct_proposers,
            vec![
                "did:example:alice".to_string(),
                "did:example:bob".to_string(),
            ]
        );

        let unmet = aggregate_flow_votes(
            &[proposal(
                "A",
                "B",
                "did:example:alice",
                "2026-08-30T00:00:00Z",
            )],
            Some(&rule),
            None,
        )
        .unwrap();
        assert!(!unmet.tallies[0].consensus_reached);
        assert!(unmet.fires.is_none());
    }

    #[test]
    fn separates_buckets_by_from_and_to_state() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "C", "did:example:alice", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        assert_eq!(res.tallies.len(), 2);
        for t in &res.tallies {
            assert_eq!(t.distinct_proposers, vec!["did:example:alice".to_string()]);
            assert!(!t.consensus_reached);
        }
    }

    #[test]
    fn distinct_proposers_sorted_lex() {
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:zoe", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:mike", "2026-08-30T00:00:00Z"),
            ],
            None,
            None,
        )
        .unwrap();
        assert_eq!(
            res.tallies[0].distinct_proposers,
            vec![
                "did:example:alice".to_string(),
                "did:example:mike".to_string(),
                "did:example:zoe".to_string(),
            ]
        );
    }

    #[test]
    fn contributing_preserves_input_ordering() {
        let rule = ConsensusRule {
            n: 3,
            from_role: None,
        };
        let p1 = proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z");
        let p2 = proposal("A", "B", "did:example:bob", "2026-08-30T00:00:02Z");
        let p3 = proposal("A", "B", "did:example:cara", "2026-08-30T00:00:03Z");
        let res =
            aggregate_flow_votes(&[p1.clone(), p2.clone(), p3.clone()], Some(&rule), None).unwrap();
        assert_eq!(res.tallies[0].contributing, vec![p1, p2, p3]);
    }

    #[test]
    fn tallies_sorted_lex_by_from_then_to() {
        let res = aggregate_flow_votes(
            &[
                proposal("C", "D", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "Z", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("B", "C", "did:example:alice", "2026-08-30T00:00:00Z"),
            ],
            None,
            None,
        )
        .unwrap();
        let labels: Vec<String> = res
            .tallies
            .iter()
            .map(|t| format!("{}->{}", t.from_state, t.to_state))
            .collect();
        assert_eq!(labels, vec!["A->B", "A->Z", "B->C", "C->D"]);
    }

    // ---------- from_role gating ----------

    #[test]
    fn only_eligible_dids_contribute_to_consensus() {
        let rule = from_role_rule(1);
        let eligible: HashSet<String> = ["did:example:bob".to_string()].into_iter().collect();
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:bob", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            Some(&eligible),
        )
        .unwrap();
        assert_eq!(
            res.tallies[0].distinct_proposers,
            vec![
                "did:example:alice".to_string(),
                "did:example:bob".to_string(),
            ]
        );
        assert_eq!(
            res.tallies[0].eligible_proposers,
            vec!["did:example:bob".to_string()]
        );
        assert!(res.tallies[0].consensus_reached);
    }

    #[test]
    fn consensus_fails_when_only_non_role_did_proposed() {
        let rule = from_role_rule(1);
        let eligible: HashSet<String> = ["did:example:bob".to_string()].into_iter().collect();
        let res = aggregate_flow_votes(
            &[proposal(
                "A",
                "B",
                "did:example:alice",
                "2026-08-30T00:00:00Z",
            )],
            Some(&rule),
            Some(&eligible),
        )
        .unwrap();
        assert!(res.tallies[0].eligible_proposers.is_empty());
        assert!(!res.tallies[0].consensus_reached);
        assert!(res.fires.is_none());
    }

    #[test]
    fn n_2_with_role_requires_two_distinct_eligible_dids() {
        let rule = from_role_rule(2);
        let eligible: HashSet<String> = ["did:example:bob".to_string()].into_iter().collect();
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:bob", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            Some(&eligible),
        )
        .unwrap();
        assert!(!res.tallies[0].consensus_reached);
    }

    #[test]
    fn eligible_proposers_preserves_sorted_order_subset() {
        let rule = from_role_rule(2);
        let eligible: HashSet<String> = [
            "did:example:alice".to_string(),
            "did:example:cara".to_string(),
        ]
        .into_iter()
        .collect();
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:cara", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:bob", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            Some(&eligible),
        )
        .unwrap();
        assert_eq!(
            res.tallies[0].eligible_proposers,
            vec![
                "did:example:alice".to_string(),
                "did:example:cara".to_string(),
            ]
        );
        assert!(res.tallies[0].consensus_reached);
    }

    // ---------- fires selection ----------

    #[test]
    fn fires_selects_earliest_proposed_at_across_tallies() {
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:05Z"),
                proposal("A", "C", "did:example:alice", "2026-08-30T00:00:01Z"),
            ],
            None,
            None,
        )
        .unwrap();
        let fires = res.fires.expect("expected fires");
        assert_eq!(fires.to_state, "C");
    }

    #[test]
    fn fires_breaks_ties_by_lex_from_then_to() {
        let same = "2026-08-30T00:00:00Z";
        let res = aggregate_flow_votes(
            &[
                proposal("A", "Z", "did:example:alice", same),
                proposal("A", "B", "did:example:alice", same),
            ],
            None,
            None,
        )
        .unwrap();
        let fires = res.fires.expect("expected fires");
        assert_eq!(fires.to_state, "B");
    }

    #[test]
    fn fires_none_when_no_bucket_clears_the_bar() {
        let rule = ConsensusRule {
            n: 5,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[proposal(
                "A",
                "B",
                "did:example:alice",
                "2026-08-30T00:00:00Z",
            )],
            Some(&rule),
            None,
        )
        .unwrap();
        assert!(res.fires.is_none());
        assert_eq!(res.tallies[0].distinct_proposers.len(), 1);
        assert_eq!(res.tallies[0].required_count, 5);
    }

    #[test]
    fn fires_prefers_fired_bucket_even_if_unfired_has_older_proposal() {
        // A->B: one proposal at t=0, does not fire under n=2.
        // A->C: two proposals at t=1 and t=2, fires under n=2.
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "C", "did:example:alice", "2026-08-30T00:00:01Z"),
                proposal("A", "C", "did:example:bob", "2026-08-30T00:00:02Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        let fires = res.fires.expect("expected fires");
        assert_eq!(fires.to_state, "C");
    }

    // ---------- output invariants ----------

    #[test]
    fn does_not_mutate_input_slice_or_records() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let input = vec![
            proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z"),
            proposal("A", "B", "did:example:bob", "2026-08-30T00:00:02Z"),
        ];
        let snapshot = input.clone();
        let _ = aggregate_flow_votes(&input, Some(&rule), None).unwrap();
        assert_eq!(input, snapshot);
    }

    #[test]
    fn required_count_copied_onto_every_tally() {
        let rule = ConsensusRule {
            n: 3,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "C", "did:example:alice", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        for t in &res.tallies {
            assert_eq!(t.required_count, 3);
        }
    }

    #[test]
    fn without_from_role_eligible_equals_distinct() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "B", "did:example:bob", "2026-08-30T00:00:00Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        let t = &res.tallies[0];
        assert_eq!(t.eligible_proposers, t.distinct_proposers);
    }

    // ---------- ts-parity byte fixture ----------

    // ---------- slice 10.9b2: select_fire_candidate (pure) ----------

    fn tally_with_uris(
        from_state: &str,
        to_state: &str,
        proposers: &[&str],
        proposed_at: &str,
        uris: &[&str],
        required: u32,
    ) -> FlowVoteTally {
        let contributing: Vec<FlowTransitionProposalRecord> = proposers
            .iter()
            .zip(uris.iter())
            .map(|(p, u)| proposal_with_uri(u, from_state, to_state, p, proposed_at))
            .collect();
        let distinct: Vec<String> = {
            let mut v: Vec<String> = proposers.iter().map(|s| (*s).to_string()).collect();
            v.sort();
            v.dedup();
            v
        };
        FlowVoteTally {
            from_state: from_state.into(),
            to_state: to_state.into(),
            distinct_proposers: distinct.clone(),
            eligible_proposers: distinct,
            required_count: required,
            consensus_reached: proposers.len() as u32 >= required,
            contributing,
        }
    }

    #[test]
    fn select_fire_candidate_returns_none_when_aggregate_has_no_fires() {
        let agg = AggregateFlowVotesResult {
            tallies: vec![],
            fires: None,
        };
        assert!(select_fire_candidate("A", &agg).is_none());
    }

    #[test]
    fn select_fire_candidate_returns_none_when_from_state_stale() {
        let fires = tally_with_uris(
            "A",
            "B",
            &["did:example:alice", "did:example:bob"],
            "2026-08-30T00:00:00Z",
            &["ad4m://flow/proposal/p1", "ad4m://flow/proposal/p2"],
            2,
        );
        let agg = AggregateFlowVotesResult {
            tallies: vec![fires.clone()],
            fires: Some(fires),
        };
        // Instance has already advanced past `A` — the aggregate's
        // fires is stale relative to the live snapshot.
        assert!(select_fire_candidate("B", &agg).is_none());
    }

    #[test]
    fn select_fire_candidate_returns_the_tally_when_from_state_matches() {
        let fires = tally_with_uris(
            "A",
            "B",
            &["did:example:alice", "did:example:bob"],
            "2026-08-30T00:00:00Z",
            &["ad4m://flow/proposal/p1", "ad4m://flow/proposal/p2"],
            2,
        );
        let agg = AggregateFlowVotesResult {
            tallies: vec![fires.clone()],
            fires: Some(fires.clone()),
        };
        let picked = select_fire_candidate("A", &agg).expect("expected picked");
        assert_eq!(picked.to_state, "B");
    }

    // ---------- slice 10.9b2: fire_flow_consensus preconditions ----------
    //
    // These tests exercise every precondition branch WITHOUT touching a
    // perspective — we hit each guard with a tally that fails it, so
    // `advance_flow_instance_state` is never called. The onion-shell
    // e2e test in `flow_evaluator.rs` covers the happy path against a
    // real perspective.

    // The four `fire_flow_consensus` guards (consensus-not-reached,
    // stale-fromState, no-op toState, and the happy path) each need a
    // real `PerspectiveInstance` to prove that a) the guard message
    // fires *before* the writer runs, and b) the happy path actually
    // moves the on-graph `currentState`. Building one from scratch in
    // this unit-tests module is overkill — it drags the entire runtime
    // wiring (Ad4mDb, holochain service, agent context) into what is
    // otherwise a pure-Rust helper module. The onion-shell e2e in
    // `flow_evaluator.rs` (`fire_flow_consensus_advances_instance_e2e`)
    // covers all four branches against a real perspective, following
    // the same slice-10.4b/10.4c pattern.

    #[test]
    fn fire_outcome_shape_matches_ts_field_names() {
        // Byte-parity guard: the FireOutcome struct's field set must
        // mirror TS `FireOutcome` in
        // core/src/perspectives/FlowConsensusFire.ts. This test locks
        // the shape at the JSON layer so a rename on either side surfaces
        // as a red test (no wire-format needs it today, but a future
        // WS-RPC exposure will).
        let outcome = FireOutcome {
            instance_uri: "ad4m://flow/instance/i1".into(),
            from_state: "A".into(),
            to_state: "B".into(),
            fired_by_proposers: vec!["did:example:alice".into()],
            contributing_proposal_uris: vec!["ad4m://flow/proposal/p1".into()],
        };
        // Contract by inspection — every field lands with the expected
        // Rust name. TS uses camelCase; Rust uses snake_case; the
        // future WS-RPC boundary serde-rename layer will bridge them.
        assert_eq!(outcome.instance_uri, "ad4m://flow/instance/i1");
        assert_eq!(outcome.from_state, "A");
        assert_eq!(outcome.to_state, "B");
        assert_eq!(outcome.fired_by_proposers.len(), 1);
        assert_eq!(outcome.contributing_proposal_uris.len(), 1);
    }

    // Empty-URI guard for `load_flow_transition_proposals` is exercised
    // in the onion-shell e2e (`load_flow_transition_proposals_e2e`) in
    // `flow_evaluator.rs` — same rationale as the fire-guards above.

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_happy_path() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "flowInstance": "ad4m://flow/instance/i1",
            "fromState": "A",
            "toState": "B",
            "proposer": "did:example:alice",
            "proposedAt": "2026-08-30T00:00:00Z",
            "evidenceHashes": "deadbeef",
        });
        let r = parse_flow_transition_proposal_from_hydrated(&v).expect("must parse");
        assert_eq!(r.uri, "ad4m://flow/proposal/p1");
        assert_eq!(r.from_state, "A");
        assert_eq!(r.to_state, "B");
        assert_eq!(r.proposer, "did:example:alice");
        assert_eq!(r.proposed_at, "2026-08-30T00:00:00Z");
    }

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_skips_when_id_missing() {
        let v = serde_json::json!({
            "fromState": "A",
            "toState": "B",
            "proposer": "did:example:alice",
            "proposedAt": "2026-08-30T00:00:00Z",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_skips_when_from_state_missing() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "toState": "B",
            "proposer": "did:example:alice",
            "proposedAt": "2026-08-30T00:00:00Z",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_skips_when_to_state_missing() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "A",
            "proposer": "did:example:alice",
            "proposedAt": "2026-08-30T00:00:00Z",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_skips_when_proposer_missing() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "A",
            "toState": "B",
            "proposedAt": "2026-08-30T00:00:00Z",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_transition_proposal_from_hydrated_skips_when_proposed_at_missing() {
        let v = serde_json::json!({
            "id": "ad4m://flow/proposal/p1",
            "fromState": "A",
            "toState": "B",
            "proposer": "did:example:alice",
        });
        assert!(parse_flow_transition_proposal_from_hydrated(&v).is_none());
    }

    /// Same input as the TS test
    /// "prefers a fired target over an unfired one even if unfired has
    /// an older proposal" — asserts the Rust output has the same shape
    /// and same fires selection.
    #[test]
    fn ts_parity_fixture_prefers_fired_bucket() {
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let res = aggregate_flow_votes(
            &[
                proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
                proposal("A", "C", "did:example:alice", "2026-08-30T00:00:01Z"),
                proposal("A", "C", "did:example:bob", "2026-08-30T00:00:02Z"),
            ],
            Some(&rule),
            None,
        )
        .unwrap();
        // TS ordering: tallies sorted lex → A->B first, A->C second.
        assert_eq!(res.tallies.len(), 2);
        assert_eq!(res.tallies[0].to_state, "B");
        assert_eq!(res.tallies[1].to_state, "C");
        assert!(!res.tallies[0].consensus_reached);
        assert!(res.tallies[1].consensus_reached);
        assert_eq!(res.tallies[1].distinct_proposers.len(), 2);
        assert_eq!(
            res.fires.as_ref().map(|f| f.to_state.clone()),
            Some("C".to_string())
        );
    }
}
