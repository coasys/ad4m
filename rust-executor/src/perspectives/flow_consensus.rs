//! Slice 10.9a — pure Rust port of `aggregateFlowVotes`
//! (`core/src/perspectives/FlowVoteAggregator.ts`).
//!
//! Byte-for-byte parity with the TS aggregator so the auto-processor
//! (engine side) and Ad4m clients (TS side) reach the same fire/no-fire
//! verdict from the same on-graph proposal bag. Without this, an
//! engine-side consensus firing pass (slice 10.9b) would either
//! double-fire when clients disagree or silently drift as consensus
//! rules evolve.
//!
//! # What this module owns
//!
//! - [`FlowTransitionProposalRecord`] — plain data mirror of the TS
//!   `FlowTransitionProposal` shape as it lands on-graph. Only the four
//!   fields the aggregator counts on (`from_state`, `to_state`,
//!   `proposer`, `proposed_at`) are load-bearing; the writer in
//!   [`super::flow_classes::write_flow_transition_proposal`] emits every
//!   field, but the aggregator doesn't rehydrate them.
//! - [`FlowVoteTally`] + [`AggregateFlowVotesResult`] — output shape,
//!   1:1 with the TS interfaces.
//! - [`aggregate_flow_votes`] — pure aggregation entry point.
//!
//! # Non-goals (deferred)
//!
//! - `fromRole` resolution — caller passes the pre-resolved eligible-DID
//!   set. Same contract as the TS side: it keeps this helper sync +
//!   composable with both engine and client firing paths (design §7.2).
//! - Weighted / delegation / time-decay consensus (design §7.5 v1.5+).
//! - Actually firing the transition (slice 10.9b: `fire_flow_consensus`).
//! - Loading proposals from the perspective (queued, own slice).

#![allow(dead_code)]

use crate::perspectives::shacl_parser::ConsensusRule;
use std::collections::{BTreeMap, HashSet};

/// Plain data mirror of the TS `FlowTransitionProposal` shape as it
/// appears in the aggregator's input. The writer stage
/// (`write_flow_transition_proposal`) emits every declared property; the
/// aggregator only counts on the four below.
///
/// Fields are `String` (not `&str`) so a loader step can construct
/// records from either a live perspective query or a fixture without
/// lifetime gymnastics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowTransitionProposalRecord {
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
///   defaults to `{ n: 1 }` per design §7.1.
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

    // Select `fires` per §7.1 rule — earliest `proposed_at` among
    // consensus-reached tallies, ties broken by lex (from_state, to_state).
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
        FlowTransitionProposalRecord {
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
