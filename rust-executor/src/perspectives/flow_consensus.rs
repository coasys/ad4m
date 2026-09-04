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
//! - [`resolve_role_dids`] — `fromRole` gate resolution: per-candidate
//!   membership check reusing the `requires` guard-translation layer.
//!   [`aggregate_flow_votes`] stays pure and takes the resolved set; when
//!   the rule has `from_role`, omitting the set errors, because a silent
//!   "all DIDs eligible" would misreport consensus.
//! - [`run_flow_consensus_pass`] — the orchestrator: auto-invalidation
//!   (superseded + stale-seal via
//!   [`super::flow_evaluator::recompute_evidence_hash`]), per-target rule
//!   resolution, role gates, firing, keep-and-mark.
//! - [`delete_flow_proposal`] — hard delete, shared by auto-invalidation
//!   and [`reject_flow_proposal`].
//! - [`accept_flow_proposal`] / [`reject_flow_proposal`] — the write API
//!   (§4.3 / §7): accept adds an idempotent `acceptedBy` link and runs the
//!   pass immediately; reject hard-deletes. Both refuse resolved proposals.
//!
//! # Non-goals here (later slices)
//!
//! - WS-RPC + MCP + TS surfaces over accept/reject.
//! - `flow-state-changed` subscription topics (§7).
//! - Weighted / delegation / time-decay consensus (v1.5+).

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::{
    advance_flow_instance_state, FLOW_TRANSITION_PROPOSAL_CLASS,
};
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_flow_instances, load_shacl_flows, retain_selected_flows,
    scope_subject, FlowInstanceRecord,
};
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, recompute_evidence_hash, requires_query_input, run_query,
    RequiresQueryable,
};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery, SHACLFlow};
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery, LinkStatus};
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
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
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

/// Hard-delete a proposal: every link the proposal authors hangs off its
/// URI as source (model properties, the `flowInstance` pointer, `acceptedBy`
/// edges, `resolved_as` marks), so removing the source-links removes the
/// instance. Shared by rejection and auto-invalidation — both are noise,
/// not Synergy atoms (§4.2); only FIRED proposals are kept-and-marked.
pub async fn delete_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
) -> anyhow::Result<()> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("delete_flow_proposal: get_links({proposal_uri}): {e:#}"))?;
    if links.is_empty() {
        return Ok(());
    }
    let exprs: Vec<LinkExpression> = links.into_iter().map(LinkExpression::from).collect();
    perspective.remove_links(exprs, None).await.map_err(|e| {
        anyhow::anyhow!("delete_flow_proposal: remove_links({proposal_uri}): {e:#}")
    })?;
    Ok(())
}

/// The rule governing a transition INTO `to_state`: the target state's own
/// `consensusRule` wins, else the flow-level one, else `None` (which
/// [`aggregate_flow_votes`] defaults to `{ n: 1 }`, §7.1).
fn effective_consensus_rule<'a>(flow: &'a SHACLFlow, to_state: &str) -> Option<&'a ConsensusRule> {
    flow.states
        .iter()
        .find(|s| s.name == to_state)
        .and_then(|s| s.consensus_rule.as_ref())
        .or(flow.consensus_rule.as_ref())
}

/// One consensus sweep over the `FlowInstance`s in scope — firing-engine
/// design §2, the pass that CONSUMES proposals. Called from the
/// auto-processor after the proposal pass and from the accept/reject API
/// (so a human click resolves immediately).
///
/// Per instance (processed in sorted-URI order, at most ONE firing each —
/// a fire changes `currentState`, so sibling groups re-validate next pass
/// instead of firing on stale state):
///
/// 1. superseded proposals (`fromState` ≠ live `currentState`) are deleted
///    (auto-invalidation trigger a);
/// 2. each sealed proposal's evidence is re-verified via
///    [`recompute_evidence_hash`] with the PROPOSER's DID — hash mismatch or
///    nothing-verifiable deletes it (trigger b), a transient store error
///    skips the whole instance this pass WITHOUT invalidating (fail
///    closed); an unsealed proposal (empty hash — e.g. a future manual
///    proposal) passes through unverified;
/// 3. survivors are grouped by `toState` — each target may carry its own
///    `consensusRule` override — `fromRole` gates resolve via
///    [`resolve_role_dids`] over that group's qualifying DIDs (role
///    resolution errors also skip the instance, fail closed);
/// 4. among groups whose threshold is met, the earliest-proposed fires:
///    [`fire_flow_consensus`] advances `currentState`, then
///    [`resolve_proposals_fired`] keep-and-marks the contributing
///    proposals.
///
/// Never fails: any error is logged and skips the narrowest safe unit
/// (target group < instance < pass). Client `flow-state-changed`
/// subscriptions are a later slice (§7) — [`FireOutcome`]s are returned so
/// the wiring can emit once the topic plumbing exists. Explicit per-state
/// history entries are deliberately absent: kept-and-marked proposals
/// (proposer set, evidence, timestamps) reconstruct the full transition
/// history already.
pub async fn run_flow_consensus_pass(
    perspective: &mut PerspectiveInstance,
    scope: Option<&Scope>,
    context: &AgentContext,
    flow_filter: Option<&[String]>,
) -> Vec<FireOutcome> {
    let loaded = async {
        let mut flows_by_uri = load_shacl_flows(perspective).await?;
        retain_selected_flows(&mut flows_by_uri, flow_filter);
        let records = match scope {
            Some(s) => load_flow_instances(perspective, &[scope_subject(s).to_string()]).await?,
            None => load_all_flow_instances(perspective).await?,
        };
        anyhow::Ok((flows_by_uri, records))
    }
    .await;
    let (flows_by_uri, mut records) = match loaded {
        Ok(l) => l,
        Err(e) => {
            log::warn!("run_flow_consensus_pass: load failed: {e:#}");
            return Vec::new();
        }
    };
    records.sort_by(|a, b| a.instance_uri.cmp(&b.instance_uri));

    let mut outcomes = Vec::new();
    'instances: for record in &records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        let proposals = match load_flow_transition_proposals(perspective, &record.instance_uri)
            .await
        {
            Ok(p) => p,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: proposal load for {} failed; skipping instance: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        if proposals.is_empty() {
            continue;
        }

        // Trigger (a): superseded — the flow moved on under these proposals.
        let (live, superseded): (Vec<_>, Vec<_>) = proposals
            .into_iter()
            .partition(|p| p.from_state == record.current_state);
        for p in &superseded {
            log::debug!(
                "run_flow_consensus_pass: invalidating superseded proposal {} ({} → {}, instance now at {})",
                p.uri, p.from_state, p.to_state, record.current_state
            );
            if let Err(e) = delete_flow_proposal(perspective, &p.uri).await {
                log::warn!("run_flow_consensus_pass: {e:#}");
            }
        }

        // Trigger (b): stale seals.
        let mut verified: Vec<FlowTransitionProposalRecord> = Vec::with_capacity(live.len());
        for p in live {
            if p.evidence_hash.is_empty() {
                log::debug!(
                    "run_flow_consensus_pass: proposal {} carries no evidence seal; counting unverified",
                    p.uri
                );
                verified.push(p);
                continue;
            }
            match recompute_evidence_hash(perspective, flow, record, &p.to_state, &p.proposer).await
            {
                Ok(Some(h)) if h == p.evidence_hash => verified.push(p),
                Ok(_) => {
                    log::warn!(
                        "run_flow_consensus_pass: evidence for proposal {} no longer verifies against the live graph; invalidating",
                        p.uri
                    );
                    if let Err(e) = delete_flow_proposal(perspective, &p.uri).await {
                        log::warn!("run_flow_consensus_pass: {e:#}");
                    }
                }
                Err(e) => {
                    log::warn!(
                        "run_flow_consensus_pass: evidence re-verify for {} failed transiently; skipping instance this pass: {e:#}",
                        record.instance_uri
                    );
                    continue 'instances;
                }
            }
        }
        if verified.is_empty() {
            continue;
        }

        // Group by target state — each may carry its own rule — and pick
        // the earliest-proposed fire candidate across groups.
        let mut groups: BTreeMap<String, Vec<FlowTransitionProposalRecord>> = BTreeMap::new();
        for p in verified {
            groups.entry(p.to_state.clone()).or_default().push(p);
        }
        let mut fire: Option<(String, FlowVoteTally)> = None;
        for (to_state, bucket) in &groups {
            let rule = effective_consensus_rule(flow, to_state);
            let eligible = match rule.and_then(|r| r.from_role.as_ref()) {
                Some(role) => {
                    let mut candidates: Vec<String> = bucket
                        .iter()
                        .flat_map(|p| p.qualifying_dids().cloned())
                        .collect();
                    candidates.sort();
                    candidates.dedup();
                    match resolve_role_dids(&*perspective, role, record, &candidates).await {
                        Ok(set) => Some(set),
                        Err(e) => {
                            log::warn!(
                                "run_flow_consensus_pass: fromRole resolution for {} → {to_state} failed; skipping instance this pass: {e:#}",
                                record.instance_uri
                            );
                            continue 'instances;
                        }
                    }
                }
                None => None,
            };
            let agg = match aggregate_flow_votes(bucket, rule, eligible.as_ref()) {
                Ok(a) => a,
                Err(e) => {
                    log::warn!(
                        "run_flow_consensus_pass: misconfigured consensus rule on {} → {to_state}; skipping target: {e:#}",
                        record.flow_uri
                    );
                    continue;
                }
            };
            if let Some(tally) = select_fire_candidate(&record.current_state, &agg) {
                let key = earliest_proposed_at(&tally.contributing);
                let earlier = match &fire {
                    None => true,
                    Some((best, _)) => key < *best,
                };
                if earlier {
                    fire = Some((key, tally.clone()));
                }
            }
        }

        let Some((_, tally)) = fire else {
            continue;
        };
        match fire_flow_consensus(perspective, record, &tally, None, context).await {
            Ok(outcome) => {
                if let Err(e) = resolve_proposals_fired(
                    perspective,
                    &outcome.contributing_proposal_uris,
                    context,
                )
                .await
                {
                    log::warn!(
                        "run_flow_consensus_pass: fired {} → {} but keep-and-mark failed (proposals stay live; superseded cleanup catches them next pass): {e:#}",
                        outcome.from_state, outcome.to_state
                    );
                }
                outcomes.push(outcome);
            }
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: firing {} failed: {e:#}",
                    record.instance_uri
                );
            }
        }
    }
    outcomes
}

// ---------------------------------------------------------------------------
// Accept / reject (design §7) — the Rust core the WS-RPC and MCP surfaces
// will expose in the wire slice.
// ---------------------------------------------------------------------------

/// All source-links of a LIVE proposal. `Err` when the URI carries no links
/// (nothing to accept/reject — a typo'd or already-deleted URI must not
/// succeed silently) or when the proposal is already resolved: resolved
/// proposals are the kept flow-atom record and are immutable to this API.
async fn live_proposal_links(
    perspective: &PerspectiveInstance,
    proposal_uri: &str,
) -> anyhow::Result<Vec<DecoratedLinkExpression>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("proposal lookup on {proposal_uri} failed: {e:#}"))?;
    if links.is_empty() {
        return Err(anyhow::anyhow!(
            "no FlowTransitionProposal at {proposal_uri}"
        ));
    }
    if let Some(resolution) = links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some(RESOLVED_AS_PREDICATE))
    {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} is already resolved ({}) — resolved proposals are the kept flow-atom record",
            resolution.data.target
        ));
    }
    Ok(links)
}

/// Accept a live proposal on behalf of the acting DID (spec §4.3): write an
/// `acceptedBy` link — idempotent per DID, raw-DID target exactly as the
/// loader reads acceptors — then run the consensus pass immediately, so a
/// click resolves now rather than on the next transcript (design §7).
///
/// The immediate pass runs unscoped: accepts are rare, user-initiated
/// events, and the pass is idempotent for every uninvolved instance it
/// sweeps. Returns whatever fired (possibly nothing, e.g. n not yet met).
pub async fn accept_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<Vec<FireOutcome>> {
    let links = live_proposal_links(perspective, proposal_uri).await?;
    let did = crate::agent::did_for_context(context)
        .map_err(|e| anyhow::anyhow!("accept_flow_proposal: no acting DID: {e:#}"))?;
    let already = links.iter().any(|l| {
        l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE) && l.data.target == did
    });
    if already {
        log::debug!("accept_flow_proposal: {proposal_uri} already accepted by {did}");
    } else {
        perspective
            .add_link(
                Link {
                    source: proposal_uri.to_string(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: did.clone(),
                },
                LinkStatus::Shared,
                None,
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("accept_flow_proposal: add_link failed: {e:#}"))?;
    }
    Ok(run_flow_consensus_pass(perspective, None, context, None).await)
}

/// Reject a live proposal: hard delete, per spec §4.2 — a rejected proposal
/// is noise, not a Synergy atom. Erroring on missing/resolved URIs is the
/// caller's signal that nothing was rejected.
pub async fn reject_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
) -> anyhow::Result<()> {
    live_proposal_links(perspective, proposal_uri).await?;
    delete_flow_proposal(perspective, proposal_uri).await
}

// ---------------------------------------------------------------------------
// fromRole resolution (firing-engine design §2 step 4, spec §7.2)
// ---------------------------------------------------------------------------

/// Resolve a `consensusRule.fromRole` gate into the subset of `candidates`
/// that satisfy it.
///
/// Both spec §7.2 shapes collapse into one per-candidate membership check,
/// because the tally only ever intersects the role set with the qualifying
/// DIDs (proposers + acceptors) — which the caller already holds:
///
/// - **Shape 2** (`$did`-templated query) is the direct reading: substitute
///   the candidate's DID, run the query, matched within `count` bounds →
///   eligible.
/// - **Shape 1** (`didProperty`) reuses the guard-translation rule
///   `didProperty` → `where.<prop> = candidate_did`, which asks "does a
///   role row naming this DID exist" — the membership form of "extract all
///   DIDs from the role rows".
///
/// A role query that references the DID in *neither* way cannot
/// discriminate between candidates: it is evaluated once and gates all
/// candidates together (matched → everyone eligible), with a `warn!`
/// because that is almost always a misconfigured rule.
///
/// Fail-closed: a translation error or a store/query error aborts with
/// `Err` — the caller must skip firing this pass rather than fire on a
/// wrong eligible set (design §9: store error during count/verify → no
/// fire).
pub async fn resolve_role_dids<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    role: &ModelQuery,
    record: &FlowInstanceRecord,
    candidates: &[String],
) -> anyhow::Result<HashSet<String>> {
    let did_dependent = role.did_property.is_some()
        || serde_json::to_string(role)
            .map(|s| s.contains("$did"))
            .unwrap_or(false);

    if !did_dependent {
        log::warn!(
            "resolve_role_dids: fromRole query on `{}` references neither `didProperty` nor `$did` — it cannot discriminate between DIDs and gates all candidates together",
            role.class_name
        );
        let input = requires_query_input(role, record, "")?;
        let matched = run_query(perspective, &role.class_name, &input).await?;
        return Ok(
            if cardinality_satisfied(role.count.as_ref(), matched.len()) {
                candidates.iter().cloned().collect()
            } else {
                HashSet::new()
            },
        );
    }

    let mut eligible = HashSet::new();
    for did in candidates {
        let input = requires_query_input(role, record, did)?;
        let matched = run_query(perspective, &role.class_name, &input).await?;
        if cardinality_satisfied(role.count.as_ref(), matched.len()) {
            eligible.insert(did.clone());
        }
    }
    Ok(eligible)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod role_resolution {
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

        /// Query-aware stub: a call whose JSON mentions one of
        /// `member_dids` returns `rows_per_match` instances;
        /// `unconditional_rows` (for DID-independent queries) wins over
        /// matching when set; `error` fails every call.
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

        #[tokio::test]
        async fn shape1_did_property_filters_candidates() {
            let stub = RoleStub {
                member_dids: dids(&["did:key:alice"]),
                rows_per_match: 1,
                ..Default::default()
            };
            let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
            let eligible = resolve_role_dids(
                &stub,
                &role,
                &record(),
                &dids(&["did:key:alice", "did:key:bob"]),
            )
            .await
            .unwrap();
            assert_eq!(eligible, HashSet::from(["did:key:alice".to_string()]));
            let calls = stub.calls.lock().unwrap();
            assert_eq!(calls.len(), 2, "one membership query per candidate");
            assert!(
                calls[0].contains("did:key:alice") && calls[1].contains("did:key:bob"),
                "each query carries its candidate's DID: {calls:?}"
            );
        }

        #[tokio::test]
        async fn shape2_did_token_substitutes_per_candidate() {
            let stub = RoleStub {
                member_dids: dids(&["did:key:bob"]),
                rows_per_match: 1,
                ..Default::default()
            };
            let role = role(json!({ "className": "ns://Member", "where": { "member": "$did" } }));
            let eligible = resolve_role_dids(
                &stub,
                &role,
                &record(),
                &dids(&["did:key:alice", "did:key:bob"]),
            )
            .await
            .unwrap();
            assert_eq!(eligible, HashSet::from(["did:key:bob".to_string()]));
        }

        #[tokio::test]
        async fn did_independent_role_gates_all_candidates_together() {
            let role = role(json!({ "className": "ns://Quorum", "where": { "open": true } }));
            let candidates = dids(&["did:key:alice", "did:key:bob"]);

            let stub = RoleStub {
                unconditional_rows: Some(1),
                ..Default::default()
            };
            let eligible = resolve_role_dids(&stub, &role, &record(), &candidates)
                .await
                .unwrap();
            assert_eq!(eligible.len(), 2, "non-empty match gates everyone in");
            assert_eq!(
                stub.calls.lock().unwrap().len(),
                1,
                "DID-independent query is evaluated once, not per candidate"
            );

            let empty = RoleStub {
                unconditional_rows: Some(0),
                ..Default::default()
            };
            let none = resolve_role_dids(&empty, &role, &record(), &candidates)
                .await
                .unwrap();
            assert!(none.is_empty(), "empty match gates everyone out");
        }

        #[tokio::test]
        async fn count_bounds_apply_to_role_membership() {
            let stub = RoleStub {
                member_dids: dids(&["did:key:alice"]),
                rows_per_match: 1,
                ..Default::default()
            };
            let role = role(json!({
                "className": "ns://Reviewer",
                "didProperty": "agent",
                "count": { "min": 2 }
            }));
            let eligible = resolve_role_dids(&stub, &role, &record(), &dids(&["did:key:alice"]))
                .await
                .unwrap();
            assert!(
                eligible.is_empty(),
                "one role row does not satisfy count.min = 2"
            );
        }

        #[tokio::test]
        async fn query_error_fails_closed() {
            let stub = RoleStub {
                error: Some("store down".into()),
                ..Default::default()
            };
            let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
            let err = resolve_role_dids(&stub, &role, &record(), &dids(&["did:key:alice"]))
                .await
                .unwrap_err();
            assert!(err.to_string().contains("store down"), "got {err:#}");
        }

        #[tokio::test]
        async fn untranslatable_role_query_fails_closed() {
            let stub = RoleStub::default();
            let role = role(json!({
                "className": "ns://Reviewer",
                "didProperty": "agent",
                "where": { "status": { "matches": ".*" } }
            }));
            assert!(
                resolve_role_dids(&stub, &role, &record(), &dids(&["did:key:alice"]))
                    .await
                    .is_err(),
                "a role query model_query cannot express must error, not pass everyone"
            );
        }
    }

    fn proposal(
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> FlowTransitionProposalRecord {
        proposal_with_uri(
            "ad4m://flow/proposal/p",
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
        ConsensusRule { n, from_role: None }
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
        let out = aggregate_flow_votes(&props, Some(&from_role_rule(1)), Some(&dids(&[]))).unwrap();
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
        let out = aggregate_flow_votes(&[p], Some(&from_role_rule(1)), Some(&dids(&["did:bob"])))
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
        assert_eq!(
            (fired.from_state.as_str(), fired.to_state.as_str()),
            ("a", "b")
        );
    }

    #[test]
    fn fires_breaks_ties_by_lex_from_then_to() {
        let props = vec![
            proposal("a", "c", "did:bob", "2026-01-01T00:00:00Z"),
            proposal("a", "b", "did:alice", "2026-01-01T00:00:00Z"),
        ];
        let out = aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap();
        let fired = out.fires.expect("one tally fires");
        assert_eq!(
            (fired.from_state.as_str(), fired.to_state.as_str()),
            ("a", "b")
        );
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
        assert!(
            r.acceptors.is_empty(),
            "acceptors come from links, not hydration"
        );
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
