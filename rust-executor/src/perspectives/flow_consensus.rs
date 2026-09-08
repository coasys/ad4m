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
//! - What this pass counts is a [`TransitionAtom`], built and re-verified by
//!   [`super::flow_instance`]; the state it counts them against is the fold
//!   from that module, never the `currentState` link.
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
//! # Non-goals here (later slices)
//!
//! - WS-RPC + MCP + TS surfaces over accept/reject.
//! - `flow-state-changed` subscription topics (§7).
//! - Weighted / delegation / time-decay consensus (v1.5+).

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::advance_flow_instance_state;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_flow_instances, load_shacl_flows, retain_selected_flows,
    scope_subject, FlowInstanceRecord,
};
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, recompute_evidence_hash, requires_query_input, run_query,
    RequiresQueryable,
};
use crate::perspectives::flow_instance::{
    declares_edge, effective_consensus_rule, FlowInstance, TransitionAtom, ACCEPTED_BY_PREDICATE,
    FIRED_MARK, FLOW_INSTANCE_PREDICATE, RESOLVED_AS_PREDICATE,
};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery};
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery, LinkStatus};
use std::collections::{BTreeMap, HashSet};

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
    /// The subset of input atoms targeting this pair, input order.
    pub contributing: Vec<TransitionAtom>,
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

/// Canonical four-key ordering for `fires` selection: earliest
/// `proposed_at` first, then lex `(from_state, to_state, uri)`. The same
/// keys, in the same order, that
/// [`crate::perspectives::flow_instance::fold`] replays history with, so
/// the live choice and the replay of that choice agree.
fn tally_ord(a: &FlowVoteTally, b: &FlowVoteTally) -> std::cmp::Ordering {
    earliest_proposed_at(&a.contributing)
        .cmp(&earliest_proposed_at(&b.contributing))
        .then_with(|| a.from_state.cmp(&b.from_state))
        .then_with(|| a.to_state.cmp(&b.to_state))
        .then_with(|| earliest_uri(&a.contributing).cmp(&earliest_uri(&b.contributing)))
}

/// Pure aggregation entry point.
///
/// - `proposals`: every live record for one `FlowInstance`; grouped by
///   `(from_state, to_state)` internally, no pre-sort required.
/// - `consensus_rule`: `None` defaults to `{ n: 1 }` (§7.1).
/// - `eligible_dids`: pre-resolved `from_role` result set. Required when
///   the rule carries `from_role`.
pub fn aggregate_flow_votes(
    proposals: &[TransitionAtom],
    consensus_rule: Option<&ConsensusRule>,
    eligible_dids: Option<&HashSet<String>>,
) -> anyhow::Result<AggregateFlowVotesResult> {
    const DEFAULT_RULE: ConsensusRule = ConsensusRule {
        n: 1,
        from_role: None,
    };
    let rule: &ConsensusRule = consensus_rule.unwrap_or(&DEFAULT_RULE);
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
    let mut buckets: BTreeMap<(String, String), Vec<TransitionAtom>> = BTreeMap::new();
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

    let fires = tallies
        .iter()
        .filter(|t| t.consensus_reached)
        .min_by(|a, b| tally_ord(a, b))
        .cloned();

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
/// `derived_state` is the state the fold puts this instance in — the
/// argument is the state, not the instance row, precisely so no caller can
/// accidentally hand this the `currentState` cache any peer can write.
///
/// Preconditions (all enforced — a violation returns `Err` before touching
/// the perspective): consensus reached, `from_state` matches the derived
/// state, and the transition is not a no-op. The caller owns id-gen /
/// batching so the fire can bundle with follow-on writes (proposal
/// resolution, a future audit event) into one commit.
pub async fn fire_flow_consensus(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    derived_state: &str,
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
    if fired_tally.from_state != derived_state {
        return Err(anyhow::anyhow!(
            "fire_flow_consensus: stale tally — fromState={} does not match the derived state {derived_state} (flow already advanced?)",
            fired_tally.from_state,
        ));
    }
    if fired_tally.to_state == derived_state {
        return Err(anyhow::anyhow!(
            "fire_flow_consensus: refusing to fire a no-op — toState={} equals the derived state",
            fired_tally.to_state,
        ));
    }

    let from_state = derived_state.to_string();
    let to_state = fired_tally.to_state.clone();

    // The `currentState` link is written through as a CACHE: it keeps the
    // SHACL `min_count 1` satisfied and lets a reader see the fold's answer
    // without walking the atoms. Nothing reads it back as authority.
    advance_flow_instance_state(perspective, instance_uri, &to_state, batch_id, context).await?;

    Ok(FireOutcome {
        instance_uri: instance_uri.to_string(),
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
/// `ad4m://flow/resolved_as` → `"fired"`. Marked proposals leave the
/// frontier and become the fold's candidate history, so a fired
/// transition's votes can never count twice — and, because the fold
/// re-verifies each of them, a mark somebody else wrote buys nothing.
///
/// Keep-and-mark rather than delete (the design-note deviation from §5.4):
/// a fired proposal is a co-signed flow-atom — the record Synergy's
/// Proof-of-Flow minting reads later. To flip to spec-literal delete,
/// replace the `add_link` below with proposal deletion; the loader and
/// aggregator need no change either way.
pub async fn resolve_proposals_fired(
    perspective: &mut PerspectiveInstance,
    proposal_uris: &[String],
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    for uri in proposal_uris {
        perspective
            .add_link(
                Link {
                    source: uri.clone(),
                    predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                    target: format!("literal:string:{}", urlencoding::encode(FIRED_MARK)),
                },
                LinkStatus::Shared,
                batch_id.clone(),
                context,
            )
            .await
            .map_err(|e| {
                anyhow::anyhow!("resolve_proposals_fired: add_link({uri}) failed: {e:#}")
            })?;
    }
    Ok(())
}

/// Smallest `proposed_at` in a non-empty bucket. RFC3339 Z-suffix
/// timestamps sort lexicographically. An empty string sorts before any
/// real value — a de-facto tie-breaker winner, the safest failure mode
/// (fires first, exposes the bug).
fn earliest_proposed_at(bucket: &[TransitionAtom]) -> String {
    bucket
        .iter()
        .map(|p| p.proposed_at.clone())
        .min()
        .unwrap_or_default()
}

/// Smallest proposal URI in a bucket — the last tie-break key, so two
/// buckets proposed in the same instant still order deterministically.
fn earliest_uri(bucket: &[TransitionAtom]) -> String {
    bucket
        .iter()
        .map(|p| p.uri.clone())
        .min()
        .unwrap_or_default()
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
    let links = source_links(
        perspective,
        proposal_uri,
        None,
        &format!("delete_flow_proposal: get_links({proposal_uri})"),
    )
    .await?;
    if links.is_empty() {
        return Ok(());
    }
    let exprs: Vec<LinkExpression> = links.into_iter().map(LinkExpression::from).collect();
    perspective.remove_links(exprs, None).await.map_err(|e| {
        anyhow::anyhow!("delete_flow_proposal: remove_links({proposal_uri}): {e:#}")
    })?;
    Ok(())
}

/// Drop a proposal the consensus pass has ruled out. A failed delete is
/// logged and swallowed: invalidation is best-effort cleanup, and a proposal
/// that survives it is simply re-examined — and re-ruled-out — next pass,
/// whereas aborting here would strand the instance's remaining proposals.
async fn invalidate_proposal(perspective: &mut PerspectiveInstance, uri: &str) {
    if let Err(e) = delete_flow_proposal(perspective, uri).await {
        log::warn!("run_flow_consensus_pass: {e:#}");
    }
}

/// `get_links` wrapper: source-filtered (+ optional predicate) with a
/// caller-supplied context string in the error.
async fn source_links(
    perspective: &PerspectiveInstance,
    uri: &str,
    predicate: Option<&str>,
    what: &str,
) -> anyhow::Result<Vec<DecoratedLinkExpression>> {
    perspective
        .get_links(&LinkQuery {
            source: Some(uri.to_string()),
            predicate: predicate.map(str::to_string),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("{what}: {e:#}"))
}

/// One consensus sweep over the `FlowInstance`s in scope — firing-engine
/// design §2, the pass that CONSUMES proposals. Called from the
/// auto-processor after the proposal pass and from the accept/reject API
/// (so a human click resolves immediately).
///
/// Per instance (processed in sorted-URI order, at most ONE firing each —
/// a fire changes the state, so sibling groups re-validate next pass
/// instead of firing on a stale one):
///
/// 0. the instance's state S is DERIVED
///    ([`FlowInstance::derive_state_from`]) by folding its re-verified,
///    marked-fired atoms. Every step below partitions against S, never
///    against the `currentState` link — that link is a cache any peer can
///    write, and believing it let one forged link wipe an honest frontier
///    as "superseded" or steer a fire onto an edge the flow never reached;
/// 1. superseded proposals (`fromState` ≠ S) are deleted
///    (auto-invalidation trigger a);
/// 2. each sealed proposal's evidence is re-verified via
///    [`recompute_evidence_hash`] with the PROPOSER's DID — hash mismatch or
///    nothing-verifiable deletes it (trigger b), a transient store error
///    skips the whole instance this pass WITHOUT invalidating (fail
///    closed); an EMPTY seal is invalidated too — unverifiable proposals
///    never count (manual proposals get real seals via the server-side
///    evidence-collection path, design §4);
/// 3. survivors are grouped by `toState`; a target with no DECLARED
///    `fromState → toState` transition on the flow is skipped (kept, not
///    invalidated) — only the declared graph may fire. Each target may
///    carry its own `consensusRule` override; `fromRole` gates resolve via
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
    instance_filter: Option<&str>,
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
    // `instance_filter` narrows the sweep to one FlowInstance — the accept
    // path uses it so a single user click never re-runs every guard of
    // every live proposal on the perspective (Marvin's #967 follow-up:
    // unscoped, that is remotely-triggerable amplification once accept is
    // a wire surface).
    if let Some(only) = instance_filter {
        records.retain(|r| r.instance_uri == only);
    }
    records.sort_by(|a, b| a.instance_uri.cmp(&b.instance_uri));

    let mut outcomes = Vec::new();
    'instances: for record in &records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        let instance = FlowInstance::from_record(record, flow);

        // One load of every proposal on this instance; the fold, the
        // frontier and the superseded set are all views on the same bag.
        let bag = match instance.load_atoms(perspective).await {
            Ok(bag) => bag,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: proposal load for {} failed; skipping instance: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        let state = match instance.derive_state_from(perspective, &bag).await {
            Ok(derived) => derived.state,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: deriving the state of {} failed; skipping instance this pass: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        // Everything downstream — guard translation, role resolution, the
        // fire guard — reads the derived state off this row, so no cached
        // value can reach them.
        let record = FlowInstanceRecord {
            state: state.clone(),
            ..record.clone()
        };

        // Trigger (a): superseded — the flow moved on under these proposals.
        for atom in bag.superseded(&state) {
            log::debug!(
                "run_flow_consensus_pass: invalidating superseded proposal {} ({} → {}, instance now at {state})",
                atom.uri, atom.from_state, atom.to_state
            );
            invalidate_proposal(perspective, &atom.uri).await;
        }
        // An empty seal is unverifiable and would count toward quorum with
        // zero evidence — any replica could sync such a proposal in
        // (CodeRabbit #967 CWE-345). Manual proposals get real seals through
        // the server-side evidence-collection path (design §4, Nico's
        // requirement), so nothing legitimate writes an empty one.
        for uri in bag.unsealed_live() {
            log::warn!(
                "run_flow_consensus_pass: proposal {uri} carries an empty evidence seal; invalidating"
            );
            invalidate_proposal(perspective, uri).await;
        }

        let frontier = bag.frontier(&state);
        if frontier.is_empty() {
            continue;
        }

        // Trigger (b): stale seals. Clock A — evidence is re-run against the
        // live graph for the FRONTIER only. History is never re-run, or
        // editing a task cited by a long-finished transition would unwind
        // the flow that consumed it.
        let mut verified: Vec<TransitionAtom> = Vec::with_capacity(frontier.len());
        for p in frontier {
            match recompute_evidence_hash(perspective, flow, &record, &p.to_state, &p.proposer)
                .await
            {
                Ok(Some(h)) if h == p.evidence_hash => verified.push(p),
                Ok(_) => {
                    log::warn!(
                        "run_flow_consensus_pass: evidence for proposal {} no longer verifies against the live graph; invalidating",
                        p.uri
                    );
                    invalidate_proposal(perspective, &p.uri).await;
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
        let mut groups: BTreeMap<String, Vec<TransitionAtom>> = BTreeMap::new();
        for p in verified {
            groups.entry(p.to_state.clone()).or_default().push(p);
        }
        let mut fire: Option<FlowVoteTally> = None;
        for (to_state, bucket) in &groups {
            // Only DECLARED transitions may fire (CodeRabbit #967 CWE-863):
            // the mint side already walks `flow.transitions` via
            // `reachable_next_states`, so this closes the same rule over
            // proposals that arrived any other way. The group is skipped,
            // not invalidated — a flow definition may legitimately gain the
            // edge later.
            if !declares_edge(flow, &state, to_state) {
                log::warn!(
                    "run_flow_consensus_pass: no declared transition {state} → {to_state} on {}; skipping target",
                    record.flow_uri
                );
                continue;
            }
            let rule = effective_consensus_rule(flow, to_state);
            let eligible = match rule.and_then(|r| r.from_role.as_ref()) {
                Some(role) => {
                    let mut candidates: Vec<String> = bucket
                        .iter()
                        .flat_map(|p| p.qualifying_dids().cloned())
                        .collect();
                    candidates.sort();
                    candidates.dedup();
                    match resolve_role_dids(&*perspective, role, &record, &candidates).await {
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
            if let Some(tally) = select_fire_candidate(&state, &agg) {
                let better = match &fire {
                    None => true,
                    Some(best) => tally_ord(tally, best).is_lt(),
                };
                if better {
                    fire = Some(tally.clone());
                }
            }
        }

        let Some(tally) = fire else {
            continue;
        };
        // Advance + keep-and-mark land in ONE batch: a crash or store error
        // between the two writes would otherwise turn the co-signed flow-atom
        // record into superseded proposals the next pass hard-deletes
        // (Marvin's #967 follow-up). Either both commit or the fire rolls
        // back and re-validates next pass.
        let batch_id = perspective.create_batch().await;
        let fired = async {
            let outcome = fire_flow_consensus(
                perspective,
                &record.instance_uri,
                &state,
                &tally,
                Some(batch_id.clone()),
                context,
            )
            .await?;
            resolve_proposals_fired(
                perspective,
                &outcome.contributing_proposal_uris,
                Some(batch_id.clone()),
                context,
            )
            .await?;
            anyhow::Ok(outcome)
        }
        .await;
        match fired {
            Ok(outcome) => match perspective.commit_batch(batch_id.clone(), context).await {
                Ok(_) => outcomes.push(outcome),
                Err(e) => {
                    perspective.discard_batch(&batch_id).await;
                    log::warn!(
                        "run_flow_consensus_pass: firing {} rolled back (commit_batch failed; re-validates next pass): {e:#}",
                        record.instance_uri
                    );
                }
            },
            Err(e) => {
                perspective.discard_batch(&batch_id).await;
                log::warn!(
                    "run_flow_consensus_pass: firing {} rolled back: {e:#}",
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
    let links = source_links(
        perspective,
        proposal_uri,
        None,
        &format!("proposal lookup on {proposal_uri} failed"),
    )
    .await?;
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
/// The immediate pass is scoped to the proposal's own `FlowInstance` — one
/// accept must not sweep every instance on the perspective.
///
/// Errors when the proposal is not ENGINE-VISIBLE — i.e. not an identity-
/// checked, sealed [`TransitionAtom`]. The pass only ever counts atoms, so
/// accepting a non-atom would write a vote that can never count and return
/// the same empty `Vec<FireOutcome>` as the legitimate "counted, quorum not
/// yet met" result — a silent no-op the client cannot distinguish from a
/// landed vote. The gate runs BEFORE the `acceptedBy` write so no stray
/// vote link reaches the shared graph. The proposal itself is deliberately
/// left in place: a mid-sync proposer link may still arrive and make it
/// acceptable, and a genuinely forged one stays rejectable noise
/// (`reject_flow_proposal` is not gated).
///
/// Returns whatever fired (possibly nothing, e.g. n not yet met).
pub async fn accept_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<Vec<FireOutcome>> {
    let links = live_proposal_links(perspective, proposal_uri).await?;
    let Some(instance_uri) = links.iter().find_map(|l| {
        (l.data.predicate.as_deref() == Some(FLOW_INSTANCE_PREDICATE))
            .then(|| l.data.target.clone())
    }) else {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} carries no {FLOW_INSTANCE_PREDICATE} link — not engine-visible, accept not recorded"
        ));
    };
    // The visibility gate IS the atom check, so what the client may vote on
    // and what the engine may count are one rule.
    TransitionAtom::from_links(&instance_uri, proposal_uri, &links).map_err(|reason| {
        anyhow::anyhow!(
            "proposal {proposal_uri} is not engine-visible ({reason} — a mid-sync proposal may still complete) — accept not recorded"
        )
    })?;
    let did = crate::agent::did_for_context(context)
        .map_err(|e| anyhow::anyhow!("accept_flow_proposal: no acting DID: {e:#}"))?;
    // Authorship-bound, mirroring the loader: a forged `acceptedBy` naming
    // this DID (author != target) is dropped there as a vote, so it must not
    // suppress the genuine self-authored vote here either.
    let already = links.iter().any(|l| {
        l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE)
            && l.data.target == did
            && l.author == did
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
    // Scope the immediate pass to the proposal's own FlowInstance — one
    // accept must not sweep every instance on the perspective. (The
    // visibility gate above already resolved that instance link.)
    Ok(run_flow_consensus_pass(perspective, None, context, None, Some(&instance_uri)).await)
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
/// discriminate between candidates — "I cannot determine membership" must
/// never degrade to "everyone is a member", so it is an `Err`: a `fromRole`
/// is a security rule, and a misconfigured security rule admits nobody.
/// The caller skips firing the instance and the operator sees the error.
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
    // `unwrap_or(false)` routes a serde failure into the same `Err` below —
    // "couldn't even inspect the rule" is the purest can't-determine case.
    let did_dependent = role.did_property.is_some()
        || serde_json::to_string(role)
            .map(|s| s.contains("$did"))
            .unwrap_or(false);

    if !did_dependent {
        anyhow::bail!(
            "resolve_role_dids: fromRole query on `{}` references neither `didProperty` nor `$did` — it cannot discriminate between DIDs, so membership is undeterminable; refusing to gate (fail-closed)",
            role.class_name
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
                state: "review".into(),
                cached_state: Some("review".into()),
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

        /// `(name, role query, member DIDs, candidates, expected eligible set,
        /// expected query count)`. `Some(n)` in the last column asserts that
        /// `n` membership queries ran — one per candidate, in order, each
        /// carrying that candidate's own DID.
        #[tokio::test]
        #[rustfmt::skip]
        async fn resolve_role_dids_success_cases() {
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
                let eligible = resolve_role_dids(&stub, &role(role_json), &record(), &dids(candidates))
                    .await
                    .unwrap();
                assert_eq!(eligible, dids(expected).into_iter().collect::<HashSet<_>>(), "{name}");

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

        #[tokio::test]
        async fn did_independent_role_is_an_error_not_allow_all() {
            // A fromRole that can't discriminate between DIDs must never
            // degrade to "everyone passes" — it errors, the caller skips
            // firing, and the operator finds the misconfigured rule.
            let role = role(json!({ "className": "ns://Quorum", "where": { "open": true } }));
            let candidates = dids(&["did:key:alice", "did:key:bob"]);

            let stub = RoleStub {
                unconditional_rows: Some(1),
                ..Default::default()
            };
            let err = resolve_role_dids(&stub, &role, &record(), &candidates)
                .await
                .expect_err("DID-independent role query must fail closed");
            assert!(
                err.to_string().contains("cannot discriminate"),
                "unexpected error: {err:#}"
            );
            assert!(
                stub.calls.lock().unwrap().is_empty(),
                "no query should run for an undeterminable rule"
            );
        }

        /// Both role-resolution failure modes must fail closed — error out,
        /// never degrade to "everyone passes".
        #[tokio::test]
        #[rustfmt::skip]
        async fn resolve_role_dids_error_cases() {
            let cases: Vec<(&str, RoleStub, Value, &str)> = vec![
                ("a store error must error, not pass everyone",
                 RoleStub { error: Some("store down".into()), ..Default::default() },
                 json!({ "className": "ns://Reviewer", "didProperty": "agent" }), "store down"),
                ("a role query model_query cannot express must error, not pass everyone",
                 RoleStub::default(),
                 json!({ "className": "ns://Reviewer", "didProperty": "agent",
                         "where": { "status": { "matches": ".*" } } }), ""),
            ];

            for (name, stub, role_json, expect_contains) in cases {
                let err = resolve_role_dids(&stub, &role(role_json), &record(), &dids(&["did:key:alice"]))
                    .await
                    .expect_err(name);
                assert!(err.to_string().contains(expect_contains), "{name}: got {err:#}");
            }
        }
    }

    fn proposal(
        from_state: &str,
        to_state: &str,
        proposer: &str,
        proposed_at: &str,
    ) -> TransitionAtom {
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
    ) -> TransitionAtom {
        TransitionAtom {
            uri: uri.to_string(),
            from_state: from_state.to_string(),
            to_state: to_state.to_string(),
            proposer: proposer.to_string(),
            proposed_at: proposed_at.to_string(),
            acceptors: Vec::new(),
            evidence_hash: "hash".to_string(),
            marked_fired: false,
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

    fn accepted(mut p: TransitionAtom, acceptors: &[&str]) -> TransitionAtom {
        p.acceptors = acceptors.iter().map(|s| s.to_string()).collect();
        p
    }

    const T1: &str = "2026-01-01T00:00:00Z";
    const T2: &str = "2026-01-02T00:00:00Z";

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

    /// The whole `aggregate_flow_votes` family in one table: every row is
    /// the same call shape asserting the same five things, so the rows *are*
    /// the differences between the cases. `required_count` is derived from
    /// `rule` and checked on every tally, which also pins the §7.1 `n = 1`
    /// default when `rule` is `None`.
    ///
    /// `(name, props, rule, eligible_dids, tally_count, distinct_proposers,
    /// eligible_proposers, consensus_reached, fires)` — the four expectations
    /// after `tally_count` are read off `tallies[0]`.
    type TallyCase = (
        &'static str,
        Vec<TransitionAtom>,
        Option<ConsensusRule>,
        Option<HashSet<String>>,
        usize,
        &'static [&'static str],
        &'static [&'static str],
        bool,
        bool,
    );

    #[test]
    #[rustfmt::skip]
    fn aggregate_flow_votes_tally_cases() {
        let cases: Vec<TallyCase> = vec![
            ("from_role with an empty eligible set never fires",
             vec![proposal("a", "b", "did:alice", T1)], Some(from_role_rule(1)), Some(dids(&[])),
             1, &["did:alice"], &[], false, false),
            ("rule omitted defaults to n = 1",
             vec![proposal("a", "b", "did:alice", T1)], None, None,
             1, &["did:alice"], &["did:alice"], true, true),
            ("the same DID twice for one target counts once",
             vec![proposal("a", "b", "did:alice", T1), proposal("a", "b", "did:alice", T2)],
             Some(rule(2)), None,
             1, &["did:alice"], &["did:alice"], false, false),
            ("n = 2: one distinct DID does not meet the threshold",
             vec![proposal("a", "b", "did:alice", T1)], Some(rule(2)), None,
             1, &["did:alice"], &["did:alice"], false, false),
            ("n = 2: two distinct DIDs meet the threshold",
             vec![proposal("a", "b", "did:alice", T1), proposal("a", "b", "did:bob", T2)],
             Some(rule(2)), None,
             1, &["did:alice", "did:bob"], &["did:alice", "did:bob"], true, true),
            // Design §7.2: a DID qualifies iff it has proposed OR accepted. One
            // proposal from Alice, accepted by Bob → n=2 is met without a second
            // proposal — the accept-link path.
            ("an acceptor counts like a second proposer",
             vec![accepted(proposal("a", "b", "did:alice", T1), &["did:bob"])], Some(rule(2)), None,
             1, &["did:alice", "did:bob"], &["did:alice", "did:bob"], true, true),
            ("a proposer accepting their own proposal still counts once",
             vec![accepted(proposal("a", "b", "did:alice", T1), &["did:alice"])], Some(rule(2)), None,
             1, &["did:alice"], &["did:alice"], false, false),
            // Bob (in role) accepts Alice's (not in role) proposal: only Bob
            // counts, so n=1-with-role fires on the acceptor alone.
            ("acceptors pass through the from_role gate",
             vec![accepted(proposal("a", "b", "did:alice", T1), &["did:bob"])],
             Some(from_role_rule(1)), Some(dids(&["did:bob"])),
             1, &["did:alice", "did:bob"], &["did:bob"], true, true),
            ("distinct_proposers is sorted lexicographically",
             vec![proposal("a", "b", "did:zed", T1), proposal("a", "b", "did:alice", T2)],
             Some(rule(1)), None,
             1, &["did:alice", "did:zed"], &["did:alice", "did:zed"], true, true),
            ("only eligible DIDs contribute to consensus",
             vec![proposal("a", "b", "did:alice", T1), proposal("a", "b", "did:bob", T2)],
             Some(from_role_rule(2)), Some(dids(&["did:alice"])),
             1, &["did:alice", "did:bob"], &["did:alice"], false, false),
            ("consensus fails when only a non-role DID proposed",
             vec![proposal("a", "b", "did:mallory", T1)],
             Some(from_role_rule(1)), Some(dids(&["did:alice"])),
             1, &["did:mallory"], &[], false, false),
            ("required_count is copied onto every tally",
             vec![proposal("a", "b", "did:alice", T1), proposal("b", "c", "did:bob", T2)],
             Some(rule(3)), None,
             2, &["did:alice"], &["did:alice"], false, false),
            ("without from_role, eligible_proposers equals distinct_proposers",
             vec![proposal("a", "b", "did:alice", T1), proposal("a", "b", "did:bob", T2)],
             Some(rule(1)), None,
             1, &["did:alice", "did:bob"], &["did:alice", "did:bob"], true, true),
        ];

        for (name, props, rule, eligible, tally_count, distinct, eligible_out, reached, fires) in cases {
            let required = rule.as_ref().map_or(1, |r| r.n);
            let out = aggregate_flow_votes(&props, rule.as_ref(), eligible.as_ref())
                .unwrap_or_else(|e| panic!("{name}: {e:#}"));
            assert_eq!(out.tallies.len(), tally_count, "{name}: tally count");
            assert!(out.tallies.iter().all(|t| t.required_count == required),
                    "{name}: required_count must be {required} on every tally: {:?}", out.tallies);
            let t = &out.tallies[0];
            assert_eq!(t.distinct_proposers, distinct, "{name}: distinct_proposers");
            assert_eq!(t.eligible_proposers, eligible_out, "{name}: eligible_proposers");
            assert_eq!(t.consensus_reached, reached, "{name}: consensus_reached");
            assert_eq!(out.fires.is_some(), fires, "{name}: fires");
        }
    }

    #[test]
    fn empty_proposal_list_returns_empty_tallies_no_fire() {
        let out = aggregate_flow_votes(&[], Some(&rule(1)), None).unwrap();
        assert!(out.tallies.is_empty());
        assert!(out.fires.is_none());
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

    /// Which tally `fires`: earliest `proposed_at` first, ties broken lex by
    /// `(from_state, to_state)`, and nothing at all when no bucket reaches
    /// its threshold. Rows are `(name, props, rule.n, expected (from, to))`.
    #[test]
    #[rustfmt::skip]
    fn fires_picks_earliest_then_lex_lowest_reached_tally() {
        let cases: Vec<(&str, Vec<TransitionAtom>, u32, Option<(&str, &str)>)> = vec![
            ("earliest proposed_at across tallies wins",
             vec![proposal("a", "c", "did:bob", "2026-01-05T00:00:00Z"),
                  proposal("a", "b", "did:alice", T1)], 1, Some(("a", "b"))),
            ("equal proposed_at breaks lex by (from, to)",
             vec![proposal("a", "c", "did:bob", T1),
                  proposal("a", "b", "did:alice", T1)], 1, Some(("a", "b"))),
            ("no bucket clears the bar: nothing fires",
             vec![proposal("a", "b", "did:alice", T1),
                  proposal("a", "c", "did:bob", T2)], 2, None),
        ];

        for (name, props, n, expected) in cases {
            let out = aggregate_flow_votes(&props, Some(&rule(n)), None).unwrap();
            let fired = out.fires.map(|f| (f.from_state, f.to_state));
            assert_eq!(fired.as_ref().map(|(f, t)| (f.as_str(), t.as_str())), expected, "{name}");
        }
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

    // ---- select_fire_candidate ------------------------------------------

    fn aggregate_firing(from: &str, to: &str) -> AggregateFlowVotesResult {
        let props = vec![proposal(from, to, "did:alice", T1)];
        aggregate_flow_votes(&props, Some(&rule(1)), None).unwrap()
    }

    #[test]
    #[rustfmt::skip]
    fn select_fire_candidate_gates_on_the_derived_state() {
        let no_fires = aggregate_flow_votes(&[], Some(&rule(1)), None).unwrap();
        let firing = aggregate_firing("a", "b");
        let cases = [
            ("the aggregate has no fires", "a", &no_fires, None),
            ("flow already advanced past `a` — votes are stale", "b", &firing, None),
            ("from_state matches the derived state", "a", &firing, Some(("a", "b"))),
        ];

        for (name, derived_state, out, expected) in cases {
            let picked = select_fire_candidate(derived_state, out)
                .map(|t| (t.from_state.as_str(), t.to_state.as_str()));
            assert_eq!(picked, expected, "{name}");
        }
    }
}
