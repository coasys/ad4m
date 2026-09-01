//! Async layer (slice 10.4a2) over `model_query`.
//!
//! - [`RequiresQueryable`] — the one perspective-side call the evaluator
//!   needs, factored behind a trait so tests can stub it without a live
//!   `PerspectiveInstance`. `PerspectiveInstance` gets a blanket impl.
//! - [`evaluate_single_query`] — one `model_query` call + cardinality
//!   check + evidence extraction.
//! - [`evaluate_state_requires`] — AND across a state's `requires` array;
//!   returns `Some((class_names, evidence_ids))` when all guards match.
//! - [`evaluate_flow_transitions`] — the top composer that walks every
//!   active flow's reachable next-states and returns
//!   `Vec<SatisfiedTransition>`. Silent-skip on unknown flow name,
//!   guardless states, and query errors so a single bad shape cannot
//!   poison the whole pass.

#![allow(dead_code)]

use super::primitives::{
    build_query_input_for_requires, cardinality_satisfied, evidence_hash, SatisfiedTransition,
};
use crate::perspectives::flow_context::{reachable_next_states, FlowInstanceRecord};
use crate::perspectives::shacl_parser::{ModelQuery, SHACLFlow};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::{HashMap, HashSet};

/// The one perspective-side call the evaluator needs. Trait-based so tests
/// can stub `model_query` deterministically without spinning up a full
/// `PerspectiveInstance` (SPARQL store, Prolog engine, SDNA resolver,
/// shape cache). `PerspectiveInstance` gets a blanket impl below so
/// slice 10.4a3's call-site in `run.rs` can pass `&perspective` verbatim.
///
/// The return contract mirrors `PerspectiveInstance::model_query`: a JSON
/// string of `{ instances: [...], totalCount: N }`. The evaluator only
/// reads `instances[*].id` off the parsed shape — anything richer we'd
/// wire in when a `requires` sub-query needs it.
#[async_trait]
pub trait RequiresQueryable: Send + Sync {
    async fn model_query(
        &self,
        class_name: &str,
        query_json: &str,
    ) -> Result<String, deno_core::anyhow::Error>;
}

#[async_trait]
impl RequiresQueryable for crate::perspectives::perspective_instance::PerspectiveInstance {
    async fn model_query(
        &self,
        class_name: &str,
        query_json: &str,
    ) -> Result<String, deno_core::anyhow::Error> {
        // Delegate to the inherent method — same signature, no impl gymnastics.
        crate::perspectives::perspective_instance::PerspectiveInstance::model_query(
            self, class_name, query_json,
        )
        .await
    }
}

/// Evaluate one `ModelQuery` against the live perspective.
///
/// Returns `(satisfied, evidence_ids)`:
/// - `satisfied` folds the cardinality check ([`cardinality_satisfied`])
///   over the number of matched instances — the caller does not have to
///   re-check `query.count`.
/// - `evidence_ids` is every instance URI the query matched, in the
///   perspective's returned order. Kept even when `!satisfied` so the
///   caller can log or reason about near-misses.
///
/// Errors bubble up when `model_query` fails or when its result is not
/// the documented `{ instances: [...], totalCount: N }` shape.
/// Slice 10.4a3 wraps this in a `debug!` + skip so one malformed shape
/// registration cannot poison the whole post-processing pass.
pub async fn evaluate_single_query<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    query: &ModelQuery,
    acting_did: &str,
) -> Result<(bool, Vec<String>), deno_core::anyhow::Error> {
    let input = build_query_input_for_requires(query, acting_did);
    let input_str = serde_json::to_string(&input)?;
    let result_str = perspective
        .model_query(&query.class_name, &input_str)
        .await?;
    let result_json: Value = serde_json::from_str(&result_str).map_err(|e| {
        deno_core::anyhow::anyhow!(
            "model_query for `{}` returned non-JSON payload: {}",
            query.class_name,
            e
        )
    })?;
    let instances = result_json
        .get("instances")
        .and_then(|v| v.as_array())
        .ok_or_else(|| {
            deno_core::anyhow::anyhow!(
                "model_query for `{}` result missing `instances` array (got: {})",
                query.class_name,
                result_json
            )
        })?;
    let ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst.get("id").and_then(|v| v.as_str()).map(str::to_string))
        .collect();
    let satisfied = cardinality_satisfied(query.count.as_ref(), ids.len());
    Ok((satisfied, ids))
}

/// AND across the `requires` array — every guard in the array must be
/// satisfied for a state's `requires` to hold.
///
/// Returns:
/// - `Ok(None)` — at least one guard failed. Short-circuits: the caller
///   does not need to distinguish "which one" for the deterministic
///   post-processing pass; slice 10.5's `semanticCheck` layer only fires
///   when this returns `Some(_)`.
/// - `Ok(Some((class_names, evidence_ids)))` — every guard was satisfied.
///   `class_names` is deduplicated and preserves first-occurrence order;
///   `evidence_ids` is the union of every satisfied guard's matches,
///   deduplicated globally. The pair is what feeds [`evidence_hash`] so
///   the seal on a `SatisfiedTransition` covers all evidence used.
///
/// Errors bubble up when any single query errors — same rationale as
/// [`evaluate_single_query`]: a query surface should not be silently
/// swallowed at the state level; slice 10.4a3 handles skip-on-error
/// at the top composer.
pub async fn evaluate_state_requires<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    requires: &[ModelQuery],
    acting_did: &str,
) -> Result<Option<(Vec<String>, Vec<String>)>, deno_core::anyhow::Error> {
    let mut class_names: Vec<String> = Vec::new();
    let mut evidence_ids: Vec<String> = Vec::new();
    let mut seen_ids: HashSet<String> = HashSet::new();
    let mut seen_classes: HashSet<String> = HashSet::new();
    for query in requires {
        let (satisfied, ids) = evaluate_single_query(perspective, query, acting_did).await?;
        if !satisfied {
            return Ok(None);
        }
        if seen_classes.insert(query.class_name.clone()) {
            class_names.push(query.class_name.clone());
        }
        for id in ids {
            if seen_ids.insert(id.clone()) {
                evidence_ids.push(id);
            }
        }
    }
    Ok(Some((class_names, evidence_ids)))
}

/// Walk every active flow record's reachable next-states, evaluate each
/// state's `requires`, and return a [`SatisfiedTransition`] per satisfied
/// (record, next-state) pair.
///
/// Silent-skip rules mirror slice 10.1b (`load_flow_instances`) — the
/// deterministic post-processing pass should never blow up because *one*
/// flow definition or SDNA class went sideways:
///
/// - Record whose `flow_name` is not in `flows_by_name` → skipped
///   (definition unpublished or hasn't synced yet).
/// - State whose `requires` is `None` or empty → skipped (no
///   deterministic guard; slice 10.5's `semanticCheck` picks these up
///   separately when 10.5 lands).
/// - A `model_query` call that errors → logged at `debug!` and skipped.
///   The consensus engine (slice 10.6) will re-evaluate on the next tick
///   so a transient perspective error is self-healing.
///
/// The effective `consensus_rule` per output prefers the per-state
/// override (§7.1) and falls back to the flow-level default when unset.
pub async fn evaluate_flow_transitions<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    records: &[FlowInstanceRecord],
    flows_by_name: &HashMap<String, SHACLFlow>,
    acting_did: &str,
) -> Vec<SatisfiedTransition> {
    let mut out = Vec::new();
    for record in records {
        let Some(flow) = flows_by_name.get(&record.flow_name) else {
            continue;
        };
        for state in reachable_next_states(flow, &record.current_state) {
            let Some(requires) = state.requires.as_deref() else {
                continue;
            };
            if requires.is_empty() {
                continue;
            }
            match evaluate_state_requires(perspective, requires, acting_did).await {
                Ok(None) => {}
                Ok(Some((class_names, evidence_ids))) => {
                    let hash = evidence_hash(&class_names, &evidence_ids);
                    let effective_consensus = state
                        .consensus_rule
                        .clone()
                        .or_else(|| flow.consensus_rule.clone());
                    out.push(SatisfiedTransition {
                        flow_name: record.flow_name.clone(),
                        instance_uri: record.instance_uri.clone(),
                        subject: record.subject.clone(),
                        from_state: record.current_state.clone(),
                        to_state: state.name.clone(),
                        evidence_ids,
                        evidence_hash: hash,
                        semantic_check: state.semantic_check.clone(),
                        consensus_rule: effective_consensus,
                    });
                }
                Err(e) => {
                    log::debug!(
                        "flow evaluator: model_query failed for {}.{} on {}: {:#}",
                        record.flow_name,
                        state.name,
                        record.instance_uri,
                        e
                    );
                }
            }
        }
    }
    out
}
