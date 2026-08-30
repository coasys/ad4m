//! Slice 10.4a of the flow-implementation arc — the deterministic
//! `FlowTransitionProposal` post-processing pass. Turns each active
//! `FlowInstance` and its reachable next-states into a
//! [`SatisfiedTransition`] per (record, next-state) whose `requires` array
//! is fully satisfied against the committed perspective graph.
//!
//! Design authority: `planning/flow-interpretation-hints-design.md` §5 step 5
//! ("Post-processing (engine, deterministic)") and §7 (`ConsensusRule` +
//! `didProperty` role-gate).
//!
//! # What this module owns
//!
//! Pure primitives (slice 10.4a1):
//!
//! - [`SatisfiedTransition`] — the record slice 10.4b's writer stage
//!   consumes.
//! - [`build_query_input_for_requires`] — translator from `ModelQuery`
//!   (flow-side type) to `serde_json::Value` (`model_query`'s input
//!   shape). Substitutes `$did` in `didProperty` at translation time.
//!   Recursive over `ModelQuery.or`.
//! - [`cardinality_satisfied`] — `count.{min,max}` cardinality check.
//! - [`evidence_hash`] — deterministic SHA256 of a (class, sorted
//!   matched-ids) pair. Used to seed the evidence field on the
//!   `FlowTransitionProposal` that slice 10.4b emits, so a re-verification
//!   pass in slice 10.6 can catch a tampered proposal.
//!
//! Async layer (slice 10.4a2, this commit):
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
//!
//! # Why pure primitives + trait-backed async layer
//!
//! Slice 10.4b will emit `FlowTransitionProposal` writes on behalf of the
//! extraction DID from these results. Any bug in the ModelQuery→ModelQueryInput
//! translation would either miss a satisfied requires (flow silently
//! stalls) or synthesize a wrong-guard proposal (garbage in the flow's
//! evidence chain). Isolating the translation from graph I/O gives us
//! fixture-driven unit tests for every `PropertyCondition` variant +
//! `$did` substitution; the [`RequiresQueryable`] trait gives us the same
//! coverage for the composition and error-handling shape without paying
//! the cost of a live perspective per test.

#![allow(dead_code)]

use crate::perspectives::flow_context::{reachable_next_states, FlowInstanceRecord};
use crate::perspectives::shacl_parser::ConsensusRule;
use crate::perspectives::shacl_parser::{
    ModelQuery, ModelQueryCount, PropertyCondition, SHACLFlow,
};
use async_trait::async_trait;
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};

/// One (flow_instance, next-state) pair whose `requires` array has been
/// evaluated to fully-satisfied on the committed perspective graph. The
/// output of slice 10.4a2's async evaluator; the input to slice 10.4b's
/// [`synthesize_engine_proposals`].
///
/// `evidence_ids` is the union of matched instance IDs across every
/// `ModelQuery` in the state's `requires` array. `evidence_hash` is a
/// content-hash of the same set (computed via [`evidence_hash`]) so a
/// re-verification pass in slice 10.6 can catch a tampered proposal.
///
/// (Not `PartialEq`: `consensus_rule: Option<ConsensusRule>` transitively
/// holds `PropertyCondition` with float variants that can't derive `Eq`.
/// Test assertions compare field-by-field.)
#[derive(Debug, Clone)]
pub struct SatisfiedTransition {
    /// Flow this transition belongs to — matches `SHACLFlow.name` and
    /// `FlowInstance.flow`.
    pub flow_name: String,
    /// Instance URI the transition applies to — matches
    /// `FlowInstanceRecord.instance_uri`.
    pub instance_uri: String,
    /// Base expression the instance is bound to — matches
    /// `FlowInstanceRecord.subject`.
    pub subject: String,
    /// State the instance is currently in (must equal
    /// `FlowInstanceRecord.current_state` at evaluation time).
    pub from_state: String,
    /// State the instance would move to.
    pub to_state: String,
    /// Every matched instance-id across all queries in the state's
    /// `requires` array, in the order they appeared per query then
    /// sorted globally. Used by slice 10.4b as the proposal's evidence
    /// bag; the same list is fed to [`evidence_hash`].
    pub evidence_ids: Vec<String>,
    /// SHA256 of `(class_names_joined, evidence_ids_sorted)` — a
    /// tamper-detectable seal the consensus engine can re-verify in
    /// slice 10.6.
    pub evidence_hash: String,
    /// Per-state `semanticCheck` hint carried forward so slice 10.5's
    /// optional 2nd-pass LLM confirmation can be triggered. `None` =
    /// state-level `requires` matches are sufficient to fire the
    /// proposal.
    pub semantic_check: Option<String>,
    /// The consensus rule that must be met before the flow actually
    /// advances. Prefer the per-state override, fall back to the
    /// flow-level default.
    pub consensus_rule: Option<ConsensusRule>,
}

/// Deterministic hash of the evidence bag for a satisfied transition.
///
/// Input is `(class_names_joined, evidence_ids_sorted)`: class names are
/// joined with `|` (a character that never appears in a URI), the
/// evidence-ID vector is sorted lexicographically then joined with `\n`,
/// then the two are separated by `\0`. SHA256'd, hex-encoded.
///
/// The sort makes the hash independent of the order the perspective
/// returned instances in — otherwise two evaluations of the same
/// requires against the same graph state could produce different
/// hashes.
pub fn evidence_hash(class_names: &[String], evidence_ids: &[String]) -> String {
    let mut sorted_ids = evidence_ids.to_vec();
    sorted_ids.sort();
    let mut hasher = Sha256::new();
    hasher.update(class_names.join("|").as_bytes());
    hasher.update(b"\0");
    hasher.update(sorted_ids.join("\n").as_bytes());
    hex::encode(hasher.finalize())
}

/// Cardinality check — is `actual` within `count.{min, max}`?
///
/// Semantics match the design doc §7:
/// - Unset `count` = at least one match (equivalent to `{ min: 1 }`).
/// - `min` unset = no lower bound (0 matches is allowed).
/// - `max` unset = no upper bound.
/// - Both bounds are inclusive.
pub fn cardinality_satisfied(count: Option<&ModelQueryCount>, actual: usize) -> bool {
    let Some(c) = count else {
        return actual >= 1;
    };
    if let Some(min) = c.min {
        if actual < min as usize {
            return false;
        }
    }
    if let Some(max) = c.max {
        if actual > max as usize {
            return false;
        }
    }
    // Both bounds unset ⇒ every count satisfies, including 0. Matches
    // the design intent ("at most 0 matches" is a legal negative guard).
    true
}

/// Translate a `ModelQuery` guard (flow-side type) to the
/// `ModelQueryInput` shape (`model_query`'s serialized input). Pure —
/// slice 10.4a2's async evaluator calls this once per query and hands
/// the result to `PerspectiveInstance::model_query`.
///
/// `acting_did` resolves `$did` in `didProperty` at translation time
/// (§7.2). The convention `"$did"` triggers substitution; any other
/// string is passed through verbatim — an escape hatch for hardcoded
/// roles that never made it into the design doc but which we should
/// not silently break.
///
/// `or` composition recurses; each alternative is translated to a
/// sub-object under the `where.OR` key using the `SubClauses` shape
/// (`WhereCondition::SubClauses`). Nested `or` composes further.
pub fn build_query_input_for_requires(query: &ModelQuery, acting_did: &str) -> Value {
    // Base where clause — carry through everything the caller declared,
    // translating each PropertyCondition to the matching WhereCondition
    // JSON shape.
    let mut where_obj = serde_json::Map::new();
    if let Some(w) = query.r#where.as_ref() {
        for (field, cond) in w {
            where_obj.insert(field.clone(), property_condition_to_where(cond));
        }
    }

    // didProperty gate — add `<didProperty>: $did` (or the raw value if
    // the caller hard-coded a role).
    if let Some(prop) = query.did_property.as_ref() {
        let resolved = if prop.contains("$did") {
            // Rare: caller wants the DID in a bigger string. Substitute
            // in-place. Keeps this future-proof for expressions.
            prop.replace("$did", acting_did)
        } else {
            // Common case: `didProperty: "author"` means where.author = $did.
            // The design doc does not spell out a way to hardcode a role
            // via didProperty; if we ever want that, the caller writes
            // `where: { author: "did:key:..." }` directly.
            acting_did.to_string()
        };
        where_obj.insert(
            // If the caller wrote a $did-expression as the property name,
            // use the property name they meant — the LHS of an `=` needs
            // an actual field name. Otherwise use the didProperty verbatim.
            if prop.contains("$did") {
                // Not expressible in the current schema; log-worthy in
                // the async layer, but here we keep pure semantics and
                // fall back to the raw string as a field name so the
                // model_query will reject with a clear "no such property".
                prop.clone()
            } else {
                prop.clone()
            },
            Value::String(resolved),
        );
    }

    // OR sub-composition — recurse per alternative, wrap in the `SubClauses`
    // shape under the `OR` key (matches WhereCondition::SubClauses).
    if let Some(alts) = query.or.as_ref() {
        if !alts.is_empty() {
            let branches: Vec<Value> = alts
                .iter()
                .map(|alt| {
                    // Each branch's translated where clause. We only lift
                    // the .where field into the branch — count / linkedTo
                    // on an alt would layer awkwardly; deferred until a
                    // real caller needs it (see 10.4a2 comment).
                    let sub = build_query_input_for_requires(alt, acting_did);
                    sub.get("where").cloned().unwrap_or(json!({}))
                })
                .collect();
            where_obj.insert("OR".to_string(), Value::Array(branches));
        }
    }

    let mut input = serde_json::Map::new();
    if !where_obj.is_empty() {
        input.insert("where".to_string(), Value::Object(where_obj));
    }

    Value::Object(input)
}

/// Translate one `PropertyCondition` (flow-side) to its `WhereCondition`
/// JSON representation. Scalar shorthands compile to the direct-value
/// WhereCondition variants; typed operators compile to the `Ops` shape.
fn property_condition_to_where(cond: &PropertyCondition) -> Value {
    match cond {
        PropertyCondition::Str(s) => Value::String(s.clone()),
        PropertyCondition::Num(n) => json!(n),
        PropertyCondition::Bool(b) => Value::Bool(*b),
        PropertyCondition::Equals { equals } => equals.clone(),
        PropertyCondition::In { one_of } => {
            // WhereCondition::StringArray / NumberArray untagged-matches
            // on the array shape at deserialize time. Pass through as-is.
            Value::Array(one_of.clone())
        }
        PropertyCondition::Exists { exists } => {
            // No first-class "exists" in WhereCondition. Model as
            // `{ not: { equals: null } }` for the true case and the
            // inverse for the false case; WhereOps supports `not`.
            if *exists {
                json!({ "not": { "equals": Value::Null } })
            } else {
                json!({ "equals": Value::Null })
            }
        }
        PropertyCondition::Matches { matches } => json!({ "regex": matches }),
    }
}

// ============================================================================
// Slice 10.4a2 — async layer over `model_query`
// ============================================================================

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

// ============================================================================
// Slice 10.4b — writer stage: SatisfiedTransition → on-graph proposal
// ============================================================================

/// Slice 10.4b — convenience over
/// [`crate::perspectives::flow_classes::write_flow_transition_proposal`]
/// for the engine-generated path.
///
/// Consumes a [`SatisfiedTransition`] (the deterministic
/// requires-satisfied record produced by [`evaluate_flow_transitions`])
/// and threads its fields into the primitive writer. Kept in the
/// evaluator module — not `flow_classes` — so the classes layer stays a
/// leaf: it doesn't need to know about `SatisfiedTransition` to mint
/// proposals, and this wrapper is only compiled on the evaluator's dep
/// path.
///
/// `proposal_id` / `proposed_at` / `batch_id` are caller-supplied to
/// stay consistent with `mint_flow_instance` — the auto-processor
/// call-site (slice 10.4c) will generate the id + timestamp and thread
/// its own batch so the whole extraction pass commits atomically.
///
/// Returns the freshly-minted proposal URI.
#[allow(clippy::too_many_arguments)]
pub async fn write_engine_proposal(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    proposal_id: &str,
    proposer_did: &str,
    proposed_at: &str,
    transition: &SatisfiedTransition,
    rationale: Option<&str>,
    batch_id: Option<String>,
    context: &crate::agent::AgentContext,
) -> anyhow::Result<String> {
    crate::perspectives::flow_classes::write_flow_transition_proposal(
        perspective,
        proposal_id,
        proposer_did,
        proposed_at,
        &transition.instance_uri,
        &transition.from_state,
        &transition.to_state,
        &transition.evidence_ids,
        &transition.evidence_hash,
        rationale,
        batch_id,
        context,
    )
    .await
}

/// Slice 10.6c — an LLM-emitted "proposal to advance this flow" that the
/// engine may honour when the deterministic `requires` guard also fires.
///
/// This is the boundary type between the interpretation layer (which parses
/// [`crate::perspectives::interpretation::types::LlmFlowProposal`] from the
/// LLM's JSON) and the flow-post-processing engine. Keeping the boundary
/// type here (rather than importing `LlmFlowProposal` directly) avoids the
/// `flow_evaluator` ← `interpretation` ← `flow_evaluator` module cycle.
///
/// - `instance_uri` — the FlowInstance URI the LLM cited (verbatim from
///   the prompt's `active_flows[i].instance`).
/// - `to_state` — one of that FlowInstance's `nextStates[j].name` values.
/// - `reason` — optional short attribution. Written as the proposal's
///   `rationale` field only when `Some(text)` with non-empty text.
///
/// Match semantics inside [`run_engine_proposal_pass`]: a hint matches a
/// [`SatisfiedTransition`] iff BOTH `instance_uri` and `to_state` match.
/// Unmatched hints (no satisfied transition for that pair) are silently
/// discarded — the LLM cannot bypass the deterministic guard, and the
/// prompt already documents this behavior.
#[derive(Debug, Clone, PartialEq)]
pub struct LlmProposalHint {
    pub instance_uri: String,
    pub to_state: String,
    pub reason: Option<String>,
}

// ============================================================================
// Slice 10.4c — the auto-processor entry point
// ============================================================================

/// Slice 10.4c — compose the load → evaluate → write pipeline into one
/// call that the extraction pass (`interpretation::run`) invokes AFTER
/// `apply_with_overlay` has committed the LLM-derived writes. At that
/// point the graph state on which `requires` model-queries run is what
/// the pass just produced, so a transition satisfied by fresh evidence
/// is immediately turned into a proposal on behalf of the acting DID.
///
/// Silent-fail throughout — the extraction pass MUST NOT break because
/// the flow layer stumbled. Loader errors, unknown-DID errors, and
/// individual `write_engine_proposal` failures are logged (`warn!` for
/// the loader path, `debug!` for per-transition writes) and downgraded
/// to an empty result / a partial list.
///
/// `scope`, when `Some`, narrows the FlowInstance load to the pass's
/// anchor URI (same policy as [`crate::perspectives::flow_context::gather_active_flow_contexts`]).
///
/// `semantic_check`, when `Some((llm, model_id))`, wires the slice 10.5
/// 2nd-pass LLM confirmation between the deterministic evaluator and the
/// on-graph write. For each `SatisfiedTransition` whose target state
/// carries a `semantic_check` hint, [`crate::perspectives::flow_semantic_check::run_semantic_check`]
/// is invoked and only a `Pass` verdict advances the transition to the
/// write stage; `Fail` and `Ambiguous` discard the transition (fail-safe:
/// an uncertain LLM must not silently advance a flow). Transitions
/// without a per-state `semantic_check` hint are auto-passed without an
/// LLM call. LLM I/O errors are treated as `discard` — flow layer must
/// never break the extraction pass. When `semantic_check` is `None`
/// (call sites pre-10.5c), the gate is skipped entirely and the pass
/// behaves exactly as slice 10.4c shipped.
///
/// `llm_hints` (slice 10.6c) carries the LLM's own `flow_proposals` output
/// as a slice of [`LlmProposalHint`]s. When a hint matches a satisfied
/// transition by `(instance_uri, to_state)`, the LLM's `reason` (if any)
/// is written as the proposal's `rationale` field — attribution flows from
/// the LLM to the on-graph proposal. Hints WITHOUT a matching satisfied
/// transition are silently discarded (design §5.4 step 5: LLM cannot
/// bypass the deterministic `requires` guard). Satisfied transitions
/// without a matching hint still get an engine-emitted proposal, exactly
/// as slice 10.4c/10.5c shipped — but with `rationale = None` (byte-
/// identical writes to the pre-10.6c path). Pass `&[]` to opt out.
///
/// Returns the URIs of every `FlowTransitionProposal` this pass minted.
/// The extraction pass threads these into
/// [`crate::perspectives::interpretation::run::InterpretationOutcome::flow_proposals`]
/// so tests / callers can observe.
pub async fn run_engine_proposal_pass(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    scope: Option<&crate::perspectives::model_query::types::Scope>,
    context: &crate::agent::AgentContext,
    semantic_check: Option<(
        &dyn crate::perspectives::flow_semantic_check::SemanticCheckLlm,
        &str,
    )>,
    llm_hints: &[LlmProposalHint],
) -> Vec<String> {
    // Load the flow catalogue. Empty on I/O failure — same policy as
    // `gather_active_flow_contexts`. An empty perspective has zero flows,
    // and there's nothing to post-process.
    let flows_by_name = match crate::perspectives::flow_context::load_shacl_flows(perspective).await
    {
        Ok(m) => m,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: load_shacl_flows failed: {e:#}");
            return Vec::new();
        }
    };
    if flows_by_name.is_empty() {
        return Vec::new();
    }

    // Load active FlowInstances, scope-narrowed if the pass carries an
    // anchor. Same silent-fallback as the pre-pass loader.
    let subject = scope.map(crate::perspectives::flow_context::scope_subject);
    let records =
        match crate::perspectives::flow_context::load_flow_instances(perspective, subject).await {
            Ok(r) => r,
            Err(e) => {
                log::warn!("run_engine_proposal_pass: load_flow_instances failed: {e:#}");
                return Vec::new();
            }
        };
    if records.is_empty() {
        return Vec::new();
    }

    // The proposer of an engine-generated proposal is the acting DID of
    // the extraction pass — same identity that owns the InterpretationRun
    // for the committed writes. Silent-fallback on lookup failure: no DID
    // → no proposals; the transitions remain latent for the next pass.
    let acting_did = match crate::agent::did_for_context(context) {
        Ok(d) => d,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: did_for_context failed: {e:#}");
            return Vec::new();
        }
    };

    let satisfied =
        evaluate_flow_transitions(perspective, &records, &flows_by_name, &acting_did).await;
    if satisfied.is_empty() {
        return Vec::new();
    }

    // Slice 10.5b — index FlowContext by instance_uri so the semantic-check
    // gate can look up the flow's overall interpretationHint + next-state
    // summaries when composing its confirmation prompt. Computed once per
    // pass (not per transition) since multiple SatisfiedTransitions can
    // share the same active FlowInstance. Only needed when
    // `semantic_check` is `Some` — an empty HashMap when the gate is off
    // costs one allocation and keeps the per-transition loop uniform.
    let flow_ctx_by_uri: std::collections::HashMap<
        String,
        crate::perspectives::flow_context::FlowContext,
    > = if semantic_check.is_some() {
        crate::perspectives::flow_context::build_flow_contexts(&records, &flows_by_name)
            .into_iter()
            .map(|c| (c.instance_uri.clone(), c))
            .collect()
    } else {
        std::collections::HashMap::new()
    };

    // Each proposal writes inside its own batch — same
    // create_batch / commit_batch discipline `apply_with_overlay` uses
    // for `mint_interpretation_run`. `write_flow_transition_proposal`
    // internally does `create_subject` + N-1 `update_subject` for the
    // evidence bag, so wrapping them in a batch is what makes the
    // proposal land atomically on-graph (readers never see a
    // half-populated proposal). Per-transition batches — not one batch
    // for the whole pass — so a single failure only rolls back that
    // proposal and the others still ship.
    let proposed_at = chrono::Utc::now().timestamp_millis().to_string();

    let mut minted = Vec::with_capacity(satisfied.len());
    for transition in &satisfied {
        // Slice 10.5b — semantic-check gate. Runs BEFORE the write so a
        // rejected/uncertain transition never lands as a proposal. The gate
        // is skipped entirely when the caller passes `None` (back-compat
        // with slice 10.4c's callers). When `Some((llm, model_id))`:
        //   - transition has no `semantic_check` hint → `run_semantic_check`
        //     short-circuits to `Pass` without an LLM call (see the
        //     `build_semantic_check_prompt` contract).
        //   - hint present → LLM is called; only `Pass` fires. `Fail` and
        //     `Ambiguous` discard (fail-safe: uncertain LLM must not
        //     silently advance a flow).
        //   - LLM I/O error → discard the transition, log at `debug!`. The
        //     flow layer must never break the extraction pass.
        //   - FlowContext lookup miss (unexpected: every SatisfiedTransition
        //     came from a record we built contexts for) → discard + log,
        //     same fail-safe philosophy.
        if let Some((llm, model_id)) = semantic_check {
            let Some(flow_ctx) = flow_ctx_by_uri.get(&transition.instance_uri) else {
                log::debug!(
                    "run_engine_proposal_pass: no FlowContext for {}.{}→{} (instance {}); discarding",
                    transition.flow_name,
                    transition.from_state,
                    transition.to_state,
                    transition.instance_uri,
                );
                continue;
            };
            match crate::perspectives::flow_semantic_check::run_semantic_check(
                llm, model_id, transition, flow_ctx,
            )
            .await
            {
                Ok(verdict) => {
                    if !crate::perspectives::flow_semantic_check::should_fire_proposal(verdict) {
                        log::debug!(
                            "run_engine_proposal_pass: semantic-check {verdict:?} on {}.{}→{}; discarding",
                            transition.flow_name,
                            transition.from_state,
                            transition.to_state,
                        );
                        continue;
                    }
                }
                Err(e) => {
                    log::debug!(
                        "run_engine_proposal_pass: semantic-check LLM error on {}.{}→{}: {e:#}; discarding",
                        transition.flow_name,
                        transition.from_state,
                        transition.to_state,
                    );
                    continue;
                }
            }
        }

        // Slice 10.6c — match LLM hints by (instance_uri, to_state). The
        // first matching hint wins if the LLM emitted several for the
        // same pair (the prompt caps at one per instance per pass, but
        // this is a fail-safe against a chatty small model). An unmatched
        // satisfied transition still writes — just without a rationale —
        // preserving byte-identical behavior with the pre-10.6c path
        // when `llm_hints` is empty or nothing matches.
        let rationale = llm_hints
            .iter()
            .find(|h| {
                h.instance_uri == transition.instance_uri && h.to_state == transition.to_state
            })
            .and_then(|h| h.reason.as_deref())
            .filter(|s| !s.is_empty());

        let proposal_id = uuid::Uuid::new_v4().to_string();
        let batch_id = perspective.create_batch().await;
        let write_res = write_engine_proposal(
            perspective,
            &proposal_id,
            &acting_did,
            &proposed_at,
            transition,
            rationale,
            Some(batch_id.clone()),
            context,
        )
        .await;
        match write_res {
            Ok(uri) => match perspective.commit_batch(batch_id.clone(), context).await {
                Ok(_) => minted.push(uri),
                Err(e) => {
                    let _ = perspective.discard_batch(&batch_id).await;
                    log::debug!(
                        "run_engine_proposal_pass: commit_batch for {}.{}→{} failed: {e:#}",
                        transition.flow_name,
                        transition.from_state,
                        transition.to_state,
                    );
                }
            },
            Err(e) => {
                // One bad write must not sink the rest — mirror the
                // per-transition silent-skip policy `evaluate_flow_transitions`
                // uses for query errors.
                let _ = perspective.discard_batch(&batch_id).await;
                log::debug!(
                    "run_engine_proposal_pass: write_engine_proposal for {}.{}→{} failed: {e:#}",
                    transition.flow_name,
                    transition.from_state,
                    transition.to_state,
                );
            }
        }
    }
    minted
}

// ============================================================================
// Slice 10.10a — engine-side consensus firing pass
// ============================================================================

/// Slice 10.10a — engine-side auto-processor entry point for consensus
/// firing. Walks every active `FlowInstance` on `scope`, loads its
/// on-graph `FlowTransitionProposal` bag, aggregates the votes against
/// the flow's `ConsensusRule`, and fires the earliest tally whose
/// `fromState` matches the instance's live `currentState`. Byte-for-byte
/// symmetric with the TS `fireIfConsensus` composition
/// (`core/src/perspectives/FlowConsensusFire.ts`): same loader, same
/// aggregator, same stale-`fromState` guard, same writer.
///
/// Intended call site: the extraction pass (`interpretation::run`),
/// invoked AFTER [`run_engine_proposal_pass`] so a proposal minted on
/// this pass can immediately participate in consensus if the on-graph
/// bag already carries enough peer votes to reach the rule's `n`. That
/// wiring lands in slice 10.10b; this fn is standalone in 10.10a so
/// tests can prove the composition end-to-end without touching
/// `run.rs`.
///
/// Consensus rule resolution mirrors TS `FlowInstance.consensusRule`:
///   1. state-level `consensus_rule` on the current `FlowState` (if any)
///   2. flow-level `consensus_rule` on the `SHACLFlow` (if any)
///   3. otherwise `None` — [`crate::perspectives::flow_consensus::aggregate_flow_votes`]
///      then falls back to its `DEFAULT_N` (currently 1) so a
///      single-proposer neighbourhood still fires.
///
/// `from_role` eligibility gating is DEFERRED — the auto-processor does
/// not yet resolve "who is eligible to vote from role X" against the
/// live perspective. Instances whose resolved rule carries `from_role`
/// are logged (`warn!`) and skipped: firing without a real eligibility
/// answer would silently misreport (mirrors the `aggregate_flow_votes`
/// silent-default guard). A future slice will resolve DIDs via
/// `didProperty`-style class queries and drop this branch.
///
/// Silent-fallback throughout — the extraction pass MUST NOT break
/// because a downstream flow layer stumbled. Every loader error, every
/// aggregator error, every writer error is logged and the offending
/// instance is skipped; the pass returns whatever it did manage to fire.
///
/// `scope`, when `Some`, narrows the FlowInstance load to the pass's
/// anchor URI — same policy as
/// [`crate::perspectives::flow_context::gather_active_flow_contexts`]
/// and [`run_engine_proposal_pass`].
///
/// Returns one [`crate::perspectives::flow_consensus::FireOutcome`] per
/// instance whose `currentState` this pass advanced. The extraction pass
/// threads these into
/// [`crate::perspectives::interpretation::run::InterpretationOutcome`]
/// so tests / callers can observe which flows moved.
pub async fn run_flow_consensus_pass(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    scope: Option<&crate::perspectives::model_query::types::Scope>,
    context: &crate::agent::AgentContext,
) -> Vec<crate::perspectives::flow_consensus::FireOutcome> {
    // Same absent-catalogue silent return as `run_engine_proposal_pass`
    // — an empty perspective has no flows to fire.
    let flows_by_name = match crate::perspectives::flow_context::load_shacl_flows(perspective).await
    {
        Ok(m) => m,
        Err(e) => {
            log::warn!("run_flow_consensus_pass: load_shacl_flows failed: {e:#}");
            return Vec::new();
        }
    };
    if flows_by_name.is_empty() {
        return Vec::new();
    }

    let subject = scope.map(crate::perspectives::flow_context::scope_subject);
    let records =
        match crate::perspectives::flow_context::load_flow_instances(perspective, subject).await {
            Ok(r) => r,
            Err(e) => {
                log::warn!("run_flow_consensus_pass: load_flow_instances failed: {e:#}");
                return Vec::new();
            }
        };
    if records.is_empty() {
        return Vec::new();
    }

    let mut fired = Vec::new();
    for instance in &records {
        // Resolve flow → state → consensus rule. Unknown-flow, unknown-
        // state, and unresolved-rule cases all `continue` (with a
        // `debug!` note) since none of them are consensus-firing errors:
        // an instance for a flow definition that hasn't been synced yet
        // is normal; a state name that drifted is a definition-vs-
        // instance mismatch worth logging but not worth aborting the
        // whole pass over.
        let Some(flow) = flows_by_name.get(&instance.flow_name) else {
            log::debug!(
                "run_flow_consensus_pass: no SHACLFlow for '{}' (instance {}); skipping",
                instance.flow_name,
                instance.instance_uri,
            );
            continue;
        };
        let state_rule = flow
            .states
            .iter()
            .find(|s| s.name == instance.current_state)
            .and_then(|s| s.consensus_rule.clone());
        let rule = state_rule.or_else(|| flow.consensus_rule.clone());

        // `from_role` gating deferred (see fn doc). Log + skip so a
        // future slice can lift the guard without changing the write
        // shape (fired-outcomes contract stays stable).
        if let Some(r) = rule.as_ref() {
            if r.from_role.is_some() {
                log::debug!(
                    "run_flow_consensus_pass: consensus rule for {}.{} has from_role — deferring until eligibility resolution ships; skipping instance {}",
                    instance.flow_name,
                    instance.current_state,
                    instance.instance_uri,
                );
                continue;
            }
        }

        let loaded = match crate::perspectives::flow_consensus::load_flow_transition_proposals(
            perspective,
            &instance.instance_uri,
        )
        .await
        {
            Ok(v) => v,
            Err(e) => {
                log::debug!(
                    "run_flow_consensus_pass: load_flow_transition_proposals({}) failed: {e:#}",
                    instance.instance_uri,
                );
                continue;
            }
        };
        if loaded.is_empty() {
            continue;
        }

        let aggregate = match crate::perspectives::flow_consensus::aggregate_flow_votes(
            &loaded,
            rule.as_ref(),
            None,
        ) {
            Ok(a) => a,
            Err(e) => {
                log::debug!(
                    "run_flow_consensus_pass: aggregate_flow_votes({}) failed: {e:#}",
                    instance.instance_uri,
                );
                continue;
            }
        };

        let Some(picked) = crate::perspectives::flow_consensus::select_fire_candidate(
            &instance.current_state,
            &aggregate,
        ) else {
            continue;
        };

        let batch_id = perspective.create_batch().await;
        match crate::perspectives::flow_consensus::fire_flow_consensus(
            perspective,
            instance,
            picked,
            Some(batch_id.clone()),
            context,
        )
        .await
        {
            Ok(outcome) => match perspective.commit_batch(batch_id.clone(), context).await {
                Ok(_) => fired.push(outcome),
                Err(e) => {
                    let _ = perspective.discard_batch(&batch_id).await;
                    log::debug!(
                        "run_flow_consensus_pass: commit_batch for {} ({}→{}) failed: {e:#}",
                        instance.instance_uri,
                        picked.from_state,
                        picked.to_state,
                    );
                }
            },
            Err(e) => {
                let _ = perspective.discard_batch(&batch_id).await;
                log::debug!(
                    "run_flow_consensus_pass: fire_flow_consensus({}) failed: {e:#}",
                    instance.instance_uri,
                );
            }
        }
    }
    fired
}

/// Slice 10.16 — report-only preview of what
/// [`run_flow_consensus_pass`] would fire for a single `FlowInstance`
/// if invoked right now, without touching the graph.
///
/// Engine-side counterpart of TS `FlowInstance.selectFireCandidate`
/// (slice 10.15, `core/src/perspectives/FlowModels.ts`). Composes the
/// same load → resolve-rule → load-proposals → aggregate → select
/// sequence as one iteration of `run_flow_consensus_pass`, stopping at
/// [`crate::perspectives::flow_consensus::select_fire_candidate`]
/// instead of proceeding to
/// [`crate::perspectives::flow_consensus::fire_flow_consensus`].
///
/// Intended use: observability paths that want to log
/// "would-fire {from}→{to}, {n}/{required}" without side effects, or
/// tests that need the pass's per-instance verdict without running the
/// full scope walk.
///
/// # Returns
///
/// - `Ok(Some(tally))` — a tally has met consensus and its `from_state`
///   matches the instance's `current_state`. `run_flow_consensus_pass`
///   would fire this transition on its next invocation (assuming the
///   graph does not change in between).
/// - `Ok(None)` — one of: instance URI not found on this perspective,
///   flow definition absent, consensus rule has `from_role` (deferred,
///   same policy as `run_flow_consensus_pass`), proposal bag empty, no
///   tally reached consensus, or winning tally's `from_state` is stale
///   relative to `instance.current_state`.
/// - `Err(_)` — caller-side violation (empty `flow_instance_uri`).
///   Loader / aggregator errors surface as `Ok(None)` + `log::debug!`,
///   same soft-fail contract as `run_flow_consensus_pass` (a report-only
///   entry that panics on transient perspective glitches would be worse
///   than one that returns "cannot answer right now").
///
/// # Report-only contract (enforced by construction)
///
/// The imports reached from this function are:
/// [`crate::perspectives::flow_context::load_flow_instances`] (read),
/// [`crate::perspectives::flow_context::load_shacl_flows`] (read),
/// [`crate::perspectives::flow_consensus::load_flow_transition_proposals`]
/// (read),
/// [`crate::perspectives::flow_consensus::aggregate_flow_votes`] (pure),
/// [`crate::perspectives::flow_consensus::select_fire_candidate`] (pure).
///
/// No call reaches `advance_flow_instance_state`, `fire_flow_consensus`,
/// `create_batch`, or `commit_batch`. The `&PerspectiveInstance`
/// (shared, not `&mut`) signature is deliberate — a compile-time proof
/// that no on-graph write can escape this path.
pub async fn preview_fire_for_instance(
    perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
    flow_instance_uri: &str,
) -> anyhow::Result<Option<crate::perspectives::flow_consensus::FlowVoteTally>> {
    if flow_instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "preview_fire_for_instance: flow_instance_uri must not be empty"
        ));
    }

    // Locate the FlowInstance by its own URI.
    // `load_flow_instances` filters by anchor `subject`, not by instance
    // URI — so we load all and match. Bounded in practice (a perspective
    // holds a handful of live flow instances), and avoiding a
    // "by-instance-URI" model_query surface keeps the API narrow.
    let records = match crate::perspectives::flow_context::load_flow_instances(perspective, None)
        .await
    {
        Ok(r) => r,
        Err(e) => {
            log::debug!(
                "preview_fire_for_instance: load_flow_instances failed for {flow_instance_uri}: {e:#}"
            );
            return Ok(None);
        }
    };
    let Some(instance) = records.iter().find(|r| r.instance_uri == flow_instance_uri) else {
        log::debug!(
            "preview_fire_for_instance: no FlowInstance with URI {flow_instance_uri} on this perspective"
        );
        return Ok(None);
    };

    let flows_by_name = match crate::perspectives::flow_context::load_shacl_flows(perspective).await
    {
        Ok(m) => m,
        Err(e) => {
            log::debug!("preview_fire_for_instance: load_shacl_flows failed: {e:#}");
            return Ok(None);
        }
    };
    let Some(flow) = flows_by_name.get(&instance.flow_name) else {
        log::debug!(
            "preview_fire_for_instance: no SHACLFlow for '{}' (instance {})",
            instance.flow_name,
            instance.instance_uri,
        );
        return Ok(None);
    };

    // Rule resolution mirrors `run_flow_consensus_pass`: state override
    // wins, then flow default, then aggregator default (n = 1).
    // `from_role` gating is DEFERRED, same policy as the walker — an
    // observer that reports "would fire" on a role-gated rule without
    // resolving eligibility would misreport.
    let state_rule = flow
        .states
        .iter()
        .find(|s| s.name == instance.current_state)
        .and_then(|s| s.consensus_rule.clone());
    let rule = state_rule.or_else(|| flow.consensus_rule.clone());
    if let Some(r) = rule.as_ref() {
        if r.from_role.is_some() {
            log::debug!(
                "preview_fire_for_instance: consensus rule for {}.{} has from_role — deferring until eligibility resolution ships; instance {}",
                instance.flow_name,
                instance.current_state,
                instance.instance_uri,
            );
            return Ok(None);
        }
    }

    let loaded = match crate::perspectives::flow_consensus::load_flow_transition_proposals(
        perspective,
        &instance.instance_uri,
    )
    .await
    {
        Ok(v) => v,
        Err(e) => {
            log::debug!(
                "preview_fire_for_instance: load_flow_transition_proposals({}) failed: {e:#}",
                instance.instance_uri,
            );
            return Ok(None);
        }
    };
    if loaded.is_empty() {
        return Ok(None);
    }

    let aggregate = match crate::perspectives::flow_consensus::aggregate_flow_votes(
        &loaded,
        rule.as_ref(),
        None,
    ) {
        Ok(a) => a,
        Err(e) => {
            log::debug!(
                "preview_fire_for_instance: aggregate_flow_votes({}) failed: {e:#}",
                instance.instance_uri,
            );
            return Ok(None);
        }
    };

    Ok(crate::perspectives::flow_consensus::select_fire_candidate(
        &instance.current_state,
        &aggregate,
    )
    .cloned())
}

/// Slice 10.14 — per-instance mutating counterpart of TS
/// `FlowInstance.fireIfConsensus`
/// (`core/src/perspectives/FlowModels.ts`).
///
/// Runs the same load-instance → resolve-rule → load-proposals →
/// aggregate → `select_fire_candidate` chain as
/// [`preview_fire_for_instance`], and, when the chain yields a
/// consensus-reached tally, proceeds to
/// [`crate::perspectives::flow_consensus::fire_flow_consensus`] under a
/// fresh atomic batch. Returns `Ok(Some(outcome))` only after the
/// commit lands; `Ok(None)` for every soft-fail case that would keep
/// the on-graph state stationary; `Err` only on empty URI (the same
/// caller-mistake shape as [`preview_fire_for_instance`]).
///
/// This is the single-instance counterpart to
/// [`run_flow_consensus_pass`] (10.10a's scope-wide walker). Callers
/// that already know *which* `FlowInstance` they want to advance
/// (e.g. an auto-processor keyed off a specific inbound proposal, or
/// a UI action that resolves to a known instance) should prefer this
/// entry: it avoids the walker's `load_flow_instances(None)` sweep
/// and reports back exactly one outcome instead of a `Vec` the caller
/// then filters.
///
/// # Report-then-fire, not fire-then-report
///
/// The mutation only fires when `select_fire_candidate` returns
/// `Some(tally)` with `tally.consensus_reached == true`. The
/// [`crate::perspectives::flow_consensus::fire_flow_consensus`]
/// primitive also enforces `consensus_reached` +
/// `from_state == current_state` + non-noop `to_state`, so a
/// programming error at this layer (calling `fire_flow_consensus` on
/// a stale tally) surfaces as `Err` from the fire primitive; a
/// commit failure after a successful fire discards the batch and
/// surfaces as `Ok(None)` (soft-fail — the graph is intact and the
/// next call can retry).
///
/// # Symmetry with TS
///
/// TS `FlowInstance.fireIfConsensus` composes
/// `this.aggregateVotes(...)` (which loads its own proposal bag) +
/// `FlowConsensusFire.fireIfConsensus(perspective, this, aggregate)`
/// (which owns the stale-guard + `save()`). This Rust entry uses the
/// same substrate (aggregate → select-candidate → fire → commit) with
/// two engine-specific differences: (1) it re-derives the rule from
/// the on-graph SHACLFlow catalogue on every call (vs. TS taking
/// `consensusRule` as a call-time param), so an updated flow
/// definition on the perspective takes effect immediately; (2) it
/// batches the fire in its own commit unit so callers can call it
/// inside a larger flow without leaking a caller batch. Consensus
/// verifiers that walk the on-graph shape cannot distinguish an
/// advance produced by this entry from one produced by
/// [`run_flow_consensus_pass`] or the TS OO wrapper — same
/// `FlowInstance.currentState` link replacement under `setSingleTarget`.
///
/// # Soft-fail contract
///
/// Returns `Ok(None)` (with `log::debug!`) for:
///   - `load_flow_instances` transient error
///   - unknown flow instance URI (no matching record on-graph)
///   - `load_shacl_flows` transient error
///   - no SHACLFlow catalogued for the instance's `flow_name`
///   - `from_role` rule set (eligibility resolution deferred, same
///     policy as `run_flow_consensus_pass` and
///     `preview_fire_for_instance`)
///   - `load_flow_transition_proposals` transient error
///   - empty proposal bag
///   - `aggregate_flow_votes` internal error
///   - `select_fire_candidate` returns `None` (below-threshold or
///     stale from_state)
///   - `fire_flow_consensus` returns `Err` (batch discarded)
///   - `commit_batch` returns `Err` after successful fire (batch
///     discarded — the fire is buffered but not yet committed, so
///     discarding rolls it back cleanly)
///
/// Returns `Err` only on empty `flow_instance_uri` — a caller
/// programming error that would degenerate into a full-perspective
/// scan through the load path.
pub async fn fire_if_consensus_for_instance(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    flow_instance_uri: &str,
    context: &crate::agent::AgentContext,
) -> anyhow::Result<Option<crate::perspectives::flow_consensus::FireOutcome>> {
    if flow_instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "fire_if_consensus_for_instance: flow_instance_uri must not be empty"
        ));
    }

    let records = match crate::perspectives::flow_context::load_flow_instances(perspective, None)
        .await
    {
        Ok(r) => r,
        Err(e) => {
            log::debug!(
                "fire_if_consensus_for_instance: load_flow_instances failed for {flow_instance_uri}: {e:#}"
            );
            return Ok(None);
        }
    };
    let Some(instance) = records
        .iter()
        .find(|r| r.instance_uri == flow_instance_uri)
        .cloned()
    else {
        log::debug!(
            "fire_if_consensus_for_instance: no FlowInstance with URI {flow_instance_uri} on this perspective"
        );
        return Ok(None);
    };

    let flows_by_name = match crate::perspectives::flow_context::load_shacl_flows(perspective).await
    {
        Ok(m) => m,
        Err(e) => {
            log::debug!("fire_if_consensus_for_instance: load_shacl_flows failed: {e:#}");
            return Ok(None);
        }
    };
    let Some(flow) = flows_by_name.get(&instance.flow_name) else {
        log::debug!(
            "fire_if_consensus_for_instance: no SHACLFlow for '{}' (instance {})",
            instance.flow_name,
            instance.instance_uri,
        );
        return Ok(None);
    };

    // Rule resolution mirrors `run_flow_consensus_pass` +
    // `preview_fire_for_instance`: state override wins, then flow
    // default, then aggregator default (n = 1). `from_role` gating is
    // DEFERRED — same policy as the walker. A per-instance entry that
    // silently ignored `from_role` would drift out of parity with the
    // walker's contract.
    let state_rule = flow
        .states
        .iter()
        .find(|s| s.name == instance.current_state)
        .and_then(|s| s.consensus_rule.clone());
    let rule = state_rule.or_else(|| flow.consensus_rule.clone());
    if let Some(r) = rule.as_ref() {
        if r.from_role.is_some() {
            log::debug!(
                "fire_if_consensus_for_instance: consensus rule for {}.{} has from_role — deferring until eligibility resolution ships; instance {}",
                instance.flow_name,
                instance.current_state,
                instance.instance_uri,
            );
            return Ok(None);
        }
    }

    let loaded = match crate::perspectives::flow_consensus::load_flow_transition_proposals(
        perspective,
        &instance.instance_uri,
    )
    .await
    {
        Ok(v) => v,
        Err(e) => {
            log::debug!(
                "fire_if_consensus_for_instance: load_flow_transition_proposals({}) failed: {e:#}",
                instance.instance_uri,
            );
            return Ok(None);
        }
    };
    if loaded.is_empty() {
        return Ok(None);
    }

    let aggregate = match crate::perspectives::flow_consensus::aggregate_flow_votes(
        &loaded,
        rule.as_ref(),
        None,
    ) {
        Ok(a) => a,
        Err(e) => {
            log::debug!(
                "fire_if_consensus_for_instance: aggregate_flow_votes({}) failed: {e:#}",
                instance.instance_uri,
            );
            return Ok(None);
        }
    };

    let Some(picked) = crate::perspectives::flow_consensus::select_fire_candidate(
        &instance.current_state,
        &aggregate,
    )
    .cloned() else {
        return Ok(None);
    };

    // Own the batch here so callers don't have to. Symmetric with
    // `run_flow_consensus_pass`: each firing is one atomic commit
    // unit; a commit failure discards the batch and yields Ok(None)
    // so the graph state is unchanged and the caller can retry.
    let batch_id = perspective.create_batch().await;
    match crate::perspectives::flow_consensus::fire_flow_consensus(
        perspective,
        &instance,
        &picked,
        Some(batch_id.clone()),
        context,
    )
    .await
    {
        Ok(outcome) => match perspective.commit_batch(batch_id.clone(), context).await {
            Ok(_) => Ok(Some(outcome)),
            Err(e) => {
                let _ = perspective.discard_batch(&batch_id).await;
                log::debug!(
                    "fire_if_consensus_for_instance: commit_batch for {} ({}→{}) failed: {e:#}",
                    instance.instance_uri,
                    picked.from_state,
                    picked.to_state,
                );
                Ok(None)
            }
        },
        Err(e) => {
            let _ = perspective.discard_batch(&batch_id).await;
            log::debug!(
                "fire_if_consensus_for_instance: fire_flow_consensus({}) failed: {e:#}",
                instance.instance_uri,
            );
            Ok(None)
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::{ModelQuery, ModelQueryCount};
    use std::collections::BTreeMap;

    fn mq(class: &str) -> ModelQuery {
        ModelQuery {
            class_name: class.to_string(),
            r#where: None,
            count: None,
            linked_to: None,
            did_property: None,
            or: None,
        }
    }

    // ---- evidence_hash ----

    #[test]
    fn evidence_hash_stable_across_id_permutations() {
        let classes = vec!["ns://Perspective".to_string()];
        let a = evidence_hash(&classes, &["b://2".into(), "a://1".into(), "c://3".into()]);
        let b = evidence_hash(&classes, &["a://1".into(), "b://2".into(), "c://3".into()]);
        let c = evidence_hash(&classes, &["c://3".into(), "a://1".into(), "b://2".into()]);
        assert_eq!(a, b);
        assert_eq!(a, c);
    }

    #[test]
    fn evidence_hash_differs_on_class_change() {
        let ids = vec!["a://1".into(), "b://2".into()];
        let a = evidence_hash(&["ns://Perspective".into()], &ids);
        let b = evidence_hash(&["ns://Tension".into()], &ids);
        assert_ne!(a, b);
    }

    #[test]
    fn evidence_hash_differs_on_id_diff() {
        let classes = vec!["ns://Perspective".into()];
        let a = evidence_hash(&classes, &["a://1".into()]);
        let b = evidence_hash(&classes, &["a://2".into()]);
        assert_ne!(a, b);
    }

    #[test]
    fn evidence_hash_hex_length() {
        // SHA256 is 32 bytes → 64 hex chars. Guards against a future
        // switch to a different digest silently changing the on-graph
        // shape.
        let h = evidence_hash(&["ns://X".into()], &[]);
        assert_eq!(h.len(), 64);
        assert!(h.chars().all(|c| c.is_ascii_hexdigit()));
    }

    /// Byte-for-byte golden fixtures locking parity with the TypeScript
    /// implementation in `core/src/perspectives/FlowEvidenceHash.ts`.
    ///
    /// The same expected hashes are asserted from the TS side in
    /// `core/src/perspectives/FlowEvidenceHash.test.ts` — a divergence in
    /// either language will fail both sides of the lock in the same PR.
    ///
    /// The fixtures were computed with `printf '<bytes>' | sha256sum`:
    /// - `printf 'ns://Perspective\0' | sha256sum`
    /// - `printf 'ns://X|ns://Y\0a\nb' | sha256sum`
    /// - `printf 'ns://X\0a://1' | sha256sum`
    #[test]
    fn evidence_hash_matches_ts_parity_fixtures() {
        assert_eq!(
            evidence_hash(&["ns://Perspective".into()], &[]),
            "2fa6bf06f407e1eeeda6f76b92285cdc2fd88feaaa141807aade362459990872",
        );
        assert_eq!(
            evidence_hash(
                &["ns://X".into(), "ns://Y".into()],
                &["b".into(), "a".into()]
            ),
            "5245f683b6dcc4efe4ce46e7b0126bd56a37c8794298c2213a335248a9383f66",
        );
        assert_eq!(
            evidence_hash(&["ns://X".into()], &["a://1".into()]),
            "dcbb3c36dba1ec498c46f6f6129ae78e6585a781abd1a89f8ac6d5f7c4a3e568",
        );
    }

    // ---- cardinality_satisfied ----

    #[test]
    fn cardinality_unset_requires_one_match() {
        assert!(!cardinality_satisfied(None, 0));
        assert!(cardinality_satisfied(None, 1));
        assert!(cardinality_satisfied(None, 100));
    }

    #[test]
    fn cardinality_min_only() {
        let c = ModelQueryCount {
            min: Some(2),
            max: None,
        };
        assert!(!cardinality_satisfied(Some(&c), 0));
        assert!(!cardinality_satisfied(Some(&c), 1));
        assert!(cardinality_satisfied(Some(&c), 2));
        assert!(cardinality_satisfied(Some(&c), 999));
    }

    #[test]
    fn cardinality_max_only() {
        let c = ModelQueryCount {
            min: None,
            max: Some(3),
        };
        assert!(cardinality_satisfied(Some(&c), 0));
        assert!(cardinality_satisfied(Some(&c), 3));
        assert!(!cardinality_satisfied(Some(&c), 4));
    }

    #[test]
    fn cardinality_range() {
        let c = ModelQueryCount {
            min: Some(1),
            max: Some(3),
        };
        assert!(!cardinality_satisfied(Some(&c), 0));
        assert!(cardinality_satisfied(Some(&c), 1));
        assert!(cardinality_satisfied(Some(&c), 2));
        assert!(cardinality_satisfied(Some(&c), 3));
        assert!(!cardinality_satisfied(Some(&c), 4));
    }

    #[test]
    fn cardinality_both_unset_object_accepts_zero() {
        // Distinct from `count = None`: the caller explicitly passed
        // `{}` — treat as "no bound at all", 0 is a legal count.
        let c = ModelQueryCount {
            min: None,
            max: None,
        };
        assert!(cardinality_satisfied(Some(&c), 0));
        assert!(cardinality_satisfied(Some(&c), 5));
    }

    #[test]
    fn cardinality_max_zero_negative_guard() {
        // "at most 0 matches" — a valid Popperian falsifier.
        let c = ModelQueryCount {
            min: None,
            max: Some(0),
        };
        assert!(cardinality_satisfied(Some(&c), 0));
        assert!(!cardinality_satisfied(Some(&c), 1));
    }

    // ---- build_query_input_for_requires ----

    #[test]
    fn build_query_bare_class_produces_empty_input() {
        // Just a className, no where / count / or → the guard becomes
        // "does this class have any instances?" No filter needed.
        let out = build_query_input_for_requires(&mq("ns://Perspective"), "did:key:acting");
        assert_eq!(out, json!({}));
    }

    #[test]
    fn build_query_where_scalar_shorthands() {
        let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w.insert("state".into(), PropertyCondition::Str("done".into()));
        w.insert("priority".into(), PropertyCondition::Num(3.0));
        w.insert("archived".into(), PropertyCondition::Bool(false));
        let q = ModelQuery {
            class_name: "ns://Task".into(),
            r#where: Some(w),
            ..mq("ns://Task")
        };
        let out = build_query_input_for_requires(&q, "did:key:acting");
        assert_eq!(
            out,
            json!({
                "where": {
                    "state": "done",
                    "priority": 3.0,
                    "archived": false,
                }
            })
        );
    }

    #[test]
    fn build_query_where_typed_operators() {
        let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w.insert(
            "author".into(),
            PropertyCondition::Equals {
                equals: json!("alice"),
            },
        );
        w.insert(
            "tag".into(),
            PropertyCondition::In {
                one_of: vec![json!("a"), json!("b")],
            },
        );
        w.insert(
            "title".into(),
            PropertyCondition::Matches {
                matches: r"^Q\d+".into(),
            },
        );
        w.insert(
            "deletedAt".into(),
            PropertyCondition::Exists { exists: false },
        );
        w.insert(
            "createdAt".into(),
            PropertyCondition::Exists { exists: true },
        );
        let q = ModelQuery {
            class_name: "ns://Thing".into(),
            r#where: Some(w),
            ..mq("ns://Thing")
        };
        let out = build_query_input_for_requires(&q, "did:key:acting");
        // Verify each field individually (BTreeMap order is deterministic
        // but readability matters more than terseness in the assertion).
        let where_ = out.get("where").unwrap();
        assert_eq!(where_.get("author"), Some(&json!("alice")));
        assert_eq!(where_.get("tag"), Some(&json!(["a", "b"])));
        assert_eq!(where_.get("title"), Some(&json!({"regex": "^Q\\d+"})));
        assert_eq!(
            where_.get("deletedAt"),
            Some(&json!({"equals": Value::Null}))
        );
        assert_eq!(
            where_.get("createdAt"),
            Some(&json!({"not": {"equals": Value::Null}}))
        );
    }

    #[test]
    fn build_query_did_property_substitutes_acting_did() {
        let q = ModelQuery {
            class_name: "ns://Endorsement".into(),
            did_property: Some("author".into()),
            ..mq("ns://Endorsement")
        };
        let did = "did:key:zAlice";
        let out = build_query_input_for_requires(&q, did);
        assert_eq!(out, json!({ "where": { "author": did } }));
    }

    #[test]
    fn build_query_did_property_combines_with_where() {
        let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w.insert("state".into(), PropertyCondition::Str("approved".into()));
        let q = ModelQuery {
            class_name: "ns://Review".into(),
            r#where: Some(w),
            did_property: Some("reviewer".into()),
            ..mq("ns://Review")
        };
        let out = build_query_input_for_requires(&q, "did:key:zBob");
        assert_eq!(
            out,
            json!({
                "where": {
                    "state": "approved",
                    "reviewer": "did:key:zBob",
                }
            })
        );
    }

    #[test]
    fn build_query_did_property_expression_substitutes_in_place() {
        // Escape hatch for hardcoded expressions ("agent:$did" etc.) —
        // we substitute $did in the string but the field-name column is
        // still the raw property (which model_query will reject with a
        // clear "no such property" — that's OK, this is a schema-level
        // typo the caller has to catch).
        let q = ModelQuery {
            class_name: "ns://Note".into(),
            did_property: Some("owner:$did".into()),
            ..mq("ns://Note")
        };
        let out = build_query_input_for_requires(&q, "did:key:zCarol");
        let field_val = out
            .get("where")
            .and_then(|w| w.get("owner:$did"))
            .expect("expression field preserved");
        assert_eq!(field_val, &json!("owner:did:key:zCarol"));
    }

    #[test]
    fn build_query_or_composes_to_subclauses() {
        let mut w1: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w1.insert("role".into(), PropertyCondition::Str("moderator".into()));
        let mut w2: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w2.insert("role".into(), PropertyCondition::Str("owner".into()));
        let q = ModelQuery {
            class_name: "ns://Membership".into(),
            or: Some(vec![
                ModelQuery {
                    class_name: "ns://Membership".into(),
                    r#where: Some(w1),
                    ..mq("ns://Membership")
                },
                ModelQuery {
                    class_name: "ns://Membership".into(),
                    r#where: Some(w2),
                    ..mq("ns://Membership")
                },
            ]),
            ..mq("ns://Membership")
        };
        let out = build_query_input_for_requires(&q, "did:key:acting");
        assert_eq!(
            out,
            json!({
                "where": {
                    "OR": [
                        { "role": "moderator" },
                        { "role": "owner" },
                    ]
                }
            })
        );
    }

    #[test]
    fn build_query_or_composes_with_top_level_where() {
        let mut top: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        top.insert(
            "channel".into(),
            PropertyCondition::Str("ch://alpha".into()),
        );
        let mut branch: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        branch.insert("role".into(), PropertyCondition::Str("owner".into()));
        let q = ModelQuery {
            class_name: "ns://Access".into(),
            r#where: Some(top),
            or: Some(vec![ModelQuery {
                class_name: "ns://Access".into(),
                r#where: Some(branch),
                ..mq("ns://Access")
            }]),
            ..mq("ns://Access")
        };
        let out = build_query_input_for_requires(&q, "did:key:acting");
        let where_ = out.get("where").unwrap();
        assert_eq!(where_.get("channel"), Some(&json!("ch://alpha")));
        let or_ = where_.get("OR").unwrap();
        assert_eq!(or_, &json!([{"role": "owner"}]));
    }

    #[test]
    fn build_query_or_empty_omitted() {
        let q = ModelQuery {
            class_name: "ns://X".into(),
            or: Some(vec![]),
            ..mq("ns://X")
        };
        let out = build_query_input_for_requires(&q, "did:key:acting");
        // Empty or-array → no OR key emitted (would otherwise be a
        // never-matches false-positive on model_query).
        assert_eq!(out, json!({}));
    }

    #[test]
    fn build_query_recursive_or_nests_subclauses() {
        // Two-level OR — mirrors §7.3 multi-role composition with a
        // fallback that itself has alternatives.
        let mut w_leaf: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w_leaf.insert("role".into(), PropertyCondition::Str("admin".into()));
        let leaf = ModelQuery {
            class_name: "ns://M".into(),
            r#where: Some(w_leaf),
            ..mq("ns://M")
        };
        let inner_or = ModelQuery {
            class_name: "ns://M".into(),
            or: Some(vec![leaf]),
            ..mq("ns://M")
        };
        let mut w_outer: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w_outer.insert("role".into(), PropertyCondition::Str("owner".into()));
        let outer = ModelQuery {
            class_name: "ns://M".into(),
            or: Some(vec![
                ModelQuery {
                    class_name: "ns://M".into(),
                    r#where: Some(w_outer),
                    ..mq("ns://M")
                },
                inner_or,
            ]),
            ..mq("ns://M")
        };
        let out = build_query_input_for_requires(&outer, "did:key:acting");
        // Outer OR carries two branches: {role: owner} and {OR: [{role: admin}]}
        let branches = out
            .get("where")
            .unwrap()
            .get("OR")
            .unwrap()
            .as_array()
            .unwrap();
        assert_eq!(branches.len(), 2);
        assert_eq!(branches[0], json!({"role": "owner"}));
        assert_eq!(branches[1], json!({"OR": [{"role": "admin"}]}));
    }

    // ============================================================================
    // Slice 10.4a2 — async layer tests (stubbed perspective)
    // ============================================================================
    //
    // These stub `RequiresQueryable` in-process so the evaluator's async
    // composition can be exercised deterministically without spinning up a
    // `PerspectiveInstance`. The end-to-end e2e_tests module below adds a
    // live-perspective integration test that pins the same behaviour against
    // the real SPARQL/Prolog/SDNA stack.

    use crate::perspectives::flow_context::FlowInstanceRecord;
    use crate::perspectives::shacl_parser::{FlowState, FlowTransition, LinkPattern, SHACLFlow};
    use std::collections::HashMap;
    use std::sync::Mutex;

    /// In-process stub. Records every `model_query` call for assertions,
    /// and either returns a canned JSON string or an error keyed on
    /// `class_name`.
    struct StubPerspective {
        // (class_name, query_json) recorded in call order.
        calls: Mutex<Vec<(String, String)>>,
        // class_name -> either a raw JSON response or an error string.
        responses: HashMap<String, Result<String, String>>,
    }

    impl StubPerspective {
        fn new() -> Self {
            Self {
                calls: Mutex::new(Vec::new()),
                responses: HashMap::new(),
            }
        }
        fn with_instances(mut self, class: &str, ids: &[&str]) -> Self {
            let payload = json!({
                "instances": ids.iter().map(|id| json!({"id": *id})).collect::<Vec<_>>(),
                "totalCount": ids.len(),
            })
            .to_string();
            self.responses.insert(class.to_string(), Ok(payload));
            self
        }
        fn with_error(mut self, class: &str, msg: &str) -> Self {
            self.responses
                .insert(class.to_string(), Err(msg.to_string()));
            self
        }
        fn call_count_for(&self, class: &str) -> usize {
            self.calls
                .lock()
                .unwrap()
                .iter()
                .filter(|(c, _)| c == class)
                .count()
        }
    }

    #[async_trait]
    impl RequiresQueryable for StubPerspective {
        async fn model_query(
            &self,
            class_name: &str,
            query_json: &str,
        ) -> Result<String, deno_core::anyhow::Error> {
            self.calls
                .lock()
                .unwrap()
                .push((class_name.to_string(), query_json.to_string()));
            match self.responses.get(class_name) {
                Some(Ok(payload)) => Ok(payload.clone()),
                Some(Err(msg)) => Err(deno_core::anyhow::anyhow!(msg.clone())),
                None => Err(deno_core::anyhow::anyhow!(
                    "StubPerspective: no canned response for `{}`",
                    class_name
                )),
            }
        }
    }

    // ---- evaluate_single_query ----

    #[tokio::test]
    async fn single_query_satisfied_with_unset_count_at_one_match() {
        let stub = StubPerspective::new().with_instances("ns://T", &["ad4m://t/1"]);
        let (ok, ids) = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
            .await
            .unwrap();
        assert!(ok);
        assert_eq!(ids, vec!["ad4m://t/1".to_string()]);
        assert_eq!(stub.call_count_for("ns://T"), 1);
    }

    #[tokio::test]
    async fn single_query_unsatisfied_when_zero_matches_and_unset_count() {
        let stub = StubPerspective::new().with_instances("ns://T", &[]);
        let (ok, ids) = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
            .await
            .unwrap();
        assert!(!ok);
        assert!(ids.is_empty());
    }

    #[tokio::test]
    async fn single_query_folds_cardinality_min_and_max() {
        let mut q = mq("ns://T");
        q.count = Some(ModelQueryCount {
            min: Some(2),
            max: Some(3),
        });

        // 2 matches → satisfied
        let stub = StubPerspective::new().with_instances("ns://T", &["a", "b"]);
        let (ok, _) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
        assert!(ok);

        // 1 match → unsatisfied (below min)
        let stub = StubPerspective::new().with_instances("ns://T", &["a"]);
        let (ok, _) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
        assert!(!ok);

        // 4 matches → unsatisfied (above max) but ids still returned
        let stub = StubPerspective::new().with_instances("ns://T", &["a", "b", "c", "d"]);
        let (ok, ids) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
        assert!(!ok);
        assert_eq!(ids.len(), 4);
    }

    #[tokio::test]
    async fn single_query_bubbles_perspective_error() {
        let stub = StubPerspective::new().with_error("ns://T", "SDNA class not registered");
        let err = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
            .await
            .unwrap_err();
        assert!(err.to_string().contains("SDNA class not registered"));
    }

    #[tokio::test]
    async fn single_query_errors_on_missing_instances_key() {
        let mut stub = StubPerspective::new();
        stub.responses.insert(
            "ns://T".to_string(),
            Ok(json!({"totalCount": 0}).to_string()),
        );
        let err = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
            .await
            .unwrap_err();
        assert!(err.to_string().contains("missing `instances`"));
    }

    #[tokio::test]
    async fn single_query_serializes_translated_input() {
        // Guards that the async layer forwards the pure translator's
        // output verbatim — a regression in either half would show up
        // here as a diff on the recorded query JSON.
        let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
        w.insert(
            "author".into(),
            PropertyCondition::Str("did:key:xyz".into()),
        );
        let q = ModelQuery {
            class_name: "ns://T".into(),
            r#where: Some(w),
            ..mq("ns://T")
        };
        let stub = StubPerspective::new().with_instances("ns://T", &["ad4m://t/1"]);
        let _ = evaluate_single_query(&stub, &q, "did:key:acting")
            .await
            .unwrap();
        let calls = stub.calls.lock().unwrap();
        assert_eq!(calls.len(), 1);
        let recorded: Value = serde_json::from_str(&calls[0].1).unwrap();
        assert_eq!(recorded, json!({"where": {"author": "did:key:xyz"}}));
    }

    // ---- evaluate_state_requires ----

    #[tokio::test]
    async fn state_requires_empty_returns_some_empty_evidence() {
        let stub = StubPerspective::new();
        let out = evaluate_state_requires(&stub, &[], "did:key:x")
            .await
            .unwrap();
        assert_eq!(out, Some((Vec::new(), Vec::new())));
        // No calls should be made — vacuous truth.
        assert_eq!(stub.calls.lock().unwrap().len(), 0);
    }

    #[tokio::test]
    async fn state_requires_and_short_circuits_on_first_unsat() {
        let stub = StubPerspective::new()
            .with_instances("ns://A", &["ad4m://a/1"])
            .with_instances("ns://B", &[]) // fails the AND
            .with_instances("ns://C", &["ad4m://c/1"]);
        let requires = vec![mq("ns://A"), mq("ns://B"), mq("ns://C")];
        let out = evaluate_state_requires(&stub, &requires, "did:key:x")
            .await
            .unwrap();
        assert_eq!(out, None);
        // A + B queried, C skipped after B failed.
        assert_eq!(stub.call_count_for("ns://A"), 1);
        assert_eq!(stub.call_count_for("ns://B"), 1);
        assert_eq!(stub.call_count_for("ns://C"), 0);
    }

    #[tokio::test]
    async fn state_requires_unions_evidence_and_dedups_across_queries() {
        // Same instance URI returned by two different guards; classes de-dup
        // and IDs de-dup while preserving first-occurrence order.
        let stub = StubPerspective::new()
            .with_instances("ns://A", &["ad4m://x/1", "ad4m://x/2"])
            .with_instances("ns://B", &["ad4m://x/2", "ad4m://x/3"]);
        let requires = vec![mq("ns://A"), mq("ns://B"), mq("ns://A")];
        let out = evaluate_state_requires(&stub, &requires, "did:key:x")
            .await
            .unwrap();
        let (classes, ids) = out.expect("all guards satisfied");
        assert_eq!(classes, vec!["ns://A".to_string(), "ns://B".to_string()]);
        assert_eq!(
            ids,
            vec![
                "ad4m://x/1".to_string(),
                "ad4m://x/2".to_string(),
                "ad4m://x/3".to_string()
            ]
        );
    }

    #[tokio::test]
    async fn state_requires_bubbles_query_error() {
        let stub = StubPerspective::new()
            .with_instances("ns://A", &["ad4m://a/1"])
            .with_error("ns://B", "SDNA class not registered");
        let requires = vec![mq("ns://A"), mq("ns://B")];
        let err = evaluate_state_requires(&stub, &requires, "did:key:x")
            .await
            .unwrap_err();
        assert!(err.to_string().contains("SDNA class not registered"));
    }

    // ---- evaluate_flow_transitions ----

    fn simple_flow(name: &str, transitions: &[(&str, &str)]) -> SHACLFlow {
        let mut states: Vec<FlowState> = Vec::new();
        let mut seen = HashSet::new();
        for (from, to) in transitions {
            for s in [from, to] {
                if seen.insert(s.to_string()) {
                    states.push(FlowState {
                        name: s.to_string(),
                        value: 0.0,
                        state_check: LinkPattern {
                            source: None,
                            predicate: format!("{}://state", name.to_lowercase()),
                            target: format!("{}://{}", name.to_lowercase(), s),
                        },
                        interpretation_hint: None,
                        requires: None,
                        semantic_check: None,
                        consensus_rule: None,
                    });
                }
            }
        }
        SHACLFlow {
            name: name.to_string(),
            namespace: format!("{}://", name.to_lowercase()),
            start_action: Vec::new(),
            states,
            transitions: transitions
                .iter()
                .map(|(f, t)| FlowTransition {
                    action_name: format!("{}To{}", f, t),
                    from_state: f.to_string(),
                    to_state: t.to_string(),
                    actions: Vec::new(),
                })
                .collect(),
            interpretation_hint: None,
            input_types: Vec::new(),
            output_types: Vec::new(),
            creation_hint: None,
            context: None,
            consensus_rule: None,
        }
    }

    fn set_requires(state: &mut FlowState, requires: Vec<ModelQuery>) {
        state.requires = Some(requires);
    }

    fn record(flow: &str, uri: &str, subject: &str, state: &str) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_name: flow.into(),
            instance_uri: uri.into(),
            subject: subject.into(),
            current_state: state.into(),
            started_at: None,
        }
    }

    #[tokio::test]
    async fn flow_transitions_emits_one_satisfied_per_reachable_state() {
        let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
        let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
        set_requires(scoped, vec![mq("ns://Task")]);
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![record(
            "Delivery",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        )];
        let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].flow_name, "Delivery");
        assert_eq!(out[0].instance_uri, "ad4m://flow/instance/1");
        assert_eq!(out[0].subject, "ad4m://task/1");
        assert_eq!(out[0].from_state, "identified");
        assert_eq!(out[0].to_state, "scoped");
        assert_eq!(out[0].evidence_ids, vec!["ad4m://task/1".to_string()]);
        assert_eq!(out[0].evidence_hash.len(), 64);
    }

    #[tokio::test]
    async fn flow_transitions_skips_state_when_requires_unsatisfied() {
        let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
        let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
        set_requires(scoped, vec![mq("ns://Task")]);
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![record(
            "Delivery",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        )];
        let stub = StubPerspective::new().with_instances("ns://Task", &[]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert!(out.is_empty());
    }

    #[tokio::test]
    async fn flow_transitions_skips_states_without_requires() {
        // No `requires` = no deterministic guard; slice 10.5's semanticCheck
        // is a separate concern and doesn't fire here.
        let flow = simple_flow("Delivery", &[("identified", "scoped")]);
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![record(
            "Delivery",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        )];
        let stub = StubPerspective::new();
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert!(out.is_empty());
        // No query should have run — no guards.
        assert_eq!(stub.calls.lock().unwrap().len(), 0);
    }

    #[tokio::test]
    async fn flow_transitions_skips_records_with_unknown_flow_name() {
        // Definition unpublished or not yet synced — must not blow up.
        let flow = simple_flow("Delivery", &[("identified", "scoped")]);
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![
            record(
                "Delivery",
                "ad4m://flow/instance/1",
                "ad4m://task/1",
                "identified",
            ),
            record("Unknown", "ad4m://flow/instance/2", "ad4m://task/2", "some"),
        ];
        let stub = StubPerspective::new();
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert!(out.is_empty());
    }

    #[tokio::test]
    async fn flow_transitions_swallows_query_error_at_debug_and_continues() {
        // A `requires` on state A errors; state B in another record should
        // still be evaluated. Guards against one bad shape poisoning the
        // whole pass.
        let mut delivery = simple_flow("Delivery", &[("identified", "scoped")]);
        let scoped = delivery
            .states
            .iter_mut()
            .find(|s| s.name == "scoped")
            .unwrap();
        set_requires(scoped, vec![mq("ns://Broken")]);
        let mut deliberation = simple_flow("Deliberation", &[("proposal", "tension")]);
        let tension = deliberation
            .states
            .iter_mut()
            .find(|s| s.name == "tension")
            .unwrap();
        set_requires(tension, vec![mq("ns://Perspective")]);
        let flows = HashMap::from([
            ("Delivery".into(), delivery),
            ("Deliberation".into(), deliberation),
        ]);
        let recs = vec![
            record(
                "Delivery",
                "ad4m://flow/instance/1",
                "ad4m://task/1",
                "identified",
            ),
            record(
                "Deliberation",
                "ad4m://flow/instance/2",
                "ad4m://proposal/1",
                "proposal",
            ),
        ];
        let stub = StubPerspective::new()
            .with_error("ns://Broken", "unregistered class")
            .with_instances("ns://Perspective", &["ad4m://persp/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].flow_name, "Deliberation");
        assert_eq!(out[0].to_state, "tension");
    }

    #[tokio::test]
    async fn flow_transitions_uses_state_consensus_over_flow_default() {
        let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
        flow.consensus_rule = Some(ConsensusRule {
            n: 1,
            from_role: None,
        });
        let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
        set_requires(scoped, vec![mq("ns://Task")]);
        scoped.consensus_rule = Some(ConsensusRule {
            n: 3,
            from_role: None,
        });
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![record(
            "Delivery",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        )];
        let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].consensus_rule.as_ref().unwrap().n, 3);
    }

    #[tokio::test]
    async fn flow_transitions_falls_back_to_flow_consensus_when_state_unset() {
        let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
        flow.consensus_rule = Some(ConsensusRule {
            n: 2,
            from_role: None,
        });
        let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
        set_requires(scoped, vec![mq("ns://Task")]);
        let flows = HashMap::from([("Delivery".into(), flow)]);
        let recs = vec![record(
            "Delivery",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        )];
        let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].consensus_rule.as_ref().unwrap().n, 2);
    }
}

// ============================================================================
// Slice 10.4a3 — live-perspective integration test
// ============================================================================
//
// The stub-perspective tests above pin every failure mode inside the
// composer (unknown flow, guardless state, empty requires, unsatisfied
// cardinality, `or` composition, per-state consensus override, query
// error) but they route around the real `PerspectiveInstance::model_query`
// implementation. What they *cannot* prove is that the
// `ModelQuery → ModelQueryInput` translation `build_query_input_for_requires`
// emits is a shape the real SPARQL/SDNA-backed `model_query` actually
// accepts. This module closes that gap with one end-to-end pass against
// a genuine perspective (real store, real Prolog, real `add_sdna`, real
// `create_subject`, real `model_query`).
//
// Complements the 10.3d test in `flow_context.rs`:
//   - 10.3d: read-side integration — definitions + minted instance
//            → `FlowContext[]` + rendered prompt block.
//   - 10.4a3 (this): write-side integration — same substrate → committed
//            evidence flowing back through `model_query` → the
//            deterministic `SatisfiedTransition[]` that slice 10.4b will
//            turn into on-graph `FlowTransitionProposal` writes.
//
// No LLM is spun up.
#[cfg(test)]
mod e2e_tests {
    use super::*;
    use crate::perspectives::flow_classes::mint_flow_instance;
    use crate::perspectives::flow_context::{load_flow_instances, load_shacl_flows};
    use crate::perspectives::interpretation_test_support::{
        seed_instance, setup_perspective_no_llm, TASK_SDNA,
    };
    use crate::perspectives::shacl_parser::parse_flow_to_links;
    use crate::types::{Link, LinkStatus};

    /// URL-encoded string-literal target, matching the wire shape the
    /// slice 10.3a reader decodes.
    fn lit(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    /// Minimal two-state Delivery flow: `identified → scoped`. The
    /// `requires` guard on `scoped` (at least one `ns://Task`) is
    /// appended as a v5 link *after* `parse_flow_to_links` emits the
    /// v4 predicates — same seeding pattern as the 10.3d e2e test.
    fn delivery_flow_json() -> String {
        serde_json::json!({
            "name": "Delivery",
            "namespace": "delivery://",
            "start_action": [],
            "states": [
                {
                    "name": "identified",
                    "value": 0.0,
                    "state_check": {
                        "source": null,
                        "predicate": "delivery://state",
                        "target": "delivery://identified"
                    }
                },
                {
                    "name": "scoped",
                    "value": 0.5,
                    "state_check": {
                        "source": null,
                        "predicate": "delivery://state",
                        "target": "delivery://scoped"
                    }
                }
            ],
            "transitions": [
                {
                    "action_name": "Scope",
                    "from_state": "identified",
                    "to_state": "scoped",
                    "actions": []
                }
            ],
        })
        .to_string()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn evaluate_flow_transitions_wires_definition_and_evidence_e2e() {
        // 1) Real perspective, no LLM, `ns://Task` registered via
        //    `add_sdna` — this is the same path the interpretation
        //    pipeline uses, so `model_query("ns://Task", ...)` will
        //    resolve against the real SPARQL store.
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        // 2) Seed the Delivery flow definition. The v4 predicates
        //    (type / flowName / hasState / hasTransition / …) go
        //    through the writer; the one v5 predicate we need — the
        //    `requires` link on the `scoped` state — is added by hand
        //    because `parse_flow_to_links` does not yet emit v5. The
        //    reader (slice 10.3a) already walks the v5 shape, and the
        //    evaluator needs to consume it today; this test pins that
        //    contract until the writer catches up.
        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        // Cardinality `{ min: 1 }` is deliberate — proves the guard
        // has genuinely got teeth (0 = unmet, 1 = met) rather than
        // trivially satisfied by unset-count-defaults.
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");

        // 3) Mint a FlowInstance in `identified` on a base URI. Same
        //    call site the auto-processor / Model C write-side will
        //    use.
        let base_uri = "ad4m://task/onboarding";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-inst-1",
            "2026-08-26T21:30:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // 4) Load records + catalogue exactly as `run.rs` will after
        //    slice 10.4b. Same shape 10.3d exercised on the read side,
        //    but this time both are fed into the *write*-side gate.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances");
        assert_eq!(records.len(), 1, "one active FlowInstance ⇒ one record");
        let flows_by_name = load_shacl_flows(&perspective)
            .await
            .expect("load_shacl_flows");
        assert_eq!(
            flows_by_name.len(),
            1,
            "one Delivery definition ⇒ one catalogue entry"
        );
        // Reader guarantee: the hand-seeded v5 `requires` link must
        // survive the round-trip back into a `ModelQuery[]` — otherwise
        // the evaluator would see `None` and silent-skip regardless of
        // the graph state, which would make the negative path below a
        // false positive.
        let scoped = flows_by_name
            .get("Delivery")
            .expect("Delivery in catalogue")
            .states
            .iter()
            .find(|s| s.name == "scoped")
            .expect("scoped state parsed");
        let reqs = scoped
            .requires
            .as_deref()
            .expect("scoped.requires decoded from v5 link");
        assert_eq!(reqs.len(), 1);
        assert_eq!(reqs[0].class_name, "ns://Task");

        // 5) Negative pass: no Task in the graph ⇒ requires unmet ⇒
        //    zero satisfied transitions. Proves the "unmet" branch is
        //    reached through the real SPARQL/SDNA stack, not just the
        //    stub.
        let before =
            evaluate_flow_transitions(&perspective, &records, &flows_by_name, "did:key:acting")
                .await;
        assert!(
            before.is_empty(),
            "no Task instances ⇒ requires unmet ⇒ 0 satisfied, got {before:?}"
        );

        // 6) Seed a real Task via the same `create_subject` path the
        //    interpretation pipeline uses. This is what makes the
        //    positive assertion below a genuine end-to-end proof: the
        //    evidence has to come out of the store the evaluator
        //    queries, not out of a canned stub response.
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/1",
            "Onboard Ana",
        )
        .await;

        // 7) Positive pass: one FlowInstance × one reachable next-state
        //    × requires met ⇒ exactly one SatisfiedTransition.
        let after =
            evaluate_flow_transitions(&perspective, &records, &flows_by_name, "did:key:acting")
                .await;
        assert_eq!(
            after.len(),
            1,
            "requires met ⇒ 1 satisfied transition, got {after:?}"
        );
        let t = &after[0];
        assert_eq!(t.flow_name, "Delivery");
        assert_eq!(t.instance_uri, inst_uri);
        assert_eq!(t.subject, base_uri);
        assert_eq!(t.from_state, "identified");
        assert_eq!(t.to_state, "scoped");
        assert!(
            t.evidence_ids.contains(&"ad4m://task/1".to_string()),
            "evidence_ids must include the seeded Task URI, got {:?}",
            t.evidence_ids
        );
        assert_eq!(
            t.evidence_hash,
            evidence_hash(&["ns://Task".to_string()], &t.evidence_ids),
            "evidence_hash must be a deterministic seal over (class_names, evidence_ids)"
        );
        // Flow definition carries no per-state semanticCheck and no
        // consensus rule (flow-level or per-state) — both must fall
        // through unset. Slice 10.5 exercises the semanticCheck path;
        // the stub tests above cover consensus override precedence.
        assert!(t.semantic_check.is_none());
        assert!(t.consensus_rule.is_none());
    }

    /// Slice 10.4b — write-side end-to-end. Re-uses the 10.4a3 fixture:
    /// real perspective, Delivery flow with `requires: 1 × ns://Task`,
    /// one active FlowInstance, one seeded Task ⇒ one
    /// `SatisfiedTransition`. On top of that, this test calls
    /// [`write_engine_proposal`] and asserts every declared
    /// FlowTransitionProposal predicate landed on-graph with the
    /// expected target.
    ///
    /// Assertions cover the two silent-failure modes the writer has to
    /// rule out:
    /// - **Wrong-key drop** — 2026-08-20 bug shape where a mismatched
    ///   JSON key returns Ok without writing. The parity test in
    ///   `flow_classes.rs` guards static shape; this test proves the
    ///   dynamic write path is honest.
    /// - **Collection under-write** — `create_subject` writing only the
    ///   first collection element (last-one-wins on a `setSingleTarget`
    ///   setter). The write path fans out subsequent elements through
    ///   `update_subject`; this test seeds 2 Tasks to exercise
    ///   fan-out and asserts both `flow/evidence` targets land.
    #[tokio::test(flavor = "multi_thread")]
    async fn write_engine_proposal_lands_all_declared_predicates_e2e() {
        use crate::types::LinkQuery;

        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        // Seed the Delivery flow definition (v4 predicates + hand-added
        // v5 requires link) exactly as the read-side test above.
        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");

        let base_uri = "ad4m://task/onboarding";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-inst-writer",
            "2026-08-27T00:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Seed TWO Tasks so the evidence collection has more than one
        // element — proves the create_subject + update_subject fan-out
        // (not a single-target overwrite) actually lands both.
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/1",
            "Onboard Ana",
        )
        .await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/2",
            "Onboard Bo",
        )
        .await;

        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances");
        let flows_by_name = load_shacl_flows(&perspective)
            .await
            .expect("load_shacl_flows");
        let satisfied =
            evaluate_flow_transitions(&perspective, &records, &flows_by_name, "did:key:acting")
                .await;
        assert_eq!(
            satisfied.len(),
            1,
            "one active FlowInstance × one reachable next-state × requires met ⇒ 1"
        );
        let t = &satisfied[0];
        assert_eq!(
            t.evidence_ids.len(),
            2,
            "two seeded Tasks ⇒ two evidence entries, got {:?}",
            t.evidence_ids
        );

        // Write the proposal via the convenience wrapper. Caller-supplied
        // id + timestamp mirror the mint_flow_instance contract.
        let proposer_did = "did:key:acting";
        let proposed_at = "2026-08-27T00:05:00Z";
        let proposal_uri = write_engine_proposal(
            &mut perspective,
            "e2e-prop-1",
            proposer_did,
            proposed_at,
            t,
            None, // rationale (slice 10.6c) — this e2e is the engine-only path
            None,
            &ctx,
        )
        .await
        .expect("write_engine_proposal");
        assert_eq!(
            proposal_uri, "ad4m://flow/proposal/e2e-prop-1",
            "URI scheme mirrors flow_transition_proposal_uri",
        );

        // Query every outgoing link from the proposal URI and index by
        // predicate. Every one of the seven declared SDNA properties
        // must have at least one link landed.
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(proposal_uri.clone()),
                ..Default::default()
            })
            .await
            .expect("get_links(proposal)");
        let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
        for l in &links {
            if let Some(pred) = &l.data.predicate {
                by_pred
                    .entry(pred.clone())
                    .or_default()
                    .push(l.data.target.clone());
            }
        }

        // URI-valued predicate: safe-IRI ⇒ stored raw, no literal wrap.
        let inst_targets = by_pred
            .get("ad4m://flow/instance")
            .expect("proposal must carry flow/instance link");
        assert!(
            inst_targets.contains(&inst_uri),
            "flow/instance target must be the minted instance URI, got {inst_targets:?}",
        );

        // Evidence: two safe-IRI targets ⇒ both stored raw, both must land.
        let evidence_targets = by_pred
            .get("ad4m://flow/evidence")
            .expect("proposal must carry flow/evidence links");
        for expected in ["ad4m://task/1", "ad4m://task/2"] {
            assert!(
                evidence_targets.iter().any(|t| t == expected),
                "flow/evidence collection must include `{expected}`, got {evidence_targets:?} \
                 — write_flow_transition_proposal's create_subject + update_subject fan-out \
                 must land every element (regression guard for silent last-one-wins)",
            );
        }

        // The proposer is a `did:key:…` URI. `looks_like_absolute_iri`
        // accepts it (starts with ASCII letter, has a colon), so
        // `resolve_property_value` stores it raw — not literal-wrapped.
        // Guarding both branches of the writer's encoding here so a
        // future is_safe_iri_target tweak that changes DID handling is
        // caught by test rather than by a downstream DID-lookup failure.
        let proposer_targets = by_pred
            .get("ad4m://flow/proposer")
            .expect("proposal must carry flow/proposer link");
        assert!(
            proposer_targets.iter().any(|t| t == proposer_did),
            "flow/proposer target must be the raw DID URI, got {proposer_targets:?}",
        );

        // Non-IRI string predicates: `literal:string:` wrapped because
        // they either start with a digit (timestamp), contain no `:`
        // (state names / hex hash), or otherwise fail is_safe_iri_target.
        // Assert the exact wire form so a future encoding change is
        // caught here (and reviewers reading a proposal on-graph can
        // tell at a glance which fields are typed literals).
        for (pred, expected) in [
            ("ad4m://flow/from_state", "identified"),
            ("ad4m://flow/to_state", "scoped"),
            ("ad4m://flow/evidence_hashes", t.evidence_hash.as_str()),
            ("ad4m://flow/created_at", proposed_at),
        ] {
            let targets = by_pred
                .get(pred)
                .unwrap_or_else(|| panic!("proposal must carry {pred} link"));
            let want = format!("literal:string:{}", urlencoding::encode(expected));
            assert!(
                targets.iter().any(|t| t == &want),
                "{pred} target must be `{want}`, got {targets:?}",
            );
        }
    }

    /// Slice 10.4c — the end-to-end onion shell for the auto-processor
    /// entry point. Verifies that a single call to
    /// [`run_engine_proposal_pass`] against a live perspective:
    ///
    /// 1. Loads flows + records + evaluates + writes without any
    ///    caller-side plumbing.
    /// 2. Returns the URIs of every minted proposal.
    /// 3. Actually lands each proposal on-graph with the acting DID
    ///    as `proposer` — a per-transition detail that would silently
    ///    fail if `did_for_context` were bypassed.
    ///
    /// This is the shape `interpretation::run::run_interpretation_with_strategy_and_model`
    /// calls after `apply_with_overlay`. If this test passes, the
    /// extraction pass becomes flow-post-processing-aware end-to-end.
    #[tokio::test(flavor = "multi_thread")]
    async fn run_engine_proposal_pass_lands_a_proposal_e2e() {
        use crate::types::LinkQuery;

        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        // Same Delivery + `requires` seed as the evaluator + writer
        // e2e tests. `requires: 1 × ns://Task` on the `scoped` state,
        // so an unseeded graph = 0 proposals, one seeded Task = 1.
        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");

        let base_uri = "ad4m://task/onboarding";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.4c-inst",
            "2026-08-27T02:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Empty graph — no Task seeded — must return zero proposals.
        // Proves the pass is genuinely gated on the guard's satisfaction
        // and doesn't optimistically mint on every pass.
        let before = crate::perspectives::flow_evaluator::run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            None,
            &[], // llm_hints (slice 10.6c) — engine-only path
        )
        .await;
        assert!(
            before.is_empty(),
            "empty graph ⇒ 0 satisfied ⇒ 0 proposals, got {before:?}",
        );

        // Now seed the evidence — the exact same `create_subject` path
        // the interpretation pipeline uses, so the eventual proposal
        // reflects real committed graph state, not a canned response.
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/1",
            "Onboard Ana",
        )
        .await;

        let minted = crate::perspectives::flow_evaluator::run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            None,
            &[], // llm_hints (slice 10.6c) — engine-only path
        )
        .await;
        assert_eq!(
            minted.len(),
            1,
            "one satisfied transition ⇒ one proposal, got {minted:?}",
        );
        let proposal_uri = &minted[0];
        assert!(
            proposal_uri.starts_with("ad4m://flow/proposal/"),
            "proposal URI must follow flow_transition_proposal_uri scheme, got {proposal_uri}",
        );

        // Resolve the acting DID the same way `run_engine_proposal_pass`
        // did internally, so the assertion below tests actual identity
        // threading rather than tolerating any DID that happens to land.
        let acting_did =
            crate::agent::did_for_context(&ctx).expect("did_for_context on test agent context");

        // Walk the proposal on-graph and confirm proposer + linked
        // instance + from/to states. The `write_engine_proposal` e2e
        // test above already covers every declared predicate; here we
        // spot-check the fields that would silently regress if the
        // acting-DID plumbing broke or `run_engine_proposal_pass`
        // truncated the SatisfiedTransition it hands to the writer.
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(proposal_uri.clone()),
                ..Default::default()
            })
            .await
            .expect("get_links(proposal)");
        let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
        for l in &links {
            if let Some(pred) = &l.data.predicate {
                by_pred
                    .entry(pred.clone())
                    .or_default()
                    .push(l.data.target.clone());
            }
        }

        assert!(
            by_pred
                .get("ad4m://flow/proposer")
                .map(|ts| ts.iter().any(|t| t == &acting_did))
                .unwrap_or(false),
            "proposer must be the acting DID resolved via did_for_context, \
             got {:?}",
            by_pred.get("ad4m://flow/proposer"),
        );
        assert!(
            by_pred
                .get("ad4m://flow/instance")
                .map(|ts| ts.iter().any(|t| t == &inst_uri))
                .unwrap_or(false),
            "flow/instance must be the minted instance URI, got {:?}",
            by_pred.get("ad4m://flow/instance"),
        );
        for (pred, expected) in [
            ("ad4m://flow/from_state", "identified"),
            ("ad4m://flow/to_state", "scoped"),
        ] {
            let want = format!("literal:string:{}", urlencoding::encode(expected));
            assert!(
                by_pred
                    .get(pred)
                    .map(|ts| ts.iter().any(|t| t == &want))
                    .unwrap_or(false),
                "{pred} must carry `{want}`, got {:?}",
                by_pred.get(pred),
            );
        }
    }

    // -----------------------------------------------------------------
    // Slice 10.5b — the semantic-check gate wired into
    // `run_engine_proposal_pass`.
    // -----------------------------------------------------------------

    /// Stub [`SemanticCheckLlm`] whose `confirm` returns a canned response
    /// verbatim. Records prompt + model_id per call so the tests can
    /// assert the gate is threading the right context down to the LLM
    /// (same pattern the async-layer unit tests in
    /// `flow_semantic_check` use).
    struct CannedLlm {
        response: String,
        error: Option<String>,
        calls: std::sync::Mutex<Vec<(String, String)>>, // (model_id, prompt)
    }

    impl CannedLlm {
        fn responding(text: &str) -> Self {
            Self {
                response: text.to_string(),
                error: None,
                calls: std::sync::Mutex::new(Vec::new()),
            }
        }
        fn erroring(msg: &str) -> Self {
            Self {
                response: String::new(),
                error: Some(msg.to_string()),
                calls: std::sync::Mutex::new(Vec::new()),
            }
        }
        fn call_count(&self) -> usize {
            self.calls.lock().unwrap().len()
        }
    }

    #[async_trait::async_trait]
    impl crate::perspectives::flow_semantic_check::SemanticCheckLlm for CannedLlm {
        async fn confirm(&self, model_id: &str, prompt: &str) -> anyhow::Result<String> {
            self.calls
                .lock()
                .unwrap()
                .push((model_id.to_string(), prompt.to_string()));
            if let Some(msg) = &self.error {
                return Err(anyhow::anyhow!(msg.clone()));
            }
            Ok(self.response.clone())
        }
    }

    /// Seed the same Delivery + `requires` + FlowInstance shape the
    /// 10.4c e2e uses, PLUS a per-state `ad4m://semantic_check` hint on
    /// `scoped`. Returns `(perspective, ctx, instance_uri)`.
    async fn seed_semantic_check_e2e_fixture(
        semantic_check_hint: &str,
    ) -> (
        crate::perspectives::perspective_instance::PerspectiveInstance,
        crate::agent::AgentContext,
        String,
    ) {
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");
        // The 10.5b payload — per-state semanticCheck hint (predicate is
        // `ad4m://semanticCheck` in camelCase to match the parser at
        // `shacl_parser::find_link`). Parser (slice 10.3a) reads this
        // and mounts it on `FlowState.semantic_check`, which
        // `evaluate_flow_transitions` threads into
        // `SatisfiedTransition.semantic_check`, which
        // `build_semantic_check_prompt` uses to produce a non-`None`
        // prompt — which is the whole reason `run_semantic_check`
        // actually calls the LLM here.
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://semanticCheck".to_string()),
                    target: lit(semantic_check_hint),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.semanticCheck)");

        let base_uri = "ad4m://task/onboarding-10.5b";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.5b-inst",
            "2026-08-27T02:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Seed the Task that satisfies `requires` — same
        // `create_subject` path the interpretation pipeline uses. The
        // deterministic guard is now met; the SEMANTIC-CHECK gate is the
        // only thing between the transition and the on-graph proposal.
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/1",
            "Onboard Ana",
        )
        .await;

        (perspective, ctx, inst_uri)
    }

    /// Semantic-check `Pass` (LLM returns "YES") ⇒ proposal fires,
    /// exactly one LLM call, prompt was threaded through with the
    /// correct model_id.
    #[tokio::test(flavor = "multi_thread")]
    async fn semantic_check_pass_fires_proposal_e2e() {
        let (mut perspective, ctx, _inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        let llm = CannedLlm::responding("YES");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &[],
        )
        .await;
        assert_eq!(minted.len(), 1, "Pass verdict ⇒ 1 proposal, got {minted:?}",);
        assert_eq!(
            llm.call_count(),
            1,
            "gated transition ⇒ exactly one semantic-check LLM call, got {}",
            llm.call_count(),
        );
        let (model_id, prompt) = llm.calls.lock().unwrap()[0].clone();
        assert_eq!(model_id, "test-model-42", "model_id must thread through");
        assert!(
            prompt.contains("scoped") || prompt.to_lowercase().contains("scope"),
            "prompt must mention the target state / hint, got: {prompt}",
        );
    }

    /// Semantic-check `Fail` (LLM returns "NO") ⇒ transition is
    /// discarded despite `requires` being satisfied. This is the
    /// load-bearing property: an uncertain LLM cannot silently advance
    /// a flow even when the deterministic guard says it could.
    #[tokio::test(flavor = "multi_thread")]
    async fn semantic_check_fail_discards_transition_e2e() {
        let (mut perspective, ctx, _inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        let llm = CannedLlm::responding("NO");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &[],
        )
        .await;
        assert!(
            minted.is_empty(),
            "Fail verdict ⇒ 0 proposals despite requires-satisfied, got {minted:?}",
        );
        assert_eq!(
            llm.call_count(),
            1,
            "gate must still make the LLM call before deciding, got {}",
            llm.call_count(),
        );
    }

    /// Semantic-check LLM error ⇒ transition is discarded (fail-safe:
    /// the flow layer must never break the extraction pass, and an
    /// erroring LLM is treated the same as `Fail`).
    #[tokio::test(flavor = "multi_thread")]
    async fn semantic_check_llm_error_discards_transition_e2e() {
        let (mut perspective, ctx, _inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        let llm = CannedLlm::erroring("simulated LLM outage");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &[],
        )
        .await;
        assert!(
            minted.is_empty(),
            "LLM error ⇒ 0 proposals (fail-safe), got {minted:?}",
        );
        assert_eq!(
            llm.call_count(),
            1,
            "gate must attempt the LLM call before deciding, got {}",
            llm.call_count(),
        );
    }

    /// A transition WITHOUT a `semantic_check` hint ⇒ the LLM is not
    /// called even when the gate is enabled (auto-pass short-circuit),
    /// and the proposal still fires. Locks the "hint absent = no LLM
    /// spend" invariant that keeps the gate cheap on flows without
    /// explicit semantic checks.
    #[tokio::test(flavor = "multi_thread")]
    async fn semantic_check_absent_hint_autopasses_no_llm_call_e2e() {
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        // Same shape as the 10.4c fixture — Delivery + requires on
        // scoped, NO semantic_check link on any state.
        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");
        let _ = mint_flow_instance(
            &mut perspective,
            "Delivery",
            "ad4m://task/no-hint",
            "identified",
            "e2e-10.5b-no-hint-inst",
            "2026-08-27T02:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/nohint-1",
            "Onboard Ana",
        )
        .await;

        // An "erroring" LLM: if the gate ever calls it, the whole
        // transition would be discarded (per the LLM-error test above).
        // Since the hint is absent, `build_semantic_check_prompt`
        // returns None ⇒ `run_semantic_check` short-circuits to `Pass`
        // WITHOUT calling `.confirm()`. The proposal must still fire
        // and the LLM must record zero calls.
        let llm = CannedLlm::erroring("gate must not call me — no hint on this transition");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &[],
        )
        .await;
        assert_eq!(minted.len(), 1, "auto-pass ⇒ 1 proposal, got {minted:?}",);
        assert_eq!(
            llm.call_count(),
            0,
            "auto-pass short-circuit ⇒ zero LLM calls, got {}",
            llm.call_count(),
        );
    }

    // ---------------------------------------------------------------------
    // Slice 10.6c — LlmProposalHint matching / rationale attribution
    // ---------------------------------------------------------------------

    /// Helper: read the `ad4m://flow/rationale` link off a proposal URI and
    /// return the decoded scalar value (or `None` if the property is absent).
    /// The writer stores scalars as `literal:string:...` targets, so decode
    /// by stripping the prefix. Kept local — the assertion shape (was the
    /// rationale predicate set at all + does its value round-trip) is
    /// specific enough that inlining across three tests would obscure it.
    async fn read_rationale(
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        proposal_uri: &str,
    ) -> Option<String> {
        let links = perspective
            .get_links(&crate::types::LinkQuery {
                source: Some(proposal_uri.to_string()),
                predicate: Some("ad4m://flow/rationale".to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links(proposal.rationale)");
        let target = links.into_iter().next()?.data.target;
        // `literal:string:<url-encoded>` — decode the same way callers of
        // `resolve_property_value` would; a minimal peel here is sufficient
        // because the write path uses a plain ASCII rationale in these tests.
        target.strip_prefix("literal:string:").map(|s| {
            // Cheap decode: real reader uses `urlencoding::decode`, but the
            // test strings are ASCII with no reserved chars, so a percent-
            // free string is byte-identical after decode. Fall back to the
            // full decoder if a reserved char shows up.
            urlencoding::decode(s)
                .map(|c| c.into_owned())
                .unwrap_or_else(|_| s.to_string())
        })
    }

    /// LLM hint matches a satisfied transition ⇒ the proposal fires with
    /// the LLM's `reason` written as the on-graph `rationale`. This is the
    /// load-bearing "LLM attribution rides through" property of slice 10.6c.
    #[tokio::test(flavor = "multi_thread")]
    async fn llm_hint_matches_transition_writes_rationale_e2e() {
        let (mut perspective, ctx, inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        let hints = vec![LlmProposalHint {
            instance_uri: inst_uri.clone(),
            to_state: "scoped".to_string(),
            reason: Some("LLM saw one Task and moved the flow forward".to_string()),
        }];
        let llm = CannedLlm::responding("YES");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &hints,
        )
        .await;
        assert_eq!(
            minted.len(),
            1,
            "matched hint + Pass verdict ⇒ 1 proposal, got {minted:?}",
        );
        let rationale = read_rationale(&perspective, &minted[0])
            .await
            .expect("matched hint MUST write a rationale link on the proposal");
        assert_eq!(
            rationale, "LLM saw one Task and moved the flow forward",
            "written rationale must round-trip the LLM's reason verbatim",
        );
    }

    /// LLM hint for a transition NOT in the satisfied set ⇒ hint is
    /// silently dropped; the engine's own satisfied-transition proposal
    /// still fires without a rationale. Documents design §5.4 step 5:
    /// LLM cannot bypass the deterministic `requires` guard.
    #[tokio::test(flavor = "multi_thread")]
    async fn llm_hint_without_matching_transition_is_discarded_e2e() {
        let (mut perspective, ctx, inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        // Two hints: one names a state the flow never proposes on this
        // pass (`does-not-exist`); the other names the correct state on
        // an unknown instance URI. Both must be discarded.
        let hints = vec![
            LlmProposalHint {
                instance_uri: inst_uri.clone(),
                to_state: "does-not-exist".to_string(),
                reason: Some("LLM guessed a state".to_string()),
            },
            LlmProposalHint {
                instance_uri: "ad4m://flow/instance/never-minted".to_string(),
                to_state: "scoped".to_string(),
                reason: Some("LLM invented an instance URI".to_string()),
            },
        ];
        let llm = CannedLlm::responding("YES");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &hints,
        )
        .await;
        assert_eq!(
            minted.len(),
            1,
            "unmatched hints must not spawn extra proposals — engine still fires the satisfied one",
        );
        assert!(
            read_rationale(&perspective, &minted[0]).await.is_none(),
            "engine-emitted proposal (no matching hint) must NOT carry a rationale",
        );
    }

    /// LLM hint matches but `reason=None` (or empty string) ⇒ the write
    /// path drops the rationale entirely rather than persisting an empty
    /// scalar. Byte-identical to the pre-10.6c engine-only path for the
    /// on-graph proposal shape.
    #[tokio::test(flavor = "multi_thread")]
    async fn llm_hint_with_no_reason_writes_no_rationale_e2e() {
        let (mut perspective, ctx, inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        let hints = vec![LlmProposalHint {
            instance_uri: inst_uri.clone(),
            to_state: "scoped".to_string(),
            reason: None,
        }];
        let llm = CannedLlm::responding("YES");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&llm, "test-model-42")),
            &hints,
        )
        .await;
        assert_eq!(minted.len(), 1, "matched hint + Pass ⇒ 1 proposal");
        assert!(
            read_rationale(&perspective, &minted[0]).await.is_none(),
            "reason=None must NOT write a rationale — the SDNA allows omission and \
             writing an empty scalar bloats the graph with a link carrying no signal",
        );
    }

    // ---------------------------------------------------------------------
    // Slice 10.7d — full-stack harness→engine end-to-end
    //
    // The 10.6c tests above prove `run_engine_proposal_pass` writes a
    // rationale when handed a `LlmProposalHint` directly. The 10.7b tests
    // (in `ai_service::harness::flow_propose`) prove the decorator turns a
    // `_propose_transition` tool call into a buffered hint. This test
    // stitches BOTH halves plus the harness runner (`run_with_tools`)
    // together against a real perspective:
    //
    //   ScriptedLLM emits `Delivery_propose_transition`
    //     → FlowTransitionProposeProvider.call() validates + buffers
    //     → run_with_tools loop terminates on the follow-up plain answer
    //     → flow_buffer.drain() → &llm_hints
    //     → run_engine_proposal_pass matches (instance, toState)
    //     → FlowTransitionProposal written on-graph with `rationale`
    //
    // The only unit-mocked seam is the LLM. Everything else — the buffer,
    // the decorator, the harness loop, the SemanticCheck gate, the on-graph
    // writer — is the real implementation the runner uses. Slice 10.7c
    // (`run.rs`) wires these together identically; if this test passes and
    // the strategy path's 10.6c test passes, the runner's wiring has no
    // further seam to break.
    // ---------------------------------------------------------------------

    /// Scripted [`CompletionSource`] that returns a queued
    /// [`HarnessCompletion`] per `complete()` call and records what the
    /// harness advertised as tools on each round. Symmetric to the
    /// `ScriptedLLM` fixture inside `ai_service::harness::mod::tests`, but
    /// local to this test module so we don't have to make the harness's
    /// test-only doubles `pub(crate)` for one cross-module test.
    struct ScriptedLlm {
        script: std::sync::Mutex<Vec<crate::ai_service::harness::HarnessCompletion>>,
        calls: std::sync::Mutex<
            Vec<(
                Vec<serde_json::Value>,
                Vec<crate::ai_service::harness::provider::ToolSchema>,
            )>,
        >,
    }

    impl ScriptedLlm {
        fn new(script: Vec<crate::ai_service::harness::HarnessCompletion>) -> Self {
            Self {
                script: std::sync::Mutex::new(script),
                calls: std::sync::Mutex::new(Vec::new()),
            }
        }

        fn tools_on_call(&self, n: usize) -> Vec<crate::ai_service::harness::provider::ToolSchema> {
            self.calls.lock().unwrap()[n].1.clone()
        }
    }

    #[async_trait::async_trait]
    impl crate::ai_service::harness::CompletionSource for ScriptedLlm {
        async fn complete(
            &self,
            _model_id: &str,
            messages: &[serde_json::Value],
            tools: Vec<crate::ai_service::harness::provider::ToolSchema>,
        ) -> anyhow::Result<crate::ai_service::harness::HarnessCompletion> {
            self.calls
                .lock()
                .unwrap()
                .push((messages.to_vec(), tools.clone()));
            let next = self.script.lock().unwrap().remove(0);
            Ok(next)
        }
    }

    /// Minimal inner [`ToolProvider`] — advertises no tools, errors on any
    /// call. The whole point of this test is that the decorator (added
    /// on top) is the one the LLM's `_propose_transition` call routes to;
    /// the inner surface never comes into play. A real inner in production
    /// would be `ProposeWritesProvider` wrapping `Ad4mToolProvider`, but
    /// those would drag in an MCP context + AIService we don't need to
    /// prove the flow-attribution pipeline.
    struct EmptyInner;

    #[async_trait::async_trait]
    impl crate::ai_service::harness::provider::ToolProvider for EmptyInner {
        async fn tools(&self) -> Vec<crate::ai_service::harness::provider::ToolSchema> {
            Vec::new()
        }
        async fn call(&self, name: &str, _args: serde_json::Value) -> anyhow::Result<String> {
            Err(anyhow::anyhow!(
                "EmptyInner: no non-flow tools are advertised in this test; got call `{name}`"
            ))
        }
    }

    /// Scripted LLM emits `Delivery_propose_transition` on turn 1 with a
    /// reason string, then a plain answer on turn 2. After the harness
    /// loop terminates, the flow buffer holds one hint whose `instance` /
    /// `toState` / `reason` round-tripped through the decorator; feeding
    /// that hint into `run_engine_proposal_pass` produces a
    /// FlowTransitionProposal whose on-graph `rationale` matches the LLM's
    /// original `reason` verbatim. This is the load-bearing "harness
    /// tool-call routes attribution all the way to the graph" property.
    #[tokio::test(flavor = "multi_thread")]
    async fn harness_propose_transition_tool_call_routes_rationale_to_graph_e2e() {
        use crate::ai_service::harness::flow_propose::{
            propose_transition_tool_name, FlowProposalBuffer, FlowTransitionProposeProvider,
        };
        use crate::ai_service::harness::{
            run_with_tools, HarnessCompletion, HarnessConfig, HarnessToolCall,
        };
        use crate::perspectives::flow_context::gather_active_flow_contexts;
        use std::sync::Arc;

        // 1) Seed a real perspective with the Delivery flow, a satisfying
        //    Task, and one FlowInstance in `identified` — the same fixture
        //    the 10.6c tests use, so any drift between hint-driven and
        //    tool-call-driven attribution surfaces immediately.
        let (mut perspective, ctx, inst_uri) =
            seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

        // 2) Build the FlowContext list exactly as `run.rs` does. Passing
        //    `None` for `scope` matches how the auto-processor call site
        //    was passing when this slice was written.
        let active_flows = gather_active_flow_contexts(&perspective, None).await;
        assert_eq!(
            active_flows.len(),
            1,
            "fixture ⇒ one Delivery FlowInstance ⇒ one FlowContext, got {active_flows:?}",
        );
        assert_eq!(active_flows[0].flow_name, "Delivery");
        assert_eq!(active_flows[0].instance_uri, inst_uri);

        // 3) Compose the ToolProvider stack the same way the runner does:
        //    inner (no-op here) → FlowTransitionProposeProvider wrapping
        //    it with per-flow `_propose_transition` tools.
        let flow_buffer = FlowProposalBuffer::new();
        let provider: Arc<dyn crate::ai_service::harness::provider::ToolProvider> =
            Arc::new(FlowTransitionProposeProvider::new(
                Arc::new(EmptyInner),
                active_flows.clone(),
                flow_buffer.clone(),
            ));

        // 4) Script the LLM: one propose-transition turn, then a plain
        //    answer to terminate the loop.
        let expected_reason =
            "Task `ad4m://task/1` (Onboard Ana) has been scoped; advancing to `scoped`.";
        let script = vec![
            HarnessCompletion {
                content: String::new(),
                tool_calls: vec![HarnessToolCall {
                    id: "call-1".to_string(),
                    name: propose_transition_tool_name("Delivery"),
                    arguments: serde_json::json!({
                        "instance": inst_uri.clone(),
                        "toState": "scoped",
                        "reason": expected_reason,
                    }),
                }],
            },
            HarnessCompletion {
                content: "done".to_string(),
                tool_calls: Vec::new(),
            },
        ];
        let llm = Arc::new(ScriptedLlm::new(script));

        // 5) Drive the harness loop. Empty initial_messages is fine —
        //    this test's contract is "tool call routes to buffer,"
        //    prompt-shaping is covered by slice 10.2 tests.
        let _final_text = run_with_tools(
            "test-model-42",
            vec![serde_json::json!({"role": "user", "content": "extract"})],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
            None,
        )
        .await
        .expect("run_with_tools should terminate cleanly on the plain-answer turn");

        // 6) The decorator must have advertised `Delivery_propose_transition`
        //    on the first round's tools[] — proves the wire-through from
        //    active_flows into the tool schema is live (not just the
        //    dispatch path).
        let advertised = llm.tools_on_call(0);
        assert!(
            advertised
                .iter()
                .any(|t| t.name == propose_transition_tool_name("Delivery")),
            "first-round tools[] must advertise `Delivery_propose_transition`, got: {:?}",
            advertised.iter().map(|t| &t.name).collect::<Vec<_>>(),
        );

        // 7) Buffer must hold exactly one hint carrying the LLM's args
        //    verbatim (round-trip through JSON parsing + validation).
        let llm_hints = flow_buffer.drain();
        assert_eq!(
            llm_hints.len(),
            1,
            "one tool call ⇒ one buffered LlmProposalHint, got {llm_hints:?}",
        );
        assert_eq!(llm_hints[0].instance_uri, inst_uri);
        assert_eq!(llm_hints[0].to_state, "scoped");
        assert_eq!(llm_hints[0].reason.as_deref(), Some(expected_reason));

        // 8) Engine pass with a Pass-verdict semantic check ⇒ exactly one
        //    FlowTransitionProposal, and its `rationale` matches the LLM's
        //    original `reason` verbatim. This is the full-stack proof.
        let semantic_check = CannedLlm::responding("YES");
        let minted = run_engine_proposal_pass(
            &mut perspective,
            None,
            &ctx,
            Some((&semantic_check, "test-model-42")),
            &llm_hints,
        )
        .await;
        assert_eq!(
            minted.len(),
            1,
            "matched hint + Pass verdict ⇒ 1 proposal, got {minted:?}",
        );
        let on_graph_rationale = read_rationale(&perspective, &minted[0])
            .await
            .expect("full-stack matched hint MUST write a rationale link on the proposal");
        assert_eq!(
            on_graph_rationale, expected_reason,
            "on-graph rationale must round-trip the LLM's original `reason` verbatim through \
             the tool-call → decorator → buffer → engine → writer pipeline",
        );
    }

    // ========================================================================
    // Slice 10.9b2 — fire_flow_consensus onion-shell e2e
    // ========================================================================
    //
    // Read + write proof for the engine-side consensus firing pass. The
    // unit tests in `flow_consensus.rs` cover byte-parity of the pure
    // primitives (`aggregate_flow_votes`, `select_fire_candidate`,
    // parse helpers, guard message shapes); this test proves the whole
    // loop against a real perspective — same 10.4b/10.4c substrate:
    //
    //   1. Seed Delivery flow + `requires` guard + FlowInstance in
    //      `identified`, and one satisfying Task.
    //   2. Write 3 distinct-DID FlowTransitionProposals for
    //      `identified → scoped` directly via `write_engine_proposal`
    //      (no LLM, no engine pass — this test scopes to firing).
    //   3. Load them back via `load_flow_transition_proposals`.
    //      Assert count + URI presence.
    //   4. Aggregate with `n: 2`. Assert `fires` is populated and
    //      targets the right transition.
    //   5. `select_fire_candidate` against current state ⇒ Some.
    //   6. `fire_flow_consensus` ⇒ FireOutcome shape correct AND the
    //      on-graph `FlowInstance.currentState` is now `scoped`
    //      (round-trip via `load_flow_instances`).
    //   7. Second call to `select_fire_candidate` on the SAME aggregate
    //      with the NEW current state (`scoped`) ⇒ None — the stale
    //      guard prevents double-fire.
    //   8. Direct `fire_flow_consensus` with the stale tally against
    //      the advanced instance ⇒ Err (guard fires before writer).

    #[tokio::test(flavor = "multi_thread")]
    async fn fire_flow_consensus_advances_instance_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::flow_consensus::{
            aggregate_flow_votes, fire_flow_consensus, load_flow_transition_proposals,
            select_fire_candidate,
        };
        use crate::perspectives::flow_context::load_flow_instances;
        use crate::perspectives::shacl_parser::ConsensusRule;

        // (1) Same substrate the 10.4b/10.5 fixtures use: real perspective,
        //     no LLM, Task shape registered, Delivery flow seeded with a
        //     `requires` guard on `scoped`. Seed one Task so the guard is
        //     met — the writer path doesn't need it, but keeping the
        //     shape identical means a future refactor that folds the
        //     firing pass into the auto-processor will only have to
        //     tweak the assertions, not rebuild the fixture.
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let scoped_uri = "delivery://Delivery.scoped";
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: scoped_uri.to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");
        let base_uri = "ad4m://task/onboarding-10.9b2";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.9b2-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/1",
            "Onboard Ana",
        )
        .await;

        // (2) Three distinct-DID proposals for `identified → scoped`.
        //     We write directly via `write_flow_transition_proposal`
        //     to isolate the firing loop from the engine pass. Each
        //     proposal cites the seeded Task as evidence with the same
        //     `evidence_hash` (so the aggregator's grouping key stays
        //     honest even if a future change starts to key on hash).
        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-e2e-hash";
        let proposers = [
            ("did:key:alice", "p-alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "p-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-cara", "2026-08-30T10:05:02Z"),
        ];
        for (did, pid, ts) in &proposers {
            write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal");
        }

        // (3) Loader — every proposal we wrote comes back, and every
        //     URI is the one we handed to the writer. This is where a
        //     silent parse-drop would surface: if the hydrated JSON
        //     shape drifted, we'd see a shorter Vec.
        let loaded = load_flow_transition_proposals(&perspective, &inst_uri)
            .await
            .expect("load_flow_transition_proposals");
        assert_eq!(loaded.len(), 3, "3 written ⇒ 3 loaded, got {loaded:?}");
        let mut uris: Vec<&str> = loaded.iter().map(|r| r.uri.as_str()).collect();
        uris.sort();
        assert_eq!(
            uris,
            vec![
                "ad4m://flow/proposal/p-alice",
                "ad4m://flow/proposal/p-bob",
                "ad4m://flow/proposal/p-cara",
            ],
            "loaded proposal URIs must match the ones the writer minted",
        );
        for r in &loaded {
            assert_eq!(r.from_state, "identified");
            assert_eq!(r.to_state, "scoped");
        }

        // (4) Aggregate with `n: 2` ⇒ consensus met, fires populated.
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let agg = aggregate_flow_votes(&loaded, Some(&rule), None).expect("aggregate_flow_votes");
        let fires = agg.fires.as_ref().expect("agg.fires must be Some");
        assert_eq!(fires.from_state, "identified");
        assert_eq!(fires.to_state, "scoped");
        assert!(fires.consensus_reached);
        assert_eq!(fires.eligible_proposers.len(), 3);

        // (5) select_fire_candidate against current on-graph state.
        let records_before = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances before firing");
        assert_eq!(records_before.len(), 1);
        let instance_before = records_before[0].clone();
        assert_eq!(instance_before.current_state, "identified");
        let picked = select_fire_candidate(&instance_before.current_state, &agg)
            .expect("select_fire_candidate must return Some for a matching current state");
        assert_eq!(picked.to_state, "scoped");

        // (6) Fire — the writer path advances currentState on-graph;
        //     FireOutcome carries the outcome shape.
        let outcome = fire_flow_consensus(&mut perspective, &instance_before, picked, None, &ctx)
            .await
            .expect("fire_flow_consensus must succeed");
        assert_eq!(outcome.instance_uri, inst_uri);
        assert_eq!(outcome.from_state, "identified");
        assert_eq!(outcome.to_state, "scoped");
        assert_eq!(
            outcome.fired_by_proposers,
            vec![
                "did:key:alice".to_string(),
                "did:key:bob".to_string(),
                "did:key:cara".to_string(),
            ],
        );
        let mut fired_uris = outcome.contributing_proposal_uris.clone();
        fired_uris.sort();
        assert_eq!(
            fired_uris,
            vec![
                "ad4m://flow/proposal/p-alice".to_string(),
                "ad4m://flow/proposal/p-bob".to_string(),
                "ad4m://flow/proposal/p-cara".to_string(),
            ],
            "contributing_proposal_uris must round-trip the on-graph proposal URIs",
        );

        // (7) Rehydrated on-graph state must show `scoped`. Proves the
        //     writer path actually wrote the link; a stub or a
        //     silently-dropped `update_subject` would leave `identified`
        //     on-graph.
        let records_after = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after firing");
        assert_eq!(records_after.len(), 1);
        let instance_after = records_after[0].clone();
        assert_eq!(instance_after.current_state, "scoped");

        // (8a) Same aggregate against the NEW current state ⇒ stale
        //      guard fires ⇒ select_fire_candidate returns None. This
        //      is the concurrency-hazard case the guard exists for.
        assert!(
            select_fire_candidate(&instance_after.current_state, &agg).is_none(),
            "select_fire_candidate must return None once the flow has advanced past the aggregate's from_state",
        );

        // (8b) Direct fire with the stale tally against the advanced
        //      instance ⇒ Err. This proves the guard fires INSIDE the
        //      async writer path too, not just the pre-selector — so a
        //      caller that skips `select_fire_candidate` and hands the
        //      raw `agg.fires` to `fire_flow_consensus` still can't
        //      double-fire.
        let stale_fire =
            fire_flow_consensus(&mut perspective, &instance_after, picked, None, &ctx).await;
        assert!(
            stale_fire
                .as_ref()
                .err()
                .map(|e| e.to_string().contains("stale tally"))
                .unwrap_or(false),
            "stale-fromState guard must fire — got {stale_fire:?}",
        );

        // Sanity: on-graph currentState still `scoped` — the stale-fire
        // attempt must NOT have touched the instance.
        let records_after_stale = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after stale-fire attempt");
        assert_eq!(records_after_stale[0].current_state, "scoped");
    }

    /// Slice 10.9b2 — below-threshold aggregate ⇒ no fires ⇒ nothing
    /// advances on-graph. Complements the happy-path e2e above by
    /// proving the "no consensus" branch also hits real perspective
    /// state without touching the writer.
    #[tokio::test(flavor = "multi_thread")]
    async fn fire_flow_consensus_below_threshold_no_advance_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::flow_consensus::{
            aggregate_flow_votes, load_flow_transition_proposals, select_fire_candidate,
        };
        use crate::perspectives::flow_context::load_flow_instances;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let base_uri = "ad4m://task/onboarding-10.9b2-below";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.9b2-below-inst",
            "2026-08-30T11:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Only one proposal against an n=2 rule.
        write_flow_transition_proposal(
            &mut perspective,
            "p-only",
            "did:key:solo",
            "2026-08-30T11:05:00Z",
            &inst_uri,
            "identified",
            "scoped",
            &["ad4m://task/1".to_string()],
            "sha256:below-hash",
            None,
            None,
            &ctx,
        )
        .await
        .expect("write_flow_transition_proposal(solo)");

        let loaded = load_flow_transition_proposals(&perspective, &inst_uri)
            .await
            .expect("load_flow_transition_proposals");
        assert_eq!(loaded.len(), 1);
        let rule = ConsensusRule {
            n: 2,
            from_role: None,
        };
        let agg = aggregate_flow_votes(&loaded, Some(&rule), None).expect("aggregate_flow_votes");
        assert!(
            agg.fires.is_none(),
            "1 vote < n=2 ⇒ agg.fires must be None, got {:?}",
            agg.fires,
        );
        assert!(
            select_fire_candidate("identified", &agg).is_none(),
            "no fires ⇒ select_fire_candidate must be None",
        );

        // On-graph state unchanged — writer path was never reached.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances");
        assert_eq!(records[0].current_state, "identified");
    }

    /// Slice 10.9b2 — loader empty-URI guard against a real perspective.
    #[tokio::test(flavor = "multi_thread")]
    async fn load_flow_transition_proposals_rejects_empty_uri_e2e() {
        use crate::perspectives::flow_consensus::load_flow_transition_proposals;
        let (perspective, _shapes, _ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let err = load_flow_transition_proposals(&perspective, "")
            .await
            .expect_err("empty URI must return Err before hitting model_query");
        assert!(
            err.to_string().contains("must not be empty"),
            "guard message must state the failure clearly, got {err}",
        );
    }

    // ------------------------------------------------------------------
    // Slice 10.10a — engine-side consensus firing pass
    //
    // These e2e tests exercise `run_flow_consensus_pass` end-to-end
    // against a live perspective: seeded Delivery flow, one or more
    // minted `FlowInstance`s, distinct-DID proposals via the same
    // writer the client-side factory uses. The composition under test
    // is the ONLY thing that changed between slice 10.9b2's
    // `fire_flow_consensus_advances_instance_e2e` and these tests —
    // if the aggregator + writer stay green but this pass does not,
    // the failure is inside the composition.
    // ------------------------------------------------------------------

    /// Slice 10.10a — happy path. Three proposals against a Delivery
    /// flow with a top-level `ConsensusRule { n: 2 }` ⇒
    /// `run_flow_consensus_pass` returns exactly one `FireOutcome`,
    /// the on-graph `currentState` advances to `scoped`, and a
    /// SECOND call returns an empty vec (idempotent — proposals for
    /// the now-stale `fromState` no longer match `select_fire_candidate`).
    #[tokio::test(flavor = "multi_thread")]
    async fn run_flow_consensus_pass_fires_and_is_idempotent_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        // Delivery flow with a `ConsensusRule { n: 2 }` at flow-level so
        // the pass's rule-resolution walk (state → flow) picks it up.
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://Delivery".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_uri = "ad4m://task/onboarding-10.10a-happy";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.10a-happy-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-10.10a-happy";
        let proposers = [
            ("did:key:alice", "p-alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "p-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-cara", "2026-08-30T10:05:02Z"),
        ];
        for (did, pid, ts) in &proposers {
            write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal");
        }

        // First run — consensus reached, single instance advances.
        let fired = run_flow_consensus_pass(&mut perspective, None, &ctx).await;
        assert_eq!(
            fired.len(),
            1,
            "1 instance × consensus reached ⇒ 1 FireOutcome, got {fired:?}",
        );
        let outcome = &fired[0];
        assert_eq!(outcome.instance_uri, inst_uri);
        assert_eq!(outcome.from_state, "identified");
        assert_eq!(outcome.to_state, "scoped");
        assert_eq!(outcome.fired_by_proposers.len(), 3);
        assert_eq!(outcome.contributing_proposal_uris.len(), 3);

        // Rehydrated on-graph — currentState actually advanced. A
        // silently-dropped writer batch would leave `identified`.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after fire");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].current_state, "scoped");

        // Second run — same proposals still on-graph, but they all
        // target `identified → scoped`; select_fire_candidate rejects
        // them against the new `scoped` current state. Empty vec, no
        // panic, no accidental re-fire.
        let refired = run_flow_consensus_pass(&mut perspective, None, &ctx).await;
        assert!(
            refired.is_empty(),
            "post-fire pass must be idempotent — proposals target stale from_state, got {refired:?}",
        );
        let records_after = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after idempotency check");
        assert_eq!(records_after[0].current_state, "scoped");
    }

    /// Slice 10.10a — scope narrowing. Two FlowInstances on different
    /// `subject` anchors, each with 3-of-2 consensus. Pass a scope
    /// naming only the first ⇒ exactly one fires, and the on-graph
    /// state of the untargeted instance stays `identified`. Proves
    /// the scope arg is threaded through `load_flow_instances` (a
    /// regression here would mint proposals but fire the wrong
    /// instance on the wrong extraction pass).
    #[tokio::test(flavor = "multi_thread")]
    async fn run_flow_consensus_pass_respects_scope_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::model_query::types::Scope;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://Delivery".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_a = "ad4m://task/scope-A";
        let base_b = "ad4m://task/scope-B";
        let inst_a = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_a,
            "identified",
            "e2e-10.10a-scope-A",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance(A)");
        let inst_b = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_b,
            "identified",
            "e2e-10.10a-scope-B",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance(B)");

        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-10.10a-scope";
        let proposers = [
            ("did:key:alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "2026-08-30T10:05:02Z"),
        ];
        for (i, (did, ts)) in proposers.iter().enumerate() {
            write_flow_transition_proposal(
                &mut perspective,
                &format!("p-A-{i}"),
                did,
                ts,
                &inst_a,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write proposal(A)");
            write_flow_transition_proposal(
                &mut perspective,
                &format!("p-B-{i}"),
                did,
                ts,
                &inst_b,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write proposal(B)");
        }

        // Scope narrows to base_a — only instance A fires, B stays put.
        // `Scope::Raw { id, predicate }`'s `id` is what
        // `scope_subject` returns, so a Raw scope pointing at base_a
        // filters `load_flow_instances(subject=Some(base_a))`. The
        // predicate is unused by the flow-layer filter.
        let scope_a = Scope::Raw {
            id: base_a.to_string(),
            predicate: "ad4m://has_child".to_string(),
        };
        let fired = run_flow_consensus_pass(&mut perspective, Some(&scope_a), &ctx).await;
        assert_eq!(
            fired.len(),
            1,
            "scope narrowed to base_a ⇒ 1 FireOutcome (instance A), got {fired:?}",
        );
        assert_eq!(fired[0].instance_uri, inst_a);

        // Rehydrate both: A advanced, B untouched.
        let all = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances(None)");
        let a = all
            .iter()
            .find(|r| r.instance_uri == inst_a)
            .expect("instance A must load");
        let b = all
            .iter()
            .find(|r| r.instance_uri == inst_b)
            .expect("instance B must load");
        assert_eq!(a.current_state, "scoped", "A must have advanced");
        assert_eq!(
            b.current_state, "identified",
            "B must stay put — scope narrowing gate",
        );

        // No-scope run picks up B and fires it.
        let fired_b = run_flow_consensus_pass(&mut perspective, None, &ctx).await;
        assert_eq!(
            fired_b.len(),
            1,
            "second pass with no scope ⇒ 1 FireOutcome (only B remains fireable), got {fired_b:?}",
        );
        assert_eq!(fired_b[0].instance_uri, inst_b);
    }

    /// Slice 10.10a — `from_role` deferred branch. Consensus rule at
    /// state-level carries a `from_role`; the pass logs+skips, on-graph
    /// state does NOT advance even though the proposal count meets `n`.
    /// This locks the "no silent auto-fire without eligibility
    /// resolution" contract until a future slice ships DID resolution.
    #[tokio::test(flavor = "multi_thread")]
    async fn run_flow_consensus_pass_skips_when_from_role_set_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        // State-level rule with from_role — resolution walk picks the
        // state's rule over the (absent) flow-level one, then the pass
        // skips because we can't answer "who is eligible?" yet.
        // `ConsensusRule::from_role` is a `ModelQuery` (per §7); the
        // exact shape doesn't matter for this test — the pass short-
        // circuits on `is_some()` before it would ever run the query.
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: Some(crate::perspectives::shacl_parser::ModelQuery {
                class_name: "ns://Assignee".to_string(),
                r#where: None,
                count: None,
                linked_to: None,
                did_property: Some("did".to_string()),
                or: None,
            }),
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://Delivery.identified".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(state.consensusRule with from_role)");

        let base_uri = "ad4m://task/from-role-defer";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.10a-from-role-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-10.10a-from-role";
        for (did, pid, ts) in &[
            ("did:key:alice", "p-fr-alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "p-fr-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-fr-cara", "2026-08-30T10:05:02Z"),
        ] {
            write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal");
        }

        let fired = run_flow_consensus_pass(&mut perspective, None, &ctx).await;
        assert!(
            fired.is_empty(),
            "from_role set + eligibility deferral ⇒ empty FireOutcome, got {fired:?}",
        );
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after skip");
        assert_eq!(
            records[0].current_state, "identified",
            "on-graph state must NOT have advanced when from_role is set",
        );
    }

    /// Slice 10.10b — engine-proposal→consensus chained back-to-back,
    /// the same sequence `interpretation::run` executes. Proves the two
    /// passes compose correctly on a real perspective:
    ///
    ///   1. Seed a Delivery flow whose `scoped` state has `requires:
    ///      count>=1 ns://Task` and a top-level `ConsensusRule { n: 1 }`
    ///      (single-proposer neighbourhood — auto-processor advances
    ///      on its own vote).
    ///   2. Seed one Task so the deterministic evaluator finds a
    ///      satisfied transition on the next `run_engine_proposal_pass`.
    ///   3. `run_engine_proposal_pass` mints one `FlowTransitionProposal`
    ///      on-graph, proposer = acting DID.
    ///   4. `run_flow_consensus_pass` immediately aggregates against
    ///      the on-graph bag and fires the freshly-minted proposal
    ///      (n=1, no from_role). On-graph `currentState` advances
    ///      `identified → scoped`.
    ///
    /// This is the composition run.rs owns: proposal pass, then
    /// consensus pass. A regression that broke the wire-in (e.g.
    /// consensus pass runs BEFORE proposal pass, or scope threading
    /// drops) would show up here even without running the extraction
    /// LLM path.
    #[tokio::test(flavor = "multi_thread")]
    async fn engine_proposal_then_consensus_fire_composition_e2e() {
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        // `requires` guard on `scoped` — the trigger for a satisfied
        // transition once a Task is seeded.
        let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
        perspective
            .add_link(
                Link {
                    source: "delivery://Delivery.scoped".to_string(),
                    predicate: Some("ad4m://requires".to_string()),
                    target: lit(requires_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(scoped.requires)");
        // `ConsensusRule { n: 1 }` — single-proposer fires. Deferred
        // `from_role` branch stays off (rule.from_role is None).
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 1,
            from_role: None,
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://Delivery".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_uri = "ad4m://task/compose-10.10b";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.10b-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");
        // Seed one Task so the `requires` guard is satisfied.
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "ad4m://task/compose-1",
            "Compose Test",
        )
        .await;

        // Step 3 — engine-proposal pass mints a proposal. No LLM hints,
        // no semantic-check gate (we're proving the composition, not
        // the gate).
        let minted = run_engine_proposal_pass(&mut perspective, None, &ctx, None, &[]).await;
        assert_eq!(
            minted.len(),
            1,
            "engine-proposal pass must mint exactly one proposal for the seeded satisfied transition, got {minted:?}",
        );

        // Step 4 — consensus firing pass immediately advances the
        // instance. n=1 + one proposer (acting_did) ⇒ consensus met.
        let fired = run_flow_consensus_pass(&mut perspective, None, &ctx).await;
        assert_eq!(
            fired.len(),
            1,
            "consensus firing pass must advance exactly one instance chained after the proposal pass, got {fired:?}",
        );
        assert_eq!(fired[0].instance_uri, inst_uri);
        assert_eq!(fired[0].from_state, "identified");
        assert_eq!(fired[0].to_state, "scoped");

        // The single contributing proposal must be the one the engine
        // pass just minted — proves the two passes see the SAME on-
        // graph bag (a store-consistency bug between them would show
        // as either 0 proposals loaded or a different URI).
        assert_eq!(
            fired[0].contributing_proposal_uris.len(),
            1,
            "n=1 fire ⇒ 1 contributing proposal",
        );
        assert_eq!(
            fired[0].contributing_proposal_uris[0], minted[0],
            "the consensus pass must have fired on the freshly-minted proposal",
        );

        // On-graph state actually advanced.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after chained composition");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].current_state, "scoped");
    }

    // ------------------------------------------------------------------
    // Slice 10.16 — report-only preview_fire_for_instance
    //
    // These e2e tests exercise `preview_fire_for_instance` end-to-end
    // against a live perspective. Same substrate + writer as the 10.10a
    // tests above — the ONLY thing being validated here is that the
    // report-only path reaches the same verdict as one iteration of
    // `run_flow_consensus_pass`, WITHOUT advancing on-graph state.
    //
    // The "leaves graph untouched" assertion is the load-bearing one —
    // a `.selectFireCandidate` on the OO side or a `preview_fire_for_instance`
    // on the engine side that accidentally advanced `currentState` would
    // silently break the auto-processor's stale-vote guard on the next
    // real firing pass.
    // ------------------------------------------------------------------

    /// Slice 10.16 — happy path. Same 3-proposals / n=2 setup as the
    /// 10.10a idempotence test, but through `preview_fire_for_instance`.
    /// Assertions:
    ///   - `Ok(Some(tally))` returned, tally has met consensus, matches
    ///     the `identified → scoped` transition.
    ///   - On-graph `currentState` is STILL `identified` after the
    ///     preview call (no write happened).
    ///   - Second preview call returns the same tally (report-only is
    ///     idempotent — a second call cannot advance the flow past the
    ///     transition it was reporting on).
    #[tokio::test(flavor = "multi_thread")]
    async fn preview_fire_for_instance_returns_tally_and_leaves_graph_untouched_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        // NOTE: the flow's on-graph URI is `delivery://DeliveryFlow`
        // (see `parse_flow_to_links` — `format!("{}{}Flow", namespace,
        // flow_name)`), NOT `delivery://Delivery`. Attaching the rule
        // to the correct source is what makes `parse_flow_from_links`
        // pick it up in `load_shacl_flows`; the adjacent 10.10a walker
        // tests attach to `delivery://Delivery` and happen to pass only
        // because the default aggregator rule (n=1) is satisfied by
        // their proposer counts.
        perspective
            .add_link(
                Link {
                    source: "delivery://DeliveryFlow".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_uri = "ad4m://task/preview-10.16-happy";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.16-happy-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-10.16-happy";
        for (did, pid, ts) in &[
            ("did:key:alice", "p-preview-alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "p-preview-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-preview-cara", "2026-08-30T10:05:02Z"),
        ] {
            write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal");
        }

        let previewed = preview_fire_for_instance(&perspective, &inst_uri)
            .await
            .expect("preview_fire_for_instance ok");
        let tally = previewed.expect("consensus reached ⇒ Some(tally)");
        assert!(
            tally.consensus_reached,
            "returned tally must have consensus_reached=true (would-fire predicate), got {tally:?}",
        );
        assert_eq!(tally.from_state, "identified");
        assert_eq!(tally.to_state, "scoped");
        assert_eq!(
            tally.eligible_proposers.len(),
            3,
            "3 distinct proposers on the bag ⇒ 3 eligible in the tally",
        );
        assert_eq!(tally.required_count, 2, "n=2 rule surfaces on the tally");

        // Load-bearing assertion: on-graph state MUST be unchanged.
        // A preview that accidentally advanced would look green in the
        // "returned tally" checks above but silently poison the next
        // real firing pass's stale-vote guard.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after preview");
        assert_eq!(records.len(), 1);
        assert_eq!(
            records[0].current_state, "identified",
            "preview_fire_for_instance MUST NOT advance currentState — report-only contract",
        );

        // Idempotent — a second preview against the still-unchanged
        // graph returns the same-shape tally. (If the first call had
        // written anything, the second would either see stale votes
        // and return None, or hit a doubled-write consistency bug.)
        let repreview = preview_fire_for_instance(&perspective, &inst_uri)
            .await
            .expect("second preview ok");
        let tally2 = repreview.expect("second preview also returns Some(tally)");
        assert_eq!(tally2.from_state, "identified");
        assert_eq!(tally2.to_state, "scoped");
        assert_eq!(tally2.eligible_proposers, tally.eligible_proposers);
    }

    /// Slice 10.16 — below-threshold and stale-fromState both surface as
    /// `Ok(None)`. Bundling into one test since they share the same
    /// setup + touch adjacent branches of the report-only path.
    ///
    /// Part A: 1 proposal vs `n=2` rule ⇒ aggregate has no `fires` tally
    /// ⇒ `Ok(None)`.
    ///
    /// Part B: bump rule to `n=1`, add a proposal, verify `Some(tally)`
    /// once — then use `advance_flow_instance_state` to move `currentState`
    /// past the tally's `fromState` and prove the next preview call
    /// returns `Ok(None)` on the same on-graph bag (proves the
    /// `select_fire_candidate` stale-guard fires from this path).
    #[tokio::test(flavor = "multi_thread")]
    async fn preview_fire_for_instance_below_threshold_and_stale_from_state_return_none_e2e() {
        use crate::perspectives::flow_classes::{
            advance_flow_instance_state, write_flow_transition_proposal,
        };
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition v4)");
        }

        // Part A: n=2 rule, 1 proposal ⇒ below threshold.
        let rule_json_n2 = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        // Correct flow URI — see note in the happy-path test above.
        perspective
            .add_link(
                Link {
                    source: "delivery://DeliveryFlow".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json_n2),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule n=2)");

        let base_uri = "ad4m://task/preview-10.16-below";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.16-below-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        write_flow_transition_proposal(
            &mut perspective,
            "p-below-alice",
            "did:key:alice",
            "2026-08-30T10:05:00Z",
            &inst_uri,
            "identified",
            "scoped",
            &["ad4m://task/1".to_string()],
            "sha256:dummy-10.16-below",
            None,
            None,
            &ctx,
        )
        .await
        .expect("write_flow_transition_proposal(below)");

        let below = preview_fire_for_instance(&perspective, &inst_uri)
            .await
            .expect("preview_fire_for_instance ok (below-threshold)");
        assert!(
            below.is_none(),
            "1 proposer vs n=2 ⇒ no fires ⇒ Ok(None), got {below:?}",
        );
        let records_after_below = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after below-threshold preview");
        assert_eq!(
            records_after_below[0].current_state, "identified",
            "below-threshold preview must not touch state either",
        );

        // Part B: swap in n=1 (single-vote fires), advance the instance
        // manually via the writer primitive, then re-preview and prove
        // the stale-fromState guard fires from this path.
        //
        // We can't mint two flow instances against the same anchor
        // (mint refuses duplicates); reuse the same instance across
        // parts. The `add_link` here appends a second `consensusRule`
        // link. `parse_flow_from_links` reads the FIRST such link, so
        // append order matters. To make the update deterministic, we
        // rewrite the rule via a fresh perspective load below — but
        // instead we prove the stale guard by keeping the n=2 rule
        // and advancing to `scoped`: now the SAME on-graph bag
        // (1 proposal targeting identified→scoped) is stale relative
        // to the new currentState=scoped, and preview returns None.

        let batch_id = perspective.create_batch().await;
        advance_flow_instance_state(
            &mut perspective,
            &inst_uri,
            "scoped",
            Some(batch_id.clone()),
            &ctx,
        )
        .await
        .expect("advance_flow_instance_state");
        perspective
            .commit_batch(batch_id, &ctx)
            .await
            .expect("commit_batch");

        let records_after_advance = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after manual advance");
        assert_eq!(
            records_after_advance[0].current_state, "scoped",
            "manual advance persisted",
        );

        // Add 2 more proposals so the aggregate for identified→scoped
        // has 3 eligible proposers vs required 2 (would-fire IF the
        // instance were still at identified). But instance is now
        // scoped, so `select_fire_candidate` rejects the tally as stale.
        for (did, pid, ts) in &[
            ("did:key:bob", "p-below-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-below-cara", "2026-08-30T10:05:02Z"),
        ] {
            write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &["ad4m://task/1".to_string()],
                "sha256:dummy-10.16-below-stale",
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal(stale)");
        }

        let stale = preview_fire_for_instance(&perspective, &inst_uri)
            .await
            .expect("preview_fire_for_instance ok (stale-fromState)");
        assert!(
            stale.is_none(),
            "aggregate has fires but tally.from_state=identified != instance.currentState=scoped ⇒ select_fire_candidate rejects ⇒ Ok(None), got {stale:?}",
        );
    }

    /// Slice 10.16 — empty `flow_instance_uri` returns `Err` (caller-
    /// side violation) rather than defaulting to "no fire". The empty-
    /// URI path is explicitly the ONLY error-returning branch — every
    /// other failure surface returns `Ok(None)` under the report-only
    /// soft-fail contract.
    #[tokio::test(flavor = "multi_thread")]
    async fn preview_fire_for_instance_rejects_empty_uri_e2e() {
        let (perspective, _shapes, _ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let err = preview_fire_for_instance(&perspective, "")
            .await
            .expect_err("empty URI must return Err before loading anything");
        assert!(
            err.to_string().contains("must not be empty"),
            "guard message must state the failure clearly, got {err}",
        );
    }

    /// Slice 10.16 — an instance URI not present on this perspective
    /// returns `Ok(None)` (not `Err`). An observer polling a URI that
    /// hasn't been minted yet (or was minted on a different perspective
    /// still syncing over) should not surface as an error — matches the
    /// walker's silent-skip policy for absent flow definitions.
    #[tokio::test(flavor = "multi_thread")]
    async fn preview_fire_for_instance_unknown_uri_returns_none_e2e() {
        let (perspective, _shapes, _ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let res = preview_fire_for_instance(&perspective, "ad4m://flow/instance/does-not-exist")
            .await
            .expect("unknown URI must not error, must return Ok(None)");
        assert!(
            res.is_none(),
            "unknown instance URI ⇒ Ok(None), got {res:?}",
        );
    }

    // ========================================================================
    // Slice 10.14 — mutating fire_if_consensus_for_instance
    // ========================================================================
    //
    // Mirror of the 10.16 preview e2e suite but proves the mutation
    // path. Each test verifies the on-graph `currentState` shape via
    // `load_flow_instances` post-call (the load-bearing regression
    // guard: a fire that succeeds but doesn't advance is worse than a
    // fire that visibly panics).

    /// 10.14 — happy path: n=2 rule, 3 distinct proposers,
    /// `fire_if_consensus_for_instance` advances the flow AND returns
    /// a matching `FireOutcome` whose `contributing_proposal_uris`
    /// come from the freshly-written proposals.
    #[tokio::test(flavor = "multi_thread")]
    async fn fire_if_consensus_for_instance_advances_and_returns_outcome_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }
        // Same URI-correctness rationale as the 10.16 happy-path test —
        // flow-level rule must attach to `delivery://DeliveryFlow`, not
        // `delivery://Delivery`, for `parse_flow_from_links` to pick it
        // up via `load_shacl_flows`.
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://DeliveryFlow".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_uri = "ad4m://task/fire-10.14-happy";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.14-happy-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        let evidence_ids = vec!["ad4m://task/1".to_string()];
        let evidence_hash = "sha256:dummy-10.14-happy";
        let mut proposal_uris = Vec::new();
        for (did, pid, ts) in &[
            ("did:key:alice", "p-fire-alice", "2026-08-30T10:05:00Z"),
            ("did:key:bob", "p-fire-bob", "2026-08-30T10:05:01Z"),
            ("did:key:cara", "p-fire-cara", "2026-08-30T10:05:02Z"),
        ] {
            let uri = write_flow_transition_proposal(
                &mut perspective,
                pid,
                did,
                ts,
                &inst_uri,
                "identified",
                "scoped",
                &evidence_ids,
                evidence_hash,
                None,
                None,
                &ctx,
            )
            .await
            .expect("write_flow_transition_proposal");
            proposal_uris.push(uri);
        }

        let outcome = fire_if_consensus_for_instance(&mut perspective, &inst_uri, &ctx)
            .await
            .expect("fire_if_consensus_for_instance ok")
            .expect("consensus reached ⇒ Some(outcome)");
        assert_eq!(outcome.instance_uri, inst_uri);
        assert_eq!(outcome.from_state, "identified");
        assert_eq!(outcome.to_state, "scoped");
        assert_eq!(
            outcome.fired_by_proposers.len(),
            3,
            "n=2 rule with 3 distinct proposers ⇒ 3 eligible proposers on the outcome",
        );
        // Contributing proposal URIs must be exactly the ones we wrote
        // — proves the outcome is derived from the same bag the fire
        // read, not a stale cache. Order-insensitive compare because
        // aggregate_flow_votes bucketing doesn't guarantee input order.
        let mut got: Vec<String> = outcome.contributing_proposal_uris.clone();
        got.sort();
        let mut want = proposal_uris.clone();
        want.sort();
        assert_eq!(
            got, want,
            "contributing_proposal_uris must equal the freshly-minted proposal URIs",
        );

        // Load-bearing assertion: on-graph state MUST be advanced.
        // The complementary check to the 10.16 "MUST NOT advance"
        // guard — a fire path that reports success but doesn't
        // persist is silently broken.
        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after fire");
        assert_eq!(records.len(), 1);
        assert_eq!(
            records[0].current_state, "scoped",
            "fire_if_consensus_for_instance MUST advance currentState on the same on-graph shape TS FlowInstance.fireIfConsensus writes",
        );

        // Idempotence-style follow-up: a second call on the now-advanced
        // instance sees the same proposals but their fromState is now
        // stale relative to currentState, so `select_fire_candidate`
        // returns None ⇒ Ok(None). This proves the stale-guard fires
        // from THIS path (not just from `preview_fire_for_instance`)
        // and no double-advance to a third state can occur.
        let follow_up = fire_if_consensus_for_instance(&mut perspective, &inst_uri, &ctx)
            .await
            .expect("follow-up call must not error");
        assert!(
            follow_up.is_none(),
            "second fire on advanced instance MUST return Ok(None) (stale votes), got {follow_up:?}",
        );
        let records_after = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after follow-up");
        assert_eq!(
            records_after[0].current_state, "scoped",
            "follow-up must NOT double-advance beyond 'scoped'",
        );
    }

    /// 10.14 — below-threshold: 1 proposal vs `n=2` ⇒ `Ok(None)` AND
    /// no on-graph mutation. Bundled with the "stale-fromState"
    /// stress in the 10.16 preview suite; here we split them because
    /// the mutating path has a distinct commit failure mode we don't
    /// want to entangle with the stale-guard case.
    #[tokio::test(flavor = "multi_thread")]
    async fn fire_if_consensus_for_instance_below_threshold_returns_none_no_advance_e2e() {
        use crate::perspectives::flow_classes::write_flow_transition_proposal;
        use crate::perspectives::shacl_parser::ConsensusRule;

        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

        for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
            .expect("parse_flow_to_links(Delivery)")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }
        let rule_json = serde_json::to_string(&ConsensusRule {
            n: 2,
            from_role: None,
        })
        .unwrap();
        perspective
            .add_link(
                Link {
                    source: "delivery://DeliveryFlow".to_string(),
                    predicate: Some("ad4m://consensusRule".to_string()),
                    target: lit(&rule_json),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("add_link(flow.consensusRule)");

        let base_uri = "ad4m://task/fire-10.14-below";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "Delivery",
            base_uri,
            "identified",
            "e2e-10.14-below-inst",
            "2026-08-30T10:00:00Z",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Only one proposer vs n=2 — aggregate has no `fires` tally.
        write_flow_transition_proposal(
            &mut perspective,
            "p-below-alice",
            "did:key:alice",
            "2026-08-30T10:05:00Z",
            &inst_uri,
            "identified",
            "scoped",
            &["ad4m://task/1".to_string()],
            "sha256:dummy-10.14-below",
            None,
            None,
            &ctx,
        )
        .await
        .expect("write_flow_transition_proposal");

        let outcome = fire_if_consensus_for_instance(&mut perspective, &inst_uri, &ctx)
            .await
            .expect("fire_if_consensus_for_instance ok (below-threshold)");
        assert!(
            outcome.is_none(),
            "1 vote vs n=2 ⇒ Ok(None), got {outcome:?}",
        );

        let records = load_flow_instances(&perspective, None)
            .await
            .expect("load_flow_instances after below-threshold call");
        assert_eq!(
            records[0].current_state, "identified",
            "below-threshold path MUST NOT touch on-graph currentState",
        );
    }

    /// 10.14 — empty URI is a caller programming error, not a soft
    /// fail. Mirrors [`preview_fire_for_instance_rejects_empty_uri_e2e`].
    #[tokio::test(flavor = "multi_thread")]
    async fn fire_if_consensus_for_instance_rejects_empty_uri_e2e() {
        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let err = fire_if_consensus_for_instance(&mut perspective, "", &ctx)
            .await
            .expect_err("empty URI must return Err before loading anything");
        assert!(
            err.to_string().contains("must not be empty"),
            "guard message must state the failure clearly, got {err}",
        );
    }

    /// 10.14 — an instance URI not present on this perspective
    /// returns `Ok(None)` (not `Err`). Same silent-skip policy as
    /// [`preview_fire_for_instance_unknown_uri_returns_none_e2e`] and
    /// [`run_flow_consensus_pass`]'s absent-catalogue branch.
    #[tokio::test(flavor = "multi_thread")]
    async fn fire_if_consensus_for_instance_unknown_uri_returns_none_e2e() {
        let (mut perspective, _shapes, ctx) =
            setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;
        let res = fire_if_consensus_for_instance(
            &mut perspective,
            "ad4m://flow/instance/does-not-exist",
            &ctx,
        )
        .await
        .expect("unknown URI must not error, must return Ok(None)");
        assert!(
            res.is_none(),
            "unknown instance URI ⇒ Ok(None), got {res:?}",
        );
    }
}
