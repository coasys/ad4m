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
        batch_id,
        context,
    )
    .await
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
        let satisfied = evaluate_flow_transitions(
            &perspective,
            &records,
            &flows_by_name,
            "did:key:acting",
        )
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
}
