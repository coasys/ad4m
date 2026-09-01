//! Pure primitives (slice 10.4a1): the [`SatisfiedTransition`] record type
//! plus the guard-evaluation building blocks that don't need a live
//! perspective — [`build_query_input_for_requires`] (`ModelQuery` →
//! `ModelQueryInput` translation), [`cardinality_satisfied`]
//! (`count.{min,max}` check), and [`evidence_hash`] (deterministic seal
//! over a satisfied guard's evidence bag).
//!
//! See `super` (the `flow_evaluator` module doc) for the full slice
//! breakdown and design-authority pointer.

#![allow(dead_code)]

use crate::perspectives::shacl_parser::ConsensusRule;
use crate::perspectives::shacl_parser::{ModelQuery, ModelQueryCount, PropertyCondition};
use serde_json::{json, Value};
use sha2::{Digest, Sha256};

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
