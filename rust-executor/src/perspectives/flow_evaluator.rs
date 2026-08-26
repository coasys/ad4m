//! Slice 10.4a of the flow-implementation arc — the pure primitives that turn a
//! [`ModelQuery`] guard into a [`ModelQueryInput`] the perspective's
//! `model_query` accepts, check cardinality against the returned result, and
//! content-hash the evidence bag that a satisfied transition carries into the
//! [`SatisfiedTransition`] produced by slice 10.4a2's async evaluator.
//!
//! Design authority: `planning/flow-interpretation-hints-design.md` §5 step 5
//! ("Post-processing (engine, deterministic)") and §7 (`ConsensusRule` +
//! `didProperty` role-gate).
//!
//! # What this module owns (slice 10.4a — this commit)
//!
//! - [`SatisfiedTransition`] — the record slice 10.4a2's async evaluator
//!   returns per (flow_instance, reachable_next_state) whose `requires` are
//!   fully satisfied.
//! - [`build_query_input_for_requires`] — pure translator from `ModelQuery`
//!   (flow-side type) to `serde_json::Value` (`model_query`'s input shape).
//!   Substitutes `$did` in `didProperty` at translation time. Recursive over
//!   `ModelQuery.or`.
//! - [`cardinality_satisfied`] — pure `count.{min,max}` cardinality check.
//! - [`evidence_hash`] — deterministic SHA256 of a (class, sorted matched-ids)
//!   pair. Used to seed the evidence field on the FlowTransitionProposal that
//!   slice 10.4b emits, so a re-verification pass in slice 10.6 can catch a
//!   tampered proposal.
//!
//! # What slice 10.4a2 adds (next commit, NOT here)
//!
//! - `evaluate_single_query(perspective, mq, acting_did) -> Result<(bool, Vec<String>)>`
//!   — the one async call site into `PerspectiveInstance::model_query`.
//! - `evaluate_state_requires(perspective, requires, acting_did) -> Result<Option<Vec<String>>>`
//!   — AND across the array; returns `Some(evidence_ids)` when all satisfied.
//! - `evaluate_flow_transitions(perspective, active_flows, flows_by_name, acting_did)`
//!   — the top composer that walks every active flow's reachable next-states
//!   and returns `Vec<SatisfiedTransition>`.
//!
//! # Why pure
//!
//! Slice 10.4b will emit `FlowTransitionProposal` writes on behalf of the
//! extraction DID from these results. Any bug in the ModelQuery→ModelQueryInput
//! translation would either miss a satisfied requires (flow silently stalls)
//! or synthesize a wrong-guard proposal (garbage in the flow's evidence
//! chain). Isolating the translation from graph I/O gives us fixture-driven
//! unit tests for every `PropertyCondition` variant + $did substitution
//! without needing a live perspective.

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
}
