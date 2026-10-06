//! Post-hydration where-clause filtering and multi-key instance sorting.
//!
//! Some where-clause conditions cannot be pushed into SPARQL (e.g. `Ops`
//! conditions on getter-computed properties, or complex numeric comparisons).
//! After hydration and getter evaluation, [`matches_where`] is used to
//! evaluate those remaining conditions in Rust.
//!
//! [`sort_instances`] provides multi-key sorting with type-aware comparison
//! (numeric before string, nulls pushed to end).

use super::types::{ModelShape, OrderDirection, WhereCondition, WhereOps};
use super::utils::to_f64;
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::BTreeMap;

/// Test whether an instance passes all conditions in a where clause.
///
/// `id` and `base` conditions are evaluated here unconditionally, even the
/// shapes the compiler normally pushes to SPARQL: re-testing one that was
/// pushed is cheap and cannot reject a row wrongly, whereas assuming it was
/// pushed is wrong inside any combinator the compiler declined.
///
/// String/array conditions on collection properties are evaluated here as well,
/// with contains-semantics ([`collection_contains`]). They used to be skipped,
/// on the assumption that SPARQL had applied them. Inside a declined
/// combinator that assumption let a role gate's DID condition pass for every
/// candidate.
pub(super) fn matches_where(
    instance: &Value,
    where_clause: &BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> bool {
    for (prop_name, condition) in where_clause {
        // --- Logical combinators ---

        // OR: instance must match at least one branch. Fails closed (rejects
        // the instance) if `condition` isn't the expected SubClauses shape,
        // rather than silently skipping the filter on malformed input.
        if prop_name == "OR" {
            let WhereCondition::SubClauses(branches) = condition else {
                return false;
            };
            if branches.is_empty() {
                return false;
            }
            if !branches
                .iter()
                .any(|branch| matches_where(instance, branch, shape))
            {
                return false;
            }
            continue;
        }

        // AND: instance must match every branch. Fails closed on malformed input.
        if prop_name == "AND" {
            let WhereCondition::SubClauses(branches) = condition else {
                return false;
            };
            if !branches
                .iter()
                .all(|branch| matches_where(instance, branch, shape))
            {
                return false;
            }
            continue;
        }

        // NOT: instance must NOT match the branch. Fails closed on malformed input.
        if prop_name == "NOT" {
            let Some(branch) = condition.as_not_clause() else {
                return false;
            };
            if matches_where(instance, &branch, shape) {
                return false;
            }
            continue;
        }

        if prop_name == "base" || prop_name == "id" {
            // Total over every condition shape, including the String and
            // StringArray ones the compiler usually does push to SPARQL.
            //
            // Skipping those used to be the fast path, on the assumption that
            // reaching here at all meant SPARQL had already applied them. That
            // assumption does not survive a combinator. When one branch of an
            // `OR` cannot be compiled the whole disjunction is declined and
            // left to this filter; a branch whose only clause was an id
            // equality then passed vacuously, `any` was trivially satisfied,
            // and the id constraint was applied in neither layer.
            //
            // Re-testing a condition SPARQL did push is redundant but never
            // wrong: a hydrated instance's `id` is its own source URI, the
            // same string the compiler matched on. One string comparison buys
            // the removal of the coordination.
            //
            // Hydrated instances use "id" (not "base"), so map "base" → "id".
            let lookup_key = if prop_name == "base" {
                "id"
            } else {
                prop_name.as_str()
            };
            if !matches_condition(&instance[lookup_key], condition) {
                return false;
            }
            continue;
        }

        // String/StringArray on a collection: contains-semantics, the same
        // question the SPARQL relation arm asks. `String` holds when some
        // value in the collection equals it. `StringArray` holds when some
        // value is in the set. That is "any", because
        // `VALUES ?v { … } ?source <p> ?v` matches an instance linked to any
        // one of them.
        //
        // This arm used to `continue`, on the assumption that SPARQL had
        // already applied the condition. At the top level of a clause it had,
        // because pushable leaves are emitted beside unpushable ones. Inside a
        // combinator the compiler declined, it had not: the branch re-runs
        // here, and a skipped leaf passes vacuously. For a role gate whose
        // `didProperty` names a collection, that leaf is the DID condition.
        // One unpushable sibling in an `or` branch was enough to make every
        // role instance match every candidate (#1129 review).
        //
        // Re-testing a condition SPARQL did push is redundant but never wrong,
        // the same trade the `id` arm makes. See `collection_contains` for how
        // the hydrated values are compared.
        if matches!(
            condition.eq_normalized(),
            WhereCondition::String(_) | WhereCondition::StringArray(_)
        ) && shape
            .properties
            .iter()
            .any(|p| p.name == *prop_name && p.is_collection)
        {
            if !collection_contains(&instance[prop_name], condition) {
                return false;
            }
            continue;
        }

        let val = &instance[prop_name];
        if !matches_condition(val, condition) {
            return false;
        }
    }
    true
}

/// Check whether a single JSON value matches a where condition.
///
/// Handles all [`WhereCondition`] variants: exact match (string, number,
/// bool), set membership (string/number arrays), and operator-based
/// comparisons ([`WhereOps`]).
pub(crate) fn matches_condition(val: &Value, condition: &WhereCondition) -> bool {
    match condition.eq_normalized() {
        WhereCondition::String(expected) => match val {
            Value::String(s) => s == expected,
            Value::Null => false,
            _ => val.to_string().trim_matches('"') == expected.as_str(),
        },
        WhereCondition::Number(expected) => {
            to_f64(val).map(|v| (v - expected).abs() < f64::EPSILON) == Some(true)
        }
        WhereCondition::Bool(expected) => val.as_bool() == Some(*expected),
        WhereCondition::StringArray(expected) => {
            // IN operator: value must be in the array
            match val {
                Value::String(s) => expected.contains(s),
                _ => {
                    let s = val.to_string().trim_matches('"').to_string();
                    expected.contains(&s)
                }
            }
        }
        WhereCondition::NumberArray(expected) => {
            if let Some(v) = to_f64(val) {
                expected.iter().any(|e| (v - e).abs() < f64::EPSILON)
            } else {
                false
            }
        }
        WhereCondition::Ops(ops) => matches_ops(val, ops),
        // SubClauses/SubClause are handled at the where-clause level (matches_where),
        // not per-value — reaching here means an unexpected field structure.
        WhereCondition::SubClauses(_) | WhereCondition::SubClause(_) => false,
    }
}

/// Contains-semantics for a String/StringArray condition on a collection
/// property: does any hydrated value satisfy it?
///
/// - An array holds when any element matches ([`matches_condition`] per
///   element). An empty array holds nothing.
/// - A single value is a to-one relation that hydration unwrapped
///   (`is_scalar_relation`). It is tested as itself.
/// - Absent or `null` holds nothing. That includes a getter-backed collection:
///   getters run after this filter, so its value is not there yet. The
///   condition then rejects the row, the same answer the scalar arm gives a
///   getter property. Rejecting is the safe failure. Passing would repeat the
///   fail-open this replaced.
///
/// Values are compared as hydrated. A collection with a `datatype` holds
/// decoded literals, so the condition must name the decoded value.
fn collection_contains(val: &Value, condition: &WhereCondition) -> bool {
    match val {
        Value::Array(items) => items.iter().any(|item| matches_condition(item, condition)),
        Value::Null => false,
        single => matches_condition(single, condition),
    }
}

/// Evaluate a value against operator-based conditions (`not`, `gt`, `lt`,
/// `gte`, `lte`, `between`, `contains`).
///
/// Numeric operators attempt `f64` coercion (including ISO-8601 timestamp
/// parsing).  Non-numeric strings fail numeric comparisons.  `contains`
/// does case-insensitive substring matching on strings and element-in-array
/// matching on arrays.
pub(super) fn matches_ops(val: &Value, ops: &WhereOps) -> bool {
    // Relation quantifiers are answerable only against the store — they ask
    // about linked *records*, not about a value already on this instance — so
    // they are compiled to `FILTER [NOT] EXISTS` and never evaluated here.
    //
    // Reaching this point means the compiler declined to push one down and the
    // clause fell back to post-hydration filtering. Fail closed: ignoring the
    // condition would return rows that do not satisfy the query, which is the
    // failure mode this whole path exists to avoid.
    //
    // Deliberately silent. This runs once per hydrated instance, from the
    // `retain` closure in `execute_model_query`, so a warning here is one line
    // per row of a result set that is about to be emptied — and it could only
    // ever say *that* a quantifier was declined, never why. The reasons are
    // logged once, at each decline site in `compile_relation_quantifier` and
    // its caller, where they are known.
    if ops.some.is_some() || ops.none.is_some() {
        return false;
    }

    // A nested `author` is a condition on the link that carries the value, and
    // after hydration the links are gone: the instance has one `author`, its
    // earliest link's. `refuse_unanswerable_link_author` refuses any query
    // that would bring one here, so this is the second line, and it fails
    // closed for the same reason as the quantifier arm above. `eq` reaches
    // here only beside `author` or beside another operator, which is refused
    // as malformed; alone it was unwrapped by `matches_condition`.
    if ops.author.is_some() || ops.eq.is_some() {
        return false;
    }

    // NOT
    if let Some(ref not_val) = ops.not {
        match not_val {
            Value::String(s) => {
                if let Value::String(v) = val {
                    if v == s {
                        return false;
                    }
                }
            }
            Value::Number(n) => {
                if let Some(v) = to_f64(val) {
                    if let Some(e) = n.as_f64() {
                        if (v - e).abs() < f64::EPSILON {
                            return false;
                        }
                    }
                }
            }
            Value::Bool(b) => {
                if val.as_bool() == Some(*b) {
                    return false;
                }
            }
            Value::Array(arr) => {
                // NOT IN: value must NOT be in the array
                for item in arr {
                    if match (val, item) {
                        (Value::String(v), Value::String(s)) => v == s,
                        (Value::Number(_), Value::Number(_)) => to_f64(val)
                            .zip(item.as_f64())
                            .map(|(a, b)| (a - b).abs() < f64::EPSILON)
                            .unwrap_or(false),
                        // Cross-type: ISO string vs epoch number (timestamps)
                        (Value::String(_), Value::Number(_))
                        | (Value::Number(_), Value::String(_)) => to_f64(val)
                            .zip(to_f64(item))
                            .map(|(a, b)| (a - b).abs() < f64::EPSILON)
                            .unwrap_or(false),
                        _ => false,
                    } {
                        return false;
                    }
                }
            }
            _ => {}
        }
    }

    // Numeric comparisons
    if let Some(v) = to_f64(val) {
        if let Some(lt) = ops.lt {
            if v >= lt {
                return false;
            }
        }
        if let Some(lte) = ops.lte {
            if v > lte {
                return false;
            }
        }
        if let Some(gt) = ops.gt {
            if v <= gt {
                return false;
            }
        }
        if let Some(gte) = ops.gte {
            if v < gte {
                return false;
            }
        }
        if let Some((lo, hi)) = ops.between {
            if v < lo || v > hi {
                return false;
            }
        }
    } else {
        // For string values, try numeric parse; if it fails AND a numeric op is
        // present, the condition does not match (instead of silently passing).
        let has_numeric_op = ops.lt.is_some()
            || ops.lte.is_some()
            || ops.gt.is_some()
            || ops.gte.is_some()
            || ops.between.is_some();

        if let Value::String(s) = val {
            match s.parse::<f64>() {
                Ok(sv) => {
                    if let Some(lt) = ops.lt {
                        if sv >= lt {
                            return false;
                        }
                    }
                    if let Some(lte) = ops.lte {
                        if sv > lte {
                            return false;
                        }
                    }
                    if let Some(gt) = ops.gt {
                        if sv <= gt {
                            return false;
                        }
                    }
                    if let Some(gte) = ops.gte {
                        if sv < gte {
                            return false;
                        }
                    }
                    if let Some((lo, hi)) = ops.between {
                        if sv < lo || sv > hi {
                            return false;
                        }
                    }
                }
                Err(_) if has_numeric_op => {
                    // Non-numeric string with a numeric comparator → no match
                    return false;
                }
                _ => {}
            }
        }
        // If value is null and we have numeric conditions, don't match
        if val.is_null() {
            if ops.lt.is_some()
                || ops.lte.is_some()
                || ops.gt.is_some()
                || ops.gte.is_some()
                || ops.between.is_some()
            {
                return false;
            }
        }
    }

    // Contains
    if let Some(ref contains_val) = ops.contains {
        match val {
            Value::String(s) => {
                let needle = match contains_val {
                    Value::String(cs) => cs.clone(),
                    _ => contains_val.to_string(),
                };
                if !s.to_lowercase().contains(&needle.to_lowercase()) {
                    return false;
                }
            }
            Value::Array(arr) => {
                let found = arr.iter().any(|item| match (item, contains_val) {
                    (Value::String(a), Value::String(b)) => a == b,
                    (Value::Number(_), Value::Number(_)) => item == contains_val,
                    _ => false,
                });
                if !found {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}

// ---------------------------------------------------------------------------
// Sorting
// ---------------------------------------------------------------------------

/// Sort instances in-place by the given multi-key order specification.
///
/// Each key is evaluated in order; ties on the first key fall through to
/// the second, and so on.  Uses [`compare_values`] for type-aware comparison.
///
/// Keys may be dotted paths (`"location.name"`) for relation-property sorts.
/// When the intermediate value is an array (a forward collection relation),
/// the first element is used.  Unknown or missing paths compare as `Null`
/// (sorted to end), which preserves SPARQL-side ordering for pushed sorts.
pub(super) fn sort_instances(instances: &mut [Value], order: &[(String, OrderDirection)]) {
    instances.sort_by(|a, b| {
        for (prop, dir) in order {
            let av = extract_sort_value(a, prop);
            let bv = extract_sort_value(b, prop);

            let cmp = compare_values(&av, &bv);

            if cmp != Ordering::Equal {
                return if *dir == OrderDirection::DESC {
                    cmp.reverse()
                } else {
                    cmp
                };
            }
        }
        Ordering::Equal
    });
}

/// Resolve a sort key against a JSON instance, supporting dotted paths.
///
/// A plain key (`"name"`) clones the direct field value.  A dotted path
/// (`"location.name"`) traverses nested objects — if an intermediate value
/// is an array, the first element is used for sort purposes.  Returns
/// `Value::Null` for any missing step so unknown paths sort to the end.
fn extract_sort_value(instance: &Value, key: &str) -> Value {
    if let Some(dot_pos) = key.find('.') {
        let head = &key[..dot_pos];
        let tail = &key[dot_pos + 1..];
        let intermediate = &instance[head];
        match intermediate {
            Value::Object(_) => extract_sort_value(intermediate, tail),
            Value::Array(arr) => arr
                .first()
                .map(|v| extract_sort_value(v, tail))
                .unwrap_or(Value::Null),
            _ => Value::Null,
        }
    } else {
        instance[key].clone()
    }
}

/// Compare two JSON values with type-aware ordering.
///
/// Ordering rules:
/// 1. `Null` values are pushed to the end (greater than any non-null).
/// 2. If both values can be coerced to `f64`, numeric comparison is used.
/// 3. Otherwise, values are compared as strings (lexicographic).
pub(super) fn compare_values(a: &Value, b: &Value) -> Ordering {
    // Handle nulls — push to end
    match (a.is_null(), b.is_null()) {
        (true, true) => return Ordering::Equal,
        (true, false) => return Ordering::Greater,
        (false, true) => return Ordering::Less,
        _ => {}
    }

    // Try numeric comparison first
    if let (Some(an), Some(bn)) = (to_f64(a), to_f64(b)) {
        return an.partial_cmp(&bn).unwrap_or(Ordering::Equal);
    }

    // String comparison
    let as_str = match a {
        Value::String(s) => s.clone(),
        _ => a.to_string(),
    };
    let bs_str = match b {
        Value::String(s) => s.clone(),
        _ => b.to_string(),
    };

    as_str.cmp(&bs_str)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::*;
    use serde_json::{json, Value};
    use std::cmp::Ordering;

    #[test]
    fn test_matches_condition_string() {
        let val = Value::String("hello".to_string());
        assert!(matches_condition(
            &val,
            &WhereCondition::String("hello".to_string())
        ));
        assert!(!matches_condition(
            &val,
            &WhereCondition::String("world".to_string())
        ));
    }

    #[test]
    fn test_matches_condition_number() {
        let val = Value::Number(42.into());
        assert!(matches_condition(&val, &WhereCondition::Number(42.0)));
        assert!(!matches_condition(&val, &WhereCondition::Number(43.0)));
    }

    #[test]
    fn test_matches_ops_gt_lt() {
        let val = Value::Number(5.into());
        assert!(matches_ops(
            &val,
            &WhereOps {
                gt: Some(3.0),
                lt: Some(10.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                gt: Some(5.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_between() {
        let val = Value::Number(5.into());
        assert!(matches_ops(
            &val,
            &WhereOps {
                between: Some((1.0, 10.0)),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                between: Some((6.0, 10.0)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_contains() {
        let val = Value::String("hello world".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("world".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("xyz".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not() {
        let val = Value::String("hello".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::String("world".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::String("hello".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_compare_values_numeric() {
        assert_eq!(
            compare_values(&Value::Number(1.into()), &Value::Number(2.into())),
            Ordering::Less
        );
        assert_eq!(
            compare_values(&Value::Number(2.into()), &Value::Number(1.into())),
            Ordering::Greater
        );
    }

    #[test]
    fn test_compare_values_null() {
        assert_eq!(
            compare_values(&Value::Null, &Value::Number(1.into())),
            Ordering::Greater
        );
        assert_eq!(
            compare_values(&Value::Number(1.into()), &Value::Null),
            Ordering::Less
        );
    }

    #[test]
    fn test_sort_instances() {
        let mut instances = vec![
            json!({"name": "C", "age": 30}),
            json!({"name": "A", "age": 10}),
            json!({"name": "B", "age": 20}),
        ];
        sort_instances(&mut instances, &[("age".to_string(), OrderDirection::ASC)]);
        assert_eq!(instances[0]["age"], 10);
        assert_eq!(instances[1]["age"], 20);
        assert_eq!(instances[2]["age"], 30);
    }

    #[test]
    fn test_matches_condition_bool() {
        assert!(matches_condition(
            &Value::Bool(true),
            &WhereCondition::Bool(true)
        ));
        assert!(!matches_condition(
            &Value::Bool(true),
            &WhereCondition::Bool(false)
        ));
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::Bool(true)
        ));
    }

    #[test]
    fn test_matches_condition_string_array() {
        let cond = WhereCondition::StringArray(vec!["active".to_string(), "pending".to_string()]);
        assert!(matches_condition(
            &Value::String("active".to_string()),
            &cond
        ));
        assert!(matches_condition(
            &Value::String("pending".to_string()),
            &cond
        ));
        assert!(!matches_condition(
            &Value::String("done".to_string()),
            &cond
        ));
    }

    #[test]
    fn test_matches_condition_number_array() {
        let cond = WhereCondition::NumberArray(vec![1.0, 2.0, 3.0]);
        assert!(matches_condition(&Value::Number(2.into()), &cond));
        assert!(!matches_condition(&Value::Number(4.into()), &cond));
    }

    #[test]
    fn test_matches_condition_string_on_null() {
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::String("x".to_string())
        ));
    }

    #[test]
    fn test_matches_condition_number_on_null() {
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::Number(5.0)
        ));
    }

    #[test]
    fn test_matches_ops_lte_gte() {
        let val = Value::Number(5.into());
        assert!(matches_ops(
            &val,
            &WhereOps {
                gte: Some(5.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                gte: Some(6.0),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &val,
            &WhereOps {
                lte: Some(5.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                lte: Some(4.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_number() {
        let val = Value::Number(42.into());
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::Number(42.into())),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::Number(43.into())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_bool() {
        assert!(!matches_ops(
            &Value::Bool(true),
            &WhereOps {
                not: Some(Value::Bool(true)),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &Value::Bool(true),
            &WhereOps {
                not: Some(Value::Bool(false)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_array() {
        let val = Value::String("active".to_string());
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(json!(["active", "pending"])),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(json!(["done", "archived"])),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_contains_array() {
        let val = json!(["apple", "banana", "cherry"]);
        assert!(matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("banana".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("grape".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_null_with_numeric_ops() {
        assert!(!matches_ops(
            &Value::Null,
            &WhereOps {
                gt: Some(0.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &Value::Null,
            &WhereOps {
                between: Some((0.0, 100.0)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_non_numeric_string_with_numeric_ops() {
        assert!(!matches_ops(
            &Value::String("hello".to_string()),
            &WhereOps {
                gt: Some(0.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_numeric_string() {
        let val = Value::String("42".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                gt: Some(40.0),
                lt: Some(50.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_where_multiple_conditions() {
        let instance = json!({
            "id": "test://1",
            "name": "Task 1",
            "status": "active",
            "priority": 5
        });
        let s = shape(
            "Task",
            vec![
                prop("name", "task://name"),
                prop("status", "task://status"),
                prop("priority", "task://priority"),
            ],
        );

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        where_clause.insert("priority".to_string(), WhereCondition::Number(5.0));
        assert!(matches_where(&instance, &where_clause, &s));

        let mut where_clause2 = BTreeMap::new();
        where_clause2.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        where_clause2.insert("priority".to_string(), WhereCondition::Number(10.0));
        assert!(!matches_where(&instance, &where_clause2, &s));
    }

    #[test]
    fn test_matches_where_filters_id_string() {
        // This used to assert the opposite — that a *wrong* id still matched,
        // because the arm skipped String conditions as "already pushed".
        let instance = json!({"id": "test://1", "name": "X"});
        let s = shape("Test", vec![prop("name", "test://name")]);

        let mut wrong = BTreeMap::new();
        wrong.insert(
            "id".to_string(),
            WhereCondition::String("test://wrong".to_string()),
        );
        assert!(!matches_where(&instance, &wrong, &s));

        let mut right = BTreeMap::new();
        right.insert(
            "id".to_string(),
            WhereCondition::String("test://1".to_string()),
        );
        assert!(matches_where(&instance, &right, &s));
    }

    #[test]
    fn test_matches_where_filters_id_string_array() {
        let instance = json!({"id": "test://1"});
        let s = shape("Test", vec![]);

        let mut excluded = BTreeMap::new();
        excluded.insert(
            "id".to_string(),
            WhereCondition::StringArray(vec!["test://2".to_string(), "test://3".to_string()]),
        );
        assert!(!matches_where(&instance, &excluded, &s));

        let mut included = BTreeMap::new();
        included.insert(
            "id".to_string(),
            WhereCondition::StringArray(vec!["test://1".to_string(), "test://2".to_string()]),
        );
        assert!(matches_where(&instance, &included, &s));
    }

    #[test]
    fn test_matches_where_filters_base_string() {
        // "base" is the same constraint under its wire name, and maps to the
        // hydrated "id" key.
        let instance = json!({"id": "test://1"});
        let s = shape("Test", vec![]);

        let mut wrong = BTreeMap::new();
        wrong.insert(
            "base".to_string(),
            WhereCondition::String("test://wrong".to_string()),
        );
        assert!(!matches_where(&instance, &wrong, &s));

        let mut right = BTreeMap::new();
        right.insert(
            "base".to_string(),
            WhereCondition::String("test://1".to_string()),
        );
        assert!(matches_where(&instance, &right, &s));
    }

    #[test]
    fn test_matches_where_id_ops_not_skipped() {
        let instance = json!({"id": "test://1"});
        let s = shape("Test", vec![]);

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "id".to_string(),
            WhereCondition::Ops(WhereOps {
                contains: Some(Value::String("test".to_string())),
                ..Default::default()
            }),
        );
        assert!(matches_where(&instance, &where_clause, &s));
    }

    /// String/StringArray on a collection is contains-semantics, not a skip.
    ///
    /// This test used to assert the skip: `tags: "nonexistent"` matched
    /// `["a", "b"]`. Every row where the condition is false is the row a
    /// skip admits. `members` in the last rows is a role's DID collection, the
    /// case the #1129 review found failing open.
    #[test]
    #[rustfmt::skip]
    fn test_matches_where_collection_string_is_contains() {
        let s = shape("Test", vec![
            relation("tags", "test://tag"),
            scalar_relation("owner", "test://owner"),
            relation("members", "test://member"),
        ]);
        let strs = |v: &[&str]| WhereCondition::StringArray(v.iter().map(|s| s.to_string()).collect());
        let one = |v: &str| WhereCondition::String(v.to_string());
        let cases: Vec<(&str, Value, &str, WhereCondition, bool)> = vec![
            ("String: an element equals it",        json!({"tags": ["a", "b"]}), "tags", one("b"), true),
            ("String: no element equals it",        json!({"tags": ["a", "b"]}), "tags", one("nonexistent"), false),
            ("String: empty collection",            json!({"tags": []}),         "tags", one("a"), false),
            ("String: absent collection",           json!({}),                   "tags", one("a"), false),
            ("String: null collection",             json!({"tags": null}),       "tags", one("a"), false),
            ("StringArray: any element in the set", json!({"tags": ["a", "b"]}), "tags", strs(&["x", "b"]), true),
            ("StringArray: no element in the set",  json!({"tags": ["a", "b"]}), "tags", strs(&["x", "y"]), false),
            // `matches_condition(null, StringArray)` stringifies to "null", so
            // this row is what the explicit `Null` arm exists for.
            ("StringArray: null is not \"null\"",   json!({"tags": null}),       "tags", strs(&["null"]), false),
            ("unwrapped to-one relation: equal",    json!({"owner": "did:a"}),   "owner", one("did:a"), true),
            ("unwrapped to-one relation: unequal",  json!({"owner": "did:a"}),   "owner", one("did:b"), false),
            ("role gate: the member",               json!({"members": ["did:a"]}), "members", one("did:a"), true),
            ("role gate: a non-member",             json!({"members": ["did:a"]}), "members", one("did:m"), false),
        ];
        for (name, instance, prop, condition, expected) in cases {
            let wc = make_where(vec![(prop, condition)]);
            assert_eq!(matches_where(&instance, &wc, &s), expected, "{name}");
            // Inside an `OR` the compiler declined, the same answer. This is
            // the route the role gate reached the skip by.
            let or = make_where(vec![("OR", WhereCondition::SubClauses(vec![wc]))]);
            assert_eq!(matches_where(&instance, &or, &s), expected, "{name} (inside OR)");
        }
    }

    #[test]
    fn test_sort_instances_desc() {
        let mut instances = vec![
            json!({"name": "A", "score": 10}),
            json!({"name": "B", "score": 30}),
            json!({"name": "C", "score": 20}),
        ];
        sort_instances(
            &mut instances,
            &[("score".to_string(), OrderDirection::DESC)],
        );
        assert_eq!(instances[0]["score"], 30);
        assert_eq!(instances[1]["score"], 20);
        assert_eq!(instances[2]["score"], 10);
    }

    #[test]
    fn test_sort_instances_multi_key() {
        let mut instances = vec![
            json!({"group": "B", "name": "Z"}),
            json!({"group": "A", "name": "Y"}),
            json!({"group": "A", "name": "X"}),
        ];
        sort_instances(
            &mut instances,
            &[
                ("group".to_string(), OrderDirection::ASC),
                ("name".to_string(), OrderDirection::ASC),
            ],
        );
        assert_eq!(instances[0]["group"], "A");
        assert_eq!(instances[0]["name"], "X");
        assert_eq!(instances[1]["group"], "A");
        assert_eq!(instances[1]["name"], "Y");
        assert_eq!(instances[2]["group"], "B");
    }

    #[test]
    fn test_sort_instances_null_pushed_to_end() {
        let mut instances = vec![
            json!({"name": "B"}),
            json!({"name": null}),
            json!({"name": "A"}),
        ];
        sort_instances(&mut instances, &[("name".to_string(), OrderDirection::ASC)]);
        assert_eq!(instances[0]["name"], "A");
        assert_eq!(instances[1]["name"], "B");
        assert!(instances[2]["name"].is_null());
    }

    #[test]
    fn test_compare_values_string() {
        assert_eq!(
            compare_values(
                &Value::String("apple".to_string()),
                &Value::String("banana".to_string())
            ),
            Ordering::Less
        );
        assert_eq!(
            compare_values(
                &Value::String("same".to_string()),
                &Value::String("same".to_string())
            ),
            Ordering::Equal
        );
    }

    // ---- extract_sort_value tests ------------------------------------------

    #[test]
    fn test_extract_sort_value_plain_key() {
        let inst = json!({"name": "Alice", "age": 30});
        assert_eq!(extract_sort_value(&inst, "name"), json!("Alice"));
        assert_eq!(extract_sort_value(&inst, "age"), json!(30));
        assert_eq!(extract_sort_value(&inst, "missing"), Value::Null);
    }

    #[test]
    fn test_extract_sort_value_dotted_path_object() {
        let inst = json!({
            "location": { "name": "London", "country": "UK" }
        });
        assert_eq!(extract_sort_value(&inst, "location.name"), json!("London"));
        assert_eq!(extract_sort_value(&inst, "location.country"), json!("UK"));
        assert_eq!(extract_sort_value(&inst, "location.missing"), Value::Null);
    }

    #[test]
    fn test_extract_sort_value_dotted_path_array_uses_first() {
        // Forward collection relation: "location" is an array of objects;
        // only the first element's property is used for sorting.
        let inst = json!({
            "location": [
                { "name": "Alpha" },
                { "name": "Zeta" }
            ]
        });
        assert_eq!(
            extract_sort_value(&inst, "location.name"),
            json!("Alpha"),
            "should use first element of array for dotted sort"
        );
    }

    #[test]
    fn test_extract_sort_value_dotted_path_empty_array() {
        let inst = json!({ "tags": [] });
        assert_eq!(
            extract_sort_value(&inst, "tags.name"),
            Value::Null,
            "empty array should give Null"
        );
    }

    #[test]
    fn test_extract_sort_value_dotted_path_scalar_intermediate() {
        // Intermediate value is a plain string, not an object or array.
        let inst = json!({ "title": "hello" });
        assert_eq!(
            extract_sort_value(&inst, "title.something"),
            Value::Null,
            "scalar intermediate should give Null"
        );
    }

    // ---- sort_instances dotted-path tests -----------------------------------

    #[test]
    fn test_sort_instances_dotted_path_asc() {
        let mut instances = vec![
            json!({ "id": "c", "location": { "name": "Zebra" } }),
            json!({ "id": "a", "location": { "name": "Alpha" } }),
            json!({ "id": "b", "location": { "name": "Middle" } }),
        ];
        sort_instances(
            &mut instances,
            &[("location.name".to_string(), OrderDirection::ASC)],
        );
        let names: Vec<&str> = instances
            .iter()
            .map(|i| i["id"].as_str().unwrap())
            .collect();
        assert_eq!(names, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_sort_instances_dotted_path_desc() {
        let mut instances = vec![
            json!({ "id": "a", "author": { "name": "Alice" } }),
            json!({ "id": "c", "author": { "name": "Charlie" } }),
            json!({ "id": "b", "author": { "name": "Bob" } }),
        ];
        sort_instances(
            &mut instances,
            &[("author.name".to_string(), OrderDirection::DESC)],
        );
        let names: Vec<&str> = instances
            .iter()
            .map(|i| i["id"].as_str().unwrap())
            .collect();
        assert_eq!(names, vec!["c", "b", "a"]);
    }

    #[test]
    fn test_sort_instances_dotted_path_nulls_to_end() {
        // Instances missing the nested path sort to the end.
        let mut instances = vec![
            json!({ "id": "no-location" }),
            json!({ "id": "b", "location": { "name": "Beta" } }),
            json!({ "id": "a", "location": { "name": "Alpha" } }),
        ];
        sort_instances(
            &mut instances,
            &[("location.name".to_string(), OrderDirection::ASC)],
        );
        let ids: Vec<&str> = instances
            .iter()
            .map(|i| i["id"].as_str().unwrap())
            .collect();
        assert_eq!(ids, vec!["a", "b", "no-location"]);
    }

    #[test]
    fn test_sort_instances_plain_key_unchanged() {
        // Verify that introducing extract_sort_value didn't break plain key sorts.
        let mut instances = vec![
            json!({ "score": 10 }),
            json!({ "score": 1 }),
            json!({ "score": 5 }),
        ];
        sort_instances(
            &mut instances,
            &[("score".to_string(), OrderDirection::ASC)],
        );
        let scores: Vec<i64> = instances
            .iter()
            .map(|i| i["score"].as_i64().unwrap())
            .collect();
        assert_eq!(scores, vec![1, 5, 10]);
    }

    // ---- OR / AND / NOT combinator tests ------------------------------------

    fn empty_shape() -> ModelShape {
        ModelShape {
            target_class: "Test".to_string(),
            shape_uri: "".to_string(),
            properties: vec![],
            include_relations: vec![],
            interpretation_hint: None,
        }
    }

    fn make_where(entries: Vec<(&str, WhereCondition)>) -> BTreeMap<String, WhereCondition> {
        entries
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect()
    }

    #[test]
    fn test_or_matches_when_any_branch_passes() {
        let shape = empty_shape();
        let instance = json!({ "status": "active", "name": "Alice" });

        // OR: status == "active" OR name == "Bob"
        // "active" matches first branch → passes
        let wc = make_where(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "status",
                    WhereCondition::String("active".to_string()),
                )]),
                make_where(vec![("name", WhereCondition::String("Bob".to_string()))]),
            ]),
        )]);
        assert!(matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_or_fails_when_no_branch_passes() {
        let shape = empty_shape();
        let instance = json!({ "status": "inactive", "name": "Alice" });

        // OR: status == "active" OR name == "Bob"
        // neither matches → fails
        let wc = make_where(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "status",
                    WhereCondition::String("active".to_string()),
                )]),
                make_where(vec![("name", WhereCondition::String("Bob".to_string()))]),
            ]),
        )]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_or_empty_branches_fails() {
        let shape = empty_shape();
        let instance = json!({ "status": "active" });
        let wc = make_where(vec![("OR", WhereCondition::SubClauses(vec![]))]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_or_malformed_condition_fails_closed() {
        // "OR" present but its value isn't SubClauses (e.g. a raw string) —
        // must reject the instance rather than silently skip the filter.
        let shape = empty_shape();
        let instance = json!({ "status": "active" });
        let wc = make_where(vec![("OR", WhereCondition::String("bogus".to_string()))]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_and_passes_when_all_branches_pass() {
        let shape = empty_shape();
        let instance = json!({ "status": "active", "role": "admin" });

        let wc = make_where(vec![(
            "AND",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "status",
                    WhereCondition::String("active".to_string()),
                )]),
                make_where(vec![("role", WhereCondition::String("admin".to_string()))]),
            ]),
        )]);
        assert!(matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_and_fails_when_any_branch_fails() {
        let shape = empty_shape();
        let instance = json!({ "status": "active", "role": "viewer" });

        let wc = make_where(vec![(
            "AND",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "status",
                    WhereCondition::String("active".to_string()),
                )]),
                make_where(vec![("role", WhereCondition::String("admin".to_string()))]),
            ]),
        )]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_and_empty_branches_passes() {
        let shape = empty_shape();
        let instance = json!({ "status": "active" });
        let wc = make_where(vec![("AND", WhereCondition::SubClauses(vec![]))]);
        // all() on empty iterator is true
        assert!(matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_and_malformed_condition_fails_closed() {
        let shape = empty_shape();
        let instance = json!({ "status": "active" });
        let wc = make_where(vec![("AND", WhereCondition::Number(1.0))]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_not_passes_when_branch_does_not_match() {
        let shape = empty_shape();
        let instance = json!({ "status": "active" });

        // NOT { status: "deleted" } — instance has "active", so NOT matches
        let wc = make_where(vec![(
            "NOT",
            WhereCondition::SubClause(make_where(vec![(
                "status",
                WhereCondition::String("deleted".to_string()),
            )])),
        )]);
        assert!(matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_not_fails_when_branch_matches() {
        let shape = empty_shape();
        let instance = json!({ "status": "deleted" });

        let wc = make_where(vec![(
            "NOT",
            WhereCondition::SubClause(make_where(vec![(
                "status",
                WhereCondition::String("deleted".to_string()),
            )])),
        )]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_not_malformed_condition_fails_closed() {
        let shape = empty_shape();
        let instance = json!({ "status": "active" });
        let wc = make_where(vec![("NOT", WhereCondition::Bool(true))]);
        assert!(!matches_where(&instance, &wc, &shape));
    }

    #[test]
    fn test_or_combined_with_top_level_condition() {
        let shape = empty_shape();

        // { role: "admin", OR: [{ name: "Alice" }, { name: "Bob" }] }
        // role must be "admin" AND (name is Alice or Bob)
        let instance_pass = json!({ "role": "admin", "name": "Alice" });
        let instance_fail_role = json!({ "role": "viewer", "name": "Alice" });
        let instance_fail_name = json!({ "role": "admin", "name": "Charlie" });

        let wc = {
            let mut m = BTreeMap::new();
            m.insert(
                "role".to_string(),
                WhereCondition::String("admin".to_string()),
            );
            m.insert(
                "OR".to_string(),
                WhereCondition::SubClauses(vec![
                    make_where(vec![("name", WhereCondition::String("Alice".to_string()))]),
                    make_where(vec![("name", WhereCondition::String("Bob".to_string()))]),
                ]),
            );
            m
        };

        assert!(matches_where(&instance_pass, &wc, &shape));
        assert!(!matches_where(&instance_fail_role, &wc, &shape));
        assert!(!matches_where(&instance_fail_name, &wc, &shape));
    }

    #[test]
    fn test_nested_or_inside_or() {
        let shape = empty_shape();
        // OR: [{ OR: [{ a: "1" }, { a: "2" }] }, { b: "x" }]
        // matches when a is "1" or "2", OR b is "x"
        let inner_or = WhereCondition::SubClauses(vec![
            make_where(vec![("a", WhereCondition::String("1".to_string()))]),
            make_where(vec![("a", WhereCondition::String("2".to_string()))]),
        ]);
        let wc = make_where(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                {
                    let mut m = BTreeMap::new();
                    m.insert("OR".to_string(), inner_or);
                    m
                },
                make_where(vec![("b", WhereCondition::String("x".to_string()))]),
            ]),
        )]);

        assert!(matches_where(&json!({ "a": "1" }), &wc, &shape));
        assert!(matches_where(&json!({ "a": "2" }), &wc, &shape));
        assert!(matches_where(&json!({ "b": "x" }), &wc, &shape));
        assert!(!matches_where(&json!({ "a": "3", "b": "y" }), &wc, &shape));
    }

    /// The reproducer for the leak the `id` arm used to have.
    ///
    /// Reaching this filter at all means the compiler declined the `OR` — here,
    /// because the second branch's id is not IRI-valid, so it would emit a
    /// `FILTER` that tests `?source` rather than a `VALUES` that binds it, and
    /// a `UNION` branch that does not bind `?source` is not sound to push.
    ///
    /// With the arm skipping String conditions, each branch's only clause was
    /// skipped, every branch returned true vacuously, `any` was satisfied, and
    /// the whole disjunction admitted every hydrated row.
    #[test]
    fn test_or_of_ids_filters_after_the_compiler_declines() {
        let shape = empty_shape();
        let wc = make_where(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "id",
                    WhereCondition::String("test://post/1".to_string()),
                )]),
                make_where(vec![(
                    "id",
                    WhereCondition::String("draft-note".to_string()),
                )]),
            ]),
        )]);

        assert!(matches_where(
            &json!({ "id": "test://post/1" }),
            &wc,
            &shape
        ));
        assert!(matches_where(&json!({ "id": "draft-note" }), &wc, &shape));
        // The one that used to be admitted along with everything else.
        assert!(!matches_where(
            &json!({ "id": "test://post/2" }),
            &wc,
            &shape
        ));
    }

    /// The likelier route to the same place: an `OR` declined not because of
    /// the id branch but because a *sibling* branch could not compile — a
    /// getter-computed property has no predicate to emit. The id branch is
    /// perfectly pushable on its own and still ends up here.
    #[test]
    fn test_or_of_id_and_getter_prop_filters_both_branches() {
        let shape = empty_shape();
        let wc = make_where(vec![(
            "OR",
            WhereCondition::SubClauses(vec![
                make_where(vec![(
                    "id",
                    WhereCondition::String("test://post/1".to_string()),
                )]),
                make_where(vec![("computed", WhereCondition::String("x".to_string()))]),
            ]),
        )]);

        assert!(matches_where(
            &json!({ "id": "test://post/1" }),
            &wc,
            &shape
        ));
        assert!(matches_where(
            &json!({ "id": "test://post/9", "computed": "x" }),
            &wc,
            &shape
        ));
        assert!(!matches_where(
            &json!({ "id": "test://post/9", "computed": "y" }),
            &wc,
            &shape
        ));
    }

    /// `NOT` over an id inverts properly now that the inner clause is tested
    /// rather than skipped. Skipping made the inner branch match everything,
    /// so `NOT` rejected everything.
    #[test]
    fn test_not_over_id_excludes_only_that_id() {
        let shape = empty_shape();
        let wc = make_where(vec![(
            "NOT",
            WhereCondition::SubClause(make_where(vec![(
                "id",
                WhereCondition::String("test://post/1".to_string()),
            )])),
        )]);

        assert!(!matches_where(
            &json!({ "id": "test://post/1" }),
            &wc,
            &shape
        ));
        assert!(matches_where(
            &json!({ "id": "test://post/2" }),
            &wc,
            &shape
        ));
    }
}
