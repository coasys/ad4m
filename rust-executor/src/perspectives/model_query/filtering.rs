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
/// Conditions that are known to have been pushed to SPARQL (e.g. simple
/// string equality on `id`, or string/array conditions on collection
/// relations) are skipped here to avoid redundant work.
pub(super) fn matches_where(
    instance: &Value,
    where_clause: &BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> bool {
    for (prop_name, condition) in where_clause {
        if prop_name == "base" || prop_name == "id" {
            // String and StringArray id conditions are pushed to SPARQL.
            // Complex ops (Ops, NumberArray, etc.) still need post-hydration.
            // Hydrated instances use "id" (not "base"), so map "base" → "id".
            let lookup_key = if prop_name == "base" {
                "id"
            } else {
                prop_name.as_str()
            };
            match condition {
                WhereCondition::String(_) | WhereCondition::StringArray(_) => continue,
                _ => {
                    let val = &instance[lookup_key];
                    if !matches_condition(val, condition) {
                        return false;
                    }
                    continue;
                }
            }
        }

        // Relation-based where conditions (String/StringArray on collection props)
        // are already pushed to SPARQL — skip them here.
        if matches!(
            condition,
            WhereCondition::String(_) | WhereCondition::StringArray(_)
        ) {
            if shape
                .properties
                .iter()
                .any(|p| p.name == *prop_name && p.is_collection)
            {
                continue;
            }
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
pub(super) fn matches_condition(val: &Value, condition: &WhereCondition) -> bool {
    match condition {
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
pub(super) fn sort_instances(instances: &mut [Value], order: &[(String, OrderDirection)]) {
    instances.sort_by(|a, b| {
        for (prop, dir) in order {
            let av = &a[prop];
            let bv = &b[prop];

            let cmp = compare_values(av, bv);

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
    fn test_matches_where_skips_id_string() {
        let instance = json!({"id": "test://1", "name": "X"});
        let s = shape("Test", vec![prop("name", "test://name")]);

        let mut where_clause_wrong = BTreeMap::new();
        where_clause_wrong.insert(
            "id".to_string(),
            WhereCondition::String("test://wrong".to_string()),
        );
        assert!(matches_where(&instance, &where_clause_wrong, &s));
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

    #[test]
    fn test_matches_where_skips_collection_string() {
        let instance = json!({"id": "test://1", "tags": ["a", "b"]});
        let s = shape("Test", vec![relation("tags", "test://tag")]);

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "tags".to_string(),
            WhereCondition::String("nonexistent".to_string()),
        );
        assert!(matches_where(&instance, &where_clause, &s));
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
}
