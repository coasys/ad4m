//! Value-free model RPC diagnostics. The slot belongs to one dispatch task, never a socket.
use serde_json::{json, Value};
use std::sync::Mutex;

type Fields = Vec<(&'static str, String)>;
tokio::task_local! {
    static FIELDS: Mutex<Fields>;
}

pub(crate) async fn collect<F: std::future::Future>(f: F) -> (F::Output, Fields) {
    FIELDS
        .scope(Mutex::new(Vec::new()), async move {
            let out = f.await;
            let fields = FIELDS
                .with(|slot| std::mem::take(&mut *slot.lock().unwrap_or_else(|e| e.into_inner())));
            (out, fields)
        })
        .await
}

pub(crate) fn bounded(value: &str, max: usize) -> String {
    if value.len() <= max {
        return value.to_owned();
    }
    let mut end = max.saturating_sub(3);
    while !value.is_char_boundary(end) {
        end -= 1;
    }
    format!("{}...", &value[..end])
}

/// Only names survive: operand objects of value operators are not traversed.
fn where_shape(value: &Value, depth: usize) -> Value {
    if depth >= 16 {
        return json!("depth-limit");
    }
    match value {
        Value::Object(map) => {
            let mut out = serde_json::Map::new();
            // Explicit sorting also works when serde_json enables preserve_order.
            let mut keys: Vec<_> = map.keys().collect();
            keys.sort();
            for key in keys {
                let child = &map[key];
                let shape = match key.as_str() {
                    "eq" | "not" | "contains" | "between" | "lt" | "lte" | "gt" | "gte"
                    | "author" => json!("operand"),
                    _ => where_shape(child, depth + 1),
                };
                out.insert(key.clone(), shape);
            }
            Value::Object(out)
        }
        Value::Array(items) if items.iter().any(Value::is_object) => {
            Value::Array(items.iter().map(|v| where_shape(v, depth + 1)).collect())
        }
        Value::Array(_) => json!("in"),
        _ => json!("eq"),
    }
}

fn keys(value: &Value) -> Vec<&str> {
    let mut keys: Vec<_> = value
        .as_object()
        .into_iter()
        .flat_map(|m| m.keys().map(String::as_str))
        .collect();
    keys.sort_unstable();
    keys
}

/// Hash the complete sanitized shape (FNV-1a, not Rust's version-dependent DefaultHasher),
/// then bound its display. The hash is a grouping hint, not an identity/security primitive.
fn summarize(query: &str) -> (String, String) {
    let shape = match serde_json::from_str::<Value>(query) {
        Ok(q) if q.is_object() => {
            let mut order = Vec::new();
            let mut add = |property: &str, direction: &Value| {
                let direction = match direction.as_str() {
                    Some(d) if d.eq_ignore_ascii_case("desc") => "DESC",
                    _ => "ASC",
                };
                order.push(json!([property, direction]));
            };
            match &q["order"] {
                // Source key order: a multi-key `order` ranks by key order
                // (serde_json keeps insertion order here, `preserve_order`).
                Value::Object(map) => {
                    for (key, direction) in map {
                        add(key, direction);
                    }
                }
                Value::Array(items) => {
                    for item in items {
                        if let Some(p) = item.get(0).and_then(Value::as_str) {
                            add(p, item.get(1).unwrap_or(&Value::Null));
                        }
                    }
                }
                _ => {}
            }
            json!({"limit": q["limit"].as_u64(), "offset": q["offset"].as_u64(),
                "order": order, "scope": !q["parent"].is_null(),
                "where": where_shape(&q["where"], 0),
                "include": keys(&q["include"]), "projections": keys(&q["projections"])})
        }
        _ => json!({"invalid": true}),
    }
    .to_string();
    let hash = shape.bytes().fold(0xcbf29ce484222325u64, |h, b| {
        (h ^ u64::from(b)).wrapping_mul(0x100000001b3)
    });
    (bounded(&shape, 2048), format!("{hash:016x}"))
}

pub(crate) fn query(perspective: &str, classes: &[String], query: &str) {
    let _ = FIELDS.try_with(|slot| {
        let (shape, hash) = summarize(query);
        *slot.lock().unwrap_or_else(|e| e.into_inner()) = vec![
            ("perspective", bounded(perspective, 128)),
            ("classes", bounded(&classes.join(","), 512)),
            ("shape", shape),
            ("shape_hash", hash),
        ];
    });
}

pub(crate) fn results(json: &str) {
    let _ = FIELDS.try_with(|slot| {
        let mut fields = slot.lock().unwrap_or_else(|e| e.into_inner());
        if fields.is_empty() {
            return;
        }
        // Ignore instance bodies instead of allocating another multi-MB result tree.
        #[derive(serde::Deserialize)]
        struct Count {
            instances: Vec<serde::de::IgnoredAny>,
        }
        if let Ok(count) = serde_json::from_str::<Count>(json) {
            fields.push(("results", count.instances.len().to_string()));
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn values_never_reach_shape_or_hash() {
        let a = r#"{"parent":{"id":"secret"},"where":{"name":"secret","age":{"gt":23},"OR":[{"text":{"contains":"secret"}}]},"limit":20,"order":[["name","DESC"]],"include":{"children":true},"projections":{"$count":{"from":"secret"}}}"#;
        let b = a.replace("secret", "other").replace("23", "99");
        let (shape, hash) = summarize(a);
        assert_eq!((shape.clone(), hash.clone()), summarize(&b));
        assert!(!shape.contains("secret"));
        assert!(!shape.contains("23"));
        for name in [
            "name", "age", "gt", "OR", "contains", "children", "$count", "DESC",
        ] {
            assert!(shape.contains(name));
        }
        assert_eq!(hash.len(), 16);
        assert_ne!(summarize(a), summarize(&a.replace("gt", "lt")));
    }
    #[test]
    fn canonical_keys_and_bounded_utf8() {
        assert_eq!(
            summarize(r#"{"where":{"b":1,"a":2}}"#),
            summarize(r#"{"where":{"a":8,"b":9}}"#)
        );
        let query = json!({"where": {("é".repeat(3000)): "private"}}).to_string();
        let (shape, hash) = summarize(&query);
        assert!(shape.len() <= 2048);
        assert!(shape.ends_with("..."));
        assert_eq!(hash, summarize(&query).1);
        assert_eq!(bounded(&"é".repeat(1000), 512).len(), 511);
        assert_eq!(summarize("secret"), summarize("other invalid input"));
    }
    #[tokio::test]
    async fn slots_are_per_request_and_keep_initial_count() {
        let (_, fields) = collect(async {
            query("p", &["C".into()], "{}");
            results(r#"{"instances":[{},{}]}"#);
        })
        .await;
        assert!(fields.contains(&("results", "2".into())));
        assert!(collect(async {}).await.1.is_empty());
    }
    #[tokio::test]
    async fn concurrent_slots_do_not_mix_and_fields_are_bounded() {
        let (a, b) = tokio::join!(
            collect(async {
                query("a", &["é".repeat(1000)], "{}");
                tokio::task::yield_now().await;
                results(r#"{"instances":[],"totalCount":999}"#);
            }),
            collect(async {
                query("b", &["B".into()], "{}");
                tokio::task::yield_now().await;
                results(r#"{"instances":[{"text":"private"}]}"#);
            }),
        );
        assert!(a.1.contains(&("perspective", "a".into())));
        assert!(a.1.contains(&("results", "0".into())));
        assert!(
            a.1.iter()
                .find(|(key, _)| *key == "classes")
                .unwrap()
                .1
                .len()
                <= 512
        );
        assert!(b.1.contains(&("perspective", "b".into())));
        assert!(b.1.contains(&("results", "1".into())));
        assert!(!format!("{:?}", b.1).contains("private"));
    }

    #[test]
    fn multi_key_order_keeps_its_key_order() {
        let a = summarize(r#"{"order":{"timestamp":"DESC","name":"ASC"}}"#);
        let b = summarize(r#"{"order":{"name":"ASC","timestamp":"DESC"}}"#);
        assert_ne!(
            a.1, b.1,
            "different primary sort keys must not share a shape hash"
        );
        assert!(a.0.find("timestamp").unwrap() < a.0.find("name").unwrap());
    }

    #[test]
    fn malformed_order_does_not_panic_or_log_values() {
        for order in [
            json!([["name"]]),
            json!([null, 7]),
            json!({"name":"secret"}),
        ] {
            let (shape, _) = summarize(&json!({"order":order}).to_string());
            assert!(!shape.contains("secret"));
        }
    }
}
