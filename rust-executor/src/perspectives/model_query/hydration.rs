use serde_json::{Map, Value};
use std::collections::{BTreeMap, HashMap};

use super::types::{InstanceLinks, ModelShape, ShapeProperty};
use super::utils::parse_literal_value;

/// Group SPARQL result rows by `?source` to collect all links per instance.
pub(super) fn group_results_by_source(rows: &[Value], _shape: &ModelShape) -> Vec<InstanceLinks> {
    let mut map: BTreeMap<String, Vec<(String, String, String, String)>> = BTreeMap::new();

    for row in rows {
        let source = match row["source"].as_str() {
            Some(s) => s.to_string(),
            None => continue,
        };
        let predicate = row["predicate"].as_str().unwrap_or("").to_string();
        let target = row["target"].as_str().unwrap_or("").to_string();
        let author = row["author"].as_str().unwrap_or("").to_string();
        let timestamp = row["timestamp"].as_str().unwrap_or("").to_string();

        map.entry(source)
            .or_default()
            .push((predicate, target, author, timestamp));
    }

    map.into_iter()
        .map(|(source, links)| InstanceLinks { source, links })
        .collect()
}

/// Hydrate instances from grouped link data.
pub(super) fn hydrate_instances(shape: &ModelShape, grouped: &[InstanceLinks]) -> Vec<Value> {
    grouped
        .iter()
        .filter_map(|inst_links| hydrate_one(shape, inst_links))
        .collect()
}

/// Hydrate a single instance from its links.
pub(super) fn hydrate_one(shape: &ModelShape, inst: &InstanceLinks) -> Option<Value> {
    let mut obj = Map::new();

    obj.insert("id".to_string(), Value::String(inst.source.clone()));
    obj.insert(
        "baseExpression".to_string(),
        Value::String(inst.source.clone()),
    );

    let mut pred_to_props: HashMap<&str, Vec<&ShapeProperty>> = HashMap::new();
    for p in shape.properties.iter().filter(|p| p.getter.is_none()) {
        pred_to_props
            .entry(p.predicate.as_str())
            .or_default()
            .push(p);
    }

    let mut prop_timestamps: HashMap<&str, &str> = HashMap::new();
    let mut collection_values: HashMap<&str, Vec<(&str, &str)>> = HashMap::new();
    let mut earliest_timestamp: Option<&str> = None;
    let mut earliest_author: Option<&str> = None;
    let mut latest_timestamp: Option<&str> = None;

    for (predicate, target, author, timestamp) in &inst.links {
        let ts = timestamp.as_str();
        match earliest_timestamp {
            None => {
                earliest_timestamp = Some(ts);
                earliest_author = Some(author.as_str());
            }
            Some(et) if ts < et => {
                earliest_timestamp = Some(ts);
                earliest_author = Some(author.as_str());
            }
            _ => {}
        }
        match latest_timestamp {
            None => {
                latest_timestamp = Some(ts);
            }
            Some(lt) if ts > lt => {
                latest_timestamp = Some(ts);
            }
            _ => {}
        }

        if let Some(props) = pred_to_props.get(predicate.as_str()) {
            for prop in props {
                if prop.is_collection {
                    collection_values
                        .entry(prop.name.as_str())
                        .or_default()
                        .push((target.as_str(), ts));
                } else if prop.is_flag {
                    let current_ts = prop_timestamps.get(prop.name.as_str()).copied();
                    if current_ts.is_none() || current_ts.map(|t| ts > t).unwrap_or(true) {
                        prop_timestamps.insert(prop.name.as_str(), ts);
                        let val = parse_literal_value(target);
                        obj.insert(prop.name.clone(), val);
                    }
                } else {
                    let current_ts = prop_timestamps.get(prop.name.as_str()).copied();
                    if current_ts.is_none() || current_ts.map(|t| ts > t).unwrap_or(true) {
                        prop_timestamps.insert(prop.name.as_str(), ts);
                        let val = parse_literal_value(target);
                        obj.insert(prop.name.clone(), val);
                    }
                }
            }
        }
    }

    for (name, mut values) in collection_values {
        values.sort_by_key(|&(_, ts)| ts);
        let is_relation = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.direction.is_some());
        let arr: Vec<Value> = values
            .iter()
            .map(|&(target, _)| {
                if is_relation {
                    Value::String(target.to_string())
                } else if target.starts_with("literal:") {
                    parse_literal_value(target)
                } else {
                    Value::String(target.to_string())
                }
            })
            .collect();
        let is_scalar = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.is_scalar_relation);
        if is_scalar {
            if let Some(first) = arr.into_iter().next() {
                obj.insert(name.to_string(), first);
            }
        } else {
            obj.insert(name.to_string(), Value::Array(arr));
        }
    }

    if let Some(ts) = earliest_timestamp {
        obj.insert("createdAt".to_string(), Value::String(ts.to_string()));
    }
    if let Some(ts) = latest_timestamp {
        obj.insert("updatedAt".to_string(), Value::String(ts.to_string()));
    }
    if let Some(author) = earliest_author {
        obj.insert("author".to_string(), Value::String(author.to_string()));
    }

    if let Some(ts) = earliest_timestamp {
        obj.insert("timestamp".to_string(), Value::String(ts.to_string()));
    }

    Some(Value::Object(obj))
}

/// Filter an instance to only include requested properties.
pub(super) fn filter_properties(instance: Value, requested: &[String]) -> Value {
    if let Value::Object(mut obj) = instance {
        let always_keep = ["id", "baseExpression"];
        let keys: Vec<String> = obj.keys().cloned().collect();
        for key in keys {
            if always_keep.contains(&key.as_str()) {
                continue;
            }
            if !requested.iter().any(|r| r == &key) {
                obj.remove(&key);
            }
        }
        Value::Object(obj)
    } else {
        instance
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::test_helpers::*;
    use serde_json::json;

    #[test]
    fn test_filter_properties() {
        let inst = json!({
            "id": "test://1",
            "baseExpression": "test://1",
            "name": "Test",
            "age": 25,
            "secret": "hidden"
        });
        let filtered = filter_properties(inst, &["name".to_string(), "age".to_string()]);
        assert!(filtered.get("id").is_some());
        assert!(filtered.get("name").is_some());
        assert!(filtered.get("age").is_some());
        assert!(filtered.get("secret").is_none());
    }

    #[test]
    fn test_hydrate_shared_predicate_all_relations_populated() {
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                prop("name", "flux://has_channel_name"),
                relation("views", "ad4m://has_child"),
                relation("messages", "ad4m://has_child"),
                relation("conversations", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch1",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("flux://has_channel_name", "literal:string:General"),
                ("ad4m://has_child", "literal:string:app1"),
                ("ad4m://has_child", "literal:string:conv1"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        let views = result["views"]
            .as_array()
            .expect("views should be an array");
        let messages = result["messages"]
            .as_array()
            .expect("messages should be an array");
        let conversations = result["conversations"]
            .as_array()
            .expect("conversations should be an array");

        assert_eq!(views.len(), 2, "views must have 2 items");
        assert_eq!(messages.len(), 2, "messages must have 2 items");
        assert_eq!(conversations.len(), 2, "conversations must have 2 items");

        let expected_ids: Vec<&str> = vec!["literal:string:app1", "literal:string:conv1"];
        for rel_name in &["views", "messages", "conversations"] {
            let ids: Vec<String> = result[rel_name]
                .as_array()
                .unwrap()
                .iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect();
            assert_eq!(
                ids, expected_ids,
                "{} must contain both child IDs",
                rel_name
            );
        }
    }

    #[test]
    fn test_hydrate_shared_predicate_single_relation_still_works() {
        let s = shape(
            "Simple",
            vec![
                flag("type", "test://type", "test://simple"),
                relation("items", "test://has_item"),
            ],
        );

        let inst = inst_links(
            "literal:string:s1",
            vec![
                ("test://type", "test://simple"),
                ("test://has_item", "literal:string:item1"),
                ("test://has_item", "literal:string:item2"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let items = result["items"]
            .as_array()
            .expect("items should be an array");
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn test_hydrate_shared_predicate_with_distinct_predicates() {
        let s = shape(
            "Model",
            vec![
                flag("type", "test://type", "test://model"),
                relation("alpha", "test://pred_a"),
                relation("beta", "test://pred_b"),
            ],
        );

        let inst = inst_links(
            "literal:string:m1",
            vec![
                ("test://type", "test://model"),
                ("test://pred_a", "literal:string:a1"),
                ("test://pred_b", "literal:string:b1"),
                ("test://pred_b", "literal:string:b2"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        let alpha = result["alpha"].as_array().unwrap();
        let beta = result["beta"].as_array().unwrap();

        assert_eq!(alpha.len(), 1, "alpha has 1 item");
        assert_eq!(beta.len(), 2, "beta has 2 items");
        assert_eq!(alpha[0].as_str().unwrap(), "literal:string:a1");
    }

    #[test]
    fn test_hydrate_shared_predicate_no_targets() {
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                relation("views", "ad4m://has_child"),
                relation("messages", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch_empty",
            vec![("flux://entry_type", "flux://has_channel")],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        assert!(
            result.get("views").is_none(),
            "views should be absent when no has_child links"
        );
        assert!(
            result.get("messages").is_none(),
            "messages should be absent when no has_child links"
        );
    }

    #[test]
    fn test_hydrate_shared_predicate_preserves_scalar_properties() {
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                prop("name", "flux://has_channel_name"),
                prop("description", "flux://has_channel_description"),
                relation("views", "ad4m://has_child"),
                relation("posts", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch2",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("flux://has_channel_name", "literal:string:General"),
                (
                    "flux://has_channel_description",
                    "literal:string:Main%20channel",
                ),
                ("ad4m://has_child", "literal:string:child1"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        assert_eq!(result["name"], json!("General"));
        assert_eq!(result["description"], json!("Main channel"));
        assert_eq!(result["type"], json!("flux://has_channel"));

        assert_eq!(result["views"].as_array().unwrap().len(), 1);
        assert_eq!(result["posts"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn test_hydrate_many_relations_same_predicate() {
        let rel_names = vec![
            "views",
            "messages",
            "conversations",
            "childChannels",
            "boards",
            "taskColumns",
            "tasks",
            "posts",
        ];
        let mut props = vec![flag("type", "flux://entry_type", "flux://has_channel")];
        for name in &rel_names {
            props.push(relation(name, "ad4m://has_child"));
        }
        let s = shape("Channel", props);

        let inst = inst_links(
            "literal:string:ch_stress",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("ad4m://has_child", "literal:string:c1"),
                ("ad4m://has_child", "literal:string:c2"),
                ("ad4m://has_child", "literal:string:c3"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        for name in &rel_names {
            let arr = result[name]
                .as_array()
                .unwrap_or_else(|| panic!("{} should be an array", name));
            assert_eq!(
                arr.len(),
                3,
                "{} must have all 3 children, got {}",
                name,
                arr.len()
            );
        }
    }
}
