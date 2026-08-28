//! Hydration: converting raw SPARQL result rows into typed JSON instances.
//!
//! The Oxigraph store returns flat rows with `?source ?predicate ?target
//! ?author ?timestamp` bindings.  This module groups those rows by source
//! IRI ([`group_results_by_source`]) and then builds fully typed JSON
//! objects ([`hydrate_instances`] / [`hydrate_one`]) by:
//!
//! - Parsing `literal:` URIs into native JSON types (string, number, boolean)
//! - Collecting collection (relation) values into arrays
//! - Selecting the latest value for scalar properties (last-write-wins by
//!   timestamp)
//! - Attaching computed metadata: `createdAt`, `updatedAt`, `author`,
//!   `timestamp`
//!
//! [`filter_properties`] strips unrequested properties from the final output.

use serde_json::{Map, Value};
use std::collections::{BTreeMap, HashMap};

use super::types::{InstanceLinks, ModelShape, ShapeProperty};
use super::utils::parse_literal_value;

/// Group raw SPARQL result rows by `?source` IRI.
///
/// Returns one [`InstanceLinks`] per unique source, preserving insertion
/// order (via `BTreeMap`) so that results are deterministic.
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

/// Hydrate all grouped link sets into JSON instance objects.
///
/// Delegates to [`hydrate_one`] for each group and collects the results.
pub(super) fn hydrate_instances(shape: &ModelShape, grouped: &[InstanceLinks]) -> Vec<Value> {
    grouped
        .iter()
        .filter_map(|inst_links| hydrate_one(shape, inst_links))
        .collect()
}

/// Hydrate a single instance from its collected links.
///
/// For each link `(predicate, target, author, timestamp)`:
/// - If the predicate maps to a **scalar property** (or flag), the latest
///   value by timestamp wins (`parse_literal_value` for typed conversion).
/// - If the predicate maps to a **collection relation**, the target is
///   appended to an array (sorted by timestamp).
/// - If multiple shape properties share the same predicate (e.g. several
///   `@HasMany` relations with `ad4m://has_child`), each gets a copy of
///   the targets.
///
/// The output includes synthetic fields: `id`, `baseExpression`, `createdAt`,
/// `updatedAt`, `author`, `timestamp`.
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
        // Whether to decode `literal:<type>:<value>` wire form is a
        // shape-level decision, not a per-target one. A relation with
        // `sh:datatype` (produced by `@HasMany({ datatype: "xsd:string" })`
        // or the equivalent JSON key on a hardwired class) holds encoded
        // literal values — the writer wraps `HasMany<string>` targets as
        // `literal:string:<hex>` under PR #874 typed literals, and the
        // reader must unwrap. A relation without `sh:datatype` points at
        // instance URIs (Recipe→Ingredient, Channel→Message, DIDs,
        // external URLs); those pass through byte-for-byte even when a
        // URI happens to start with `literal:`. See the `sh:datatype` vs
        // `sh:class` distinction in SHACL — this is the same split.
        let decode_literals = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.datatype.is_some());
        let decode = |target: &str| -> Value {
            if decode_literals && target.starts_with("literal:") {
                parse_literal_value(target)
            } else {
                Value::String(target.to_string())
            }
        };

        let is_scalar = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.is_scalar_relation);

        if is_scalar {
            // A scalar relation (`@HasOne` / `@BelongsToOne`) resolves
            // last-write-wins, exactly as a scalar property does above.
            //
            // It previously took the *earliest* link — the collection was sorted
            // ascending and the first entry won — so re-pointing a to-one relation
            // without first removing the old link kept serving the original target
            // forever, while a scalar property on the same instance updated
            // normally. Today `collectionSetter` deletes before adding, which
            // leaves one link and hides this; concurrent writes from two peers do
            // not, and neither will the incremental diff that replaces it.
            //
            // Strict `>` means the first of several links sharing a timestamp
            // wins, matching the scalar-property branch rather than diverging
            // from it on ties.
            let mut winner: Option<(&str, &str)> = None;
            for &(target, ts) in &values {
                if winner.map_or(true, |(_, best_ts)| ts > best_ts) {
                    winner = Some((target, ts));
                }
            }
            if let Some((target, _)) = winner {
                obj.insert(name.to_string(), decode(target));
            }
        } else {
            // Collapse duplicate targets.
            //
            // A link is stored as a direct triple plus a reifier keyed on
            // `sha256(source, predicate, target, timestamp)`, so two links
            // carrying the same triple at different timestamps — two peers
            // asserting the same membership, or a re-add racing a remote write —
            // share one triple but produce two reifiers. The instance query joins
            // through the reifier to recover author and timestamp, so it returns a
            // row per reifier and the target lands here twice.
            //
            // `$count` projections already answer `COUNT(DISTINCT ?t)`, so leaving
            // this undeduplicated made a count disagree with the list it counts
            // within a single query.
            //
            // `values` is sorted ascending, so retaining the first sighting of each
            // target keeps its earliest timestamp — the same instant `createdAt`
            // and the append-by-timestamp fallback ordering already use.
            let mut seen = std::collections::HashSet::new();
            values.retain(|&(target, _)| seen.insert(target));

            let arr: Vec<Value> = values.iter().map(|&(target, _)| decode(target)).collect();
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

/// Strip an instance down to only the requested properties.
///
/// `id` and `baseExpression` are always preserved.  Included relation
/// names are added to the keep-list by the caller.
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

        // Shape here uses plain `relation()` (no `sh:datatype`) — a
        // URI relation. Even though the fixture stored `literal:string:X`
        // targets, they pass through unchanged because the property does
        // not opt into literal decoding.  Tests that exercise the decode
        // path use a shape whose relation declares `datatype`.
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
                "{} must contain both child IDs (URI relation pass-through)",
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
        // URI relation, no `sh:datatype` — targets pass through
        // unchanged even when they carry the reserved `literal:` prefix.
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
    fn test_hydrate_hasmany_of_uri_targets_pass_through_unchanged() {
        // The common case: relation targets are subject-instance URIs
        // (e.g. `flux://message/abc`, `did:key:z…`). These never start
        // with `literal:` and must pass through the hydrator verbatim
        // so downstream reference lookups still resolve.
        let s = shape("Channel", vec![relation("messages", "ad4m://has_child")]);
        let inst = inst_links(
            "flux://channel/general",
            vec![
                ("ad4m://has_child", "flux://message/m1"),
                ("ad4m://has_child", "did:key:z6MkabcXYZ"),
                ("ad4m://has_child", "https://example.com/thing"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let messages: Vec<String> = result["messages"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect();
        assert_eq!(
            messages,
            vec![
                "flux://message/m1".to_string(),
                "did:key:z6MkabcXYZ".to_string(),
                "https://example.com/thing".to_string(),
            ],
            "URI relation targets must round-trip byte-for-byte"
        );
    }

    #[test]
    fn test_hydrate_hasmany_of_literal_string_targets_decodes() {
        // The bug this fix closes: a `HasMany<string>` (e.g. the
        // auto-processor's `InterpretationRun.sources` holding turn-IDs
        // as plain hex) reaches the store as `literal:string:<hex>`
        // wire form under PR #874. The property declares
        // `sh:datatype xsd:string` (the `@HasMany({ datatype:
        // "xsd:string" })` case), so the reader unwraps the wire form
        // and callers see plain values, matching what any subsequent
        // comparison (e.g. against `event.itemIds`) expects.
        let s = shape(
            "Run",
            vec![relation_with_datatype(
                "sources",
                "ad4m://interp/sources",
                "xsd://string",
            )],
        );
        let inst = inst_links(
            "ad4m://interp/run/abc",
            vec![
                ("ad4m://interp/sources", "literal:string:turn-hex-1"),
                ("ad4m://interp/sources", "literal:string:turn-hex-2"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let sources: Vec<String> = result["sources"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect();
        assert_eq!(
            sources,
            vec!["turn-hex-1".to_string(), "turn-hex-2".to_string()],
            "wire-form `literal:string:<hex>` HasMany targets must be decoded to plain values",
        );
    }

    // ---- duplicate collapsing --------------------------------------------
    //
    // A link is a direct triple plus a reifier keyed on
    // `sha256(source, predicate, target, timestamp)`. Two peers asserting the
    // same membership share the triple but mint two reifiers, and the instance
    // query joins through the reifier to recover author/timestamp — so the same
    // target arrives twice. These fixtures reproduce that by repeating a
    // `(predicate, target)` pair at two timestamps.

    #[test]
    fn test_hydrate_collection_collapses_duplicate_targets() {
        let s = shape("Post", vec![relation("signals", "we://signal")]);
        let inst = inst_links_at(
            "we://post/1",
            vec![
                ("we://signal", "we://signal/a", "2026-01-01T00:00:00.000Z"),
                // Same triple, asserted again by a peer a second later.
                ("we://signal", "we://signal/a", "2026-01-01T00:00:01.000Z"),
                ("we://signal", "we://signal/b", "2026-01-01T00:00:02.000Z"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let signals: Vec<&str> = result["signals"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        // Two links, one membership. `$count` already answered 1 here via
        // COUNT(DISTINCT ?t), so an undeduplicated list disagreed with the
        // count of the same relation inside one query.
        assert_eq!(
            signals,
            vec!["we://signal/a", "we://signal/b"],
            "a target asserted by two peers must appear once",
        );
    }

    #[test]
    fn test_hydrate_collection_dedup_keeps_earliest_position() {
        // The duplicate is the *earliest* link for `a`, so collapsing on first
        // sighting has to keep `a` ahead of `b` — not move it to where its
        // second assertion landed. Ordering is by earliest timestamp, the same
        // instant `createdAt` and the append-by-timestamp fallback use.
        let s = shape("Post", vec![relation("children", "we://children")]);
        let inst = inst_links_at(
            "we://post/1",
            vec![
                ("we://children", "we://block/a", "2026-01-01T00:00:00.000Z"),
                ("we://children", "we://block/b", "2026-01-01T00:00:01.000Z"),
                ("we://children", "we://block/a", "2026-01-01T00:00:09.000Z"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let children: Vec<&str> = result["children"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        assert_eq!(children, vec!["we://block/a", "we://block/b"]);
    }

    #[test]
    fn test_hydrate_collection_dedup_survives_literal_decoding() {
        // Decoding happens after collapsing, so a duplicated literal target
        // must collapse on its wire form and still decode once.
        let s = shape(
            "Run",
            vec![relation_with_datatype(
                "sources",
                "ad4m://interp/sources",
                "xsd://string",
            )],
        );
        let inst = inst_links_at(
            "ad4m://interp/run/1",
            vec![
                (
                    "ad4m://interp/sources",
                    "literal:string:turn-1",
                    "2026-01-01T00:00:00.000Z",
                ),
                (
                    "ad4m://interp/sources",
                    "literal:string:turn-1",
                    "2026-01-01T00:00:01.000Z",
                ),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        assert_eq!(
            result["sources"].as_array().unwrap().len(),
            1,
            "duplicate literal targets collapse before decoding",
        );
        assert_eq!(result["sources"][0].as_str().unwrap(), "turn-1");
    }

    // ---- scalar relations resolve last-write-wins -------------------------

    #[test]
    fn test_hydrate_scalar_relation_last_write_wins() {
        // Re-pointing a `@HasOne` without removing the old link previously kept
        // serving the *original* target forever, because the collection was
        // sorted ascending and the first entry won — while a scalar property on
        // the same instance updated normally.
        let s = shape("Space", vec![scalar_relation("location", "we://location")]);
        let inst = inst_links_at(
            "we://space/1",
            vec![
                ("we://location", "we://loc/old", "2026-01-01T00:00:00.000Z"),
                ("we://location", "we://loc/new", "2026-01-01T00:00:05.000Z"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        assert_eq!(
            result["location"].as_str().unwrap(),
            "we://loc/new",
            "a to-one relation must resolve to its latest link, as a scalar property does",
        );
    }

    #[test]
    fn test_hydrate_scalar_relation_lww_regardless_of_link_order() {
        // The winner is decided by timestamp, not by the order rows happened to
        // arrive from the store.
        let s = shape("Space", vec![scalar_relation("location", "we://location")]);
        let inst = inst_links_at(
            "we://space/1",
            vec![
                ("we://location", "we://loc/new", "2026-01-01T00:00:05.000Z"),
                ("we://location", "we://loc/old", "2026-01-01T00:00:00.000Z"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        assert_eq!(result["location"].as_str().unwrap(), "we://loc/new");
    }

    #[test]
    fn test_hydrate_scalar_relation_tie_matches_scalar_property() {
        // Two links sharing a timestamp: the scalar-property branch keeps the
        // first seen (its comparison is strict `>`), and the relation branch
        // must not diverge from it on ties.
        let s = shape(
            "Space",
            vec![
                scalar_relation("location", "we://location"),
                prop("name", "we://name"),
            ],
        );
        let inst = inst_links_at(
            "we://space/1",
            vec![
                (
                    "we://location",
                    "we://loc/first",
                    "2026-01-01T00:00:00.000Z",
                ),
                (
                    "we://location",
                    "we://loc/second",
                    "2026-01-01T00:00:00.000Z",
                ),
                (
                    "we://name",
                    "literal:string:first",
                    "2026-01-01T00:00:00.000Z",
                ),
                (
                    "we://name",
                    "literal:string:second",
                    "2026-01-01T00:00:00.000Z",
                ),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        assert_eq!(result["location"].as_str().unwrap(), "we://loc/first");
        assert_eq!(
            result["name"].as_str().unwrap(),
            "first",
            "the relation branch must tiebreak the same way the property branch does",
        );
    }

    #[test]
    fn test_hydrate_scalar_relation_absent_stays_absent() {
        // No link at all must leave the key off entirely rather than writing
        // null — callers distinguish "unset" from "explicitly nothing".
        let s = shape("Space", vec![scalar_relation("location", "we://location")]);
        let inst = inst_links_at(
            "we://space/1",
            vec![("we://other", "we://x", "2026-01-01T00:00:00.000Z")],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        assert!(result.get("location").is_none());
    }

    #[test]
    fn test_hydrate_hasmany_of_uri_relation_ignores_literal_prefix() {
        // Companion to the decode test: a URI relation (no `sh:datatype`)
        // passes targets through unchanged even when they carry the
        // reserved `literal:` prefix. Guards against the bug where a
        // relation-collection blindly decoded any `literal:*` target.
        let s = shape("Chan", vec![relation("posts", "ad4m://has_child")]);
        let inst = inst_links(
            "ad4m://chan/1",
            vec![
                ("ad4m://has_child", "literal:string:post_uri_1"),
                ("ad4m://has_child", "ad4m://post/normal_uri"),
            ],
        );
        let result = hydrate_one(&s, &inst).unwrap();
        let posts: Vec<String> = result["posts"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect();
        assert_eq!(
            posts,
            vec![
                "literal:string:post_uri_1".to_string(),
                "ad4m://post/normal_uri".to_string(),
            ],
            "URI relation targets must not be decoded even when the URI carries the `literal:` prefix",
        );
    }

    #[test]
    fn test_hydrate_hasmany_of_string_datatype_mixed_targets_decode_selectively() {
        // A literal-valued HasMany (declares `sh:datatype`) with a mix
        // of wire-form and non-wire-form entries: only the entries
        // carrying the `literal:` prefix get decoded, non-matching
        // entries pass through unchanged. Guards against an overreach
        // where enabling the datatype would rewrite every target.
        let s = shape(
            "Mix",
            vec![relation_with_datatype(
                "targets",
                "ns://ref",
                "xsd://string",
            )],
        );
        let inst = inst_links(
            "ns://mix/1",
            vec![
                ("ns://ref", "flux://message/real"),
                ("ns://ref", "literal:string:legacy-string"),
            ],
        );
        let result = hydrate_one(&s, &inst).unwrap();
        let targets: Vec<String> = result["targets"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect();
        assert_eq!(
            targets,
            vec![
                "flux://message/real".to_string(),
                "legacy-string".to_string()
            ],
        );
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
