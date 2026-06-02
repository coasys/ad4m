use super::getters::{
    convert_ask_to_batched_select, evaluate_getters, evaluate_getters_batch,
    inject_values_into_select, strip_trailing_limit,
};
use super::projection::{
    build_projection_order_clause, build_projection_where_patterns, resolve_projections,
};
use super::query::execute_model_query;
use super::shape::parse_shape_from_json;
use super::sparql_builder::build_instance_sparql;
use super::types::{ModelShape, ShapeProperty};
use super::utils::literal_percent_encode;
use super::*;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap};

fn make_link(source: &str, predicate: &str, target: &str, ts: &str) -> DecoratedLinkExpression {
    DecoratedLinkExpression {
        author: "did:key:test123".to_string(),
        timestamp: ts.to_string(),
        data: Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: DecoratedExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
            valid: Some(true),
            invalid: Some(false),
        },
        status: None,
    }
}

#[test]
fn test_full_model_query_with_where_filter() {
    // Create an in-memory store
    let store = SparqlStore::new(None).unwrap();

    // Simulate a Recipe with:
    //   - Flag: <ad4m://type> → <ad4m://recipe>
    //   - Name: <recipe://name> → literal:string:Recipe%201

    let base1 = "literal:string:recipe1base";

    let name_target = format!("literal:string:{}", literal_percent_encode("Recipe 1"));

    // Add the type flag link
    let flag_link = make_link(base1, "ad4m://type", "ad4m://recipe", "1700000000000");
    store.add_link(&flag_link).unwrap();

    // Add the name link
    let name_link = make_link(base1, "recipe://name", &name_target, "1700000000001");
    store.add_link(&name_link).unwrap();

    // Shape JSON (like what TS sends)
    let shape_json = r#"{
        "className": "Recipe",
        "properties": {
            "type": {
                "predicate": "ad4m://type",
                "required": true,
                "flag": true,
                "initial": "ad4m://recipe"
            },
            "name": {
                "predicate": "recipe://name",
                "required": false,
                "resolveLanguage": "literal"
            }
        },
        "relations": {}
    }"#;

    // Query without WHERE - should find 1 instance
    let query_no_where = ModelQueryInput::default();
    let result = execute_model_query(&store, "Recipe", &query_no_where, Some(shape_json)).unwrap();
    assert_eq!(
        result.instances.len(),
        1,
        "Should find 1 recipe without WHERE"
    );

    // Check that name is hydrated
    let name_val = &result.instances[0]["name"];
    assert_eq!(name_val, &json!("Recipe 1"), "Name should be 'Recipe 1'");

    // Query WITH WHERE - should also find 1 instance
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "name".to_string(),
        WhereCondition::String("Recipe 1".to_string()),
    );
    let query_with_where = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };
    let result2 =
        execute_model_query(&store, "Recipe", &query_with_where, Some(shape_json)).unwrap();
    assert_eq!(
        result2.instances.len(),
        1,
        "WHERE name='Recipe 1' should match 1 recipe"
    );
}

#[test]
fn test_where_clause_raw_uri_property() {
    // Properties without resolve_language store raw URIs as targets.
    // The where clause must match the raw URI, not wrap it in literal:string:...
    let store = SparqlStore::new(None).unwrap();

    let base1 = "literal:string:todo1";

    // Flag link
    store
        .add_link(&make_link(
            base1,
            "ad4m://type",
            "ad4m://todo",
            "1700000000000",
        ))
        .unwrap();

    // State property — raw URI target (no resolveLanguage)
    store
        .add_link(&make_link(
            base1,
            "todo://state",
            "todo://ready",
            "1700000000001",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Todo",
        "properties": {
            "type": {
                "predicate": "ad4m://type",
                "required": true,
                "flag": true,
                "initial": "ad4m://todo"
            },
            "state": {
                "predicate": "todo://state",
                "required": true
            }
        },
        "relations": {}
    }"#;

    // Where clause matching raw URI
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "state".to_string(),
        WhereCondition::String("todo://ready".to_string()),
    );
    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };
    let result = execute_model_query(&store, "Todo", &query, Some(shape_json)).unwrap();
    assert_eq!(
        result.instances.len(),
        1,
        "WHERE state='todo://ready' should match raw URI target"
    );
    assert_eq!(result.instances[0]["state"], json!("todo://ready"));
}

#[test]
fn test_where_clause_literal_prop_with_raw_uri_value() {
    // @Property defaults resolveLanguage to "literal", but constructor
    // initial values are stored as raw URIs. The where clause must match
    // both literal-encoded and raw URI forms.
    let store = SparqlStore::new(None).unwrap();

    let base1 = "literal:string:todo1";

    store
        .add_link(&make_link(
            base1,
            "ad4m://type",
            "ad4m://todo",
            "1700000000000",
        ))
        .unwrap();

    // Raw URI target — set by constructor action, NOT literal-encoded
    store
        .add_link(&make_link(
            base1,
            "todo://state",
            "todo://ready",
            "1700000000001",
        ))
        .unwrap();

    // Shape has resolveLanguage: "literal" (the @Property default)
    let shape_json = r#"{
        "className": "Todo",
        "properties": {
            "type": {
                "predicate": "ad4m://type",
                "required": true,
                "flag": true,
                "initial": "ad4m://todo"
            },
            "state": {
                "predicate": "todo://state",
                "required": true,
                "resolveLanguage": "literal"
            }
        },
        "relations": {}
    }"#;

    // Where clause should match even though the value is a URI stored raw
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "state".to_string(),
        WhereCondition::String("todo://ready".to_string()),
    );
    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };
    let result = execute_model_query(&store, "Todo", &query, Some(shape_json)).unwrap();
    assert_eq!(
        result.instances.len(),
        1,
        "WHERE state='todo://ready' should match raw URI even with resolveLanguage: literal"
    );
}

// -----------------------------------------------------------------------
// Integration test: shared predicate across multiple @HasMany relations
//
// This simulates the real Channel model from Flux where 8+ relations
// all use "ad4m://has_child".  Without the fix, only the last relation
// in HashMap iteration order receives targets; the others (like "views")
// are empty, causing include resolution to return zero results.
// -----------------------------------------------------------------------

#[test]
fn test_shared_predicate_relations_all_populated_via_store() {
    // Simulate a Channel with views, messages, and conversations all using
    // the same predicate "ad4m://has_child".  Each child has a different
    // flag type so include resolution (if applied later) can discriminate.
    let store = SparqlStore::new(None).unwrap();

    let channel_base = "literal:string:channel1";

    // Channel flag
    store
        .add_link(&make_link(
            channel_base,
            "flux://entry_type",
            "flux://has_channel",
            "1700000000000",
        ))
        .unwrap();

    // Channel name
    let name_target = format!("literal:string:{}", literal_percent_encode("General"));
    store
        .add_link(&make_link(
            channel_base,
            "flux://has_channel_name",
            &name_target,
            "1700000000001",
        ))
        .unwrap();

    // Child 1: an "App" (flag flux://has_app)
    let app_base = "literal:string:app1";
    store
        .add_link(&make_link(
            channel_base,
            "ad4m://has_child",
            app_base,
            "1700000000002",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            app_base,
            "flux://entry_type",
            "flux://has_app",
            "1700000000003",
        ))
        .unwrap();
    let app_name_target = format!("literal:string:{}", literal_percent_encode("Chat"));
    store
        .add_link(&make_link(
            app_base,
            "flux://has_name",
            &app_name_target,
            "1700000000004",
        ))
        .unwrap();

    // Child 2: a "Conversation" (flag flux://has_conversation)
    let conv_base = "literal:string:conv1";
    store
        .add_link(&make_link(
            channel_base,
            "ad4m://has_child",
            conv_base,
            "1700000000005",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            conv_base,
            "flux://entry_type",
            "flux://has_conversation",
            "1700000000006",
        ))
        .unwrap();

    // Child 3: a "Message" (flag flux://has_message)
    let msg_base = "literal:string:msg1";
    store
        .add_link(&make_link(
            channel_base,
            "ad4m://has_child",
            msg_base,
            "1700000000007",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            msg_base,
            "flux://entry_type",
            "flux://has_message",
            "1700000000008",
        ))
        .unwrap();

    // Shape JSON with 3 relations sharing ad4m://has_child and no includes
    let shape_json = r#"{
        "className": "Channel",
        "properties": {
            "type": {
                "predicate": "flux://entry_type",
                "required": true,
                "flag": true,
                "initial": "flux://has_channel"
            },
            "name": {
                "predicate": "flux://has_channel_name",
                "required": false,
                "resolveLanguage": "literal"
            }
        },
        "relations": {
            "views": {
                "predicate": "ad4m://has_child",
                "target": "App"
            },
            "messages": {
                "predicate": "ad4m://has_child",
                "target": "Message"
            },
            "conversations": {
                "predicate": "ad4m://has_child",
                "target": "Conversation"
            }
        }
    }"#;

    let query = ModelQueryInput::default();
    let result = execute_model_query(&store, "Channel", &query, Some(shape_json)).unwrap();

    assert_eq!(result.instances.len(), 1, "Should find 1 channel");

    let channel = &result.instances[0];
    assert_eq!(channel["name"], json!("General"));

    // All 3 relations must have all 3 children (raw IRI strings, no include)
    let views = channel["views"].as_array().expect("views must be an array");
    let messages = channel["messages"]
        .as_array()
        .expect("messages must be an array");
    let conversations = channel["conversations"]
        .as_array()
        .expect("conversations must be an array");

    // Without include resolution, all 3 children appear in each relation
    // (the store can't discriminate by target type without include)
    assert_eq!(
        views.len(),
        3,
        "views must have 3 raw child IDs (no include filter)"
    );
    assert_eq!(
        messages.len(),
        3,
        "messages must have 3 raw child IDs (no include filter)"
    );
    assert_eq!(
        conversations.len(),
        3,
        "conversations must have 3 raw child IDs (no include filter)"
    );

    // Verify the actual IDs are present
    let expected_ids = vec![
        "literal:string:app1",
        "literal:string:conv1",
        "literal:string:msg1",
    ];
    for rel_name in &["views", "messages", "conversations"] {
        let ids: Vec<String> = channel[*rel_name]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect();
        for eid in &expected_ids {
            assert!(
                ids.contains(&eid.to_string()),
                "{} should contain {} but got {:?}",
                rel_name,
                eid,
                ids
            );
        }
    }
}

#[test]
fn test_shared_predicate_with_unique_predicates_no_cross_contamination() {
    // Ensure relations with distinct predicates don't bleed into each other
    // even when one predicate is shared.
    let store = SparqlStore::new(None).unwrap();

    let parent = "literal:string:parent1";

    // Parent flag
    store
        .add_link(&make_link(
            parent,
            "test://type",
            "test://parent_type",
            "1700000000000",
        ))
        .unwrap();

    // Child via shared predicate
    store
        .add_link(&make_link(
            parent,
            "test://has_child",
            "literal:string:shared_child",
            "1700000000001",
        ))
        .unwrap();

    // Child via unique predicate
    store
        .add_link(&make_link(
            parent,
            "test://has_special",
            "literal:string:special_child",
            "1700000000002",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Parent",
        "properties": {
            "type": {
                "predicate": "test://type",
                "required": true,
                "flag": true,
                "initial": "test://parent_type"
            }
        },
        "relations": {
            "alpha": {
                "predicate": "test://has_child",
                "target": "Alpha"
            },
            "beta": {
                "predicate": "test://has_child",
                "target": "Beta"
            },
            "special": {
                "predicate": "test://has_special",
                "target": "Special"
            }
        }
    }"#;

    let query = ModelQueryInput::default();
    let result = execute_model_query(&store, "Parent", &query, Some(shape_json)).unwrap();

    assert_eq!(result.instances.len(), 1);
    let inst = &result.instances[0];

    // alpha and beta both share test://has_child → both get shared_child
    let alpha = inst["alpha"].as_array().expect("alpha must be array");
    let beta = inst["beta"].as_array().expect("beta must be array");
    let special = inst["special"].as_array().expect("special must be array");

    assert_eq!(alpha.len(), 1, "alpha should have 1 child");
    assert_eq!(beta.len(), 1, "beta should have 1 child");
    assert_eq!(special.len(), 1, "special should have 1 child");

    assert_eq!(alpha[0].as_str().unwrap(), "literal:string:shared_child");
    assert_eq!(beta[0].as_str().unwrap(), "literal:string:shared_child");
    assert_eq!(special[0].as_str().unwrap(), "literal:string:special_child");
}

// --- IncludeProjection helpers ---

#[test]
fn test_build_projection_where_patterns_empty_when_no_clause() {
    let proj = ProjectionInput {
        from: "signals".to_string(),
        count: true,
        target_shape: None,
        where_clause: None,
        limit: None,
        order: None,
    };
    assert_eq!(build_projection_where_patterns(&proj), "");
}

#[test]
fn test_build_projection_where_patterns_id_filter() {
    let mut wc = BTreeMap::new();
    wc.insert(
        "id".to_string(),
        WhereCondition::String("signal://abc".to_string()),
    );
    let proj = ProjectionInput {
        from: "signals".to_string(),
        count: false,
        target_shape: None,
        where_clause: Some(wc),
        limit: None,
        order: None,
    };
    let patterns = build_projection_where_patterns(&proj);
    assert!(
        patterns.contains("FILTER(STR(?t) = \"signal://abc\")"),
        "expected id IRI filter, got: {patterns}"
    );
}

#[test]
fn test_build_projection_where_patterns_with_target_shape() {
    let target_shape = json!({
        "className": "Signal",
        "properties": {
            "signalTypeId": { "predicate": "signal://type" }
        },
        "relations": {}
    });
    let mut wc = BTreeMap::new();
    wc.insert(
        "signalTypeId".to_string(),
        WhereCondition::String("like".to_string()),
    );
    let proj = ProjectionInput {
        from: "signals".to_string(),
        count: true,
        target_shape: Some(target_shape),
        where_clause: Some(wc),
        limit: None,
        order: None,
    };
    let patterns = build_projection_where_patterns(&proj);
    assert!(
        patterns.contains("?t <signal://type>"),
        "expected triple pattern for signal://type, got: {patterns}"
    );
    assert!(
        patterns.contains("FILTER"),
        "expected FILTER with fn/parse_literal, got: {patterns}"
    );
}

#[test]
fn test_build_projection_order_clause_empty_when_no_order() {
    let proj = ProjectionInput {
        from: "signals".to_string(),
        count: false,
        target_shape: None,
        where_clause: None,
        limit: Some(5),
        order: None,
    };
    assert_eq!(build_projection_order_clause(&proj), "");
}

#[test]
fn test_build_projection_order_clause_by_id() {
    let proj = ProjectionInput {
        from: "signals".to_string(),
        count: false,
        target_shape: None,
        where_clause: None,
        limit: None,
        order: Some(vec![("id".to_string(), OrderDirection::DESC)]),
    };
    let clause = build_projection_order_clause(&proj);
    assert!(clause.contains("ORDER BY DESC(?t)"), "got: {clause}");
}

// -----------------------------------------------------------------------
// Integration tests: resolve_projections()
//
// These tests verify that resolve_projections() correctly issues grouped
// SPARQL queries against a real SparqlStore and attaches the results to
// the parent instance objects.
// -----------------------------------------------------------------------

/// Helper to build a minimal ModelShape with one forward collection property.
fn make_shape_with_relation(class: &str, rel_name: &str, predicate: &str) -> ModelShape {
    ModelShape {
        target_class: class.to_string(),
        shape_uri: format!("{class}Shape"),
        properties: vec![ShapeProperty {
            name: rel_name.to_string(),
            predicate: predicate.to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: Some("forward".to_string()),
            is_scalar_relation: false,
            getter: None,
            where_filter: None,
            where_predicates: None,
        }],
        include_relations: vec![],
    }
}

#[test]
fn test_resolve_projections_count() {
    // Set up a store with two parent nodes, each linked to different numbers
    // of child targets via the "test://has_item" predicate.
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let parent_b = "test://parent/b";
    let item_1 = "test://item/1";
    let item_2 = "test://item/2";
    let item_3 = "test://item/3";

    store
        .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
        .unwrap();
    store
        .add_link(&make_link(parent_a, "test://has_item", item_2, "1001"))
        .unwrap();
    store
        .add_link(&make_link(parent_b, "test://has_item", item_3, "1002"))
        .unwrap();

    let shape = make_shape_with_relation("Parent", "items", "test://has_item");

    let mut instances = vec![json!({ "id": parent_a }), json!({ "id": parent_b })];

    let mut projections = HashMap::new();
    projections.insert(
        "$itemCount".to_string(),
        ProjectionInput {
            from: "items".to_string(),
            count: true,
            target_shape: None,
            where_clause: None,
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let count_a = instances[0]["$itemCount"].as_u64().unwrap_or(999);
    let count_b = instances[1]["$itemCount"].as_u64().unwrap_or(999);
    assert_eq!(count_a, 2, "parent_a should have 2 items, got {count_a}");
    assert_eq!(count_b, 1, "parent_b should have 1 item, got {count_b}");
}

#[test]
fn test_resolve_projections_list() {
    // parent_a has two children; verify list projection returns them as an array.
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let item_1 = "test://item/1";
    let item_2 = "test://item/2";

    store
        .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
        .unwrap();
    store
        .add_link(&make_link(parent_a, "test://has_item", item_2, "1001"))
        .unwrap();

    let shape = make_shape_with_relation("Parent", "items", "test://has_item");

    let mut instances = vec![json!({ "id": parent_a })];

    let mut projections = HashMap::new();
    projections.insert(
        "$items".to_string(),
        ProjectionInput {
            from: "items".to_string(),
            count: false,
            target_shape: None,
            where_clause: None,
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let items = instances[0]["$items"]
        .as_array()
        .expect("$items should be an array");
    assert_eq!(items.len(), 2, "expected 2 items, got {}", items.len());
    let item_strs: Vec<&str> = items.iter().filter_map(|v| v.as_str()).collect();
    assert!(item_strs.contains(&item_1), "missing {item_1}");
    assert!(item_strs.contains(&item_2), "missing {item_2}");
}

#[test]
fn test_resolve_projections_scalar() {
    // limit: Some(1) should unwrap to a single string, not an array.
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let item_1 = "test://item/1";

    store
        .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
        .unwrap();

    let shape = make_shape_with_relation("Parent", "items", "test://has_item");

    let mut instances = vec![json!({ "id": parent_a })];

    let mut projections = HashMap::new();
    projections.insert(
        "$firstItem".to_string(),
        ProjectionInput {
            from: "items".to_string(),
            count: false,
            target_shape: None,
            where_clause: None,
            limit: Some(1),
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let val = &instances[0]["$firstItem"];
    assert_eq!(
        val.as_str(),
        Some(item_1),
        "limit:1 should return a scalar string, got: {val}"
    );
}

#[test]
fn test_resolve_projections_count_zero_when_no_links() {
    // A parent with no linked children should get count 0, not be absent.
    let store = SparqlStore::new(None).unwrap();
    let parent_a = "test://parent/a";

    let shape = make_shape_with_relation("Parent", "items", "test://has_item");
    let mut instances = vec![json!({ "id": parent_a })];

    let mut projections = HashMap::new();
    projections.insert(
        "$itemCount".to_string(),
        ProjectionInput {
            from: "items".to_string(),
            count: true,
            target_shape: None,
            where_clause: None,
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let count = instances[0]["$itemCount"].as_u64().unwrap_or(999);
    assert_eq!(
        count, 0,
        "count should be 0 when no links exist, got {count}"
    );
}

#[test]
fn test_resolve_projections_where_filter_by_plain_iri() {
    // Flux reactions are stored as plain expression IRIs (e.g. emoji://1f44d),
    // not as literal:json: blobs.  The STR() FILTER correctly narrows to
    // only the matching reaction type.
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let like_iri = "emoji://1f44d";
    let dislike_iri = "emoji://1f44e";

    store
        .add_link(&make_link(
            parent_a,
            "test://has_reaction",
            like_iri,
            "1000",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            parent_a,
            "test://has_reaction",
            dislike_iri,
            "1001",
        ))
        .unwrap();
    // Note: COUNT(DISTINCT ?t) is used, so only distinct target IRIs are counted.

    let shape = make_shape_with_relation("Parent", "reactions", "test://has_reaction");

    // Filter by the plain IRI of the reaction target.
    let mut wc = BTreeMap::new();
    wc.insert(
        "id".to_string(),
        WhereCondition::String(like_iri.to_string()),
    );

    let mut instances = vec![json!({ "id": parent_a })];

    let mut projections = HashMap::new();
    projections.insert(
        "$likeCount".to_string(),
        ProjectionInput {
            from: "reactions".to_string(),
            count: true,
            target_shape: None,
            where_clause: Some(wc),
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let count = instances[0]["$likeCount"].as_u64().unwrap_or(999);
    assert_eq!(
        count, 1,
        "should count only the 'like' reaction, got {count}"
    );
}

#[test]
fn test_resolve_projections_where_filter_by_author() {
    // Mirrors the WE $myLikeSignal pattern:
    //   where: { author: { $store: 'adamStore.me.did' } }
    // This was previously silently ignored because the projection SPARQL
    // did not join the reifier. Now a ?_prj_reif join + FILTER is emitted.
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let signal_1 = "test://signal/1";
    let signal_2 = "test://signal/2";
    let signal_3 = "test://signal/3";

    let alice = "did:key:alice";
    let bob = "did:key:bob";

    // Two signals from alice, one from bob.
    let mut link1 = make_link(parent_a, "test://has_signal", signal_1, "1000");
    link1.author = alice.to_string();
    let mut link2 = make_link(parent_a, "test://has_signal", signal_2, "1001");
    link2.author = alice.to_string();
    let mut link3 = make_link(parent_a, "test://has_signal", signal_3, "1002");
    link3.author = bob.to_string();

    store.add_link(&link1).unwrap();
    store.add_link(&link2).unwrap();
    store.add_link(&link3).unwrap();

    let shape = make_shape_with_relation("Parent", "signals", "test://has_signal");

    let mut wc = BTreeMap::new();
    wc.insert(
        "author".to_string(),
        WhereCondition::String(alice.to_string()),
    );

    let mut instances = vec![json!({ "id": parent_a })];
    let mut projections = HashMap::new();
    projections.insert(
        "$mySignalCount".to_string(),
        ProjectionInput {
            from: "signals".to_string(),
            count: true,
            target_shape: None,
            where_clause: Some(wc),
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let count = instances[0]["$mySignalCount"].as_u64().unwrap_or(999);
    assert_eq!(count, 2, "should count only alice's 2 signals, got {count}");
}

#[test]
fn test_deep_query_flag_controls_property_getters() {
    // Create a shape with both a property getter and a relation getter
    let shape_json = r#"{
        "className": "TestModel",
        "properties": {
            "computedProp": {
                "predicate": "test://computed",
                "getter": "ASK WHERE { <Base> <test://is_active> ?x }"
            }
        },
        "relations": {
            "children": {
                "predicate": "test://has_child",
                "kind": "hasMany",
                "getter": "SELECT ?target WHERE { <Base> <test://has_child> ?target }"
            }
        }
    }"#;

    let shape = parse_shape_from_json(shape_json, "TestModel").unwrap();

    // With deep_query=false, only relation getters (is_collection/is_scalar_relation) should be collected
    let getter_props_shallow: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.getter.is_some() && (false || p.is_collection || p.is_scalar_relation))
        .collect();
    assert_eq!(
        getter_props_shallow.len(),
        1,
        "shallow: only relation getter"
    );
    assert_eq!(getter_props_shallow[0].name, "children");

    // With deep_query=true, all getters should be collected
    let getter_props_deep: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.getter.is_some() && (true || p.is_collection || p.is_scalar_relation))
        .collect();
    assert_eq!(
        getter_props_deep.len(),
        2,
        "deep: both property and relation getters"
    );
}

#[test]
fn test_evaluate_getters_batch_returns_results() {
    let store = SparqlStore::new(None).unwrap();

    // Insert a test link
    store
        .add_link(&make_link(
            "test://inst-1",
            "test://is_active",
            "literal:boolean:true",
            "2024-01-01T00:00:00Z",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "TestModel",
        "properties": {
            "isActive": {
                "predicate": "test://is_active",
                "getter": "ASK WHERE { <Base> <test://is_active> ?x }"
            }
        },
        "relations": {}
    }"#;

    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &["test://inst-1".to_string()],
        None,
        Some(shape_json),
    )
    .unwrap();

    assert!(result.is_object(), "result should be an object");
    let inst_result = &result["test://inst-1"];
    assert!(inst_result.is_object(), "should have results for inst-1");
    assert_eq!(inst_result["isActive"], Value::Bool(true));
}

#[test]
fn test_evaluate_getters_batch_empty_ids() {
    let store = SparqlStore::new(None).unwrap();
    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &[],
        None,
        Some(r#"{"className":"TestModel","properties":{},"relations":{}}"#),
    )
    .unwrap();
    assert!(result.as_object().unwrap().is_empty());
}

#[test]
fn test_evaluate_getters_batch_filters_by_property_names() {
    let store = SparqlStore::new(None).unwrap();

    let shape_json = r#"{
        "className": "TestModel",
        "properties": {
            "propA": {
                "predicate": "test://a",
                "getter": "ASK WHERE { <Base> <test://a> ?x }"
            },
            "propB": {
                "predicate": "test://b",
                "getter": "ASK WHERE { <Base> <test://b> ?x }"
            }
        },
        "relations": {}
    }"#;

    // Only request propA — propB should not appear in results
    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &["test://inst-1".to_string()],
        Some(&["propA".to_string()]),
        Some(shape_json),
    )
    .unwrap();

    assert!(result.is_object());
}

// ── VALUES batching tests ────────────────────────────────────────────

#[test]
fn test_evaluate_getters_where_compiled_literal_filter() {
    // Mimics the failing CI test: a relation getter with a where clause
    // that filters by a literal:string:X value.
    // Setup: board -> 3 tasks (2 active, 1 done)
    // The getter includes conformance checks (flag, required title, required status)
    // plus the where clause for status = "active".
    let store = SparqlStore::new(None).unwrap();
    let ts = "2024-01-01T00:00:00Z";

    let board = "literal:string:board1";
    let task1 = "literal:string:task-active-1";
    let task2 = "literal:string:task-active-2";
    let task3 = "literal:string:task-done";

    // Board -> Task links
    store
        .add_link(&make_link(board, "board://has_task", task1, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "board://has_task", task2, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "board://has_task", task3, ts))
        .unwrap();

    // Task type flags
    store
        .add_link(&make_link(task1, "task://type", "task://task", ts))
        .unwrap();
    store
        .add_link(&make_link(task2, "task://type", "task://task", ts))
        .unwrap();
    store
        .add_link(&make_link(task3, "task://type", "task://task", ts))
        .unwrap();

    // Task titles
    store
        .add_link(&make_link(
            task1,
            "task://title",
            "literal:string:Active%201",
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task2,
            "task://title",
            "literal:string:Active%202",
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task3,
            "task://title",
            "literal:string:Done%20Task",
            ts,
        ))
        .unwrap();

    // Task statuses
    store
        .add_link(&make_link(
            task1,
            "task://status",
            "literal:string:active",
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task2,
            "task://status",
            "literal:string:active",
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task3,
            "task://status",
            "literal:string:done",
            ts,
        ))
        .unwrap();

    // Conformance-only getter (no where clause in SPARQL).
    // Where filtering is done post-evaluation in Rust via where_filter.
    let getter = "SELECT ?target WHERE { <Base> <board://has_task> ?target . \
        ?target <task://type> <task://task> . \
        ?target <task://title> ?_v0 . \
        ?target <task://status> ?_v1 . }";

    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "task://status".to_string());

    let shape = ModelShape {
        target_class: "TaskBoard".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "activeTasks".to_string(),
            predicate: "board://has_task".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![serde_json::json!({"id": board})];
    let eval_result = evaluate_getters(&store, &mut instances, &shape, None, true);
    assert!(
        eval_result.is_ok(),
        "evaluate_getters should succeed: {:?}",
        eval_result.err()
    );

    let active = instances[0]
        .get("activeTasks")
        .expect("activeTasks should be set");
    let active_arr = active.as_array().expect("activeTasks should be array");
    assert_eq!(
        active_arr.len(),
        2,
        "Should have 2 active tasks via getter, got: {:?}",
        active_arr
    );
}

#[test]
fn test_strip_trailing_limit() {
    assert_eq!(
        strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . } LIMIT 1"),
        "SELECT ?t WHERE { ?s <p> ?t . }"
    );
    assert_eq!(
        strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . }"),
        "SELECT ?t WHERE { ?s <p> ?t . }"
    );
    assert_eq!(
        strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . } LIMIT 100  "),
        "SELECT ?t WHERE { ?s <p> ?t . }"
    );
}

#[test]
fn test_convert_ask_to_batched_select() {
    let result = convert_ask_to_batched_select(
        r#"ASK WHERE { ?source <test://active> "true" . }"#,
        "<test://a> <test://b>",
    );
    assert!(
        result.contains("SELECT ?source"),
        "should be SELECT: {result}"
    );
    assert!(
        result.contains("VALUES ?source { <test://a> <test://b> }"),
        "should have VALUES: {result}"
    );
    assert!(
        result.contains(r#"<test://active> "true""#),
        "should keep body: {result}"
    );
}

#[test]
fn test_convert_ask_with_base_to_batched_select() {
    let result =
        convert_ask_to_batched_select("ASK WHERE { <Base> <test://active> ?x }", "<test://a>");
    assert!(
        result.contains("?source <test://active>"),
        "should replace <Base> with ?source: {result}"
    );
    assert!(
        result.contains("VALUES ?source"),
        "should have VALUES: {result}"
    );
}

#[test]
fn test_inject_values_into_select() {
    let result = inject_values_into_select(
        "SELECT ?target WHERE { ?source <test://reply> ?target . } LIMIT 1",
        "<test://a> <test://b>",
    );
    assert!(
        result.contains("?source"),
        "should have ?source in SELECT: {result}"
    );
    assert!(
        result.contains("VALUES ?source { <test://a> <test://b> }"),
        "should have VALUES: {result}"
    );
    assert!(
        !result.to_uppercase().contains("LIMIT"),
        "should strip LIMIT: {result}"
    );
}

#[test]
fn test_inject_values_adds_source_to_projection() {
    let result = inject_values_into_select(
        "SELECT ?target WHERE { ?source <test://p> ?target . }",
        "<test://a>",
    );
    // ?source should appear in the SELECT projection
    let upper = result.to_uppercase();
    let select_end = upper.find("SELECT").unwrap() + 6;
    let where_pos = upper.find("WHERE").unwrap();
    let projection = &result[select_end..where_pos];
    assert!(
        projection.contains("?source"),
        "?source should be in projection: {result}"
    );
}

#[test]
fn test_batched_ask_getter_multiple_instances() {
    let store = SparqlStore::new(None).unwrap();

    // inst-1 is active, inst-2 is not
    store
        .add_link(&make_link(
            "test://inst-1",
            "test://is_active",
            "literal:boolean:true",
            "1000",
        ))
        .unwrap();
    // inst-2 has no is_active link

    let shape_json = r#"{
        "className": "TestModel",
        "properties": {
            "isActive": {
                "predicate": "test://is_active",
                "getter": "ASK WHERE { ?source <test://is_active> ?x }"
            }
        },
        "relations": {}
    }"#;

    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &["test://inst-1".to_string(), "test://inst-2".to_string()],
        None,
        Some(shape_json),
    )
    .unwrap();

    assert_eq!(result["test://inst-1"]["isActive"], Value::Bool(true));
    // inst-2 should be false (no matching link)
    assert!(
        result.get("test://inst-2").is_none()
            || result["test://inst-2"].get("isActive").is_none()
            || result["test://inst-2"]["isActive"] == Value::Bool(false),
        "inst-2 should have isActive=false or be absent"
    );
}

#[test]
fn test_batched_select_getter_multiple_instances() {
    let store = SparqlStore::new(None).unwrap();

    // inst-1 has a reply, inst-2 does not
    store
        .add_link(&make_link(
            "test://inst-1",
            "test://has_reply",
            "test://reply-99",
            "1000",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "TestModel",
        "properties": {
            "replyingTo": {
                "predicate": "test://has_reply",
                "getter": "SELECT ?target WHERE { ?source <test://has_reply> ?target . } LIMIT 1"
            }
        },
        "relations": {}
    }"#;

    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &["test://inst-1".to_string(), "test://inst-2".to_string()],
        None,
        Some(shape_json),
    )
    .unwrap();

    assert_eq!(
        result["test://inst-1"]["replyingTo"].as_str().unwrap(),
        "test://reply-99"
    );
    // inst-2 has no reply
    assert!(
        result.get("test://inst-2").is_none()
            || result["test://inst-2"].get("replyingTo").is_none(),
        "inst-2 should have no replyingTo"
    );
}

#[test]
fn test_batched_collection_getter() {
    let store = SparqlStore::new(None).unwrap();

    // inst-1 has two children
    store
        .add_link(&make_link(
            "test://inst-1",
            "test://has_child",
            "test://child-a",
            "1000",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            "test://inst-1",
            "test://has_child",
            "test://child-b",
            "1001",
        ))
        .unwrap();
    // inst-2 has one child
    store
        .add_link(&make_link(
            "test://inst-2",
            "test://has_child",
            "test://child-c",
            "1002",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "TestModel",
        "properties": {},
        "relations": {
            "children": {
                "predicate": "test://has_child",
                "kind": "hasMany",
                "getter": "SELECT ?target WHERE { ?source <test://has_child> ?target }"
            }
        }
    }"#;

    let result = evaluate_getters_batch(
        &store,
        "TestModel",
        &["test://inst-1".to_string(), "test://inst-2".to_string()],
        None,
        Some(shape_json),
    )
    .unwrap();

    let children_1 = result["test://inst-1"]["children"].as_array().unwrap();
    assert_eq!(children_1.len(), 2, "inst-1 should have 2 children");

    let children_2 = result["test://inst-2"]["children"].as_array().unwrap();
    assert_eq!(children_2.len(), 1, "inst-2 should have 1 child");
    assert_eq!(children_2[0].as_str().unwrap(), "test://child-c");
}

// ── Pipeline ordering: getters run post-pagination ───────────────────

#[test]
fn test_deep_query_defaults_to_true() {
    // Verify the default: when deep_query is None, property getters should run
    let store = SparqlStore::new(None).unwrap();

    let base = "test://msg-1";
    store
        .add_link(&make_link(
            base,
            "flux://entry_type",
            "flux://message",
            "1000",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            base,
            "flux://has_reply",
            "test://reply-1",
            "1001",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Message",
        "properties": {
            "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" }
        },
        "relations": {
            "replyingTo": {
                "predicate": "flux://has_reply",
                "kind": "hasOne",
                "getter": "SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1"
            }
        }
    }"#;

    let query_input = ModelQueryInput {
        deep_query: None, // not set — should default to true
        ..Default::default()
    };

    let result = execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
    assert!(!result.instances.is_empty(), "should find instance");

    let inst = &result.instances[0];
    // replyingTo is a relation getter (always runs) — should be populated
    let reply = inst.get("replyingTo").and_then(|v| v.as_str());
    assert_eq!(
        reply,
        Some("test://reply-1"),
        "replyingTo should be populated by default"
    );
}

#[test]
fn test_deep_query_false_skips_property_getters() {
    let store = SparqlStore::new(None).unwrap();

    let base = "test://msg-1";
    store
        .add_link(&make_link(
            base,
            "flux://entry_type",
            "flux://message",
            "1000",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            base,
            "flux://is_popular",
            "literal:boolean:true",
            "1001",
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Message",
        "properties": {
            "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" },
            "isPopular": {
                "predicate": "flux://is_popular",
                "getter": "ASK WHERE { ?source <flux://is_popular> ?x }"
            }
        },
        "relations": {}
    }"#;

    let query_input = ModelQueryInput {
        deep_query: Some(false),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
    assert!(!result.instances.is_empty());

    let inst = &result.instances[0];
    // isPopular is a property getter — should NOT be evaluated when deepQuery=false
    // It may still show the raw hydrated value from the link, but the getter itself
    // (ASK → bool) should not have run.
    // The hydrated value from the link is "true" (string), not true (bool).
    // If the getter ran, it would be Value::Bool(true).
    let is_popular = inst.get("isPopular");
    assert!(
        is_popular.is_none() || !is_popular.unwrap().is_boolean(),
        "property getter should not run when deepQuery=false; got: {:?}",
        is_popular
    );
}

#[test]
fn test_getters_run_after_pagination() {
    // Verify that getters run on the paginated set, not the full result set.
    // We do this by creating 5 instances but querying with limit=2.
    // If getters ran before pagination, all 5 would be evaluated.
    // After our change, only 2 should be evaluated.
    // We verify by checking that the 2 returned instances have getter values.
    let store = SparqlStore::new(None).unwrap();

    for i in 0..5 {
        let base = format!("test://msg-{i}");
        store
            .add_link(&make_link(
                &base,
                "flux://entry_type",
                "flux://message",
                &format!("{}", 1000 + i),
            ))
            .unwrap();
        store
            .add_link(&make_link(
                &base,
                "flux://has_reply",
                &format!("test://reply-{i}"),
                &format!("{}", 2000 + i),
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Message",
        "properties": {
            "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" }
        },
        "relations": {
            "replyingTo": {
                "predicate": "flux://has_reply",
                "kind": "hasOne",
                "getter": "SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1"
            }
        }
    }"#;

    let query_input = ModelQueryInput {
        limit: Some(2),
        deep_query: Some(true),
        order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
    assert_eq!(result.instances.len(), 2, "should return 2 instances");
    assert_eq!(result.total_count, 5, "total count should be 5");

    // Both returned instances should have replyingTo populated
    for inst in &result.instances {
        let reply = inst.get("replyingTo").and_then(|v| v.as_str());
        assert!(
            reply.is_some(),
            "replyingTo should be populated: {:?}",
            inst
        );
    }
}

// ===================================================================
// Where-clause filtering integration tests
//
// These test property where-clause filtering with plain literal values.
// Property values are stored as `literal:string:X`, `literal:number:X`,
// etc. and can be matched by SPARQL FILTER or Rust post-hydration.
// ===================================================================

/// Helper: create a plain literal IRI for a string value.
fn signed_literal(value: &str) -> String {
    format!("literal:string:{}", literal_percent_encode(value))
}

/// Helper: create a plain literal IRI for a numeric value.
fn signed_literal_number(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("literal:number:{}", value as i64)
    } else {
        format!("literal:number:{value}")
    }
}

#[test]
fn test_where_filter_signed_expression_string() {
    // Reproduces the exact CI failure: where clause on a property stored
    // Where clause on a property stored as literal:string:<value>.
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let board = "test://board1";
    let task1 = "test://task-active-1";
    let task2 = "test://task-active-2";
    let task3 = "test://task-done";

    // Board -> task links
    store
        .add_link(&make_link(board, "board://has_task", task1, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "board://has_task", task2, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "board://has_task", task3, ts))
        .unwrap();

    // Task flags + required properties
    for task in &[task1, task2, task3] {
        store
            .add_link(&make_link(task, "task://type", "task://task", ts))
            .unwrap();
        store
            .add_link(&make_link(
                task,
                "task://title",
                &signed_literal("Title"),
                ts,
            ))
            .unwrap();
    }

    // Statuses as signed expressions (the exact format that caused CI failure)
    store
        .add_link(&make_link(
            task1,
            "task://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task2,
            "task://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task3,
            "task://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();

    // Use post-getter where filtering (the fix)
    let getter = "SELECT ?target WHERE { <Base> <board://has_task> ?target . \
        ?target <task://type> <task://task> . \
        ?target <task://title> ?_v0 . \
        ?target <task://status> ?_v1 . }";

    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "task://status".to_string());

    let shape = ModelShape {
        target_class: "Board".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "activeTasks".to_string(),
            predicate: "board://has_task".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": board})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let active = instances[0]["activeTasks"].as_array().unwrap();
    assert_eq!(
        active.len(),
        2,
        "Should have 2 active tasks, got {:?}",
        active
    );

    // Verify correct tasks were returned
    let ids: Vec<&str> = active.iter().filter_map(|v| v.as_str()).collect();
    assert!(ids.contains(&task1));
    assert!(ids.contains(&task2));
    assert!(!ids.contains(&task3));
}

#[test]
fn test_where_filter_signed_expression_no_matches() {
    // All targets filtered out -> empty array
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let parent = "test://parent";
    let child = "test://child";

    store
        .add_link(&make_link(parent, "ns://has_child", child, ts))
        .unwrap();
    store
        .add_link(&make_link(child, "ns://type", "ns://thing", ts))
        .unwrap();
    store
        .add_link(&make_link(
            child,
            "ns://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();

    let getter = "SELECT ?target WHERE { <Base> <ns://has_child> ?target . }";

    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "ns://status".to_string());

    let shape = ModelShape {
        target_class: "Parent".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "activeChildren".to_string(),
            predicate: "ns://has_child".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": parent})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let result = instances[0]["activeChildren"].as_array().unwrap();
    assert_eq!(result.len(), 0, "Should be empty when no matches");
}

#[test]
fn test_where_filter_multiple_conditions() {
    // Multiple where conditions: status=active AND priority > 3
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let board = "test://board";
    let task_hi = "test://task-hi";
    let task_lo = "test://task-lo";
    let task_done = "test://task-done";

    store
        .add_link(&make_link(board, "ns://has", task_hi, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "ns://has", task_lo, ts))
        .unwrap();
    store
        .add_link(&make_link(board, "ns://has", task_done, ts))
        .unwrap();

    // task_hi: active, priority 5
    store
        .add_link(&make_link(
            task_hi,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task_hi,
            "ns://priority",
            &signed_literal_number(5.0),
            ts,
        ))
        .unwrap();

    // task_lo: active, priority 1
    store
        .add_link(&make_link(
            task_lo,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task_lo,
            "ns://priority",
            &signed_literal_number(1.0),
            ts,
        ))
        .unwrap();

    // task_done: done, priority 5
    store
        .add_link(&make_link(
            task_done,
            "ns://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task_done,
            "ns://priority",
            &signed_literal_number(5.0),
            ts,
        ))
        .unwrap();

    let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";

    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    where_filter.insert(
        "priority".to_string(),
        WhereCondition::Ops(WhereOps {
            gt: Some(3.0),
            ..Default::default()
        }),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "ns://status".to_string());
    where_predicates.insert("priority".to_string(), "ns://priority".to_string());

    let shape = ModelShape {
        target_class: "Board".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "highPriActive".to_string(),
            predicate: "ns://has".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": board})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let result = instances[0]["highPriActive"].as_array().unwrap();
    assert_eq!(result.len(), 1, "Only task_hi should match: {:?}", result);
    assert_eq!(result[0].as_str().unwrap(), task_hi);
}

#[test]
fn test_where_filter_missing_property_on_target() {
    // Target lacks the property being filtered on -> should not match
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let parent = "test://parent";
    let child_with = "test://child-with";
    let child_without = "test://child-without";

    store
        .add_link(&make_link(parent, "ns://has", child_with, ts))
        .unwrap();
    store
        .add_link(&make_link(parent, "ns://has", child_without, ts))
        .unwrap();

    // Only child_with has the status property
    store
        .add_link(&make_link(
            child_with,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    // child_without has no status link at all

    let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "ns://status".to_string());

    let shape = ModelShape {
        target_class: "Parent".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "active".to_string(),
            predicate: "ns://has".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": parent})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let result = instances[0]["active"].as_array().unwrap();
    assert_eq!(result.len(), 1, "Only child_with should match");
    assert_eq!(result[0].as_str().unwrap(), child_with);
}

#[test]
fn test_where_filter_plain_literal_string() {
    // Where clause on literal:string: values (not signed expressions)
    // This should also work correctly
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let parent = "test://parent";
    let child1 = "test://child1";
    let child2 = "test://child2";

    store
        .add_link(&make_link(parent, "ns://has", child1, ts))
        .unwrap();
    store
        .add_link(&make_link(parent, "ns://has", child2, ts))
        .unwrap();

    // Plain literal:string values (no signed expression envelope)
    store
        .add_link(&make_link(child1, "ns://color", "literal:string:red", ts))
        .unwrap();
    store
        .add_link(&make_link(child2, "ns://color", "literal:string:blue", ts))
        .unwrap();

    let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "color".to_string(),
        WhereCondition::String("red".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("color".to_string(), "ns://color".to_string());

    let shape = ModelShape {
        target_class: "Parent".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "redChildren".to_string(),
            predicate: "ns://has".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": parent})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let result = instances[0]["redChildren"].as_array().unwrap();
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].as_str().unwrap(), child1);
}

#[test]
fn test_where_filter_on_multiple_instances() {
    // Where filter across multiple parent instances
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let board1 = "test://board1";
    let board2 = "test://board2";
    let task_a = "test://task-a";
    let task_b = "test://task-b";
    let task_c = "test://task-c";

    // board1 -> task_a (active), task_b (done)
    store
        .add_link(&make_link(board1, "ns://has", task_a, ts))
        .unwrap();
    store
        .add_link(&make_link(board1, "ns://has", task_b, ts))
        .unwrap();
    // board2 -> task_c (active)
    store
        .add_link(&make_link(board2, "ns://has", task_c, ts))
        .unwrap();

    store
        .add_link(&make_link(
            task_a,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task_b,
            "ns://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            task_c,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();

    let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
    let mut where_filter = BTreeMap::new();
    where_filter.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let mut where_predicates = HashMap::new();
    where_predicates.insert("status".to_string(), "ns://status".to_string());

    let shape = ModelShape {
        target_class: "Board".to_string(),
        shape_uri: String::new(),
        properties: vec![ShapeProperty {
            name: "activeTasks".to_string(),
            predicate: "ns://has".to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: Some(getter.to_string()),
            where_filter: Some(where_filter),
            where_predicates: Some(where_predicates),
        }],
        include_relations: vec![],
    };

    let mut instances = vec![json!({"id": board1}), json!({"id": board2})];
    evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

    let active1 = instances[0]["activeTasks"].as_array().unwrap();
    assert_eq!(active1.len(), 1, "board1 should have 1 active task");
    assert_eq!(active1[0].as_str().unwrap(), task_a);

    let active2 = instances[1]["activeTasks"].as_array().unwrap();
    assert_eq!(active2.len(), 1, "board2 should have 1 active task");
    assert_eq!(active2[0].as_str().unwrap(), task_c);
}

#[test]
fn test_full_model_query_signed_expression_where() {
    // End-to-end: findAll with where clause on signed expression values
    // This is what the integration test does via the full pipeline
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let item1 = "test://item1";
    let item2 = "test://item2";
    let item3 = "test://item3";

    // All items have the type flag
    for item in &[item1, item2, item3] {
        store
            .add_link(&make_link(item, "ns://type", "ns://item", ts))
            .unwrap();
    }

    // Properties as signed expressions
    store
        .add_link(&make_link(item1, "ns://name", &signed_literal("Alpha"), ts))
        .unwrap();
    store
        .add_link(&make_link(
            item1,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();

    store
        .add_link(&make_link(item2, "ns://name", &signed_literal("Beta"), ts))
        .unwrap();
    store
        .add_link(&make_link(
            item2,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();

    store
        .add_link(&make_link(item3, "ns://name", &signed_literal("Gamma"), ts))
        .unwrap();
    store
        .add_link(&make_link(
            item3,
            "ns://status",
            &signed_literal("archived"),
            ts,
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Item",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
            "name": { "predicate": "ns://name", "required": true, "resolveLanguage": "literal" },
            "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // Query WITH where clause on status
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );

    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
    assert_eq!(
        result.instances.len(),
        2,
        "Should find 2 active items, got: {:?}",
        result.instances
    );

    // Verify names
    let names: Vec<&str> = result
        .instances
        .iter()
        .filter_map(|i| i["name"].as_str())
        .collect();
    assert!(names.contains(&"Alpha"));
    assert!(names.contains(&"Beta"));
    assert!(!names.contains(&"Gamma"));
}

#[test]
fn test_full_model_query_signed_expression_numeric_where() {
    // findAll with numeric where clause on signed expression values
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let item1 = "test://item1";
    let item2 = "test://item2";

    for item in &[item1, item2] {
        store
            .add_link(&make_link(item, "ns://type", "ns://item", ts))
            .unwrap();
    }

    store
        .add_link(&make_link(
            item1,
            "ns://score",
            &signed_literal_number(85.0),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            item2,
            "ns://score",
            &signed_literal_number(45.0),
            ts,
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Item",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
            "score": { "predicate": "ns://score", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // Where: score > 50
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "score".to_string(),
        WhereCondition::Ops(WhereOps {
            gt: Some(50.0),
            ..Default::default()
        }),
    );

    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
    assert_eq!(
        result.instances.len(),
        1,
        "Only item1 with score 85 should match"
    );
    assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
}

#[test]
fn test_full_model_query_signed_expression_boolean_where() {
    // findAll with boolean where clause on plain literal boolean values
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let item1 = "test://item1";
    let item2 = "test://item2";

    for item in &[item1, item2] {
        store
            .add_link(&make_link(item, "ns://type", "ns://thing", ts))
            .unwrap();
    }

    let enc_true = "literal:boolean:true";
    let enc_false = "literal:boolean:false";

    store
        .add_link(&make_link(item1, "ns://visible", enc_true, ts))
        .unwrap();
    store
        .add_link(&make_link(item2, "ns://visible", enc_false, ts))
        .unwrap();

    let shape_json = r#"{
        "className": "Thing",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://thing" },
            "visible": { "predicate": "ns://visible", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut where_clause = BTreeMap::new();
    where_clause.insert("visible".to_string(), WhereCondition::Bool(true));

    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Thing", &query, Some(shape_json)).unwrap();
    assert_eq!(result.instances.len(), 1);
    assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
}

#[test]
fn test_full_model_query_where_string_array_in() {
    // IN operator: where status IN ["active", "pending"]
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let item1 = "test://i1";
    let item2 = "test://i2";
    let item3 = "test://i3";

    for item in &[item1, item2, item3] {
        store
            .add_link(&make_link(item, "ns://type", "ns://item", ts))
            .unwrap();
    }

    store
        .add_link(&make_link(
            item1,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            item2,
            "ns://status",
            &signed_literal("pending"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            item3,
            "ns://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Item",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
            "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "status".to_string(),
        WhereCondition::StringArray(vec!["active".to_string(), "pending".to_string()]),
    );

    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
    assert_eq!(result.instances.len(), 2, "active and pending should match");
}

#[test]
fn test_full_model_query_where_ops_not() {
    // NOT operator: where status != "done"
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let item1 = "test://i1";
    let item2 = "test://i2";

    for item in &[item1, item2] {
        store
            .add_link(&make_link(item, "ns://type", "ns://item", ts))
            .unwrap();
    }

    store
        .add_link(&make_link(
            item1,
            "ns://status",
            &signed_literal("active"),
            ts,
        ))
        .unwrap();
    store
        .add_link(&make_link(
            item2,
            "ns://status",
            &signed_literal("done"),
            ts,
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Item",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
            "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "status".to_string(),
        WhereCondition::Ops(WhereOps {
            not: Some(Value::String("done".to_string())),
            ..Default::default()
        }),
    );

    let query = ModelQueryInput {
        where_clause: Some(where_clause),
        ..Default::default()
    };

    let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
    assert_eq!(result.instances.len(), 1);
    assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
}

// -----------------------------------------------------------------------
// build_instance_sparql: predicate projection tests
//
// Verifies that the VALUES ?predicate clause correctly excludes
// collection properties that have SPARQL getters (i.e. typed @HasMany
// relations resolved by evaluate_getters) while retaining scalar
// properties, flags, and raw-predicate collections without getters.
// -----------------------------------------------------------------------

/// Helper: build a minimal ShapeProperty for a scalar property.
fn scalar_prop(name: &str, predicate: &str, required: bool, flag: bool) -> ShapeProperty {
    ShapeProperty {
        name: name.to_string(),
        predicate: predicate.to_string(),
        is_collection: false,
        is_flag: flag,
        is_required: required,
        initial_value: if flag {
            Some("ns://flag_value".to_string())
        } else {
            None
        },
        resolve_language: None,
        datatype: None,
        direction: None,
        is_scalar_relation: false,
        getter: None,
        where_filter: None,
        where_predicates: None,
    }
}

/// Helper: build a ShapeProperty for a collection relation.
fn collection_prop(name: &str, predicate: &str, getter: Option<&str>) -> ShapeProperty {
    ShapeProperty {
        name: name.to_string(),
        predicate: predicate.to_string(),
        is_collection: true,
        is_flag: false,
        is_required: false,
        initial_value: None,
        resolve_language: None,
        datatype: None,
        direction: None,
        is_scalar_relation: false,
        getter: getter.map(|s| s.to_string()),
        where_filter: None,
        where_predicates: None,
    }
}

fn make_shape(props: Vec<ShapeProperty>) -> ModelShape {
    ModelShape {
        target_class: "TestModel".to_string(),
        shape_uri: String::new(),
        properties: props,
        include_relations: vec![],
    }
}

#[test]
fn test_build_instance_sparql_scalar_only_model_uses_values_clause() {
    // A model with only scalar properties (like ChannelSummary) should
    // produce a VALUES ?predicate clause listing only those predicates.
    let shape = make_shape(vec![
        scalar_prop("type", "flux://entry_type", true, true),
        scalar_prop("name", "flux://name", false, false),
        scalar_prop("description", "flux://description", false, false),
    ]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(
        sparql.contains("VALUES ?predicate"),
        "Should have VALUES clause, got:\n{}",
        sparql
    );
    assert!(sparql.contains("<flux://entry_type>"));
    assert!(sparql.contains("<flux://name>"));
    assert!(sparql.contains("<flux://description>"));
}

#[test]
fn test_build_instance_sparql_excludes_getter_backed_collections() {
    // A model like Channel with typed @HasMany relations that have
    // auto-generated getters.  The getter-backed collections (views,
    // messages) should be EXCLUDED from the VALUES clause.
    let shape = make_shape(vec![
        scalar_prop("type", "flux://entry_type", true, true),
        scalar_prop("name", "flux://name", false, false),
        // Typed @HasMany — has a getter (auto-generated conformance filter)
        collection_prop(
            "views",
            "ad4m://has_child",
            Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_app> . }"),
        ),
        // Another typed @HasMany with getter — same predicate
        collection_prop(
            "messages",
            "ad4m://has_child",
            Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_message> . }"),
        ),
    ]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(
        sparql.contains("VALUES ?predicate"),
        "Should have VALUES clause"
    );
    assert!(sparql.contains("<flux://entry_type>"));
    assert!(sparql.contains("<flux://name>"));
    // ad4m://has_child should NOT appear because both collections using
    // it have getters.
    assert!(
        !sparql.contains("<ad4m://has_child>"),
        "Should exclude getter-backed collection predicate, got:\n{}",
        sparql
    );
}

#[test]
fn test_build_instance_sparql_retains_raw_predicate_collections() {
    // A collection without a getter (raw predicate like participants)
    // should be INCLUDED in the VALUES clause because it's resolved
    // from the main query results, not by evaluate_getters.
    let shape = make_shape(vec![
        scalar_prop("type", "flux://entry_type", true, true),
        // Raw @HasMany — no target class, no getter
        collection_prop("participants", "flux://has_participant", None),
        // Typed @HasMany — has getter
        collection_prop(
            "messages",
            "ad4m://has_child",
            Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . }"),
        ),
    ]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(sparql.contains("VALUES ?predicate"));
    assert!(sparql.contains("<flux://entry_type>"));
    assert!(
        sparql.contains("<flux://has_participant>"),
        "Raw collection predicate should be included"
    );
    assert!(
        !sparql.contains("<ad4m://has_child>"),
        "Getter-backed collection predicate should be excluded"
    );
}

#[test]
fn test_build_instance_sparql_shared_predicate_mixed_getter() {
    // Edge case: two collections share the same predicate but only one
    // has a getter.  The predicate should be INCLUDED because the
    // getter-less collection needs it from the main query.
    let shape = make_shape(vec![
        scalar_prop("type", "flux://entry_type", true, true),
        // No getter — needs predicate in main query
        collection_prop("raw_children", "ad4m://has_child", None),
        // Has getter — doesn't need predicate in main query
        collection_prop(
            "typed_children",
            "ad4m://has_child",
            Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . }"),
        ),
    ]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(sparql.contains("VALUES ?predicate"));
    // ad4m://has_child should appear because raw_children needs it
    assert!(
        sparql.contains("<ad4m://has_child>"),
        "Predicate should be included when any collection without a getter uses it"
    );
}

#[test]
fn test_build_instance_sparql_empty_shape_falls_back_to_wildcard() {
    // A shape with no properties at all should fall back to the
    // unrestricted wildcard (no VALUES clause).
    let shape = make_shape(vec![]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(
        !sparql.contains("VALUES ?predicate"),
        "Empty shape should produce wildcard (no VALUES clause)"
    );
    // Should still have the basic pattern
    assert!(sparql.contains("?source ?predicate ?target"));
}

#[test]
fn test_build_instance_sparql_values_clause_is_deduplicated() {
    // If multiple scalar properties share the same predicate, the
    // VALUES clause should contain it only once.
    let shape = make_shape(vec![
        scalar_prop("type", "ns://shared_pred", true, true),
        scalar_prop("alias", "ns://shared_pred", false, false),
        scalar_prop("name", "ns://name", false, false),
    ]);
    let query = ModelQueryInput::default();
    let sparql = build_instance_sparql(&shape, &query, None).into_single();

    assert!(sparql.contains("VALUES ?predicate"));
    // Count occurrences of the shared predicate in the VALUES clause
    let values_line = sparql
        .lines()
        .find(|l| l.contains("VALUES ?predicate"))
        .unwrap();
    let count = values_line.matches("<ns://shared_pred>").count();
    assert_eq!(
        count, 1,
        "Shared predicate should appear exactly once in VALUES clause"
    );
}

#[test]
fn test_build_instance_sparql_integration_getter_excluded_from_results() {
    // Full integration test: a Channel-like model with scalar properties
    // and a getter-backed @HasMany relation.  The main query should NOT
    // return rows for the getter-backed relation's predicate, so adding
    // thousands of links with that predicate should not affect the result
    // count from the main query.
    let store = SparqlStore::new(None).unwrap();

    let channel_id = "test://channel1";

    // Add flag link
    store
        .add_link(&make_link(
            channel_id,
            "flux://entry_type",
            "flux://channel",
            "1700000000000",
        ))
        .unwrap();
    // Add name link
    store
        .add_link(&make_link(
            channel_id,
            "flux://name",
            "literal:string:general",
            "1700000000001",
        ))
        .unwrap();

    // Add 100 message children (simulating a large channel)
    for i in 0..100 {
        store
            .add_link(&make_link(
                channel_id,
                "ad4m://has_child",
                &format!("test://msg{i}"),
                &format!("17000000001{i:02}"),
            ))
            .unwrap();
    }

    // Shape: scalar properties + getter-backed collection
    let shape_json = r#"{
        "className": "Channel",
        "properties": {
            "type": {
                "predicate": "flux://entry_type",
                "required": true,
                "flag": true,
                "initial": "flux://channel"
            },
            "name": {
                "predicate": "flux://name",
                "required": false
            }
        },
        "relations": {
            "messages": {
                "predicate": "ad4m://has_child",
                "getter": "SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_message> . }"
            }
        }
    }"#;

    let query = ModelQueryInput::default();
    let result = execute_model_query(&store, "Channel", &query, Some(shape_json)).unwrap();

    assert_eq!(result.instances.len(), 1, "Should find exactly 1 channel");
    assert_eq!(
        result.instances[0]["name"],
        json!("general"),
        "Name should be hydrated"
    );
    // The 100 message children should NOT appear in the main hydration
    // because their predicate (ad4m://has_child) is excluded by the
    // VALUES clause.  The "messages" relation has a getter, so it would
    // be resolved by evaluate_getters (not tested here — that's a
    // separate code path).
    let messages = result.instances[0].get("messages");
    // messages should either be absent or empty (getter not run in this test)
    match messages {
        None => {} // expected — not hydrated from main query
        Some(Value::Array(arr)) => assert!(
            arr.is_empty(),
            "Messages should not be hydrated from main query"
        ),
        other => panic!("Unexpected messages value: {:?}", other),
    }
}

// -----------------------------------------------------------------------
// Ops-based SPARQL push tests
// -----------------------------------------------------------------------

#[test]
fn test_full_model_query_ops_gt_lt_sparql_push() {
    // Verify gt/lt numeric ops are pushed to SPARQL and return correct results
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let items: Vec<String> = (0..5).map(|i| format!("test://item-{i}")).collect();
    let scores = [10.0, 30.0, 50.0, 70.0, 90.0];

    for (item, &score) in items.iter().zip(&scores) {
        store
            .add_link(&make_link(item, "ns://type", "ns://scored", ts))
            .unwrap();
        store
            .add_link(&make_link(
                item,
                "ns://score",
                &signed_literal_number(score),
                ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Scored",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://scored" },
            "score": { "predicate": "ns://score", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // gt: 50 → items with 70, 90
    let mut wc = BTreeMap::new();
    wc.insert(
        "score".to_string(),
        WhereCondition::Ops(WhereOps {
            gt: Some(50.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2, "gt:50 should match 70 and 90");

    // lt: 50 → items with 10, 30
    let mut wc = BTreeMap::new();
    wc.insert(
        "score".to_string(),
        WhereCondition::Ops(WhereOps {
            lt: Some(50.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2, "lt:50 should match 10 and 30");

    // between: [30, 70] → items with 30, 50, 70
    let mut wc = BTreeMap::new();
    wc.insert(
        "score".to_string(),
        WhereCondition::Ops(WhereOps {
            between: Some((30.0, 70.0)),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(
        result.instances.len(),
        3,
        "between:[30,70] should match 30, 50, 70"
    );
}

#[test]
fn test_full_model_query_ops_gte_lte_sparql_push() {
    // Verify gte/lte numeric ops via SPARQL
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    for (i, score) in [10.0, 50.0, 90.0].iter().enumerate() {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://scored", ts))
            .unwrap();
        store
            .add_link(&make_link(
                &item,
                "ns://val",
                &signed_literal_number(*score),
                ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Scored",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://scored" },
            "val": { "predicate": "ns://val", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // gte: 50 → 50, 90
    let mut wc = BTreeMap::new();
    wc.insert(
        "val".to_string(),
        WhereCondition::Ops(WhereOps {
            gte: Some(50.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2, "gte:50 should match 50 and 90");

    // lte: 50 → 10, 50
    let mut wc = BTreeMap::new();
    wc.insert(
        "val".to_string(),
        WhereCondition::Ops(WhereOps {
            lte: Some(50.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2, "lte:50 should match 10 and 50");
}

#[test]
fn test_full_model_query_ops_not_string_sparql_push() {
    // NOT operator with string should be pushed to SPARQL
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    for (i, status) in ["active", "done", "active"].iter().enumerate() {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://task", ts))
            .unwrap();
        store
            .add_link(&make_link(
                &item,
                "ns://status",
                &signed_literal(status),
                ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Task",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://task" },
            "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut wc = BTreeMap::new();
    wc.insert(
        "status".to_string(),
        WhereCondition::Ops(WhereOps {
            not: Some(Value::String("done".to_string())),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Task",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2, "NOT done → 2 active tasks");
}

#[test]
fn test_full_model_query_ops_not_array_sparql_push() {
    // NOT IN (array) should be pushed to SPARQL
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    for (i, status) in ["alpha", "beta", "gamma", "delta"].iter().enumerate() {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://thing", ts))
            .unwrap();
        store
            .add_link(&make_link(&item, "ns://phase", &signed_literal(status), ts))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Thing",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://thing" },
            "phase": { "predicate": "ns://phase", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut wc = BTreeMap::new();
    wc.insert(
        "phase".to_string(),
        WhereCondition::Ops(WhereOps {
            not: Some(Value::Array(vec![
                Value::String("alpha".to_string()),
                Value::String("gamma".to_string()),
            ])),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Thing",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(
        result.instances.len(),
        2,
        "NOT IN [alpha, gamma] → beta, delta"
    );
}

#[test]
fn test_full_model_query_ops_with_pagination_pushed() {
    // Ops + pagination should both be pushed to SPARQL (no contains)
    let store = SparqlStore::new(None).unwrap();

    for i in 0..10 {
        let item = format!("test://item-{i}");
        let ts = format!("{}", 1700000000000i64 + i);
        store
            .add_link(&make_link(&item, "ns://type", "ns://scored", &ts))
            .unwrap();
        store
            .add_link(&make_link(
                &item,
                "ns://score",
                &signed_literal_number(i as f64 * 10.0),
                &ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Scored",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://scored" },
            "score": { "predicate": "ns://score", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // score >= 30 (items 3-9 = 7 items), paginate: offset 2, limit 3
    let mut wc = BTreeMap::new();
    wc.insert(
        "score".to_string(),
        WhereCondition::Ops(WhereOps {
            gte: Some(30.0),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            where_clause: Some(wc),
            limit: Some(3),
            offset: Some(2),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 3, "Should get 3 items in page");
    assert_eq!(result.total_count, 7, "Total matching items: score >= 30");
}

#[test]
fn test_full_model_query_ops_contains_sparql_push() {
    // `contains` in Ops should be pushed to SPARQL via CONTAINS+ENCODE_FOR_URI
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    for (i, name) in ["Alice Smith", "Bob Jones", "Alice Cooper"]
        .iter()
        .enumerate()
    {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://person", ts))
            .unwrap();
        store
            .add_link(&make_link(&item, "ns://name", &signed_literal(name), ts))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Person",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://person" },
            "name": { "predicate": "ns://name", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    let mut wc = BTreeMap::new();
    wc.insert(
        "name".to_string(),
        WhereCondition::Ops(WhereOps {
            contains: Some(Value::String("alice".to_string())),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Person",
        &ModelQueryInput {
            where_clause: Some(wc),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(
        result.instances.len(),
        2,
        "Case-insensitive contains 'alice' → Alice Smith, Alice Cooper"
    );
}

#[test]
fn test_full_model_query_ops_contains_with_pagination() {
    // `contains` + pagination should both be pushed to SPARQL
    let store = SparqlStore::new(None).unwrap();
    let ts_base = 1700000000000i64;

    let names = [
        "Alice Adams",
        "Bob Baker",
        "Alice Brown",
        "Charlie Clark",
        "Alice Chen",
        "Dave Davis",
    ];
    for (i, name) in names.iter().enumerate() {
        let item = format!("test://item-{i}");
        let ts = format!("{}", ts_base + i as i64);
        store
            .add_link(&make_link(&item, "ns://type", "ns://person", &ts))
            .unwrap();
        store
            .add_link(&make_link(&item, "ns://name", &signed_literal(name), &ts))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Person",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://person" },
            "name": { "predicate": "ns://name", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // contains "alice" → 3 matches (Alice Adams, Alice Brown, Alice Chen)
    // paginate: offset 1, limit 1
    let mut wc = BTreeMap::new();
    wc.insert(
        "name".to_string(),
        WhereCondition::Ops(WhereOps {
            contains: Some(Value::String("alice".to_string())),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Person",
        &ModelQueryInput {
            where_clause: Some(wc),
            limit: Some(1),
            offset: Some(1),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 1, "Should get 1 item in page");
    assert_eq!(result.total_count, 3, "Total matching: 3 Alices");
    assert_eq!(
        result.instances[0]["name"].as_str().unwrap(),
        "Alice Brown",
        "Second Alice by timestamp ASC"
    );
}

/// Helper: create a signed-envelope literal IRI (mimics what expression.create("literal", value)
/// produces in production). The signed envelope is JSON with {author, timestamp, data, proof}.
fn signed_envelope_literal(value: &str) -> String {
    let envelope = serde_json::json!({
        "author": "did:key:zQ3shTestAgent",
        "timestamp": "2024-01-01T00:00:00.000Z",
        "data": value,
        "proof": {
            "key": "#zQ3shTestAgent",
            "signature": "fake-sig",
            "valid": true,
            "invalid": false
        }
    });
    let json_str = serde_json::to_string(&envelope).unwrap();
    format!("literal:json:{}", literal_percent_encode(&json_str))
}

/// Regression test for signed-envelope literals with fn/parse_literal WHERE clauses.
/// Exercises the exact pattern used by paginateSubscribe: model query with WHERE
/// filtering on a literal property, pagination (limit/offset), and count=true,
/// where stored values are signed expression envelopes (literal:json:{signed}).
#[test]
fn test_signed_envelope_where_paginate_count() {
    let store = SparqlStore::new(None).unwrap();
    let ts_base = 1700000000000i64;

    // Insert 4 items: 3 active, 1 inactive — all using signed envelope format
    let items = vec![
        ("test://item-1", "active", "Alpha"),
        ("test://item-2", "active", "Beta"),
        ("test://item-3", "inactive", "Gamma"),
        ("test://item-4", "active", "Delta"),
    ];
    for (i, (uri, status, name)) in items.iter().enumerate() {
        let ts = format!("{}", ts_base + i as i64);
        store
            .add_link(&make_link(uri, "ns://type", "ns://task", &ts))
            .unwrap();
        store
            .add_link(&make_link(
                uri,
                "ns://status",
                &signed_envelope_literal(status),
                &ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                uri,
                "ns://name",
                &signed_envelope_literal(name),
                &ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Task",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://task" },
            "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" },
            "name": { "predicate": "ns://name", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // Query: WHERE status = "active", paginated (limit 2, offset 0), ordered by timestamp ASC
    let mut wc = BTreeMap::new();
    wc.insert(
        "status".to_string(),
        WhereCondition::String("active".to_string()),
    );
    let result = execute_model_query(
        &store,
        "Task",
        &ModelQueryInput {
            where_clause: Some(wc.clone()),
            limit: Some(2),
            offset: Some(0),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            count: Some(true),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();

    // Should return 2 items in page, total_count = 3 (all active items)
    assert_eq!(result.instances.len(), 2, "Page should have 2 items");
    assert_eq!(result.total_count, 3, "Total active items should be 3");
    assert_eq!(
        result.instances[0]["name"].as_str().unwrap(),
        "Alpha",
        "First item by timestamp"
    );
    assert_eq!(
        result.instances[1]["name"].as_str().unwrap(),
        "Beta",
        "Second item by timestamp"
    );

    // Verify hydration: name should be the unwrapped data, not the full signed envelope
    assert_eq!(
        result.instances[0]["status"].as_str().unwrap(),
        "active",
        "Status should be unwrapped from signed envelope"
    );

    // Page 2: offset 2
    let result2 = execute_model_query(
        &store,
        "Task",
        &ModelQueryInput {
            where_clause: Some(wc),
            limit: Some(2),
            offset: Some(2),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            count: Some(true),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();

    assert_eq!(
        result2.instances.len(),
        1,
        "Page 2 should have 1 remaining item"
    );
    assert_eq!(result2.total_count, 3, "Total count unchanged");
    assert_eq!(
        result2.instances[0]["name"].as_str().unwrap(),
        "Delta",
        "Third active item"
    );
}

/// Regression: mixed literal formats (plain + signed envelope) coexist in the same query.
/// This can happen during migration or when different code paths create links.
#[test]
fn test_mixed_plain_and_signed_envelope_where() {
    let store = SparqlStore::new(None).unwrap();
    let ts_base = 1700000000000i64;

    // Item 1: plain literal (old format)
    store
        .add_link(&make_link(
            "test://old",
            "ns://type",
            "ns://msg",
            &format!("{ts_base}"),
        ))
        .unwrap();
    store
        .add_link(&make_link(
            "test://old",
            "ns://body",
            &signed_literal("hello plain"),
            &format!("{ts_base}"),
        ))
        .unwrap();

    // Item 2: signed envelope (new format)
    store
        .add_link(&make_link(
            "test://new",
            "ns://type",
            "ns://msg",
            &format!("{}", ts_base + 1),
        ))
        .unwrap();
    store
        .add_link(&make_link(
            "test://new",
            "ns://body",
            &signed_envelope_literal("hello signed"),
            &format!("{}", ts_base + 1),
        ))
        .unwrap();

    let shape_json = r#"{
        "className": "Msg",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://msg" },
            "body": { "predicate": "ns://body", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // Query with contains "hello" — should match both formats
    let mut wc = BTreeMap::new();
    wc.insert(
        "body".to_string(),
        WhereCondition::Ops(WhereOps {
            contains: Some(Value::String("hello".to_string())),
            ..Default::default()
        }),
    );
    let result = execute_model_query(
        &store,
        "Msg",
        &ModelQueryInput {
            where_clause: Some(wc),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();

    assert_eq!(result.instances.len(), 2, "Both formats should match");
    assert_eq!(result.instances[0]["body"].as_str().unwrap(), "hello plain");
    assert_eq!(
        result.instances[1]["body"].as_str().unwrap(),
        "hello signed"
    );

    // Exact match on signed envelope value
    let mut wc2 = BTreeMap::new();
    wc2.insert(
        "body".to_string(),
        WhereCondition::String("hello signed".to_string()),
    );
    let result2 = execute_model_query(
        &store,
        "Msg",
        &ModelQueryInput {
            where_clause: Some(wc2),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();

    assert_eq!(result2.instances.len(), 1, "Exact match on signed envelope");
    assert_eq!(
        result2.instances[0]["body"].as_str().unwrap(),
        "hello signed"
    );
}

// -----------------------------------------------------------------------
// Performance / scale tests
// -----------------------------------------------------------------------

#[test]
fn test_perf_large_dataset_paginated_query() {
    // Simulate Flux-like scenario: many messages across channels.
    // Verifies that paginated queries complete in a reasonable time.
    let store = SparqlStore::new(None).unwrap();
    let ts_base = 1700000000000i64;
    let num_channels = 3;
    let msgs_per_channel = 1000;

    for ch in 0..num_channels {
        let channel = format!("test://channel-{ch}");
        for i in 0..(msgs_per_channel as i64) {
            let msg = format!("test://msg-{ch}-{i}");
            let ts = format!("{}", ts_base + ch as i64 * 100000 + i);
            store
                .add_link(&make_link(&msg, "flux://type", "flux://message", &ts))
                .unwrap();
            store
                .add_link(&make_link(&msg, "flux://channel", &channel, &ts))
                .unwrap();
            store
                .add_link(&make_link(
                    &msg,
                    "flux://body",
                    &signed_literal(&format!("Message {i} in channel {ch}")),
                    &ts,
                ))
                .unwrap();
        }
    }

    let shape_json = r#"{
        "className": "Message",
        "properties": {
            "type": { "predicate": "flux://type", "required": true, "flag": true, "initial": "flux://message" },
            "body": { "predicate": "flux://body", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {
            "channel": { "predicate": "flux://channel", "kind": "belongsToOne", "direction": "forward" }
        }
    }"#;

    // Query messages in channel-1 with pagination (typical Flux query)
    let mut wc = BTreeMap::new();
    wc.insert(
        "channel".to_string(),
        WhereCondition::String("test://channel-1".to_string()),
    );

    let start = std::time::Instant::now();
    let result = execute_model_query(
        &store,
        "Message",
        &ModelQueryInput {
            where_clause: Some(wc),
            limit: Some(50),
            offset: Some(0),
            order: Some(vec![("timestamp".to_string(), OrderDirection::DESC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    let elapsed = start.elapsed();

    assert_eq!(result.instances.len(), 50, "Should get 50 messages");
    assert_eq!(
        result.total_count, msgs_per_channel,
        "Total count should be messages in one channel"
    );
    eprintln!(
        "Paginated query over {} total messages ({} matching): {:?}",
        num_channels * msgs_per_channel,
        msgs_per_channel,
        elapsed
    );
    assert!(
        elapsed.as_secs() < 5,
        "Query took {:?} — too slow for {} messages",
        elapsed,
        num_channels * msgs_per_channel
    );
}

#[test]
fn test_perf_flux_message_parent_scope_paginated() {
    // Simulates the exact Flux chat-view query:
    //   useLiveQuery(Message, perspective, {
    //     parent: { model: Channel, id: source },
    //     query: { order: { createdAt: 'DESC' } },
    //     pageSize: 30,
    //   })
    //
    // Message model shape:
    //   @Flag({ through: 'flux://entry_type', value: 'flux://has_message' })
    //   @Property({ through: 'flux://body' })
    //   @HasMany({ through: 'flux://has_reaction' })  — collection, no getter
    //   @Property({ through: 'flux://has_reply', getter: "SELECT ?target WHERE { ... }" }) — getter
    //   @Property({ through: 'flux://is_popular', getter: "ASK WHERE { ... }" }) — getter
    //   @HasMany({ through: 'flux://has_thread_message' }) — collection
    //   @HasMany({ through: 'flux://has_reply' }) — collection
    //
    // Parent scope: channel -> ad4m://has_child -> message
    let store = SparqlStore::new(None).unwrap();
    let ts_base = 1700000000000i64;
    let num_channels = 5;
    let msgs_per_channel = 2000;

    for ch in 0..num_channels {
        let channel = format!("test://channel-{ch}");
        for i in 0..(msgs_per_channel as i64) {
            let msg = format!("test://msg-{ch}-{i}");
            let ts = format!("{}", ts_base + ch as i64 * 1000000 + i * 100);
            // Parent link: channel -> has_child -> message
            store
                .add_link(&make_link(&channel, "ad4m://has_child", &msg, &ts))
                .unwrap();
            // Flag: entry_type = has_message
            store
                .add_link(&make_link(
                    &msg,
                    "flux://entry_type",
                    "flux://has_message",
                    &ts,
                ))
                .unwrap();
            // Body property
            store
                .add_link(&make_link(
                    &msg,
                    "flux://body",
                    &signed_literal(&format!("Message {i} in channel {ch}")),
                    &ts,
                ))
                .unwrap();
        }
    }

    let shape_json = r#"{
        "className": "Message",
        "properties": {
            "type": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://has_message" },
            "body": { "predicate": "flux://body", "required": false, "resolveLanguage": "literal" },
            "transcriptStartedAt": { "predicate": "flux://transcript_started_at", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {
            "reactions": { "predicate": "flux://has_reaction", "kind": "hasMany", "direction": "forward" },
            "replyingTo": { "predicate": "flux://has_reply", "kind": "hasOne", "direction": "forward", "getter": "SELECT ?target WHERE { ?target <flux://has_reply> ?source . } LIMIT 1" },
            "isPopular": { "predicate": "flux://is_popular", "kind": "hasOne", "direction": "forward", "getter": "ASK WHERE { SELECT (COUNT(DISTINCT ?reactor) AS ?count) WHERE { ?reactor <flux://has_reaction> ?source . FILTER(?reactor = <emoji://1f44d>) } HAVING(?count > 5) }" },
            "thread": { "predicate": "flux://has_thread_message", "kind": "hasMany", "direction": "forward" },
            "replies": { "predicate": "flux://has_reply", "kind": "hasMany", "direction": "forward" }
        }
    }"#;

    // Query: get 30 most recent messages from channel-2 (ORDER BY createdAt DESC)
    let start = std::time::Instant::now();
    let result = execute_model_query(
        &store,
        "Message",
        &ModelQueryInput {
            parent: Some(ParentScope::Raw {
                id: "test://channel-2".to_string(),
                predicate: "ad4m://has_child".to_string(),
            }),
            limit: Some(30),
            offset: Some(0),
            order: Some(vec![("createdAt".to_string(), OrderDirection::DESC)]),
            count: Some(true),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    let elapsed = start.elapsed();

    assert_eq!(result.instances.len(), 30, "Should get 30 messages");
    assert_eq!(
        result.total_count, msgs_per_channel,
        "Total count should be {} messages in channel-2",
        msgs_per_channel
    );

    // Verify ordering: DESC by createdAt
    for i in 1..result.instances.len() {
        let prev_ts = result.instances[i - 1]["createdAt"].as_str().unwrap_or("");
        let curr_ts = result.instances[i]["createdAt"].as_str().unwrap_or("");
        assert!(
            prev_ts >= curr_ts,
            "Messages should be ordered DESC by createdAt: {} < {}",
            prev_ts,
            curr_ts
        );
    }

    eprintln!(
        "Flux Message query (parent scope, 30 of {}, {} total, with getters): {:?}",
        msgs_per_channel,
        num_channels * msgs_per_channel,
        elapsed
    );
    assert!(
        elapsed.as_secs() < 5,
        "Query took {:?} — too slow for {} messages with parent scope",
        elapsed,
        num_channels * msgs_per_channel
    );

    // --- Raw SPARQL comparison: flag-reifier vs parent-reifier vs no ORDER ---
    // Test 1: No ORDER BY (just the conformance)
    let t_no_order = std::time::Instant::now();
    let q_no_order = r#"SELECT DISTINCT ?source WHERE {
        <test://channel-2> <ad4m://has_child> ?source .
        ?source <flux://entry_type> <flux://has_message> .
    } LIMIT 30"#;
    let _r = store.query(q_no_order).unwrap();
    eprintln!("[RAW] no ORDER BY: {:?}", t_no_order.elapsed());

    // Test 2: Flag-reifier probe (what we currently generate)
    let t_flag = std::time::Instant::now();
    let q_flag = r#"SELECT DISTINCT ?source ?_first_ts WHERE {
        <test://channel-2> <ad4m://has_child> ?source .
        ?source <flux://entry_type> <flux://has_message> .
        ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <flux://entry_type> <flux://has_message> )>> .
        ?_r <ad4m://ontology/timestamp> ?_first_ts .
    } ORDER BY DESC(?_first_ts) LIMIT 30"#;
    let _r = store.query(q_flag).unwrap();
    eprintln!("[RAW] flag-reifier ORDER BY: {:?}", t_flag.elapsed());

    // Test 3: Parent-reifier probe (uses the fully-bound parent IRI)
    let t_parent = std::time::Instant::now();
    let q_parent = r#"SELECT DISTINCT ?source ?_first_ts WHERE {
        <test://channel-2> <ad4m://has_child> ?source .
        ?source <flux://entry_type> <flux://has_message> .
        ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( <test://channel-2> <ad4m://has_child> ?source )>> .
        ?_r <ad4m://ontology/timestamp> ?_first_ts .
    } ORDER BY DESC(?_first_ts) LIMIT 30"#;
    let _r = store.query(q_parent).unwrap();
    eprintln!("[RAW] parent-reifier ORDER BY: {:?}", t_parent.elapsed());

    // Test 4: Wildcard reifier (the old expensive pattern)
    let t_wild = std::time::Instant::now();
    let q_wild = r#"SELECT DISTINCT ?source ?_first_ts WHERE {
        <test://channel-2> <ad4m://has_child> ?source .
        ?source <flux://entry_type> <flux://has_message> .
        ?source ?_anyP ?_anyT .
        ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?_anyP ?_anyT )>> .
        ?_r <ad4m://ontology/timestamp> ?_first_ts .
    } ORDER BY DESC(?_first_ts) LIMIT 30"#;
    let _r = store.query(q_wild).unwrap();
    eprintln!("[RAW] wildcard-reifier ORDER BY: {:?}", t_wild.elapsed());

    // Test 5: Full outer query (the actual generated pattern)
    let t_full = std::time::Instant::now();
    let q_full = r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
        {
            SELECT DISTINCT ?source ?_first_ts WHERE {
                <test://channel-2> <ad4m://has_child> ?source .
                ?source <flux://entry_type> <flux://has_message> .
                ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <flux://entry_type> <flux://has_message> )>> .
                ?_r <ad4m://ontology/timestamp> ?_first_ts .
            } ORDER BY DESC(?_first_ts) LIMIT 30
        }
        VALUES ?predicate { <flux://body> <flux://entry_type> <flux://has_reaction> <flux://has_reply> <flux://has_thread_message> <flux://transcript_started_at> }
        ?source ?predicate ?target .
        ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
        FILTER(isIRI(?predicate))
        ?_reifier <ad4m://ontology/author> ?author .
        ?_reifier <ad4m://ontology/timestamp> ?timestamp .
    }"#;
    let r_full: Vec<Value> = serde_json::from_str(&store.query(q_full).unwrap()).unwrap();
    eprintln!(
        "[RAW] full outer query: {:?} ({} rows)",
        t_full.elapsed(),
        r_full.len()
    );

    // Test 6: Outer query without subquery (pre-materialized sources via VALUES)
    // Get the 30 source IDs first, then query their properties
    let q_ids = r#"SELECT DISTINCT ?source WHERE {
        <test://channel-2> <ad4m://has_child> ?source .
        ?source <flux://entry_type> <flux://has_message> .
        ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <flux://entry_type> <flux://has_message> )>> .
        ?_r <ad4m://ontology/timestamp> ?_first_ts .
    } ORDER BY DESC(?_first_ts) LIMIT 30"#;
    let ids_json: Vec<Value> = serde_json::from_str(&store.query(q_ids).unwrap()).unwrap();
    let source_values: String = ids_json
        .iter()
        .filter_map(|r| r["source"].as_str())
        .map(|s| format!("<{s}>"))
        .collect::<Vec<_>>()
        .join(" ");
    let t_outer = std::time::Instant::now();
    let q_outer = format!(
        r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
        VALUES ?source {{ {source_values} }}
        VALUES ?predicate {{ <flux://body> <flux://entry_type> <flux://has_reaction> <flux://has_reply> <flux://has_thread_message> <flux://transcript_started_at> }}
        ?source ?predicate ?target .
        ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
        FILTER(isIRI(?predicate))
        ?_reifier <ad4m://ontology/author> ?author .
        ?_reifier <ad4m://ontology/timestamp> ?timestamp .
    }}"#
    );
    let r_outer: Vec<Value> = serde_json::from_str(&store.query(&q_outer).unwrap()).unwrap();
    eprintln!(
        "[RAW] pre-materialized VALUES outer: {:?} ({} rows)",
        t_outer.elapsed(),
        r_outer.len()
    );
}

// -----------------------------------------------------------------------
// Property-key sort push tests
// -----------------------------------------------------------------------

#[test]
fn test_full_model_query_order_by_property_string() {
    // ORDER BY a string property should be pushed to SPARQL
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let names = ["Charlie", "Alice", "Bob"];
    for (i, name) in names.iter().enumerate() {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://person", ts))
            .unwrap();
        store
            .add_link(&make_link(&item, "ns://name", &signed_literal(name), ts))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Person",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://person" },
            "name": { "predicate": "ns://name", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // ORDER BY name ASC with limit (triggers SPARQL pagination)
    let result = execute_model_query(
        &store,
        "Person",
        &ModelQueryInput {
            limit: Some(2),
            order: Some(vec![("name".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2);
    assert_eq!(result.total_count, 3);
    // First 2 alphabetically: Alice, Bob
    assert_eq!(result.instances[0]["name"].as_str().unwrap(), "Alice");
    assert_eq!(result.instances[1]["name"].as_str().unwrap(), "Bob");

    // ORDER BY name DESC with limit
    let result = execute_model_query(
        &store,
        "Person",
        &ModelQueryInput {
            limit: Some(2),
            order: Some(vec![("name".to_string(), OrderDirection::DESC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 2);
    // First 2 reverse alphabetically: Charlie, Bob
    assert_eq!(result.instances[0]["name"].as_str().unwrap(), "Charlie");
    assert_eq!(result.instances[1]["name"].as_str().unwrap(), "Bob");
}

#[test]
fn test_full_model_query_order_by_property_number() {
    // ORDER BY a numeric property
    let store = SparqlStore::new(None).unwrap();
    let ts = "1700000000000";

    let scores = [100.0, 5.0, 42.0, 1.0, 999.0];
    for (i, &score) in scores.iter().enumerate() {
        let item = format!("test://item-{i}");
        store
            .add_link(&make_link(&item, "ns://type", "ns://scored", ts))
            .unwrap();
        store
            .add_link(&make_link(
                &item,
                "ns://score",
                &signed_literal_number(score),
                ts,
            ))
            .unwrap();
    }

    let shape_json = r#"{
        "className": "Scored",
        "properties": {
            "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://scored" },
            "score": { "predicate": "ns://score", "required": false, "resolveLanguage": "literal" }
        },
        "relations": {}
    }"#;

    // ORDER BY score ASC, limit 3 → should get 1, 5, 42
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            limit: Some(3),
            order: Some(vec![("score".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    assert_eq!(result.instances.len(), 3);
    assert_eq!(result.total_count, 5);
    let got_scores: Vec<f64> = result
        .instances
        .iter()
        .map(|i| i["score"].as_f64().unwrap())
        .collect();
    assert_eq!(got_scores, vec![1.0, 5.0, 42.0], "ASC numeric sort");

    // ORDER BY score DESC, limit 2 → should get 999, 100
    let result = execute_model_query(
        &store,
        "Scored",
        &ModelQueryInput {
            limit: Some(2),
            order: Some(vec![("score".to_string(), OrderDirection::DESC)]),
            ..Default::default()
        },
        Some(shape_json),
    )
    .unwrap();
    let got_scores: Vec<f64> = result
        .instances
        .iter()
        .map(|i| i["score"].as_f64().unwrap())
        .collect();
    assert_eq!(got_scores, vec![999.0, 100.0], "DESC numeric sort");
}

#[test]
fn test_resolve_projections_where_filter_via_target_shape_property() {
    // Mirrors the WE $totalLikeCount pattern:
    //   include: {
    //     $totalLikeCount: { from: 'signals', where: { signalTypeId: 'like_type_id123' }, count: true }
    //   }
    // where signalTypeId is a @Property stored via literal_encode, and the WHERE
    // filter value is the plain decoded form (what the TS caller passes).
    // The fn/parse_literal FILTER decodes the stored literal:string:X IRI back to "X"
    // for comparison — so caller passes "like_type_id123", not "literal:string:like_type_id123".
    let store = SparqlStore::new(None).unwrap();

    let parent_a = "test://parent/a";
    let like_signal = "test://signal/like";
    let dislike_signal = "test://signal/dislike";
    let like_type_id_stored = "literal:string:like_type_id123";
    let dislike_type_id_stored = "literal:string:dislike_type_id456";
    let like_type_id_filter = "like_type_id123"; // plain decoded value — what the caller passes

    // Add parent → signal links
    store
        .add_link(&make_link(
            parent_a,
            "test://has_signal",
            like_signal,
            "1000",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            parent_a,
            "test://has_signal",
            dislike_signal,
            "1001",
        ))
        .unwrap();

    // Add signal → signalTypeId property links (stored as literal:string: IRIs via literal_encode)
    store
        .add_link(&make_link(
            like_signal,
            "test://signal_type_id",
            like_type_id_stored,
            "1002",
        ))
        .unwrap();
    store
        .add_link(&make_link(
            dislike_signal,
            "test://signal_type_id",
            dislike_type_id_stored,
            "1003",
        ))
        .unwrap();

    let shape = make_shape_with_relation("Parent", "signals", "test://has_signal");

    let target_shape = json!({
        "className": "Signal",
        "properties": {
            "signalTypeId": { "predicate": "test://signal_type_id" }
        },
        "relations": {}
    });

    // Filter passes the plain decoded value — fn/parse_literal in SPARQL decodes
    // the stored literal:string:like_type_id123 IRI back to "like_type_id123".
    let mut wc = BTreeMap::new();
    wc.insert(
        "signalTypeId".to_string(),
        WhereCondition::String(like_type_id_filter.to_string()),
    );

    let mut instances = vec![json!({ "id": parent_a })];
    let mut projections = HashMap::new();
    projections.insert(
        "$totalLikeCount".to_string(),
        ProjectionInput {
            from: "signals".to_string(),
            count: true,
            target_shape: Some(target_shape.clone()),
            where_clause: Some(wc.clone()),
            limit: None,
            order: None,
        },
    );

    resolve_projections(&store, &mut instances, &projections, &shape, 0).unwrap();

    let count = instances[0]["$totalLikeCount"].as_u64().unwrap_or(999);
    assert_eq!(
        count, 1,
        "should count only the 'like' signal (1 of 2), got {count}"
    );

    // Also verify the LIST variant ($myLikeSignal pattern with limit: 1).
    let mut instances2 = vec![json!({ "id": parent_a })];
    let mut projections2 = HashMap::new();
    projections2.insert(
        "$myLikeSignal".to_string(),
        ProjectionInput {
            from: "signals".to_string(),
            count: false,
            target_shape: Some(target_shape),
            where_clause: Some(wc),
            limit: Some(1),
            order: None,
        },
    );

    resolve_projections(&store, &mut instances2, &projections2, &shape, 0).unwrap();

    let got = &instances2[0]["$myLikeSignal"];
    assert_eq!(
        got["id"].as_str().unwrap_or(""),
        like_signal,
        "list with limit:1 should return the hydrated like signal id, got {got}"
    );
}
