//! The `activity` order key ([`super::ACTIVITY_ORDER_KEY`]).
//!
//! The last activity of an instance is the newest link it is the source of
//! (its creation, its `updatedAt`) or the target of (a row that attached to
//! it). These tests seed a store with links at known seconds and read the
//! order a query gives.

use super::test_helpers::execute_model_query_from_json;
use super::types::{ModelQueryInput, OrderDirection};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};

const TOPIC_SHAPE_JSON: &str = r#"{
    "className": "Topic",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"pt://Topic"},
        "title": {"predicate":"pt://title"}
    },
    "relations": {}
}"#;

fn at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 11, 18, 12, 0, second)
        .unwrap()
}

fn add(store: &SparqlStore, signer: &TestSigner, s: &str, p: &str, t: &str, second: u32) {
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: s.to_string(),
            predicate: Some(p.to_string()),
            target: t.to_string(),
        },
        at(second),
    ));
    l.status = Some(LinkStatus::Shared);
    assert!(l.compute_proof_valid(), "the fixture must verify");
    store.add_link(&l).unwrap();
}

/// Three topics, created at 1, 2 and 3. Topic 1 gets a new title at 10.
/// Topic 2 gets a row that points to it at 20 (a row of another class that
/// attached to it).
fn seed(store: &SparqlStore) {
    let a = TestSigner::generate();
    for (n, second) in [(1, 1), (2, 2), (3, 3)] {
        let id = format!("pt://topic/{n}");
        add(store, &a, &id, "ad4m://type", "pt://Topic", second);
        add(
            store,
            &a,
            &id,
            "pt://title",
            &format!("literal:string:Topic%20{n}"),
            second,
        );
    }
    add(
        store,
        &a,
        "pt://topic/1",
        "pt://title",
        "literal:string:Topic%201%20renamed",
        10,
    );
    add(store, &a, "pt://item/x", "pt://about", "pt://topic/2", 20);
}

async fn ids(store: &SparqlStore, q: ModelQueryInput) -> Vec<String> {
    execute_model_query_from_json(store, "Topic", &q, TOPIC_SHAPE_JSON)
        .await
        .unwrap()
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap().to_string())
        .collect()
}

fn ordered(key: &str, limit: Option<usize>) -> ModelQueryInput {
    ModelQueryInput {
        order: Some(vec![(key.to_string(), OrderDirection::DESC)]),
        limit,
        ..Default::default()
    }
}

#[tokio::test]
async fn activity_puts_a_node_first_after_a_row_attaches_to_it() {
    let store = SparqlStore::new(None).unwrap();
    seed(&store);
    // Creation time alone: newest created first.
    assert_eq!(
        ids(&store, ordered("createdAt", None)).await,
        vec!["pt://topic/3", "pt://topic/2", "pt://topic/1"]
    );
    // Activity: topic 2 (a row attached at 20), topic 1 (changed at 10),
    // topic 3 (created at 3).
    assert_eq!(
        ids(&store, ordered("activity", None)).await,
        vec!["pt://topic/2", "pt://topic/1", "pt://topic/3"]
    );
}

#[tokio::test]
async fn activity_pages_in_the_store_and_counts_all_rows() {
    let store = SparqlStore::new(None).unwrap();
    seed(&store);
    let result = execute_model_query_from_json(
        &store,
        "Topic",
        &ModelQueryInput {
            count: Some(true),
            ..ordered("activity", Some(2))
        },
        TOPIC_SHAPE_JSON,
    )
    .await
    .unwrap();
    let page: Vec<&str> = result
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap())
        .collect();
    assert_eq!(page, vec!["pt://topic/2", "pt://topic/1"]);
    assert_eq!(result.total_count, Some(3));

    // Ascending: the least active first.
    let asc = ModelQueryInput {
        order: Some(vec![("activity".to_string(), OrderDirection::ASC)]),
        ..Default::default()
    };
    assert_eq!(
        ids(&store, asc).await,
        vec!["pt://topic/3", "pt://topic/1", "pt://topic/2"]
    );
}

/// A `where` the store cannot answer runs after hydration. The activity
/// order still holds, and the window is applied after the filter.
#[tokio::test]
async fn activity_order_holds_with_a_filter() {
    let store = SparqlStore::new(None).unwrap();
    seed(&store);
    let q = ModelQueryInput {
        order: Some(vec![("activity".to_string(), OrderDirection::DESC)]),
        where_clause: Some(
            serde_json::from_value(serde_json::json!({
                "title": {"contains": "Topic"}
            }))
            .unwrap(),
        ),
        limit: Some(2),
        ..Default::default()
    };
    assert_eq!(ids(&store, q).await, vec!["pt://topic/2", "pt://topic/1"]);
}
