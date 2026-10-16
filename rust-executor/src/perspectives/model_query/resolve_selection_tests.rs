//! Which properties a query resolves through their language: only those it hands back or still
//! reads. A property resolved through a language is a fetch, often a network round trip, and one the
//! query did not ask for is stripped before the result leaves — so resolving it was the whole cost of
//! a record's file for a query that wanted its name.

use super::query::{properties_to_resolve, resolve_language_transforms};
use super::test_helpers::{prop, shape, StaticShapeResolver};
use super::types::{ModelQueryInput, OrderDirection, TransformExpression, WhereCondition};
use serde_json::json;
use std::collections::{BTreeMap, HashSet};

fn names(set: Option<HashSet<String>>) -> Option<Vec<String>> {
    set.map(|s| {
        let mut v: Vec<String> = s.into_iter().collect();
        v.sort();
        v
    })
}

fn model() -> super::types::ModelShape {
    let mut body = prop("body", "test://body");
    body.resolve_language = Some("QmFileStorage".to_string());
    shape("Document", vec![prop("name", "test://name"), body])
}

#[test]
fn resolves_everything_when_no_properties_are_named() {
    let query = ModelQueryInput::default();
    assert_eq!(
        names(properties_to_resolve(
            &query,
            &model(),
            &StaticShapeResolver::new()
        )),
        None
    );
}

#[test]
fn resolves_only_what_was_asked_for() {
    let query = ModelQueryInput {
        properties: Some(vec!["name".to_string()]),
        ..Default::default()
    };
    assert_eq!(
        names(properties_to_resolve(
            &query,
            &model(),
            &StaticShapeResolver::new()
        )),
        Some(vec!["name".to_string()])
    );
}

#[test]
fn keeps_what_a_sort_reads() {
    let query = ModelQueryInput {
        properties: Some(vec!["name".to_string()]),
        order: Some(vec![("body".to_string(), OrderDirection::ASC)]),
        ..Default::default()
    };
    assert_eq!(
        names(properties_to_resolve(
            &query,
            &model(),
            &StaticShapeResolver::new()
        )),
        Some(vec!["body".to_string(), "name".to_string()])
    );
}

#[test]
fn resolves_everything_for_a_filter_run_after_hydration() {
    let mut where_clause = BTreeMap::new();
    where_clause.insert(
        "notAProperty".to_string(),
        WhereCondition::String("x".to_string()),
    );
    let query = ModelQueryInput {
        properties: Some(vec!["name".to_string()]),
        where_clause: Some(where_clause),
        ..Default::default()
    };
    let resolver = StaticShapeResolver::new();
    assert!(
        !super::sparql_builder::all_where_pushable(&query, &model(), Some(&resolver)),
        "the case needs a where clause the store cannot take"
    );
    assert_eq!(
        names(properties_to_resolve(&query, &model(), &resolver)),
        None
    );
}

/// A deterministic property with a transform is transformed here without any language, so whether
/// it was left alone says whether the selection reached it.
#[tokio::test]
async fn leaves_a_property_it_was_not_asked_for_as_it_was() {
    let mut label = prop("label", "test://label");
    label.transform = Some(TransformExpression::Literal {
        value: json!("transformed"),
    });
    let shape = shape("Thing", vec![prop("name", "test://name"), label]);

    let mut skipped = vec![json!({ "id": "a", "name": "A", "label": "stored" })];
    let only: HashSet<String> = ["name".to_string()].into_iter().collect();
    resolve_language_transforms(&shape, &mut skipped, Some(&only))
        .await
        .unwrap();
    assert_eq!(skipped[0]["label"], json!("stored"));

    let mut resolved = vec![json!({ "id": "a", "name": "A", "label": "stored" })];
    resolve_language_transforms(&shape, &mut resolved, None)
        .await
        .unwrap();
    assert_eq!(resolved[0]["label"], json!("transformed"));
}
