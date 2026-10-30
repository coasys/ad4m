//! Which properties a query resolves through their language: only those it hands back or still
//! reads. A property resolved through a language is a fetch, often a network round trip, and one the
//! query did not ask for is stripped before the result leaves — so resolving it was the whole cost of
//! a record's file for a query that wanted its name.
//!
//! What the `where` and `order` read is resolved for every row before the window, whether the query
//! returns it or not; the page pass resolves the rest of what it returns.

use super::page_before_resolve_tests::{
    data_uri, file_type, id, ids, run, seed, shape_json, ts_ms, FileLanguage,
};
use super::query::{resolve_language_transforms, returned};
use super::test_helpers::{prop, shape};
use super::types::{ModelQueryInput, TransformExpression};
use serde_json::json;
use std::time::Duration;

fn sorted(keys: Option<Vec<String>>) -> Option<Vec<String>> {
    keys.map(|mut v| {
        v.sort();
        v
    })
}

fn strings(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

#[test]
fn returns_everything_when_no_properties_are_named() {
    assert_eq!(returned(&ModelQueryInput::default()), None);
}

#[test]
fn returns_only_what_was_asked_for() {
    let query = ModelQueryInput {
        properties: Some(strings(&["name"])),
        ..Default::default()
    };
    assert_eq!(
        sorted(returned(&query)),
        Some(strings(&["baseExpression", "id", "name"]))
    );
}

#[test]
fn returns_what_an_include_hands_back() {
    let query: ModelQueryInput = serde_json::from_value(json!({
        "properties": ["name"],
        "include": { "body": true }
    }))
    .unwrap();
    assert_eq!(
        sorted(returned(&query)),
        Some(strings(&["baseExpression", "body", "id", "name"]))
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
    resolve_language_transforms(&shape, &mut skipped, |name| name == "name")
        .await
        .unwrap();
    assert_eq!(skipped[0]["label"], json!("stored"));

    let mut resolved = vec![json!({ "id": "a", "name": "A", "label": "stored" })];
    resolve_language_transforms(&shape, &mut resolved, |_| true)
        .await
        .unwrap();
    assert_eq!(resolved[0]["label"], json!("transformed"));
}

/// No `properties`: every row of the page comes back with its file decoded.
#[tokio::test]
async fn a_query_without_properties_resolves_every_file_on_its_page() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(&store, &shape, json!({ "limit": 4 })).await;

    assert_eq!(rows.len(), 4);
    for row in &rows {
        let i: usize = row["id"].as_str().unwrap()[7..].parse().unwrap();
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), 4);
    files.stop().await;
}

/// `properties` that leave the file out: the file is stripped, so it is not fetched.
#[tokio::test]
async fn a_query_naming_its_properties_fetches_no_file_it_leaves_out() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({ "properties": ["title"], "limit": 4 }),
    )
    .await;

    assert_eq!(rows.len(), 4);
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
        assert!(row["title"].is_string(), "{row}");
    }
    assert_eq!(files.gets(), 0, "fetched a file the query left out");
    files.stop().await;
}

/// `properties` that name the file, with a filter that runs after hydration: the page's files.
#[tokio::test]
async fn a_query_naming_the_file_fetches_it_for_the_page() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(30, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["title", "file"],
            "where": { "kind": "post", "timestamp": { "gt": ts_ms(10) } },
            "limit": 3,
        }),
    )
    .await;

    let page: Vec<usize> = vec![12, 14, 16];
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, 9);
    for (row, &i) in rows.iter().zip(&page) {
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), page.len());
    files.stop().await;
}

/// The strip keeps `include` keys beside `properties`, so a value it hands back under one is
/// resolved too.
#[tokio::test]
async fn an_include_key_is_resolved_as_the_strip_keeps_it() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({ "properties": ["title"], "include": { "file": true }, "limit": 3 }),
    )
    .await;

    assert_eq!(rows.len(), 3);
    for row in &rows {
        let i: usize = row["id"].as_str().unwrap()[7..].parse().unwrap();
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), 3);
    files.stop().await;
}

/// A `where` on the file that `properties` leaves out still reads every row's decoded file, and
/// the file is then stripped: the filter needs it, the result does not.
#[tokio::test]
async fn a_where_on_a_file_left_out_of_properties_still_reads_the_decoded_file() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let n = 30;
    let store = seed(n, Some(&files));
    let shape = shape_json(Some(&files));

    // The `timestamp` arm keeps the whole `OR` out of the store.
    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["title"],
            "where": { "OR": [
                { "file": { "contains": "image/png" } },
                { "timestamp": { "gt": ts_ms(25) } }
            ] },
            "limit": 4,
            "offset": 2,
            "order": { "timestamp": "DESC" },
        }),
    )
    .await;

    let matching: Vec<usize> = (0..n).rev().filter(|&i| i % 3 == 0 || i > 25).collect();
    let page: Vec<usize> = matching.iter().copied().skip(2).take(4).collect();
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, matching.len());
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
    }
    assert_eq!(files.gets(), n, "each file is fetched once, for the filter");
    files.stop().await;
}

/// A sort on a field of the file that `properties` leaves out picks the page from decoded values.
#[tokio::test]
async fn a_sort_on_a_file_left_out_of_properties_picks_the_page_from_decoded_values() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    // `rank` = 7i mod 13 is distinct for each of 12 items, so the order has no ties.
    let n = 12;
    let store = seed(n, Some(&files));
    // `focus` keeps the fetched expression data, so `file` decodes to an object.
    let shape = format!(
        r#"{{
        "className": "Item",
        "properties": {{
            "type": {{ "predicate": "ad4m://type", "required": true, "flag": true, "initial": "item://Item" }},
            "rank": {{ "predicate": "item://rank" }},
            "file": {{ "predicate": "item://file", "resolveLanguage": "{}", "transform": {{ "type": "focus" }} }}
        }},
        "relations": {{}}
    }}"#,
        files.address()
    );

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["rank"],
            "order": [["file.file_type", "DESC"], ["rank", "ASC"]],
            "limit": 5,
            "offset": 2,
        }),
    )
    .await;

    let mut expected: Vec<usize> = (0..n).collect();
    expected.sort_by(|&a, &b| {
        file_type(b)
            .cmp(file_type(a))
            .then(((a * 7) % 13).cmp(&((b * 7) % 13)))
    });
    let page: Vec<usize> = expected.into_iter().skip(2).take(5).collect();
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
    }
    assert_eq!(files.gets(), n, "each file is fetched once, for the sort");
    files.stop().await;
}
