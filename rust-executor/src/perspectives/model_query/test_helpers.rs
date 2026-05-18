use super::types::{InstanceLinks, ModelShape, ShapeProperty};

/// Helper: build a minimal ShapeProperty for a scalar (non-collection) field.
pub fn prop(name: &str, predicate: &str) -> ShapeProperty {
    ShapeProperty {
        name: name.to_string(),
        predicate: predicate.to_string(),
        is_collection: false,
        is_flag: false,
        is_required: false,
        initial_value: None,
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
pub fn relation(name: &str, predicate: &str) -> ShapeProperty {
    ShapeProperty {
        name: name.to_string(),
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
    }
}

/// Helper: build a ShapeProperty for a flag field.
pub fn flag(name: &str, predicate: &str, initial: &str) -> ShapeProperty {
    ShapeProperty {
        name: name.to_string(),
        predicate: predicate.to_string(),
        is_collection: false,
        is_flag: true,
        is_required: true,
        initial_value: Some(initial.to_string()),
        resolve_language: None,
        datatype: None,
        direction: None,
        is_scalar_relation: false,
        getter: None,
        where_filter: None,
        where_predicates: None,
    }
}

/// Helper: build a ModelShape from a list of properties.
pub fn shape(class: &str, properties: Vec<ShapeProperty>) -> ModelShape {
    ModelShape {
        target_class: class.to_string(),
        shape_uri: format!("{class}Shape"),
        properties,
        include_relations: Vec::new(),
    }
}

/// Helper: build an InstanceLinks entry.
pub fn inst_links(source: &str, links: Vec<(&str, &str)>) -> InstanceLinks {
    InstanceLinks {
        source: source.to_string(),
        links: links
            .into_iter()
            .enumerate()
            .map(|(i, (pred, tgt))| {
                (
                    pred.to_string(),
                    tgt.to_string(),
                    "did:key:testauthor".to_string(),
                    format!("2026-01-01T00:00:{i:02}.000Z"),
                )
            })
            .collect(),
    }
}
