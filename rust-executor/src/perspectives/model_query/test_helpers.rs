use super::shape::parse_shape_from_json;
use super::types::{InstanceLinks, ModelShape, ShapeProperty, ShapeResolver};
use deno_core::anyhow::{anyhow, Error};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

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

/// In-memory `ShapeResolver` used by the test suite to mimic the
/// production cache-backed resolver without a `PerspectiveInstance`.
/// Test code pre-registers `ModelShape` instances keyed by class name;
/// recursive include resolution then looks them up just like in production.
pub struct StaticShapeResolver {
    shapes: RwLock<HashMap<String, Arc<ModelShape>>>,
}

impl StaticShapeResolver {
    pub fn new() -> Self {
        Self {
            shapes: RwLock::new(HashMap::new()),
        }
    }

    pub fn register(&self, class_name: &str, shape: ModelShape) {
        self.shapes
            .write()
            .unwrap()
            .insert(class_name.to_string(), Arc::new(shape));
    }

    pub fn register_arc(&self, class_name: &str, shape: Arc<ModelShape>) {
        self.shapes
            .write()
            .unwrap()
            .insert(class_name.to_string(), shape);
    }

    pub fn from_json(class_name: &str, shape_json: &str) -> Result<(Self, Arc<ModelShape>), Error> {
        let shape = parse_shape_from_json(shape_json, class_name)?;
        let arc = Arc::new(shape);
        let resolver = Self::new();
        resolver.register_arc(class_name, arc.clone());
        Ok((resolver, arc))
    }
}

impl ShapeResolver for StaticShapeResolver {
    fn get_shape(&self, class_name: &str) -> Result<Arc<ModelShape>, Error> {
        self.shapes
            .read()
            .unwrap()
            .get(class_name)
            .cloned()
            .ok_or_else(|| {
                anyhow!(
                    "StaticShapeResolver: no shape registered for '{}'",
                    class_name
                )
            })
    }
}

/// Test wrapper preserving the legacy `(store, class_name, query, Some(shape_json))`
/// invocation pattern.  Builds a one-shot `StaticShapeResolver` from the
/// shape JSON and delegates to the production `execute_model_query`.
pub fn execute_model_query_from_json(
    store: &crate::perspectives::sparql_store::SparqlStore,
    class_name: &str,
    query_input: &super::types::ModelQueryInput,
    shape_json: &str,
) -> Result<super::types::ModelQueryResult, Error> {
    let (resolver, shape) = StaticShapeResolver::from_json(class_name, shape_json)?;
    super::query::execute_model_query(store, shape.as_ref(), query_input, &resolver)
}

/// Test wrapper for `evaluate_getters_batch` that takes shape JSON.
pub fn evaluate_getters_batch_from_json(
    store: &crate::perspectives::sparql_store::SparqlStore,
    class_name: &str,
    instance_ids: &[String],
    property_names: Option<&[String]>,
    shape_json: &str,
) -> Result<serde_json::Value, Error> {
    let shape = parse_shape_from_json(shape_json, class_name)?;
    super::getters::evaluate_getters_batch(store, &shape, instance_ids, property_names)
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
