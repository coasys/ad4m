use super::shape::parse_shape_from_json;
use super::types::{InstanceLinks, ModelShape, ShapeProperty, ShapeResolver};
use crate::agent::signatures::TestSigner;
use crate::types::{Link, LinkExpression, LinkStatus};
use chrono::{DateTime, SecondsFormat, Utc};
use deno_core::anyhow::{anyhow, Error};
use std::collections::HashMap;
use std::str::FromStr;
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
        transform: None,
        interpretation_hint: None,
        identity: false,
        local: false,
        ordering: None,
    }
}

/// Helper: build a ShapeProperty for a URI-valued collection relation
/// (no `sh:datatype`, so targets pass through byte-for-byte on hydration).
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
        transform: None,
        interpretation_hint: None,
        identity: false,
        local: false,
        ordering: None,
    }
}

/// Helper: build a ShapeProperty for a scalar (to-one) relation — the
/// `@HasOne` / `@BelongsToOne` case. Still `is_collection` (all relations are,
/// so the query pipeline treats them uniformly); `is_scalar_relation` is what
/// collapses it to a single value on hydration.
pub fn scalar_relation(name: &str, predicate: &str) -> ShapeProperty {
    let mut p = relation(name, predicate);
    p.is_scalar_relation = true;
    p
}

/// Helper: build a ShapeProperty for a literal-valued collection relation
/// (declares `sh:datatype`, so `literal:<type>:<value>` wire form is
/// decoded on hydration — the `@HasMany({ datatype: "xsd:string" })`
/// case in TypeScript). Use `xsd://string` for the common
/// `HasMany<string>` scenario.
pub fn relation_with_datatype(name: &str, predicate: &str, datatype: &str) -> ShapeProperty {
    let mut p = relation(name, predicate);
    p.datatype = Some(datatype.to_string());
    p
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
        transform: None,
        interpretation_hint: None,
        identity: false,
        local: false,
        ordering: None,
    }
}

/// Helper: build a ModelShape from a list of properties.
pub fn shape(class: &str, properties: Vec<ShapeProperty>) -> ModelShape {
    ModelShape {
        target_class: class.to_string(),
        shape_uri: format!("{class}Shape"),
        properties,
        include_relations: Vec::new(),
        interpretation_hint: None,
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
pub async fn execute_model_query_from_json(
    store: &crate::perspectives::sparql_store::SparqlStore,
    class_name: &str,
    query_input: &super::types::ModelQueryInput,
    shape_json: &str,
) -> Result<super::types::ModelQueryResult, Error> {
    let (resolver, shape) = StaticShapeResolver::from_json(class_name, shape_json)?;
    super::query::execute_model_query(store, shape.as_ref(), query_input, &resolver).await
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
    super::getters::evaluate_getters_batch(
        &crate::perspectives::sparql_store::EvalStop::none(),
        store,
        &shape,
        instance_ids,
        property_names,
    )
}

/// A link signed for real ([`TestSigner`]), stored with `ts` as its timestamp.
///
/// Reads that count only verified links — the classifier
/// (`subject_classes_of`), every read a `LinkGuard` joins — see a fixture's
/// links the same way the unguarded reads always did only when their
/// signature holds, and the signature covers the stored timestamp, so it must
/// name a date. `ts` stays verbatim when it parses (tests read the timestamp
/// back) and maps to epoch millis for a bare number, the forms the
/// classifier's fixtures use — different `ts` values still mint different
/// links. Anything else stays verbatim too: signed or not its timestamp never
/// parses, so it never counts, exactly as a fake proof never did.
pub fn signed_link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
    let (at, timestamp) = match DateTime::<Utc>::from_str(ts) {
        Ok(at) => (at, ts.to_string()),
        Err(_) => match ts.parse::<i64>() {
            Ok(ms) => {
                let at = DateTime::<Utc>::from_timestamp_millis(ms).expect("epoch millis");
                (at, at.to_rfc3339_opts(SecondsFormat::Millis, true))
            }
            Err(_) => {
                use std::hash::{Hash, Hasher};
                let mut h = std::collections::hash_map::DefaultHasher::new();
                ts.hash(&mut h);
                (
                    DateTime::<Utc>::from_timestamp_millis((h.finish() % 10_000_000_000) as i64)
                        .unwrap(),
                    ts.to_string(),
                )
            }
        },
    };
    let mut le = LinkExpression::from(
        TestSigner::generate().sign_at(
            Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }
            .normalize(),
            at,
        ),
    );
    le.timestamp = timestamp;
    // The store refuses status-less inserts.
    le.status = Some(LinkStatus::Shared);
    le
}

/// Helper: build an InstanceLinks entry with explicit per-link timestamps.
///
/// `inst_links` derives an increasing timestamp from position, which is right
/// for most tests but cannot express the two cases that matter for duplicate
/// and last-write-wins handling: the same `(predicate, target)` appearing twice
/// at *different* timestamps (two reifiers over one triple), and links arriving
/// out of timestamp order. Each entry is `(predicate, target, timestamp)`.
pub fn inst_links_at(source: &str, links: Vec<(&str, &str, &str)>) -> InstanceLinks {
    InstanceLinks {
        source: source.to_string(),
        links: links
            .into_iter()
            .map(|(pred, tgt, ts)| {
                (
                    pred.to_string(),
                    tgt.to_string(),
                    "did:key:testauthor".to_string(),
                    ts.to_string(),
                )
            })
            .collect(),
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
