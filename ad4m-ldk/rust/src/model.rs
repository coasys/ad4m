//! Mount-class shape support for `#[derive(Ad4mModel)]`.
//!
//! The derive generates `Class::generate_shacl()` — the Rust
//! counterpart of the TS model decorators' `generateSHACL()` — in terms
//! of the [`MountShapeBuilder`] here, so the generated code never names
//! `serde_json` itself (a language crate needs only the `ad4m-ldk`
//! dependency). [`MountShapes::from_classes`] then wraps the classes
//! into the `mountShapes()` envelope — the `mount.classes` form of the
//! JS `defineLanguage`.
//!
//! The generated shape is the *mount* form: the fields the mount
//! contract reads, emitted the way `generateSHACL()` emits them for a
//! `mount: true` model — no datatype inference, `writable` only when a
//! property really is writable, and `call` actions in place of link
//! writes.

use crate::types::{MountShapeEntry, MountShapes};

/// `{ name, shape }` — one class's `generate_shacl()` answer, the mount
/// shape the language lists under `mount.classes`. `shape` keeps
/// `node_shape_uri` and the `{language}` placeholder: both are resolved
/// when [`MountShapes::from_classes`] builds the envelope, as in the JS
/// LDK.
pub struct MountClassShape {
    pub name: String,
    pub shape: serde_json::Value,
}

/// Builder the derive emits `generate_shacl()` against — the Rust
/// counterpart of `core/src/shacl/SHACLShape.ts`. Setter calls append
/// one property object to `properties`; [`finish`](Self::finish) folds
/// the object into its `serde_json::Value` wire form.
pub struct MountShapeBuilder {
    shape: serde_json::Map<String, serde_json::Value>,
    properties: Vec<serde_json::Value>,
}

/// One `call` action — `{ action: "call", source, predicate, target }`,
/// the wire form the executor's call-action engine reads. `target` is
/// the params JSON verbatim (a string, already-encoded).
fn call_action(interaction: &str, source: &str, target_params: &str) -> serde_json::Value {
    serde_json::json!({
        "action": "call",
        "source": source,
        "predicate": interaction,
        "target": target_params,
    })
}

fn call_actions(calls: &[(&str, &str, &str)]) -> serde_json::Value {
    serde_json::Value::Array(calls.iter().map(|c| call_action(c.0, c.1, c.2)).collect())
}

impl MountShapeBuilder {
    /// `SHACLShape::new(shape_uri, target_class)`.
    pub fn new(node_shape_uri: &str, target_class: &str) -> Self {
        let mut shape = serde_json::Map::new();
        shape.insert("node_shape_uri".into(), node_shape_uri.into());
        shape.insert("target_class".into(), target_class.into());
        Self {
            shape,
            properties: Vec::new(),
        }
    }

    /// `constructor_actions` — declared `call` actions only (a mounted
    /// class's lifecycle belongs to the language). `(interaction,
    /// source, target-params-json)` triples; constructors must name
    /// their `source` — `this` means nothing before the record exists.
    pub fn constructor_actions(&mut self, calls: &[(&str, &str, &str)]) -> &mut Self {
        self.shape
            .insert("constructor_actions".into(), call_actions(calls));
        self
    }

    /// `destructor_actions` — declared `call` actions; `source` is
    /// usually `this`.
    pub fn destructor_actions(&mut self, calls: &[(&str, &str, &str)]) -> &mut Self {
        self.shape
            .insert("destructor_actions".into(), call_actions(calls));
        self
    }

    /// One mounted property — the `properties` entry the contract reads.
    /// `path` is the predicate IRI; the rest are the optional fields the
    /// wire form knows. `setter_calls` are `(interaction, source,
    /// target-params)` triples; a setter marks the property writable.
    #[allow(clippy::too_many_arguments)]
    pub fn property(
        &mut self,
        name: &str,
        path: &str,
        required: bool,
        collection: bool,
        flag_value: Option<&str>,
        writable: bool,
        local: bool,
        node_kind: Option<&str>,
        relation_kind: Option<&str>,
        alias: Option<&str>,
        datatype: Option<&str>,
        setter_calls: &[(&str, &str, &str)],
        adder_calls: &[(&str, &str, &str)],
        remover_calls: &[(&str, &str, &str)],
    ) -> &mut Self {
        let mut p = serde_json::Map::new();
        p.insert("name".into(), name.into());
        p.insert("path".into(), path.into());
        if required {
            p.insert("min_count".into(), 1.into());
        }
        if collection {
            p.insert("collection".into(), true.into());
        } else {
            p.insert("max_count".into(), 1.into());
        }
        if let Some(v) = flag_value {
            p.insert("has_value".into(), v.into());
        }
        if writable {
            p.insert("writable".into(), true.into());
        }
        if local {
            p.insert("local".into(), true.into());
        }
        if let Some(v) = node_kind {
            p.insert("node_kind".into(), v.into());
        }
        if let Some(v) = relation_kind {
            p.insert("relation_kind".into(), v.into());
        }
        if let Some(v) = alias {
            p.insert("alias".into(), v.into());
        }
        if let Some(v) = datatype {
            p.insert("datatype".into(), v.into());
        }
        if !setter_calls.is_empty() {
            p.insert("setter".into(), call_actions(setter_calls));
        }
        if !adder_calls.is_empty() {
            p.insert("adder".into(), call_actions(adder_calls));
        }
        if !remover_calls.is_empty() {
            p.insert("remover".into(), call_actions(remover_calls));
        }
        self.properties.push(serde_json::Value::Object(p));
        self
    }

    /// `SHACLShape::toJSON()` — the shape with its `properties` list.
    pub fn finish(&self) -> serde_json::Value {
        let mut shape = self.shape.clone();
        shape.insert("properties".into(), self.properties.clone().into());
        serde_json::Value::Object(shape)
    }
}

/// Replace `{language}` with the mounting language's address in every
/// string of the shape. The placeholder only ever appears in call-action
/// `source`/`target` fields (a class is decorated before it knows which
/// language mounts it); the whole-shape walk is the Rust port of the JS
/// LDK's `resolveMountCallSources`, applied to every string so a
/// `{language}` in a `path` or `has_value` resolves too.
fn resolve_language_placeholder(value: &mut serde_json::Value, language_address: &str) {
    match value {
        serde_json::Value::String(s) => {
            if s.contains("{language}") {
                *s = s.replace("{language}", language_address);
            }
        }
        serde_json::Value::Array(items) => {
            for item in items {
                resolve_language_placeholder(item, language_address);
            }
        }
        serde_json::Value::Object(map) => {
            for v in map.values_mut() {
                resolve_language_placeholder(v, language_address);
            }
        }
        _ => {}
    }
}

impl MountShapes {
    /// The `mount.classes` form: build the `mountShapes()` envelope from
    /// each class's `generate_shacl()`. `node_shape_uri` is dropped (the
    /// executor's shape parser derives `{ns}{Class}Shape` from
    /// `target_class`, matching the hand-written convention) and
    /// `{language}` resolves against this language's own address — the
    /// `languageAddress()` host import.
    ///
    /// `from_classes` calls the host import, so it only runs inside a
    /// language runtime; tests and languages that already know their
    /// address use [`from_classes_at`](Self::from_classes_at).
    pub fn from_classes(version: impl Into<String>, classes: &[MountClassShape]) -> Self {
        Self::from_classes_at(version, classes, &crate::imports::language_address())
    }

    /// As [`from_classes`](Self::from_classes), with the language
    /// address given explicitly.
    pub fn from_classes_at(
        version: impl Into<String>,
        classes: &[MountClassShape],
        language_address: &str,
    ) -> Self {
        let shapes = classes
            .iter()
            .map(|c| {
                let mut shape = c.shape.clone();
                if let serde_json::Value::Object(ref mut map) = shape {
                    map.remove("node_shape_uri");
                }
                resolve_language_placeholder(&mut shape, language_address);
                MountShapeEntry {
                    name: c.name.clone(),
                    shape,
                }
            })
            .collect();
        Self {
            version: version.into(),
            shapes,
        }
    }
}
