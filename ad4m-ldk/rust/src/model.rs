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
        alias: &[(&str, &str)],
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
        if !alias.is_empty() {
            // The TS `Record<string, string>` — the executor's
            // `PropertyShape.alias: Option<BTreeMap<String, String>>`.
            let obj: serde_json::Map<String, serde_json::Value> = alias
                .iter()
                .map(|(k, v)| ((*k).to_string(), (*v).into()))
                .collect();
            p.insert("alias".into(), obj.into());
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

/// Replace `{language}` with the mounting language's address — in a
/// call action's `source`/`target` only, the same fields the JS LDK's
/// `resolveMountCallSources` touches (a class is decorated before it
/// knows which language mounts it, so `{language}` defers the address
/// to `mountShapes()` time). A `{language}` inside a `name`, `path` or
/// `has_value` is a literal the shape keeps.
fn resolve_action_list(list: &mut serde_json::Value, language_address: &str) {
    if let serde_json::Value::Array(actions) = list {
        for action in actions {
            if let serde_json::Value::Object(map) = action {
                for key in ["source", "target"] {
                    if let Some(serde_json::Value::String(s)) = map.get_mut(key) {
                        if s.contains("{language}") {
                            *s = s.replace("{language}", language_address);
                        }
                    }
                }
            }
        }
    }
}

/// The node-shape URI the executor stores when `node_shape_uri` is absent:
/// `{extract_namespace(target_class)}{class_name_of(target_class)}Shape`
/// (`shacl_parser::shape_links`, called from `register_shapes` with
/// `class_name_of(target_class)`). `mount_types.rs` does not derive it.
/// `None` when the parser would refuse the target class for having no
/// namespace — the parent link is then left as declared.
fn derived_mount_shape_uri(target_class: &str) -> Option<String> {
    let scheme_pos = target_class.find("://");
    let mut namespace: Option<&str> = None;
    if let Some(scheme_pos) = scheme_pos {
        let after = &target_class[scheme_pos + 3..];
        if !after.contains('/') && !after.contains('#') {
            namespace = Some(&target_class[..scheme_pos + 3]);
        }
    }
    if namespace.is_none() {
        if let Some(hash_pos) = target_class.rfind('#') {
            namespace = Some(&target_class[..=hash_pos]);
        }
    }
    if namespace.is_none() {
        if let Some(scheme_pos) = scheme_pos {
            let after = &target_class[scheme_pos + 3..];
            if let Some(last_slash) = after.rfind('/') {
                namespace = Some(&target_class[..scheme_pos + 3 + last_slash + 1]);
            }
        }
    }
    let namespace = namespace?;
    let local_at = ['#', ':', '/']
        .into_iter()
        .filter_map(|delimiter| target_class.rfind(delimiter))
        .max()?;
    let local = &target_class[local_at + 1..];
    if local.is_empty() {
        return None;
    }
    Some(format!("{namespace}{local}Shape"))
}

fn rewrite_parent_shapes(
    shape: &mut serde_json::Value,
    declared_to_derived: &std::collections::BTreeMap<String, String>,
) {
    let Some(parents) = shape
        .get_mut("parent_shapes")
        .and_then(|v| v.as_array_mut())
    else {
        return;
    };
    for uri in parents {
        if let Some(current) = uri.as_str() {
            if let Some(derived) = declared_to_derived.get(current) {
                *uri = serde_json::Value::String(derived.clone());
            }
        }
    }
}

fn resolve_language_placeholder(shape: &mut serde_json::Value, language_address: &str) {
    let serde_json::Value::Object(map) = shape else {
        return;
    };
    for key in ["constructor_actions", "destructor_actions"] {
        if let Some(list) = map.get_mut(key) {
            resolve_action_list(list, language_address);
        }
    }
    if let Some(serde_json::Value::Array(props)) = map.get_mut("properties") {
        for prop in props {
            if let serde_json::Value::Object(pmap) = prop {
                for key in ["setter", "adder", "remover"] {
                    if let Some(list) = pmap.get_mut(key) {
                        resolve_action_list(list, language_address);
                    }
                }
            }
        }
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
    /// `from_classes` calls the host import — but only when a shape
    /// actually carries `{language}`, the same laziness the JS LDK's
    /// `mountShapes()` uses, so a class with no placeholder never needs
    /// a runtime. Tests and languages that already know their address
    /// use [`from_classes_at`](Self::from_classes_at).
    pub fn from_classes(version: impl Into<String>, classes: &[MountClassShape]) -> Self {
        let address = classes
            .iter()
            .any(|c| c.shape.to_string().contains("{language}"))
            .then(crate::imports::language_address)
            .unwrap_or_default();
        Self::from_classes_at(version, classes, &address)
    }

    /// As [`from_classes`](Self::from_classes), with the language
    /// address given explicitly.
    pub fn from_classes_at(
        version: impl Into<String>,
        classes: &[MountClassShape],
        language_address: &str,
    ) -> Self {
        let mut declared_to_derived = std::collections::BTreeMap::new();
        let mut shapes: Vec<MountShapeEntry> = classes
            .iter()
            .map(|c| {
                let mut shape = c.shape.clone();
                if let serde_json::Value::Object(ref mut map) = shape {
                    // Record the declared URI before dropping it. Parent links
                    // still name that URI; the executor stores the parent at
                    // the URI derived from its `target_class`.
                    if let (
                        Some(serde_json::Value::String(declared)),
                        Some(serde_json::Value::String(target_class)),
                    ) = (
                        map.get("node_shape_uri").cloned(),
                        map.get("target_class").cloned(),
                    ) {
                        if let Some(derived) = derived_mount_shape_uri(&target_class) {
                            declared_to_derived.insert(declared, derived);
                        }
                    }
                    map.remove("node_shape_uri");
                }
                resolve_language_placeholder(&mut shape, language_address);
                MountShapeEntry {
                    name: c.name.clone(),
                    shape,
                }
            })
            .collect();
        for entry in &mut shapes {
            rewrite_parent_shapes(&mut entry.shape, &declared_to_derived);
        }
        Self {
            version: version.into(),
            shapes,
        }
    }
}
