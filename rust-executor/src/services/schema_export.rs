//! Built-in interfaces from Rust types (SPEC §6.1). For a builtin the Rust
//! `params` / `result` / payload types stay the source of truth; this
//! builder turns them into an interface document with `schemars`. A unit
//! test per builtin compares the result with the checked-in document, the
//! way #1193's test guards `RpcMethods.ts`.

use schemars::{generate::SchemaSettings, JsonSchema, SchemaGenerator};
use serde_json::{json, Map, Value};

use super::interface::{Risk, Selection};

#[derive(Debug, Clone, Default)]
pub struct MethodOptions {
    pub read: bool,
    pub long: bool,
    /// The event a streaming method's chunks arrive as.
    pub stream: Option<&'static str>,
    pub tool: Option<bool>,
    /// `(name, code)` of declared method errors.
    pub errors: Vec<(&'static str, u16)>,
}

pub struct InterfaceBuilder {
    generator: SchemaGenerator,
    doc: Map<String, Value>,
}

impl InterfaceBuilder {
    pub fn new(name: &str, author: &str, version: &str, selection: Selection, description: &str) -> Self {
        let mut doc = Map::new();
        doc.insert("name".into(), json!(name));
        doc.insert("author".into(), json!(author));
        doc.insert("version".into(), json!(version));
        doc.insert("description".into(), json!(description));
        doc.insert("selection".into(), serde_json::to_value(selection).expect("selection serialises"));
        for k in ["methods", "events", "actions"] {
            doc.insert(k.into(), Value::Object(Map::new()));
        }
        Self {
            generator: SchemaSettings::draft2020_12().into_generator(),
            doc,
        }
    }

    /// Mark this version as the successor of `previous` in `module`.
    pub fn successor(mut self, module: &str, previous: &str) -> Self {
        self.doc.insert("module".into(), json!(module));
        self.doc.insert("previous".into(), json!(previous));
        self
    }

    pub fn action(mut self, name: &str, label: &str, description: &str, risk: Risk) -> Self {
        self.section("actions").insert(
            name.into(),
            json!({ "label": label, "description": description, "risk": risk }),
        );
        self
    }

    pub fn method<P: JsonSchema, R: JsonSchema>(
        mut self,
        name: &str,
        action: &str,
        description: &str,
        options: MethodOptions,
    ) -> Self {
        let params = self.object_schema::<P>();
        let result = self.generator.subschema_for::<R>().to_value();
        let mut m = json!({ "description": description, "params": params, "result": result, "action": action });
        if options.read {
            m["read"] = json!(true);
        }
        if options.long {
            m["long"] = json!(true);
        }
        if let Some(e) = options.stream {
            m["stream"] = json!({ "event": e });
        }
        if let Some(t) = options.tool {
            m["tool"] = json!(t);
        }
        if !options.errors.is_empty() {
            m["errors"] = Value::Object(
                options.errors.iter().map(|(n, c)| (n.to_string(), json!({ "code": c }))).collect(),
            );
        }
        self.section("methods").insert(name.into(), m);
        self
    }

    pub fn event<T: JsonSchema>(mut self, name: &str, action: &str, description: &str, scope: Option<&str>) -> Self {
        let payload = self.object_schema::<T>();
        let mut e = json!({ "description": description, "payload": payload, "action": action });
        if let Some(s) = scope {
            e["scope"] = json!(s);
        }
        self.section("events").insert(name.into(), e);
        self
    }

    /// The finished document: shared definitions become `types`, and every
    /// `#/$defs/X` ref becomes `#/types/X`.
    pub fn build(mut self) -> Value {
        let defs = self.generator.take_definitions(true);
        let mut doc = Value::Object(self.doc);
        if !defs.is_empty() {
            doc["types"] = Value::Object(defs);
        }
        clean(&mut doc);
        doc
    }

    fn section(&mut self, key: &str) -> &mut Map<String, Value> {
        self.doc.get_mut(key).and_then(Value::as_object_mut).expect("section exists")
    }

    /// The schema of `T` inlined (params and payloads must be object schemas,
    /// not refs).
    fn object_schema<T: JsonSchema>(&mut self) -> Value {
        let s = self.generator.subschema_for::<T>().to_value();
        let Some(name) = s.get("$ref").and_then(Value::as_str).and_then(|r| r.strip_prefix("#/$defs/")) else {
            return s;
        };
        // A params / payload struct is not a shared type: move it inline.
        let name = name.to_string();
        self.generator.definitions_mut().remove(&name).unwrap_or(s)
    }
}

/// `#/$defs/` → `#/types/`, and drop `$schema`.
fn clean(v: &mut Value) {
    match v {
        Value::Object(map) => {
            map.remove("$schema");
            for (k, v) in map.iter_mut() {
                if k == "$ref" {
                    if let Value::String(r) = v {
                        *r = r.replacen("#/$defs/", "#/types/", 1);
                    }
                } else {
                    clean(v);
                }
            }
        }
        Value::Array(items) => items.iter_mut().for_each(clean),
        _ => {}
    }
}
