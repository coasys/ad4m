//! Service interface documents.
//!
//! An interface version is one JSON document. Its hash is the AD4M content
//! address of its JCS canonical form. A module (all versions of one
//! interface) is identified by the hash of its first version (its genesis).
//! That hash covers the genesis `author`, so the module ID pins its author:
//! resolve the genesis to learn it, as a neighbourhood URL resolves to its
//! link language. Every later version must keep that author.

use std::collections::BTreeMap;

use cid::Cid;
use multibase::Base;
use multihash_codetable::{Code, MultihashDigest};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

/// The AD4M content address: SHA-256 → CIDv1 (raw) → base58btc, `Qm`-prefixed.
/// The same function addresses languages.
pub fn content_hash(data: &[u8]) -> String {
    let multihash = Code::Sha2_256.digest(data);
    let cid = Cid::new_v1(0x00, multihash);
    format!("Qm{}", multibase::encode(Base::Base58Btc, cid.to_bytes()))
}

/// `true` when `s` has the shape of a content address. Base58 holds no `.`,
/// so `<hash>.<name>` always splits at the first dot.
pub fn is_hash(s: &str) -> bool {
    s.len() > 10 && s.starts_with("Qm") && s.chars().all(|c| c.is_ascii_alphanumeric())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
pub enum Selection {
    /// Each user may prefer an implementation; the admin sets a default.
    PerUser,
    /// Only the admin default applies (one provider per executor).
    Executor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
pub enum Risk {
    Safe,
    Write,
    Spend,
    Admin,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ActionDef {
    pub label: String,
    #[serde(default)]
    pub description: String,
    pub risk: Risk,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ErrorDef {
    pub code: u16,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct StreamDef {
    pub event: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct MeterDef {
    pub operation: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct MethodDef {
    #[serde(default)]
    pub description: String,
    pub params: Value,
    pub result: Value,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub errors: BTreeMap<String, ErrorDef>,
    pub action: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub read: bool,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub long: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub stream: Option<StreamDef>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub meter: Option<MeterDef>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct EventDef {
    #[serde(default)]
    pub description: String,
    pub payload: Value,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub scope: Option<String>,
    pub action: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ProvisionDef {
    pub params: Value,
    #[serde(default = "empty_object_schema")]
    pub result: Value,
}

fn empty_object_schema() -> Value {
    serde_json::json!({ "type": "object" })
}

/// One interface version.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ServiceInterface {
    /// Display label only. Not unique.
    pub name: String,
    /// DID that signs every version of the module.
    pub author: String,
    /// Hash of the genesis version. Absent in the genesis itself.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module: Option<String>,
    /// Hash of the version this one succeeds. Absent in the genesis.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub previous: Option<String>,
    /// Metadata only: the module this one was forked from.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fork_of: Option<String>,
    pub version: String,
    #[serde(default)]
    pub description: String,
    pub selection: Selection,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub types: BTreeMap<String, Value>,
    #[serde(default)]
    pub methods: BTreeMap<String, MethodDef>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub events: BTreeMap<String, EventDef>,
    pub actions: BTreeMap<String, ActionDef>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub provision: Option<ProvisionDef>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub deprovision: Option<ProvisionDef>,
}

/// A parsed, structurally valid interface version with its hash.
#[derive(Debug, Clone)]
pub struct InterfaceDocument {
    pub doc: ServiceInterface,
    /// The document exactly as published; the hash covers this.
    pub raw: Value,
    pub hash: String,
    pub version: semver::Version,
}

impl InterfaceDocument {
    /// Parse, validate and hash a document.
    pub fn parse(raw: Value) -> Result<Self, String> {
        let doc: ServiceInterface =
            serde_json::from_value(raw.clone()).map_err(|e| format!("invalid interface: {}", e))?;
        let version = semver::Version::parse(&doc.version)
            .map_err(|e| format!("version `{}` is not semver: {}", doc.version, e))?;
        validate_structure(&doc)?;
        let canonical = serde_json_canonicalizer::to_vec(&raw)
            .map_err(|e| format!("cannot canonicalise interface: {}", e))?;
        Ok(Self {
            hash: content_hash(&canonical),
            doc,
            raw,
            version,
        })
    }

    /// The module ID: the genesis hash, which also fixes the author.
    pub fn module_id(&self) -> &str {
        self.doc.module.as_deref().unwrap_or(&self.hash)
    }

    pub fn is_genesis(&self) -> bool {
        self.doc.module.is_none()
    }

    /// The compatibility line of this version: the major for `>= 1.0.0`,
    /// `0.<minor>` below it (semver: anything may change in 0.y).
    pub fn compat(&self) -> String {
        compat_key(&self.version)
    }

    /// Check the author's signature over the hash (hex, as `agent.sign`
    /// produces it).
    pub fn verify_signature(&self, signature_hex: &str) -> bool {
        crate::agent::signatures::verify_string_signed_by_did(
            &self.doc.author,
            &self.hash,
            signature_hex,
        )
        .unwrap_or(false)
    }

    /// A method or event schema as a standalone JSON Schema: the interface's
    /// `types` become `$defs`, and `#/types/X` refs point at them.
    pub fn standalone_schema(&self, schema: &Value) -> Value {
        standalone_schema(schema, &self.doc.types)
    }
}

pub fn compat_key(v: &semver::Version) -> String {
    if v.major > 0 {
        v.major.to_string()
    } else {
        format!("0.{}", v.minor)
    }
}

pub(crate) fn standalone_schema(schema: &Value, types: &BTreeMap<String, Value>) -> Value {
    // Only the types `schema` reaches, directly or through other types.
    let mut reached: Vec<String> = Vec::new();
    let mut queue = vec![schema];
    while let Some(s) = queue.pop() {
        let mut found = Vec::new();
        refs(s, &mut found);
        for name in found.iter().filter_map(|r| r.strip_prefix("#/types/")) {
            if let Some(t) = types.get(name) {
                if !reached.iter().any(|n| n == name) {
                    reached.push(name.to_string());
                    queue.push(t);
                }
            }
        }
    }
    let mut out = rewrite_refs(schema);
    if !reached.is_empty() {
        if let Value::Object(map) = &mut out {
            let defs = map
                .entry("$defs")
                .or_insert_with(|| Value::Object(Default::default()));
            if let Value::Object(defs) = defs {
                for name in reached {
                    defs.insert(name.clone(), rewrite_refs(&types[&name]));
                }
            }
        }
    }
    out
}

fn rewrite_refs(v: &Value) -> Value {
    match v {
        Value::Object(map) => Value::Object(
            map.iter()
                .map(|(k, v)| match (k.as_str(), v) {
                    ("$ref", Value::String(r)) if r.starts_with("#/types/") => (
                        k.clone(),
                        Value::String(r.replacen("#/types/", "#/$defs/", 1)),
                    ),
                    _ => (k.clone(), rewrite_refs(v)),
                })
                .collect(),
        ),
        Value::Array(items) => Value::Array(items.iter().map(rewrite_refs).collect()),
        _ => v.clone(),
    }
}

/// Every `$ref` in `v`.
fn refs(v: &Value, out: &mut Vec<String>) {
    match v {
        Value::Object(map) => {
            for (k, v) in map {
                match (k.as_str(), v) {
                    ("$ref", Value::String(r)) => out.push(r.clone()),
                    _ => refs(v, out),
                }
            }
        }
        Value::Array(items) => items.iter().for_each(|v| refs(v, out)),
        _ => {}
    }
}

fn is_camel(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_lowercase()) && chars.all(|c| c.is_ascii_alphanumeric())
}

fn is_kebab(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_lowercase())
        && chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
        && !name.ends_with('-')
}

/// Codes the protocol itself answers with. A method error must
/// use another code so callers can tell them apart.
const RESERVED_CODES: &[u16] = &[400, 401, 402, 403, 404, 408, 500, 502, 503, 504];

fn object_properties(schema: &Value) -> Option<&serde_json::Map<String, Value>> {
    schema.get("properties").and_then(Value::as_object)
}

fn is_object_schema(schema: &Value) -> bool {
    schema.get("type").and_then(Value::as_str) == Some("object")
}

fn validate_structure(doc: &ServiceInterface) -> Result<(), String> {
    if doc.name.trim().is_empty() {
        return Err("`name` must not be empty".into());
    }
    if !doc.author.starts_with("did:") {
        return Err(format!("`author` must be a DID, got `{}`", doc.author));
    }
    match (&doc.module, &doc.previous) {
        (None, None) => {}
        (Some(m), Some(p)) if is_hash(m) && is_hash(p) => {}
        (Some(_), Some(_)) => return Err("`module` and `previous` must be hashes".into()),
        _ => {
            return Err("`module` and `previous` must both be set, or both absent (genesis)".into())
        }
    }
    if let Some(f) = &doc.fork_of {
        if !is_hash(f) {
            return Err("`forkOf` must be a module ID (a genesis hash)".into());
        }
    }
    for name in doc.types.keys() {
        if name.is_empty() || name.contains('/') {
            return Err(format!("invalid type name `{}`", name));
        }
    }
    for (name, a) in &doc.actions {
        if name.is_empty()
            || !name
                .chars()
                .all(|c| c.is_ascii_uppercase() || c == '_' || c.is_ascii_digit())
        {
            return Err(format!("action `{}` must be UPPER_SNAKE_CASE", name));
        }
        if a.label.trim().is_empty() {
            return Err(format!("action `{}` needs a label", name));
        }
    }
    let check_action = |owner: &str, action: &str| -> Result<(), String> {
        if doc.actions.contains_key(action) {
            Ok(())
        } else {
            Err(format!("{} names unknown action `{}`", owner, action))
        }
    };

    let mut schemas: Vec<(String, &Value)> = Vec::new();
    for (name, m) in &doc.methods {
        if !is_camel(name) {
            return Err(format!("method `{}` must be camelCase", name));
        }
        let owner = format!("method `{}`", name);
        check_action(&owner, &m.action)?;
        if !is_object_schema(&m.params) {
            return Err(format!("{}: params must be an object schema", owner));
        }
        // Closed params let a MINOR add optional params safely, and
        // make unknown params a 400 instead of silently ignored.
        if m.params.get("additionalProperties") != Some(&Value::Bool(false)) {
            return Err(format!(
                "{}: params must set `additionalProperties: false`",
                owner
            ));
        }
        for (err_name, e) in &m.errors {
            if !err_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
                || !err_name.chars().all(|c| c.is_ascii_alphanumeric())
            {
                return Err(format!(
                    "{}: error `{}` must be PascalCase",
                    owner, err_name
                ));
            }
            if !(400..600).contains(&e.code) || RESERVED_CODES.contains(&e.code) {
                return Err(format!(
                    "{}: error `{}` uses code {}; use a non-reserved 4xx/5xx code",
                    owner, err_name, e.code
                ));
            }
        }
        if let Some(s) = &m.stream {
            let ev = doc
                .events
                .get(&s.event)
                .ok_or_else(|| format!("{}: stream event `{}` is not declared", owner, s.event))?;
            if ev.scope.as_deref() != Some("streamId") {
                return Err(format!(
                    "{}: stream event `{}` must have scope `streamId`",
                    owner, s.event
                ));
            }
            let required = m.params.get("required").and_then(Value::as_array);
            if !object_properties(&m.params).is_some_and(|p| p.contains_key("streamId"))
                || !required.is_some_and(|r| r.iter().any(|v| v == "streamId"))
            {
                return Err(format!(
                    "{}: a streaming method takes a required `streamId` param",
                    owner
                ));
            }
        }
        schemas.push((format!("{} params", owner), &m.params));
        schemas.push((format!("{} result", owner), &m.result));
        for (err_name, e) in &m.errors {
            if let Some(d) = &e.data {
                schemas.push((format!("{} error `{}`", owner, err_name), d));
            }
        }
    }
    for (name, e) in &doc.events {
        if !is_kebab(name) {
            return Err(format!("event `{}` must be kebab-case", name));
        }
        let owner = format!("event `{}`", name);
        check_action(&owner, &e.action)?;
        if !is_object_schema(&e.payload) {
            return Err(format!("{}: payload must be an object schema", owner));
        }
        if let Some(scope) = &e.scope {
            let props = object_properties(&e.payload);
            let required = e.payload.get("required").and_then(Value::as_array);
            if !props.is_some_and(|p| p.contains_key(scope))
                || !required.is_some_and(|r| r.iter().any(|v| v == scope))
            {
                return Err(format!(
                    "{}: scope `{}` must be a required payload property",
                    owner, scope
                ));
            }
        }
        schemas.push((format!("{} payload", owner), &e.payload));
    }
    for (label, def) in [
        ("provision", &doc.provision),
        ("deprovision", &doc.deprovision),
    ] {
        if let Some(p) = def {
            schemas.push((format!("{} params", label), &p.params));
            schemas.push((format!("{} result", label), &p.result));
        }
    }
    for t in doc.types.values() {
        schemas.push(("type".into(), t));
    }

    for (owner, schema) in schemas {
        let mut found = Vec::new();
        refs(schema, &mut found);
        for r in found {
            let ok = r
                .strip_prefix("#/types/")
                .is_some_and(|name| doc.types.contains_key(name))
                || r.starts_with("#/$defs/")
                || r == "#";
            if !ok {
                return Err(format!(
                    "{}: `$ref` `{}` must point into `#/types/`",
                    owner, r
                ));
            }
        }
        jsonschema::draft202012::new(&standalone_schema(schema, &doc.types))
            .map_err(|e| format!("{}: invalid JSON Schema: {}", owner, e))?;
    }
    Ok(())
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use serde_json::json;

    pub(crate) fn genesis(author: &str) -> Value {
        json!({
            "name": "echo",
            "author": author,
            "version": "1.0.0",
            "description": "Test service.",
            "selection": "per-user",
            "types": { "Text": { "type": "string", "maxLength": 100 } },
            "methods": {
                "say": {
                    "description": "Say something in a room.",
                    "params": { "type": "object", "required": ["room", "text"], "additionalProperties": false,
                                "properties": { "room": { "type": "string" }, "text": { "$ref": "#/types/Text" } } },
                    "result": { "type": "object", "required": ["text"], "properties": { "text": { "type": "string" } } },
                    "errors": { "Muted": { "code": 409 } },
                    "action": "SAY",
                    "read": true
                },
                "count": {
                    "description": "Count up to `to`, streaming each number.",
                    "params": { "type": "object", "required": ["to", "streamId"], "additionalProperties": false,
                                "properties": { "to": { "type": "integer", "minimum": 1, "maximum": 10 }, "streamId": { "type": "string" } } },
                    "result": { "type": "object", "required": ["total"], "properties": { "total": { "type": "integer" } } },
                    "action": "SAY",
                    "long": true,
                    "stream": { "event": "count-tick" }
                }
            },
            "events": {
                "said": {
                    "description": "Text said in a room.",
                    "payload": { "type": "object", "required": ["room", "text"],
                                 "properties": { "room": { "type": "string" }, "text": { "type": "string" } } },
                    "scope": "room",
                    "action": "SAY"
                },
                "count-tick": {
                    "description": "One number of a count.",
                    "payload": { "type": "object", "required": ["streamId", "n"],
                                 "properties": { "streamId": { "type": "string" }, "n": { "type": "integer" } } },
                    "scope": "streamId",
                    "action": "SAY"
                }
            },
            "actions": { "SAY": { "label": "Say things", "description": "Echo text.", "risk": "safe" } }
        })
    }

    #[test]
    fn hash_ignores_key_order_and_whitespace() {
        let a = InterfaceDocument::parse(genesis("did:key:z6Mkx")).unwrap();
        let text = serde_json::to_string_pretty(&genesis("did:key:z6Mkx")).unwrap();
        let reordered: Value = serde_json::from_str(&text).unwrap();
        let b = InterfaceDocument::parse(reordered).unwrap();
        assert_eq!(a.hash, b.hash);
        assert!(is_hash(&a.hash));
        assert_eq!(a.module_id(), a.hash);
        assert_eq!(a.compat(), "1");
    }

    #[test]
    fn hash_changes_with_content() {
        let a = InterfaceDocument::parse(genesis("did:key:z6Mkx")).unwrap();
        let mut raw = genesis("did:key:z6Mkx");
        raw["description"] = json!("Changed.");
        assert_ne!(a.hash, InterfaceDocument::parse(raw).unwrap().hash);
    }

    #[test]
    fn compat_key_follows_semver_zero_rule() {
        assert_eq!(compat_key(&semver::Version::new(0, 3, 1)), "0.3");
        assert_eq!(compat_key(&semver::Version::new(2, 1, 0)), "2");
    }

    fn rejects(mutate: impl FnOnce(&mut Value), needle: &str) {
        let mut raw = genesis("did:key:z6Mkx");
        mutate(&mut raw);
        let err = InterfaceDocument::parse(raw).unwrap_err();
        assert!(
            err.contains(needle),
            "`{}` does not mention `{}`",
            err,
            needle
        );
    }

    #[test]
    fn structural_rules() {
        rejects(|r| r["unknown"] = json!(1), "unknown field");
        rejects(|r| r["version"] = json!("1.0"), "not semver");
        rejects(|r| r["author"] = json!("bob"), "must be a DID");
        rejects(|r| r["module"] = json!("QmAAAAAAAAAAAA"), "both be set");
        rejects(
            |r| r["methods"]["say"]["action"] = json!("NOPE"),
            "unknown action",
        );
        rejects(
            |r| r["methods"]["Say"] = r["methods"]["say"].clone(),
            "camelCase",
        );
        rejects(
            |r| r["events"]["Said"] = r["events"]["said"].clone(),
            "kebab-case",
        );
        rejects(
            |r| r["methods"]["say"]["params"] = json!({ "type": "string" }),
            "object schema",
        );
        rejects(
            |r| r["methods"]["say"]["params"]["additionalProperties"] = json!(true),
            "additionalProperties",
        );
        rejects(
            |r| r["methods"]["say"]["errors"]["Muted"]["code"] = json!(403),
            "reserved",
        );
        rejects(
            |r| {
                r["methods"]["say"]["params"]["properties"]["text"] =
                    json!({ "$ref": "#/types/Missing" })
            },
            "$ref",
        );
        rejects(|r| r["events"]["said"]["scope"] = json!("nope"), "scope");
        rejects(
            |r| r["methods"]["count"]["stream"] = json!({ "event": "said" }),
            "scope `streamId`",
        );
        rejects(
            |r| r["methods"]["say"]["result"] = json!({ "type": 5 }),
            "invalid JSON Schema",
        );
        rejects(
            |r| r["actions"]["say"] = json!({ "label": "x", "risk": "safe" }),
            "UPPER_SNAKE_CASE",
        );
    }

    #[test]
    fn standalone_schema_resolves_types() {
        let d = InterfaceDocument::parse(genesis("did:key:z6Mkx")).unwrap();
        let s = d.standalone_schema(&d.doc.methods["say"].params);
        let v = jsonschema::draft202012::new(&s).unwrap();
        assert!(v.is_valid(&json!({ "room": "a", "text": "hi" })));
        assert!(!v.is_valid(&json!({ "room": "a", "text": "x".repeat(101) })));
    }
}
