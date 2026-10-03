//! Strict semver between consecutive interface versions.
//!
//! The host checks every new version against its `previous`:
//! - PATCH: only annotations change (descriptions, titles, examples, labels).
//! - MINOR: only additive edits. The checker is deliberately conservative
//!   (full JSON Schema subset checking is undecidable in general): it proves a
//!   small set of edits safe and refuses the rest.
//! - MAJOR: anything; the host treats the new line as a separate interface.

use std::collections::BTreeMap;

use serde_json::{Map, Value};

use super::interface::{compat_key, ErrorDef, InterfaceDocument};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bump {
    Patch,
    Minor,
    Major,
}

/// Classify `next` against `prev` and check the rules of its bump.
pub fn check_successor(prev: &InterfaceDocument, next: &InterfaceDocument) -> Result<Bump, String> {
    if next.doc.author != prev.doc.author {
        return Err("a later version must have the same author".into());
    }
    if next.version <= prev.version {
        return Err(format!(
            "version {} does not follow {}",
            next.version, prev.version
        ));
    }
    if compat_key(&next.version) != compat_key(&prev.version) {
        return Ok(Bump::Major);
    }
    let bump =
        if next.version.major == prev.version.major && next.version.minor == prev.version.minor {
            Bump::Patch
        } else {
            Bump::Minor
        };
    match bump {
        Bump::Patch => check_patch(prev, next)?,
        _ => check_minor(prev, next)?,
    }
    Ok(bump)
}

/// Keys that carry no validation meaning.
const ANNOTATIONS: &[&str] = &["description", "title", "examples", "$comment", "label"];
/// Keys whose value maps names to definitions: the names are data, so a
/// property called `title` survives.
const NAME_MAPS: &[&str] = &[
    "properties",
    "patternProperties",
    "dependentSchemas",
    "$defs",
    "definitions",
    "types",
    "methods",
    "events",
    "actions",
    "errors",
];
/// Keys whose value is data, never schema.
const LITERALS: &[&str] = &["enum", "const", "default"];

fn strip_annotations(v: &Value) -> Value {
    strip(v, false)
}

/// `names`: the keys of `v` are names, not keywords.
fn strip(v: &Value, names: bool) -> Value {
    match v {
        Value::Object(map) => Value::Object(
            map.iter()
                .filter(|(k, _)| names || !ANNOTATIONS.contains(&k.as_str()))
                .map(|(k, v)| {
                    let child = if names {
                        strip(v, false)
                    } else if LITERALS.contains(&k.as_str()) {
                        v.clone()
                    } else {
                        strip(v, NAME_MAPS.contains(&k.as_str()))
                    };
                    (k.clone(), child)
                })
                .collect(),
        ),
        Value::Array(items) => Value::Array(items.iter().map(|i| strip(i, false)).collect()),
        _ => v.clone(),
    }
}

fn check_patch(prev: &InterfaceDocument, next: &InterfaceDocument) -> Result<(), String> {
    let strip = |d: &InterfaceDocument| {
        let mut v = strip_annotations(&d.raw);
        if let Value::Object(m) = &mut v {
            for k in ["version", "previous", "module", "name"] {
                m.remove(k);
            }
        }
        v
    };
    if strip(prev) == strip(next) {
        Ok(())
    } else {
        Err(format!(
            "{} is a PATCH of {} but changes more than annotations",
            next.version, prev.version
        ))
    }
}

fn check_minor(prev: &InterfaceDocument, next: &InterfaceDocument) -> Result<(), String> {
    let (p, n) = (&prev.doc, &next.doc);
    let fail = |what: String| {
        Err(format!(
            "{} is a MINOR of {}: {}",
            next.version, prev.version, what
        ))
    };
    if p.selection != n.selection {
        return fail("`selection` changed".into());
    }
    for (name, a) in &p.actions {
        match n.actions.get(name) {
            Some(b) if b.risk == a.risk => {}
            Some(_) => return fail(format!("action `{}` changed its risk", name)),
            None => return fail(format!("action `{}` removed", name)),
        }
    }
    let types = Types {
        old: &p.types,
        new: &n.types,
    };
    for (name, m) in &p.methods {
        let Some(nm) = n.methods.get(name) else {
            return fail(format!("method `{}` removed", name));
        };
        if nm.action != m.action {
            return fail(format!("method `{}` changed its action", name));
        }
        if nm.stream != m.stream {
            return fail(format!("method `{}` changed its stream", name));
        }
        if !types.widens(&m.params, &nm.params, 0) {
            return fail(format!("method `{}` params accept less than before", name));
        }
        if !types.narrows(&m.result, &nm.result, 0) {
            return fail(format!(
                "method `{}` result can hold values the old result schema refuses",
                name
            ));
        }
        for (err, e) in &m.errors {
            let data_narrows = |ne: &ErrorDef| match (&e.data, &ne.data) {
                (None, _) => true,
                (Some(o), Some(n)) => types.narrows(o, n, 0),
                (Some(_), None) => false,
            };
            match nm.errors.get(err) {
                Some(ne) if ne.code == e.code && data_narrows(ne) => {}
                _ => {
                    return fail(format!(
                        "method `{}` error `{}` removed, recoded or widened",
                        name, err
                    ))
                }
            }
        }
    }
    for (name, e) in &p.events {
        let Some(ne) = n.events.get(name) else {
            return fail(format!("event `{}` removed", name));
        };
        if ne.action != e.action || ne.scope != e.scope {
            return fail(format!("event `{}` changed its action or scope", name));
        }
        if !types.narrows(&e.payload, &ne.payload, 0) {
            return fail(format!(
                "event `{}` payload can hold values old consumers refuse",
                name
            ));
        }
    }
    for (label, old, new) in [
        ("provision", &p.provision, &n.provision),
        ("deprovision", &p.deprovision, &n.deprovision),
    ] {
        match (old, new) {
            (None, _) => {}
            (Some(_), None) => return fail(format!("`{}` removed", label)),
            (Some(o), Some(nw)) => {
                if !types.widens(&o.params, &nw.params, 0)
                    || !types.narrows(&o.result, &nw.result, 0)
                {
                    return fail(format!("`{}` changed incompatibly", label));
                }
            }
        }
    }
    Ok(())
}

/// Schema comparison with `#/types/` refs resolved on each side.
struct Types<'a> {
    old: &'a BTreeMap<String, Value>,
    new: &'a BTreeMap<String, Value>,
}

const MAX_DEPTH: usize = 32;

impl Types<'_> {
    fn resolve<'v>(&'v self, v: &'v Value, old: bool) -> &'v Value {
        let types = if old { self.old } else { self.new };
        let mut cur = v;
        for _ in 0..MAX_DEPTH {
            // A `$ref` with sibling keywords stays as is: following it would
            // drop the siblings' constraints.
            let bare = cur.as_object().is_some_and(|m| {
                m.keys()
                    .all(|k| k == "$ref" || ANNOTATIONS.contains(&k.as_str()))
            });
            match Some(cur)
                .filter(|_| bare)
                .and_then(|c| c.get("$ref"))
                .and_then(Value::as_str)
                .and_then(|r| r.strip_prefix("#/types/"))
                .and_then(|name| types.get(name))
            {
                Some(t) => cur = t,
                None => break,
            }
        }
        cur
    }

    fn same(&self, a: &Value, b: &Value, depth: usize) -> bool {
        self.widens(a, b, depth) && self.narrows(a, b, depth)
    }

    /// Does `new` accept every value `old` accepts? (params: callers built
    /// against `old` must stay valid.)
    fn widens(&self, old: &Value, new: &Value, depth: usize) -> bool {
        self.compare(old, new, depth, true)
    }

    /// Does `old` accept every value `new` accepts? (results and events:
    /// consumers built against `old` must accept them.)
    fn narrows(&self, old: &Value, new: &Value, depth: usize) -> bool {
        self.compare(old, new, depth, false)
    }

    fn compare(&self, old: &Value, new: &Value, depth: usize, widening: bool) -> bool {
        if depth > MAX_DEPTH {
            return false;
        }
        let (o, n) = (
            strip_annotations(self.resolve(old, true)),
            strip_annotations(self.resolve(new, false)),
        );
        if o == n && !contains_ref(&o) {
            return true;
        }
        let (Some(om), Some(nm)) = (o.as_object(), n.as_object()) else {
            return false;
        };
        if om.get("type") != nm.get("type") {
            return false;
        }
        if om.get("type").and_then(Value::as_str) == Some("object") {
            return self.compare_objects(om, nm, depth, widening);
        }
        if om.get("type").and_then(Value::as_str) == Some("array") {
            if !same_except(om, nm, &["items"]) {
                return false;
            }
            return match (om.get("items"), nm.get("items")) {
                (Some(a), Some(b)) => self.compare(a, b, depth + 1, widening),
                (None, None) => true,
                _ => false,
            };
        }
        if let (Some(oe), Some(ne)) = (
            om.get("enum").and_then(Value::as_array),
            nm.get("enum").and_then(Value::as_array),
        ) {
            if !same_except(om, nm, &["enum"]) {
                return false;
            }
            let (small, big) = if widening { (oe, ne) } else { (ne, oe) };
            return small.iter().all(|v| big.contains(v));
        }
        false
    }

    fn compare_objects(
        &self,
        om: &Map<String, Value>,
        nm: &Map<String, Value>,
        depth: usize,
        widening: bool,
    ) -> bool {
        if !same_except(om, nm, &["properties", "required", "additionalProperties"]) {
            return false;
        }
        let empty = Map::new();
        let op = om
            .get("properties")
            .and_then(Value::as_object)
            .unwrap_or(&empty);
        let np = nm
            .get("properties")
            .and_then(Value::as_object)
            .unwrap_or(&empty);
        let req = |m: &Map<String, Value>| -> Vec<Value> {
            m.get("required")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default()
        };
        let (oreq, nreq) = (req(om), req(nm));
        let closed =
            |m: &Map<String, Value>| m.get("additionalProperties") == Some(&Value::Bool(false));
        let (oclosed, nclosed) = (closed(om), closed(nm));
        // Extras checked by a schema, a pattern or `unevaluatedProperties`
        // depend on the property set: demand the same shape.
        let schema_extras = |m: &Map<String, Value>| {
            m.get("additionalProperties").is_some_and(Value::is_object)
                || m.contains_key("patternProperties")
                || m.contains_key("unevaluatedProperties")
        };
        if schema_extras(om) || schema_extras(nm) {
            let ap = om.get("additionalProperties");
            return ap == nm.get("additionalProperties")
                && !ap.is_some_and(contains_ref)
                && op.len() == np.len()
                && op
                    .iter()
                    .all(|(k, v)| np.get(k).is_some_and(|nv| self.same(v, nv, depth + 1)))
                && oreq == nreq;
        }
        if widening {
            // New may drop requirements and add optional properties.
            if !nreq.iter().all(|r| oreq.contains(r)) {
                return false;
            }
            if nclosed && !oclosed {
                return false;
            }
            // A property new adds must not reject values old let through as extras.
            if !oclosed && np.keys().any(|k| !op.contains_key(k)) {
                return false;
            }
            op.iter().all(|(k, v)| {
                np.get(k)
                    .is_some_and(|nv| self.compare(v, nv, depth + 1, true))
            })
        } else {
            // Every value new accepts must pass old.
            if !oreq.iter().all(|r| nreq.contains(r)) {
                return false;
            }
            if oclosed && !nclosed {
                return false;
            }
            if oclosed && np.keys().any(|k| !op.contains_key(k)) {
                return false;
            }
            op.iter().all(|(k, v)| match np.get(k) {
                Some(nv) => self.compare(v, nv, depth + 1, false),
                // Old constrains `k`, new would let any value through as an extra.
                None => nclosed,
            })
        }
    }
}

/// Equal outside `except`, with no `$ref` there: a ref's target may differ
/// between the versions.
fn same_except(a: &Map<String, Value>, b: &Map<String, Value>, except: &[&str]) -> bool {
    let rest = |m: &Map<String, Value>| -> Map<String, Value> {
        m.iter()
            .filter(|(k, _)| !except.contains(&k.as_str()))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    };
    let ra = rest(a);
    ra == rest(b) && !ra.values().any(contains_ref)
}

fn contains_ref(v: &Value) -> bool {
    match v {
        Value::Object(m) => m.contains_key("$ref") || m.values().any(contains_ref),
        Value::Array(items) => items.iter().any(contains_ref),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::interface::tests::genesis;
    use serde_json::json;

    const AUTHOR: &str = "did:key:z6Mkx";

    fn doc(raw: Value) -> InterfaceDocument {
        InterfaceDocument::parse(raw).unwrap()
    }

    fn successor(
        prev: &InterfaceDocument,
        version: &str,
        mutate: impl FnOnce(&mut Value),
    ) -> InterfaceDocument {
        let mut raw = prev.raw.clone();
        raw["version"] = json!(version);
        raw["module"] = json!(prev.module_id());
        raw["previous"] = json!(prev.hash);
        mutate(&mut raw);
        doc(raw)
    }

    #[test]
    fn patch_allows_annotations_only() {
        let g = doc(genesis(AUTHOR));
        let ok = successor(&g, "1.0.1", |r| {
            r["description"] = json!("Better words.");
            r["methods"]["say"]["description"] = json!("Say it.");
            r["actions"]["SAY"]["label"] = json!("Echo");
        });
        assert_eq!(check_successor(&g, &ok), Ok(Bump::Patch));
        let bad = successor(&g, "1.0.1", |r| {
            r["types"]["Text"]["maxLength"] = json!(200)
        });
        assert!(check_successor(&g, &bad).unwrap_err().contains("PATCH"));
    }

    #[test]
    fn minor_allows_additions() {
        let g = doc(genesis(AUTHOR));
        let ok = successor(&g, "1.1.0", |r| {
            r["methods"]["shout"] = r["methods"]["say"].clone();
            r["methods"]["say"]["params"]["properties"]["loud"] = json!({ "type": "boolean" });
            r["methods"]["say"]["result"]["properties"]["echoes"] = json!({ "type": "integer" });
            r["events"]["shouted"] = r["events"]["said"].clone();
            r["actions"]["SHOUT"] = json!({ "label": "Shout", "risk": "write" });
            r["methods"]["say"]["errors"]["Hoarse"] = json!({ "code": 422 });
        });
        assert_eq!(check_successor(&g, &ok), Ok(Bump::Minor));
    }

    #[test]
    fn minor_refuses_breaking_edits() {
        let g = doc(genesis(AUTHOR));
        let cases: Vec<(&str, Box<dyn FnOnce(&mut Value)>)> = vec![
            (
                "removed",
                Box::new(|r: &mut Value| {
                    r["methods"].as_object_mut().unwrap().remove("count");
                }),
            ),
            (
                "action",
                Box::new(|r: &mut Value| {
                    r["actions"]["OTHER"] = json!({ "label": "o", "risk": "safe" });
                    r["methods"]["say"]["action"] = json!("OTHER");
                }),
            ),
            (
                "params",
                Box::new(|r: &mut Value| {
                    r["methods"]["say"]["params"]["required"] = json!(["room", "text", "extra"]);
                    r["methods"]["say"]["params"]["properties"]["extra"] =
                        json!({ "type": "string" });
                }),
            ),
            (
                "result",
                Box::new(|r: &mut Value| {
                    r["methods"]["say"]["result"]["required"] = json!([]);
                }),
            ),
            (
                "risk",
                Box::new(|r: &mut Value| {
                    r["actions"]["SAY"]["risk"] = json!("write");
                }),
            ),
            (
                "selection",
                Box::new(|r: &mut Value| {
                    r["selection"] = json!("executor");
                }),
            ),
            (
                "event",
                Box::new(|r: &mut Value| {
                    r["events"]["said"]["payload"]["properties"]["text"] =
                        json!({ "type": "integer" });
                }),
            ),
            (
                "error",
                Box::new(|r: &mut Value| {
                    r["methods"]["say"]["errors"]["Muted"]["code"] = json!(410);
                }),
            ),
        ];
        for (needle, mutate) in cases {
            let next = successor(&g, "1.1.0", mutate);
            let err = check_successor(&g, &next).unwrap_err();
            assert!(err.contains("MINOR"), "{}: {}", needle, err);
        }
    }

    #[test]
    fn annotation_names_used_as_data_still_count() {
        let mut raw = genesis(AUTHOR);
        raw["methods"]["say"]["params"]["properties"]["title"] = json!({ "type": "string" });
        raw["methods"]["say"]["result"]["properties"]["label"] =
            json!({ "const": { "title": "a" } });
        let g = doc(raw);
        let retyped = successor(&g, "1.0.1", |r| {
            r["methods"]["say"]["params"]["properties"]["title"] = json!({ "type": "integer" })
        });
        assert!(check_successor(&g, &retyped).is_err());
        let reconst = successor(&g, "1.0.1", |r| {
            r["methods"]["say"]["result"]["properties"]["label"] =
                json!({ "const": { "title": "b" } })
        });
        assert!(check_successor(&g, &reconst).is_err());
        let renamed = successor(&g, "1.1.0", |r| {
            r["methods"]["say"]["params"]["properties"]
                .as_object_mut()
                .unwrap()
                .remove("title");
        });
        assert!(check_successor(&g, &renamed).is_err());
    }

    #[test]
    fn minor_sees_through_refs_only_when_sound() {
        let mut raw = genesis(AUTHOR);
        raw["methods"]["say"]["result"]["properties"]["text"] =
            json!({ "$ref": "#/types/Text", "minLength": 1 });
        raw["types"]["Tag"] = json!({ "type": "string" });
        raw["methods"]["say"]["params"]["properties"]["tags"] = json!({
            "type": "array", "items": { "type": "string" }, "contains": { "$ref": "#/types/Tag" }
        });
        let g = doc(raw);
        // The sibling `minLength` goes away: results may now be empty.
        let sibling = successor(&g, "1.1.0", |r| {
            r["methods"]["say"]["result"]["properties"]["text"] = json!({ "$ref": "#/types/Text" })
        });
        assert!(check_successor(&g, &sibling).is_err());
        // The type behind an unchanged `contains` ref narrows: params accept less.
        let behind = successor(&g, "1.1.0", |r| r["types"]["Tag"]["maxLength"] = json!(2));
        assert!(check_successor(&g, &behind).is_err());
    }

    #[test]
    fn minor_refuses_wider_error_data() {
        let mut raw = genesis(AUTHOR);
        raw["methods"]["say"]["errors"]["Muted"]["data"] = json!({ "type": "object", "required": ["until"], "properties": { "until": { "type": "integer" } } });
        let g = doc(raw);
        let dropped = successor(&g, "1.1.0", |r| {
            r["methods"]["say"]["errors"]["Muted"]
                .as_object_mut()
                .unwrap()
                .remove("data");
        });
        assert!(check_successor(&g, &dropped).is_err());
    }

    #[test]
    fn enums_widen_in_params_and_narrow_in_results() {
        let mut raw = genesis(AUTHOR);
        raw["methods"]["say"]["params"]["properties"]["room"] =
            json!({ "type": "string", "enum": ["a", "b"] });
        raw["methods"]["say"]["result"]["properties"]["text"] =
            json!({ "type": "string", "enum": ["x", "y"] });
        let g = doc(raw);
        let ok = successor(&g, "1.1.0", |r| {
            r["methods"]["say"]["params"]["properties"]["room"]["enum"] = json!(["a", "b", "c"]);
            r["methods"]["say"]["result"]["properties"]["text"]["enum"] = json!(["x"]);
        });
        assert_eq!(check_successor(&g, &ok), Ok(Bump::Minor));
        let bad = successor(&g, "1.1.0", |r| {
            r["methods"]["say"]["result"]["properties"]["text"]["enum"] = json!(["x", "y", "z"]);
        });
        assert!(check_successor(&g, &bad).is_err());
    }

    #[test]
    fn major_allows_anything_and_order_is_enforced() {
        let g = doc(genesis(AUTHOR));
        let major = successor(&g, "2.0.0", |r| {
            r["methods"].as_object_mut().unwrap().remove("count");
            r["events"].as_object_mut().unwrap().remove("count-tick");
        });
        assert_eq!(check_successor(&g, &major), Ok(Bump::Major));
        let back = successor(&g, "0.9.0", |_| {});
        assert!(check_successor(&g, &back)
            .unwrap_err()
            .contains("does not follow"));
        let other = successor(&g, "1.0.1", |r| r["author"] = json!("did:key:z6Mky"));
        assert!(check_successor(&g, &other).unwrap_err().contains("author"));
    }

    #[test]
    fn zero_minor_is_a_new_line() {
        let mut raw = genesis(AUTHOR);
        raw["version"] = json!("0.1.0");
        let g = doc(raw);
        let next = successor(&g, "0.2.0", |r| {
            r["methods"].as_object_mut().unwrap().remove("say");
        });
        assert_eq!(check_successor(&g, &next), Ok(Bump::Major));
    }
}
