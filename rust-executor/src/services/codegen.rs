//! Generated surfaces from one interface document: TypeScript
//! client types, MCP tool descriptors and a Markdown reference page.

use std::collections::BTreeMap;
use std::fmt::Write;

use serde_json::{json, Value};

use super::interface::{InterfaceDocument, Risk};

/// `ai.inference` + `1.2.0` → `AiInference_1_2_0`.
pub fn ident(doc: &InterfaceDocument) -> String {
    let mut out = pascal(&doc.doc.name);
    if out.is_empty() || out.starts_with(|c: char| c.is_ascii_digit()) {
        out.insert(0, 'S');
    }
    format!("{}_{}", out, doc.doc.version.replace(['.', '-', '+'], "_"))
}

fn pascal(name: &str) -> String {
    let mut out = String::new();
    let mut upper = true;
    for c in name.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(if upper { c.to_ascii_uppercase() } else { c });
            upper = false;
        } else {
            upper = true;
        }
    }
    out
}

fn quote(s: &str) -> String {
    serde_json::to_string(s).expect("string serialises")
}

fn property_key(k: &str) -> String {
    if !k.is_empty()
        && k.chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
        && !k.starts_with(|c: char| c.is_ascii_digit())
    {
        k.to_string()
    } else {
        quote(k)
    }
}

fn doc_comment(out: &mut String, indent: &str, text: &str) {
    let text = text.trim();
    if text.is_empty() {
        return;
    }
    let _ = writeln!(
        out,
        "{}/** {} */",
        indent,
        text.replace("*/", "*\\/").replace('\n', " ")
    );
}

/// A TypeScript type for a JSON Schema. `types` maps `#/types/X` to the
/// emitted type name. Unknown or unsupported constructs become `unknown`.
fn ts_type(schema: &Value, types: &BTreeMap<String, String>, depth: usize) -> String {
    if depth > 24 {
        return "unknown".into();
    }
    let Some(s) = schema.as_object() else {
        return match schema {
            Value::Bool(false) => "never".into(),
            _ => "unknown".into(),
        };
    };
    if let Some(r) = s.get("$ref").and_then(Value::as_str) {
        return r
            .strip_prefix("#/types/")
            .and_then(|n| types.get(n))
            .cloned()
            .unwrap_or_else(|| "unknown".into());
    }
    if let Some(c) = s.get("const") {
        return c.to_string();
    }
    if let Some(e) = s.get("enum").and_then(Value::as_array) {
        return e
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" | ");
    }
    for (key, sep) in [("anyOf", " | "), ("oneOf", " | "), ("allOf", " & ")] {
        if let Some(items) = s.get(key).and_then(Value::as_array) {
            return items
                .iter()
                .map(|i| format!("({})", ts_type(i, types, depth + 1)))
                .collect::<Vec<_>>()
                .join(sep);
        }
    }
    let one = |t: &str| -> String {
        match t {
            "string" => "string".into(),
            "integer" | "number" => "number".into(),
            "boolean" => "boolean".into(),
            "null" => "null".into(),
            "array" => {
                let items = s
                    .get("items")
                    .map(|i| ts_type(i, types, depth + 1))
                    .unwrap_or_else(|| "unknown".into());
                format!("Array<{}>", items)
            }
            "object" => ts_object(s, types, depth),
            _ => "unknown".into(),
        }
    };
    match s.get("type") {
        Some(Value::String(t)) => one(t),
        Some(Value::Array(ts)) => ts
            .iter()
            .filter_map(Value::as_str)
            .map(one)
            .collect::<Vec<_>>()
            .join(" | "),
        _ if s.contains_key("properties") => ts_object(s, types, depth),
        _ => "unknown".into(),
    }
}

fn ts_object(
    s: &serde_json::Map<String, Value>,
    types: &BTreeMap<String, String>,
    depth: usize,
) -> String {
    let required: Vec<&str> = s
        .get("required")
        .and_then(Value::as_array)
        .map(|r| r.iter().filter_map(Value::as_str).collect())
        .unwrap_or_default();
    let mut fields = Vec::new();
    if let Some(props) = s.get("properties").and_then(Value::as_object) {
        for (k, v) in props {
            let opt = if required.contains(&k.as_str()) {
                ""
            } else {
                "?"
            };
            fields.push(format!(
                "{}{}: {}",
                property_key(k),
                opt,
                ts_type(v, types, depth + 1)
            ));
        }
    }
    match s.get("additionalProperties") {
        Some(Value::Bool(false)) => {}
        Some(v @ Value::Object(_)) => {
            fields.push(format!("[key: string]: {}", ts_type(v, types, depth + 1)))
        }
        _ if fields.is_empty() => return "Record<string, unknown>".into(),
        _ => {}
    }
    if fields.is_empty() {
        "Record<string, never>".into()
    } else {
        format!("{{ {} }}", fields.join("; "))
    }
}

/// The TypeScript module for one interface version. It exports the types,
/// the method and event tables, and a `ServiceDefinition` constant that
/// `ad4m.service(...)` takes.
pub fn typescript(doc: &InterfaceDocument) -> String {
    let id = ident(doc);
    let d = &doc.doc;
    // `a-b` and `aB` both become `AB`: number the later ones so the module compiles.
    let mut type_names: BTreeMap<String, String> = BTreeMap::new();
    for k in d.types.keys() {
        let base = format!("{}_{}", id, pascal(k));
        let mut name = base.clone();
        let mut n = 2;
        while type_names.values().any(|v| *v == name) {
            name = format!("{}{}", base, n);
            n += 1;
        }
        type_names.insert(k.clone(), name);
    }
    let mut out = String::new();
    let _ = writeln!(
        out,
        "// Generated by `ad4m service-gen` from interface {}.",
        doc.hash
    );
    let _ = writeln!(
        out,
        "// {} {} — module {}. Do not edit.\n",
        d.name.replace(['\n', '\r'], " "),
        d.version,
        doc.module_id()
    );
    let _ = writeln!(
        out,
        "import type {{ ServiceDefinition }} from \"@coasys/ad4m\";\n"
    );
    for (k, v) in &d.types {
        doc_comment(
            &mut out,
            "",
            v.get("description").and_then(Value::as_str).unwrap_or(""),
        );
        let _ = writeln!(
            out,
            "export type {} = {};\n",
            type_names[k],
            ts_type(v, &type_names, 0)
        );
    }
    let _ = writeln!(out, "export type {}_Methods = {{", id);
    for (name, m) in &d.methods {
        doc_comment(&mut out, "  ", &m.description);
        let chunk = m
            .stream
            .as_ref()
            .and_then(|s| d.events.get(&s.event))
            .map(|e| format!("; chunk: {}", ts_type(&e.payload, &type_names, 0)))
            .unwrap_or_default();
        let _ = writeln!(
            out,
            "  {}: {{ params: {}; result: {}{} }};",
            name,
            ts_type(&m.params, &type_names, 0),
            ts_type(&m.result, &type_names, 0),
            chunk
        );
    }
    let _ = writeln!(out, "}};\n");
    let _ = writeln!(out, "export type {}_Events = {{", id);
    for (name, e) in &d.events {
        doc_comment(&mut out, "  ", &e.description);
        let _ = writeln!(
            out,
            "  {}: {};",
            quote(name),
            ts_type(&e.payload, &type_names, 0)
        );
    }
    let _ = writeln!(out, "}};\n");
    let list = |names: Vec<&String>| {
        names
            .iter()
            .map(|n| quote(n))
            .collect::<Vec<_>>()
            .join(", ")
    };
    let reads = list(
        d.methods
            .iter()
            .filter(|(_, m)| m.read)
            .map(|(n, _)| n)
            .collect(),
    );
    let longs = list(
        d.methods
            .iter()
            .filter(|(_, m)| m.long)
            .map(|(n, _)| n)
            .collect(),
    );
    let streams = d
        .methods
        .iter()
        .filter_map(|(n, m)| {
            m.stream
                .as_ref()
                .map(|s| format!("{}: {}", n, quote(&s.event)))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let scopes = d
        .events
        .iter()
        .filter_map(|(n, e)| {
            e.scope
                .as_ref()
                .map(|s| format!("{}: {}", quote(n), quote(s)))
        })
        .collect::<Vec<_>>()
        .join(", ");
    doc_comment(&mut out, "", &d.description);
    let _ = writeln!(
        out,
        "export const {}: ServiceDefinition<{}_Methods, {}_Events> = {{",
        id, id, id
    );
    let _ = writeln!(out, "  hash: {},", quote(&doc.hash));
    let _ = writeln!(out, "  moduleId: {},", quote(doc.module_id()));
    let _ = writeln!(out, "  name: {},", quote(&d.name));
    let _ = writeln!(out, "  version: {},", quote(&d.version));
    let _ = writeln!(out, "  read: new Set([{}]),", reads);
    let _ = writeln!(out, "  long: new Set([{}]),", longs);
    let _ = writeln!(out, "  streams: {{ {} }},", streams);
    let _ = writeln!(out, "  scopes: {{ {} }},", scopes);
    let _ = writeln!(out, "}};");
    out
}

/// MCP tool descriptors: one per method whose `tool` flag (default: `safe`
/// risk) exposes it. `inputSchema` is standalone (types inlined as `$defs`).
pub fn mcp_tools(doc: &InterfaceDocument) -> Value {
    let tools: Vec<Value> = doc
        .doc
        .methods
        .iter()
        .filter(|(_, m)| {
            m.tool.unwrap_or_else(|| {
                doc.doc
                    .actions
                    .get(&m.action)
                    .is_some_and(|a| a.risk == Risk::Safe)
            })
        })
        .map(|(name, m)| {
            json!({
                "name": format!("{}.{}", doc.doc.name, name),
                "description": m.description,
                "inputSchema": doc.standalone_schema(&m.params),
                "_meta": { "ad4m/method": format!("{}.{}", doc.hash, name) },
            })
        })
        .collect();
    Value::Array(tools)
}

fn schema_block(out: &mut String, doc: &InterfaceDocument, schema: &Value) {
    let _ = writeln!(
        out,
        "```ts\n{}\n```\n",
        ts_type(
            schema,
            &doc.doc
                .types
                .keys()
                .map(|k| (k.clone(), k.clone()))
                .collect(),
            0
        )
    );
}

/// A Markdown reference page.
pub fn markdown(doc: &InterfaceDocument) -> String {
    let d = &doc.doc;
    let mut out = String::new();
    let _ = writeln!(out, "# {} {}\n", d.name, d.version);
    if !d.description.is_empty() {
        let _ = writeln!(out, "{}\n", d.description);
    }
    let _ = writeln!(out, "| | |\n|---|---|");
    let _ = writeln!(out, "| Interface hash | `{}` |", doc.hash);
    let _ = writeln!(out, "| Module ID | `{}` |", doc.module_id());
    let _ = writeln!(out, "| Compatibility line | `{}` |", doc.compat());
    let _ = writeln!(
        out,
        "| Selection | `{}` |\n",
        serde_json::to_value(d.selection)
            .unwrap()
            .as_str()
            .unwrap_or("")
    );
    let _ = writeln!(
        out,
        "## Actions\n\n| Action | Label | Risk | Description |\n|---|---|---|---|"
    );
    for (n, a) in &d.actions {
        let risk = serde_json::to_value(a.risk).unwrap();
        let _ = writeln!(
            out,
            "| `{}` | {} | {} | {} |",
            n,
            a.label,
            risk.as_str().unwrap_or(""),
            a.description
        );
    }
    let _ = writeln!(out, "\n## Methods\n");
    for (n, m) in &d.methods {
        let mut flags = vec![format!("action `{}`", m.action)];
        if m.read {
            flags.push("read".into());
        }
        if m.long {
            flags.push("long".into());
        }
        if let Some(s) = &m.stream {
            flags.push(format!("streams `{}`", s.event));
        }
        let _ = writeln!(
            out,
            "### `{}`\n\n{}\n\n{}\n",
            n,
            m.description,
            flags.join(" · ")
        );
        let _ = writeln!(out, "Params:\n");
        schema_block(&mut out, doc, &m.params);
        let _ = writeln!(out, "Result:\n");
        schema_block(&mut out, doc, &m.result);
        if !m.errors.is_empty() {
            let _ = writeln!(out, "| Error | Code |\n|---|---|");
            for (e, def) in &m.errors {
                let _ = writeln!(out, "| `{}` | {} |", e, def.code);
            }
            out.push('\n');
        }
    }
    if !d.events.is_empty() {
        let _ = writeln!(out, "## Events\n");
        for (n, e) in &d.events {
            let scope = e
                .scope
                .as_deref()
                .map(|s| format!(" · scope `{}`", s))
                .unwrap_or_default();
            let _ = writeln!(
                out,
                "### `{}`\n\n{}\n\naction `{}`{}\n",
                n, e.description, e.action, scope
            );
            schema_block(&mut out, doc, &e.payload);
        }
    }
    if !d.types.is_empty() {
        let _ = writeln!(out, "## Types\n");
        for (n, t) in &d.types {
            let _ = writeln!(out, "### `{}`\n", n);
            schema_block(&mut out, doc, t);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::interface::tests::genesis;

    fn doc() -> InterfaceDocument {
        InterfaceDocument::parse(genesis("did:key:z6Mkx")).unwrap()
    }

    #[test]
    fn typescript_has_tables_and_definition() {
        let d = doc();
        let ts = typescript(&d);
        assert!(ts.contains("export type Echo_1_0_0_Text = string;"));
        assert!(ts.contains(
            "say: { params: { room: string; text: Echo_1_0_0_Text }; result: { text: string } };"
        ));
        assert!(ts.contains("count: { params: { to: number; streamId: string }; result: { total: number }; chunk: { streamId: string; n: number } };"));
        assert!(ts.contains("\"count-tick\": { streamId: string; n: number };"));
        assert!(ts.contains(&format!("hash: \"{}\"", d.hash)));
        assert!(ts.contains("read: new Set([\"say\"])"));
        assert!(ts.contains("long: new Set([\"count\"])"));
        assert!(ts.contains("streams: { count: \"count-tick\" }"));
        assert!(ts.contains("scopes: { \"count-tick\": \"streamId\", \"said\": \"room\" }"));
    }

    #[test]
    fn clashing_type_names_get_numbered() {
        let mut raw = genesis("did:key:z6Mkx");
        raw["types"]["a-b"] = json!({ "type": "string" });
        raw["types"]["aB"] = json!({ "type": "number" });
        let ts = typescript(&InterfaceDocument::parse(raw).unwrap());
        // Keys in order: `a-b` first.
        assert!(ts.contains("export type Echo_1_0_0_AB = string;"));
        assert!(ts.contains("export type Echo_1_0_0_AB2 = number;"));
    }

    #[test]
    fn ts_types_cover_common_schemas() {
        let t = BTreeMap::new();
        assert_eq!(
            ts_type(&json!({ "type": ["string", "null"] }), &t, 0),
            "string | null"
        );
        assert_eq!(
            ts_type(&json!({ "enum": ["a", "b"] }), &t, 0),
            "\"a\" | \"b\""
        );
        assert_eq!(
            ts_type(
                &json!({ "type": "array", "items": { "type": "integer" } }),
                &t,
                0
            ),
            "Array<number>"
        );
        assert_eq!(
            ts_type(&json!({ "type": "object" }), &t, 0),
            "Record<string, unknown>"
        );
        assert_eq!(
            ts_type(
                &json!({ "type": "object", "additionalProperties": { "type": "number" } }),
                &t,
                0
            ),
            "{ [key: string]: number }"
        );
        assert_eq!(
            ts_type(
                &json!({ "anyOf": [{ "type": "string" }, { "type": "null" }] }),
                &t,
                0
            ),
            "(string) | (null)"
        );
        assert_eq!(
            ts_type(
                &json!({ "type": "object", "properties": { "a-b": { "type": "string" } } }),
                &t,
                0
            ),
            "{ \"a-b\"?: string }"
        );
    }

    #[test]
    fn mcp_tools_follow_risk() {
        let mut raw = genesis("did:key:z6Mkx");
        raw["actions"]["POST"] = json!({ "label": "Post", "risk": "write" });
        raw["methods"]["post"] = raw["methods"]["say"].clone();
        raw["methods"]["post"]["action"] = json!("POST");
        let d = InterfaceDocument::parse(raw).unwrap();
        let tools = mcp_tools(&d);
        let names: Vec<&str> = tools
            .as_array()
            .unwrap()
            .iter()
            .map(|t| t["name"].as_str().unwrap())
            .collect();
        assert_eq!(names, vec!["echo.count", "echo.say"]);
        let say = &tools[1];
        assert_eq!(
            say["_meta"]["ad4m/method"],
            json!(format!("{}.say", d.hash))
        );
        assert!(say["inputSchema"]["$defs"]["Text"].is_object());
    }

    #[test]
    fn markdown_lists_everything() {
        let md = markdown(&doc());
        for needle in [
            "# echo 1.0.0",
            "| `SAY` | Say things | safe |",
            "### `say`",
            "| `Muted` | 409 |",
            "### `said`",
            "scope `room`",
            "### `Text`",
        ] {
            assert!(md.contains(needle), "missing {}", needle);
        }
    }
}
