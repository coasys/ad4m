//! `template_read` + `template_update` — a WE template, read and edited by WE's own rules.
//!
//! The rules are WE's, compiled into the executor (`tool_bundles::builtin`); this side reads and
//! writes. A call reads the Template record as the model query returns it (its schema file
//! resolved to its content), hands it to the bundle with the arguments, and writes whatever
//! properties come back through the class's setters, in one batch, as the calling agent — the same
//! path and the same schema check `instance_update` takes, so the schema is stored as a file
//! expression, a value of the wrong type or cardinality is refused, and the schema and its version
//! change together. Each call runs in a fresh isolate, so a record is seen by that call alone.
//!
//! The bundle also gets the perspective's models ([`models`]): a template may query any class
//! the perspective defines, and WE's rules refuse a `$query` naming one they were not told of.
//!
//! A tool that writes holds [`WRITES`] from reading the record until its write commits, and the
//! version is read again just before the write: the bundle checks the caller's version against
//! the record it was handed, and that record must still be the stored one when the result lands.
//!
//! The two tools' descriptions and arguments are the bundle's own ([`from_bundle`]), so they
//! follow WE when `build.rs` is pinned to a newer WE release.

use super::{check_setters, class_properties, error_json, fetch_instance, instance_uri};
use super::{resolve_class, subject_class, validate_properties, Ad4mMcpHandler, WriteMode};
use crate::agent::did_for_context;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::tool_bundles::builtin;
use rmcp::{handler::server::wrapper::Parameters, model::Tool, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::sync::Arc;
use tokio::sync::Mutex;

/// Held by a tool that writes, from reading the record until its write commits, so two updates
/// cannot both start from the same version. One for every record: calls run one at a time in the
/// bundle anyway (`builtin::call`), so a finer lock would buy nothing.
static WRITES: Mutex<()> = Mutex::const_new(());

/// Parameters for a template tool: the record, and the tool's own arguments beside them
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct TemplateToolParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// URI of the Template record
    pub base_uri: String,
    /// The tool's own arguments
    #[serde(flatten)]
    pub arguments: Map<String, Value>,
}

/// A template tool as MCP lists it: the bundle's description, and its arguments with the two that
/// name the record. The attribute the `#[tool]` macro generates stands only if the bundle has no
/// tool of that name, which the tests rule out.
pub(crate) fn from_bundle(mut attr: Tool) -> Tool {
    let Some(tool) = builtin::tool(&attr.name) else {
        return attr;
    };
    let mut schema = tool.parameters.as_object().cloned().unwrap_or_default();
    let properties = schema
        .entry("properties")
        .or_insert_with(|| json!({}))
        .as_object_mut()
        .expect("a tool's parameters are an object schema");
    properties.insert(
        "perspective_id".into(),
        json!({ "type": "string", "description": "Perspective UUID" }),
    );
    properties.insert(
        "base_uri".into(),
        json!({ "type": "string", "description": "URI of the Template record" }),
    );
    let mut required = vec![json!("perspective_id"), json!("base_uri")];
    if let Some(Value::Array(own)) = schema.get("required") {
        required.extend(own.iter().cloned());
    }
    schema.insert("required".into(), Value::Array(required));
    schema.insert("type".into(), json!("object"));

    attr.description = Some(tool.description.clone().into());
    attr.input_schema = Arc::new(schema);
    attr
}

impl Ad4mMcpHandler {
    /// Read a WE template. Description and arguments: [`from_bundle`].
    #[tool(description = "Read a WE template.")]
    pub async fn template_read(&self, params: Parameters<TemplateToolParams>) -> String {
        self.call_template_tool("template_read", params.0).await
    }

    /// Patch a WE template and save it. Description and arguments: [`from_bundle`].
    #[tool(description = "Patch a WE template and save it.")]
    pub async fn template_update(&self, params: Parameters<TemplateToolParams>) -> String {
        self.call_template_tool("template_update", params.0).await
    }

    async fn call_template_tool(&self, name: &str, p: TemplateToolParams) -> String {
        let Some(tool) = builtin::tool(name) else {
            return error_json(format!("This executor's template tools have no '{name}'."));
        };
        // A tool that writes checks the write rule before it reads anything, as the other write
        // tools do: a caller who may only read is refused as the RPC guard would refuse it.
        let perspective = if tool.writes {
            self.get_writable_perspective(&p.perspective_id)
                .await
                .map(|(perspective, _)| perspective)
        } else {
            self.get_readable_perspective(&p.perspective_id).await
        };
        let perspective = match perspective {
            Ok(v) => v,
            Err(e) => return e,
        };
        let agent_context = match self.get_agent_context().await {
            Ok(v) => v,
            Err(e) => return error_json(e),
        };
        let base_uri = match instance_uri("base_uri", &p.base_uri) {
            Ok(uri) => uri,
            Err(e) => return e,
        };

        let _writing = if tool.writes {
            Some(WRITES.lock().await)
        } else {
            None
        };
        let applies_to = &builtin::declared().applies_to;
        let mut record = None;
        for class_name in applies_to {
            match fetch_instance(&perspective, class_name, &base_uri).await {
                Ok(Some(instance)) => {
                    record = Some((class_name.clone(), instance));
                    break;
                }
                Ok(None) => {}
                // A class this perspective does not define is simply not this record's.
                Err(_) => {}
            }
        }
        let Some((class_name, instance)) = record else {
            return error_json(format!(
                "'{name}' works on {}, and there is no such record at '{base_uri}'.",
                applies_to.join(" or ")
            ));
        };

        let version = instance.get("version").cloned();
        let input = json!({
            "record": { "id": base_uri, "className": class_name, "properties": instance },
            "args": Value::Object(p.arguments),
            "models": models(&perspective).await,
            "author": did_for_context(&agent_context).ok(),
        });
        let answer = match builtin::call(name, input).await {
            Ok(answer) => answer,
            Err(e) => return error_json(format!("'{name}' failed: {e}")),
        };
        let text = answer["text"].as_str().unwrap_or_default().to_string();
        let properties = match answer.get("properties") {
            Some(Value::Object(map)) if !map.is_empty() => map.clone(),
            _ => return text,
        };
        if !tool.writes {
            return error_json(format!(
                "'{name}' says it writes nothing, yet answered with properties to write; nothing was written."
            ));
        }
        // WRITES keeps out the other template tool calls; this catches a write from anywhere else
        // (the app's own save) while the bundle ran.
        match fetch_instance(&perspective, &class_name, &base_uri).await {
            Ok(Some(now)) if now.get("version") == version.as_ref() => {}
            _ => {
                return error_json(format!(
                    "Nothing was written: the record changed while '{name}' ran. Read it again, \
                     then make the change against what it says now."
                ))
            }
        }

        match self
            .write_record(&p.perspective_id, &class_name, &base_uri, properties)
            .await
        {
            Ok(()) => text,
            Err(e) => error_json(format!("{e} The tool had said: {text}")),
        }
    }
    /// Write what a tool answered with through the class's setters, every property in one batch.
    async fn write_record(
        &self,
        perspective_id: &str,
        class_name: &str,
        base_uri: &str,
        properties: Map<String, Value>,
    ) -> Result<(), String> {
        let (mut perspective, agent_context) =
            self.get_writable_perspective(perspective_id).await?;
        let (class_name, shape) = resolve_class(&perspective, class_name).await?;
        // The same checks instance_update makes: the class schema, then the setters, on the
        // values the schema check coerced. A setter does not check a value's type or cardinality.
        let refused = |problems: Vec<super::ValidationError>| {
            format!(
                "Nothing was written: {class_name} cannot set {}.",
                problems
                    .iter()
                    .map(|m| format!("{}: {}", m.property, m.problem))
                    .collect::<Vec<_>>()
                    .join("; ")
            )
        };
        let validated =
            validate_properties(&shape, &properties, WriteMode::Update).map_err(&refused)?;
        let missing = check_setters(&perspective, &shape, &validated.scalars).await;
        if !missing.is_empty() {
            return Err(refused(missing));
        }

        let batch_id = perspective.create_batch().await;
        if let Err(e) = perspective
            .update_subject(
                subject_class(&class_name),
                base_uri.to_string(),
                Value::Object(validated.scalars),
                Some(batch_id.clone()),
                &agent_context,
            )
            .await
        {
            perspective.discard_batch(&batch_id).await;
            return Err(format!("Nothing was written: {e:#}."));
        }
        perspective
            .commit_batch(batch_id, &agent_context)
            .await
            .map(|_| ())
            .map_err(|e| format!("The write was not committed: {e:#}."))
    }
}

/// The perspective's models as the bundle takes them (WE's `EntityManifestEntry`): each class by
/// name with its properties. WE's own classes are named in its reference already; the bundle
/// describes the others to the model in full.
async fn models(perspective: &PerspectiveInstance) -> Vec<Value> {
    let mut names = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    names.sort();
    names.dedup();
    names
        .iter()
        .filter_map(|name| {
            let shape = perspective.get_shape(name).ok()?;
            let properties: Vec<Value> = class_properties(&shape)
                .iter()
                .filter(|p| !p.flag())
                .map(|p| {
                    let kind = match p.type_name().as_str() {
                        "reference" => "uri",
                        "integer" | "number" => "number",
                        "boolean" => "boolean",
                        _ => "string",
                    };
                    let mut v = json!({
                        "name": p.name(),
                        "predicate": p.predicate(),
                        "type": kind,
                        "isCollection": p.collection(),
                        "required": p.required(),
                        "writable": p.writable(),
                    });
                    if let Some((_, target)) = p.relation() {
                        v["relatedEntity"] = json!(target);
                    }
                    if let Some(language) = p.resolve_language() {
                        v["resolveLanguage"] = json!(language);
                    }
                    if p.reverse() {
                        v["reverse"] = json!(true);
                    }
                    v
                })
                .collect();
            Some(json!({
                "name": name,
                "targetClass": shape.target_class,
                "properties": properties,
            }))
        })
        .collect()
}
