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
//! A tool that writes holds its record's lock ([`lock_record`]) from reading the record until its
//! write commits, and the version is read again just before the write: the bundle checks the caller's version against
//! the record it was handed, and that record must still be the stored one when the result lands.
//! The batch commits only if the record's version links are still those of that last read
//! (`commit_batch_if`), so a save from the app landing after it is refused as well.
//!
//! The two tools' descriptions and arguments are the bundle's own ([`from_bundle`]), so they
//! follow WE when `build.rs` is pinned to a newer WE release.

use super::{check_setters, class_properties, error_json, fetch_instance, instance_uri};
use super::{resolve_class, subject_class, validate_properties, Ad4mMcpHandler, WriteMode};
use crate::agent::did_for_context;
use crate::perspectives::perspective_instance::{ExpectedLinks, LinksChanged, PerspectiveInstance};
use crate::tool_bundles::builtin;
use deno_core::error::AnyError;
use rmcp::{handler::server::wrapper::Parameters, model::Tool, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::collections::HashMap;
use std::sync::{Arc, LazyLock, PoisonError, Weak};
use tokio::sync::{Mutex, OwnedMutexGuard};

/// One lock per record, by perspective and record URI, held by a tool that writes from reading the
/// record until its write commits ([`lock_record`]). A lock nobody holds or waits for is pruned.
type RecordLocks = std::sync::Mutex<HashMap<(String, String), Weak<Mutex<()>>>>;
static WRITES: LazyLock<RecordLocks> = LazyLock::new(Default::default);

/// The lock of record `base_uri` in perspective `perspective_id`, so two updates of it cannot both
/// start from the same version. Only that record's: reading a record can wait long (a joined
/// perspective that has not synced the record's class yet), and that must not hold up a template
/// in another perspective, or another record.
async fn lock_record(perspective_id: &str, base_uri: &str) -> OwnedMutexGuard<()> {
    let lock = {
        let mut locks = WRITES.lock().unwrap_or_else(PoisonError::into_inner);
        locks.retain(|_, lock| lock.strong_count() > 0);
        let key = (perspective_id.to_string(), base_uri.to_string());
        match locks.get(&key).and_then(Weak::upgrade) {
            Some(lock) => lock,
            None => {
                let lock = Arc::new(Mutex::new(()));
                locks.insert(key, Arc::downgrade(&lock));
                lock
            }
        }
    };
    lock.lock_owned().await
}

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

/// Tools the bundle declares that MCP does not offer yet. Every other tool the bundle declares has a
/// `#[tool]` method below; a test fails on one that is in neither place.
///
/// - `template_create` makes a new Template record, and its `place` argument chooses where: the
///   caller's own `library` or the `space`. The executor has no rule yet for which perspective is
///   a caller's library, so the tool waits for one. The bundle's documentation says the tool is
///   there only where the host offers it.
///
/// Only the test reads it: a tool without a `#[tool]` method is never listed anyway.
#[cfg(test)]
pub(crate) const NOT_OFFERED: &[&str] = &["template_create"];

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
            Some(lock_record(&p.perspective_id, &base_uri).await)
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
        // The record's lock keeps out the other template tool calls; this catches a write from
        // anywhere else (the app's own save) while the bundle ran. The version links are taken
        // before this read and the write commits only if they still stand, so a save landing after
        // it is caught too.
        let expected = match version_links(&perspective, &class_name, &base_uri) {
            Ok(expected) => expected,
            Err(e) => return error_json(format!("Nothing was written: {e:#}.")),
        };
        match fetch_instance(&perspective, &class_name, &base_uri).await {
            Ok(Some(now)) if now.get("version") == version.as_ref() => {}
            _ => return error_json(record_changed(name)),
        }
        #[cfg(test)]
        pause::after_last_read(&base_uri).await;

        match self
            .write_record(
                &p.perspective_id,
                &class_name,
                &base_uri,
                properties,
                expected,
            )
            .await
        {
            Ok(()) => text,
            Err(WriteError::Changed) => error_json(record_changed(name)),
            Err(WriteError::Refused(e)) => error_json(format!("{e} The tool had said: {text}")),
        }
    }
    /// Write what a tool answered with through the class's setters, every property in one batch,
    /// committed only if the store still holds `expected`.
    async fn write_record(
        &self,
        perspective_id: &str,
        class_name: &str,
        base_uri: &str,
        properties: Map<String, Value>,
        expected: Vec<ExpectedLinks>,
    ) -> Result<(), WriteError> {
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
            return Err(refused(missing).into());
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
            return Err(format!("Nothing was written: {e:#}.").into());
        }
        match perspective
            .commit_batch_if(batch_id, &agent_context, &expected)
            .await
        {
            Ok(_) => Ok(()),
            Err(e) if e.is::<LinksChanged>() => Err(WriteError::Changed),
            Err(e) => Err(format!("The write was not committed: {e:#}.").into()),
        }
    }
}

/// Why [`Ad4mMcpHandler::write_record`] wrote nothing.
enum WriteError {
    /// The record changed after the tool's last read of it.
    Changed,
    Refused(String),
}

impl From<String> for WriteError {
    fn from(e: String) -> Self {
        WriteError::Refused(e)
    }
}

fn record_changed(name: &str) -> String {
    format!(
        "Nothing was written: the record changed while '{name}' ran. Read it again, then make \
         the change against what it says now."
    )
}

/// The record's version links as the store holds them now, for the write to expect. None for a
/// class without a version.
fn version_links(
    perspective: &PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
) -> Result<Vec<ExpectedLinks>, AnyError> {
    let shape = perspective.get_shape(class_name)?;
    let version = class_properties(&shape)
        .into_iter()
        .find(|p| p.name() == "version");
    version
        .map(|p| perspective.expect_links(base_uri, p.predicate()))
        .into_iter()
        .collect()
}

/// Test seam: parks a template tool call right after its last read of the record, before it
/// writes, so a test can land another write in that gap.
#[cfg(test)]
pub(crate) mod pause {
    use std::sync::Mutex;
    use tokio::sync::oneshot;

    type Pause = (String, oneshot::Sender<()>, oneshot::Receiver<()>);
    static PAUSE: Mutex<Option<Pause>> = Mutex::new(None);

    /// Park the next call on `base_uri`: the first receiver fires when it has read the record,
    /// and it goes on to write when the sender is used or dropped.
    pub(crate) fn arm(base_uri: &str) -> (oneshot::Receiver<()>, oneshot::Sender<()>) {
        let (reached, on_reached) = oneshot::channel();
        let (go, on_go) = oneshot::channel();
        *PAUSE.lock().unwrap() = Some((base_uri.to_string(), reached, on_go));
        (on_reached, go)
    }

    pub(super) async fn after_last_read(base_uri: &str) {
        let armed = {
            let mut pause = PAUSE.lock().unwrap();
            match pause.take() {
                Some((uri, reached, go)) if uri == base_uri => Some((reached, go)),
                other => {
                    *pause = other;
                    None
                }
            }
        };
        if let Some((reached, go)) = armed {
            let _ = reached.send(());
            let _ = go.await;
        }
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
