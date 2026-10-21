//! `list_record_tools` + `call_record_tool` — the tools apps install for their own records.
//!
//! An app can install a tool bundle on the node (`tool_bundles`): JavaScript that knows how that
//! app's records work — what a valid one is, how an edit to one applies. Its tools are reached
//! through these two, as the per-class operations are through `instance_*`: the tool is a
//! parameter, so the tool list stays the same whatever is installed.
//!
//! The bundle computes; this side reads and writes. `call_record_tool` reads the record as the
//! model query returns it (file properties resolved to their content), hands it to the bundle with
//! the arguments, and writes whatever properties come back through the class's setters, in one
//! batch, as the calling agent — the same path `instance_update` takes, so a file property is
//! stored as an expression and every property changes together.

use super::{check_setters, error_json, fetch_instance, instance_uri, pretty, resolve_class};
use super::{subject_class, Ad4mMcpHandler};
use crate::agent::did_for_context;
use crate::tool_bundles::ToolBundleRegistry;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

/// Parameters for listing the tools apps have installed
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct ListRecordToolsParams {
    /// Only the tools for this subject class. Omit for every installed tool.
    #[serde(default)]
    pub class_name: Option<String>,
}

/// Parameters for calling an installed tool on one record
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct CallRecordToolParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// URI of the record the tool works on
    pub base_uri: String,
    /// The tool's name, as list_record_tools gives it
    pub tool: String,
    /// The tool's own arguments, as its parameters in list_record_tools describe them
    #[serde(default)]
    pub arguments: Map<String, Value>,
}

impl Ad4mMcpHandler {
    /// List the tools apps have installed for their records.
    #[tool(
        description = "List the tools apps have installed on this node for their own kinds of record — editing an app's page layouts, say. Each set of tools names the subject classes it works on, its documentation topic to read first (with get_documentation), and each tool's description, parameters and whether it writes. Call one with call_record_tool. Pass class_name to see only the tools for that class."
    )]
    pub async fn list_record_tools(&self, params: Parameters<ListRecordToolsParams>) -> String {
        let wanted = params.0.class_name;
        let toolsets: Vec<Value> = ToolBundleRegistry::global()
            .toolsets()
            .await
            .into_iter()
            .filter(|t| wanted.as_ref().is_none_or(|c| t.applies_to.contains(c)))
            .map(|t| {
                json!({
                    "toolset": t.name,
                    "appliesTo": t.applies_to,
                    "documentation": t.docs.first().map(|d| d.path.clone()),
                    "tools": t.tools,
                })
            })
            .collect();
        pretty(&json!({
            "toolsets": toolsets,
            "call": "call_record_tool(perspective_id, base_uri, tool, arguments)",
        }))
    }

    /// Call an installed tool on one record.
    #[tool(
        description = "Call a tool an app installed (see list_record_tools) on one record: base_uri is the record, of a class the tool works on; arguments are the tool's own, as its parameters describe. Answers with what the tool says. A tool that writes saves its change to the record as you, in one write — read its documentation first. Example: call_record_tool(perspective_id, base_uri='<id>', tool='template_read', arguments={})."
    )]
    pub async fn call_record_tool(&self, params: Parameters<CallRecordToolParams>) -> String {
        let p = params.0;
        let registry = ToolBundleRegistry::global();
        let Some(found) = registry.find_tool(&p.tool).await else {
            let installed: Vec<String> = registry
                .toolsets()
                .await
                .into_iter()
                .flat_map(|t| t.tools.into_iter().map(|tool| tool.name))
                .collect();
            return error_json(format!(
                "No installed tool is called '{}'. Installed: {}.",
                p.tool,
                if installed.is_empty() {
                    "none".to_string()
                } else {
                    installed.join(", ")
                }
            ));
        };

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
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

        let mut record = None;
        for class_name in &found.applies_to {
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
                "'{}' works on {}, and there is no such record at '{base_uri}'.",
                p.tool,
                found.applies_to.join(" or ")
            ));
        };

        let input = json!({
            "record": { "id": base_uri, "className": class_name, "properties": instance },
            "args": Value::Object(p.arguments),
            "author": did_for_context(&agent_context).ok(),
        });
        let answer = match found
            .sandbox
            .call(
                &["toolsets", &found.toolset, "call"],
                vec![json!(found.tool.name), input],
            )
            .await
        {
            Ok(answer) => answer,
            Err(e) => return error_json(format!("'{}' failed: {e}", p.tool)),
        };
        let text = answer["text"].as_str().unwrap_or_default().to_string();
        let properties = match answer.get("properties") {
            Some(Value::Object(map)) if !map.is_empty() => map.clone(),
            _ => return text,
        };
        if !found.tool.writes {
            return error_json(format!(
                "'{}' says it writes nothing, yet answered with properties to write; nothing was written.",
                p.tool
            ));
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
        let missing = check_setters(&perspective, &shape, &properties).await;
        if !missing.is_empty() {
            return Err(format!(
                "Nothing was written: {class_name} cannot set {}.",
                missing
                    .iter()
                    .map(|m| format!("{}: {}", m.property, m.problem))
                    .collect::<Vec<_>>()
                    .join("; ")
            ));
        }

        let batch_id = perspective.create_batch().await;
        if let Err(e) = perspective
            .update_subject(
                subject_class(&class_name),
                base_uri.to_string(),
                Value::Object(properties),
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
