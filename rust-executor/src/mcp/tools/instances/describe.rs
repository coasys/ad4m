//! `describe_perspective` — the discovery step before any `instance_*` call.
//!
//! Projects the registered SHACL classes (and flows) into plain JSON an
//! agent can read: property names, friendly types, cardinality, hints.

use super::{class_properties, pretty, Ad4mMcpHandler, PropView};
use crate::perspectives::flow_context::load_shacl_flows;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::PerspectiveHandle;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::BTreeSet;

/// Parameters for describing a perspective's data model
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct DescribePerspectiveParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// A relation may name a `target_class` the perspective never registered —
/// the shape only carries the name, nothing checks it resolves. That is a dead
/// end for a caller: `describe_perspective` is the authority on what it may
/// write, so an unregistered target reads as "create one of these" when no
/// `class_name` will accept it. Say it on the property rather than leaving the
/// caller to diff `target_class` against `classes` itself.
fn flag_unresolved_relation_targets(props: &mut [Value], registered: &BTreeSet<&str>) {
    for prop in props {
        let Some(target) = prop.get("target_class").and_then(Value::as_str) else {
            continue;
        };
        if registered.contains(target) {
            continue;
        }
        let note = format!(
            "`{target}` is not registered in this perspective — it is absent from `classes`, so \
             no instance_* call will accept it as a class_name. Point this property at an \
             existing URI if you have one; otherwise leave it unset and report the gap. \
             Registering the missing class with add_model is what makes the target reachable."
        );
        prop["target_class_registered"] = json!(false);
        prop["target_class_note"] = json!(note);
    }
}

/// Describe one class as data: hint, single-valued `properties`, multi-valued
/// `collections`, and which property (if any) is the dedup identity.
///
/// `registered` is every class name the perspective declares, used to mark
/// relation targets that do not resolve to one of them.
pub(crate) fn describe_class(
    class_name: &str,
    shape: &ModelShape,
    registered: &BTreeSet<&str>,
) -> Value {
    let infos = class_properties(shape);
    let mut properties: Vec<Value> = infos
        .iter()
        .filter(|i| !i.flag() && !i.collection())
        .map(PropView::to_json)
        .collect();
    let mut collections: Vec<Value> = infos
        .iter()
        .filter(|i| !i.flag() && i.collection())
        .map(PropView::to_json)
        .collect();
    flag_unresolved_relation_targets(&mut properties, registered);
    flag_unresolved_relation_targets(&mut collections, registered);
    let identity = infos
        .iter()
        .find(|i| i.identity())
        .map(|i| i.name().to_string());
    let mut v = json!({
        "name": class_name,
        "class_uri": shape.target_class,
        "properties": properties,
        "collections": collections,
    });
    if let Some(hint) = &shape.interpretation_hint {
        v["interpretation_hint"] = json!(hint);
    }
    if let Some(id) = identity {
        v["identity_property"] = json!(id);
    }
    v
}

/// Describe the flows (state machines) registered on a perspective, compact
/// enough for an agent to read: states with their hints, transitions with
/// the action name that triggers them, and which classes a flow runs on.
pub(crate) async fn describe_flows(perspective: &PerspectiveInstance) -> Vec<Value> {
    let flows = match load_shacl_flows(perspective).await {
        Ok(f) => f,
        Err(e) => {
            log::warn!("describe_perspective: could not load flows: {e:#}");
            return vec![];
        }
    };
    let mut entries: Vec<(String, Value)> = flows
        .iter()
        .map(|(uri, flow)| {
            let states: Vec<Value> = flow
                .states
                .iter()
                .map(|s| {
                    let mut v = json!({ "name": s.name, "value": s.value });
                    if let Some(h) = &s.interpretation_hint {
                        v["interpretation_hint"] = json!(h);
                    }
                    v
                })
                .collect();
            let transitions: Vec<Value> = flow
                .transitions
                .iter()
                .map(|t| {
                    json!({
                        "action": t.action_name,
                        "from_state": t.from_state,
                        "to_state": t.to_state,
                    })
                })
                .collect();
            let mut v = json!({
                "name": flow.name,
                "uri": uri,
                "input_types": flow.input_types,
                "output_types": flow.output_types,
                "states": states,
                "transitions": transitions,
            });
            if let Some(h) = &flow.interpretation_hint {
                v["interpretation_hint"] = json!(h);
            }
            if let Some(h) = &flow.creation_hint {
                v["creation_hint"] = json!(h);
            }
            (uri.clone(), v)
        })
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries.into_iter().map(|(_, v)| v).collect()
}

/// The full `describe_perspective` payload.
pub(crate) async fn describe_perspective_value(
    perspective: &PerspectiveInstance,
    handle: &PerspectiveHandle,
) -> Value {
    let mut class_names = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    class_names.sort();
    class_names.dedup();

    let registered: BTreeSet<&str> = class_names.iter().map(String::as_str).collect();

    let mut classes = Vec::with_capacity(class_names.len());
    for name in &class_names {
        match perspective.get_shape(name) {
            Ok(shape) => classes.push(describe_class(name, &shape, &registered)),
            // Never drop a class silently — an agent that can't see a class
            // can't tell a missing class from a broken shape.
            Err(e) => classes.push(json!({ "name": name, "error": format!("{e:#}") })),
        }
    }

    let flows = describe_flows(perspective).await;

    json!({
        "perspective_id": handle.uuid,
        "name": handle.name,
        "shared_url": handle.shared_url,
        "is_neighbourhood": handle.neighbourhood.is_some(),
        "classes": classes,
        "flows": flows,
        "usage": "Pass a class `name` from `classes` as `class_name` to instance_create / \
                  instance_query / instance_get / instance_update / instance_add_to_collection / \
                  instance_remove_from_collection / instance_remove / instance_transcript. \
                  `properties` entries are single JSON values typed as shown; `collections` \
                  are written with instance_add_to_collection / instance_remove_from_collection \
                  using item URIs. Properties are validated against this schema on every \
                  write. For the raw ad4m://has_child tree (e.g. children of ad4m://self) use \
                  add_child / get_children.",
    })
}

impl Ad4mMcpHandler {
    /// Describe the data model of a perspective — the discovery step before any instance_* call.
    #[tool(
        description = "Describe the data model of a perspective: every registered subject class (model) with its properties (name, type, required, cardinality, hints), its collections, and any flows (state machines). Call this right after list_perspectives / neighbourhood_join_from_url — it returns the schema as data so you can then use the generic instance_create / instance_query / instance_get / instance_update / instance_add_to_collection / instance_remove_from_collection / instance_remove / instance_transcript tools with class_name set to one of the returned class names. Property values passed to those tools are validated against this schema."
    )]
    pub async fn describe_perspective(
        &self,
        params: Parameters<DescribePerspectiveParams>,
    ) -> String {
        let p = &params.0;
        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };
        let handle = perspective.persisted.lock().await.clone();
        pretty(&describe_perspective_value(&perspective, &handle).await)
    }
}
