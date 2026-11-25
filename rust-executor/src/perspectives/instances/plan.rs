//! A write plan: several instance writes that land together or not at all.
//!
//! A tool that makes a thing of several records (a post and its parts, a board and its columns)
//! answers with the records to create and the links to add, and the executor writes them: each
//! create through [`create`] and each add through [`add_to_collection`], every one checked
//! against its class's shape, all in one batch committed at the end. A write the shape refuses,
//! or any other failure, discards the batch, so nothing of the plan lands.
//!
//! The plan names the records it creates by a `ref` of its own choosing. Wherever a record is
//! named (a parent, a property value, an item), `{ "ref": "<name>" }` stands for the record that
//! create makes. Ids are minted before anything is written, so a ref may name a create later in
//! the plan.
//!
//! Only creates and adds. A plan changes no existing property and removes nothing: what it can do
//! to records it did not make is add them to a collection, the one write a reply into a thread
//! or an item into a list needs, and the write least able to lose anything.

use super::write::{add_to_collection, create, InstanceAddToCollection, InstanceCreate};
use super::{generate_instance_uri, InstanceError};
use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::HashMap;

/// The most writes one plan may hold.
pub const MAX_PLANNED_WRITES: usize = 200;

/// A record a plan names: an existing one by id, or one the plan creates by its `ref`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PlannedRecord {
    Created {
        #[serde(rename = "ref")]
        reference: String,
    },
    Existing(String),
}

/// One write of a plan.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", rename_all_fields = "camelCase")]
pub enum PlannedWrite {
    /// A new instance. `parent` puts it under a record, as `instance_create`'s parent does (the
    /// parent class's own `children` relation, else `ad4m://has_child`), in the order the plan
    /// lists the creates.
    Create {
        #[serde(rename = "ref")]
        reference: String,
        class_name: String,
        properties: Option<Map<String, Value>>,
        parent: Option<PlannedRecord>,
    },
    /// An item added to a collection of an existing instance.
    Add {
        class_name: String,
        base_uri: String,
        collection: String,
        item: PlannedRecord,
    },
}

/// What a plan created, by its `ref`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlannedCreated {
    #[serde(rename = "ref")]
    pub reference: String,
    pub class_name: String,
    pub base_uri: String,
}

/// Why a plan wrote nothing. `index` is the write's place in the plan.
#[derive(Debug)]
pub enum PlanError {
    Empty,
    TooMany {
        count: usize,
    },
    /// Two creates with one `ref`.
    DuplicateRef {
        index: usize,
        reference: String,
    },
    /// A `{ "ref" }` no create of the plan has.
    UnknownRef {
        index: usize,
        reference: String,
    },
    /// A write the instance core refused or failed.
    Write {
        index: usize,
        error: InstanceError,
    },
    /// The batch did not commit.
    Commit {
        error: deno_core::anyhow::Error,
    },
}

/// Write `writes` as `agent`, in one batch: all of them, or none.
pub async fn apply(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    writes: Vec<PlannedWrite>,
) -> Result<Vec<PlannedCreated>, PlanError> {
    if writes.is_empty() {
        return Err(PlanError::Empty);
    }
    if writes.len() > MAX_PLANNED_WRITES {
        return Err(PlanError::TooMany {
            count: writes.len(),
        });
    }

    // Every ref's id first, so a write may name a record created after it.
    let mut ids: HashMap<String, String> = HashMap::new();
    for (index, write) in writes.iter().enumerate() {
        if let PlannedWrite::Create { reference, .. } = write {
            if ids
                .insert(reference.clone(), generate_instance_uri())
                .is_some()
            {
                return Err(PlanError::DuplicateRef {
                    index,
                    reference: reference.clone(),
                });
            }
        }
    }
    let id_of = |index: usize, record: &PlannedRecord| -> Result<String, PlanError> {
        match record {
            PlannedRecord::Existing(id) => Ok(id.clone()),
            PlannedRecord::Created { reference } => {
                ids.get(reference).cloned().ok_or(PlanError::UnknownRef {
                    index,
                    reference: reference.clone(),
                })
            }
        }
    };

    // Resolve every ref before writing anything, so a bad one writes nothing.
    enum Resolved {
        Create(String, InstanceCreate),
        Add(InstanceAddToCollection),
    }
    let mut resolved = Vec::with_capacity(writes.len());
    for (index, write) in writes.into_iter().enumerate() {
        resolved.push(match write {
            PlannedWrite::Create {
                reference,
                class_name,
                properties,
                parent,
            } => {
                let properties = properties
                    .map(|p| resolve_refs(index, Value::Object(p), &id_of))
                    .transpose()?
                    .and_then(|v| match v {
                        Value::Object(map) => Some(map),
                        _ => None,
                    });
                let parent = parent.map(|p| id_of(index, &p)).transpose()?;
                Resolved::Create(
                    reference.clone(),
                    InstanceCreate {
                        class_name,
                        properties,
                        base_uri: Some(ids[&reference].clone()),
                        parent,
                        batch_id: None,
                    },
                )
            }
            PlannedWrite::Add {
                class_name,
                base_uri,
                collection,
                item,
            } => Resolved::Add(InstanceAddToCollection {
                class_name,
                base_uri,
                collection,
                item_uri: id_of(index, &item)?,
                batch_id: None,
            }),
        });
    }

    let batch = perspective.create_batch().await;
    let mut created = Vec::new();
    for (index, write) in resolved.into_iter().enumerate() {
        let outcome = match write {
            Resolved::Create(reference, mut params) => {
                params.batch_id = Some(batch.clone());
                create(perspective, agent, params).await.map(|made| {
                    created.push(PlannedCreated {
                        reference,
                        class_name: made.class_name,
                        base_uri: made.base_uri,
                    })
                })
            }
            Resolved::Add(mut params) => {
                params.batch_id = Some(batch.clone());
                add_to_collection(perspective, agent, params)
                    .await
                    .map(|_| ())
            }
        };
        if let Err(error) = outcome {
            perspective.discard_batch(&batch).await;
            return Err(PlanError::Write { index, error });
        }
    }
    if let Err(error) = perspective.commit_batch(batch, agent).await {
        return Err(PlanError::Commit { error });
    }
    Ok(created)
}

/// `value` with every `{ "ref": "<name>" }` object in it (at any depth) replaced by the id of the
/// record that ref names.
fn resolve_refs(
    index: usize,
    value: Value,
    id_of: &impl Fn(usize, &PlannedRecord) -> Result<String, PlanError>,
) -> Result<Value, PlanError> {
    Ok(match value {
        Value::Object(map) if map.len() == 1 && map.get("ref").is_some_and(Value::is_string) => {
            let reference = map["ref"].as_str().unwrap_or_default().to_string();
            Value::String(id_of(index, &PlannedRecord::Created { reference })?)
        }
        Value::Object(map) => Value::Object(
            map.into_iter()
                .map(|(k, v)| Ok((k, resolve_refs(index, v, id_of)?)))
                .collect::<Result<_, PlanError>>()?,
        ),
        Value::Array(items) => Value::Array(
            items
                .into_iter()
                .map(|v| resolve_refs(index, v, id_of))
                .collect::<Result<_, _>>()?,
        ),
        other => other,
    })
}
