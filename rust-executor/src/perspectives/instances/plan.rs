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
//! A plan registers no class: a create of one of the executor's own classes (a flow run, an
//! interpretation run) that the perspective does not hold yet is refused, since registering it
//! is a write no batch can take back.
//!
//! Nor does it write a property stored through a language of its own (a `resolveLanguage` other
//! than `literal`): that value is created in the language while the plan runs, outside the batch,
//! and a plan refused later could not take it back. Such a write is refused before anything is
//! written.
//!
//! A parent the plan creates is taken to be of the class it is created as. A committed parent is
//! of every class whose shape it matches, so where two classes share their required links and only
//! one of them holds children through a relation of its own, a part under a parent of the plan can
//! be linked otherwise than under the same parent once committed.
//!
//! Only creates and adds. A plan changes no existing property and removes nothing: what it can do
//! to records it did not make is add them to a collection, the one write a reply into a thread
//! or an item into a list needs, and the write least able to lose anything.

use super::write::{add_to_collection, create_under, InstanceAddToCollection, InstanceCreate};
use super::{generate_instance_uri, resolve_class, InstanceError};
use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::HashMap;

tokio::task_local! {
    /// Set while a plan's writes run. [`in_plan`] reads it.
    static IN_PLAN: ();
}

/// Whether the caller runs inside a plan's writes. The property encoder
/// (`PerspectiveInstance::resolve_property_value`) refuses a value for a language of its own
/// here, whatever shape it reads: the check before the batch reads the shape once, and a shape
/// registered or synced in between could make a property the plan sets language-stored.
pub(crate) fn in_plan() -> bool {
    IN_PLAN.try_with(|_| ()).is_ok()
}

#[cfg(test)]
tokio::task_local! {
    /// Test seam: skip the check before the batch, as a shape changed after it would.
    pub(super) static SKIP_LANGUAGE_CHECK: ();
}

#[cfg(test)]
fn skip_language_check() -> bool {
    SKIP_LANGUAGE_CHECK.try_with(|_| ()).is_ok()
}
#[cfg(not(test))]
fn skip_language_check() -> bool {
    false
}

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
    /// A create of one of the executor's own classes, not registered in the perspective yet.
    UnregisteredClass {
        index: usize,
        class_name: String,
    },
    /// A value for a property stored through a language of its own (`resolveLanguage` other than
    /// `literal`).
    LanguageProperty {
        index: usize,
        class_name: String,
        property: String,
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

    // Every create's class, by target class IRI, before anything is written. A child is put under
    // its parent through the parent class's own `children` relation, and the store cannot tell a
    // parent's class while the parent is only staged in the plan's batch.
    // A property stored through a language of its own is refused here too (see the module doc).
    let mut class_of: HashMap<String, String> = HashMap::new();
    for (index, write) in writes.iter().enumerate() {
        let (reference, class_name, named): (Option<&String>, &String, Vec<&String>) = match write {
            PlannedWrite::Create {
                reference,
                class_name,
                properties,
                ..
            } => (
                Some(reference),
                class_name,
                properties.iter().flat_map(|p| p.keys()).collect(),
            ),
            PlannedWrite::Add {
                class_name,
                collection,
                ..
            } => (None, class_name, vec![collection]),
        };
        let unregistered = crate::perspectives::hardwired_class::unstored(perspective, class_name)
            .map_err(|error| PlanError::Write {
                index,
                error: InstanceError::Failed {
                    class_name: class_name.clone(),
                    step: super::WriteStep::CreateSubject,
                    error,
                },
            })?;
        if unregistered.is_some() && reference.is_some() {
            return Err(PlanError::UnregisteredClass {
                index,
                class_name: class_name.clone(),
            });
        }
        let (_, shape) = resolve_class(perspective, class_name)
            .await
            .map_err(|error| PlanError::Write { index, error })?;
        if let Some(property) = shape.properties.iter().find(|p| {
            p.resolve_language
                .as_deref()
                .is_some_and(|language| language != "literal")
                && named.iter().any(|n| n.eq_ignore_ascii_case(&p.name))
                && !skip_language_check()
        }) {
            return Err(PlanError::LanguageProperty {
                index,
                class_name: class_name.clone(),
                property: property.name.clone(),
            });
        }
        if let Some(reference) = reference {
            class_of.insert(ids[reference].clone(), shape.target_class.clone());
        }
    }

    // Resolve every ref before writing anything, so a bad one writes nothing.
    enum Resolved {
        Create(String, InstanceCreate, Option<String>),
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
                // Value by value: the properties map is never a ref itself, even one whose only
                // property is named `ref`.
                let properties = properties
                    .map(|p| {
                        p.into_iter()
                            .map(|(k, v)| Ok((k, resolve_refs(index, v, &id_of)?)))
                            .collect::<Result<Map<String, Value>, PlanError>>()
                    })
                    .transpose()?;
                let parent_class = match &parent {
                    Some(PlannedRecord::Created { .. }) => parent
                        .as_ref()
                        .map(|p| id_of(index, p))
                        .transpose()?
                        .and_then(|id| class_of.get(&id).cloned()),
                    _ => None,
                };
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
                    parent_class,
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
    // Under `IN_PLAN`: a language value is refused at the write too (see [`in_plan`]).
    let written: Result<(), PlanError> = IN_PLAN
        .scope((), async {
            for (index, write) in resolved.into_iter().enumerate() {
                let outcome = match write {
                    Resolved::Create(reference, mut params, parent_class) => {
                        params.batch_id = Some(batch.clone());
                        let parent_classes = parent_class.map(|c| vec![c]);
                        create_under(perspective, agent, params, parent_classes.as_deref())
                            .await
                            .map(|made| {
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
            Ok(())
        })
        .await;
    written?;
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
