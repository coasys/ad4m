//! `mutate`: an ordered list of typed ops (create, update, delete) written in one batch and
//! committed once. The planner over [`super::write`]: each op runs the validated write of its
//! kind, so the shape checks of a single write run once per op and nothing is planned twice.
//!
//! - A later op sees the earlier ones: an instance an earlier op created is found, one it
//!   removed is not (see [`Pending`]), and the link reads of the writes see the batch's staged
//!   links.
//! - The call applies all its ops or none. Without a `batch` the ops stage into a batch of
//!   their own that is committed at the end and discarded on failure. With a caller's `batch`
//!   they stage into a private copy of it, merged into the caller's batch in one step when the
//!   call succeeds and dropped when it fails; nothing is committed. A caller's batch that is
//!   gone by the merge (committed or discarded meanwhile) fails the call, and so does one that
//!   changed since the copy (a raw add, a raw update, another call's merge): the copy's result
//!   may no longer follow from it, so nothing is merged (`BatchConflict`, retryable).
//! - An update op writes its `set` properties, then its `remove` items, then its `add` items.

use super::write::{
    add_to_collection_with, create_with, remove_from_collection_with, remove_with,
    require_members_left, update_with,
};
use super::{
    class_properties, find_property, instance_uri, link_target, resolve_class,
    InstanceAddToCollection, InstanceCreate, InstanceError, InstanceRemove,
    InstanceRemoveFromCollection, InstanceUpdate, Pending, WriteStep,
};
use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::{LinksChanged, MergeOutcome, PerspectiveInstance};
use crate::types::{Link, LinkStatus};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::BTreeMap;
use ts_rs::TS;

/// One write of a `mutate` call.
#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(tag = "op", rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub enum MutateOp {
    /// Create an instance of `class`.
    Create {
        /// The subject class, as the perspective names it.
        class: String,
        /// The new instance's URI. A random `ad4m://obj/...` one when absent.
        #[serde(default)]
        #[ts(optional)]
        id: Option<String>,
        /// Values by property name; an array of item URIs per collection. Required properties
        /// must be present.
        #[serde(default)]
        #[ts(optional, type = "Record<string, unknown>")]
        values: Map<String, Value>,
        /// Where to put the new instance.
        #[serde(default)]
        #[ts(optional)]
        parent: Option<ParentRef>,
    },
    /// Change an instance that exists, or that an earlier op of the call created.
    Update {
        class: String,
        id: String,
        /// Properties to set; the others stay. `null` or `""` clears one; an array on a
        /// relation sets its members.
        #[serde(default)]
        #[ts(optional, type = "Record<string, unknown>")]
        set: Map<String, Value>,
        /// Items to add, by collection name.
        #[serde(default)]
        #[ts(optional, type = "Record<string, string[]>")]
        add: BTreeMap<String, Vec<String>>,
        /// Items to remove, by collection name.
        #[serde(default)]
        #[ts(optional, type = "Record<string, string[]>")]
        remove: BTreeMap<String, Vec<String>>,
    },
    /// Remove an instance: run its class's destructor, else remove its declared predicates,
    /// then every link that points at it.
    Delete { class: String, id: String },
}

/// Where a created instance goes.
#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(untagged, rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub enum ParentRef {
    /// Into the `relation` collection of the parent, an instance of `model`. Without `model`,
    /// the one class of the parent instance that declares `relation`.
    Relation {
        id: String,
        relation: String,
        #[serde(default)]
        #[ts(optional)]
        model: Option<String>,
    },
    /// One plain link `parent --predicate--> child`.
    Predicate { id: String, predicate: String },
    /// Under the parent's own `children` relation, else `ad4m://has_child`.
    Contained { id: String },
}

/// What a `mutate` call returns besides the ids.
#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(tag = "kind", rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub enum Returns {
    // Struct variants, not unit ones: serde's `deny_unknown_fields` does not reach a unit
    // variant of a tagged enum.
    /// The ids only.
    None {},
    /// The ids only; the default.
    Ids {},
    /// Also each created or updated instance as a read shows it after the commit, with only
    /// `properties` of it when given. Needs a call without `batch`: a caller's batch is not
    /// committed yet.
    Rows {
        #[serde(default)]
        #[ts(optional)]
        properties: Option<Vec<String>>,
    },
}

/// What a `mutate` call did.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[ts(export)]
pub struct PerspectiveMutateResult {
    /// One id per op, in order: the new instance's id for a create, the id the op named else.
    pub ids: Vec<String>,
    /// With `returns: rows`: one entry per op, `null` for a delete.
    #[ts(optional, type = "Array<Record<string, unknown> | null>")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub rows: Option<Vec<Option<Value>>>,
}

/// An op of a `mutate` call that was refused or failed. Nothing of the call was kept.
#[derive(Debug)]
pub struct MutateError {
    /// The op that failed; `None` for the call (an unknown `batch`, a failed commit).
    pub op_index: Option<usize>,
    pub error: InstanceError,
}

/// What one op wrote.
pub(crate) struct Mutated {
    pub class_name: String,
    pub id: String,
    pub deleted: bool,
}

fn call_error(error: InstanceError) -> MutateError {
    MutateError {
        op_index: None,
        error,
    }
}

/// Run `ops` in order, all or none; see the module doc.
pub(crate) async fn mutate(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    ops: Vec<MutateOp>,
    batch: Option<String>,
) -> Result<Vec<Mutated>, MutateError> {
    if ops.is_empty() {
        return Ok(Vec::new());
    }
    match batch {
        Some(batch_id) => {
            // The ops stage into a private batch that starts as a copy of the caller's; a
            // success merges what they changed into the caller's batch in one step, a failure
            // drops the private batch. The caller's batch is touched only by that merge.
            let Some((private, base)) = perspective.fork_batch(&batch_id).await else {
                return Err(call_error(InstanceError::BatchNotFound { batch_id }));
            };
            let guard = DropDiscard::new(perspective, &private);
            match run_ops(perspective, agent, ops, &private).await {
                Ok(done) => {
                    // Disarmed only once the merge has returned: a call cancelled while
                    // the merge waits for the batch store still discards the fork.
                    let outcome = perspective.merge_batch(&batch_id, &private, base).await;
                    guard.disarm();
                    match outcome {
                        MergeOutcome::Merged => Ok(done),
                        MergeOutcome::Gone => {
                            Err(call_error(InstanceError::BatchNotFound { batch_id }))
                        }
                        MergeOutcome::Conflict => {
                            Err(call_error(InstanceError::BatchConflict { batch_id }))
                        }
                    }
                }
                Err(error) => {
                    perspective.discard_batch(&private).await;
                    guard.disarm();
                    Err(error)
                }
            }
        }
        None => {
            let batch_id = perspective.create_batch().await;
            let guard = DropDiscard::new(perspective, &batch_id);
            match run_ops(perspective, agent, ops, &batch_id).await {
                Ok(done) => {
                    let class_name = done.first().map(|m| m.class_name.clone());
                    // Disarmed once the commit has returned, whatever it answered: a
                    // cancel while it waits for the batch store still discards the batch.
                    // What the ops replaced must still be what they read (an empty property
                    // included), or two calls setting one single-valued property would both
                    // commit and leave two values: the commit checks it under the store's
                    // write lock.
                    let committed = match perspective.expected_by_batch(&batch_id).await {
                        Ok(Some(expected)) => {
                            perspective
                                .commit_batch_if(batch_id.clone(), agent, &expected)
                                .await
                        }
                        Ok(None) => Err(anyhow::anyhow!("No batch found with given UUID")),
                        Err(error) => {
                            perspective.discard_batch(&batch_id).await;
                            Err(error)
                        }
                    };
                    guard.disarm();
                    committed.map_err(|error| {
                        if error.is::<LinksChanged>() {
                            return call_error(InstanceError::ConcurrentWrite);
                        }
                        call_error(InstanceError::Failed {
                            class_name: class_name.unwrap_or_default(),
                            step: WriteStep::Commit,
                            error,
                        })
                    })?;
                    Ok(done)
                }
                Err(error) => {
                    perspective.discard_batch(&batch_id).await;
                    guard.disarm();
                    Err(error)
                }
            }
        }
    }
}

/// Discards a batch when it is dropped still armed: a call the client cancels drops the
/// handler's future between two awaits, and the private batch it worked in must not stay
/// open (the caller's own batch is untouched until the merge, so nothing else needs undoing).
struct DropDiscard {
    perspective: PerspectiveInstance,
    batch_id: String,
    armed: bool,
}

impl DropDiscard {
    fn new(perspective: &PerspectiveInstance, batch_id: &str) -> Self {
        DropDiscard {
            perspective: perspective.clone(),
            batch_id: batch_id.to_string(),
            armed: true,
        }
    }

    /// The call ends the batch itself from here on.
    fn disarm(mut self) {
        self.armed = false;
    }
}

impl Drop for DropDiscard {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        let perspective = self.perspective.clone();
        let batch_id = std::mem::take(&mut self.batch_id);
        if let Ok(runtime) = tokio::runtime::Handle::try_current() {
            runtime.spawn(async move {
                perspective.discard_batch(&batch_id).await;
            });
        }
    }
}

async fn run_ops(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    ops: Vec<MutateOp>,
    batch_id: &str,
) -> Result<Vec<Mutated>, MutateError> {
    let mut pending = Pending {
        json_objects: true,
        ..Pending::default()
    };
    let mut done = Vec::with_capacity(ops.len());
    for (index, op) in ops.into_iter().enumerate() {
        // What the id's classes were before the op: a delete through an abstract class removes
        // the classes it reads, which only the state before it can name.
        let before = match &op {
            MutateOp::Delete { id, .. } => instance_uri("base_uri", id)
                .ok()
                .map(|id| pending.staged(&id)),
            _ => None,
        };
        let mutated = run_op(perspective, agent, op, batch_id, &mut pending)
            .await
            .map_err(|error| MutateError {
                op_index: Some(index),
                error,
            })?;
        if mutated.deleted {
            let at = |error| MutateError {
                op_index: Some(index),
                error,
            };
            let (class_name, shape) = resolve_class(perspective, &mutated.class_name)
                .await
                .map_err(at)?;
            let is_abstract = perspective
                .is_abstract_class(&class_name)
                .map_err(|error| {
                    at(InstanceError::Failed {
                        class_name: class_name.clone(),
                        step: WriteStep::ReadShape,
                        error,
                    })
                })?;
            if is_abstract {
                // The classes the abstract class reached, each as gone.
                let reached = perspective
                    .write_targets(
                        &class_name,
                        &link_target(&mutated.id),
                        &before.unwrap_or_default(),
                    )
                    .map_err(|error| {
                        at(InstanceError::Failed {
                            class_name: class_name.clone(),
                            step: WriteStep::RemoveSubject,
                            error,
                        })
                    })?;
                for target in &reached {
                    pending.removed(target, &mutated.id);
                }
            }
            pending.removed(&shape.target_class, &mutated.id);
        }
        done.push(mutated);
        #[cfg(test)]
        if index == 0 {
            let hook = perspective.mutate_gap_hook.lock().unwrap().take();
            if let Some(hook) = hook {
                hook().await;
            }
        }
    }
    Ok(done)
}

/// The class of the instance `id` that declares `relation`; one class must, or the caller
/// names it. The classes of `id` are those the store shows plus those this call created it as,
/// less those it deleted it from.
fn parent_class(
    perspective: &PerspectiveInstance,
    id: &str,
    relation: &str,
    pending: &Pending,
) -> Result<String, InstanceError> {
    let parent_id = instance_uri("parent", id)?;
    let parent = link_target(&parent_id);
    let mut candidates = perspective
        .classes_declaring(&parent, relation, &pending.staged(&parent_id))
        .map_err(|error| InstanceError::Failed {
            class_name: String::new(),
            step: WriteStep::ReadShape,
            error,
        })?;
    if candidates.len() == 1 {
        return Ok(candidates.remove(0));
    }
    Err(InstanceError::ParentClassUnresolved {
        parent: id.to_string(),
        relation: relation.to_string(),
        candidates,
    })
}

/// The `(class, collection)` pairs an `add` or `remove` on `class` writes. A concrete class is
/// itself. Through an abstract class (#1435) it is each of the record's classes it reads that
/// has the collection, under its own name for it.
fn collection_targets(
    perspective: &PerspectiveInstance,
    class: &str,
    id: &str,
    collection: &str,
    pending: &Pending,
) -> Result<Vec<(String, String)>, InstanceError> {
    let failed = |error| InstanceError::Failed {
        class_name: class.to_string(),
        step: WriteStep::ResolveCollection {
            collection: collection.to_string(),
        },
        error,
    };
    if !perspective.is_abstract_class(class).map_err(failed)? {
        return Ok(vec![(class.to_string(), collection.to_string())]);
    }
    let base = link_target(&instance_uri("base_uri", id)?);
    let targets = perspective
        .collection_targets(class, &base, collection, &pending.staged(&base))
        .map_err(failed)?;
    if targets.is_empty() {
        return Err(failed(anyhow::anyhow!(
            "none of the record's classes read through '{class}' has '{collection}'"
        )));
    }
    Ok(targets)
}

async fn run_op(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    op: MutateOp,
    batch_id: &str,
    pending: &mut Pending,
) -> Result<Mutated, InstanceError> {
    let batch = Some(batch_id.to_string());
    match op {
        MutateOp::Create {
            class,
            id,
            values,
            parent,
        } => {
            let contained = match &parent {
                Some(ParentRef::Contained { id }) => Some(id.clone()),
                _ => None,
            };
            let created = create_with(
                perspective,
                agent,
                InstanceCreate {
                    class_name: class,
                    properties: Some(values),
                    base_uri: id,
                    parent: contained,
                    batch_id: batch.clone(),
                },
                pending,
            )
            .await?;
            // The new instance is known to the parent writes below.
            let (_, shape) = resolve_class(perspective, &created.class_name).await?;
            pending.created(&shape.target_class, &created.class_name, &created.base_uri);
            match parent {
                Some(ParentRef::Relation {
                    id,
                    relation,
                    model,
                }) => {
                    let model = match model {
                        Some(model) => model,
                        None => parent_class(perspective, &id, &relation, pending)?,
                    };
                    // A single-valued relation (`@HasOne`) has one member: the child is what
                    // it points at now, a `set`, not an addition to a collection.
                    let (_, parent_shape) = resolve_class(perspective, &model).await?;
                    let single = find_property(&class_properties(&parent_shape), &relation)
                        .is_some_and(|info| info.relation().is_some() && !info.collection());
                    if single {
                        let mut properties = Map::new();
                        properties.insert(relation, Value::String(created.base_uri.clone()));
                        update_with(
                            perspective,
                            agent,
                            InstanceUpdate {
                                class_name: model,
                                base_uri: id,
                                properties,
                                batch_id: batch,
                            },
                            pending,
                        )
                        .await?;
                    } else {
                        add_to_collection_with(
                            perspective,
                            agent,
                            InstanceAddToCollection {
                                class_name: model,
                                base_uri: id,
                                collection: relation,
                                item_uri: created.base_uri.clone(),
                                batch_id: batch,
                            },
                            pending,
                        )
                        .await?;
                    }
                }
                Some(ParentRef::Predicate { id, predicate }) => {
                    let parent_uri = instance_uri("parent", &id)?;
                    let link = Link {
                        source: link_target(&parent_uri),
                        predicate: Some(predicate),
                        target: created.base_uri.clone(),
                    };
                    perspective
                        .add_link(link, LinkStatus::Shared, batch, agent)
                        .await
                        .map_err(|error| InstanceError::Failed {
                            class_name: created.class_name.clone(),
                            step: WriteStep::AddToParent { parent: id },
                            error,
                        })?;
                }
                Some(ParentRef::Contained { .. }) | None => {}
            }
            Ok(Mutated {
                class_name: created.class_name,
                id: created.base_uri,
                deleted: false,
            })
        }
        MutateOp::Update {
            class,
            id,
            set,
            add,
            remove,
        } => {
            if set.is_empty()
                && add.values().all(Vec::is_empty)
                && remove.values().all(Vec::is_empty)
            {
                return Err(InstanceError::EmptyProperties);
            }
            let mut class_name = class.clone();
            let mut base_uri = id.clone();
            if !set.is_empty() {
                let updated = update_with(
                    perspective,
                    agent,
                    InstanceUpdate {
                        class_name: class.clone(),
                        base_uri: id.clone(),
                        properties: set,
                        batch_id: batch.clone(),
                    },
                    pending,
                )
                .await?;
                class_name = updated.class_name;
                base_uri = updated.base_uri;
            }
            let mut emptied = Vec::new();
            for (collection, items) in remove {
                for (target_class, own) in
                    collection_targets(perspective, &class, &id, &collection, pending)?
                {
                    if !items.is_empty() {
                        emptied.push((target_class.clone(), own.clone()));
                    }
                    for item in &items {
                        let removed = remove_from_collection_with(
                            perspective,
                            agent,
                            InstanceRemoveFromCollection {
                                class_name: target_class.clone(),
                                base_uri: id.clone(),
                                collection: own.clone(),
                                item_uri: item.clone(),
                                batch_id: batch.clone(),
                            },
                            pending,
                        )
                        .await?;
                        if target_class == class {
                            class_name = removed.class_name;
                        }
                        base_uri = removed.base_uri;
                    }
                }
            }
            for (collection, items) in add {
                for (target_class, own) in
                    collection_targets(perspective, &class, &id, &collection, pending)?
                {
                    for item in &items {
                        let added = add_to_collection_with(
                            perspective,
                            agent,
                            InstanceAddToCollection {
                                class_name: target_class.clone(),
                                base_uri: id.clone(),
                                collection: own.clone(),
                                item_uri: item.clone(),
                                batch_id: batch.clone(),
                            },
                            pending,
                        )
                        .await?;
                        if target_class == class {
                            class_name = added.class_name;
                        }
                        base_uri = added.base_uri;
                    }
                }
            }
            // After the additions: replacing the last member in one update is not emptying.
            for (target_class, own) in emptied {
                require_members_left(
                    perspective,
                    &target_class,
                    &own,
                    &id,
                    batch.as_deref(),
                    pending,
                )
                .await?;
            }
            Ok(Mutated {
                class_name,
                id: base_uri,
                deleted: false,
            })
        }
        MutateOp::Delete { class, id } => {
            let removed = remove_with(
                perspective,
                agent,
                InstanceRemove {
                    class_name: class,
                    base_uri: id,
                    batch_id: batch,
                },
                pending,
            )
            .await?;
            Ok(Mutated {
                class_name: removed.class_name,
                id: removed.base_uri,
                deleted: true,
            })
        }
    }
}
