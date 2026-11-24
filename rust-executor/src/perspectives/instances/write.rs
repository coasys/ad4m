//! The five validated writes: create, update, remove, add to and remove from a collection.
//!
//! Each takes the perspective, the agent that writes ([`AgentContext`], the caller's
//! `acts_as()`), and typed params, checks the write against the class's shape before writing
//! anything, and returns a typed result or an [`InstanceError`].
//!
//! Batches: a write without `batch_id` lands in a batch of its own that it commits (a create or
//! update) or writes its links one by one (a collection write, a remove's cascade). With
//! `batch_id` it writes into the caller's open batch and leaves it open: nothing is visible
//! until the caller commits. A failure part way through a write into the caller's batch leaves
//! what it staged there; the caller discards the batch (`perspective.discardBatch`) rather than
//! commit it to keep the calls that succeeded — earlier calls' staged work is the caller's,
//! not this write's, to throw away. Reads the write does of the links it manages see the
//! batch's staged work (`get_links_effective`); instance existence reads committed state
//! only, except in a `mutate` call, which overlays the instances its own earlier ops created or
//! removed ([`super::Pending`]), so create-then-update of a new instance in one call works.
//! In separate calls into one batch, the later call fails `notFound`: the earlier one only
//! staged the instance, and the store does not show it before the commit.

use super::validate::{resolve_collection, value_text};
use super::{
    check_setters, class_properties, coerce_scalar, decoded_literal, fetch_instance, find_property,
    generate_instance_uri, instance_uri, link_target, property_target, property_value_target,
    remove_incoming_links_of, require_instance, resolve_class, resolve_writable_class,
    subject_class, validate_properties, InstanceError, Pending, WriteMode, WriteStep,
};
use crate::agent::AgentContext;
use crate::mcp::shacl;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};
use deno_core::anyhow::anyhow;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::HashMap;
use ts_rs::TS;

/// Create an instance of a subject class.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
pub struct InstanceCreate {
    /// The subject class, as the perspective names it (matched case-insensitively).
    pub class_name: String,
    /// Values by property name: one JSON value per single-valued property (of the property's
    /// type), an array of item URIs per relation. `null` or `""` writes nothing. Required
    /// properties must be present.
    #[ts(optional, type = "Record<string, unknown>")]
    pub properties: Option<Map<String, Value>>,
    /// The new instance's URI. A random `ad4m://obj/…` one when absent; a bare string becomes a
    /// literal URI.
    pub base_uri: Option<String>,
    /// A node to put the new instance under, through the parent class's own `children` relation
    /// when it has one, else `ad4m://has_child`.
    pub parent: Option<String>,
    /// An open batch (`perspective.createBatch`) to stage the write into; it lands on the
    /// batch's commit. Absent: the create commits on its own. On a non-validation error the
    /// batch may hold a partial write — discard it (`perspective.discardBatch`), don't commit
    /// it.
    pub batch_id: Option<String>,
}

/// Set properties of an instance; the others stay as they are.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
pub struct InstanceUpdate {
    pub class_name: String,
    pub base_uri: String,
    /// Values by property name, at least one. `null` or `""` clears a property (a required one
    /// is refused); an array on a relation sets its members.
    #[ts(type = "Record<string, unknown>")]
    pub properties: Map<String, Value>,
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
    pub batch_id: Option<String>,
}

/// Remove an instance: every link from and to it. Through an abstract class (#1435), the
/// destructor of each of the record's classes it reads.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
pub struct InstanceRemove {
    pub class_name: String,
    pub base_uri: String,
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
    pub batch_id: Option<String>,
}

/// Add an item to a collection of an instance.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
pub struct InstanceAddToCollection {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    /// The item, usually another instance's URI.
    pub item_uri: String,
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
    pub batch_id: Option<String>,
}

/// Remove an item from a collection of an instance (the membership link only).
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
pub struct InstanceRemoveFromCollection {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    /// The item as a read of the instance lists it.
    pub item_uri: String,
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
    pub batch_id: Option<String>,
}

/// The items a create put into one collection.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
pub struct CollectionItems {
    pub collection: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct InstanceCreated {
    /// The class's canonical name.
    pub class_name: String,
    /// The new instance's URI (its id).
    pub base_uri: String,
    pub properties_set: Vec<String>,
    pub collections_set: Vec<CollectionItems>,
    /// The parent it was put under, when one was named.
    pub parent: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct InstanceUpdated {
    pub class_name: String,
    pub base_uri: String,
    pub updated_properties: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct InstanceRemoved {
    pub class_name: String,
    pub base_uri: String,
    /// The links the remove took from the effective graph: the instance's own (its destructor's,
    /// or its declared predicates') and the incoming ones. Inside a caller's batch they are
    /// staged, not committed — including staged additions the call cancelled.
    pub links_removed: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct CollectionItemAdded {
    pub class_name: String,
    pub base_uri: String,
    /// The collection's canonical name.
    pub collection: String,
    pub item_uri: String,
    /// 0 when the item was in the collection already (as the batch's effective graph has it).
    pub links_added: usize,
    pub already_member: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct CollectionItemRemoved {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    /// 0 when the item was not in the collection (as the batch's effective graph has it).
    /// Inside a batch, a member that only the batch had added counts too — it leaves by
    /// cancelling the staged addition.
    pub links_removed: usize,
}

/// The batch a create or update writes into.
enum Batch {
    /// The caller's: left open.
    Caller(String),
    /// The write's own: committed at the end, discarded on failure.
    Own(String),
}

impl Batch {
    async fn open(
        perspective: &PerspectiveInstance,
        batch_id: Option<String>,
    ) -> Result<Self, InstanceError> {
        match batch_id {
            Some(id) => {
                check_batch(perspective, Some(&id)).await?;
                Ok(Batch::Caller(id))
            }
            None => Ok(Batch::Own(perspective.create_batch().await)),
        }
    }

    fn id(&self) -> String {
        match self {
            Batch::Caller(id) | Batch::Own(id) => id.clone(),
        }
    }

    /// A step failed: drop the write's own batch so it does not sit in the store until the
    /// timeout sweep. The caller's batch is the caller's to discard.
    async fn abandon(&self, perspective: &PerspectiveInstance) {
        if let Batch::Own(id) = self {
            perspective.discard_batch(id).await;
        }
    }

    async fn finish(
        self,
        perspective: &mut PerspectiveInstance,
        agent: &AgentContext,
        class_name: &str,
    ) -> Result<(), InstanceError> {
        match self {
            Batch::Caller(_) => Ok(()),
            Batch::Own(id) => perspective
                .commit_batch(id, agent)
                .await
                .map(|_| ())
                .map_err(|error| failed(class_name, WriteStep::Commit, error)),
        }
    }
}

/// `BatchNotFound` unless `batch_id` is absent or names an open batch.
async fn check_batch(
    perspective: &PerspectiveInstance,
    batch_id: Option<&str>,
) -> Result<(), InstanceError> {
    match batch_id {
        Some(id) if !perspective.has_batch(id).await => Err(InstanceError::BatchNotFound {
            batch_id: id.to_string(),
        }),
        _ => Ok(()),
    }
}

fn failed(class_name: &str, step: WriteStep, error: deno_core::anyhow::Error) -> InstanceError {
    InstanceError::Failed {
        class_name: class_name.to_string(),
        step,
        error,
    }
}

fn invalid(class_name: &str, errors: Vec<super::ValidationError>) -> InstanceError {
    InstanceError::Validation {
        class_name: class_name.to_string(),
        errors,
    }
}

/// The texts a collection item is looked for by, each with its literal encoding: the text the
/// caller gave and, when validation coerces it, the coerced value's — which is what
/// `add_to_collection` stores, so `TRUE` finds a stored `true` and `+005` a stored `5`.
fn item_spellings(given: &str, coerced: Option<&Value>) -> Vec<(String, String)> {
    let mut texts = vec![given.to_string()];
    if let Some(text) = coerced.map(value_text) {
        if text != given {
            texts.push(text);
        }
    }
    texts
        .into_iter()
        .map(|text| {
            let encoded = link_target(&text);
            (text, encoded)
        })
        .collect()
}

/// Whether a stored collection link target denotes the item. The target may be the item URI
/// itself (references), the literal-encoded form of a bare value, or a signed literal envelope
/// whose payload is the value — accept all three spellings so the caller can pass back whatever
/// a read showed them.
fn target_matches(target: &str, spellings: &[(String, String)]) -> bool {
    let decoded = decoded_literal(target);
    spellings
        .iter()
        .any(|(wanted, encoded)| target == wanted || target == encoded || decoded == *wanted)
}

/// Create an instance of a subject class, its collection items and its parent link, in one
/// batch.
pub async fn create(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceCreate,
) -> Result<InstanceCreated, InstanceError> {
    create_with(perspective, agent, params, &Pending::default()).await
}

/// [`create`], with what the earlier ops of the same `mutate` call did.
pub(crate) async fn create_with(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceCreate,
    pending: &Pending,
) -> Result<InstanceCreated, InstanceError> {
    // A hard-wired class (a flow run, an auto-processor) is the executor's to register: the
    // first instance written registers it, so resolving it below finds its shape.
    crate::perspectives::hardwired_class::ensure_before_write(
        perspective,
        &params.class_name,
        agent,
    )
    .await
    .map_err(|error| InstanceError::Failed {
        class_name: params.class_name.clone(),
        step: WriteStep::CreateSubject,
        error,
    })?;
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;

    let props = params.properties.unwrap_or_default();
    let validated = validate_properties(&shape, &props, WriteMode::Create, pending.json_objects)
        .map_err(|errors| invalid(&class_name, errors))?;
    let setter_errors = check_setters(perspective, &shape, &validated.scalars).await;
    if !setter_errors.is_empty() {
        return Err(invalid(&class_name, setter_errors));
    }

    // Normalize once: a bare caller-supplied id ("my-channel") becomes a literal URI here, and
    // this single value is used for the subject, the collection links AND the parent link —
    // otherwise they would target different nodes.
    let base_uri = match params.base_uri.as_deref() {
        Some(uri) if !uri.trim().is_empty() => link_target(uri.trim()),
        _ => generate_instance_uri(),
    };
    // Three arms, not `if let Ok(Some(_))`: a store/query failure must not read as "no instance
    // found" and let the create land on top of an existing instance, merging its property
    // links.
    // An id this call removed or created as another class is not found again in the store.
    let existing = match pending.lookup(&shape.target_class, &base_uri) {
        Some(true) => Ok(Some(Value::Null)),
        Some(false) => Ok(None),
        None => fetch_instance(perspective, &class_name, &base_uri).await,
    };
    match existing {
        Ok(Some(_)) => {
            return Err(InstanceError::AlreadyExists {
                class_name,
                base_uri,
            })
        }
        Ok(None) => {}
        Err(e) => {
            return Err(failed(
                &class_name,
                WriteStep::CheckExisting { base_uri },
                anyhow!(e),
            ))
        }
    }

    // Resolve every collection predicate BEFORE creating the subject, so a bad collection name
    // fails the whole call cleanly instead of leaving a live instance with partial collection
    // membership. Collection links are written directly here, not through the collection's
    // `adder` action, so the status has to be resolved from the property shape
    // (`ad4m://local`) for each one.
    let mut collection_links: HashMap<String, (String, LinkStatus)> = HashMap::new();
    for (collection, _) in &validated.collections {
        let status =
            shacl::resolve_property_link_status(perspective, &class_name, collection).await;
        let predicate = shacl::resolve_property_predicate(perspective, &class_name, collection)
            .await
            .map_err(|e| {
                failed(
                    &class_name,
                    WriteStep::ResolveCollection {
                        collection: collection.clone(),
                    },
                    anyhow!(e),
                )
            })?;
        collection_links.insert(collection.clone(), (predicate, status));
    }

    let initial_values = if validated.scalars.is_empty() {
        None
    } else {
        Some(Value::Object(validated.scalars.clone()))
    };

    // One batch for the whole create — subject, collection links, and the parent link land
    // atomically or not at all: no partially-constructed instance is ever observable, by peers
    // or by a failed call's aftermath.
    let batch = Batch::open(perspective, params.batch_id).await?;
    if let Err(e) = perspective
        .create_subject_raw(
            subject_class(&class_name),
            base_uri.clone(),
            initial_values,
            Some(batch.id()),
            agent,
        )
        .await
    {
        batch.abandon(perspective).await;
        return Err(failed(&class_name, WriteStep::CreateSubject, e));
    }

    // Collections: one link per item, same path as `add_to_collection`.
    let mut collections_set = Vec::with_capacity(validated.collections.len());
    for (collection, items) in &validated.collections {
        let (predicate, status) = collection_links[collection].clone();
        let mut added = Vec::with_capacity(items.len());
        let mut targets = Vec::with_capacity(items.len());
        for item in items {
            let item_str = match item {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            };
            // The validated value, not its text: re-parsing would turn the string "false" of
            // a string collection into a boolean.
            let target =
                property_value_target(perspective, &class_name, collection, item, agent).await;
            let link = Link {
                source: base_uri.clone(),
                predicate: Some(predicate.clone()),
                target: target.clone(),
            };
            targets.push(target);
            if let Err(e) = perspective
                .add_link(link, status.clone(), Some(batch.id()), agent)
                .await
            {
                batch.abandon(perspective).await;
                return Err(failed(
                    &class_name,
                    WriteStep::AddCollectionItem {
                        collection: collection.clone(),
                        item: item_str,
                    },
                    e,
                ));
            }
            added.push(item_str);
        }
        // An ordered relation also needs the entries that place its members.
        if let Err(e) = perspective
            .order_appended(
                &base_uri,
                &predicate,
                &[],
                &targets,
                status.clone(),
                Some(batch.id()),
                agent,
            )
            .await
        {
            batch.abandon(perspective).await;
            return Err(failed(
                &class_name,
                WriteStep::ResolveCollection {
                    collection: collection.clone(),
                },
                e,
            ));
        }
        collections_set.push(super::CollectionItems {
            collection: collection.clone(),
            items: added,
        });
    }

    let parent = params.parent.filter(|s| !s.trim().is_empty());
    if let Some(parent) = &parent {
        let parent_id = link_target(parent.trim());
        if let Err(e) = perspective
            .add_to_container(
                &parent_id,
                &base_uri,
                &pending.created_targets(&parent_id),
                Some(batch.id()),
                agent,
            )
            .await
        {
            batch.abandon(perspective).await;
            return Err(failed(
                &class_name,
                WriteStep::AddToParent {
                    parent: parent.clone(),
                },
                e,
            ));
        }
    }

    batch.finish(perspective, agent, &class_name).await?;

    Ok(InstanceCreated {
        properties_set: validated.scalars.keys().cloned().collect(),
        class_name,
        base_uri,
        collections_set,
        parent,
    })
}

/// Set properties of an existing instance, in one batch. "Existing" means
/// committed: an instance created in the same batch is not found yet (`notFound`).
pub async fn update(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceUpdate,
) -> Result<InstanceUpdated, InstanceError> {
    update_with(perspective, agent, params, &Pending::default()).await
}

/// [`update`], with what the earlier ops of the same `mutate` call did.
pub(crate) async fn update_with(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceUpdate,
    pending: &Pending,
) -> Result<InstanceUpdated, InstanceError> {
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;
    if params.properties.is_empty() {
        return Err(InstanceError::EmptyProperties);
    }

    let validated = validate_properties(
        &shape,
        &params.properties,
        WriteMode::Update,
        pending.json_objects,
    )
    .map_err(|errors| invalid(&class_name, errors))?;
    // Through an abstract class (#1435) the record's own classes' setters write;
    // `update_subject_raw` checks those.
    let is_abstract = perspective
        .is_abstract_class(&class_name)
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    // What the class-side write gets: the values, `null` for a property to clear, an array
    // for the members a collection is set to.
    let mut values = validated.scalars.clone();
    for name in &validated.clears {
        values.insert(name.clone(), Value::Null);
    }
    for (name, items) in &validated.collections {
        values.insert(name.clone(), Value::Array(items.clone()));
    }
    if !is_abstract {
        let setter_errors = check_setters(perspective, &shape, &values).await;
        if !setter_errors.is_empty() {
            return Err(invalid(&class_name, setter_errors));
        }
    }

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(
        perspective,
        &class_name,
        &shape.target_class,
        &base_uri,
        pending,
    )
    .await?;

    // One batch for the whole update so remove-old + add-new per property lands atomically
    // (same reasoning as the per-class setters: without it a peer can observe the property as
    // unset in between).
    let batch = Batch::open(perspective, params.batch_id).await?;
    if let Err(e) = perspective
        .update_subject_raw(
            subject_class(&class_name),
            stored,
            Value::Object(values.clone()),
            Some(batch.id()),
            agent,
        )
        .await
    {
        batch.abandon(perspective).await;
        return Err(failed(&class_name, WriteStep::UpdateSubject, e));
    }
    batch.finish(perspective, agent, &class_name).await?;

    Ok(InstanceUpdated {
        updated_properties: values.keys().cloned().collect(),
        class_name,
        base_uri,
    })
}

/// Remove an instance and every link touching it — the committed ones and, in a caller's
/// batch, the ones the batch itself staged. Refuses when no committed instance of the class
/// lives at the URI (one created in the same batch is not found yet), so a wrong class or URI
/// cannot delete something else.
pub async fn remove(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceRemove,
) -> Result<InstanceRemoved, InstanceError> {
    remove_with(perspective, agent, params, &Pending::default()).await
}

/// How many links `uri` holds, on the batch's effective graph.
async fn links_held(
    perspective: &PerspectiveInstance,
    uri: &str,
    batch_id: Option<&str>,
) -> Result<usize, deno_core::anyhow::Error> {
    let links = perspective
        .get_links_effective(
            &LinkQuery {
                source: Some(uri.to_string()),
                ..Default::default()
            },
            batch_id,
        )
        .await?;
    Ok(links.len())
}

/// [`remove`], with what the earlier ops of the same `mutate` call did.
pub(crate) async fn remove_with(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceRemove,
    pending: &Pending,
) -> Result<InstanceRemoved, InstanceError> {
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;
    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(
        perspective,
        &class_name,
        &shape.target_class,
        &base_uri,
        pending,
    )
    .await?;
    check_batch(perspective, params.batch_id.as_deref()).await?;

    // The class's side first: its destructor, else the links of its declared predicates;
    // through an abstract class (#1435), that of each of the record's classes it reads, so its
    // other classes keep their links. Then what points at the instance.
    let batch = params.batch_id.as_deref();
    let held_before = links_held(perspective, &stored, batch)
        .await
        .map_err(|e| failed(&class_name, WriteStep::ReadInstance, e))?;
    perspective
        .remove_subject_raw(
            subject_class(&class_name),
            stored.clone(),
            params.batch_id.clone(),
            agent,
        )
        .await
        .map_err(|e| failed(&class_name, WriteStep::RemoveSubject, e))?;
    let held_after = links_held(perspective, &stored, batch)
        .await
        .map_err(|e| failed(&class_name, WriteStep::ReadInstance, e))?;
    let held_removed = held_before.saturating_sub(held_after);

    // Outside a batch the cascade is not atomic, so on failure the links already removed stay
    // removed: the error says so, with the count.
    match remove_incoming_links_of(perspective, &stored, batch).await {
        Ok(removed) => Ok(InstanceRemoved {
            class_name,
            base_uri,
            links_removed: held_removed + removed,
        }),
        Err(failure) => Err(InstanceError::PartiallyRemoved {
            class_name,
            base_uri,
            removed: held_removed + failure.removed,
            step: failure.step,
            error: failure.error,
        }),
    }
}

/// Add an item to a collection property of an instance. Adding an item that is already in the
/// collection writes nothing (`linksAdded: 0`); membership is read on the batch's effective
/// graph, so a member the same batch dropped reads as out and one it added reads as in.
pub async fn add_to_collection(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceAddToCollection,
) -> Result<CollectionItemAdded, InstanceError> {
    add_to_collection_with(perspective, agent, params, &Pending::default()).await
}

/// [`add_to_collection`], with what the earlier ops of the same `mutate` call did.
pub(crate) async fn add_to_collection_with(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceAddToCollection,
    pending: &Pending,
) -> Result<CollectionItemAdded, InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(
        &class_name,
        &infos,
        &params.collection,
        &params.item_uri,
        false,
    )
    .map_err(|e| invalid(&class_name, vec![e]))?;
    let coerced = coerce_scalar(info, &Value::String(params.item_uri.clone()))
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(
        perspective,
        &class_name,
        &shape.target_class,
        &base_uri,
        pending,
    )
    .await?;
    check_batch(perspective, params.batch_id.as_deref()).await?;

    let predicate = shacl::resolve_property_predicate(perspective, &class_name, &collection)
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ResolveCollection {
                    collection: collection.clone(),
                },
                anyhow!(e),
            )
        })?;

    // Idempotent: the store dedups the raw triple, but every signed link expression gets its
    // own reifier, so adding the same item twice would show up as a duplicate member. Look for
    // an existing membership link first and leave the collection untouched if the item is
    // already in it — on the batch's effective graph, so a member the same batch dropped reads
    // as out and one it added reads as in.
    let wanted = params.item_uri.trim();
    let spellings = item_spellings(wanted, Some(&coerced));
    let existing = perspective
        .get_links_effective(
            &LinkQuery {
                source: Some(stored.clone()),
                predicate: Some(predicate.clone()),
                ..Default::default()
            },
            params.batch_id.as_deref(),
        )
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ReadCollection {
                    collection: collection.clone(),
                },
                e,
            )
        })?;
    if existing
        .iter()
        .any(|effective| target_matches(&effective.link.data.target, &spellings))
    {
        return Ok(CollectionItemAdded {
            class_name,
            base_uri,
            collection,
            item_uri: params.item_uri,
            links_added: 0,
            already_member: true,
        });
    }

    // Write the coerced value, so a string collection keeps "false" a string. A language-stored
    // collection takes the item as it is (coercion leaves it text), still JSON-parsed as before.
    let target = if info.stored_by_language().is_some() {
        property_target(
            perspective,
            &class_name,
            &collection,
            &params.item_uri,
            agent,
        )
        .await
    } else {
        property_value_target(perspective, &class_name, &collection, &coerced, agent).await
    };
    let members: Vec<(String, String)> = existing
        .iter()
        .map(|e| (e.link.data.target.clone(), e.link.timestamp.clone()))
        .collect();
    let link = Link {
        source: stored.clone(),
        predicate: Some(predicate.clone()),
        target: target.clone(),
    };
    // This path writes the membership link directly rather than running the collection's
    // `adder` action, so the action's `local` flag never reaches it — resolve the status from
    // the property shape instead, or a `local: true` collection would be gossiped by this path
    // alone.
    let status = if info.local() {
        LinkStatus::Local
    } else {
        LinkStatus::Shared
    };
    // The member and its ordering entry go in one batch: with no caller's, they commit
    // together or not at all.
    let batch = Batch::open(perspective, params.batch_id.clone()).await?;
    let step = || WriteStep::AddCollectionItem {
        collection: collection.clone(),
        item: params.item_uri.clone(),
    };
    if let Err(e) = perspective
        .add_link(link, status.clone(), Some(batch.id()), agent)
        .await
    {
        batch.abandon(perspective).await;
        return Err(failed(&class_name, step(), e));
    }
    // An ordered relation also needs the entry that places the new member.
    if let Err(e) = perspective
        .order_appended(
            &stored,
            &predicate,
            &members,
            &[target],
            status,
            Some(batch.id()),
            agent,
        )
        .await
    {
        batch.abandon(perspective).await;
        return Err(failed(&class_name, step(), e));
    }
    batch.finish(perspective, agent, &class_name).await?;
    Ok(CollectionItemAdded {
        class_name,
        base_uri,
        collection,
        item_uri: params.item_uri,
        links_added: 1,
        already_member: false,
    })
}

/// A required collection keeps a member: after the removals and additions of one update, an
/// instance whose `collection` has none left is refused, as `set: []` is. Replacing the last
/// member in the same update passes. Reads the batch's effective graph.
pub(super) async fn require_members_left(
    perspective: &PerspectiveInstance,
    class_name: &str,
    collection: &str,
    base_uri: &str,
    batch_id: Option<&str>,
    pending: &Pending,
) -> Result<(), InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, class_name).await?;
    let infos = class_properties(&shape);
    let Some(info) = find_property(&infos, collection).filter(|info| info.required()) else {
        return Ok(());
    };
    let base_uri = instance_uri("base_uri", base_uri)?;
    let stored = require_instance(
        perspective,
        &class_name,
        &shape.target_class,
        &base_uri,
        pending,
    )
    .await?;
    let predicate = shacl::resolve_property_predicate(perspective, &class_name, info.name())
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ResolveCollection {
                    collection: collection.to_string(),
                },
                anyhow!(e),
            )
        })?;
    let left = perspective
        .get_links_effective(
            &LinkQuery {
                source: Some(stored),
                predicate: Some(predicate),
                ..Default::default()
            },
            batch_id,
        )
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ReadCollection {
                    collection: collection.to_string(),
                },
                e,
            )
        })?;
    if left.is_empty() {
        return Err(invalid(
            &class_name,
            vec![super::ValidationError::on(
                info,
                super::ValidationProblem::MissingRequired,
                None,
            )],
        ));
    }
    Ok(())
}

/// Remove an item from a collection property of an instance: only the membership link, the item
/// itself is untouched. Removing an item that is not in the collection writes nothing
/// (`linksRemoved: 0`); membership is read on the batch's effective graph, and a member that
/// exists only inside the batch leaves by cancelling its staged addition.
pub async fn remove_from_collection(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceRemoveFromCollection,
) -> Result<CollectionItemRemoved, InstanceError> {
    remove_from_collection_with(perspective, agent, params, &Pending::default()).await
}

/// [`remove_from_collection`], with what the earlier ops of the same `mutate` call did.
pub(crate) async fn remove_from_collection_with(
    perspective: &mut PerspectiveInstance,
    _agent: &AgentContext,
    params: InstanceRemoveFromCollection,
    pending: &Pending,
) -> Result<CollectionItemRemoved, InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(
        &class_name,
        &infos,
        &params.collection,
        &params.item_uri,
        true,
    )
    .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(
        perspective,
        &class_name,
        &shape.target_class,
        &base_uri,
        pending,
    )
    .await?;
    check_batch(perspective, params.batch_id.as_deref()).await?;

    let predicate = shacl::resolve_property_predicate(perspective, &class_name, &collection)
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ResolveCollection {
                    collection: collection.clone(),
                },
                anyhow!(e),
            )
        })?;

    let wanted = params.item_uri.trim();
    // Members are stored as the coerced value. A value validation refuses is still looked for
    // as given: removal names what is there, it does not validate.
    let coerced = coerce_scalar(info, &Value::String(params.item_uri.clone())).ok();
    let spellings = item_spellings(wanted, coerced.as_ref());
    // The batch's effective graph, so a member the same batch dropped reads as out already
    // and one it added is found and removed.
    let links = perspective
        .get_links_effective(
            &LinkQuery {
                source: Some(stored),
                predicate: Some(predicate),
                ..Default::default()
            },
            params.batch_id.as_deref(),
        )
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::ReadCollection {
                    collection: collection.clone(),
                },
                e,
            )
        })?;
    let stored_targets: Vec<String> = links
        .iter()
        .map(|effective| effective.link.data.target.clone())
        .collect();
    let mut removed = 0usize;
    for effective in links {
        if !target_matches(&effective.link.data.target, &spellings) {
            continue;
        }
        let link = LinkExpression::from(effective.link);
        // Both presences can hold at once (`addLinkExpression` restages the stored
        // expression verbatim): the staged copies drop, and the committed copy is staged
        // for removal — one without the other would leave the membership behind.
        if effective.staged {
            if let Some(batch_id) = params.batch_id.as_deref() {
                perspective.cancel_batch_addition(batch_id, &link).await;
            }
        }
        if effective.committed {
            if let Err(e) = perspective.remove_link(link, params.batch_id.clone()).await {
                return Err(failed(
                    &class_name,
                    WriteStep::RemoveCollectionItem {
                        collection,
                        item: wanted.to_string(),
                        removed,
                    },
                    e,
                ));
            }
        }
        removed += 1;
    }

    // A collection whose property declares a custom `resolveLanguage` stores each member as an
    // `expression_create` address, not as the value the caller passed to `add_to_collection` —
    // so nothing here can match by value and "0 links removed, success" would be a silent lie.
    // Say what the stored addresses are instead.
    if removed == 0 && !stored_targets.is_empty() {
        if let Ok(Some(language)) = perspective
            .get_resolve_language_from_shacl(&class_name, &collection)
            .await
        {
            if language != "literal" {
                return Err(InstanceError::StoredAsExpressions {
                    class_name,
                    collection,
                    item_uri: wanted.to_string(),
                    language,
                    members: stored_targets,
                });
            }
        }
    }

    Ok(CollectionItemRemoved {
        class_name,
        base_uri,
        collection,
        item_uri: params.item_uri,
        links_removed: removed,
    })
}
