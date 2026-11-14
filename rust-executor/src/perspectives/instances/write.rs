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
//! what it staged there; the caller discards the batch.

use super::validate::resolve_collection;
use super::{
    check_setters, class_properties, coerce_scalar, decoded_literal, fetch_instance,
    generate_instance_uri, instance_uri, link_target, property_target, remove_all_links_of,
    require_instance, resolve_class, resolve_writable_class, subject_class, validate_properties,
    InstanceError, WriteMode, WriteStep,
};
use crate::agent::AgentContext;
use crate::mcp::shacl;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkQuery, LinkStatus};
use deno_core::anyhow::anyhow;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::HashMap;
use ts_rs::TS;

/// Create an instance of a subject class.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceCreate {
    /// The subject class, as the perspective names it (matched case-insensitively).
    pub class_name: String,
    /// Values by property name: one JSON value per single-valued property (of the property's
    /// type), an array of item URIs per collection. Required properties must be present.
    #[ts(optional, type = "Record<string, unknown>")]
    pub properties: Option<Map<String, Value>>,
    /// The new instance's URI. A random `ad4m://obj/…` one when absent; a bare string becomes a
    /// literal URI.
    pub base_uri: Option<String>,
    /// A node to put the new instance under, through the parent class's own `children` relation
    /// when it has one, else `ad4m://has_child`.
    pub parent: Option<String>,
    /// An open batch (`perspective.createBatch`) to write into. Absent: the create commits on
    /// its own.
    pub batch_id: Option<String>,
}

/// Set single-valued properties of an instance; the others stay as they are.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceUpdate {
    pub class_name: String,
    pub base_uri: String,
    /// Values by property name, at least one; single-valued properties only.
    #[ts(type = "Record<string, unknown>")]
    pub properties: Map<String, Value>,
    pub batch_id: Option<String>,
}

/// Remove an instance: every link from and to it. Through an abstract class (#1435), the
/// destructor of each of the record's classes it reads.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceRemove {
    pub class_name: String,
    pub base_uri: String,
    pub batch_id: Option<String>,
}

/// Add an item to a collection of an instance.
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceAddToCollection {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    /// The item, usually another instance's URI.
    pub item_uri: String,
    pub batch_id: Option<String>,
}

/// Remove an item from a collection of an instance (the membership link only).
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceRemoveFromCollection {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    /// The item as a read of the instance lists it.
    pub item_uri: String,
    pub batch_id: Option<String>,
}

/// The items a create put into one collection.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[ts(export)]
pub struct CollectionItems {
    pub collection: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
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
#[ts(export)]
pub struct InstanceUpdated {
    pub class_name: String,
    pub base_uri: String,
    pub updated_properties: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceRemoved {
    pub class_name: String,
    pub base_uri: String,
    /// The links removed; `None` through an abstract class, whose classes' destructors ran.
    pub links_removed: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CollectionItemAdded {
    pub class_name: String,
    pub base_uri: String,
    /// The collection's canonical name.
    pub collection: String,
    pub item_uri: String,
    /// 0 when the item was in the collection already.
    pub links_added: usize,
    pub already_member: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CollectionItemRemoved {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    /// 0 when the item was not in the collection.
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

/// Whether a stored collection link target denotes `wanted`. The target may be the item URI
/// itself (references), the literal-encoded form of a bare value, or a signed literal envelope
/// whose payload is the value — accept all three spellings so the caller can pass back whatever
/// a read showed them.
fn target_matches(target: &str, wanted: &str, encoded: &str) -> bool {
    target == wanted || target == encoded || decoded_literal(target) == wanted
}

/// Create an instance of a subject class, its collection items and its parent link, in one
/// batch.
pub async fn create(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceCreate,
) -> Result<InstanceCreated, InstanceError> {
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;

    let props = params.properties.unwrap_or_default();
    let validated = validate_properties(&shape, &props, WriteMode::Create)
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
    match fetch_instance(perspective, &class_name, &base_uri).await {
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
        .create_subject(
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
        for item in items {
            let item_str = match item {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            };
            let target =
                property_target(perspective, &class_name, collection, &item_str, agent).await;
            let link = Link {
                source: base_uri.clone(),
                predicate: Some(predicate.clone()),
                target,
            };
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
        collections_set.push(super::CollectionItems {
            collection: collection.clone(),
            items: added,
        });
    }

    let parent = params.parent.filter(|s| !s.trim().is_empty());
    if let Some(parent) = &parent {
        if let Err(e) = perspective
            .add_to_container(
                &link_target(parent.trim()),
                &base_uri,
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

/// Set single-valued properties of an existing instance, in one batch.
pub async fn update(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceUpdate,
) -> Result<InstanceUpdated, InstanceError> {
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;
    if params.properties.is_empty() {
        return Err(InstanceError::EmptyProperties);
    }

    let validated = validate_properties(&shape, &params.properties, WriteMode::Update)
        .map_err(|errors| invalid(&class_name, errors))?;
    // Through an abstract class (#1435) the record's own classes' setters write;
    // `update_subject` checks those.
    let is_abstract = perspective
        .is_abstract_class(&class_name)
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    if !is_abstract {
        let setter_errors = check_setters(perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return Err(invalid(&class_name, setter_errors));
        }
    }

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    require_instance(perspective, &class_name, &base_uri).await?;

    // One batch for the whole update so remove-old + add-new per property lands atomically
    // (same reasoning as the per-class setters: without it a peer can observe the property as
    // unset in between).
    let batch = Batch::open(perspective, params.batch_id).await?;
    if let Err(e) = perspective
        .update_subject(
            subject_class(&class_name),
            base_uri.clone(),
            Value::Object(validated.scalars.clone()),
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
        updated_properties: validated.scalars.keys().cloned().collect(),
        class_name,
        base_uri,
    })
}

/// Remove an instance and every link touching it. Refuses when no instance of the class lives
/// at the URI, so a wrong class or URI cannot delete something else.
pub async fn remove(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceRemove,
) -> Result<InstanceRemoved, InstanceError> {
    let (class_name, _shape) = resolve_class(perspective, &params.class_name).await?;
    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    require_instance(perspective, &class_name, &base_uri).await?;
    check_batch(perspective, params.batch_id.as_deref()).await?;

    // Through an abstract class (#1435), the destructors of the record's classes it reads: the
    // cascade below would delete its other classes.
    let is_abstract = perspective
        .is_abstract_class(&class_name)
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    if is_abstract {
        perspective
            .remove_subject(
                subject_class(&class_name),
                base_uri.clone(),
                params.batch_id,
                agent,
            )
            .await
            .map_err(|e| failed(&class_name, WriteStep::RemoveSubject, e))?;
        return Ok(InstanceRemoved {
            class_name,
            base_uri,
            links_removed: None,
        });
    }

    // Outside a batch the cascade is not atomic, so on failure the links already removed stay
    // removed: the error says so, with the count.
    match remove_all_links_of(perspective, &base_uri, params.batch_id.as_deref()).await {
        Ok(removed) => Ok(InstanceRemoved {
            class_name,
            base_uri,
            links_removed: Some(removed),
        }),
        Err(failure) => Err(InstanceError::PartiallyRemoved {
            class_name,
            base_uri,
            removed: failure.removed,
            step: failure.step,
            error: failure.error,
        }),
    }
}

/// Add an item to a collection property of an instance. Adding an item that is already in the
/// collection writes nothing (`linksAdded: 0`).
pub async fn add_to_collection(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceAddToCollection,
) -> Result<CollectionItemAdded, InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(&class_name, &infos, &params.collection, &params.item_uri)
        .map_err(|e| invalid(&class_name, vec![e]))?;
    coerce_scalar(info, &Value::String(params.item_uri.clone()))
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    require_instance(perspective, &class_name, &base_uri).await?;
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
    // already in it.
    let wanted = params.item_uri.trim();
    let encoded = link_target(wanted);
    let existing = perspective
        .get_links(&LinkQuery {
            source: Some(base_uri.clone()),
            predicate: Some(predicate.clone()),
            ..Default::default()
        })
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
        .any(|link| target_matches(&link.data.target, wanted, &encoded))
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

    let target = property_target(
        perspective,
        &class_name,
        &collection,
        &params.item_uri,
        agent,
    )
    .await;
    let link = Link {
        source: base_uri.clone(),
        predicate: Some(predicate),
        target,
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
    perspective
        .add_link(link, status, params.batch_id, agent)
        .await
        .map_err(|e| {
            failed(
                &class_name,
                WriteStep::AddCollectionItem {
                    collection: collection.clone(),
                    item: params.item_uri.clone(),
                },
                e,
            )
        })?;
    Ok(CollectionItemAdded {
        class_name,
        base_uri,
        collection,
        item_uri: params.item_uri,
        links_added: 1,
        already_member: false,
    })
}

/// Remove an item from a collection property of an instance: only the membership link, the item
/// itself is untouched. Removing an item that is not in the collection writes nothing
/// (`linksRemoved: 0`).
pub async fn remove_from_collection(
    perspective: &mut PerspectiveInstance,
    _agent: &AgentContext,
    params: InstanceRemoveFromCollection,
) -> Result<CollectionItemRemoved, InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(&class_name, &infos, &params.collection, &params.item_uri)
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    require_instance(perspective, &class_name, &base_uri).await?;
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
    let encoded = link_target(wanted);
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base_uri.clone()),
            predicate: Some(predicate),
            ..Default::default()
        })
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
    let stored_targets: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
    let mut removed = 0usize;
    for link in links {
        if !target_matches(&link.data.target, wanted, &encoded) {
            continue;
        }
        if let Err(e) = perspective
            .remove_link(link.into(), params.batch_id.clone())
            .await
        {
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
