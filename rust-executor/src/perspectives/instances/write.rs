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
//! batch's staged work (`get_links_effective`); instance existence still reads committed state
//! only, so create-then-update / -remove of a new instance in one batch fails `notFound`.

use super::validate::{resolve_collection, value_text};
use super::{
    check_setters, class_properties, coerce_scalar, decoded_literal, fetch_instance, find_property,
    generate_instance_uri, instance_uri, link_target, property_target, property_value_target,
    remove_all_links_of, require_instance, resolve_class, resolve_writable_class, subject_class,
    validate_properties, validate_update, InstanceError, WriteMode, WriteStep,
};
use crate::agent::AgentContext;
use crate::mcp::shacl;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SetterKind};
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
    /// An open batch (`perspective.createBatch`) to stage the write into; it lands on the
    /// batch's commit. Absent: the create commits on its own. On a non-validation error the
    /// batch may hold a partial write — discard it (`perspective.discardBatch`), don't commit
    /// it.
    pub batch_id: Option<String>,
}

/// Set properties of an instance; the others stay as they are. A collection with
/// a declared setter takes the full member list it should have — `[]` clears
/// it; any other collection is refused (`collectionNotSettable`).
#[derive(Debug, Clone, Default, Serialize, Deserialize, TS)]
#[ts(optional_fields)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstanceUpdate {
    pub class_name: String,
    pub base_uri: String,
    /// Values by property name, at least one: one JSON value per single-valued
    /// property, an array of member URIs per collection that declares a setter.
    #[ts(type = "Record<string, unknown>")]
    pub properties: Map<String, Value>,
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
    /// A write whose lifecycle actions include `call`s cannot batch (the calls
    /// cannot be rolled back): with `batchId` it is refused (`callsInBatch`)
    /// before anything is called or written.
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
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
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
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
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
    /// An open batch to stage the write into; see `InstanceCreate`'s `batchId`.
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
    /// The links the cascade removed from the effective graph; `None` through an abstract
    /// class, whose classes' destructors ran. Inside a caller's batch they are staged, not
    /// committed — including staged additions the call cancelled.
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
    /// 0 when the item was in the collection already (as the batch's effective graph has it),
    /// or when a language call made the write (`languageCall`).
    pub links_added: usize,
    pub already_member: bool,
    /// The collection's declared adder is a `call`: the language added the member, and its
    /// answer, not a link written here, makes the membership.
    pub language_call: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CollectionItemRemoved {
    pub class_name: String,
    pub base_uri: String,
    pub collection: String,
    pub item_uri: String,
    /// 0 when the item was not in the collection (as the batch's effective graph has it).
    /// Inside a batch, a member that only the batch had added counts too — it leaves by
    /// cancelling the staged addition. 0 too when a language call made the write.
    pub links_removed: usize,
    /// The collection's declared remover is a `call`: the language removed the member.
    pub language_call: bool,
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

/// A write step failed: drop `batch` when it is the write's own (a write with
/// `call` actions opens none, so `None` is a no-op). The caller's batch is the
/// caller's to discard.
async fn abandon_batch(perspective: &PerspectiveInstance, batch: &Option<Batch>) {
    if let Some(batch) = batch {
        batch.abandon(perspective).await;
    }
}

/// A write whose lifecycle actions call into a language runs outside any batch: discarding a
/// batch cannot undo a language call. So a caller's batch is refused — `BatchNotFound` when it
/// is not open, else `CallsInBatch` — before any call or link write.
async fn refuse_batch_for_calls(
    perspective: &PerspectiveInstance,
    class_name: &str,
    batch_id: Option<&str>,
) -> Result<(), InstanceError> {
    check_batch(perspective, batch_id).await?;
    match batch_id {
        Some(id) => Err(InstanceError::CallsInBatch {
            class_name: class_name.to_string(),
            batch_id: id.to_string(),
        }),
        None => Ok(()),
    }
}

/// Whether `collection` of `class_name` declares a `call` adder or remover
/// (`predicate`).
async fn member_call(
    perspective: &PerspectiveInstance,
    class_name: &str,
    collection: &str,
    predicate: &str,
) -> Result<bool, InstanceError> {
    Ok(perspective
        .property_member_kind(class_name, collection, predicate)
        .await
        .map_err(|e| failed(class_name, WriteStep::ReadShape, e))?
        == Some(SetterKind::Call))
}

/// A scalar with no `ad4m://setter` is written as a one-member replace
/// through a `collectionSetter` synthesized from its `path`: right for a
/// link `adder`/`remover` pair, but a `call` pair's language would never
/// see the write (and a caller's batch would pass). Such a scalar is
/// refused as read-only, before anything is written.
async fn refuse_call_pair_scalar(
    perspective: &PerspectiveInstance,
    class_name: &str,
    info: &super::PropView<'_>,
) -> Result<(), InstanceError> {
    for predicate in ["ad4m://adder", "ad4m://remover"] {
        if member_call(perspective, class_name, info.name(), predicate).await? {
            return Err(invalid(
                class_name,
                vec![super::ValidationError {
                    property: info.name().to_string(),
                    problem: super::ValidationProblem::ReadOnly {
                        reason: super::ReadOnlyReason::NoSetter,
                    },
                    expected_type: Some(info.type_name()),
                    cardinality: Some(info.cardinality()),
                    allowed_values: None,
                    received: None,
                    item: None,
                }],
            ));
        }
    }
    Ok(())
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

    // Route every supplied value by the actions the class declares. A `call`
    // constructor gets the whole record — collections included — and answers
    // the address it minted in the language. For link constructors a
    // collection with a declared `ad4m://setter` goes through the setter
    // (`create_subject` runs it, `call` or `collectionSetter`); one without is
    // written as item links below. A create that includes calls runs
    // unbatched and refuses a caller's batch (`refuse_batch_for_calls`).
    let ctor_is_call = perspective
        .constructor_is_call(&class_name)
        .await
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    let mut has_calls = ctor_is_call;
    let mut initial = serde_json::Map::new();
    // A scalar with no declared `ad4m://setter` — a `hasOne` relation's
    // `adder`/`remover` pair — is a one-member replace written after the
    // subject exists, like `plain_collections` below. A `call` constructor
    // gets it instead: the record's whole values are the call's params.
    let mut scalar_replaces: Vec<(String, String, bool, Value)> = Vec::new();
    let infos = class_properties(&shape);
    for (name, value) in &validated.scalars {
        match perspective
            .property_setter_kind(&class_name, name)
            .await
            .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
        {
            Some(kind) => {
                has_calls |= kind == SetterKind::Call;
                initial.insert(name.clone(), value.clone());
            }
            None if ctor_is_call => {
                initial.insert(name.clone(), value.clone());
            }
            None => {
                let Some(info) = find_property(&infos, name) else {
                    return Err(failed(
                        &class_name,
                        WriteStep::ReadShape,
                        anyhow!("property `{name}` is not a property of {class_name}"),
                    ));
                };
                refuse_call_pair_scalar(perspective, &class_name, info).await?;
                scalar_replaces.push((
                    name.clone(),
                    info.predicate().to_string(),
                    info.local(),
                    value.clone(),
                ));
            }
        }
    }
    let mut plain_collections: Vec<String> = Vec::new();
    for (collection, items) in &validated.collections {
        let through_subject = match perspective
            .property_setter_kind(&class_name, collection)
            .await
            .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
        {
            Some(SetterKind::Call) => {
                has_calls = true;
                true
            }
            Some(SetterKind::Links) => true,
            // With no setter the only writer left is the constructor call
            // itself; a link constructor leaves it to the item links below,
            // whatever its setters do (`create_subject` skips it).
            None => ctor_is_call,
        };
        if through_subject {
            initial.insert(collection.clone(), Value::Array(items.clone()));
        } else {
            plain_collections.push(collection.clone());
        }
    }
    let initial_values = (!initial.is_empty()).then(|| Value::Object(initial));

    // One batch for the whole create — subject, collection links, and the parent link land
    // atomically or not at all: no partially-constructed instance is ever observable, by peers
    // or by a failed call's aftermath. With `call` actions there is no batch
    // at all: a caller's is refused before the call, and none of our own is
    // opened (`run_call_actions` refuses one).
    let batch = if has_calls {
        refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        None
    } else {
        Some(Batch::open(perspective, params.batch_id.clone()).await?)
    };
    let subject_batch = batch.as_ref().map(Batch::id);
    let links_batch = subject_batch.clone();
    let base_uri = match perspective
        .create_subject(
            subject_class(&class_name),
            base_uri.clone(),
            initial_values,
            subject_batch,
            agent,
        )
        .await
    {
        // A `call` constructor answers the address it minted in the language —
        // that, not the preselected `base_uri`, is the instance's id. The link
        // constructor path answers `None`: the given `base_uri` stands.
        Ok(minted) => minted.unwrap_or(base_uri),
        Err(e) => {
            abandon_batch(perspective, &batch).await;
            return Err(failed(&class_name, WriteStep::CreateSubject, e));
        }
    };

    // Scalars `create_subject` had no setter for: each is a replace with the
    // single member — the `collectionSetter` an `executeAction` caller writes.
    for (name, predicate, local, value) in &scalar_replaces {
        if let Err(e) = perspective
            .replace_collection_without_setter(
                &class_name,
                name,
                predicate,
                *local,
                &base_uri,
                &Value::Array(vec![value.clone()]),
                links_batch.clone(),
                agent,
            )
            .await
        {
            abandon_batch(perspective, &batch).await;
            return Err(failed(&class_name, WriteStep::UpdateSubject, e));
        }
    }

    // Collections `create_subject` did not write: one link per item, same path
    // as `add_to_collection`.
    let mut collections_set = Vec::with_capacity(validated.collections.len());
    for (collection, items) in &validated.collections {
        if !plain_collections.iter().any(|name| name == collection) {
            collections_set.push(super::CollectionItems {
                collection: collection.clone(),
                items: items
                    .iter()
                    .map(|item| match item {
                        Value::String(s) => s.clone(),
                        other => other.to_string(),
                    })
                    .collect(),
            });
            continue;
        }
        let (predicate, status) = collection_links[collection].clone();
        let mut added = Vec::with_capacity(items.len());
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
                target,
            };
            if let Err(e) = perspective
                .add_link(link, status.clone(), links_batch.clone(), agent)
                .await
            {
                abandon_batch(perspective, &batch).await;
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
                links_batch.clone(),
                agent,
            )
            .await
        {
            abandon_batch(perspective, &batch).await;
            return Err(failed(
                &class_name,
                WriteStep::AddToParent {
                    parent: parent.clone(),
                },
                e,
            ));
        }
    }

    if let Some(batch) = batch {
        batch.finish(perspective, agent, &class_name).await?;
    }

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
/// A collection with a declared setter replaces its members (`[]` clears them);
/// any other collection is refused.
pub async fn update(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceUpdate,
) -> Result<InstanceUpdated, InstanceError> {
    let (class_name, shape) = resolve_class(perspective, &params.class_name).await?;
    if params.properties.is_empty() {
        return Err(InstanceError::EmptyProperties);
    }

    // Through an abstract class (#1435) the record's own classes' setters write;
    // `update_subject` checks those. Which collections they can set depends on
    // the record, so it is found first.
    let is_abstract = perspective
        .is_abstract_class(&class_name)
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    let abstract_record = if is_abstract {
        let base_uri = instance_uri("base_uri", &params.base_uri)?;
        Some(require_instance(perspective, &class_name, &base_uri).await?)
    } else {
        None
    };

    // A collection takes a member list only through a declared
    // `ad4m://setter` (a mounted class's `call`, or a `collectionSetter`);
    // through an abstract class, through what the record's own classes
    // declare. `validate_update` refuses any other one.
    let infos = class_properties(&shape);
    let mut settable = Vec::new();
    for given in params.properties.keys() {
        let Some(info) = find_property(&infos, given) else {
            continue;
        };
        if !info.collection() {
            continue;
        }
        let can_set = match &abstract_record {
            Some(stored) => perspective
                .abstract_collection_settable(&class_name, stored, info.name())
                .await
                .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?,
            None => perspective
                .property_setter_kind(&class_name, info.name())
                .await
                .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
                .is_some(),
        };
        if can_set {
            settable.push(info.name().to_string());
        }
    }
    let validated = validate_update(&shape, &params.properties, &settable)
        .map_err(|errors| invalid(&class_name, errors))?;
    if !is_abstract {
        let setter_errors = check_setters(perspective, &shape, &validated.scalars).await;
        if !setter_errors.is_empty() {
            return Err(invalid(&class_name, setter_errors));
        }
    }

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = match abstract_record {
        Some(stored) => stored,
        None => require_instance(perspective, &class_name, &base_uri).await?,
    };

    // A collection (validation let through only those with a declared
    // `ad4m://setter`) joins the subject values verbatim — `update_subject`
    // gives a `call` setter `{coll: [ids]}` and a `collectionSetter` the
    // member list. A scalar with no declared setter (a `hasOne` relation,
    // which carries only `adder`/`remover`) gets a collectionSetter
    // synthesized from its `path` and `local`, with one member — the replace
    // an `executeAction` caller would write.
    let mut subject_values = serde_json::Map::new();
    let mut plain_collections: Vec<(String, String, bool, Vec<Value>)> = Vec::new();
    let mut has_calls = false;
    if is_abstract {
        subject_values.extend(validated.scalars.clone());
    } else {
        for (name, value) in &validated.scalars {
            match perspective
                .property_setter_kind(&class_name, name)
                .await
                .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
            {
                Some(kind) => {
                    has_calls |= kind == SetterKind::Call;
                    subject_values.insert(name.clone(), value.clone());
                }
                // `check_setters` passed, so an `adder`/`remover` pair covers
                // the write — `update_subject` would drop the value.
                None => {
                    let Some(info) = find_property(&infos, name) else {
                        return Err(failed(
                            &class_name,
                            WriteStep::ReadShape,
                            anyhow!("property `{name}` is not a property of {class_name}"),
                        ));
                    };
                    refuse_call_pair_scalar(perspective, &class_name, info).await?;
                    plain_collections.push((
                        name.clone(),
                        info.predicate().to_string(),
                        info.local(),
                        vec![value.clone()],
                    ));
                }
            }
        }
    }
    for (collection, items) in &validated.collections {
        // Through an abstract class a bare array is the setter op (`Op::of`)
        // the record's own classes' collection setters write;
        // `abstract_update_calls` below classifies them.
        if !is_abstract
            && perspective
                .property_setter_kind(&class_name, collection)
                .await
                .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
                == Some(SetterKind::Call)
        {
            has_calls = true;
        }
        subject_values.insert(collection.clone(), Value::Array(items.clone()));
    }

    // Through an abstract class the record's own classes' setters write:
    // ask them whether any of those calls a language.
    if is_abstract {
        has_calls = perspective
            .abstract_update_calls(&class_name, &stored, &Value::Object(subject_values.clone()))
            .await
            .map_err(|e| failed(&class_name, WriteStep::UpdateSubject, e))?;
    }

    // One batch for the whole update so remove-old + add-new per property lands atomically
    // (same reasoning as the per-class setters: without it a peer can observe the property as
    // unset in between). With `call` actions there is no batch at all: a
    // caller's is refused before the call, and none of our own is opened.
    let batch = if has_calls {
        refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        None
    } else {
        Some(Batch::open(perspective, params.batch_id.clone()).await?)
    };
    let subject_batch = batch.as_ref().map(Batch::id);
    let links_batch = subject_batch.clone();
    if let Err(e) = perspective
        .update_subject(
            subject_class(&class_name),
            stored.clone(),
            Value::Object(subject_values.clone()),
            subject_batch,
            agent,
        )
        .await
    {
        abandon_batch(perspective, &batch).await;
        return Err(failed(&class_name, WriteStep::UpdateSubject, e));
    }
    for (collection, predicate, local, items) in &plain_collections {
        if let Err(e) = perspective
            .replace_collection_without_setter(
                &class_name,
                collection,
                predicate,
                *local,
                &stored,
                &Value::Array(items.clone()),
                links_batch.clone(),
                agent,
            )
            .await
        {
            abandon_batch(perspective, &batch).await;
            return Err(failed(&class_name, WriteStep::UpdateSubject, e));
        }
    }
    if let Some(batch) = batch {
        batch.finish(perspective, agent, &class_name).await?;
    }

    Ok(InstanceUpdated {
        updated_properties: subject_values
            .keys()
            .chain(plain_collections.iter().map(|(name, _, _, _)| name))
            .cloned()
            .collect(),
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
    let (class_name, _shape) = resolve_class(perspective, &params.class_name).await?;
    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(perspective, &class_name, &base_uri).await?;
    check_batch(perspective, params.batch_id.as_deref()).await?;

    // Through an abstract class (#1435), the destructors of the record's classes it reads: the
    // cascade below would delete its other classes.
    let is_abstract = perspective
        .is_abstract_class(&class_name)
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?;
    if is_abstract {
        if perspective
            .abstract_remove_calls(&class_name, &stored)
            .await
            .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
        {
            refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        }
        perspective
            .remove_subject(subject_class(&class_name), stored, params.batch_id, agent)
            .await
            .map_err(|e| failed(&class_name, WriteStep::RemoveSubject, e))?;
        return Ok(InstanceRemoved {
            class_name,
            base_uri,
            links_removed: None,
        });
    }

    // A concrete class's declared destructor runs first — a `call` destructor
    // runs unbatched and refuses a caller's batch before it calls; link
    // destructors stage into the caller's batch as before. The cascade still
    // follows: it cleans the links the destructor does not own (incoming
    // ones, links of other classes on this node).
    if let Some(kind) = perspective
        .destructor_kind(&class_name)
        .await
        .map_err(|e| failed(&class_name, WriteStep::ReadShape, e))?
    {
        if kind == SetterKind::Call {
            refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        }
        perspective
            .remove_subject(
                subject_class(&class_name),
                stored.clone(),
                params.batch_id.clone(),
                agent,
            )
            .await
            .map_err(|e| failed(&class_name, WriteStep::RemoveSubject, e))?;
    }

    // Outside a batch the cascade is not atomic, so on failure the links already removed stay
    // removed: the error says so, with the count.
    match remove_all_links_of(perspective, &stored, params.batch_id.as_deref()).await {
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
/// collection writes nothing (`linksAdded: 0`); membership is read on the batch's effective
/// graph, so a member the same batch dropped reads as out and one it added reads as in.
pub async fn add_to_collection(
    perspective: &mut PerspectiveInstance,
    agent: &AgentContext,
    params: InstanceAddToCollection,
) -> Result<CollectionItemAdded, InstanceError> {
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(&class_name, &infos, &params.collection, &params.item_uri)
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let coerced = coerce_scalar(info, &Value::String(params.item_uri.clone()))
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(perspective, &class_name, &base_uri).await?;

    // A declared `call` adder is the language's to run: it gets the member,
    // and its delta, not a link of ours, makes the membership. Unbatched.
    if member_call(perspective, &class_name, &collection, "ad4m://adder").await? {
        refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        perspective
            .run_member_calls(
                &class_name,
                &collection,
                "ad4m://adder",
                &stored,
                vec![coerced],
                agent,
            )
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
        return Ok(CollectionItemAdded {
            class_name,
            base_uri,
            collection,
            item_uri: params.item_uri,
            links_added: 0,
            already_member: false,
            language_call: true,
        });
    }
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
            language_call: false,
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
    let link = Link {
        source: stored,
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
        language_call: false,
    })
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
    let (class_name, shape) = resolve_writable_class(perspective, &params.class_name).await?;

    let infos = class_properties(&shape);
    let info = resolve_collection(&class_name, &infos, &params.collection, &params.item_uri)
        .map_err(|e| invalid(&class_name, vec![e]))?;
    let collection = info.name().to_string();

    let base_uri = instance_uri("base_uri", &params.base_uri)?;
    // Where it is stored: a legacy-spelled id's node is not the normalised one.
    let stored = require_instance(perspective, &class_name, &base_uri).await?;

    // A declared `call` remover, as a `call` adder in `add_to_collection`:
    // the language removes the member; nothing here reads or writes links.
    if member_call(perspective, &class_name, &collection, "ad4m://remover").await? {
        refuse_batch_for_calls(perspective, &class_name, params.batch_id.as_deref()).await?;
        let member = coerce_scalar(info, &Value::String(params.item_uri.clone()))
            .unwrap_or_else(|_| Value::String(params.item_uri.clone()));
        perspective
            .run_member_calls(
                &class_name,
                &collection,
                "ad4m://remover",
                &stored,
                vec![member],
                agent,
            )
            .await
            .map_err(|e| {
                failed(
                    &class_name,
                    WriteStep::RemoveCollectionItem {
                        collection: collection.clone(),
                        item: params.item_uri.clone(),
                        removed: 0,
                    },
                    e,
                )
            })?;
        return Ok(CollectionItemRemoved {
            class_name,
            base_uri,
            collection,
            item_uri: params.item_uri,
            links_removed: 0,
            language_call: true,
        });
    }
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
        language_call: false,
    })
}
