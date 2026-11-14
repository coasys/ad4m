//! Validated instance writes: the one implementation behind the MCP `instance_*` write tools and
//! the `perspective.instance*` RPC methods.
//!
//! Every write is checked against the class's SHACL shape before anything is written
//! ([`validate::validate_properties`]: types, required properties, single- vs multi-valued,
//! `sh:in`); a rejection carries every [`ValidationError`] as data and writes nothing. Accepted
//! writes go through the same
//! `create_subject` / `update_subject` / link paths the per-class tools use, in one batch (the
//! caller's, when it names one).
//!
//! The functions here return typed results and a typed [`InstanceError`]; the callers write the
//! text (MCP its tool strings, the RPC its error message and data).
//!
//! - [`write`]: create, update, remove, add to and remove from a collection
//! - [`validate`]: SHACL-derived property validation
//! - [`error`]: [`InstanceError`] and the step a store failure happened in
//! - this file: the borrowed [`PropView`] over a class shape, class resolution, instance URIs,
//!   the instance lookup and the link cascade

// The errors are what these functions produce for a refused write (every problem, as data, for
// the caller to show), not a rare path to keep small.
#![allow(clippy::result_large_err)]

pub mod error;
pub mod validate;
pub mod write;

pub use error::{CascadeStep, InstanceError, WriteStep};
pub(crate) use validate::coerce_scalar;
pub use validate::{
    validate_properties, Cardinality, ReadOnlyReason, ValidatedWrite, ValidationError,
    ValidationProblem, WriteMode,
};
pub use write::{
    add_to_collection, create, remove, remove_from_collection, update, CollectionItemAdded,
    CollectionItemRemoved, CollectionItems, InstanceAddToCollection, InstanceCreate,
    InstanceCreated, InstanceRemove, InstanceRemoveFromCollection, InstanceRemoved, InstanceUpdate,
    InstanceUpdated,
};

use crate::agent::AgentContext;
use crate::mcp::shacl;
use crate::perspectives::model_query::is_safe_iri_target;
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty, ShapeRelation};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{LinkExpression, LinkQuery};
use crate::utils::{normalize_legacy_literal, other_literal_spelling};
use serde_json::{json, Value};
use std::sync::Arc;
use std::time::Duration;

/// How long class resolution waits for a class's SHACL to arrive over p-diff-sync on a freshly
/// joined neighbourhood before reporting it as unknown.
pub(crate) const CLASS_SYNC_WAIT: Duration = Duration::from_secs(5);

// ============================================================================
// Schema projection: SHACL shape → the properties a write is checked against
// ============================================================================

/// One property of a class as it is validated (and presented to MCP clients).
///
/// A *borrowed view* over the `ShapeProperty` (plus its `ShapeRelation`, when the property is a
/// link) that the query pipeline already loaded, not a second copy of the schema. Everything the
/// write path needs is either a field of those two or a one-line derivation from them, so this
/// type owns no state and cannot drift from what `model_query` validates against.
#[derive(Debug, Clone, Copy)]
pub(crate) struct PropView<'a> {
    prop: &'a ShapeProperty,
    /// `Some` when the property is a relation, matched by name in
    /// `ModelShape::include_relations`.
    relation: Option<&'a ShapeRelation>,
}

impl<'a> PropView<'a> {
    pub(crate) fn name(&self) -> &'a str {
        &self.prop.name
    }

    pub(crate) fn predicate(&self) -> &'a str {
        &self.prop.predicate
    }

    /// Friendly type name: `string` / `boolean` / `integer` / `number` / `datetime` /
    /// `reference` (link to another instance) / raw xsd local name for anything else.
    pub(crate) fn type_name(&self) -> String {
        if self.relation.is_some() {
            "reference".to_string()
        } else {
            friendly_type(self.prop.datatype.as_deref())
        }
    }

    pub(crate) fn required(&self) -> bool {
        self.prop.is_required
    }

    /// The language the value is stored through as an expression, when it is not a literal — a
    /// file property's file-storage language, say. Such a value is what the language takes, which
    /// need not be the declared datatype: a file is an object of its bytes and type.
    pub(crate) fn stored_by_language(&self) -> Option<&'a str> {
        self.prop
            .resolve_language
            .as_deref()
            .filter(|language| *language != "literal")
    }

    /// Multi-valued (collection or `*Many` relation).
    ///
    /// Relations are all `is_collection` in the shape (the pipeline hydrates them as arrays);
    /// `is_scalar_relation` is what tells us which ones are really single-valued.
    pub(crate) fn collection(&self) -> bool {
        if self.relation.is_some() {
            !self.prop.is_scalar_relation
        } else {
            self.prop.is_collection
        }
    }

    /// Relation kind (`hasOne`, `hasMany`, `belongsToOne`, `belongsToMany`) and the target class
    /// name, for link-typed properties.
    pub(crate) fn relation(&self) -> Option<(&'a str, &'a str)> {
        self.relation
            .map(|r| (r.kind.as_str(), r.target_class_name.as_str()))
    }

    /// `belongsTo*`: the link is stored on the *other* instance, so this side cannot write it.
    pub(crate) fn reverse(&self) -> bool {
        self.prop.direction.as_deref() == Some("reverse")
    }

    /// Class marker (`sh:hasValue` + `minCount 1`), set by the constructor. Never supplied by
    /// clients.
    pub(crate) fn flag(&self) -> bool {
        self.prop.is_flag
    }

    /// Derived via a getter expression; read-only. Relations carry a getter too (it encodes
    /// conformance filtering), so they are excluded.
    pub(crate) fn computed(&self) -> bool {
        self.relation.is_none() && self.prop.getter.is_some()
    }

    pub(crate) fn resolve_language(&self) -> Option<&'a str> {
        self.prop.resolve_language.as_deref()
    }

    pub(crate) fn interpretation_hint(&self) -> Option<&'a str> {
        self.prop.interpretation_hint.as_deref()
    }

    /// Dedup identity of the class (the "title-like" key).
    pub(crate) fn identity(&self) -> bool {
        self.prop.identity
    }

    /// `sh:in` allowed values, in declared order — `None` when the property is unconstrained.
    /// An empty list is present-and-empty: it allows nothing. Entries are as written:
    /// `{value, label?}` objects or bare values.
    pub(crate) fn in_values(&self) -> Option<&'a [Value]> {
        self.prop.in_values.as_deref()
    }

    /// Declared `local: true` — this property's links are written with `LinkStatus::Local` and
    /// stay in this executor's store: never gossiped to the neighbourhood, invisible to remote
    /// agents, and not recoverable from the network. Writable as normal; only the reach differs.
    pub(crate) fn local(&self) -> bool {
        self.prop.local
    }

    /// Whether a client may write this property at all.
    pub(crate) fn writable(&self) -> bool {
        self.read_only().is_none()
    }

    /// Why a client may not write this property, if it may not.
    pub(crate) fn read_only(&self) -> Option<ReadOnlyReason> {
        if self.flag() {
            Some(ReadOnlyReason::ClassMarker)
        } else if self.reverse() {
            Some(ReadOnlyReason::ReverseRelation)
        } else if self.computed() {
            Some(ReadOnlyReason::Computed)
        } else {
            None
        }
    }

    /// The bounds on the number of values.
    pub(crate) fn cardinality(&self) -> Cardinality {
        Cardinality {
            min: u32::from(self.required()),
            max: (!self.collection()).then_some(1),
        }
    }
}

/// Map an xsd datatype IRI (`xsd://string`, `xsd:dateTime`, …) to the friendly type vocabulary
/// used in `describe_perspective` output and validation errors. Unknown datatypes pass through
/// as their local name.
pub(crate) fn friendly_type(datatype: Option<&str>) -> String {
    let Some(dt) = datatype else {
        return "string".to_string();
    };
    let local = dt
        .rsplit(['/', '#', ':'])
        .next()
        .unwrap_or(dt)
        .to_ascii_lowercase();
    match local.as_str() {
        "string" | "normalizedstring" | "token" | "anyuri" => "string".to_string(),
        "boolean" => "boolean".to_string(),
        "integer" | "int" | "long" | "short" | "byte" | "nonnegativeinteger"
        | "positiveinteger" | "unsignedint" | "unsignedlong" | "unsignedshort" => {
            "integer".to_string()
        }
        "decimal" | "float" | "double" => "number".to_string(),
        "datetime" | "date" | "time" | "datetimestamp" => "datetime".to_string(),
        other => other.to_string(),
    }
}

/// Project a class shape into the per-property view. Flags are kept (so validation can reject
/// them by name) but marked; callers presenting the schema drop them.
pub(crate) fn class_properties(shape: &ModelShape) -> Vec<PropView<'_>> {
    shape
        .properties
        .iter()
        .map(|prop| PropView {
            prop,
            relation: shape.include_relations.iter().find(|r| r.name == prop.name),
        })
        .collect()
}

/// Look a property up by name, exact first, then case-insensitively — agents get the casing
/// wrong often enough that a hard failure is not worth it.
pub(crate) fn find_property<'a, 'p>(
    infos: &'a [PropView<'p>],
    name: &str,
) -> Option<&'a PropView<'p>> {
    infos
        .iter()
        .find(|i| i.name() == name)
        .or_else(|| infos.iter().find(|i| i.name().eq_ignore_ascii_case(name)))
}

// ============================================================================
// Classes, ids and lookups
// ============================================================================

/// Random instance URI, in the same form the TypeScript SDK mints for a `Ad4mModel` without an
/// explicit base expression (`Ad4mModel.ts:468`): `ad4m://obj/` + 24 lowercase letters.
///
/// Deliberately *not* a `literal:` URI. Two reasons:
/// 1. `literal://string:x` is not a parseable IRI (`string:x` reads as host:port with a
///    non-numeric port), so it cannot be emitted inside `<…>` in SPARQL — every query that
///    inlines an id breaks on it.
/// 2. The single-colon form `literal:string:x` is not an option either: the store decodes
///    `literal:*:` *targets* into RDF literals (`sparql_store::target_to_storage_term`), so an
///    id in that form would stop being a node the moment it is used as a link target — which is
///    exactly what a `parent` / collection link does.
pub(crate) fn generate_instance_uri() -> String {
    let random_id: String = (0..24)
        .map(|_| (b'a' + (rand::random::<u8>() % 26)) as char)
        .collect();
    format!("ad4m://obj/{random_id}")
}

/// Resolve a caller-supplied class name to its canonical spelling and shape.
///
/// Exact name first (waiting briefly for SHACL to sync on a neighbourhood), then
/// case-insensitive, else [`InstanceError::UnknownClass`] with the registered classes.
pub(crate) async fn resolve_class(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> Result<(String, Arc<ModelShape>), InstanceError> {
    if let Ok(shape) = perspective
        .get_shape_or_wait(class_name, CLASS_SYNC_WAIT)
        .await
    {
        return Ok((class_name.to_string(), shape));
    }
    if let Some(canonical) = shacl::find_class_name(perspective, &class_name.to_lowercase()).await {
        if let Ok(shape) = perspective.get_shape(&canonical) {
            return Ok((canonical, shape));
        }
    }
    let mut registered = perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default();
    registered.sort();
    Err(InstanceError::UnknownClass {
        class_name: class_name.to_string(),
        registered,
    })
}

/// [`resolve_class`] for the collection writes. They write membership links at one class's
/// predicate, so through an abstract class (#1435), which has no predicate of its own, they are
/// refused.
pub(crate) async fn resolve_writable_class(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> Result<(String, Arc<ModelShape>), InstanceError> {
    let (class_name, shape) = resolve_class(perspective, class_name).await?;
    match perspective.is_abstract_class(&class_name) {
        Ok(false) => Ok((class_name, shape)),
        Ok(true) => Err(InstanceError::AbstractClass { class_name }),
        Err(error) => Err(InstanceError::Failed {
            class_name,
            step: WriteStep::ReadShape,
            error,
        }),
    }
}

/// Run a model query for `class_name` and return `(instances, total_count)`.
pub(crate) async fn run_model_query(
    perspective: &PerspectiveInstance,
    class_name: &str,
    query: &Value,
) -> Result<(Vec<Value>, usize), String> {
    let raw = perspective
        .model_query(class_name, &query.to_string())
        .await
        .map_err(|e| format!("{e:#}"))?;
    let parsed: Value = serde_json::from_str(&raw).map_err(|e| e.to_string())?;
    let instances = parsed
        .get("instances")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let total = parsed
        .get("totalCount")
        .and_then(Value::as_u64)
        .map(|n| n as usize)
        .unwrap_or(instances.len());
    Ok((instances, total))
}

/// Fetch one hydrated instance by URI, or `None` if no instance of that class lives at
/// `base_uri`.
///
/// Tries the counterpart `literal:` spelling when the first lookup misses. [`instance_uri`]
/// normalises a caller's id to the single-colon form before we get here, but a node written
/// before that normalisation existed is stored under the double-slash form, so the normalised
/// lookup is well-formed and finds nothing — the instance reads as absent rather than as
/// differently spelled. `get_children` hands back exactly those stored ids, so the id an agent
/// just received can be the one that misses.
///
/// The second query only runs on a miss, and only for ids that have a counterpart spelling at
/// all.
pub(crate) async fn fetch_instance(
    perspective: &PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
) -> Result<Option<Value>, String> {
    let lookup = |uri: String| async move {
        let query = json!({ "where": { "id": uri }, "limit": 1 });
        let (instances, _) = run_model_query(perspective, class_name, &query).await?;
        Ok::<Option<Value>, String>(instances.into_iter().next())
    };

    if let Some(instance) = lookup(base_uri.to_string()).await? {
        return Ok(Some(instance));
    }
    match other_literal_spelling(base_uri) {
        Some(counterpart) => lookup(counterpart).await,
        None => Ok(None),
    }
}

/// `Ok` when an instance of `class_name` lives at `base_uri`, else `NotFound` (or `Failed` when
/// the lookup itself failed).
async fn require_instance(
    perspective: &PerspectiveInstance,
    class_name: &str,
    base_uri: &str,
) -> Result<(), InstanceError> {
    match fetch_instance(perspective, class_name, base_uri).await {
        Ok(Some(_)) => Ok(()),
        Ok(None) => Err(InstanceError::NotFound {
            class_name: class_name.to_string(),
            base_uri: base_uri.to_string(),
        }),
        Err(e) => Err(InstanceError::Failed {
            class_name: class_name.to_string(),
            step: WriteStep::ReadInstance,
            error: deno_core::anyhow::anyhow!(e),
        }),
    }
}

/// Whether the SDNA declares a setter for `prop` on `class_uri`. `create_subject` /
/// `update_subject` silently skip properties without one (they only log), so the check has to
/// happen here for the write to be honest about what it did.
async fn has_setter(perspective: &PerspectiveInstance, class_uri: &str, prop: &str) -> bool {
    let prop_shape_uri = format!("{class_uri}.{prop}");
    perspective
        .get_links(&LinkQuery {
            source: Some(prop_shape_uri),
            predicate: Some("ad4m://setter".to_string()),
            ..Default::default()
        })
        .await
        .map(|links| !links.is_empty())
        .unwrap_or(false)
}

/// Reject scalar writes to properties the SDNA can't actually set.
pub(crate) async fn check_setters(
    perspective: &PerspectiveInstance,
    shape: &ModelShape,
    scalars: &serde_json::Map<String, Value>,
) -> Vec<ValidationError> {
    let infos = class_properties(shape);
    let mut errors = Vec::new();
    for name in scalars.keys() {
        if !has_setter(perspective, &shape.target_class, name).await {
            let info = infos.iter().find(|i| i.name() == name);
            errors.push(ValidationError {
                property: name.clone(),
                problem: ValidationProblem::ReadOnly {
                    reason: ReadOnlyReason::NoSetter,
                },
                expected_type: info.map(|i| i.type_name()),
                cardinality: info.map(|i| i.cardinality()),
                allowed_values: None,
                received: None,
                item: None,
            });
        }
    }
    errors
}

pub(crate) fn subject_class(class_name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(class_name.to_string()),
        query: None,
    }
}

// ============================================================================
// Literals and link targets
// ============================================================================

/// The value inside a `literal:` URL, or the target unchanged when it is not one.
pub(crate) fn resolve_literal_value(target: &str) -> String {
    use ad4m_client::literal::Literal;
    if let Ok(literal) = Literal::from_url(target.to_string()) {
        match literal.get() {
            Ok(value) => value.to_string(),
            Err(_) => target.to_string(),
        }
    } else {
        target.to_string()
    }
}

/// Encode a string value as a `literal:string:` URL. Uses rust-client's `Literal` for proper
/// URL encoding.
pub(crate) fn encode_literal(value: &str) -> String {
    use ad4m_client::literal::Literal;
    Literal::from_string(value.to_string())
        .to_url()
        .unwrap_or_else(|_| format!("literal:string:{}", value))
}

/// The plain value behind a literal link target: a `literal:string:x` decodes to `x`, a signed
/// literal envelope (`literal:json:{…,"data":…}`) to its `data` payload. Non-literal targets come
/// back unchanged.
pub(crate) fn decoded_literal(target: &str) -> String {
    let decoded = resolve_literal_value(target);
    if decoded == target {
        return decoded;
    }
    if let Ok(Value::Object(envelope)) = serde_json::from_str::<Value>(&decoded) {
        if let Some(data) = envelope.get("data") {
            return match data {
                Value::String(s) => s.clone(),
                other => other.to_string(),
            };
        }
    }
    decoded
}

/// URI-or-literal encoding for link targets, shared with the per-class tools.
///
/// Anything that already is a URI passes through: `scheme://…`, the single-colon `literal:…`
/// form the store hands back as link targets (so an id read from `get_children` /
/// `instance_get` can be passed straight back in without being wrapped a second time), and
/// `did:…`. Everything else is a bare string and becomes a literal URI. The legacy
/// `literal://…` spelling is normalised first ([`normalize_legacy_literal`]).
pub(crate) fn link_target(value: &str) -> String {
    let value = normalize_legacy_literal(value);
    if value.contains("://") || value.starts_with("literal:") || value.starts_with("did:") {
        value.into_owned()
    } else {
        encode_literal(&value)
    }
}

/// Gate a caller-supplied instance identifier — a `base_uri`, a `parent`, a child id — before
/// it is inlined into SPARQL as `<…>`.
///
/// Trims, normalises the legacy `literal://…` spelling, then requires the result to be a
/// well-formed absolute IRI (the same predicate the query builder uses to decide a value is safe
/// to emit inside `<…>`). Without the gate a value like `literal://string:my-channel` or
/// `Note: buy milk` reaches the SPARQL builder and either fails to parse — taking the whole query
/// with it — or is stored as an invalid `NamedNode` no query can match.
pub(crate) fn instance_uri(field: &str, value: &str) -> Result<String, InstanceError> {
    let candidate = normalize_legacy_literal(value.trim());
    if is_safe_iri_target(&candidate) {
        return Ok(candidate.into_owned());
    }
    Err(InstanceError::InvalidUri {
        field: field.to_string(),
        value: value.to_string(),
    })
}

/// The link target a property value is written as: a well-formed absolute IRI as it is, else
/// the expression the property's language creates for it (a literal by default).
pub(crate) async fn property_target(
    perspective: &PerspectiveInstance,
    class_name: &str,
    property_name: &str,
    value: &str,
    agent_context: &AgentContext,
) -> String {
    // Short-circuit only when the value is a well-formed absolute IRI — the same predicate the
    // SPARQL query builder uses to gate its `UNION { ?s <pred> <val> }` IRI arm. Sharing the
    // check keeps write and read agreeing on what "already a URI" means; otherwise values like
    // `"Note: buy milk"` would slip through and become invalid `NamedNode::new_unchecked`
    // targets that no query can match.
    if is_safe_iri_target(value) {
        return value.to_string();
    }

    // Resolve the original-cased property name from SHACL. Dynamic tools lowercase the property
    // name (e.g. "isconversation"), but the SHACL links store the original case (e.g.
    // "isConversation").
    let original_property_name = {
        let props: Vec<shacl::ShaclProperty> =
            shacl::load_class_properties(perspective, class_name).await;
        let prop_lower = property_name.to_lowercase();
        props
            .into_iter()
            .find(|p| p.name == property_name || p.name.to_lowercase() == prop_lower)
            .map(|p| p.name)
            .unwrap_or_else(|| property_name.to_string())
    };

    // Try to parse as JSON to recover native types (bool, number). MCP tool schemas declare all
    // values as strings, so "false" arrives as the string "false" — but the language controller
    // needs json!(false).
    let json_value: Value =
        serde_json::from_str(value).unwrap_or_else(|_| Value::String(value.to_string()));

    match perspective
        .resolve_property_value(
            class_name,
            &original_property_name,
            &json_value,
            agent_context,
        )
        .await
    {
        Ok(url) => url,
        Err(e) => {
            log::warn!(
                "Failed to resolve property value for {}.{}: {}, falling back to encode_literal",
                class_name,
                original_property_name,
                e
            );
            encode_literal(value)
        }
    }
}

// ============================================================================
// Link cascade
// ============================================================================

/// A link cascade that stopped on a store error, and how far it got.
///
/// Outside a batch the cascade's removals land one by one, so the links removed before the
/// failure stay removed — the caller has to report that, not hide it behind a success.
#[derive(Debug)]
pub(crate) struct CascadeFailure {
    /// Links removed before the error.
    pub removed: usize,
    /// What the cascade was doing.
    pub step: CascadeStep,
    /// What failed.
    pub error: deno_core::anyhow::Error,
}

/// Remove every link touching `uri` (as source or target), in `batch_id` when given. Same
/// cascade as `delete_subject` / `{class}_delete`.
///
/// Enumerates the batch's effective graph (`get_links_effective`): a link the batch has
/// already staged for removal is not staged again, and one that exists only inside the batch
/// — an earlier call's staged addition touching `uri` — leaves by cancelling the addition
/// (`remove_link` insists on a stored expression). `removed` counts effective links: staged
/// removals and cancelled additions alike.
///
/// Stops at the first `get_links` / `remove_link` error and reports it together with the number
/// of links already removed. `Ok` means the two queries and every removal succeeded.
pub(crate) async fn remove_all_links_of(
    perspective: &mut PerspectiveInstance,
    uri: &str,
    batch_id: Option<&str>,
) -> Result<usize, CascadeFailure> {
    let mut removed = 0;
    let mut seen = std::collections::HashSet::new();
    for query in [
        LinkQuery {
            source: Some(uri.to_string()),
            ..Default::default()
        },
        LinkQuery {
            target: Some(uri.to_string()),
            ..Default::default()
        },
    ] {
        let links = match perspective.get_links_effective(&query, batch_id).await {
            Ok(links) => links,
            Err(error) => {
                return Err(CascadeFailure {
                    removed,
                    step: CascadeStep::List {
                        uri: uri.to_string(),
                    },
                    error,
                })
            }
        };
        for effective in links {
            let link = LinkExpression::from(effective.link);
            // A link naming `uri` both ways answers both queries; remove it once. The
            // committed entry of an identity comes before its staged copies, so `seen`
            // always decides on the committed spelling first.
            if !seen.insert(crate::db::link_identity(&link)) {
                continue;
            }
            let step = CascadeStep::Remove {
                source: link.data.source.clone(),
                predicate: link.data.predicate.clone(),
                target: link.data.target.clone(),
            };
            if let Some(batch_id) = batch_id {
                // Every staged copy of this identity goes — not only when this entry is
                // one of them: `addLinkExpression` can restage the very expression the
                // store holds, and that staged twin must not commit on top of the
                // removal staged just below.
                perspective.cancel_batch_addition(batch_id, &link).await;
            }
            if effective.committed {
                if let Err(error) = perspective
                    .remove_link(link, batch_id.map(str::to_string))
                    .await
                {
                    return Err(CascadeFailure {
                        removed,
                        step,
                        error,
                    });
                }
            }
            removed += 1;
        }
    }
    Ok(removed)
}
