use crate::perspectives::interpretation::types::{ExistingInstances, InstanceContext};
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty};
use std::collections::{BTreeMap, HashMap, HashSet};

mod read;
mod write;
pub use read::*;
pub use write::*;

/// Group existing instances by class local name for the per-class reasoning
/// the prompt and dedup paths do. Each class's instances are sorted by `id` so
/// the prompt text and the semantic-embedding batch order are deterministic
/// across runs (an id-keyed map has no inherent order).
pub(crate) fn instances_by_class(
    existing: &ExistingInstances,
) -> BTreeMap<String, Vec<&InstanceContext>> {
    let mut out: BTreeMap<String, Vec<&InstanceContext>> = BTreeMap::new();
    for inst in existing.values() {
        out.entry(inst.class.clone()).or_default().push(inst);
    }
    for rows in out.values_mut() {
        rows.sort_by(|a, b| a.id.cmp(&b.id));
    }
    out
}

/// Per-class raw identity values (titles) projected from the id-keyed existing
/// set — the comparison basis for both dedup paths (the string path normalizes
/// them, the semantic path embeds them). Deterministic order (by `id`) so the
/// semantic embedding batch is stable.
pub(crate) fn identity_values_by_class(
    existing: &ExistingInstances,
) -> HashMap<String, Vec<String>> {
    instances_by_class(existing)
        .into_iter()
        .map(|(class, rows)| (class, rows.into_iter().map(|r| r.title.clone()).collect()))
        .collect()
}

/// The property a class declares as its dedup identity (its title-like
/// interpretation key) — the first property with `identity == true`. `None`
/// when the SDNA declared no identity, in which case the class is never
/// deduplicated (still interpreted and created, just not deduped).
pub(crate) fn identity_property(shape: &ModelShape) -> Option<&ShapeProperty> {
    shape.properties.iter().find(|p| p.identity)
}

/// Canonicalize an identity value for equality: trim, collapse internal
/// whitespace to single spaces, and lowercase. So "Ship  the MVP " and
/// "ship the mvp" compare equal. Semantic/embedding dedup is a later
/// follow-up; this is deliberately a cheap normalized string match.
pub(crate) fn normalize_identity(s: &str) -> String {
    s.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_lowercase()
}

/// Names of the shape's relation (link-typed) properties. `load_shape` lists
/// every relation both in `properties` (so the query pipeline sees its
/// predicate) *and* in `include_relations`; we key off the latter to recognise
/// them. Relations never travel through the scalar write path: their targets are
/// instance URIs, not literals, so `create_subject`/`update_subject` must not see
/// them in `initial_values` (a setter would literal-encode an instance ref into a
/// bogus `literal:` URI). They are resolved separately into
/// [`InterpretationOp::AddLinks`].
pub(crate) fn relation_names(shape: &ModelShape) -> HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.name.as_str())
        .collect()
}

/// Predicates of the shape's relation properties — the `properties`-side view of
/// [`relation_names`], used where a shape property must be recognised as a
/// relation by predicate (prompt field rendering, hint join).
pub(crate) fn relation_predicates(shape: &ModelShape) -> HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.predicate.as_str())
        .collect()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}
