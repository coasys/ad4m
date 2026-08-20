use crate::perspectives::interpretation::types::{ExistingInstances, InstanceContext};
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty};
use std::collections::{BTreeMap, HashSet};

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
    for entries in existing.values() {
        for inst in entries {
            out.entry(inst.class.clone()).or_default().push(inst);
        }
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
#[cfg(test)]
pub(crate) fn identity_values_by_class(
    existing: &ExistingInstances,
) -> std::collections::HashMap<String, Vec<String>> {
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

/// Class identifier shown to the LLM and used as the cross-map key throughout
/// interpretation (prompt `name`, the `existing`-by-class grouping, `identity_props`,
/// and the Create/Update routing `find`).
///
/// Bare local name when it is unique among `shapes` — the common single-namespace
/// case, which keeps the identifier short so small models echo it reliably. When
/// two shapes share a local name (e.g. `flux://Task` vs `soa://Task`), the full
/// `target_class` URI is used for BOTH, so they never collapse into one bucket
/// (which would let the `existing`/`identity_props` maps overwrite each other and
/// make the routing `find` resolve to whichever shape came first). `load_shape` /
/// `create_subject` resolve a full-URI class name via exact match
/// (`STR(?targetClass) = "<uri>"`), so a disambiguated label still round-trips to
/// the real subject class. Every call site derives this from the same `shapes`
/// slice, so they agree on each class's label.
pub(crate) fn class_label(target_class: &str, shapes: &[ModelShape]) -> String {
    let local = class_local_name(target_class);
    let collides = shapes
        .iter()
        .filter(|s| s.target_class != target_class)
        .any(|s| class_local_name(&s.target_class) == local);
    if collides {
        target_class.to_string()
    } else {
        local.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shape(target_class: &str) -> ModelShape {
        ModelShape {
            target_class: target_class.to_string(),
            shape_uri: format!("{target_class}Shape"),
            properties: Vec::new(),
            include_relations: Vec::new(),
            interpretation_hint: None,
        }
    }

    #[test]
    fn class_label_uses_bare_local_name_when_unique() {
        let shapes = vec![shape("soa://Task"), shape("soa://Belief")];
        assert_eq!(class_label("soa://Task", &shapes), "Task");
        assert_eq!(class_label("soa://Belief", &shapes), "Belief");
    }

    #[test]
    fn class_label_disambiguates_cross_namespace_collision_with_full_uri() {
        // `flux://Task` and `soa://Task` share the local name "Task": both must
        // fall back to their full URIs so the `existing`/`identity_props` maps and
        // the Create/Update routing `find` never collapse them into one bucket.
        let shapes = vec![
            shape("flux://Task"),
            shape("soa://Task"),
            shape("soa://Belief"),
        ];
        assert_eq!(class_label("flux://Task", &shapes), "flux://Task");
        assert_eq!(class_label("soa://Task", &shapes), "soa://Task");
        // A non-colliding class in the same set still gets its short name.
        assert_eq!(class_label("soa://Belief", &shapes), "Belief");
    }
}
