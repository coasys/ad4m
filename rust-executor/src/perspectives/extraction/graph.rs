use super::{parse_extraction_response, ProposedInstance};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::Link;
use std::collections::HashMap;
use uuid::Uuid;

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, title) already exists in the graph, case-insensitively. This is the
/// hard guarantee behind the soft `existing` hint in [`build_extraction_input`]
/// — even if the model re-proposes a known item, it never becomes a link.
///
/// `existing` maps a class's local name to the titles already present. Only the
/// `title` field is compared (the human-facing identity of an SoA node);
/// instances without a `title` are always kept.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
) -> Vec<ProposedInstance> {
    let known: HashMap<&String, std::collections::HashSet<String>> = existing
        .iter()
        .map(|(class, titles)| (class, titles.iter().map(|t| t.to_lowercase()).collect()))
        .collect();
    instances
        .into_iter()
        .filter(|inst| {
            let Some(title) = inst.props.get("title").and_then(|v| v.as_str()) else {
                return true; // no title to compare on — keep it
            };
            let already = known
                .get(&inst.class)
                .map(|set| set.contains(&title.to_lowercase()))
                .unwrap_or(false);
            if already {
                log::debug!(
                    "extraction: dropping already-present {} '{}'",
                    inst.class,
                    title
                );
            }
            !already
        })
        .collect()
}

/// Predicates of the shape's relation (link-typed) properties. `load_shape`
/// lists every relation both in `properties` (so the query pipeline sees its
/// predicate) *and* in `include_relations`; we key off the latter to recognise
/// them. Relations are excluded from generic scalar extraction: their targets
/// are instance URIs, not literals, so we neither offer them to the LLM nor
/// write LLM-proposed values through `value_to_literal_uri` (which would encode
/// e.g. an array as a bogus `literal:json:` URI). Relation extraction is a
/// later PR.
pub(crate) fn relation_predicates(shape: &ModelShape) -> std::collections::HashSet<&str> {
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

/// turn a `ProposedInstance` (parsed LLM output) into perspective links
/// anchored at `base`. Pure — no store, no LLM. Emits, in shape order:
///   1. one link per type-flag property (predicate = flag path, target = the
///      flag's constant `initial_value`), so downstream queries recognise the
///      class;
///   2. one link per non-flag shape property that appears in `inst.props`
///      (predicate = property path, target = literal-encoded value).
///
/// Unknown/extra fields in `inst.props` are dropped — the LLM cannot inject
/// links outside the declared class shape.
pub fn instance_links(shape: &ModelShape, inst: &ProposedInstance, base: &str) -> Vec<Link> {
    let mut out = Vec::new();
    let rel_preds = relation_predicates(shape);
    for prop in &shape.properties {
        if prop.is_flag {
            if let Some(target) = prop.initial_value.as_ref() {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target: target.clone(),
                });
            }
            continue;
        }
        // Skip relation properties: their targets are instance URIs, not
        // literals. Writing an LLM-proposed value here would mint a bogus
        // literal link. Relation extraction is a later PR.
        if rel_preds.contains(prop.predicate.as_str()) {
            continue;
        }
        if let Some(value) = inst.props.get(&prop.name) {
            if let Some(target) = value_to_literal_uri(value) {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target,
                });
            }
        }
    }
    out
}

/// Encode a JSON scalar into an AD4M `literal:` URI by delegating to the
/// canonical [`crate::languages::literal::literal_encode`] — the same encoder
/// the literal Language uses and that `model_query`'s `parse_literal_value`
/// round-trips against — and prefixing the `literal:` scheme it omits. `null`
/// is skipped so a missing optional field never becomes a `literal:json:null`
/// link.
fn value_to_literal_uri(value: &serde_json::Value) -> Option<String> {
    if value.is_null() {
        return None;
    }
    Some(format!(
        "literal:{}",
        crate::languages::literal::literal_encode(value)
    ))
}

/// parse a raw LLM response and turn it into the set of links that
/// would be written into the perspective. Callers minted a fresh instance base
/// URI per proposed instance under `base_prefix` and delegate to
/// [`instance_links`] for the actual shape-driven link construction.
///
/// The lookup from `inst.class` to a `ModelShape` is by local class name
/// (final segment of `target_class`). Proposed instances whose class doesn't
/// match any provided shape are silently dropped — the LLM cannot inject
/// links outside the caller's declared shape set.
///
/// Returned tuples pair each fresh base URI with the links anchored on it, so
/// the caller ([`run_extraction`] or a test) can decide how to persist them.
pub fn apply_extraction_raw(
    shapes: &[ModelShape],
    raw: &str,
    base_prefix: &str,
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
    let proposed = parse_extraction_response(raw)?;
    Ok(place_instances(shapes, &proposed, base_prefix))
}

/// Core of [`apply_extraction_raw`], factored out so [`run_extraction`] can
/// reuse it without a redundant JSON round-trip. Same semantics: unknown-class
/// instances are dropped; every kept instance gets a fresh UUID-tagged base.
pub fn place_instances(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<(String, Vec<Link>)> {
    let mut out = Vec::with_capacity(proposed.len());
    for inst in proposed {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "extraction: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let base = format!(
            "{base_prefix}{}/{}",
            inst.class.to_lowercase(),
            Uuid::new_v4()
        );
        let links = instance_links(shape, inst, &base);
        out.push((base, links));
    }
    out
}

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_extraction`] and skip this helper.
pub async fn gather_transcript(
    perspective: &PerspectiveInstance,
    source: &str,
    message_predicate: &str,
) -> anyhow::Result<Vec<(String, String)>> {
    use crate::types::LinkQuery;
    let query = LinkQuery {
        source: Some(source.to_string()),
        predicate: Some(message_predicate.to_string()),
        ..Default::default()
    };
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| anyhow::anyhow!("gather_transcript: get_links failed: {e:#}"))?;
    let mut out = Vec::with_capacity(links.len());
    for l in links {
        if let Some(body) = decode_literal_string(&l.data.target) {
            out.push((l.author, body));
        }
    }
    Ok(out)
}

fn decode_literal_string(uri: &str) -> Option<String> {
    let rest = uri.strip_prefix("literal:string:")?;
    percent_encoding::percent_decode_str(rest)
        .decode_utf8()
        .ok()
        .map(|c| c.into_owned())
}

/// read the titles of instances already present in the perspective for each
/// target class, keyed by the class's local name. Used to steer the LLM away
/// from re-proposing known items ([`build_extraction_input`]) and to enforce
/// dedup deterministically ([`filter_already_present`]).
///
/// An instance is located by its class type-flag link (predicate + constant
/// value); its identity is the `title` property. Classes without a type flag or
/// a `title` property are skipped (no dedup key).
pub async fn existing_instance_titles(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<String>>> {
    use crate::types::LinkQuery;
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for shape in shapes {
        let Some(flag) = shape
            .properties
            .iter()
            .find(|p| p.is_flag && p.initial_value.is_some())
        else {
            continue;
        };
        let Some(title_prop) = shape.properties.iter().find(|p| p.name == "title") else {
            continue;
        };
        let flag_value = flag.initial_value.as_ref().unwrap();

        // All instances of this class = sources of the type-flag link.
        let flag_links = perspective
            .get_links(&LinkQuery {
                predicate: Some(flag.predicate.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| {
                anyhow::anyhow!("existing_instance_titles: get_links(flag) failed: {e:#}")
            })?;
        let bases: Vec<String> = flag_links
            .into_iter()
            .filter(|l| &l.data.target == flag_value)
            .map(|l| l.data.source)
            .collect();

        let mut titles = Vec::new();
        for base in bases {
            let title_links = perspective
                .get_links(&LinkQuery {
                    source: Some(base),
                    predicate: Some(title_prop.predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| {
                    anyhow::anyhow!("existing_instance_titles: get_links(title) failed: {e:#}")
                })?;
            for tl in title_links {
                if let Some(title) = decode_literal_string(&tl.data.target) {
                    titles.push(title);
                }
            }
        }
        if !titles.is_empty() {
            out.insert(class_local_name(&shape.target_class).to_string(), titles);
        }
    }
    Ok(out)
}
