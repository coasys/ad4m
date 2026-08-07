use super::ProposedInstance;
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::HashMap;

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

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, identity-value) already exists in the graph, compared under
/// [`normalize_identity`]. This is the hard guarantee behind the soft
/// `existing` hint in [`build_interpretation_input`] — even if the model
/// re-proposes a known item, it never becomes a link.
///
/// `existing` maps a class's local name to the already-present identity
/// values (already normalized by [`existing_instance_identities`]).
/// `identity_props` maps a class's local name to the NAME of its declared
/// identity property. An instance whose class has no identity property is
/// always kept (no dedup); likewise one missing that property's value.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
    identity_props: &HashMap<String, String>,
) -> Vec<ProposedInstance> {
    let known: HashMap<&String, std::collections::HashSet<String>> = existing
        .iter()
        .map(|(class, values)| {
            (
                class,
                values.iter().map(|v| normalize_identity(v)).collect(),
            )
        })
        .collect();
    instances
        .into_iter()
        .filter(|inst| {
            // No declared identity for this class ⇒ no dedup.
            let Some(idp_name) = identity_props.get(&inst.class) else {
                return true;
            };
            let Some(value) = inst.props.get(idp_name).and_then(|v| v.as_str()) else {
                return true; // no identity value to compare on — keep it
            };
            let normalized = normalize_identity(value);
            let already = known
                .get(&inst.class)
                .map(|set| set.contains(&normalized))
                .unwrap_or(false);
            if already {
                log::debug!(
                    "interpretation: dropping already-present {} '{}'",
                    inst.class,
                    value
                );
            }
            !already
        })
        .collect()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_interpretation`] and skip this helper.
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

/// Decode a `literal:` URI to a plain `String`, but only when it holds a
/// string value. Reuses `ad4m-client`'s `Literal` (the canonical
/// parse/serialize type); non-string literals (number/bool/json) and
/// non-literal URIs yield `None`.
fn decode_literal_string(uri: &str) -> Option<String> {
    use ad4m_client::literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        _ => None,
    }
}

/// read the identity values of instances already present in the perspective
/// for each target class, keyed by the class's local name. Used to steer the
/// LLM away from re-proposing known items ([`build_interpretation_input`]) and to
/// enforce dedup deterministically ([`filter_already_present`]).
///
/// The dedup key is whichever property the class declares as its `identity`
/// (via [`identity_property`]), not a hard-coded `title`. Classes with no
/// identity property are skipped entirely — no identity ⇒ no dedup. Values are
/// normalized ([`normalize_identity`]) as they are read, so downstream
/// comparison is a plain set lookup.
///
/// Instances are read through the model-query API (`PerspectiveInstance::
/// model_query`) — the symmetric counterpart to writing them via
/// `create_subject` — so class conformance and field decoding go through the
/// class's own shape/getters rather than hand-matched type-flag links. A
/// per-class query failure (e.g. the class isn't registered in this
/// perspective) is treated as "no existing instances" — dedup is a soft hint,
/// guaranteed deterministically downstream by [`filter_already_present`].
pub async fn existing_instance_identities(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<String>>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for shape in shapes {
        // No declared identity property ⇒ no dedup key ⇒ skip.
        let Some(idp) = identity_property(shape) else {
            continue;
        };
        let idp_name = idp.name.clone();
        let class = class_local_name(&shape.target_class);

        let query = format!(r#"{{"properties":["{idp_name}"]}}"#);
        let result_json = match perspective.model_query(class, &query).await {
            Ok(json) => json,
            Err(e) => {
                log::warn!("existing_instance_identities: model_query({class}) failed, treating as no existing instances: {e:#}");
                continue;
            }
        };
        let result: serde_json::Value = serde_json::from_str(&result_json).map_err(|e| {
            anyhow::anyhow!(
                "existing_instance_identities: bad model_query result for {class}: {e:#}"
            )
        })?;

        let values: Vec<String> = result
            .get("instances")
            .and_then(|v| v.as_array())
            .map(|rows| {
                rows.iter()
                    .filter_map(|inst| {
                        inst.get(&idp_name)
                            .and_then(|t| t.as_str())
                            .map(normalize_identity)
                    })
                    .collect()
            })
            .unwrap_or_default();

        if !values.is_empty() {
            out.insert(class.to_string(), values);
        }
    }
    Ok(out)
}
