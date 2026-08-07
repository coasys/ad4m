use super::ProposedInstance;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::HashMap;

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

/// read the titles of instances already present in the perspective for each
/// target class, keyed by the class's local name. Used to steer the LLM away
/// from re-proposing known items ([`build_extraction_input`]) and to enforce
/// dedup deterministically ([`filter_already_present`]).
///
/// Instances are read through the model-query API (`PerspectiveInstance::
/// model_query`) — the symmetric counterpart to writing them via
/// `create_subject` — so class conformance and field decoding go through the
/// class's own shape/getters rather than hand-matched type-flag + title links.
/// Classes without a `title` property are skipped (no dedup key). A per-class
/// query failure (e.g. the class isn't registered in this perspective) is
/// treated as "no existing instances" — dedup is a soft hint, guaranteed
/// deterministically downstream by [`filter_already_present`].
pub async fn existing_instance_titles(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<String>>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for shape in shapes {
        // Only classes carrying a `title` scalar have a dedup key.
        if !shape.properties.iter().any(|p| p.name == "title") {
            continue;
        }
        let class = class_local_name(&shape.target_class);

        let result_json = match perspective
            .model_query(class, r#"{"properties":["title"]}"#)
            .await
        {
            Ok(json) => json,
            Err(e) => {
                log::warn!("existing_instance_titles: model_query({class}) failed, treating as no existing instances: {e:#}");
                continue;
            }
        };
        let result: serde_json::Value = serde_json::from_str(&result_json).map_err(|e| {
            anyhow::anyhow!("existing_instance_titles: bad model_query result for {class}: {e:#}")
        })?;

        let titles: Vec<String> = result
            .get("instances")
            .and_then(|v| v.as_array())
            .map(|rows| {
                rows.iter()
                    .filter_map(|inst| {
                        inst.get("title")
                            .and_then(|t| t.as_str())
                            .map(|s| s.to_string())
                    })
                    .collect()
            })
            .unwrap_or_default();

        if !titles.is_empty() {
            out.insert(class.to_string(), titles);
        }
    }
    Ok(out)
}
