use super::ProposedInstance;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::utils::parse_literal_value;
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
/// string value. Delegates to the canonical [`parse_literal_value`] (which also
/// unwraps signed-expression envelopes); non-string literals (number/bool/json)
/// and non-literal URIs yield `None`.
fn decode_literal_string(uri: &str) -> Option<String> {
    match parse_literal_value(uri) {
        serde_json::Value::String(s) => Some(s),
        _ => None,
    }
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
