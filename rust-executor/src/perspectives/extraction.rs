//! Generic LLM extraction: turn conversation text into typed subject-class
//! instances, steered by the natural-language `extraction_hint` declared on each
//! class/property (see `shacl_parser` S0 + `model_query` S1).
//!
//! Build sequence (planning/generic-extraction-spec.md §9):
//!   S2 — `parse_extraction_response` (this commit): LLM JSON -> `ProposedInstance`s
//!   S3 — `build_extraction_input`  : shapes' hints + transcript -> prompt
//!   S4 — `instance_links`          : `ProposedInstance` -> perspective links
//!   S5 — system prompt + `ensure_extraction_task`
//!   S6 — `run_extraction`          : async shell wiring S1-S5 + AIService
//!
//! S2 is pure (no perspective, no LLM) and fully CI-tested.

use crate::perspectives::model_query::types::ModelShape;
use serde::Deserialize;
use std::collections::HashMap;

/// One instance the LLM proposes creating: the target class name plus a flat
/// map of field-name -> value. Extra/unknown fields are tolerated (kept in
/// `props`); `instance_links` (S4) filters them against the class shape.
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct ProposedInstance {
    pub class: String,
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}

/// Parse a raw LLM response into proposed instances.
///
/// Local models wrap JSON in reasoning/markdown noise, so we first strip the
/// common wrappers (mirrors Flux `LLMutils.ts`): `<think>…</think>` blocks,
/// ```-fences, and trailing commas. Then parse as a JSON array.
pub fn parse_extraction_response(raw: &str) -> anyhow::Result<Vec<ProposedInstance>> {
    let cleaned = clean_llm_json(raw);
    let instances: Vec<ProposedInstance> = serde_json::from_str(&cleaned).map_err(|e| {
        anyhow::anyhow!("extraction JSON parse failed: {e}; cleaned payload: {cleaned}")
    })?;
    Ok(instances)
}

/// Strip the reasoning/markdown noise local models add around JSON.
fn clean_llm_json(raw: &str) -> String {
    // 1. Drop <think>…</think> reasoning blocks (non-greedy, across newlines).
    let think = regex::Regex::new(r"(?s)<think>.*?</think>").unwrap();
    let s = think.replace_all(raw, "");

    // 2. Drop code fences ```json / ``` (keep the inner content).
    let fence = regex::Regex::new(r"```[a-zA-Z0-9]*").unwrap();
    let s = fence.replace_all(&s, "");

    // 3. Remove trailing commas before a closing } or ] (invalid JSON, common).
    let trailing = regex::Regex::new(r",(\s*[}\]])").unwrap();
    let s = trailing.replace_all(&s, "$1");

    s.trim().to_string()
}

/// S3: assemble the per-call LLM input from the target shapes' extraction hints
/// plus the transcript. Pure — this is exactly where `extraction_hint` enters
/// the prompt. Shape (matches the system prompt in S5):
/// `{ "classes": [{ "name", "hint", "fields": [{ "name", "required", "hint" }] }],
///    "transcript": [{ "speaker", "text" }] }`.
pub fn build_extraction_input(shapes: &[ModelShape], transcript: &[(String, String)]) -> String {
    let classes: Vec<serde_json::Value> = shapes
        .iter()
        .map(|s| {
            let fields: Vec<serde_json::Value> = s
                .properties
                .iter()
                // The type flag is set by instance_links (S4), not the LLM.
                .filter(|p| !p.is_flag)
                .map(|p| {
                    serde_json::json!({
                        "name": p.name,
                        "required": p.is_required,
                        "hint": p.extraction_hint,
                    })
                })
                .collect();
            serde_json::json!({
                "name": class_local_name(&s.target_class),
                "hint": s.extraction_hint,
                "fields": fields,
            })
        })
        .collect();
    let turns: Vec<serde_json::Value> = transcript
        .iter()
        .map(|(speaker, text)| serde_json::json!({ "speaker": speaker, "text": text }))
        .collect();
    serde_json::json!({ "classes": classes, "transcript": turns }).to_string()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::model_query::shape::load_shape;
    use crate::perspectives::shacl_parser::parse_shacl_to_links;
    use crate::perspectives::sparql_store::SparqlStore;
    use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};

    const BELIEF_SDNA: &str = r#"{
      "target_class":"ns://Belief",
      "extraction_hint":"A claim a participant holds to be true about the world or the group. Not a task or a question.",
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://belief","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"One-sentence statement in the claimant's framing."}
      ]
    }"#;

    const INTENTION_SDNA: &str = r#"{
      "target_class":"ns://Intention",
      "extraction_hint":"Something a participant commits to doing - an actionable outcome with a plausible owner.",
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://intention","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Imperative summary of the work."},
        {"path":"ns://owner","name":"owner","min_count":0,"max_count":1,"resolve_language":"literal","extraction_hint":"Who committed to it, if stated."}
      ]
    }"#;

    /// Build a ModelShape via the real writer -> store -> loader path, so the
    /// class/property `extraction_hint`s are actually populated (the direct
    /// JSON path sets them to None).
    fn shape_from_sdna(class: &str, sdna: &str) -> ModelShape {
        let store = SparqlStore::new(None).unwrap();
        let target = format!("ns://{class}");
        let shape_uri = format!("ns://{class}Shape");
        let mut links = vec![
            Link {
                source: target.clone(),
                predicate: Some("rdf://type".into()),
                target: "ad4m://SubjectClass".into(),
            },
            Link {
                source: target,
                predicate: Some("ad4m://shape".into()),
                target: shape_uri,
            },
        ];
        links.extend(parse_shacl_to_links(sdna, class).unwrap());
        for l in links {
            store
                .add_link(&DecoratedLinkExpression {
                    author: "did:key:test".into(),
                    timestamp: "1700000000000".into(),
                    data: l,
                    proof: DecoratedExpressionProof {
                        key: "k".into(),
                        signature: "s".into(),
                        valid: Some(true),
                        invalid: Some(false),
                    },
                    status: None,
                })
                .unwrap();
        }
        load_shape(&store, class).unwrap()
    }

    #[test]
    fn extraction_hint_lands_in_prompt() {
        let shapes = vec![
            shape_from_sdna("Belief", BELIEF_SDNA),
            shape_from_sdna("Intention", INTENTION_SDNA),
        ];
        let input = build_extraction_input(
            &shapes,
            &[(
                "Nico".into(),
                "I'll extract the LLM processing into ADAM".into(),
            )],
        );

        // class-level hints reach the prompt
        assert!(input.contains("A claim a participant holds to be true"));
        assert!(input.contains("actionable outcome with a plausible owner"));
        // per-field hint + required flag
        assert!(input.contains("Imperative summary of the work"));
        assert!(input.contains("\"required\":true"));
        // transcript included
        assert!(input.contains("Nico") && input.contains("extract the LLM processing"));

        // valid JSON, two classes, type-flag excluded from fields
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        assert_eq!(v["classes"].as_array().unwrap().len(), 2);
        let intention = v["classes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"] == "Intention")
            .expect("Intention class in prompt");
        let field_names: Vec<&str> = intention["fields"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|f| f["name"].as_str())
            .collect();
        assert!(field_names.contains(&"title") && field_names.contains(&"owner"));
        assert!(
            !field_names.contains(&"type"),
            "type flag must not be a field"
        );
    }

    fn titles(instances: &[ProposedInstance]) -> Vec<&str> {
        instances
            .iter()
            .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
            .collect()
    }

    #[test]
    fn parses_clean_json_array() {
        let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing from Flux into ADAM","owner":"Nico"},
          {"class":"Belief","title":"Graph viz is the hardest part"}
        ]"#;
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].class, "Intention");
        assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("Nico"));
        assert_eq!(
            titles(&out),
            vec![
                "Extract LLM processing from Flux into ADAM",
                "Graph viz is the hardest part"
            ]
        );
    }

    #[test]
    fn strips_code_fences() {
        let raw = "```json\n[{\"class\":\"Belief\",\"title\":\"X\"}]\n```";
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Belief");
    }

    #[test]
    fn strips_think_block() {
        let raw =
            "<think>Let me find the intentions...</think>\n[{\"class\":\"Intention\",\"title\":\"Do X\"}]";
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Intention");
    }

    #[test]
    fn tolerates_trailing_commas() {
        let raw = r#"[
          {"class":"Task","title":"A",},
          {"class":"Task","title":"B"},
        ]"#;
        let out = parse_extraction_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(titles(&out), vec!["A", "B"]);
    }

    #[test]
    fn empty_array_yields_no_instances() {
        assert!(parse_extraction_response("[]").unwrap().is_empty());
        // and empty inside a fence / with whitespace
        assert!(parse_extraction_response("```json\n[]\n```")
            .unwrap()
            .is_empty());
    }

    #[test]
    fn garbage_is_an_error_not_a_panic() {
        assert!(parse_extraction_response("not json at all").is_err());
    }
}
