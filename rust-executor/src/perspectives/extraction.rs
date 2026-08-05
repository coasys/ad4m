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

#[cfg(test)]
mod tests {
    use super::*;

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
