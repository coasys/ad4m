use super::{InterpretationOutput, ProposedInstance};

/// Parse a raw LLM response into proposed instances.
///
/// Local models wrap JSON in reasoning/markdown noise, so we first strip the
/// common wrappers (mirrors Flux `LLMutils.ts`): `<think>…</think>` blocks,
/// ```-fences, and trailing commas. Then parse as a JSON array.
///
/// This is the pre-slice-10.6 legacy contract: the LLM must return a JSON
/// array of instances, and anything else (including a bare object) is a
/// parse error so the retry loop can re-prompt. Post-slice-10.6 callers
/// that also want LLM-emitted flow proposals should use
/// [`parse_interpretation_output`], which additionally accepts the wrapping
/// object shape.
pub fn parse_interpretation_response(raw: &str) -> anyhow::Result<Vec<ProposedInstance>> {
    let cleaned = clean_llm_json(raw);
    let instances: Vec<ProposedInstance> = serde_json::from_str(&cleaned).map_err(|e| {
        anyhow::anyhow!(
            "interpretation JSON parse failed: {e}; cleaned payload length: {} bytes",
            cleaned.len()
        )
    })?;
    Ok(instances)
}

/// Parse a raw LLM response into the full [`InterpretationOutput`] payload —
/// both extracted instances and any LLM-emitted flow proposals.
///
/// Accepts two shapes so a model trained on the legacy prompt still works
/// while slice 10.6b rolls out the wrapping-object teach:
///
///   1. **Wrapping object** (post-slice-10.6): `{"instances": [...], "flow_proposals": [...]}`
///      — either key may be omitted; missing keys default to empty vectors
///      (see [`InterpretationOutput`]). Requires at least one of the two
///      keys to be present so a bare `{class, title}` object (a misplaced
///      instance) is still rejected as it was under the legacy contract.
///   2. **Bare array** (legacy): `[{class, ...}, ...]` — the whole array is
///      treated as `instances`, `flow_proposals` defaults to empty.
pub fn parse_interpretation_output(raw: &str) -> anyhow::Result<InterpretationOutput> {
    let cleaned = clean_llm_json(raw);
    let value: serde_json::Value = serde_json::from_str(&cleaned).map_err(|e| {
        anyhow::anyhow!(
            "interpretation JSON parse failed: {e}; cleaned payload length: {} bytes",
            cleaned.len()
        )
    })?;
    match value {
        serde_json::Value::Object(map) => {
            // Reject shapes that look like a lone misplaced instance
            // (`{class, ...}` without any wrapper keys) so a confused-LLM
            // response still errors out and the retry loop re-prompts —
            // matching the legacy [`parse_interpretation_response`] contract.
            if !map.contains_key("instances") && !map.contains_key("flow_proposals") {
                return Err(anyhow::anyhow!(
                    "interpretation JSON parse failed: object has neither `instances` nor `flow_proposals` key; cleaned payload length: {} bytes",
                    cleaned.len()
                ));
            }
            serde_json::from_value(serde_json::Value::Object(map)).map_err(|e| {
                anyhow::anyhow!(
                    "interpretation object parse failed: {e}; cleaned payload length: {} bytes",
                    cleaned.len()
                )
            })
        }
        serde_json::Value::Array(items) => {
            let instances: Vec<ProposedInstance> =
                serde_json::from_value(serde_json::Value::Array(items)).map_err(|e| {
                    anyhow::anyhow!(
                        "interpretation array parse failed: {e}; cleaned payload length: {} bytes",
                        cleaned.len()
                    )
                })?;
            Ok(InterpretationOutput {
                instances,
                flow_proposals: Vec::new(),
            })
        }
        other => Err(anyhow::anyhow!(
            "interpretation JSON parse failed: expected object or array at top level, got {}",
            match other {
                serde_json::Value::Null => "null",
                serde_json::Value::Bool(_) => "bool",
                serde_json::Value::Number(_) => "number",
                serde_json::Value::String(_) => "string",
                _ => unreachable!(),
            }
        )),
    }
}

/// Strip the reasoning/markdown noise local models add around JSON.
fn clean_llm_json(raw: &str) -> String {
    // 1. Drop reasoning blocks the model wraps around its answer. Gemma3/Qwen
    //    variously emit `<think>`, `<analysis>`, `<reasoning>` — strip each
    //    known tag (`regex` crate has no backreferences, so we alternate).
    let mut s = std::borrow::Cow::Borrowed(raw);
    for tag in ["think", "analysis", "reasoning", "scratchpad", "thought"] {
        let re = regex::Regex::new(&format!(r"(?s)<{tag}>.*?</{tag}>")).unwrap();
        // regex::Regex::replace_all returns Cow; keep threading it as owned
        // when it actually replaced anything.
        s = std::borrow::Cow::Owned(re.replace_all(&s, "").into_owned());
    }

    // 2. Drop code fences ```json / ``` (keep the inner content).
    let fence = regex::Regex::new(r"```[a-zA-Z0-9]*").unwrap();
    let s = fence.replace_all(&s, "");

    // 3. Extract the first JSON array (or object) if surrounded by prose.
    //    Mirrors Flux `LLMutils.ts` — models sometimes prefix an explanation
    //    even after `<think>`-stripping (e.g. gemma3 emitting plain prose).
    //    This MUST run before trailing-comma stripping: `strip_trailing_commas`
    //    tracks an `in_string` flag, and an odd number of `"` in the
    //    surrounding prose would invert it before the real JSON begins, so a
    //    comma inside a genuine string value could be dropped. Extracting the
    //    bracketed block first confines the string-scanner to actual JSON.
    //
    //    Prefer whichever outer bracket appears FIRST in the trimmed input.
    //    Pre-slice-10.6 the contract was array-only, so a naive `[` first
    //    fallback-`{` was safe; post-10.6 the contract also accepts
    //    `{"instances":[…],"flow_proposals":[…]}`, and the naive `[`-first
    //    would misidentify the inner `instances` array as the payload and
    //    swallow the outer object's tail.
    let candidate = s.trim();
    let first_obj = candidate.find('{');
    let first_arr = candidate.find('[');
    // Both branches fall back to the other bracket kind — if `{`-first
    // matches a balanced brace inside prose (an LLM aside like "OK, I'll
    // extract {a couple of things}: [{\"class\":\"X\"}]"), the primary
    // extract returns bogus JSON; the `[…]` fallback recovers on the
    // same tick instead of burning a retry.
    let extracted = match (first_obj, first_arr) {
        (Some(o), Some(a)) if o < a => extract_bracketed(candidate, '{', '}')
            .or_else(|| extract_bracketed(candidate, '[', ']')),
        (Some(_), None) => extract_bracketed(candidate, '{', '}'),
        _ => extract_bracketed(candidate, '[', ']')
            .or_else(|| extract_bracketed(candidate, '{', '}')),
    }
    .unwrap_or_else(|| candidate.to_string());

    // 4. Remove trailing commas before a closing } or ] (invalid JSON, common),
    //    now scoped to the extracted JSON. Skips commas inside string literals
    //    so values like "a, }" survive.
    strip_trailing_commas(&extracted)
}

/// Return the substring from the first `open` to the matching last `close`,
/// or `None` if `open` isn't present. Greedy on the outer pair so nested
/// brackets are preserved — matches Flux's `/(\[[\s\S]*\])/` intent.
fn extract_bracketed(s: &str, open: char, close: char) -> Option<String> {
    let start = s.find(open)?;
    let end = s.rfind(close)?;
    if end <= start {
        return None;
    }
    Some(s[start..=end].to_string())
}

/// Remove commas that appear immediately before `}` or `]` (with optional
/// whitespace in between), but only when the comma is outside a JSON string
/// literal.  This avoids mangling values like `"hello, }"`.
fn strip_trailing_commas(input: &str) -> String {
    let chars: Vec<char> = input.chars().collect();
    let len = chars.len();
    let mut out = String::with_capacity(input.len());
    let mut in_string = false;
    let mut escaped = false;
    let mut i = 0;

    while i < len {
        let c = chars[i];
        if in_string {
            out.push(c);
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                in_string = false;
            }
        } else if c == '"' {
            in_string = true;
            out.push(c);
        } else if c == ',' {
            let mut j = i + 1;
            while j < len && chars[j].is_ascii_whitespace() {
                j += 1;
            }
            if j < len && (chars[j] == '}' || chars[j] == ']') {
                // trailing comma before closing bracket — drop it
            } else {
                out.push(',');
            }
        } else {
            out.push(c);
        }
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;

    #[test]
    fn parses_clean_json_array() {
        let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing from Flux into ADAM","owner":"Nico"},
          {"class":"Belief","title":"Graph viz is the hardest part"}
        ]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].class, "Intention");
        assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("Nico"));
        assert_eq!(
            prop_values(&out, "title"),
            vec![
                "Extract LLM processing from Flux into ADAM",
                "Graph viz is the hardest part"
            ]
        );
    }

    #[test]
    fn strips_code_fences() {
        let raw = "```json\n[{\"class\":\"Belief\",\"title\":\"X\"}]\n```";
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Belief");
    }

    #[test]
    fn strips_think_block() {
        let raw =
            "<think>Let me find the intentions...</think>\n[{\"class\":\"Intention\",\"title\":\"Do X\"}]";
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Intention");
    }

    #[test]
    fn tolerates_trailing_commas() {
        let raw = r#"[
          {"class":"Task","title":"A",},
          {"class":"Task","title":"B"},
        ]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(prop_values(&out, "title"), vec!["A", "B"]);
    }

    #[test]
    fn trailing_comma_cleanup_preserves_commas_inside_strings() {
        let raw = r#"[
          {"class":"Belief","title":"Hello, world}"},
          {"class":"Task","title":"A, B, and C]"},
        ]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(
            out[0].props.get("title").unwrap().as_str(),
            Some("Hello, world}")
        );
        assert_eq!(
            out[1].props.get("title").unwrap().as_str(),
            Some("A, B, and C]")
        );
    }

    #[test]
    fn trailing_commas_stripped_despite_odd_quotes_in_prose() {
        // Regression: `clean_llm_json` must extract the JSON block BEFORE stripping
        // trailing commas. The prose prefix here carries an odd number of `"`
        // (one, before "here's"), which — if the comma-stripper scanned the whole
        // text — inverts its `in_string` flag before the real JSON begins, so the
        // genuine trailing commas below would not be stripped and the payload would
        // fail to parse. Extracting first confines the scanner to actual JSON.
        let raw = "The model replied: \"here's your data\n[\n  {\"class\":\"Task\",\"title\":\"A\",},\n  {\"class\":\"Task\",\"title\":\"B\"},\n]";
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(prop_values(&out, "title"), vec!["A", "B"]);
    }

    #[test]
    fn empty_array_yields_no_instances() {
        assert!(parse_interpretation_response("[]").unwrap().is_empty());
        // and empty inside a fence / with whitespace
        assert!(parse_interpretation_response("```json\n[]\n```")
            .unwrap()
            .is_empty());
    }

    #[test]
    fn garbage_is_an_error_not_a_panic() {
        assert!(parse_interpretation_response("not json at all").is_err());
    }

    #[test]
    fn extracts_array_from_surrounding_prose() {
        // Real gemma3:12b output observed on CI 2026-08-07 (job 19580):
        // wrapped its reply in <analysis> narration followed by the JSON array.
        let raw = r#"<analysis>
    Turn 1: Nico assigns work to James.
    Turn 2: Sure -> commitment (Task); "still think the WS layer is cleanest" -> Belief.
    Turn 3: Nico asks about perspectives with no subject classes -> Question.
</analysis>


[
  {"class": "ExtTask", "title": "Write the integration test for the interpretation endpoint", "owner": "James"},
  {"class": "ExtBelief", "title": "The WS layer is the cleanest way to expose this"},
  {"class": "ExtQuestion", "title": "How do we handle a perspective that has no subject classes registered?"}
]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 3);
        assert_eq!(out[0].class, "ExtTask");
        assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("James"));
        assert_eq!(out[2].class, "ExtQuestion");
    }

    #[test]
    fn extracts_single_object_when_no_array() {
        let raw = "Here is the extracted item:\n{\"class\":\"Belief\",\"title\":\"X\"}\nthanks";
        // A bare object isn't the interpretation contract (array of instances) so
        // this must still error — but extract_bracketed shouldn't panic.
        let err = parse_interpretation_response(raw).unwrap_err();
        let msg = format!("{err}");
        assert!(
            msg.contains("interpretation JSON parse failed"),
            "got: {msg}"
        );
    }

    #[test]
    fn parse_error_does_not_leak_llm_payload() {
        // The cleaned LLM payload can carry the raw conversation transcript. It
        // must not appear in the error message, because
        // retry_interpretation_output_parse logs this error on every failed
        // attempt. Only safe metadata (length) is allowed to surface.
        let secret = "TOP_SECRET_DINNER_PLAN alice met bob at the safehouse";
        let raw = format!("[{{ \"class\":\"Note\", \"title\":\"{secret}\", NOT_JSON");
        let err = parse_interpretation_response(&raw).unwrap_err();
        let msg = format!("{err}");
        assert!(
            !msg.contains(secret),
            "parse error must not include the LLM payload; got: {msg}"
        );
        assert!(
            msg.contains("payload length"),
            "parse error must include the payload length metadata; got: {msg}"
        );
    }

    #[tokio::test]
    async fn retry_interpretation_output_parse_succeeds_on_first_attempt() {
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_interpretation_output_parse(move |_| {
            let a = attempts_clone.clone();
            async move {
                a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Ok(r#"[{"class":"Belief","title":"X"}]"#.to_string())
            }
        })
        .await
        .unwrap();
        assert_eq!(out.instances.len(), 1);
        assert!(out.flow_proposals.is_empty());
        assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn retry_interpretation_output_parse_recovers_after_bad_parse() {
        // First attempt returns unparseable garbage; second returns valid JSON.
        // retry_interpretation_output_parse must call again and succeed within
        // budget.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_interpretation_output_parse(move |_| {
            let a = attempts_clone.clone();
            async move {
                let n = a.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1;
                if n == 1 {
                    Ok("total garbage, not json".to_string())
                } else {
                    Ok(r#"[{"class":"Intention","title":"Y"}]"#.to_string())
                }
            }
        })
        .await
        .unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn retry_interpretation_output_parse_fails_after_max_attempts() {
        // Every attempt returns garbage → we exhaust INTERPRETATION_MAX_ATTEMPTS
        // and propagate the last parse error rather than looping forever.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let result: anyhow::Result<InterpretationOutput> =
            retry_interpretation_output_parse(move |_| {
                let a = attempts_clone.clone();
                async move {
                    a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    Ok("never parseable".to_string())
                }
            })
            .await;
        assert!(result.is_err());
        assert_eq!(
            attempts.load(std::sync::atomic::Ordering::SeqCst),
            INTERPRETATION_MAX_ATTEMPTS
        );
    }

    // --- slice 10.6a: parse_interpretation_output covers both shapes ---

    #[test]
    fn parse_output_legacy_bare_array_yields_instances_only() {
        // Legacy shape must still parse — a pre-slice-10.6 model gets its
        // instances into the new wrapper, flow_proposals stays empty. This
        // is the fallback that keeps the parser back-compat during the
        // slice 10.6b prompt migration.
        let raw = r#"[{"class":"Intention","title":"Do X"}]"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Intention");
        assert!(out.flow_proposals.is_empty());
    }

    #[test]
    fn parse_output_wrapping_object_with_both_keys() {
        // The full slice-10.6 shape: both instances and flow_proposals
        // populated. `toState` on the wire → `to_state` in Rust (serde
        // rename), `reason` optional.
        let raw = r#"{
          "instances": [
            {"class":"Task","title":"Ship the PR","owner":"Nico"}
          ],
          "flow_proposals": [
            {
              "instance":"ad4m://flow/instance/delivery-42",
              "toState":"review",
              "reason":"PR is up, please review"
            }
          ]
        }"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Task");
        assert_eq!(out.flow_proposals.len(), 1);
        let p = &out.flow_proposals[0];
        assert_eq!(p.instance, "ad4m://flow/instance/delivery-42");
        assert_eq!(p.to_state, "review");
        assert_eq!(p.reason.as_deref(), Some("PR is up, please review"));
    }

    #[test]
    fn parse_output_wrapping_object_omitting_flow_proposals_defaults_empty() {
        // A model that emits only the wrapping shape but no flow proposals
        // (transcript unrelated to any active flow) is valid — the field
        // just defaults to empty.
        let raw = r#"{"instances":[{"class":"Belief","title":"X"}]}"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert!(out.flow_proposals.is_empty());
    }

    #[test]
    fn parse_output_wrapping_object_omitting_instances_defaults_empty() {
        // Symmetric: a response that only proposes flow transitions and
        // extracts no new instances is valid.
        let raw = r#"{
          "flow_proposals": [
            {"instance":"ad4m://flow/instance/x","toState":"done"}
          ]
        }"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert!(out.instances.is_empty());
        assert_eq!(out.flow_proposals.len(), 1);
        assert!(out.flow_proposals[0].reason.is_none());
    }

    #[test]
    fn parse_output_rejects_lone_misplaced_instance_object() {
        // `{class,title}` at the top level is a confused-LLM response — not
        // a valid wrapping object, not a valid legacy array. Must error so
        // the retry loop re-prompts, matching the pre-slice-10.6 contract.
        let raw = r#"{"class":"Belief","title":"X"}"#;
        let err = parse_interpretation_output(raw).unwrap_err();
        let msg = format!("{err}");
        assert!(
            msg.contains("neither `instances` nor `flow_proposals`"),
            "got: {msg}"
        );
    }

    #[test]
    fn parse_output_wrapping_object_tolerates_extra_top_level_keys() {
        // Small models sometimes tack on `<metadata>` fields; the wrapper
        // must ignore anything it doesn't recognise so a stray key doesn't
        // torpedo an otherwise-valid response.
        let raw = r#"{
          "instances":[{"class":"Task","title":"A"}],
          "flow_proposals":[],
          "notes":"the model added this on its own"
        }"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
    }

    #[test]
    fn parse_output_wrapping_object_rejects_malformed_flow_proposal() {
        // A flow_proposals entry that omits the required `instance` field
        // must error — the engine cannot look up a FlowInstance without it.
        let raw = r#"{
          "instances":[],
          "flow_proposals":[{"toState":"review"}]
        }"#;
        let err = parse_interpretation_output(raw).unwrap_err();
        let msg = format!("{err}");
        assert!(
            msg.contains("interpretation object parse failed"),
            "got: {msg}"
        );
    }

    #[test]
    fn parse_output_strips_reasoning_wrappers_around_object() {
        // The wrapping-object path must run through the same clean_llm_json
        // pipeline as the legacy array path — code fences, <think> blocks,
        // and trailing commas all still apply.
        let raw = "```json\n<think>let me think...</think>\n{\"instances\":[{\"class\":\"Task\",\"title\":\"A\",}],\"flow_proposals\":[]}\n```";
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Task");
    }
}
