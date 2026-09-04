use super::{InterpretationOutput, ProposedInstance};

/// Parse a raw LLM response into proposed instances.
///
/// Local models wrap JSON in reasoning/markdown noise, so we first strip the
/// common wrappers (mirrors Flux `LLMutils.ts`): `<think>…</think>` blocks,
/// ```-fences, and trailing commas. Then parse as a JSON array.
pub fn parse_interpretation_response(raw: &str) -> anyhow::Result<Vec<ProposedInstance>> {
    parse_interpretation_output(raw).map(|out| out.instances)
}

/// Parse a raw LLM response into instances plus flow proposals. Accepts the
/// wrapping object `{ "instances": [...], "flow_proposals": [...] }` as well
/// as a bare array of instances.
pub fn parse_interpretation_output(raw: &str) -> anyhow::Result<InterpretationOutput> {
    let cleaned = clean_llm_json(raw);
    let value: serde_json::Value = serde_json::from_str(&cleaned).map_err(|e| {
        anyhow::anyhow!(
            "interpretation JSON parse failed: {e}; cleaned payload length: {} bytes",
            cleaned.len()
        )
    })?;
    match value {
        serde_json::Value::Array(_) => Ok(InterpretationOutput {
            instances: serde_json::from_value(value)
                .map_err(|e| anyhow::anyhow!("interpretation JSON parse failed: {e}"))?,
            flow_proposals: Vec::new(),
        }),
        serde_json::Value::Object(_) => serde_json::from_value(value)
            .map_err(|e| anyhow::anyhow!("interpretation JSON parse failed: {e}")),
        _ => Err(anyhow::anyhow!(
            "interpretation JSON must be an array or an object at the top level"
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
    let candidate = s.trim();
    // Strict-parse at each TOP-LEVEL bracket position in order, taking the
    // first position where a complete JSON value parses; fall back to the
    // greedy spans when none does (trailing commas are repaired below).
    //
    // Top-level-only strict attempts are what make this safe for every
    // payload shape this branch accepts: a prose-only bracket (e.g.
    // `I'll extract {a couple of things}: [...]`) fails its strict parse and
    // the scan moves on to the real payload — see the
    // `prose_braces_before_the_real_array_*` regression test — and a
    // wrapper object `{ "instances": [...], "flow_proposals": [...] }` is
    // taken whole rather than truncated to its inner instances array, which
    // an array-first chain would silently do (dropping flow_proposals).
    // Restricting to depth-0 positions keeps a repair-needing outer array
    // (trailing commas) from being hijacked by a valid inner object: the
    // inner positions are never strict-tried, so the outer falls through to
    // the greedy span + comma repair. The depth scan is string-aware; prose
    // with unbalanced quotes merely skews the scan towards the greedy
    // fallbacks, which is the pre-existing behaviour for that case.
    let mut top_level_starts = Vec::new();
    {
        let (mut depth, mut in_string, mut escaped) = (0i32, false, false);
        for (i, c) in candidate.char_indices() {
            if in_string {
                match c {
                    _ if escaped => escaped = false,
                    '\\' => escaped = true,
                    '"' => in_string = false,
                    _ => {}
                }
                continue;
            }
            match c {
                '"' => in_string = true,
                '[' | '{' => {
                    if depth == 0 {
                        top_level_starts.push(i);
                    }
                    depth += 1;
                }
                ']' | '}' => depth = (depth - 1).max(0),
                _ => {}
            }
        }
    }
    // Among the strict-parsing candidates, prefer the first that actually
    // DESERIALIZES as an interpretation payload: a model may emit an
    // unrelated-but-valid JSON value in its prose before the real payload —
    // an object (`{"model": "gemma3"}`) or, just as easily, a scalar array
    // (`["Task"]`) — and taking it just because it parses would fail the
    // semantic parse and burn a retry while the real payload sits ignored
    // right behind it. When nothing fully deserializes, fall back to the
    // first candidate merely SHAPED like a payload (array / object with an
    // `instances` key), so a slightly-malformed real payload still wins
    // over prose values and the semantic parse reports ITS mismatch rather
    // than the prose value's. When even that misses, keep the first valid
    // value — old behaviour.
    let strict_candidates: Vec<String> = top_level_starts
        .iter()
        .filter_map(|&i| extract_first_json_value(candidate, i))
        .collect();
    let extracted = strict_candidates
        .iter()
        .find(|c| parses_as_interpretation_payload(c))
        .or_else(|| {
            strict_candidates
                .iter()
                .find(|c| looks_like_interpretation_payload(c))
        })
        .or_else(|| strict_candidates.first())
        .cloned()
        .or_else(|| extract_bracketed(candidate, '[', ']'))
        .or_else(|| extract_bracketed(candidate, '{', '}'))
        .unwrap_or_else(|| candidate.to_string());

    // 4. Remove trailing commas before a closing } or ] (invalid JSON, common),
    //    now scoped to the extracted JSON. Skips commas inside string literals
    //    so values like "a, }" survive.
    strip_trailing_commas(&extracted)
}

/// Semantic payload check used as the first ranking tier for strict
/// candidates: does this JSON actually deserialize into one of the two
/// accepted payload shapes (bare instance array, or wrapper object)? An
/// unrelated scalar array like `["Task"]` parses as JSON and passes the
/// structural check below, but fails here — so the real payload behind it
/// still wins.
fn parses_as_interpretation_payload(candidate: &str) -> bool {
    match serde_json::from_str::<serde_json::Value>(candidate) {
        Ok(v @ serde_json::Value::Array(_)) => {
            serde_json::from_value::<Vec<ProposedInstance>>(v).is_ok()
        }
        Ok(v @ serde_json::Value::Object(_)) => {
            serde_json::from_value::<InterpretationOutput>(v).is_ok()
        }
        _ => false,
    }
}

/// Structural (not semantic) payload check — second ranking tier: an array,
/// or an object carrying an `instances` key. Keeps a slightly-malformed real
/// payload ranked above prose values so the semantic parse error points at
/// the payload, not the prose.
fn looks_like_interpretation_payload(candidate: &str) -> bool {
    match serde_json::from_str::<serde_json::Value>(candidate) {
        Ok(serde_json::Value::Array(_)) => true,
        Ok(serde_json::Value::Object(map)) => map.contains_key("instances"),
        _ => false,
    }
}

/// Strictly parse one JSON value starting at `start` and return exactly its
/// span, so trailing prose is not swallowed into the payload.
fn extract_first_json_value(s: &str, start: usize) -> Option<String> {
    let mut stream =
        serde_json::Deserializer::from_str(&s[start..]).into_iter::<serde_json::Value>();
    stream.next()?.ok()?;
    Some(s[start..start + stream.byte_offset()].to_string())
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
    use crate::perspectives::interpretation::LlmFlowProposal;
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
    fn prose_braces_before_the_real_array_dont_swallow_the_payload() {
        // Brace-prose before the array: the array-first chain must not let the
        // `{a couple of things}` span become the payload. Under an
        // object-first ordering the greedy `{`-matcher would short-circuit
        // here and the real payload would never be tried.
        let raw = r#"OK, I'll extract {a couple of things}: [{"class":"Task","title":"A"}]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].class, "Task");
        assert_eq!(prop_values(&out, "title"), vec!["A"]);
    }

    #[test]
    fn unrelated_valid_json_before_the_real_payload_is_skipped() {
        // A syntactically valid but unrelated object in the prose must not
        // win just because it parses: the scan prefers the first candidate
        // shaped like a payload (array, or object with `instances`).
        let raw = r#"Config used: {"model": "gemma3", "temp": 0.2}. Result: {"instances":[{"class":"Task","title":"A"}],"flow_proposals":[]}"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Task");

        // Same with a bare-array payload after an unrelated object.
        let raw = r#"Notes: {"irrelevant": true} then [{"class":"Task","title":"B"}]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(prop_values(&out, "title"), vec!["B"]);

        // No payload-shaped candidate at all: first valid value is still
        // taken and the semantic parse reports the mismatch (old behaviour).
        assert!(parse_interpretation_output(r#"Just: {"note": "hi"}"#).is_err());
    }

    #[test]
    fn unrelated_scalar_array_before_the_real_payload_is_skipped() {
        // A scalar array is valid JSON and array-shaped, but does not
        // deserialize as instances — it must not outrank the real payload.
        let raw = r#"Classes seen: ["Task"]. Result: {"instances":[{"class":"Task","title":"A"}],"flow_proposals":[]}"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Task");

        // Same with a bare-array payload after the unrelated scalar array.
        let raw = r#"Classes seen: ["Task", "Belief"] then [{"class":"Task","title":"B"}]"#;
        let out = parse_interpretation_response(raw).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(prop_values(&out, "title"), vec!["B"]);
    }

    #[test]
    fn wrapper_object_after_prose_keeps_flow_proposals() {
        // A wrapper object must be taken whole: an array-first extraction
        // chain would strict-parse the inner `instances` array and silently
        // drop `flow_proposals`.
        let raw = r#"Here {you go}: {"instances":[{"class":"Task","title":"A"}],"flow_proposals":[{"instance":"ad4m://flow/instance/1","toState":"scoped"}]}"#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.flow_proposals.len(), 1);
        assert_eq!(out.flow_proposals[0].to_state, "scoped");
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
        // must not appear in the error message, because retry_interpretation_parse
        // logs this error on every failed attempt. Only safe metadata (length) is
        // allowed to surface.
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
    async fn retry_interpretation_parse_succeeds_on_first_attempt() {
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_interpretation_parse(move |_| {
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
    async fn retry_interpretation_parse_recovers_after_bad_parse() {
        // First attempt returns unparseable garbage; second returns valid JSON.
        // retry_interpretation_parse must call again and succeed within budget.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let out = retry_interpretation_parse(move |_| {
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
    async fn retry_interpretation_parse_fails_after_max_attempts() {
        // Every attempt returns garbage → we exhaust INTERPRETATION_MAX_ATTEMPTS
        // and propagate the last parse error rather than looping forever.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let result: anyhow::Result<InterpretationOutput> = retry_interpretation_parse(move |_| {
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

    #[test]
    fn output_wrapping_object_carries_instances_and_flow_proposals() {
        let raw = r#"Here you go:
        {
          "instances": [ {"class":"Task","title":"Ship the PR"} ],
          "flow_proposals": [
            {"instance":"ad4m://flow/instance/delivery-42","toState":"review","reason":"PR is up"}
          ]
        }
        Let me know if you need more."#;
        let out = parse_interpretation_output(raw).unwrap();
        assert_eq!(out.instances.len(), 1);
        assert_eq!(out.instances[0].class, "Task");
        assert_eq!(
            out.flow_proposals,
            vec![LlmFlowProposal {
                instance: "ad4m://flow/instance/delivery-42".into(),
                to_state: "review".into(),
                reason: Some("PR is up".into()),
            }]
        );
    }

    #[test]
    fn output_accepts_bare_array_and_object_without_flow_proposals() {
        let bare =
            parse_interpretation_output(r#"[{"class":"Intention","title":"Do X"}]"#).unwrap();
        assert_eq!(bare.instances.len(), 1);
        assert!(bare.flow_proposals.is_empty());

        let wrapped =
            parse_interpretation_output(r#"{"instances":[{"class":"Belief","title":"X"}]}"#)
                .unwrap();
        assert_eq!(wrapped.instances.len(), 1);
        assert!(wrapped.flow_proposals.is_empty());
    }

    #[test]
    fn output_rejects_lone_instance_object_and_malformed_proposal() {
        assert!(parse_interpretation_output(r#"{"class":"Belief","title":"X"}"#).is_err());
        assert!(parse_interpretation_output(
            r#"{"instances":[],"flow_proposals":[{"toState":"review"}]}"#
        )
        .is_err());
    }
}
