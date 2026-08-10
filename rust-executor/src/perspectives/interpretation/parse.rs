use super::ProposedInstance;

/// Parse a raw LLM response into proposed instances.
///
/// Local models wrap JSON in reasoning/markdown noise, so we first strip the
/// common wrappers (mirrors Flux `LLMutils.ts`): `<think>…</think>` blocks,
/// ```-fences, and trailing commas. Then parse as a JSON array.
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

    // 3. Remove trailing commas before a closing } or ] (invalid JSON, common).
    //    Must skip commas inside string literals so values like "a, }" survive.
    let s: std::borrow::Cow<'_, str> = std::borrow::Cow::Owned(strip_trailing_commas(&s));

    // 4. Extract the first JSON array (or object) if surrounded by prose.
    //    Mirrors Flux `LLMutils.ts` — models sometimes prefix an explanation
    //    even after `<think>`-stripping (e.g. gemma3 emitting plain prose).
    let candidate = s.trim();
    if let Some(extracted) =
        extract_bracketed(candidate, '[', ']').or_else(|| extract_bracketed(candidate, '{', '}'))
    {
        return extracted;
    }
    candidate.to_string()
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
        assert_eq!(out.len(), 1);
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
        assert_eq!(out.len(), 1);
        assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn retry_interpretation_parse_fails_after_max_attempts() {
        // Every attempt returns garbage → we exhaust INTERPRETATION_MAX_ATTEMPTS
        // and propagate the last parse error rather than looping forever.
        let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
        let attempts_clone = attempts.clone();
        let result: anyhow::Result<Vec<ProposedInstance>> = retry_interpretation_parse(move |_| {
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
}
