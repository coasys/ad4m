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

    // 3. Extract the first JSON array (or object) if surrounded by prose.
    //    Mirrors Flux `LLMutils.ts` — models sometimes prefix an explanation
    //    even after `<think>`-stripping (e.g. gemma3 emitting plain prose).
    //    This MUST run before trailing-comma stripping: `strip_trailing_commas`
    //    tracks an `in_string` flag, and an odd number of `"` in the
    //    surrounding prose would invert it before the real JSON begins, so a
    //    comma inside a genuine string value could be dropped. Extracting the
    //    bracketed block first confines the string-scanner to actual JSON.
    let candidate = s.trim();
    let extracted = extract_bracketed(candidate, '[', ']')
        .or_else(|| extract_bracketed(candidate, '{', '}'))
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
