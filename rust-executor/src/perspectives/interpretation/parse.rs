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
    let trailing = regex::Regex::new(r",(\s*[}\]])").unwrap();
    let s = trailing.replace_all(&s, "$1");

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
