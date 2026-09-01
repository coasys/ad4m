//! Optional 2nd-pass semantic-check LLM confirmation gate.
//!
//! A `FlowState` may declare `semanticCheck: "<English sentence>"` — a
//! condition that must ALSO hold before the state's proposal fires,
//! confirmed by a targeted small-LLM call. Structural `requires` matches
//! remain load-bearing; the semantic check is an additional gate that
//! defaults to fail-safe (an uncertain LLM must not silently advance a
//! flow).
//!
//! # What this module owns
//!
//! Pure primitives (no LLM call, no perspective I/O):
//!
//! - [`SemanticCheckVerdict`] — the tri-state outcome (`Pass` / `Fail` /
//!   `Ambiguous`) the async layer will produce from an
//!   LLM response and slice 10.5b will consume as the fire/discard gate.
//! - [`build_semantic_check_prompt`] — assembles the targeted small
//!   prompt from a `SatisfiedTransition` + its parent `FlowContext`.
//!   Returns `None` when the transition carries no `semantic_check`
//!   hint — the caller short-circuits and treats that as auto-pass.
//! - [`parse_semantic_check_response`] — permissive parse of an LLM
//!   response into a [`SemanticCheckVerdict`]. Accepts common
//!   affirmations/negations, tolerates code fences and leading
//!   whitespace, defaults to `Ambiguous` when the first non-empty line
//!   does not carry a decisive token.
//! - [`should_fire_proposal`] — the fire/discard policy the writer stage
//! will apply per transition. Ambiguous defaults to
//!   discard: an uncertain LLM must not silently advance a flow.
//!
//! # Why a separate module
//!
//! `flow_evaluator` is already responsible for producing the guarded
//! `SatisfiedTransition`s; wedging the semantic-check prompt/parse logic
//! in there would blur two responsibilities (deterministic guard vs.
//! LLM confirmation) and inflate the module's test surface. Following
//! the same shape as `flow_classes` / `flow_context` / `flow_evaluator`
//! (leaf pure module → async wrapper → wire-up) keeps each slice
//! digestible and the onion shells testable in isolation.

#![allow(dead_code)]

use anyhow::Result;

use crate::perspectives::flow_context::FlowContext;
use crate::perspectives::flow_evaluator::SatisfiedTransition;

/// Outcome of the optional 2nd-pass semantic-check LLM call.
///
/// - `Pass` — LLM affirmed the check; the deterministic proposal fires.
/// - `Fail` — LLM negated the check; the proposal is discarded and the
///   flow does not advance on this pass.
/// - `Ambiguous` — LLM response did not carry a decisive token (empty,
///   free-form, or off-vocabulary). Treated as fail-safe: the proposal
///   is discarded so an uncertain confirmation cannot silently move a
///   flow forward.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticCheckVerdict {
    Pass,
    Fail,
    Ambiguous,
}

/// The fire/discard gate the writer stage applies per transition.
///
/// Currently a total function on the verdict: only `Pass` fires.
/// Extracted as its own primitive so slice 10.5b's call site is a
/// one-liner and future policy tweaks (e.g. a threshold on repeated
/// ambiguous responses) live behind one symbol.
pub fn should_fire_proposal(verdict: SemanticCheckVerdict) -> bool {
    matches!(verdict, SemanticCheckVerdict::Pass)
}

/// Assemble the targeted small prompt that asks a confirmation LLM
/// whether the transition's `semanticCheck` hint really holds against
/// the just-satisfied structural evidence.
///
/// Returns `None` when the transition carries no `semantic_check`
/// hint. Slice 10.5b short-circuits on `None` and treats the
/// deterministic requires as sufficient (auto-pass). Returning a
/// bare `String` here would force the caller to inspect the option
/// twice.
///
/// The prompt structure is:
///
/// 1. One-sentence framing of the task (LLM as verifier).
/// 2. Flow name + flow-level `interpretationHint` (when set) so the
///    LLM has global framing.
/// 3. Explicit `FROM` / `TO` states.
/// 4. The `semanticCheck` string, quoted verbatim.
/// 5. The evidence bag (matched instance IDs from the deterministic
///    guard) so the LLM knows what the engine already accepted.
/// 6. Instructions: answer exactly one of `YES` / `NO` / `UNCLEAR`
///    on the first line. Case-insensitive matching is up to the
///    parser.
///
/// The prompt deliberately does NOT include a transcript excerpt in
/// this slice — that couples 10.5a1 to the auto-processor's transcript
/// buffer and blocks the pure-testable shape. Slice 10.5a2 will thread
/// a `Option<&str>` transcript_excerpt through when the async wrapper
/// is added and the auto-processor's context is available.
pub fn build_semantic_check_prompt(
    transition: &SatisfiedTransition,
    flow_ctx: &FlowContext,
) -> Option<String> {
    let hint = transition.semantic_check.as_deref()?;

    let mut prompt = String::new();
    prompt.push_str(
        "You are checking whether a specific state transition in a group's shared workflow really applies.\n\n",
    );
    prompt.push_str(&format!("## Flow: {}\n", transition.flow_name));
    if let Some(flow_hint) = flow_ctx.flow_interpretation_hint.as_deref() {
        prompt.push_str(&format!("{flow_hint}\n"));
    }
    prompt.push('\n');
    prompt.push_str("## Transition\n");
    prompt.push_str(&format!("FROM: {}\n", transition.from_state));
    prompt.push_str(&format!("TO:   {}\n\n", transition.to_state));
    prompt.push_str("## Semantic check to verify\n");
    prompt.push_str(hint);
    prompt.push_str("\n\n## Evidence found in the graph\n");
    if transition.evidence_ids.is_empty() {
        prompt.push_str("(none)\n");
    } else {
        for id in &transition.evidence_ids {
            prompt.push_str(&format!("- {id}\n"));
        }
    }
    prompt.push_str(
        "\n## Instructions\nAnswer with exactly one word on the first line: YES, NO, or UNCLEAR.\n",
    );
    prompt.push_str("YES = the semantic check is satisfied by the current evidence.\n");
    prompt.push_str("NO = the evidence contradicts or does not support the check.\n");
    prompt.push_str("UNCLEAR = the check cannot be confidently evaluated.\n");
    Some(prompt)
}

/// Parse a raw LLM response string into a [`SemanticCheckVerdict`].
///
/// Permissive by design — small models routinely wrap answers in code
/// fences, prepend reasoning, or emit trailing punctuation. The parser:
///
/// 1. Strips leading/trailing whitespace.
/// 2. Peels off surrounding triple-backtick code fences if present.
/// 3. Takes the first non-empty line.
/// 4. Uppercases and strips punctuation from the first whitespace-
///    separated token.
/// 5. Matches against the vocabulary:
///    - `YES` / `TRUE` / `PASS` / `CONFIRM` / `CONFIRMED` / `Y` → `Pass`
///    - `NO` / `FALSE` / `FAIL` / `FAILED` / `REJECT` / `REJECTED` / `N`
///      → `Fail`
///    - anything else → `Ambiguous`
///
/// The vocabulary matches the [`build_semantic_check_prompt`] instructions
/// (YES/NO/UNCLEAR) plus common synonyms so a small model that veers off
/// vocabulary can still land on a decisive verdict — but only when its
/// intent is unambiguous. `UNCLEAR` itself parses to `Ambiguous`.
pub fn parse_semantic_check_response(raw: &str) -> SemanticCheckVerdict {
    let stripped = strip_code_fence(raw.trim());
    let first_line = stripped
        .lines()
        .find(|l| !l.trim().is_empty())
        .unwrap_or("");
    let Some(first_token_raw) = first_line.split_whitespace().next() else {
        return SemanticCheckVerdict::Ambiguous;
    };
    let first_token: String = first_token_raw
        .trim_matches(|c: char| c.is_ascii_punctuation())
        .to_uppercase();
    match first_token.as_str() {
        "YES" | "TRUE" | "PASS" | "CONFIRM" | "CONFIRMED" | "Y" => SemanticCheckVerdict::Pass,
        "NO" | "FALSE" | "FAIL" | "FAILED" | "REJECT" | "REJECTED" | "N" => {
            SemanticCheckVerdict::Fail
        }
        _ => SemanticCheckVerdict::Ambiguous,
    }
}

/// Peel off a surrounding triple-backtick code fence if present. Handles
/// both plain `` ``` `` and `` ```lang `` opening fences. Returns the
/// inner content unchanged when no fence is present.
fn strip_code_fence(input: &str) -> &str {
    let trimmed = input.trim();
    let Some(after_open) = trimmed.strip_prefix("```") else {
        return trimmed;
    };
    // Drop an optional language tag by advancing to the first newline.
    let after_lang = match after_open.find('\n') {
        Some(nl) => &after_open[nl + 1..],
        None => after_open,
    };
    match after_lang.rfind("```") {
        Some(idx) => after_lang[..idx].trim_end(),
        None => after_lang,
    }
}

// --------------------------------------------------------------------------
// async layer over an LLM seam
// --------------------------------------------------------------------------

/// The LLM seam the semantic-check pass calls into. Kept as a trait so the
/// call site can be exercised end-to-end with a canned-response stub — no
/// real LLM, no network — in the same shape the auto-processor's harness
/// uses ([`crate::ai_service::harness::CompletionSource`]).
///
/// The single real implementation delegates to `AIService::prompt` (slice
/// 10.5b wires that in from the auto-processor context). This trait is
/// intentionally narrower than `CompletionSource`: semantic check never
/// needs tool-calling, streaming, or credit-gate composition.
#[async_trait::async_trait]
pub trait SemanticCheckLlm: Send + Sync {
    /// Send a completion `prompt` to `model_id`, get raw text back.
    /// Callers pass the output to [`parse_semantic_check_response`].
    async fn confirm(&self, model_id: &str, prompt: &str) -> Result<String>;
}

/// Run the optional 2nd-pass semantic-check LLM confirmation for one
/// [`SatisfiedTransition`].
///
/// Behaviour:
///
/// 1. If the transition carries no `semantic_check` hint,
///    [`build_semantic_check_prompt`] returns `None` and we auto-pass —
///    the deterministic guard from `flow_evaluator` is treated as
///    sufficient. **No LLM call is made.**
/// 2. Otherwise, call `llm.confirm(model_id, prompt)` and parse the raw
///    response with [`parse_semantic_check_response`]. The verdict is
///    returned verbatim; the fire/discard decision is
///    [`should_fire_proposal`]'s responsibility at the call site so the
///    caller can still surface `Fail` / `Ambiguous` in debug output.
/// 3. LLM errors bubble up as `Err`. Slice 10.5b will map the `Err` case
///    to "discard this transition and log a warning" so the extraction
///    pass never breaks on a flow-layer LLM failure — but that mapping
///    is a call-site concern, not this function's, to keep the async
///    layer honest about I/O failures.
pub async fn run_semantic_check(
    llm: &dyn SemanticCheckLlm,
    model_id: &str,
    transition: &SatisfiedTransition,
    flow_ctx: &FlowContext,
) -> Result<SemanticCheckVerdict> {
    let Some(prompt) = build_semantic_check_prompt(transition, flow_ctx) else {
        return Ok(SemanticCheckVerdict::Pass);
    };
    let raw = llm.confirm(model_id, &prompt).await?;
    Ok(parse_semantic_check_response(&raw))
}

// --------------------------------------------------------------------------
// AIService-backed real implementation
// --------------------------------------------------------------------------

/// Real [`SemanticCheckLlm`] that delegates to
/// [`crate::ai_service::AIService::prompt`].
///
/// The check reuses the caller-supplied `task_id` so the semantic-check
/// completion is dispatched onto the same spawned worker (and the same
/// billing scope) as the extraction pass it gates. `model_id` on the trait
/// method is ignored here: the routing model is baked into the task row
/// AIService already looks up, so passing a different id would be a lie.
///
/// The `run.rs` call sites build one of these per interpretation pass and
/// hand it in with the pass's `task.model_id` for the *hint-rendering*
/// side (which is where the trait's `model_id` argument actually shows up
/// — in the semantic-check prompt header, not in AIService dispatch).
pub struct AIServiceSemanticCheck {
    pub task_id: String,
}

#[async_trait::async_trait]
impl SemanticCheckLlm for AIServiceSemanticCheck {
    async fn confirm(&self, _model_id: &str, prompt: &str) -> Result<String> {
        let ai = crate::ai_service::AIService::global_instance()
            .await
            .map_err(|e| anyhow::anyhow!("AIService not ready: {e:#}"))?;
        let res = ai
            .prompt(self.task_id.clone(), prompt.to_string(), None)
            .await
            .map_err(|e| anyhow::anyhow!("AIService::prompt failed: {e:#}"))?;
        Ok(res.text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_context::FlowContext;
    use crate::perspectives::flow_evaluator::SatisfiedTransition;

    fn ctx() -> FlowContext {
        FlowContext {
            flow_name: "Deliberation".to_string(),
            instance_uri: "ad4m://flow/instance/a".to_string(),
            subject: "ad4m://proposal/x".to_string(),
            current_state: "proposal".to_string(),
            flow_interpretation_hint: Some("A workflow for resolving group tensions.".to_string()),
            reachable_next_states: vec![],
            consensus_rule: None,
        }
    }

    fn transition(semantic_check: Option<&str>) -> SatisfiedTransition {
        SatisfiedTransition {
            flow_name: "Deliberation".to_string(),
            instance_uri: "ad4m://flow/instance/a".to_string(),
            subject: "ad4m://proposal/x".to_string(),
            from_state: "proposal".to_string(),
            to_state: "tension".to_string(),
            evidence_ids: vec![
                "ns://Perspective/1".to_string(),
                "ns://Perspective/2".to_string(),
            ],
            evidence_hash: "deadbeef".to_string(),
            semantic_check: semantic_check.map(str::to_string),
            consensus_rule: None,
        }
    }

    #[test]
    fn should_fire_only_on_pass() {
        assert!(should_fire_proposal(SemanticCheckVerdict::Pass));
        assert!(!should_fire_proposal(SemanticCheckVerdict::Fail));
        assert!(!should_fire_proposal(SemanticCheckVerdict::Ambiguous));
    }

    #[test]
    fn build_returns_none_when_no_hint() {
        let t = transition(None);
        assert!(build_semantic_check_prompt(&t, &ctx()).is_none());
    }

    #[test]
    fn build_includes_flow_hint_transition_and_evidence() {
        let t = transition(Some(
            "Confirm the positions actually conflict — not just two people speaking on the same side.",
        ));
        let prompt = build_semantic_check_prompt(&t, &ctx()).expect("hint present");
        assert!(prompt.contains("## Flow: Deliberation"));
        assert!(prompt.contains("A workflow for resolving group tensions."));
        assert!(prompt.contains("FROM: proposal"));
        assert!(prompt.contains("TO:   tension"));
        assert!(prompt.contains("Confirm the positions actually conflict"));
        assert!(prompt.contains("- ns://Perspective/1"));
        assert!(prompt.contains("- ns://Perspective/2"));
        assert!(prompt.contains("YES, NO, or UNCLEAR"));
    }

    #[test]
    fn build_handles_missing_flow_hint() {
        let t = transition(Some("Check it."));
        let mut c = ctx();
        c.flow_interpretation_hint = None;
        let prompt = build_semantic_check_prompt(&t, &c).expect("hint present");
        assert!(prompt.contains("## Flow: Deliberation"));
        assert!(!prompt.contains("A workflow for resolving group tensions."));
    }

    #[test]
    fn build_handles_empty_evidence() {
        let mut t = transition(Some("Check it."));
        t.evidence_ids.clear();
        let prompt = build_semantic_check_prompt(&t, &ctx()).expect("hint present");
        assert!(prompt.contains("(none)"));
        assert!(!prompt.contains("- ns://"));
    }

    #[test]
    fn parse_yes_variants() {
        for input in [
            "YES",
            "yes",
            "Yes.",
            "y",
            "Y",
            "yes, absolutely",
            "TRUE",
            "true",
            "confirmed",
            "PASS",
        ] {
            assert_eq!(
                parse_semantic_check_response(input),
                SemanticCheckVerdict::Pass,
                "input={input:?}"
            );
        }
    }

    #[test]
    fn parse_no_variants() {
        for input in [
            "NO",
            "no",
            "No.",
            "n",
            "N",
            "no — evidence contradicts",
            "FALSE",
            "reject",
            "rejected",
            "FAIL",
        ] {
            assert_eq!(
                parse_semantic_check_response(input),
                SemanticCheckVerdict::Fail,
                "input={input:?}"
            );
        }
    }

    #[test]
    fn parse_ambiguous_for_off_vocabulary() {
        for input in ["", "   ", "UNCLEAR", "maybe", "hmm", "42", "please clarify"] {
            assert_eq!(
                parse_semantic_check_response(input),
                SemanticCheckVerdict::Ambiguous,
                "input={input:?}"
            );
        }
    }

    #[test]
    fn parse_uses_first_non_empty_line() {
        assert_eq!(
            parse_semantic_check_response("\n\n  \nYES\n(details below)\nNO"),
            SemanticCheckVerdict::Pass
        );
        assert_eq!(
            parse_semantic_check_response("NO\nExplanation: the evidence...\nYES"),
            SemanticCheckVerdict::Fail
        );
    }

    #[test]
    fn parse_strips_plain_code_fence() {
        assert_eq!(
            parse_semantic_check_response("```\nYES\n```"),
            SemanticCheckVerdict::Pass
        );
    }

    #[test]
    fn parse_strips_language_tagged_code_fence() {
        assert_eq!(
            parse_semantic_check_response("```text\nNO\n```"),
            SemanticCheckVerdict::Fail
        );
    }

    #[test]
    fn parse_strips_leading_and_trailing_whitespace() {
        assert_eq!(
            parse_semantic_check_response("   \n  YES  \n  "),
            SemanticCheckVerdict::Pass
        );
    }

    #[test]
    fn parse_strips_punctuation_from_first_token() {
        assert_eq!(
            parse_semantic_check_response("YES!"),
            SemanticCheckVerdict::Pass
        );
        assert_eq!(
            parse_semantic_check_response("NO,"),
            SemanticCheckVerdict::Fail
        );
        assert_eq!(
            parse_semantic_check_response("(yes)"),
            SemanticCheckVerdict::Pass
        );
    }

    // ----------------------------------------------------------------------
    // async layer tests
    // ----------------------------------------------------------------------

    use std::sync::Mutex;

    /// Test-only stub. Returns a canned response and records every
    /// (model_id, prompt) pair it was called with so tests can assert
    /// both the auto-pass short-circuit ("was never called") and the
    /// hint-present path ("was called with the expected prompt").
    struct StubLlm {
        response: Result<String>,
        calls: Mutex<Vec<(String, String)>>,
    }

    impl StubLlm {
        fn new(response: &str) -> Self {
            Self {
                response: Ok(response.to_string()),
                calls: Mutex::new(Vec::new()),
            }
        }

        fn erroring() -> Self {
            Self {
                response: Err(anyhow::anyhow!("stub LLM failure")),
                calls: Mutex::new(Vec::new()),
            }
        }

        fn call_count(&self) -> usize {
            self.calls.lock().unwrap().len()
        }

        fn last_prompt(&self) -> Option<String> {
            self.calls.lock().unwrap().last().map(|(_, p)| p.clone())
        }
    }

    #[async_trait::async_trait]
    impl SemanticCheckLlm for StubLlm {
        async fn confirm(&self, model_id: &str, prompt: &str) -> Result<String> {
            self.calls
                .lock()
                .unwrap()
                .push((model_id.to_string(), prompt.to_string()));
            match &self.response {
                Ok(s) => Ok(s.clone()),
                Err(e) => Err(anyhow::anyhow!("{e}")),
            }
        }
    }

    #[tokio::test]
    async fn run_auto_passes_when_no_hint_and_never_calls_llm() {
        let llm = StubLlm::new("NO");
        let t = transition(None);
        let verdict = run_semantic_check(&llm, "any-model", &t, &ctx())
            .await
            .expect("no-hint path must not surface an error");
        assert_eq!(verdict, SemanticCheckVerdict::Pass);
        assert_eq!(
            llm.call_count(),
            0,
            "auto-pass short-circuit must not invoke the LLM"
        );
    }

    #[tokio::test]
    async fn run_returns_pass_on_yes_response() {
        let llm = StubLlm::new("YES");
        let t = transition(Some("Verify the two Perspectives actually oppose."));
        let verdict = run_semantic_check(&llm, "small-verifier", &t, &ctx())
            .await
            .expect("stub does not error");
        assert_eq!(verdict, SemanticCheckVerdict::Pass);
        assert_eq!(llm.call_count(), 1);
    }

    #[tokio::test]
    async fn run_returns_fail_on_no_response() {
        let llm = StubLlm::new("NO — the positions align.");
        let t = transition(Some("Verify the two Perspectives actually oppose."));
        let verdict = run_semantic_check(&llm, "small-verifier", &t, &ctx())
            .await
            .expect("stub does not error");
        assert_eq!(verdict, SemanticCheckVerdict::Fail);
    }

    #[tokio::test]
    async fn run_returns_ambiguous_on_off_vocab_response() {
        let llm = StubLlm::new("UNCLEAR — need more evidence.");
        let t = transition(Some("Verify the two Perspectives actually oppose."));
        let verdict = run_semantic_check(&llm, "small-verifier", &t, &ctx())
            .await
            .expect("stub does not error");
        assert_eq!(verdict, SemanticCheckVerdict::Ambiguous);
    }

    #[tokio::test]
    async fn run_passes_built_prompt_and_model_id_through_to_llm() {
        let llm = StubLlm::new("YES");
        let t = transition(Some(
            "Two participants must actually hold opposing positions on the same claim.",
        ));
        let _ = run_semantic_check(&llm, "gemma3:2b-check", &t, &ctx())
            .await
            .unwrap();
        // Scope the lock: last_prompt() re-locks the same mutex, so
        // hold `calls` only long enough to clone the one entry we need.
        let (model, prompt) = {
            let calls = llm.calls.lock().unwrap();
            assert_eq!(calls.len(), 1);
            calls[0].clone()
        };
        assert_eq!(model, "gemma3:2b-check");
        assert!(prompt.contains("## Flow: Deliberation"));
        assert!(prompt.contains("Two participants must actually hold opposing positions"));
        assert!(prompt.contains("FROM: proposal"));
        assert!(prompt.contains("TO:   tension"));
        // sanity: the last-prompt helper returns the same shape
        assert_eq!(llm.last_prompt().as_deref(), Some(prompt.as_str()));
    }

    #[tokio::test]
    async fn run_propagates_llm_error() {
        let llm = StubLlm::erroring();
        let t = transition(Some("Some hint."));
        let err = run_semantic_check(&llm, "any-model", &t, &ctx())
            .await
            .expect_err("LLM error must bubble up so the caller can log + discard");
        assert!(err.to_string().contains("stub LLM failure"));
        // the call still happened — the hint path did not short-circuit
        assert_eq!(llm.call_count(), 1);
    }

    #[tokio::test]
    async fn run_does_not_call_llm_when_hint_missing_even_if_llm_would_error() {
        // Guard against a refactor that accidentally moves the hint check
        // after the call: with no hint and an erroring stub, we must still
        // return Pass without touching the LLM at all.
        let llm = StubLlm::erroring();
        let t = transition(None);
        let verdict = run_semantic_check(&llm, "any-model", &t, &ctx())
            .await
            .expect("no-hint path must not touch the erroring LLM");
        assert_eq!(verdict, SemanticCheckVerdict::Pass);
        assert_eq!(llm.call_count(), 0);
    }
}
