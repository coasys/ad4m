//! Optional second LLM pass that confirms a `requires`-satisfied transition
//! semantically before the engine writes its proposal.
//!
//! A `FlowState` may carry a `semanticCheck` hint: a natural-language
//! condition that structural evidence alone cannot settle ("the scope was
//! agreed on, not merely discussed"). When the deterministic guard holds
//! and such a hint exists, the engine asks the LLM a yes/no question and
//! only a clear YES lets the proposal through. NO, an unclear answer and an
//! LLM error all discard the transition: an uncertain confirmation must
//! never advance a flow.

use crate::perspectives::flow_evaluator::SatisfiedTransition;
use anyhow::Result;

/// The one LLM call the gate needs, behind a trait so tests can script it.
#[async_trait::async_trait]
pub trait SemanticCheckLlm: Send + Sync {
    async fn confirm(&self, prompt: &str) -> Result<String>;
}

/// Prompt for one transition. `hint` is the target state's `semanticCheck`;
/// `flow_hint` the flow-level `interpretationHint`, when the definition has
/// one.
pub fn build_semantic_check_prompt(
    transition: &SatisfiedTransition,
    hint: &str,
    flow_hint: Option<&str>,
) -> String {
    let mut p = String::from(
        "You are checking whether a specific state transition in a group's shared workflow really applies.\n\n",
    );
    p.push_str(&format!("## Flow: {}\n", transition.flow_name));
    if let Some(flow_hint) = flow_hint {
        p.push_str(flow_hint);
        p.push('\n');
    }
    p.push_str(&format!(
        "\n## Transition\nFROM: {}\nTO:   {}\n\n",
        transition.from_state, transition.to_state
    ));
    p.push_str(&format!(
        "## Semantic check to verify\n{hint}\n\n## Evidence found in the graph\n"
    ));
    if transition.evidence_ids.is_empty() {
        p.push_str("(none)\n");
    }
    for id in &transition.evidence_ids {
        p.push_str(&format!("- {id}\n"));
    }
    p.push_str(
        "\n## Instructions\nAnswer with exactly one word on the first line: YES, NO, or UNCLEAR.\n\
         YES = the semantic check is satisfied by the current evidence.\n\
         NO = the evidence contradicts or does not support the check.\n\
         UNCLEAR = the check cannot be confidently evaluated.\n",
    );
    p
}

/// Only an unambiguous YES as the first word of the answer passes. Code
/// fences and punctuation around it are tolerated; anything else, including
/// UNCLEAR, fails.
pub fn semantic_check_passed(raw: &str) -> bool {
    let first_line = raw
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with("```"))
        .unwrap_or("");
    let token = first_line
        .split_whitespace()
        .next()
        .unwrap_or("")
        .trim_matches(|c: char| c.is_ascii_punctuation())
        .to_ascii_uppercase();
    matches!(token.as_str(), "YES" | "Y" | "TRUE" | "PASS")
}

/// Runs the check on the same AIService task as the extraction pass, so both
/// share one worker and billing scope.
pub struct AIServiceSemanticCheck {
    pub task_id: String,
}

#[async_trait::async_trait]
impl SemanticCheckLlm for AIServiceSemanticCheck {
    async fn confirm(&self, prompt: &str) -> Result<String> {
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

    fn transition() -> SatisfiedTransition {
        SatisfiedTransition {
            flow_name: "Delivery".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            from_state: "identified".into(),
            to_state: "scoped".into(),
            evidence_ids: vec!["ad4m://task/1".into(), "ad4m://task/2".into()],
            evidence_hash: "abc".into(),
            semantic_check: Some("The scope was agreed on.".into()),
        }
    }

    #[test]
    fn prompt_carries_flow_transition_hint_and_evidence() {
        let p = build_semantic_check_prompt(
            &transition(),
            "The scope was agreed on.",
            Some("A unit of work moving to done."),
        );
        for needle in [
            "## Flow: Delivery",
            "A unit of work moving to done.",
            "FROM: identified",
            "TO:   scoped",
            "The scope was agreed on.",
            "- ad4m://task/1",
            "- ad4m://task/2",
            "YES, NO, or UNCLEAR",
        ] {
            assert!(p.contains(needle), "prompt must contain {needle:?}:\n{p}");
        }
        let mut t = transition();
        t.evidence_ids.clear();
        assert!(build_semantic_check_prompt(&t, "x", None).contains("(none)"));
    }

    #[test]
    fn only_a_leading_yes_passes() {
        for yes in [
            "YES",
            "yes.",
            "Yes, the scope is clear.",
            "```\nYES\n```",
            "\n\n  YES",
        ] {
            assert!(semantic_check_passed(yes), "{yes:?} must pass");
        }
        for no in [
            "NO",
            "no",
            "UNCLEAR",
            "",
            "Maybe yes",
            "The answer is YES",
            "```json\n```",
        ] {
            assert!(!semantic_check_passed(no), "{no:?} must not pass");
        }
    }
}
