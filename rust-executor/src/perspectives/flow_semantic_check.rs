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
    if transition.evidence.is_empty() {
        p.push_str("(none)\n");
    }
    for item in transition.evidence.iter().take(MAX_EVIDENCE_ITEMS) {
        p.push_str(&format!("### {} ({})\n", item.id, item.class_name));
        p.push_str(&truncate_chars(&item.content, MAX_EVIDENCE_CHARS));
        p.push('\n');
    }
    if transition.evidence.len() > MAX_EVIDENCE_ITEMS {
        p.push_str(&format!(
            "…and {} more matching instance(s) not shown.\n",
            transition.evidence.len() - MAX_EVIDENCE_ITEMS
        ));
    }
    p.push_str(
        "\n## Instructions\nAnswer with exactly one word on the first line: YES, NO, or UNCLEAR.\n\
         YES = the semantic check is satisfied by the current evidence.\n\
         NO = the evidence contradicts or does not support the check.\n\
         UNCLEAR = the check cannot be confidently evaluated.\n",
    );
    p
}

/// Prompt-size guards: at most this many evidence instances are rendered…
const MAX_EVIDENCE_ITEMS: usize = 25;
/// …at most this many characters each.
const MAX_EVIDENCE_CHARS: usize = 1500;

/// Truncate on a char boundary, marking the cut.
fn truncate_chars(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        return s.to_string();
    }
    let mut out: String = s.chars().take(max).collect();
    out.push_str("…[truncated]");
    out
}

/// Only the exact word `YES` as the first word of the answer passes — the
/// same vocabulary the prompt instructs (`YES`/`NO`/`UNCLEAR`), so the
/// parser accepts nothing the prompt didn't ask for. Code fences and
/// punctuation around it are tolerated; anything else, including UNCLEAR,
/// fails.
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
    token == "YES"
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

    use crate::perspectives::flow_evaluator::EvidenceItem;

    fn transition() -> SatisfiedTransition {
        SatisfiedTransition {
            flow_name: "Delivery".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            from_state: "identified".into(),
            to_state: "scoped".into(),
            evidence_ids: vec!["ad4m://task/1".into(), "ad4m://task/2".into()],
            evidence: vec![
                EvidenceItem {
                    id: "ad4m://task/1".into(),
                    class_name: "ns://Task".into(),
                    content: r#"{"id":"ad4m://task/1","title":"Ship parser","body":"We agreed on the scope in Tuesday's standup."}"#.into(),
                },
                EvidenceItem {
                    id: "ad4m://task/2".into(),
                    class_name: "ns://Task".into(),
                    content: r#"{"id":"ad4m://task/2","title":"Write tests"}"#.into(),
                },
            ],
            evidence_hash: "abc".into(),
            semantic_check: Some("The scope was agreed on.".into()),
        }
    }

    /// The central claim of the semantic check: the LLM sees the evidence's
    /// CONTENT, not a bare URI list — "agreed on" is only decidable from
    /// property values like the body text below.
    #[test]
    fn prompt_carries_flow_transition_hint_and_evidence_content() {
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
            "### ad4m://task/1 (ns://Task)",
            "We agreed on the scope in Tuesday's standup.",
            "### ad4m://task/2 (ns://Task)",
            "Write tests",
            "YES, NO, or UNCLEAR",
        ] {
            assert!(p.contains(needle), "prompt must contain {needle:?}:\n{p}");
        }
        let mut t = transition();
        t.evidence.clear();
        assert!(build_semantic_check_prompt(&t, "x", None).contains("(none)"));
    }

    #[test]
    fn oversized_evidence_is_truncated_not_dropped() {
        let mut t = transition();
        t.evidence[0].content =
            format!(r#"{{"id":"ad4m://task/1","body":"{}"}}"#, "x".repeat(5000));
        let p = build_semantic_check_prompt(&t, "hint", None);
        assert!(p.contains("…[truncated]"));
        // The second, small item survives untouched after a truncated first.
        assert!(p.contains("Write tests"));
        assert!(p.len() < 4000);
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
        // Only the prompt's own vocabulary passes: Y/TRUE/PASS are answers
        // the prompt never asked for and must not be treated as YES.
        for no in [
            "NO",
            "no",
            "UNCLEAR",
            "",
            "Y",
            "TRUE",
            "PASS",
            "Maybe yes",
            "The answer is YES",
            "```json\n```",
        ] {
            assert!(!semantic_check_passed(no), "{no:?} must not pass");
        }
    }
}
