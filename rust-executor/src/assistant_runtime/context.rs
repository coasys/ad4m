//! Context assembly — turn an assistant's config + a thread's history into the
//! `(role, content)` message list the executor's `prompt_messages` consumes.
//!
//! The executor's `build_ephemeral_task` folds all `system` messages into the
//! system prompt, turns prior `user`/`assistant` pairs into few-shot examples,
//! and uses the **last** `user` message as the live prompt — it silently drops
//! any `tool`/`function` role. So this assembler:
//!
//! * emits the persona (assistant system prompt + personality bodies + skills)
//!   and the Hermes/Qwen `<tools>` block as `system` messages,
//! * replays thread history as `user`/`assistant` turns, folding any `tool`
//!   history into the preceding assistant text (nothing dropped),
//! * places the live user turn (which the run loop extends with a tool
//!   transcript between iterations) as the final `user` message.
//!
//! Pure and unit-testable — no perspective or model required.

use crate::api::openai_compat::tool_grammar::render_tools_system_prompt;
use crate::api::openai_compat::types::ToolDef;

use super::entities::{Assistant, Message, Personality, Skill};

/// Assemble the message list for one model call.
///
/// `history` is the thread's prior messages (ordered by `ts`), NOT including
/// the current user turn. `user_turn` is the live prompt text.
pub fn assemble_messages(
    assistant: &Assistant,
    personalities: &[Personality],
    skills: &[Skill],
    tools: &[ToolDef],
    history: &[Message],
    user_turn: &str,
) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = Vec::new();

    if !assistant.system_prompt.trim().is_empty() {
        out.push(("system".to_string(), assistant.system_prompt.clone()));
    }
    for p in personalities {
        if !p.body.trim().is_empty() {
            out.push(("system".to_string(), p.body.clone()));
        }
    }
    if !skills.is_empty() {
        out.push(("system".to_string(), render_skills(skills)));
    }
    if !tools.is_empty() {
        out.push(("system".to_string(), render_tools_system_prompt(tools)));
    }

    for m in history {
        match m.role.as_str() {
            "user" => out.push(("user".to_string(), m.content.clone())),
            "assistant" => out.push(("assistant".to_string(), m.content.clone())),
            "tool" => {
                // Fold tool output into the preceding assistant turn so the
                // executor doesn't drop it; otherwise surface it as user
                // context.
                let folded = format!("<tool_response>{}</tool_response>", m.content);
                match out.last_mut() {
                    Some(last) if last.0 == "assistant" => {
                        last.1.push('\n');
                        last.1.push_str(&folded);
                    }
                    _ => out.push(("user".to_string(), folded)),
                }
            }
            _ => {}
        }
    }

    out.push(("user".to_string(), user_turn.to_string()));
    out
}

/// Render selected skills as a single `system` block.
fn render_skills(skills: &[Skill]) -> String {
    let mut s = String::from("# Skills\n\nYou have the following skills available:\n");
    for sk in skills {
        s.push_str(&format!("\n## {}\n", sk.name));
        if !sk.description.trim().is_empty() {
            s.push_str(sk.description.trim());
            s.push('\n');
        }
        if !sk.body.trim().is_empty() {
            s.push_str(sk.body.trim());
            s.push('\n');
        }
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::openai_compat::types::{FunctionDef, ToolDef};
    use serde_json::json;

    fn assistant() -> Assistant {
        Assistant {
            id: "we://assistant/1".into(),
            name: "Ada".into(),
            model_id: "llama".into(),
            system_prompt: "You are Ada.".into(),
            ..Default::default()
        }
    }

    fn msg(role: &str, content: &str, ts: &str) -> Message {
        Message {
            id: format!("m-{ts}"),
            thread_id: "t".into(),
            role: role.into(),
            content: content.into(),
            tool_calls: String::new(),
            ts: ts.into(),
            status: "complete".into(),
        }
    }

    fn tool(name: &str) -> ToolDef {
        ToolDef {
            kind: "function".into(),
            function: FunctionDef {
                name: name.into(),
                description: Some("desc".into()),
                parameters: Some(json!({"type":"object","properties":{}})),
            },
        }
    }

    #[test]
    fn persona_and_tools_lead_as_system_messages() {
        let personalities = vec![Personality {
            id: "p".into(),
            name: "Witty".into(),
            body: "Be witty.".into(),
        }];
        let skills = vec![Skill {
            id: "s".into(),
            name: "Search".into(),
            description: "find things".into(),
            body: "use the index".into(),
        }];
        let tools = vec![tool("get_weather")];

        let out = assemble_messages(&assistant(), &personalities, &skills, &tools, &[], "hello");

        // system: assistant prompt, personality body, skills block, tools block
        assert_eq!(out[0], ("system".to_string(), "You are Ada.".to_string()));
        assert_eq!(out[1], ("system".to_string(), "Be witty.".to_string()));
        assert_eq!(out[2].0, "system");
        assert!(out[2].1.contains("# Skills"));
        assert!(out[2].1.contains("Search"));
        assert_eq!(out[3].0, "system");
        assert!(out[3].1.contains("<tools>"));
        assert!(out[3].1.contains("get_weather"));

        // final message is the live user turn
        assert_eq!(out.last().unwrap(), &("user".to_string(), "hello".to_string()));
    }

    #[test]
    fn history_replayed_as_user_assistant_turns() {
        let history = vec![
            msg("user", "hi", "1"),
            msg("assistant", "hello!", "2"),
        ];
        let out = assemble_messages(&assistant(), &[], &[], &[], &history, "next question");
        // system(1) + user + assistant + final user
        assert_eq!(out[0].0, "system");
        assert_eq!(out[1], ("user".to_string(), "hi".to_string()));
        assert_eq!(out[2], ("assistant".to_string(), "hello!".to_string()));
        assert_eq!(out[3], ("user".to_string(), "next question".to_string()));
    }

    #[test]
    fn tool_history_folds_into_assistant_turn() {
        let history = vec![
            msg("user", "weather?", "1"),
            msg("assistant", "let me check", "2"),
            msg("tool", "sunny, 24C", "3"),
        ];
        let out = assemble_messages(&assistant(), &[], &[], &[], &history, "thanks");
        // The tool message is folded into the assistant turn, not dropped.
        let assistant_turn = out.iter().find(|(r, _)| r == "assistant").unwrap();
        assert!(assistant_turn.1.contains("let me check"));
        assert!(assistant_turn.1.contains("<tool_response>sunny, 24C</tool_response>"));
        // No standalone tool role leaks through.
        assert!(!out.iter().any(|(r, _)| r == "tool"));
    }

    #[test]
    fn empty_system_prompt_is_omitted() {
        let mut a = assistant();
        a.system_prompt = "   ".into();
        let out = assemble_messages(&a, &[], &[], &[], &[], "hi");
        // Only the final user turn — no empty system message.
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].0, "user");
    }
}
