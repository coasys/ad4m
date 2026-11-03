//! An agent reply's steps: one `AgentStep` record per tool call the agent makes, written as it
//! makes it, so the person can see what the agent is doing while it works.
//!
//! A step is created `running` when the model asks for a tool and gets its outcome (`ok`, or
//! `error` with the tool's message) when the call returns. It hangs off its reply by
//! `ad4m://agent_step`, the reply's `steps`, and not by `has_child`: a child of the reply would
//! make the reply a `Container`, and a chat that reads containers would show the steps as replies
//! to it.
//!
//! **What a step never holds** is the call's arguments or its result. An argument can be a whole
//! record (`template_update` sends a template), and a result can be anything in the perspective
//! the agent works in, which may be more private than the one keeping the conversation. A step
//! names the record it acted on by id ([`target_of`]) and keeps a failure's message, capped.
//!
//! A step left `running` under a reply that is no longer `running` was cut off: by a cancel, a
//! timeout or a restart.

use crate::agent::AgentContext;
use crate::ai_service::harness::ToolObserver;
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkStatus};
use serde_json::{json, Value};

pub const AGENT_STEP_CLASS: &str = "AgentStep";
pub const AGENT_STEP_TARGET_CLASS: &str = "ad4m://AgentStep";
/// From a reply to each of its steps: the reply's `steps`.
pub const AGENT_STEP_PREDICATE: &str = "ad4m://agent_step";
const AGENT_STEP_SDNA: &str = include_str!("hardwired_sdna/agent_step.json");
const STEP_URI_PREFIX: &str = "ad4m://agent_step/";

/// How much of a failure's message a step keeps.
const ERROR_LIMIT: usize = 300;
/// The longest value a step takes as a record id.
const ID_LIMIT: usize = 512;

/// Register the class in the perspective, once.
pub async fn ensure_agent_step_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        AGENT_STEP_CLASS,
        AGENT_STEP_TARGET_CLASS,
        AGENT_STEP_SDNA,
        None,
        context,
    )
    .await
}

/// Writes a reply's steps as the loop reports its tool calls.
pub struct StepWriter {
    perspective: PerspectiveInstance,
    reply: String,
    context: AgentContext,
    /// The step of the call in flight. The loop makes one call at a time.
    current: tokio::sync::Mutex<Option<String>>,
}

impl StepWriter {
    pub fn new(perspective: PerspectiveInstance, reply: String, context: AgentContext) -> Self {
        Self {
            perspective,
            reply,
            context,
            current: tokio::sync::Mutex::new(None),
        }
    }

    async fn create(&self, tool: &str, target: Option<String>) -> anyhow::Result<String> {
        let step = format!("{STEP_URI_PREFIX}{}", uuid::Uuid::new_v4());
        let mut values = json!({ "tool": tool, "outcome": "running" });
        if let Some(target) = target {
            values["target"] = target.into();
        }
        let mut perspective = self.perspective.clone();
        let batch = perspective.create_batch().await;
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(AGENT_STEP_CLASS.to_string()),
                    query: None,
                },
                step.clone(),
                Some(values),
                Some(batch.clone()),
                &self.context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        perspective
            .add_link(
                Link {
                    source: self.reply.clone(),
                    predicate: Some(AGENT_STEP_PREDICATE.to_string()),
                    target: step.clone(),
                },
                LinkStatus::Shared,
                Some(batch.clone()),
                &self.context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        perspective
            .commit_batch(batch, &self.context)
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        Ok(step)
    }

    async fn settle(&self, step: &str, values: Value) -> anyhow::Result<()> {
        let mut perspective = self.perspective.clone();
        perspective
            .update_subject(
                SubjectClassOption {
                    class_name: Some(AGENT_STEP_CLASS.to_string()),
                    query: None,
                },
                step.to_string(),
                values,
                None,
                &self.context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))
    }
}

/// A step that can't be written is logged, never a failure of the run: the agent's work
/// matters more than the account of it.
#[async_trait::async_trait]
impl ToolObserver for StepWriter {
    async fn tool_called(&self, name: &str, arguments: &Value) {
        let created = self.create(name, target_of(arguments, None)).await;
        let mut current = self.current.lock().await;
        *current = match created {
            Ok(step) => Some(step),
            Err(e) => {
                log::warn!(
                    "agent reply {}: could not write a step for {name}: {e:#}",
                    self.reply
                );
                None
            }
        };
    }

    async fn tool_returned(&self, name: &str, arguments: &Value, result: &str) {
        let Some(step) = self.current.lock().await.take() else {
            return;
        };
        let (outcome, error) = outcome_of(result);
        let mut values = json!({ "outcome": outcome });
        if let Some(error) = error {
            values["error"] = error.into();
        }
        // A create names its record only in what it answers.
        if target_of(arguments, None).is_none() {
            if let Some(target) = target_of(&Value::Null, Some(result)) {
                values["target"] = target.into();
            }
        }
        if let Err(e) = self.settle(&step, values).await {
            log::warn!(
                "agent reply {}: could not finish the step for {name}: {e:#}",
                self.reply
            );
        }
    }
}

/// The record a call acted on, by id: the `base_uri` or `parent` it names, else the `base_uri`
/// its answer names (a create's). Only a value that reads as an id is taken, so no free text
/// from the arguments or the answer reaches the step.
pub(crate) fn target_of(arguments: &Value, result: Option<&str>) -> Option<String> {
    let from = |value: &Value| {
        ["base_uri", "parent"]
            .iter()
            .filter_map(|key| value.get(key).and_then(Value::as_str))
            .find(|id| looks_like_id(id))
            .map(str::to_string)
    };
    from(arguments).or_else(|| {
        let answer: Value = serde_json::from_str(result?).ok()?;
        answer
            .get("base_uri")
            .and_then(Value::as_str)
            .filter(|id| looks_like_id(id))
            .map(str::to_string)
    })
}

/// How a call went, from what it answered: `error` with the message when the call failed (the
/// loop's `error: …`) or the tool answered `{ "error": … }`; `ok` otherwise.
pub(crate) fn outcome_of(result: &str) -> (&'static str, Option<String>) {
    let message = result
        .strip_prefix("error:")
        .map(|m| m.trim().to_string())
        .or_else(|| {
            let answer: Value = serde_json::from_str(result).ok()?;
            match answer.get("error")? {
                Value::String(s) => Some(s.clone()),
                Value::Null => None,
                other => Some(other.to_string()),
            }
        });
    match message {
        Some(message) => ("error", Some(capped(&message))),
        None => ("ok", None),
    }
}

fn looks_like_id(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= ID_LIMIT
        && value.contains(':')
        && !value.chars().any(char::is_whitespace)
}

fn capped(message: &str) -> String {
    if message.chars().count() <= ERROR_LIMIT {
        return message.to_string();
    }
    let mut cut: String = message.chars().take(ERROR_LIMIT).collect();
    cut.push('…');
    cut
}
