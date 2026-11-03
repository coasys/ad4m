//! Agent sessions: conversations the executor owns, for a client that has no conversation of its
//! own to point an agent reply at.
//!
//! A session is an `AgentSession` record in the perspective that keeps it. It is a `Container`
//! like any node its messages hang off (`ad4m://has_child`), so `perspective.agentReply` answers
//! in it as in any other conversation. What a session adds is a home a client can create, list
//! (a model query on `AgentSession`, by `about`) and delete, and settings an agent reply reads so
//! the client doesn't send them every time.
//!
//! **The settings stay on this node.** Where the agent works (`workIn`, a perspective uuid that
//! means nothing to another node), the tools it may write with and the model are `Local` links on
//! the session (`"local": true` in `hardwired_sdna/agent_session.json`): they are never shared.
//! `title` and `about` are the session's content, shared with the perspective.
//!
//! **Delete** removes the session, its replies and their steps. The person's messages are the
//! app's records, in the app's classes: the app deletes them.

use crate::agent::AgentContext;
use crate::perspectives::agent_reply::{cancel_reply, AGENT_REPLY_CLASS};
use crate::perspectives::agent_reply_steps::AGENT_STEP_PREDICATE;
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::LinkQuery;
use serde_json::{json, Value};

pub const AGENT_SESSION_CLASS: &str = "AgentSession";
pub const AGENT_SESSION_TARGET_CLASS: &str = "ad4m://AgentSession";
const AGENT_SESSION_SDNA: &str = include_str!("hardwired_sdna/agent_session.json");
const SESSION_URI_PREFIX: &str = "ad4m://agent_session/";

/// What a session's replies run with unless a call says otherwise.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SessionSettings {
    pub work_in: Option<String>,
    pub write_tools: Vec<String>,
    pub model: Option<String>,
}

/// A new session's content and settings.
#[derive(Debug, Clone, Default)]
pub struct NewSession {
    pub title: String,
    pub about: Option<String>,
    pub settings: SessionSettings,
}

/// Register the class in the perspective, once.
pub async fn ensure_agent_session_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        AGENT_SESSION_CLASS,
        AGENT_SESSION_TARGET_CLASS,
        AGENT_SESSION_SDNA,
        None,
        context,
    )
    .await
}

/// Create a session and return its id.
pub async fn create_session(
    mut perspective: PerspectiveInstance,
    session: NewSession,
    context: &AgentContext,
) -> anyhow::Result<String> {
    ensure_agent_session_class(&mut perspective, context).await?;
    let id = format!("{SESSION_URI_PREFIX}{}", uuid::Uuid::new_v4());
    let mut values = json!({ "title": session.title });
    if let Some(about) = session.about.filter(|a| !a.trim().is_empty()) {
        values["about"] = about.into();
    }
    if let Some(work_in) = session.settings.work_in {
        values["workIn"] = work_in.into();
    }
    if !session.settings.write_tools.is_empty() {
        values["writeTools"] = serde_json::to_string(&session.settings.write_tools)?.into();
    }
    if let Some(model) = session.settings.model {
        values["model"] = model.into();
    }
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(AGENT_SESSION_CLASS.to_string()),
                query: None,
            },
            id.clone(),
            Some(values),
            None,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not create the session: {e:#}"))?;
    Ok(id)
}

/// The settings of the session at `id`, or `None` when `id` is no session.
pub async fn session_settings(
    perspective: &PerspectiveInstance,
    id: &str,
) -> anyhow::Result<Option<SessionSettings>> {
    if !crate::perspectives::hardwired_class::subject_class_registered(
        perspective,
        AGENT_SESSION_TARGET_CLASS,
    )
    .await?
    {
        return Ok(None);
    }
    let raw = perspective
        .model_query(
            AGENT_SESSION_CLASS,
            &json!({ "where": { "id": id } }).to_string(),
        )
        .await
        .map_err(|e| anyhow::anyhow!("could not read the session: {e:#}"))?;
    let parsed: Value = serde_json::from_str(&raw)?;
    let Some(row) = parsed["instances"].as_array().and_then(|rows| rows.first()) else {
        return Ok(None);
    };
    let text = |name: &str| {
        row.get(name)
            .and_then(Value::as_str)
            .filter(|s| !s.is_empty())
            .map(str::to_string)
    };
    Ok(Some(SessionSettings {
        work_in: text("workIn"),
        write_tools: text("writeTools")
            .map(|tools| serde_json::from_str(&tools))
            .transpose()?
            .unwrap_or_default(),
        model: text("model"),
    }))
}

/// Delete the session at `id`: stop its running reply, then remove the session, its replies
/// and their steps, in one batch. `false` when `id` is no session.
pub async fn delete_session(
    mut perspective: PerspectiveInstance,
    id: &str,
    context: &AgentContext,
) -> anyhow::Result<bool> {
    if session_settings(&perspective, id).await?.is_none() {
        return Ok(false);
    }
    cancel_reply(perspective.clone(), id, context).await?;

    let replies: Vec<String> = {
        let raw = perspective
            .model_query(
                AGENT_REPLY_CLASS,
                &json!({ "parent": { "id": id } }).to_string(),
            )
            .await
            .map_err(|e| anyhow::anyhow!("could not read the session's replies: {e:#}"))?;
        let parsed: Value = serde_json::from_str(&raw)?;
        parsed["instances"]
            .as_array()
            .map(|rows| {
                rows.iter()
                    .filter_map(|r| r["id"].as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };
    let mut nodes = vec![id.to_string()];
    for reply in &replies {
        nodes.push(reply.clone());
        let steps = perspective
            .get_links(&LinkQuery {
                source: Some(reply.clone()),
                predicate: Some(AGENT_STEP_PREDICATE.to_string()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        nodes.extend(steps.into_iter().map(|l| l.data.target));
    }

    let mut links = Vec::new();
    for node in &nodes {
        let from = perspective
            .get_links(&LinkQuery {
                source: Some(node.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        links.extend(from.into_iter().map(|l| l.into()));
    }
    let batch = perspective.create_batch().await;
    perspective
        .remove_links(links, Some(batch.clone()))
        .await
        .map_err(|e| anyhow::anyhow!("{e:#}"))?;
    perspective
        .commit_batch(batch, context)
        .await
        .map_err(|e| anyhow::anyhow!("could not delete the session: {e:#}"))?;
    Ok(true)
}
