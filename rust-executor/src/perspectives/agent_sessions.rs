//! Agent sessions: conversations the executor owns, for a client that has no conversation of its
//! own to point an agent reply at.
//!
//! A session is an `AgentSession` record in the perspective that keeps it. It is a `Container`
//! like any node its messages hang off (`ad4m://has_child`), so `perspective.agentReply` answers
//! in it as in any other conversation. What a session adds is a home a client can create, list
//! (a model query on `AgentSession`, by `about`) and delete, and settings an agent reply reads so
//! the client doesn't send them every time. The session's write tools go with its `workIn`: a
//! reply that names another perspective gets only the write tools it names
//! ([`SessionSettings::for_reply`]).
//!
//! **The settings stay on this node.** Where the agent works (`workIn`, a perspective uuid that
//! means nothing to another node), the person's library (`library`, likewise a uuid: where the
//! template tools look for a template too and `template_create` may make one), the tools it may
//! write with and the model are `Local` links on
//! the session (`"local": true` in `hardwired_sdna/agent_session.json`): they are never shared.
//! `title` and `about` are the session's content, shared with the perspective.
//!
//! **Delete** is the session's author's: another caller's delete answers `false`, as for an id
//! that is no session. It removes what the caller wrote: the session, the caller's replies in it
//! and their steps. Its `has_child` links go with it, so the person's messages leave the
//! conversation, but the messages themselves are the app's records, in the app's classes: the
//! app deletes them. Another member's replies and links in the session are theirs and stay, and
//! so does their running reply. The caller's slot in the session is held for the delete
//! ([`hold_for_delete`]), so no reply of theirs starts in it meanwhile.

use crate::agent::AgentContext;
use crate::perspectives::agent_reply::{hold_for_delete, AGENT_REPLY_CLASS};
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
    pub library: Option<String>,
    pub write_tools: Vec<String>,
    pub model: Option<String>,
}

impl SessionSettings {
    /// What a reply in the session runs with when the call names `work_in`, `library`,
    /// `write_tools` and `model`: what the call names, and the session's for what it leaves out.
    /// The session's write tools go with its perspectives (`workIn` and the library, both of
    /// which they write in): a call that names its own `workIn` or library gets only the write
    /// tools it names, never ones chosen for the session's perspectives. The model is no
    /// perspective's, so it falls back on its own.
    pub fn for_reply(
        self,
        work_in: Option<String>,
        library: Option<String>,
        write_tools: Option<Vec<String>>,
        model: Option<String>,
    ) -> SessionSettings {
        let saved_tools = if work_in.is_none() && library.is_none() {
            self.write_tools
        } else {
            Vec::new()
        };
        SessionSettings {
            work_in: work_in.or(self.work_in),
            library: library.or(self.library),
            write_tools: write_tools.unwrap_or(saved_tools),
            model: model.or(self.model),
        }
    }
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
    if let Some(library) = session.settings.library {
        values["library"] = library.into();
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

/// The session at `id` as `model_query` reads it, or `None` when `id` is no session.
async fn session_row(perspective: &PerspectiveInstance, id: &str) -> anyhow::Result<Option<Value>> {
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
    let mut parsed: Value = serde_json::from_str(&raw)?;
    Ok(parsed["instances"]
        .as_array_mut()
        .and_then(|rows| (!rows.is_empty()).then(|| rows.swap_remove(0))))
}

/// The settings of the session at `id`, or `None` when `id` is no session.
pub async fn session_settings(
    perspective: &PerspectiveInstance,
    id: &str,
) -> anyhow::Result<Option<SessionSettings>> {
    let Some(row) = session_row(perspective, id).await? else {
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
        library: text("library"),
        write_tools: text("writeTools")
            .map(|tools| serde_json::from_str(&tools))
            .transpose()?
            .unwrap_or_default(),
        model: text("model"),
    }))
}

/// Delete the session at `id`, as `caller_did` acting through `context`: stop the caller's reply
/// running in it, then remove what the caller wrote there (the session, the caller's replies in it
/// and their steps) in one batch. `false` when `id` is no session or the caller did not create it.
pub async fn delete_session(
    mut perspective: PerspectiveInstance,
    id: &str,
    caller_did: &str,
    context: &AgentContext,
) -> anyhow::Result<bool> {
    let Some(session) = session_row(&perspective, id).await? else {
        return Ok(false);
    };
    if session["author"].as_str() != Some(caller_did) {
        return Ok(false);
    }
    let hold = hold_for_delete(&perspective, (id.to_string(), caller_did.to_string()));
    if let Err(e) = remove_what_the_caller_wrote(&mut perspective, id, caller_did, context).await {
        hold.delete_failed(context).await;
        return Err(e);
    }
    Ok(true)
}

/// The session, the caller's replies in it and their steps: the caller's own links from each, in
/// one batch.
async fn remove_what_the_caller_wrote(
    perspective: &mut PerspectiveInstance,
    id: &str,
    caller_did: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
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
                    .filter(|r| r["author"].as_str() == Some(caller_did))
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
        nodes.extend(
            steps
                .into_iter()
                .filter(|l| l.author == caller_did)
                .map(|l| l.data.target),
        );
    }

    // Only the caller's own links: another member's are theirs to remove.
    let mut links = Vec::new();
    for node in &nodes {
        let from = perspective
            .get_links(&LinkQuery {
                source: Some(node.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("{e:#}"))?;
        links.extend(
            from.into_iter()
                .filter(|l| l.author == caller_did)
                .map(|l| l.into()),
        );
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
    Ok(())
}
