//! Perspective read/write helpers — the bridge between the pure WE entity
//! model in [`super::entities`] and the executor's in-process
//! [`PerspectiveInstance`] link store.
//!
//! Writes go through `PerspectiveInstance::add_link` / `remove_link` as the
//! main agent (no billing, no capability checks) so the subsystem can persist
//! replies regardless of which user owns the perspective. Reads use
//! `get_links` and decode `we://` property links back into entity structs.

use std::collections::HashMap;

use anyhow::Result;

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{all_perspectives, get_perspective};
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};

use super::entities::{
    decode_literal, encode_literal, Message, RunState, CLASS_MESSAGE, CLASS_RUN_STATE, FLAG,
    P_THREAD_ID, REL_MESSAGE,
};

/// Load the property map (predicate → decoded value) for a base URI by reading
/// all of its outgoing links.
pub async fn load_props(p: &PerspectiveInstance, base: &str) -> HashMap<String, String> {
    let links = p
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await
        .unwrap_or_default();
    let mut map = HashMap::new();
    for l in links {
        if let Some(pred) = l.data.predicate {
            map.insert(pred, decode_literal(&l.data.target));
        }
    }
    map
}

/// Return the base URIs of every instance of `class` in the perspective (the
/// `source` of each `--we://flag--> we://<class>` link).
pub async fn find_instances(p: &PerspectiveInstance, class: &str) -> Vec<String> {
    p.get_links(&LinkQuery {
        predicate: Some(FLAG.to_string()),
        target: Some(class.to_string()),
        ..Default::default()
    })
    .await
    .unwrap_or_default()
    .into_iter()
    .map(|l| l.data.source)
    .collect()
}

/// Find the perspective that holds `base` as an instance of `class`, scanning
/// all perspectives. Used to locate an `Assistant` (and its config) without a
/// separate we-root pointer.
pub async fn find_perspective_with_instance(base: &str, class: &str) -> Option<PerspectiveInstance> {
    for p in all_perspectives() {
        let hit = p
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some(FLAG.to_string()),
                target: Some(class.to_string()),
                ..Default::default()
            })
            .await
            .unwrap_or_default();
        if !hit.is_empty() {
            return Some(p);
        }
    }
    None
}

/// Load a single `Message` instance by base URI.
pub async fn load_message(p: &PerspectiveInstance, base: &str) -> Message {
    Message::from_props(base.to_string(), &load_props(p, base).await)
}

/// Load every `Message` in a thread, ordered by `ts`. Uses the canonical
/// `we://thread_id` join, tolerating either literal-encoded (`literal:string:…`)
/// or raw-URI storage of the id by matching on the decoded value.
pub async fn thread_messages(conv: &PerspectiveInstance, thread_id: &str) -> Vec<Message> {
    let links = conv
        .get_links(&LinkQuery {
            predicate: Some(P_THREAD_ID.to_string()),
            ..Default::default()
        })
        .await
        .unwrap_or_default();

    let mut msgs = Vec::with_capacity(links.len());
    for l in links {
        if decode_literal(&l.data.target) == thread_id {
            msgs.push(load_message(conv, &l.data.source).await);
        }
    }
    msgs.sort_by(|a, b| a.ts.cmp(&b.ts));
    msgs
}

/// Set a single-valued property link: remove any existing `(base, predicate)`
/// links, then add the new literal-encoded value. Mirrors the `setSingleTarget`
/// SDNA action WE uses for scalar properties.
pub async fn set_single_target(
    p: &mut PerspectiveInstance,
    base: &str,
    predicate: &str,
    value: &str,
    ctx: &AgentContext,
) -> Result<()> {
    let existing = p
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .unwrap_or_default();
    for d in existing {
        let le = LinkExpression::from(d);
        // Best-effort: a concurrent writer may have already removed it.
        let _ = p.remove_link(le, None).await;
    }
    p.add_link(
        Link {
            source: base.to_string(),
            predicate: Some(predicate.to_string()),
            target: encode_literal(value),
        },
        LinkStatus::Shared,
        None,
        ctx,
    )
    .await?;
    Ok(())
}

/// Persist a fresh `Message` instance: its flag + property links plus the
/// Thread→Message collection link so both the canonical (`we://thread_id`) and
/// relational (`Thread.messages`) query paths resolve it.
pub async fn write_message(
    conv: &mut PerspectiveInstance,
    msg: &Message,
    ctx: &AgentContext,
) -> Result<()> {
    for link in msg.to_links() {
        conv.add_link(link, LinkStatus::Shared, None, ctx).await?;
    }
    if !msg.thread_id.is_empty() {
        conv.add_link(
            Link {
                source: msg.thread_id.clone(),
                predicate: Some(REL_MESSAGE.to_string()),
                target: msg.id.clone(),
            },
            LinkStatus::Shared,
            None,
            ctx,
        )
        .await?;
    }
    Ok(())
}

/// Create or update the `RunState` for a thread. Reuses the existing instance
/// (found by matching `thread_id`) so a run keeps one durable record. Best
/// effort: failures are surfaced to the caller which logs and continues.
pub async fn upsert_run_state(
    conv: &mut PerspectiveInstance,
    thread_id: &str,
    status: &str,
    cursor: &str,
    pending_tool_call: &str,
) -> Result<()> {
    let ctx = AgentContext::main_agent();

    // Look for an existing RunState for this thread.
    let mut existing_base: Option<String> = None;
    for base in find_instances(conv, CLASS_RUN_STATE).await {
        let rs = RunState::from_props(base.clone(), &load_props(conv, &base).await);
        if rs.thread_id == thread_id {
            existing_base = Some(base);
            break;
        }
    }

    if let Some(base) = existing_base {
        set_single_target(conv, &base, super::entities::P_STATUS, status, &ctx).await?;
        set_single_target(conv, &base, super::entities::P_CURSOR, cursor, &ctx).await?;
        set_single_target(
            conv,
            &base,
            super::entities::P_PENDING_TOOL_CALL,
            pending_tool_call,
            &ctx,
        )
        .await?;
    } else {
        let rs = RunState {
            id: format!("we://run_state/{}", uuid::Uuid::new_v4()),
            thread_id: thread_id.to_string(),
            status: status.to_string(),
            cursor: cursor.to_string(),
            pending_tool_call: pending_tool_call.to_string(),
        };
        for link in rs.to_links() {
            conv.add_link(link, LinkStatus::Shared, None, &ctx).await?;
        }
    }
    Ok(())
}

/// Load every active `RunState` across all perspectives (for boot resume),
/// returned as `(perspective_uuid, RunState)` pairs.
pub async fn active_run_states() -> Vec<(String, RunState)> {
    let mut out = Vec::new();
    for p in all_perspectives() {
        for base in find_instances(&p, CLASS_RUN_STATE).await {
            let rs = RunState::from_props(base.clone(), &load_props(&p, &base).await);
            if rs.is_active() && !rs.thread_id.is_empty() {
                out.push((p.uuid.clone(), rs));
            }
        }
    }
    out
}

/// Fetch a mutable perspective handle by uuid.
pub fn writable(perspective_uuid: &str) -> Option<PerspectiveInstance> {
    get_perspective(perspective_uuid)
}

/// Return the base URIs of every `Message` instance in a perspective.
pub async fn message_bases(conv: &PerspectiveInstance) -> Vec<String> {
    find_instances(conv, CLASS_MESSAGE).await
}
