//! The sweep at load for agent replies (`perspectives::agent_reply`).
//!
//! A reply's task does not survive a restart, so a reply still `running` when a perspective loads
//! was cut off. Without the sweep it would read as running for good. Each one by an agent this
//! node hosts is marked `interrupted`, written as that agent. A reply by another node's agent, in
//! a shared perspective, is that node's to settle. At teardown the replies still running here are
//! stopped.

use super::PerspectiveInstance;
use crate::agent::{AgentContext, AgentService};
use crate::perspectives::agent_reply::{
    set_reply, stale_running_replies, ReplyStatus, AGENT_REPLY_TARGET_CLASS,
};
use crate::perspectives::hardwired_class::subject_class_registered;
use serde_json::json;
use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// How many times the sweep at load runs before it gives up, and how long it waits between.
const SWEEP_ATTEMPTS: u32 = 3;
const SWEEP_RETRY: Duration = Duration::from_secs(5);

impl PerspectiveInstance {
    pub(crate) async fn agent_reply_sweep_at_load(&self) {
        // The main agent's DID is how its replies are recognised, so wait for the agent, as the
        // flow sweep does.
        let main_did = loop {
            if self.is_teardown.load(Ordering::Acquire) {
                return;
            }
            let did = AgentService::global_instance()
                .lock()
                .ok()
                .and_then(|agent| agent.as_ref().and_then(|a| a.did.clone()));
            if let Some(did) = did {
                break did;
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        };
        // A sweep that fails (a read, a write) is tried again a few times, so a transient error
        // does not leave this node's replies `running` until the next load.
        for attempt in 1..=SWEEP_ATTEMPTS {
            if self.is_teardown.load(Ordering::Acquire) {
                return;
            }
            match self.interrupt_stale_replies(&main_did).await {
                Ok(()) => return,
                Err(e) if attempt < SWEEP_ATTEMPTS => {
                    log::warn!("agent replies: the sweep at load failed, trying again: {e:#}");
                    tokio::time::sleep(SWEEP_RETRY).await;
                }
                Err(e) => log::error!(
                    "agent replies: the sweep at load failed {SWEEP_ATTEMPTS} times; replies \
                     left running stay so until the next load: {e:#}"
                ),
            }
        }
    }

    /// Stop every reply running in this perspective, at teardown, so none runs on for up to
    /// `REPLY_TIMEOUT` in a removed perspective and writes its end there.
    pub(crate) fn abort_agent_replies(&self) {
        let running: Vec<_> = self
            .agent_replies
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .drain()
            .collect();
        for (_, reply) in running {
            reply.abort();
        }
    }

    /// Marks each stale reply `interrupted`. One that can't be written does not stop the rest;
    /// the first such error is returned once all were tried.
    async fn interrupt_stale_replies(&self, main_did: &str) -> anyhow::Result<()> {
        if !subject_class_registered(self, AGENT_REPLY_TARGET_CLASS).await? {
            return Ok(());
        }
        let mut hosted: HashSet<String> = AgentService::managed_agent_dids()
            .map_err(|e| anyhow::anyhow!("{e:#}"))?
            .into_iter()
            .collect();
        hosted.insert(main_did.to_string());
        let mut first_error = None;
        for (reply, author) in stale_running_replies(self, &hosted).await? {
            let context = if author == main_did {
                AgentContext::main_agent()
            } else {
                AgentContext::managed(author)
            };
            if let Err(e) = set_reply(
                self.clone(),
                &reply,
                json!({ "status": ReplyStatus::Interrupted.as_str() }),
                &context,
            )
            .await
            {
                first_error.get_or_insert(e);
            }
        }
        first_error.map_or(Ok(()), Err)
    }
}
