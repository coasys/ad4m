//! The sweep at load for agent replies (`perspectives::agent_reply`).
//!
//! A reply's task does not survive a restart, so a reply still `running` when a perspective loads
//! was cut off. Without the sweep it would read as running for good. Each one by an agent this
//! node hosts is marked `interrupted`, written as that agent. A reply by another node's agent, in
//! a shared perspective, is that node's to settle.

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
        if let Err(e) = self.interrupt_stale_replies(&main_did).await {
            log::warn!("agent replies: the sweep at load failed: {e:#}");
        }
    }

    async fn interrupt_stale_replies(&self, main_did: &str) -> anyhow::Result<()> {
        if !subject_class_registered(self, AGENT_REPLY_TARGET_CLASS).await? {
            return Ok(());
        }
        let mut hosted: HashSet<String> = AgentService::managed_agent_dids()
            .map_err(|e| anyhow::anyhow!("{e:#}"))?
            .into_iter()
            .collect();
        hosted.insert(main_did.to_string());
        for (reply, author) in stale_running_replies(self, &hosted).await? {
            let context = if author == main_did {
                AgentContext::main_agent()
            } else {
                AgentContext::managed(author)
            };
            set_reply(
                self.clone(),
                &reply,
                json!({ "status": ReplyStatus::Interrupted.as_str() }),
                &context,
            )
            .await?;
        }
        Ok(())
    }
}
