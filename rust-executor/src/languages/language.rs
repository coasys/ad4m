use super::byte_array::ByteArray;
use super::capability::{get_capabilities, Capability};
use super::LanguageController;
use crate::{
    graphql::graphql_types::{OnlineAgent, PerspectiveExpression},
    types::{Perspective, PerspectiveDiff},
};
use base64::prelude::*;
use deno_core::error::AnyError;
use std::collections::HashSet;
use std::sync::Arc;

// The executor-side Language type is intentionally kept as a single struct
// carrying a cached capability set — the capability carving follows the
// authoritative split in `ad4m-ldk/rust/src/traits.rs` (Language +
// ExpressionCapability + Perspective{Commit,Sync,Query}Capability +
// PeersCapability + TelepresenceCapability + HolochainSignalHandler). Each
// method here checks the capability once on the Rust side and the JS
// dispatcher scripts are free to call the underlying function unguarded.
#[derive(Clone)]
pub struct Language {
    address: String,
    capabilities: Arc<HashSet<Capability>>,
}

fn parse_revision(js_result: String) -> Result<Option<String>, AnyError> {
    if let Ok(maybe_revision) = serde_json::from_str::<Option<ByteArray>>(&js_result) {
        Ok(maybe_revision.map(|revision| {
            let vec: Vec<u8> = revision.into();
            BASE64_STANDARD.encode(vec)
        }))
    } else {
        Ok(serde_json::from_str::<Option<String>>(&js_result)?)
    }
}
impl Language {
    pub fn new(address: String) -> Self {
        let capabilities = get_capabilities(&address);
        Self {
            address,
            capabilities,
        }
    }

    pub fn address(&self) -> &str {
        &self.address
    }

    /// Returns true iff the language's detected capability set contains `cap`.
    ///
    /// The set is populated once in `LanguageRuntime::register_callbacks` and
    /// stored in the global capability registry; `Language::new` reads from
    /// that registry, so a language whose `register_callbacks` has not yet
    /// run (e.g. an in-flight load) will report an empty capability set.
    pub fn has(&self, cap: Capability) -> bool {
        self.capabilities.contains(&cap)
    }

    pub async fn sync(&mut self) -> Result<(), AnyError> {
        if !self.has(Capability::PerspectiveSync) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let script = r#"await language.linksAdapter.sync()"#;

        controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        if !self.has(Capability::PerspectiveCommit) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let diff_json = serde_json::to_string(&diff)?;
        // Spec §5.2 perspective-commit returns nothing, so `await commit(...)`
        // resolves to `undefined` for commit-only languages. Coerce to `null`
        // before stringifying so the v8 → Rust boundary round-trips valid JSON.
        let script = format!(
            r#"JSON.stringify((await language.linksAdapter.commit({})) ?? null)"#,
            diff_json
        );

        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        if !self.has(Capability::PerspectiveCurrentRevision) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        // Same undefined → null coercion as `commit`.
        let script = r#"JSON.stringify((await language.linksAdapter.currentRevision()) ?? null)"#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        if !self.has(Capability::PerspectiveRender) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let script = r#"JSON.stringify((await language.linksAdapter.render()) ?? null)"#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }

    pub async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        if !self.has(Capability::PeersRemote) {
            return Ok(Vec::new());
        }
        let controller = LanguageController::global_instance();
        let script =
            r#"JSON.stringify((await language.linksAdapter.others()) ?? [])"#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let others_vec = serde_json::from_str(&result)?;
        Ok(others_vec)
    }

    pub async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
        // A language has a "telepresence adapter" iff it advertises any
        // telepresence-* capability. Consult the cached set rather than
        // round-tripping through v8 on every call.
        Ok(self.has(Capability::TelepresenceSetStatus)
            || self.has(Capability::TelepresenceGetAgents)
            || self.has(Capability::TelepresenceSendSignal)
            || self.has(Capability::TelepresenceSendBroadcast))
    }

    pub async fn set_online_status(
        &mut self,
        status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        if !self.has(Capability::TelepresenceSetStatus) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"await language.telepresenceAdapter.setOnlineStatus({})"#,
            status_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
        if !self.has(Capability::TelepresenceGetAgents) {
            return Ok(Vec::new());
        }
        let controller = LanguageController::global_instance();
        let script = r#"JSON.stringify((await language.telepresenceAdapter.getOnlineAgents()) ?? [])"#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let online_agents = serde_json::from_str(&result)?;
        Ok(online_agents)
    }

    pub async fn send_signal(
        &mut self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        if !self.has(Capability::TelepresenceSendSignal) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        // JSON-encode the DID so it becomes a properly quoted JS string
        // literal. A bare `"{}"` interpolation would break for any DID
        // containing a `"`, `\`, or newline (unlikely in practice, trivial
        // to fix correctly).
        let did_literal = serde_json::to_string(&remote_agent_did)?;
        let script = format!(
            r#"await language.telepresenceAdapter.sendSignal({}, {})"#,
            did_literal, payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn send_broadcast(&mut self, payload: PerspectiveExpression) -> Result<(), AnyError> {
        if !self.has(Capability::TelepresenceSendBroadcast) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"await language.telepresenceAdapter.sendBroadcast({})"#,
            payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn set_local_agents(&mut self, agents: Vec<String>) -> Result<(), AnyError> {
        if !self.has(Capability::PeersLocal) {
            return Ok(());
        }
        log::debug!("set_local_agents: agents: {:?}", agents);
        let controller = LanguageController::global_instance();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(
            r#"await language.linksAdapter.setLocalAgents({})"#,
            agents_json
        );

        log::debug!("set_local_agents script: {}", script);
        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        log::debug!("set_local_agents result: {}", result);
        Ok(())
    }
}
