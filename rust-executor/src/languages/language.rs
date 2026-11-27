//! Executor-side view of a loaded Language.
//!
//! A thin handle: the content-address of the loaded language plus its
//! cached capability set. Every method dispatches through
//! `LanguageController::execute_on_language`, which routes to the
//! per-language Deno runtime where the actual JS (or Rust-via-WASM) code
//! runs.
//!
//! The `impl` blocks below are grouped by capability, mirroring the
//! authoritative split in `ad4m-ldk/rust/src/traits.rs`:
//!
//! | Block here                    | Rust ALDK trait                   |
//! |-------------------------------|-----------------------------------|
//! | lifecycle                     | `Language` (required)             |
//! | perspective-commit            | `PerspectiveCommitCapability`     |
//! | perspective-sync              | `PerspectiveSyncCapability`       |
//! | peers                         | `PeersCapability`                 |
//! | telepresence                  | `TelepresenceCapability`          |
//!
//! Each method checks the cached capability set and early-returns its
//! default when the language hasn't advertised the matching capability,
//! so callers can call freely without per-call guards.

use super::byte_array::ByteArray;
use super::capability::{get_capabilities, Capability};
use super::language_runtime::CallKind;
use super::LanguageController;
use crate::{
    agent::AgentContext,
    types::{LinkExpression, OnlineAgent, Perspective, PerspectiveDiff, PerspectiveExpression},
};
use base64::prelude::*;
use deno_core::error::AnyError;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Clone)]
pub struct Language {
    address: String,
    capabilities: Arc<HashSet<Capability>>,
    /// Test-only: answers [`Self::commit`] in place of the language runtime.
    #[cfg(test)]
    test_commit: Option<TestCommit>,
    /// Test-only: answers [`Self::render`] in place of the language runtime.
    #[cfg(test)]
    test_render: Option<TestRender>,
    /// Test-only: answers [`Self::missing`] in place of the language runtime.
    #[cfg(test)]
    test_missing: Option<TestMissing>,
}

#[cfg(test)]
pub(crate) type TestCommit =
    Arc<dyn Fn(PerspectiveDiff) -> Result<Option<String>, AnyError> + Send + Sync>;

#[cfg(test)]
pub(crate) type TestRender = Arc<dyn Fn() -> Result<Option<Perspective>, AnyError> + Send + Sync>;

#[cfg(test)]
pub(crate) type TestMissing =
    Arc<dyn Fn(&[LinkExpression]) -> Result<Option<Vec<LinkExpression>>, AnyError> + Send + Sync>;

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

// ---------------------------------------------------------------------------
// Lifecycle — the bits every Language has (ALDK `Language` trait).
// ---------------------------------------------------------------------------
impl Language {
    pub fn new(address: String) -> Self {
        let capabilities = get_capabilities(&address);
        Self {
            address,
            capabilities,
            #[cfg(test)]
            test_commit: None,
            #[cfg(test)]
            test_render: None,
            #[cfg(test)]
            test_missing: None,
        }
    }

    /// Test seam: a language whose `commit` is `commit`, with no runtime.
    #[cfg(test)]
    pub(crate) fn with_test_commit(address: String, commit: TestCommit) -> Self {
        Self {
            test_commit: Some(commit),
            ..Self::new(address)
        }
    }

    /// Test seam: `render` answers with `render`, whatever the capability set.
    #[cfg(test)]
    pub(crate) fn with_test_render(self, render: TestRender) -> Self {
        Self {
            test_render: Some(render),
            ..self
        }
    }

    /// Test seam: `missing` answers with `missing`, and the language has
    /// `Capability::PerspectiveMissing`.
    #[cfg(test)]
    pub(crate) fn with_test_missing(self, missing: TestMissing) -> Self {
        let mut capabilities = (*self.capabilities).clone();
        capabilities.insert(Capability::PerspectiveMissing);
        Self {
            capabilities: Arc::new(capabilities),
            test_missing: Some(missing),
            ..self
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
    /// run will report an empty capability set.
    pub fn has(&self, cap: Capability) -> bool {
        self.capabilities.contains(&cap)
    }
}

// ---------------------------------------------------------------------------
// Perspective-commit — ALDK `PerspectiveCommitCapability`.
// Write a diff; may or may not return the new revision depending on whether
// the language also exports `perspective-sync`.
// ---------------------------------------------------------------------------
impl Language {
    pub async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        #[cfg(test)]
        if let Some(commit) = &self.test_commit {
            return commit(diff);
        }
        if !self.has(Capability::PerspectiveCommit) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let diff_json = serde_json::to_string(&diff)?;
        // Spec §5.2 perspective-commit returns nothing, so `await commit(...)`
        // resolves to `undefined` for commit-only languages. Coerce to `null`
        // before stringifying so the v8 → Rust boundary round-trips valid JSON.
        let script = format!(
            r#"JSON.stringify((await language.perspectiveCommit({})) ?? null)"#,
            diff_json
        );

        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }
}

// ---------------------------------------------------------------------------
// Perspective-sync — ALDK `PerspectiveSyncCapability`.
// Pull remote state into the language's local store; report the current
// revision so callers can detect when a perspective is up to date; optionally
// render the full perspective from scratch.
// ---------------------------------------------------------------------------
impl Language {
    pub async fn sync(&mut self) -> Result<(), AnyError> {
        if !self.has(Capability::PerspectiveSync) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let script = r#"await language.perspectiveSyncSync()"#;

        controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        if !self.has(Capability::PerspectiveCurrentRevision) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let script = r#"JSON.stringify((await language.perspectiveSyncCurrentRevision()) ?? null)"#;

        // Ordered, as `render`: the fallback sync (`share_links_missing_remotely`)
        // reads both as a fence. Run beside an in-flight commit, they would miss
        // its links, and the pass would commit them again.
        let result = controller
            .call_language(
                &self.address,
                script,
                AgentContext::main_agent(),
                CallKind::Ordered,
            )
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        #[cfg(test)]
        if let Some(render) = &self.test_render {
            return render();
        }
        if !self.has(Capability::PerspectiveRender) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let script = r#"JSON.stringify((await language.perspectiveSyncRender()) ?? null)"#;

        // Ordered: see `current_revision`.
        let result = controller
            .call_language(
                &self.address,
                script,
                AgentContext::main_agent(),
                CallKind::Ordered,
            )
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }

    /// Of `links`, the ones the remote lacks, compared by author, timestamp,
    /// source, predicate (`""` = none) and target. `None` when the language
    /// does not know what the remote holds, or lacks
    /// `Capability::PerspectiveMissing`.
    pub async fn missing(
        &mut self,
        links: &[LinkExpression],
    ) -> Result<Option<Vec<LinkExpression>>, AnyError> {
        #[cfg(test)]
        if let Some(missing) = &self.test_missing {
            return missing(links);
        }
        if !self.has(Capability::PerspectiveMissing) {
            return Ok(None);
        }
        let controller = LanguageController::global_instance();
        let script = format!(
            r#"JSON.stringify((await language.perspectiveSyncMissing({})) ?? null)"#,
            serde_json::to_string(links)?
        );

        // Ordered: see `current_revision`. A commit sent before the ask is
        // applied first, so its links are not reported missing. Callers ask in
        // short batches, so other calls run between them.
        let result = controller
            .call_language(
                &self.address,
                &script,
                AgentContext::main_agent(),
                CallKind::Ordered,
            )
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(serde_json::from_str(&result)?)
    }
}

// ---------------------------------------------------------------------------
// Peers — ALDK `PeersCapability`.
// Tell the language which local agents to sign/commit as, and query the
// set of known remote agents for diff fan-out.
// ---------------------------------------------------------------------------
impl Language {
    pub async fn set_local_agents(&mut self, agents: Vec<String>) -> Result<(), AnyError> {
        if !self.has(Capability::PeersLocal) {
            return Ok(());
        }
        log::debug!("set_local_agents: agents: {:?}", agents);
        let controller = LanguageController::global_instance();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(r#"await language.peersSetLocal({})"#, agents_json);

        log::debug!("set_local_agents script: {}", script);
        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        log::debug!("set_local_agents result: {}", result);
        Ok(())
    }

    pub async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        if !self.has(Capability::PeersRemote) {
            return Ok(Vec::new());
        }
        let controller = LanguageController::global_instance();
        let script = r#"JSON.stringify((await language.peersRemote()) ?? [])"#;

        let result = controller
            .call_language(
                &self.address,
                script,
                AgentContext::main_agent(),
                CallKind::Concurrent,
            )
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let others_vec = serde_json::from_str(&result)?;
        Ok(others_vec)
    }
}

// ---------------------------------------------------------------------------
// Telepresence — ALDK `TelepresenceCapability`.
// Low-latency online-status + directed/broadcast signals that bypass the
// durable commit path.
// ---------------------------------------------------------------------------
impl Language {
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
            r#"await language.telepresenceSetOnlineStatus({})"#,
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
        let script = r#"JSON.stringify((await language.telepresenceGetOnlineAgents()) ?? [])"#;

        let result = controller
            .call_language(
                &self.address,
                script,
                AgentContext::main_agent(),
                CallKind::Concurrent,
            )
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
        // JSON-encode the DID so it embeds as a properly quoted JS string
        // literal — defensive against any DID containing `"`, `\`, or newline.
        let did_literal = serde_json::to_string(&remote_agent_did)?;
        let script = format!(
            r#"await language.telepresenceSendSignal({}, {})"#,
            did_literal, payload_json
        );

        // Emits one message and writes no state a later call depends on; signal order
        // is not guaranteed to the remote end anyway (MeshManager numbers offers and
        // drops overtaken ones), so it runs beside the other calls — the call queue
        // was the latency this removes (#1856).
        controller
            .call_language(
                &self.address,
                &script,
                AgentContext::main_agent(),
                CallKind::Concurrent,
            )
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
            r#"await language.telepresenceSendBroadcast({})"#,
            payload_json
        );

        // Same as send_signal: one emit, no language state, no order to keep.
        controller
            .call_language(
                &self.address,
                &script,
                AgentContext::main_agent(),
                CallKind::Concurrent,
            )
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::languages::capability::{register_capabilities, remove_capabilities};
    use crate::languages::language_runtime::LanguageOperation;
    use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
    use crate::types::{ExpressionProof, Link};

    fn link(target: &str) -> LinkExpression {
        LinkExpression {
            author: "did:key:alice".to_string(),
            timestamp: "2026-11-27T00:00:00.000Z".to_string(),
            data: Link {
                source: "ad4m://self".to_string(),
                predicate: None,
                target: target.to_string(),
            },
            proof: ExpressionProof::default(),
            status: None,
        }
    }

    /// `missing` sends the links to `perspectiveSyncMissing` as one ordered call
    /// (so a commit sent before it is applied first) and reads the answer: the
    /// links the remote lacks, or null when the language does not know.
    #[tokio::test]
    async fn missing_asks_the_language_in_an_ordered_call() {
        let controller = LanguageController::global_instance();
        let address = format!("QmMissing{}", uuid::Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        controller
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        register_capabilities(&address, HashSet::from([Capability::PerspectiveMissing]));
        let asked = vec![link("a"), link("b")];

        for answer in [Some(vec![link("b")]), None] {
            let mut language = Language::new(address.clone());
            let links = asked.clone();
            let call = tokio::spawn(async move { language.missing(&links).await });
            let request = requests.recv().await.unwrap();
            let LanguageOperation::Execute(script, _, kind) = request.operation else {
                panic!("not a script: {:?}", request.operation);
            };
            assert_eq!(kind, CallKind::Ordered);
            assert_eq!(
                script,
                format!(
                    "JSON.stringify((await language.perspectiveSyncMissing({})) ?? null)",
                    serde_json::to_string(&asked).unwrap()
                )
            );
            request
                .response_tx
                .send(Ok(serde_json::to_string(&answer).unwrap()))
                .unwrap();
            let got = call.await.unwrap().unwrap();
            assert_eq!(
                got.map(|l| l.iter().map(|l| l.data.target.clone()).collect::<Vec<_>>()),
                answer.map(|l| l.iter().map(|l| l.data.target.clone()).collect::<Vec<_>>())
            );
        }

        controller.set_runtime_for_test(&address, None).await;
        remove_capabilities(&address);
    }

    /// A language without the capability is never asked.
    #[tokio::test]
    async fn missing_without_the_capability_is_none() {
        let mut language = Language::new(format!("QmNoMissing{}", uuid::Uuid::new_v4().simple()));
        assert!(language.missing(&[link("a")]).await.unwrap().is_none());
    }
}
