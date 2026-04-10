use super::byte_array::ByteArray;
use super::LanguageController;
use crate::{
    graphql::graphql_types::{OnlineAgent, PerspectiveExpression},
    types::{Perspective, PerspectiveDiff},
};
use base64::prelude::*;
use deno_core::error::AnyError;

#[derive(Clone)]
pub struct Language {
    address: String,
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
        Self { address }
    }

    pub fn address(&self) -> &str {
        &self.address
    }

    // Each of the four methods below guards the corresponding
    // linksAdapter entry. The spec §5.2 split (perspective-commit /
    // perspective-sync / peers) makes every sub-method optional, and
    // the flat bootstrap shim can produce a linksAdapter with any
    // combination of sync/commit/render/currentRevision present. The
    // previous scripts assumed all four existed whenever linksAdapter
    // did and crashed deep in v8 otherwise.
    pub async fn sync(&mut self) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            (language.linksAdapter && typeof language.linksAdapter.sync === "function")
                ? await language.linksAdapter.sync()
                : null
        "#;

        controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        let controller = LanguageController::global_instance();
        let diff_json = serde_json::to_string(&diff)?;
        // Spec §5.2 perspective-commit returns nothing, so `await commit(...)`
        // resolves to `undefined` for flat commit-only languages. Plain
        // `JSON.stringify(undefined)` returns the JS value `undefined`
        // rather than the string `"undefined"`, which v8
        // to_rust_string_lossy then captures as the literal string
        // "undefined" — which is NOT valid JSON, so parse_revision fails
        // with a serde parse error and the caller treats a successful
        // commit as an infrastructure failure. Coerce the awaited value
        // to `null` before stringifying so we always round-trip valid
        // JSON across the v8 → Rust boundary.
        let script = format!(
            r#"
            JSON.stringify(
                (language.linksAdapter && typeof language.linksAdapter.commit === "function")
                    ? ((await language.linksAdapter.commit({})) ?? null)
                    : null
            )
            "#,
            diff_json
        );

        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        let controller = LanguageController::global_instance();
        // See the comment on `commit` above — the same
        // JSON.stringify(undefined) trap applies here. A language whose
        // currentRevision callable is present but returns `undefined`
        // (e.g. "never synced") would otherwise surface as a JSON parse
        // error instead of Ok(None). Coerce to null before stringifying.
        let script = r#"
            JSON.stringify(
                (language.linksAdapter && typeof language.linksAdapter.currentRevision === "function")
                    ? ((await language.linksAdapter.currentRevision()) ?? null)
                    : null
            )
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        let controller = LanguageController::global_instance();
        // Same undefined-trap fix as `commit` and `current_revision`.
        let script = r#"
            JSON.stringify(
                (language.linksAdapter && typeof language.linksAdapter.render === "function")
                    ? ((await language.linksAdapter.render()) ?? null)
                    : null
            )
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }

    pub async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        let controller = LanguageController::global_instance();
        // A Language may expose a linksAdapter without implementing the
        // peers capability (`peers-remote` in the new spec). Guard the
        // method presence and coerce a missing/null return to an empty
        // list so we never try to deserialize `null` into `Vec<String>`.
        let script = r#"
            JSON.stringify(
                (language.linksAdapter && typeof language.linksAdapter.others === "function")
                    ? (await language.linksAdapter.others() ?? [])
                    : []
            )
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let others_vec = serde_json::from_str(&result)?;
        Ok(others_vec)
    }

    pub async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            language.telepresenceAdapter ? true : false
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;

        // The result should be "true" or "false" as a string
        Ok(result.trim() == "true")
    }

    // Spec §5 — every telepresence method is optional. The bootstrap
    // shim attaches the telepresenceAdapter whenever ANY telepresence*
    // export is present, so individual methods may legitimately be
    // undefined. Each dispatcher below guards its method accordingly.
    pub async fn set_online_status(
        &mut self,
        status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"
            (language.telepresenceAdapter && typeof language.telepresenceAdapter.setOnlineStatus === "function")
                ? await language.telepresenceAdapter.setOnlineStatus({})
                : null
            "#,
            status_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify(
                (language.telepresenceAdapter && typeof language.telepresenceAdapter.getOnlineAgents === "function")
                    ? (await language.telepresenceAdapter.getOnlineAgents() ?? [])
                    : []
            )
        "#;

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
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        // JSON-encode the DID so it becomes a properly quoted and
        // escaped JS string literal. The previous `"{}"` interpolation
        // would break (or allow script injection) for any DID that
        // contained a `"`, backslash, or newline.
        let did_literal = serde_json::to_string(&remote_agent_did)?;
        let script = format!(
            r#"
            (language.telepresenceAdapter && typeof language.telepresenceAdapter.sendSignal === "function")
                ? await language.telepresenceAdapter.sendSignal({}, {})
                : null
            "#,
            did_literal, payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn send_broadcast(&mut self, payload: PerspectiveExpression) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"
            (language.telepresenceAdapter && typeof language.telepresenceAdapter.sendBroadcast === "function")
                ? await language.telepresenceAdapter.sendBroadcast({})
                : null
            "#,
            payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn set_local_agents(&mut self, agents: Vec<String>) -> Result<(), AnyError> {
        log::debug!("set_local_agents: agents: {:?}", agents);
        let controller = LanguageController::global_instance();
        let agents_json = serde_json::to_string(&agents)?;
        // Tighten the guard to `typeof === "function"` to match the rest of
        // the linksAdapter dispatchers. A language may legitimately expose
        // `linksAdapter` without a `setLocalAgents` (spec §5.2 peers is
        // optional); the old truthy check would still TypeError if some
        // language assigned a non-function truthy value to the slot.
        let script = format!(
            r#"
            (language.linksAdapter && typeof language.linksAdapter.setLocalAgents === "function")
                ? await language.linksAdapter.setLocalAgents({})
                : null
            "#,
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
