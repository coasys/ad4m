use super::byte_array::ByteArray;
use super::LanguageController;
use crate::{
    graphql::graphql_types::{OnlineAgent, PerspectiveExpression},
    js_core::JsCoreHandle,
    types::{Perspective, PerspectiveDiff},
};
use base64::prelude::*;
use deno_core::error::AnyError;

#[derive(Clone)]
pub struct Language {
    address: String,
    // Legacy field for backward compatibility
    js_core: JsCoreHandle,
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
    pub fn new(address: String, js_core: JsCoreHandle) -> Self {
        Self { address, js_core }
    }

    pub async fn sync(&mut self) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {
                    return await language.linksAdapter.sync();
                }
                return null;
            })()
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
        let script = format!(
            r#"
            JSON.stringify((async function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {{
                    return await language.linksAdapter.commit({});
                }}
                return null;
            }})())
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
        let script = r#"
            JSON.stringify((async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {
                    return await language.linksAdapter.currentRevision();
                }
                return null;
            })())
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify((async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {
                    return await language.linksAdapter.render();
                }
                return null;
            })())
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
        let script = r#"
            JSON.stringify((async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {
                    return await language.linksAdapter.others();
                }
                return null;
            })())
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
        let metadata = controller.get_language_metadata(&self.address).await;
        Ok(metadata.map(|m| m.has_telepresence_adapter).unwrap_or(false))
    }

    pub async fn set_online_status(
        &mut self,
        status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"
            (async function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {{
                    return await language.telepresenceAdapter.setOnlineStatus({});
                }}
                return null;
            }})()
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
            JSON.stringify((async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {
                    return await language.telepresenceAdapter.getOnlineAgents();
                }
                return null;
            })())
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
        let script = format!(
            r#"
            (async function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {{
                    return await language.telepresenceAdapter.sendSignal("{}", {});
                }}
                return null;
            }})()
            "#,
            remote_agent_did, payload_json
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
            (async function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {{
                    return await language.telepresenceAdapter.sendBroadcast({});
                }}
                return null;
            }})()
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
        let script = format!(
            r#"
            (async function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter && language.linksAdapter.setLocalAgents) {{
                    return await language.linksAdapter.setLocalAgents({});
                }}
                return null;
            }})()
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
