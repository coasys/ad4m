use serde_json::Value;

use crate::js_core::{self, JsCoreHandle};

pub struct Language {
    address: String,
    js_core: JsCoreHandle,
}

impl Language {
    pub fn new(address: String, js_core: JsCoreHandle) -> Self {
        Self {
            address,
            js_core
        }
    }

    pub async fn create_public(&self, data: Value) -> Result<String, AnyError> {
        let script = format!(
            r#"JSON.stringify(
            await core.callResolver(
                "Mutation",
                "runtimeAddKnownLinkLanguageTemplates",
                {{ addresses: {} }},
            ))"#,
            addresses_json,
        );
        let result = js.execute(script).await?;
        let result: JsResultType<Vec<String>> = serde_json::from_str(&result)?;
    }

    pub async fn sync() -> Result<(), AnyError> {
        let script = format!(
            r#"
            JSON.stringify(
                await core
                    .languageController
                    .languageByRef("{}")
                    .getLinksAdapter()
                    .sync() 
            )
            "#,
            address,
        );
        let _result: String = Self::global_instance().js_core.execute(script).await?;
        Ok()
    }

    pub async fn commit(diff: PerspectiveDiff) -> Result<(), AnyError> {
        let script = format!(
            r#"
            JSON.stringify(
                await core
                    .languageController
                    .languageByRef("{}")
                    .getLinksAdapter()
                    .commit({}) 
            )
            "#,
            address,
            serde_json::to_string(&diff)?,
        );
        let _result: String = Self::global_instance().js_core.execute(script).await?;
        Ok()
    }

    pub async fn currentRevision() -> Result<String, AnyError> {
        let script = format!(
            r#"
            JSON.stringify(
                await core
                    .languageController
                    .languageByRef("{}")
                    .getLinksAdapter()
                    .currentRevision() 
            )
            "#,
            address,
        );
        let result: String = Self::global_instance().js_core.execute(script).await?;
        Ok(result)
    }
}