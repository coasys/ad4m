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
}