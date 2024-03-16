use deno_core::error::AnyError;
use serde_json::Value;

use crate::{js_core::{self, JsCoreHandle}, types::{Perspective, PerspectiveDiff}};

#[derive(Clone)]
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

    pub async fn sync(&mut self) -> Result<(), AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.sync() 
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
        );
        println!("sync script: {}", script);
        let _result: String = self.js_core.execute(script).await?;
        Ok(())
    }

    pub async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.commit({}) 
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
            serde_json::to_string(&diff)?,
        );
        let result: String = self.js_core.execute(script).await?;
        let rev: Option<String> = serde_json::from_str(&result)?;
        Ok(rev)
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.currentRevision() 
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let maybe_revision = serde_json::from_str(&result)?;
        Ok(maybe_revision)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.render() 
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }
}
