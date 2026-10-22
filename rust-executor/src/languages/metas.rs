//! The metas of several languages in one call (`runtime.linkLanguageTemplates`).

use super::LanguageController;
use crate::types::LanguageMeta;

impl LanguageController {
    /// The meta of each language in `addresses`, read concurrently from the
    /// language language as `language.meta` reads it. A meta that cannot be
    /// read is left out with a warning.
    pub async fn language_metas(&self, addresses: &[String]) -> Vec<LanguageMeta> {
        let reads = addresses.iter().map(|a| self.get_language_expression(a));
        futures::future::join_all(reads)
            .await
            .into_iter()
            .zip(addresses)
            .filter_map(|(meta, address)| {
                meta.map_err(|e| log::warn!("No meta for language {}: {}", address, e))
                    .ok()
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::super::language_runtime::LanguageOperation;
    use super::super::language_runtime_handle::LanguageRuntimeHandle;
    use super::*;
    use uuid::Uuid;

    /// One call returns the meta of every template the language language
    /// has, and leaves out one it cannot read instead of failing.
    #[tokio::test]
    async fn language_metas_returns_each_readable_meta() {
        let controller = LanguageController::global_instance();
        let ll = format!("QmFakeLanguageLanguage{}", Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        controller
            .set_runtime_for_test(
                &ll,
                Some(LanguageRuntimeHandle::from_channel(ll.clone(), tx)),
            )
            .await;
        let previous = controller
            .system_addresses
            .lock()
            .await
            .language_language
            .replace(ll.clone());
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                let LanguageOperation::Execute(script, _) = &request.operation else {
                    continue;
                };
                let answer = if script.contains("expressionGet(\"QmTemplateA\")") {
                    Ok(r#"{"author":"did:test:a","data":{"name":"A","description":"a","possibleTemplateParams":["uid","name"]}}"#.to_string())
                } else if script.contains("expressionGet(\"QmTemplateB\")") {
                    Ok(r#"{"author":"did:test:b","data":{"name":"B"}}"#.to_string())
                } else {
                    Err("the store is down".to_string())
                };
                let _ = request.response_tx.send(answer);
            }
        });

        let addresses = ["QmTemplateA", "QmUnreadable", "QmTemplateB"].map(String::from);
        let metas = controller.language_metas(&addresses).await;

        controller.system_addresses.lock().await.language_language = previous;
        controller.set_runtime_for_test(&ll, None).await;
        let summary: Vec<_> = metas
            .iter()
            .map(|m| {
                (
                    m.address.as_str(),
                    m.name.as_str(),
                    m.author.as_str(),
                    m.possible_template_params.clone(),
                )
            })
            .collect();
        assert_eq!(
            summary,
            vec![
                (
                    "QmTemplateA",
                    "A",
                    "did:test:a",
                    Some(vec!["uid".to_string(), "name".to_string()])
                ),
                ("QmTemplateB", "B", "did:test:b", None),
            ]
        );
    }
}
