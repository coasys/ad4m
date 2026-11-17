use super::AIService;
use crate::types::ModelType;

impl AIService {
    /// Whether a prompt to the default LLM could run now: a default is set
    /// and its endpoint client is built. `false` on an executor with no model
    /// configured, where every prompt to a task on the default model fails.
    pub async fn default_llm_ready(&self) -> bool {
        let Ok(Some(model_id)) = crate::db_backend::db_backend().get_default_model(ModelType::Llm)
        else {
            return false;
        };
        self.llms.lock().await.contains_key(&model_id)
    }

    /// A receiver that sees a change whenever [`Self::default_llm_ready`] may
    /// have turned true: a default LLM set, or an LLM client built. A
    /// waiter checks `default_llm_ready` again on a change.
    pub fn watch_llm_readiness(&self) -> tokio::sync::watch::Receiver<u64> {
        self.llm_readiness.subscribe()
    }

    /// Announce that [`Self::default_llm_ready`] may have changed: called
    /// where a default LLM is set and where an LLM client is built, the
    /// two facts it reads.
    pub(super) fn note_llm_readiness(&self) {
        self.llm_readiness.send_modify(|g| *g = g.wrapping_add(1));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::types::{Endpoint, EndpointKind, ModelInput};

    /// Configuring a model is announced: adding an LLM (its client is built)
    /// and making it the default each change the readiness watch, and the
    /// service is ready once both happened.
    #[tokio::test]
    async fn adding_and_defaulting_an_llm_announce_readiness() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::empty();
        let endpoint = Endpoint::for_test(
            "upstream",
            EndpointKind::LlamaServer,
            "http://127.0.0.1:9",
            "",
        );
        Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).unwrap();
        let mut ready = service.watch_llm_readiness();
        ready.borrow_and_update();
        assert!(!service.default_llm_ready().await);

        let model_id = service
            .add_model(ModelInput {
                name: "remote".into(),
                model_type: ModelType::Llm,
                endpoint_id: endpoint.id.clone(),
                remote_model: "m".into(),
                native_tools: None,
            })
            .await
            .expect("model added");
        assert!(
            ready.has_changed().expect("sender alive"),
            "a built LLM client announces readiness"
        );
        ready.borrow_and_update();

        service
            .set_default_model(ModelType::Llm, model_id)
            .await
            .expect("default set");
        assert!(
            ready.has_changed().expect("sender alive"),
            "a new default LLM announces readiness"
        );
        assert!(service.default_llm_ready().await);
    }
}
