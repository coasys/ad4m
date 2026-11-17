use super::AIService;
use crate::types::ModelType;

impl AIService {
    /// Whether a prompt to the default LLM could run now: a default is set
    /// and a worker for it is spawned. `false` on an executor with no model
    /// configured, where every prompt to a task on the default model fails.
    pub async fn default_llm_ready(&self) -> bool {
        let Ok(Some(model_id)) = crate::db_backend::db_backend().get_default_model(ModelType::Llm)
        else {
            return false;
        };
        self.llm_channel.lock().await.contains_key(&model_id)
    }
}
