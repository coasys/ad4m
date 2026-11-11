//! Tasks whose `model_id` is the `default` variable must follow the
//! default LLM's worker thread when that thread is replaced.
use super::*;
use crate::db::Ad4mDb;
use crate::types::{ModelApiInput, ModelApiType, ModelInput, ModelType};

/// An OpenAI-shaped endpoint that answers every completion with `reply`.
async fn openai_stub(reply: &str) -> (mockito::ServerGuard, mockito::Mock) {
    let mut server = mockito::Server::new_async().await;
    let mock = server
        .mock("POST", "/v1/chat/completions")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(
            serde_json::json!({
                "id": "chatcmpl-test",
                "object": "chat.completion",
                "created": 0,
                "model": "stub",
                "choices": [{
                    "index": 0,
                    "message": {"role": "assistant", "content": reply},
                    "finish_reason": "stop"
                }],
                "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
            })
            .to_string(),
        )
        .expect_at_least(0)
        .create_async()
        .await;
    (server, mock)
}

fn remote_llm(name: &str, base_url: &str, model: &str) -> ModelInput {
    ModelInput {
        name: name.to_string(),
        model_type: ModelType::Llm,
        local: None,
        api: Some(ModelApiInput {
            base_url: format!("{base_url}/v1"),
            api_key: None,
            model: model.to_string(),
            api_type: ModelApiType::OpenAi.to_string(),
            max_num_ctx: None,
        }),
    }
}

/// A service without the background `load()`: only what the test adds runs.
fn bare_service() -> AIService {
    AIService {
        embedding_channel: Arc::new(Mutex::new(HashMap::new())),
        llm_channel: Arc::new(Mutex::new(HashMap::new())),
        transcription_streams: Arc::new(Mutex::new(HashMap::new())),
        cleanup_task_shutdown: Arc::new(std::sync::Mutex::new(None)),
        shared_whisper_models: Arc::new(Mutex::new(HashMap::new())),
        default_embedding: Arc::new(default_embedding::DefaultEmbedding::new(Arc::new(|_| {
            Box::pin(async { Err(anyhow!("no embedding models in this test")) })
        }))),
    }
}

async fn default_task(service: &AIService) -> AITask {
    service
        .add_task(
            AITaskInput {
                name: "adam://interpretation".into(),
                model_id: "default".into(),
                system_prompt: "Interpret.".into(),
                prompt_examples: vec![],
                meta_data: None,
            },
            None,
        )
        .await
        .expect("task on the default model")
}

// Staging, 2026-11-10: `ai.updateModel` on the default LLM restarted its
// worker and re-registered only tasks whose `model_id` was that model's
// id. The interpretation task names `default`, so every prompt through it
// then failed with "Task with ID … not spawned".
#[tokio::test]
async fn updating_the_default_model_respawns_tasks_on_default() {
    Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    let (server, _mock) = openai_stub("still here").await;
    let service = bare_service();

    let model_id = service
        .add_model(remote_llm("default-llm", &server.url(), "before"))
        .await
        .expect("add model");
    service
        .set_default_model(ModelType::Llm, model_id.clone())
        .await
        .expect("set default");
    let task = default_task(&service).await;

    service
        .update_model(
            model_id.clone(),
            remote_llm("default-llm", &server.url(), "after"),
        )
        .await
        .expect("update model");

    let result = service
        .prompt(task.task_id.clone(), "hello".into(), None)
        .await
        .expect("a task on `default` is spawned on the restarted worker");
    assert_eq!(result.text, "still here");
    assert_eq!(result.model_id, model_id);
}

// Changing the default must move `default` tasks to the new default's worker.
#[tokio::test]
async fn setting_a_new_default_moves_tasks_on_default() {
    Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    let (first_server, _first) = openai_stub("from first").await;
    let (second_server, _second) = openai_stub("from second").await;
    let service = bare_service();

    let first = service
        .add_model(remote_llm("first", &first_server.url(), "first"))
        .await
        .expect("add first model");
    let second = service
        .add_model(remote_llm("second", &second_server.url(), "second"))
        .await
        .expect("add second model");
    service
        .set_default_model(ModelType::Llm, first.clone())
        .await
        .expect("default: first");
    let task = default_task(&service).await;

    service
        .set_default_model(ModelType::Llm, second.clone())
        .await
        .expect("default: second");

    let result = service
        .prompt(task.task_id.clone(), "hello".into(), None)
        .await
        .expect("a task on `default` is spawned on the new default's worker");
    assert_eq!(result.text, "from second");
    assert_eq!(result.model_id, second);
}
