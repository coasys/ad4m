//! Tasks whose `model_id` is the `default` variable must follow the
//! default LLM when its client is replaced.
use super::*;
use crate::db::Ad4mDb;
use crate::types::{EndpointInput, EndpointKind, ModelInput, ModelType};

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

/// An LLM `model` on a new endpoint at `base_url`.
async fn remote_llm(service: &AIService, name: &str, base_url: &str, model: &str) -> ModelInput {
    let endpoint = service
        .add_endpoint(EndpointInput {
            name: name.to_string(),
            kind: EndpointKind::Vllm,
            base_url: url::Url::parse(base_url).unwrap(),
            api_key: None,
            capabilities: None,
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
        })
        .await
        .expect("add endpoint");
    ModelInput {
        name: name.to_string(),
        model_type: ModelType::Llm,
        endpoint_id: endpoint.id,
        remote_model: model.to_string(),
        native_tools: None,
    }
}

/// A service without the background `load()`: only what the test adds runs.
fn bare_service() -> AIService {
    AIService::empty()
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

    let before = remote_llm(&service, "default-llm", &server.url(), "before").await;
    let after = ModelInput {
        name: before.name.clone(),
        model_type: ModelType::Llm,
        endpoint_id: before.endpoint_id.clone(),
        remote_model: "after".into(),
        native_tools: None,
    };
    let model_id = service.add_model(before).await.expect("add model");
    service
        .set_default_model(ModelType::Llm, model_id.clone())
        .await
        .expect("set default");
    let task = default_task(&service).await;

    service
        .update_model(model_id.clone(), after)
        .await
        .expect("update model");

    let result = service
        .prompt(task.task_id.clone(), "hello".into(), None)
        .await
        .expect("a task on `default` is prompts the updated model");
    assert_eq!(result.text, "still here");
    assert_eq!(result.model_id, model_id);
}

// Changing the default must move `default` tasks to the new default.
#[tokio::test]
async fn setting_a_new_default_moves_tasks_on_default() {
    Ad4mDb::init_global_instance(":memory:").expect("in-memory db");
    let (first_server, _first) = openai_stub("from first").await;
    let (second_server, _second) = openai_stub("from second").await;
    let service = bare_service();

    let first = service
        .add_model(remote_llm(&service, "first", &first_server.url(), "first").await)
        .await
        .expect("add first model");
    let second = service
        .add_model(remote_llm(&service, "second", &second_server.url(), "second").await)
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
        .expect("a task on `default` is prompts the new default");
    assert_eq!(result.text, "from second");
    assert_eq!(result.model_id, second);
}
