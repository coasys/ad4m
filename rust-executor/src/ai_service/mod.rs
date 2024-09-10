use self::error::AIServiceError;
use crate::db::Ad4mDb;
use crate::graphql::graphql_types::AITaskInput;
use crate::types::AITask;
use anyhow::anyhow;
use deno_core::error::AnyError;
use kalosm::language::*;
use std::collections::HashMap;
use std::panic::catch_unwind;
use std::sync::Arc;
use std::thread;
use tokio::sync::{mpsc, oneshot, Mutex};

mod error;

pub type Result<T> = std::result::Result<T, AnyError>;

lazy_static! {
    static ref AI_SERVICE: Arc<Mutex<Option<AIService>>> = Arc::new(Mutex::new(None));
}

#[derive(Clone)]
pub struct AIService {
    bert: mpsc::UnboundedSender<EmbeddingRequest>,
    llama: mpsc::UnboundedSender<LLMTaskRequest>,
}

struct EmbeddingRequest {
    pub prompt: String,
    pub result_sender: oneshot::Sender<Result<Vec<f32>>>,
}

#[derive(Debug)]
struct LLMTaskSpawnRequest {
    pub task: AITask,
    pub result_sender: oneshot::Sender<Result<()>>,
}

#[derive(Debug)]
struct LLMTaskPromptRequest {
    pub task_id: String,
    pub prompt: String,
    pub result_sender: oneshot::Sender<Result<String>>,
}

#[derive(Debug)]
struct LLMTaskRemoveRequest {
    pub task_id: String,
    pub result_sender: oneshot::Sender<()>,
}

#[derive(Debug)]
enum LLMTaskRequest {
    Spawn(LLMTaskSpawnRequest),
    Prompt(LLMTaskPromptRequest),
    Remove(LLMTaskRemoveRequest),
}

impl AIService {
    pub async fn new() -> Result<Self> {
        let (bert_tx, mut bert_rx) = mpsc::unbounded_channel::<EmbeddingRequest>();
        let (llama_tx, mut llama_rx) = mpsc::unbounded_channel::<LLMTaskRequest>();

        thread::spawn(move || {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let bert = rt
                .block_on(Bert::builder().build())
                .expect("couldn't build Bert model");
            while let Some(request) = rt.block_on(bert_rx.recv()) {
                // let result: Result<Vec<f32>> = match catch_unwind(|| rt.block_on(bert.embed(request.prompt))) {
                //     Err(e) => Err(anyhow!("Bert panicked: {:?}", e)),
                //     Ok(embed_result) => embed_result.and_then(|tensor| Ok(tensor.to_vec()))
                // };

                let result: Result<Vec<f32>> = rt
                    .block_on(bert.embed(request.prompt))
                    .map(|tensor| tensor.to_vec());
                let _ = request.result_sender.send(result);
            }
        });

        thread::spawn(move || {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let llama = rt
                .block_on(
                    Llama::builder()
                        .with_source(LlamaSource::tiny_llama_1_1b())
                        .build(),
                )
                .expect("couldn't build Llama model");

            let mut tasks = HashMap::<String, Task>::new();

            while let Some(task_request) = rt.block_on(llama_rx.recv()) {
                match task_request {
                    LLMTaskRequest::Spawn(spawn_request) => {
                        let task_description = spawn_request.task;
                        let task = Task::builder(task_description.system_prompt.clone())
                            .with_examples(
                                task_description
                                    .prompt_examples
                                    .clone()
                                    .into_iter()
                                    .map(|example| (example.input, example.output))
                                    .collect::<Vec<(String, String)>>(),
                            )
                            .build();

                        let mut task_run = false;
                        let mut tries = 0;
                        while !task_run && tries < 20 {
                            tries += 1;
                            match catch_unwind(|| {
                                rt.block_on(task.run("Test example prompt", &llama).all_text())
                            }) {
                                Err(e) => log::error!(
                                    "Llama panicked during task spawn with: {:?}. Trying again..",
                                    e
                                ),
                                Ok(_) => task_run = true,
                            }
                        }

                        if task_run {
                            tasks.insert(task_description.task_id.clone(), task);
                            let _ = spawn_request.result_sender.send(Ok(()));
                        } else {
                            let _ = spawn_request
                                .result_sender
                                .send(Err(anyhow!("Couldn't run task without panicks")));
                        }
                    }

                    LLMTaskRequest::Prompt(prompt_request) => {
                        if let Some(task) = tasks.get(&prompt_request.task_id) {
                            let mut maybe_result: Option<String> = None;
                            let mut tries = 0;
                            while maybe_result.is_none() && tries < 20 {
                                tries += 1;
                                match catch_unwind(|| {
                                    rt.block_on(
                                        task.run(prompt_request.prompt.clone(), &llama).all_text(),
                                    )
                                }) {
                                    Err(e) => {
                                        log::error!("Llama panicked with: {:?}. Trying again..", e)
                                    }
                                    Ok(result) => maybe_result = Some(result),
                                }
                            }

                            if let Some(result) = maybe_result {
                                let _ = prompt_request.result_sender.send(Ok(result));
                            } else {
                                let _ = prompt_request.result_sender.send(Err(anyhow!("Unable to get response from Llama model. Giving up after 20 retries")));
                            }
                        } else {
                            let _ = prompt_request.result_sender.send(Err(anyhow!(
                                "Task with ID {} not spawned",
                                prompt_request.task_id
                            )));
                        }
                    }

                    LLMTaskRequest::Remove(remove_request) => {
                        let _ = tasks.remove(&remove_request.task_id);
                        let _ = remove_request.result_sender.send(());
                    }
                }
            }
        });

        let service = AIService {
            bert: bert_tx,
            llama: llama_tx,
        };

        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        for task in tasks {
            service.spawn_task(task).await?;
        }

        Ok(service)
    }

    pub async fn init_global_instance() -> Result<()> {
        let new_service = AIService::new().await?;
        let mut ai_service = AI_SERVICE.lock().await;
        *ai_service = Some(new_service);
        Ok(())
    }

    pub async fn global_instance() -> Result<AIService> {
        AI_SERVICE
            .lock()
            .await
            .clone()
            .ok_or(anyhow!(AIServiceError::ServiceNotInitialized))
    }

    pub async fn add_task(&self, task: AITaskInput) -> Result<AITask> {
        let task_id = Ad4mDb::with_global_instance(|db| {
            db.add_task(
                task.name.clone(),
                task.model_id.clone(),
                task.system_prompt.clone(),
                task.prompt_examples
                    .iter()
                    .map(|p| p.clone().into())
                    .collect(),
                task.meta_data.clone(),
            )
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))?
            .expect("to get task that we just created");

        self.spawn_task(task.clone()).await?;
        Ok(task)
    }

    pub async fn delete_task(&self, task_id: String) -> Result<bool> {
        Ad4mDb::with_global_instance(|db| db.remove_task(task_id.clone()))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        self.remove_task(task_id).await?;

        Ok(true)
    }

    pub fn get_tasks() -> Result<Vec<AITask>> {
        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(tasks)
    }

    pub async fn update_task(&self, task: AITask) -> Result<AITask> {
        let task_id = task.task_id.clone();
        Ad4mDb::with_global_instance(|db| {
            db.update_task(
                task.task_id,
                task.name,
                task.model_id,
                task.system_prompt,
                task.prompt_examples,
                task.meta_data,
            )
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        self.remove_task(task_id.clone()).await?;

        let updated_task = Ad4mDb::with_global_instance(|db| db.get_task(task_id.clone()))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;

        self.spawn_task(updated_task.clone()).await?;

        Ok(updated_task)
    }

    pub async fn prompt(&self, task_id: String, prompt: String) -> Result<String> {
        let (result_sender, rx) = oneshot::channel();
        self.llama
            .send(LLMTaskRequest::Prompt(LLMTaskPromptRequest {
                task_id,
                prompt,
                result_sender,
            }))?;
        rx.await?
    }

    pub async fn embed(&self, text: String) -> Result<Vec<f32>> {
        let (result_sender, rx) = oneshot::channel();

        self.bert.send(EmbeddingRequest {
            prompt: text,
            result_sender,
        })?;

        rx.await?
    }

    async fn spawn_task(&self, task: AITask) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.llama.send(LLMTaskRequest::Spawn(LLMTaskSpawnRequest {
            task,
            result_sender: tx,
        }))?;
        rx.await?
    }

    async fn remove_task(&self, task_id: String) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.llama
            .send(LLMTaskRequest::Remove(LLMTaskRemoveRequest {
                task_id,
                result_sender: tx,
            }))?;
        rx.await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::graphql::graphql_types::AIPromptExamplesInput;

    use super::*;

    #[tokio::test]
    async fn test_embedding() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().await.expect("initialization to work");
        let vector = service
            .embed("Test string".into())
            .await
            .expect("embed to return a result");
        assert!(vector.len() > 300)
    }

    #[tokio::test]
    async fn test_prompt() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().await.expect("initialization to work");

        let task = service.add_task(AITaskInput {
                name: "Test task".into(),
                model_id: "Llama tiny 1b".into(),
                system_prompt: "You are inside a test for tasks. Please make sure to create any non-zero length output".into(),
                prompt_examples: vec![AIPromptExamplesInput{
                    input: "Test string".into(),
                    output: "Yes, I'm working!".into()
                }],
                meta_data: None
            }).await.expect("add_task to work without error");

        let response = service
            .prompt(task.task_id, "Test string".into())
            .await
            .expect("prompt to return a result");
        println!("Response: {}", response);
        assert!(response.len() > 0)
    }

    #[ignore]
    #[tokio::test]
    async fn test_prompt_stress() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().await.expect("initialization to work");

        let task = service.add_task(AITaskInput {
                name: "Test task".into(),
                model_id: "Llama tiny 1b".into(),
                system_prompt: "You are inside a test for tasks. Please make sure to create any non-zero length output".into(),
                meta_data: None,
                prompt_examples: vec![AIPromptExamplesInput{
                    input: "Test string".into(),
                    output: "Yes, I'm working!".into()
                },
                AIPromptExamplesInput{
                    input: "What's up?".into(),
                    output: "Nothing, I'm working!".into()
                },
                AIPromptExamplesInput{
                    input: "Is this a test".into(),
                    output: "Yes, it's working!".into()
                },
                AIPromptExamplesInput{
                    input: "Test string loong".into(),
                    output: "Yes, I'm working! This is a longer response to test the system's ability to handle more extensive outputs. It's important to ensure that the AI can generate and process longer strings of text, as real-world applications often require more detailed and nuanced responses. By including this extended output, we're stress-testing the system's capacity and verifying its robustness in handling varied lengths of text.".into()
                },
                AIPromptExamplesInput{
                    input: "Test string super loong".into(),
                    output: "Yes, I'm working! This is an extremely long response to thoroughly test the system's capacity for handling extensive outputs. In real-world scenarios, AI models often need to generate lengthy and detailed responses to complex queries. This test string is designed to push the limits of our system, ensuring it can process and store large amounts of text without issues. It's crucial to verify that our AI service can maintain coherence and relevance even in prolonged responses. By including various sentence structures, punctuation, and a mix of short and long phrases, we're also testing the linguistic versatility of our model. Furthermore, this extended output allows us to assess the performance impact of processing large text blocks, which is essential for optimizing our system's efficiency. It's worth noting that in practical applications, responses of this length might be common in scenarios such as content generation, detailed explanations, or comprehensive analyses. Therefore, ensuring our system can handle such verbose outputs is paramount for its real-world applicability and robustness.".into()
                }],
        }).await.expect("add_task to work without error");

        let futures = (0..10)
            .map(|_| service.prompt(task.task_id.clone(), "Test string".into()))
            .collect::<Vec<_>>();

        let responses = futures::future::join_all(futures)
            .await
            .into_iter()
            .collect::<Result<Vec<_>>>()
            .expect("all prompts to return results");

        let response = responses.join("\n");
        println!("Responses: {}", response);
        assert!(response.len() > 0)
    }
}
