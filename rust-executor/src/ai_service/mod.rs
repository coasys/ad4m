use self::{audio_stream::AudioStream, error::AIServiceError};
#[allow(unused_imports)]
use crate::graphql::graphql_types::{AIModelLoadingStatus, AITaskInput, TranscriptionTextFilter};
use crate::pubsub::AI_MODEL_LOADING_STATUS;
#[allow(unused_imports)]
use crate::pubsub::AI_TRANSCRIPTION_TEXT_TOPIC;
use crate::types::{AITask, ModelType};
use crate::{db::Ad4mDb, pubsub::get_global_pubsub};
use anyhow::anyhow;
use deno_core::error::AnyError;
use futures::SinkExt;
use kalosm::sound::TextStream;
use kalosm::sound::*;
// use kalosm::sound::{DenoisedExt, VoiceActivityDetectorExt, VoiceActivityStreamExt};
use kalosm::language::*;
// use kalosm::language::Gpt3_5;
// use kalosm_common::Cache;
// use rodio::{OutputStream, Source};
use tokio::time::sleep;
// use rodio::source::Source;
use std::collections::HashMap;
// use std::io::Cursor;
use std::future::{Future, IntoFuture};
use std::panic::catch_unwind;
use std::pin::Pin;
// use std::path::PathBuf;
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot, Mutex};

mod audio_stream;
mod error;
use log::error;

pub type Result<T> = std::result::Result<T, AnyError>;

lazy_static! {
    static ref AI_SERVICE: Arc<Mutex<Option<AIService>>> = Arc::new(Mutex::new(None));
}

struct TranscriptionSession {
    samples_tx: futures_channel::mpsc::UnboundedSender<Vec<f32>>,
    drop_tx: oneshot::Sender<()>,
}

#[derive(Clone)]
pub struct AIService {
    embedding_channel: Arc<Mutex<HashMap<String, mpsc::UnboundedSender<EmbeddingRequest>>>>,
    llm_channel: Arc<Mutex<HashMap<String, mpsc::UnboundedSender<LLMTaskRequest>>>>,
    transcription_streams: Arc<Mutex<HashMap<String, TranscriptionSession>>>,
}

struct EmbeddingRequest {
    pub prompt: String,
    pub result_sender: oneshot::Sender<Result<Vec<f32>>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct LLMTaskSpawnRequest {
    pub task: AITask,
    pub result_sender: oneshot::Sender<Result<()>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct LLMTaskPromptRequest {
    pub task_id: String,
    pub prompt: String,
    pub result_sender: oneshot::Sender<Result<String>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct LLMTaskRemoveRequest {
    pub task_id: String,
    pub result_sender: oneshot::Sender<()>,
}

#[allow(dead_code)]
#[derive(Debug)]
enum LLMTaskRequest {
    Spawn(LLMTaskSpawnRequest),
    Prompt(LLMTaskPromptRequest),
    Remove(LLMTaskRemoveRequest),
}

enum LlmModel {
    Local(Llama),
    Remote(Gpt4),
}

async fn publish_model_status(model_name: String, progress: f32, status: &str, downloaded: bool) {
    let model = AIModelLoadingStatus {
        model: model_name.clone(),
        progress: progress as f64,
        status: status.to_string(),
        downloaded: if downloaded { progress == 100.0 } else { true },
        loaded: if !downloaded {
            progress == 100.0
        } else {
            false
        },
    };

    let _ = Ad4mDb::with_global_instance(|db| {
        let model = model.clone();
        db.create_or_update_model_status(
            &model.model,
            model.progress,
            &model.status,
            model.downloaded,
            model.loaded,
        )
    });

    get_global_pubsub()
        .await
        .publish(
            &AI_MODEL_LOADING_STATUS,
            &serde_json::to_string(&model).expect("AIModelLoading must be serializable"),
        )
        .await;
}

async fn handle_progress(model_name: String, progress: ModelLoadingProgress) {
    match progress {
        ModelLoadingProgress::Downloading {
            source: _,
            start_time,
            progress,
        } => {
            let progress = progress * 100.0;
            let _elapsed = start_time.elapsed().as_secs_f32();

            let status = if progress < 100.0 {
                "Downloading".to_string()
            } else {
                "Downloaded".to_string()
            };

            publish_model_status(model_name.clone(), progress, &status, true).await;
        }
        ModelLoadingProgress::Loading { progress } => {
            let progress = progress * 100.0;

            let status = if progress < 100.0 {
                "Loading".to_string()
            } else {
                "Loaded".to_string()
            };

            publish_model_status(model_name.clone(), progress, &status, false).await;
        }
    }
}

impl AIService {
    pub fn new() -> Result<Self> {
        let service = AIService {
            embedding_channel: Arc::new(Mutex::new(HashMap::new())),
            llm_channel: Arc::new(Mutex::new(HashMap::new())),
            transcription_streams: Arc::new(Mutex::new(HashMap::new())),
        };

        let clone = service.clone();
        tokio::spawn(async move {
            if let Err(e) = clone.load().await {
                error!("AIService error while loading models: {:?}", e);
            }
        });

        Ok(service)
    }

    pub async fn load(&self) -> Result<()> {
        // Get all models
        let models = Ad4mDb::with_global_instance(|db| db.get_models())?;

        // Create a future for each, initialzing that model
        let mut futures: Vec<Pin<Box<dyn Future<Output = ()> + Send>>> = vec![];
        for model in models.into_iter() {
            futures.push(Box::pin(async move {
                if let Err(e) = self.init_model(model.clone()).await {
                    error!("Error initializing model[{:?}]: {:?}", model, e);
                }
            }));
        }

        // Wait for all initialization futures in parallel
        futures::future::join_all(futures).await;

        // Spawn tasks from the database
        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        for task in tasks {
            self.spawn_task(task).await?;
        }

        Ok(())
    }

    async fn load_transcriber_model(model: &crate::types::Model) {
        let name = model.name.clone();
        publish_model_status(name.clone(), 0.0, "Loading", false).await;

        let _ = WhisperBuilder::default()
            .with_source(WhisperSource::Base)
            .build_with_loading_handler({
                let name = name.clone();
                move |progress| {
                    tokio::spawn(handle_progress(name.clone(), progress));
                }
            })
            .await;

        publish_model_status(name, 100.0, "Loaded", false).await;
    }

    async fn init_model(&self, model: crate::types::Model) -> Result<()> {
        match model.model_type {
            ModelType::Llm => self.spawn_llm_model(model).await?,
            ModelType::Embedding => self.spawn_embedding_model(model).await,
            ModelType::Transcription => Self::load_transcriber_model(&model).await,
        };
        Ok(())
    }

    pub async fn add_model(&self, model: crate::types::Model) -> Result<()> {
        self.init_model(model.clone()).await?;
        Ad4mDb::with_global_instance(|db| db.add_model(&model))
            .map_err(|e| anyhow::anyhow!("{}", e))?;
        Ok(())
    }

    pub async fn model_status(model_id: String) -> Result<AIModelLoadingStatus> {
        let status = Ad4mDb::with_global_instance(|db| db.get_model_status(&model_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::ModelNotFound)?;

        Ok(status)
    }

    pub async fn init_global_instance() -> Result<()> {
        let new_service = AIService::new()?;
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

    async fn spawn_embedding_model(&self, model_config: crate::types::Model) {
        let (bert_tx, mut bert_rx) = mpsc::unbounded_channel::<EmbeddingRequest>();
        let model_id = model_config.name.clone();
        thread::spawn({
            let model_id = model_config.name.clone();
            move || {
                let rt = tokio::runtime::Runtime::new().unwrap();

                let model = rt
                    .block_on(async {
                        publish_model_status(model_id.clone(), 0.0, "Loading", false).await;

                        let berd = Bert::builder()
                            .build_with_loading_handler({
                                let model_id = model_id.clone();
                                move |progress| {
                                    tokio::spawn(handle_progress(model_id.clone(), progress));
                                }
                            })
                            .await;

                        publish_model_status(model_id.clone(), 100.0, "Loaded", false).await;

                        berd
                    })
                    .expect("couldn't build Bert model");

                while let Some(request) = rt.block_on(bert_rx.recv()) {
                    let result: Result<Vec<f32>> = rt
                        .block_on(async { model.embed(request.prompt).await })
                        .map(|tensor| tensor.to_vec());
                    let _ = request.result_sender.send(result);
                }
            }
        });

        self.embedding_channel
            .lock()
            .await
            .insert(model_id, bert_tx);
    }

    async fn build_local_llama_from_string(
        model_name: String,
        model_size_string: String,
    ) -> Result<Llama> {
        publish_model_status(model_name.clone(), 0.0, "Loading", false).await;

        let llama = match model_size_string.as_str() {
            // Local TinyLlama models
            "llama_7b" => Llama::builder().with_source(LlamaSource::llama_7b()),
            "llama_8b" => Llama::builder().with_source(LlamaSource::llama_8b()),
            "llama_13b" => Llama::builder().with_source(LlamaSource::llama_13b()),
            "llama_70b" => Llama::builder().with_source(LlamaSource::llama_70b()),
            // Handle unknown models
            _ => {
                log::error!("Unknown model_id: {}", model_name);
                return Err(anyhow::anyhow!("Unknown model_id: {}", model_name));
            }
        };

        // Build the local Llama model
        let llama = llama
            .build_with_loading_handler({
                let model_id = model_name.clone();
                move |progress| {
                    tokio::spawn(handle_progress(model_id.clone(), progress));
                }
            })
            .await?;

        publish_model_status(model_name.clone(), 100.0, "Loaded", false).await;

        Ok(llama)
    }

    async fn build_remote_gpt4(model_id: String, api_key: String, base_url: Url) -> Gpt4 {
        publish_model_status(model_id.clone(), 0.0, "Loading", false).await;

        // Build Gpt3_5 using the external API endpoint
        let gpt4 = Gpt4::builder()
            .with_base_url(base_url.as_str())
            .with_api_key(&api_key)
            .build();

        publish_model_status(model_id.clone(), 100.0, "Loading", false).await;

        gpt4
    }

    async fn spawn_llm_model(&self, model_config: crate::types::Model) -> Result<()> {
        if model_config.local.is_none() && model_config.api.is_none() {
            return Err(anyhow!(
                "AI model definition {} doesn't have a body, nothing to spawn!",
                model_config.name
            ));
        }

        let (llama_tx, mut llama_rx) = mpsc::unbounded_channel::<LLMTaskRequest>();
        let model_id = model_config.name.clone();
        thread::spawn({
            move || {
                let model_id = model_config.name.clone();
                let rt = tokio::runtime::Runtime::new().unwrap();
                let maybe_model = rt
                    .block_on(async {
                        if let Some(local_model) = model_config.local {
                            Self::build_local_llama_from_string(model_id, local_model.file_name)
                                .await
                                .map(LlmModel::Local)
                        } else if let Some(api) = model_config.api {
                            Ok(LlmModel::Remote(Self::build_remote_gpt4(model_id, api.api_key, api.base_url).await))
                        } else {
                            Err(anyhow!("AI model definition {} doesn't have a body, and this error should have been caught above", model_config.name))
                        }
                    });

                let mut model = match maybe_model {
                    Ok(m) => m,
                    Err(e) => {
                        error!("Failed to build LLM model: {}", e);
                        return;
                    }
                };

                let mut tasks = HashMap::<String, Task>::new();
                let mut task_descriptions = HashMap::<String, AITask>::new();

                while let Some(task_request) = rt.block_on(llama_rx.recv()) {
                    match task_request {
                        LLMTaskRequest::Spawn(spawn_request) => match model {
                            LlmModel::Remote(_) => {
                                task_descriptions
                                    .insert(spawn_request.task.task_id.clone(), spawn_request.task);
                            }
                            LlmModel::Local(ref mut llama) => {
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
                                            rt.block_on(task.run("Test example prompt", llama).all_text())
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
                        },

                        LLMTaskRequest::Prompt(prompt_request) => match model {
                            LlmModel::Remote(ref mut gpt) => {
                                if let Some(task) = task_descriptions.get(&prompt_request.task_id) {
                                    let mut lines =
                                        vec![format!("You are: {}", task.system_prompt)];
                                    for example in task.prompt_examples.iter() {
                                        lines.push(format!("Input: {}", example.input));
                                        lines.push(format!("Output: {}", example.output));
                                    }
                                    lines.push(format!("Input: {}", prompt_request.prompt));
                                    lines.push("Output:".to_string());

                                    let prompt = lines.join("\n");
                                    match rt.block_on(
                                        gpt.stream_text(&prompt)
                                            .with_max_length(1000)
                                            .into_future(),
                                    ) {
                                        Err(e) => {
                                            let _ =
                                                prompt_request.result_sender.send(Err(anyhow!(
                                                    "Error connecting to remote LLM API: {:?}",
                                                    e
                                                )));
                                        }
                                        Ok(mut stream) => {
                                            let response = rt.block_on(stream.all_text());
                                            let _ = prompt_request.result_sender.send(Ok(response));
                                        }
                                    }
                                } else {
                                    let _ = prompt_request.result_sender.send(Err(anyhow!(
                                        "Task with ID {} not spawned",
                                        prompt_request.task_id
                                    )));
                                }
                            }
                            LlmModel::Local(ref mut llama) => {
                                if let Some(task) = tasks.get(&prompt_request.task_id) {
                                    let mut maybe_result: Option<String> = None;
                                    let mut tries = 0;
                                    while maybe_result.is_none() && tries < 20 {
                                        tries += 1;

                                        match catch_unwind(|| {
                                            rt.block_on(async {
                                                task.run(prompt_request.prompt.clone(), llama)
                                                    .all_text()
                                                    .await
                                            })
                                        }) {
                                            Err(e) => {
                                                log::error!(
                                                    "Llama panicked with: {:?}. Trying again..",
                                                    e
                                                )
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
                        },

                        LLMTaskRequest::Remove(remove_request) => {
                            let _ = tasks.remove(&remove_request.task_id);
                            let _ = task_descriptions.remove(&remove_request.task_id);
                            let _ = remove_request.result_sender.send(());
                        }
                    }
                }
            }
        });

        self.llm_channel.lock().await.insert(model_id, llama_tx);
        Ok(())
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

        // Retrieve the task to find the associated model_id
        let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id.clone()))
            .map_err(|e| anyhow::anyhow!("Database error: {}", e))?
            .ok_or_else(|| anyhow::anyhow!("Task not found for task_id: {}", task_id))?;

        let model_id = task.model_id;

        let llm_channel = self.llm_channel.lock().await;
        if let Some(sender) = llm_channel.get(&model_id) {
            sender.send(LLMTaskRequest::Prompt(LLMTaskPromptRequest {
                task_id,
                prompt,
                result_sender,
            }))?;
        } else {
            return Err(anyhow::anyhow!(
                "Model '{}' not found in LLM channel",
                model_id
            ));
        }

        rx.await?
    }

    pub async fn embed(&self, text: String) -> Result<Vec<f32>> {
        let (result_sender, rx) = oneshot::channel();

        let embedding_channel = self.embedding_channel.lock().await;
        if let Some(sender) = embedding_channel.get("bert") {
            sender.send(EmbeddingRequest {
                prompt: text,
                result_sender,
            })?;
        } else {
            return Err(anyhow::anyhow!(
                "Model 'bert' not found in embedding channel"
            ));
        }

        rx.await?
    }

    async fn spawn_task(&self, task: AITask) -> Result<()> {
        let (tx, rx) = oneshot::channel();

        let llm_channel = self.llm_channel.lock().await;
        if let Some(sender) = llm_channel.get(&task.model_id) {
            sender.send(LLMTaskRequest::Spawn(LLMTaskSpawnRequest {
                task: task.clone(),
                result_sender: tx,
            }))?;
        } else {
            return Err(anyhow::anyhow!(
                "Model '{}' not found in LLM channel",
                task.model_id
            ));
        }

        rx.await?
    }

    async fn remove_task(&self, task_id: String) -> Result<()> {
        let (tx, rx) = oneshot::channel();

        let llm_channel = self.llm_channel.lock().await;

        if let Some(sender) = llm_channel.get("llama") {
            sender.send(LLMTaskRequest::Remove(LLMTaskRemoveRequest {
                task_id,
                result_sender: tx,
            }))?;
        } else {
            return Err(anyhow::anyhow!("Model 'llama' not found in LLM channel"));
        }

        rx.await?;
        Ok(())
    }

    pub async fn open_transcription_stream(&self, _model_id: String) -> Result<String> {
        let stream_id = uuid::Uuid::new_v4().to_string();
        let stream_id_clone = stream_id.clone();
        let (samples_tx, samples_rx) = futures_channel::mpsc::unbounded::<Vec<f32>>();
        //TODO: use drop_rx to exit thread
        let (drop_tx, drop_rx) = oneshot::channel();
        let (done_tx, done_rx) = oneshot::channel();

        thread::spawn(move || {
            let rt = tokio::runtime::Runtime::new().unwrap();

            rt.block_on(async {
                let maybe_model = WhisperBuilder::default()
                    .with_source(WhisperSource::Base)
                    .build()
                    .await;

                if let Ok(whisper) = maybe_model {
                    let audio_stream = AudioStream {
                        read_data: Vec::new(),
                        receiver: Box::pin(samples_rx.map(futures_util::stream::iter).flatten()),
                    };

                    let mut word_stream = audio_stream
                        .voice_activity_stream()
                        .rechunk_voice_activity()
                        .with_end_window(Duration::from_millis(500))
                        .transcribe(whisper);

                    let _ = done_tx.send(Ok(()));

                    tokio::select! {
                        _ = drop_rx => {},
                        _ = async {
                            while let Some(segment) = word_stream.next().await {
                                //println!("GOT segment: {}", segment.text());
                                let stream_id_clone = stream_id_clone.clone();

                                rt.spawn(async move {
                                    let _ = get_global_pubsub()
                                        .await
                                        .publish(
                                            &AI_TRANSCRIPTION_TEXT_TOPIC,
                                            &serde_json::to_string(&TranscriptionTextFilter {
                                                stream_id: stream_id_clone.clone(),
                                                text: segment.text().to_string(),
                                            })
                                            .expect("TranscriptionTextFilter must be serializable"),
                                        )
                                        .await;
                                });

                                sleep(Duration::from_millis(50)).await;
                            }
                        } => {}
                    }
                } else {
                    let _ = done_tx.send(Err(maybe_model.err().unwrap()));
                }
            });
        });

        done_rx.await??;

        self.transcription_streams.lock().await.insert(
            stream_id.clone(),
            TranscriptionSession {
                samples_tx,
                drop_tx,
            },
        );

        Ok(stream_id)
    }

    pub async fn feed_transcription_stream(
        &self,
        stream_id: &String,
        audio_samples: Vec<f32>,
    ) -> Result<()> {
        let mut map_lock = self.transcription_streams.lock().await;
        let maybe_stream = map_lock.get_mut(stream_id);
        if let Some(stream) = maybe_stream {
            stream.samples_tx.send(audio_samples).await?;
            Ok(())
        } else {
            Err(AIServiceError::StreamNotFound.into())
        }
    }

    pub async fn close_transcription_stream(&self, stream_id: &String) -> Result<()> {
        let mut map_lock = self.transcription_streams.lock().await;

        if let Some(stream) = map_lock.remove(stream_id) {
            stream.drop_tx.send(()).map_err(|_| {
                anyhow!(AIServiceError::CrazyError(format!(
                    "Failed to close stream {}: Whisper thread may have crashed",
                    stream_id
                )))
            })
        } else {
            Err(AIServiceError::StreamNotFound.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::graphql::graphql_types::AIPromptExamplesInput;

    use super::*;

    #[ignore]
    #[tokio::test]
    async fn test_embedding() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().expect("initialization to work");
        let vector = service
            .embed("Test string".into())
            .await
            .expect("embed to return a result");
        assert!(vector.len() > 300)
    }

    #[ignore]
    #[tokio::test]
    async fn test_prompt() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().expect("initialization to work");

        let task = service.add_task(AITaskInput {
                name: "Test task".into(),
                model_id: "llama".into(),
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
        assert!(!response.is_empty())
    }

    #[ignore]
    #[tokio::test]
    async fn test_prompt_stress() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let service = AIService::new().expect("initialization to work");

        let task = service.add_task(AITaskInput {
                name: "Test task".into(),
                model_id: "llama".into(),
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
        assert!(!response.is_empty())
    }
}
