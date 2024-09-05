use crate::db::Ad4mDb;
use crate::graphql::graphql_types::AITaskInput;
use crate::types::AITask;
use deno_core::error::AnyError;
use kalosm::language::*;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::sync::Arc;
use tokio::sync::Mutex;


#[derive(Debug)]
pub enum AIServiceError {
    DatabaseError(String),
    TaskNotFound,
    ServiceNotInitialized,
    LockError,
}

impl Error for AIServiceError {}

impl fmt::Display for AIServiceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AIServiceError::DatabaseError(msg) => write!(f, "Database error: {}", msg),
            AIServiceError::TaskNotFound => write!(f, "Task not found"),
            AIServiceError::ServiceNotInitialized => write!(f, "Service not initialized"),
            AIServiceError::LockError => write!(f, "Lock error"),
        }
    }
}

pub type Result<T> = std::result::Result<T, AnyError>;

lazy_static! {
    static ref AI_SERVICE: Arc<Mutex<Option<AIService>>> = Arc::new(Mutex::new(None));
}

pub struct AIService {
    bert: Bert,
    llama: Llama,
    tasks: HashMap<String, Task>,
}

impl AIService {
    pub async fn new() -> Result<Self> {
        let llama = Llama::builder().build().await?;

        let mut mapped_tasks: HashMap<String, Task> = HashMap::new();

        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        for task in tasks {
            let task_id = task.task_id.clone();
            let _model_id = task.model_id.clone();
            let _system_prompt = task.system_prompt.clone();
            let prompt_examples = task.prompt_examples.clone();
            let task = Task::builder(task.system_prompt)
                .with_examples(prompt_examples.clone().into_iter().map(|example| {
                    (example.input.into(), example.output.into())
                }).collect::<Vec<(String, String)>>())
                .build();

            let exmaple_prompt_result = task.run("Test example prompt".to_string(), &llama);

            log::info!("Example prompt result: {:?}", exmaple_prompt_result);

            mapped_tasks.insert(task_id, task);
        }

        Ok(AIService {
            bert: Bert::builder().build().await?,
            llama,
            tasks: mapped_tasks,
        })
    }

    pub async fn init_global_instance() -> Result<()> {
        let new_service = AIService::new().await?;
        let mut ai_service = AI_SERVICE.lock().await;
        *ai_service = Some(new_service);
        Ok(())
    }

    pub fn global_instance() -> Arc<Mutex<Option<AIService>>> {
        AI_SERVICE.clone()
    }

    pub async fn with_global_instance<F, R>(f: F) -> R
    where
        F: FnOnce(&AIService) -> R,
    {
        let global_instance_arc = AIService::global_instance();
        let lock_result = global_instance_arc.lock().await;
        let ai_service_ref = lock_result.as_ref().expect("AI service not initialized");
        //let ai_service_ref = ai_service_lock.as_ref().expect("AIService not initialized");
        f(ai_service_ref)
    }

    pub async fn with_mutable_global_instance<F, R>(f: F) -> R
    where
        F: FnOnce(&mut AIService) -> R,
    {
        let global_instance_arc = AIService::global_instance();
        let mut lock_result = global_instance_arc.lock().await;
        let ai_service_mut = lock_result.as_mut().expect("AI service not initialized");
        //let ai_service_mut = ai_service_lock.as_mut().expect("AIService not initialized");
        f(ai_service_mut)
    }

    pub fn add_task(&mut self, task: AITaskInput) -> Result<AITask> {
        let task_id = Ad4mDb::with_global_instance(|db| {
            db.add_task(task.model_id, task.system_prompt, task.prompt_examples.into_iter().map(|p|p.into()).collect())
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        let retrieved_task = Ad4mDb::with_global_instance(|db| db.get_task(task_id.clone()))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;

        let task = Task::builder(retrieved_task.system_prompt.clone())
            .with_examples(
                retrieved_task.prompt_examples.clone().into_iter().map(|example| {
                    (example.input.into(), example.output.into())
                }).collect::<Vec<(String, String)>>()
            )
            .build();

        let exmaple_prompt_result = task.run("Test example prompt".to_string(), &self.llama);

        log::info!("Example prompt result: {:?}", exmaple_prompt_result);

        self.tasks.insert(task_id, task);

        Ok(retrieved_task)
    }

    pub fn delete_task(&mut self, task_id: String) -> Result<bool> {
        Ad4mDb::with_global_instance(|db| db.remove_task(task_id.clone()))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        self.tasks.remove(&task_id);

        Ok(true)
    }

    pub fn get_tasks() -> Result<Vec<AITask>> {
        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(tasks)
    }

    pub fn update_task(&mut self, task: AITask) -> Result<AITask> {
        let task_id = task.task_id.clone();
        Ad4mDb::with_global_instance(|db| {
            db.update_task(
                task.task_id,
                task.model_id,
                task.system_prompt,
                task.prompt_examples,
            )
        })
        .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        let updated_task = Ad4mDb::with_global_instance(|db| db.get_task(task_id.clone()))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;

        let task = Task::builder(updated_task.system_prompt.clone())
            .with_examples(
                updated_task.prompt_examples.clone().into_iter().map(|example| {
                    (example.input.into(), example.output.into())
                }).collect::<Vec<(String, String)>>()
            )
            .build();

        let exmaple_prompt_result = task.run("Test example prompt".to_string(), &self.llama);

        log::info!("Example prompt result: {:?}", exmaple_prompt_result);

        self.tasks.insert(task_id, task);

        Ok(updated_task)
    }

    pub async fn prompt(&self, task_id: String, prompt: String) -> Result<String> {
        let task = self.tasks.get(&task_id);

        if task.is_none() {
            return Err(AIServiceError::TaskNotFound.into());
        }

        // Run AI model with prompt
        let result: String = task.unwrap().run(prompt, &self.llama).all_text().await;

        Ok(result)
    }

    pub async fn embed(text: String) -> Result<Vec<f32>> {
        let global_instance_arc = AIService::global_instance();
        let lock_result = global_instance_arc.lock().await;
        let ai_service_ref = lock_result.as_ref().expect("AI service not initialized");

        let embedding = ai_service_ref.bert.embed(text).await?;
        Ok(embedding.to_vec())
    }
}

#[cfg(test)]
mod tests {
    use crate::types::AIPromptExamples;

    use super::*;

    #[tokio::test]
    async fn test_embedding() {
        AIService::init_global_instance()
            .await
            .expect("initialization to work");
        let vector = AIService::embed("Test string".into())
            .await
            .expect("embed to return a result");
        assert!(vector.len() > 300)
    }

    // #[tokio::test]
    // async fn test_prompt() {
    //     AIService::init_global_instance()
    //         .await
    //         .expect("initialization to work");

    //     AIService::add_task(AITask {
    //         task_id: "test".into(),
    //         model_id: "gpt-3".into(),
    //         system_prompt: "Test system prompt".into(),
    //         prompt_examples: vec![AIPromptExamples{
    //             prompt: "Test prompt".into(),
    //             response: "Test response".into()
    //         }],
    //     });

    //     let prompt = AIService::prompt("test".into(), "Test string".into())
    //         .await
    //         .expect("prompt to return a result");
    //     assert!(prompt.len() > 0)
    // }
}
