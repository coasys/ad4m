use std::error::Error;
use tokio::sync::Mutex;
use std::sync::Arc;
use std::fmt;
use deno_core::error::AnyError;
use kalosm::language::*;
use crate::db::Ad4mDb;
use crate::types::AITask;

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
}

impl AIService {
    pub async fn new() -> Result<Self> {
        Ok(AIService {
            bert: Bert::builder().build().await?,
        })
    } 

    pub async fn init_global_instance() -> Result<()> {
        let mut ai_service = AI_SERVICE.lock().await;
        *ai_service = Some(AIService::new().await?);
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
        let ai_service_ref = lock_result.as_ref().expect("Couldn't get lock on AIService");
        //let ai_service_ref = ai_service_lock.as_ref().expect("AIService not initialized");
        f(ai_service_ref)
    }

    pub async fn with_mutable_global_instance<F>(f: F)
    where
        F: FnOnce(&mut AIService),
    {
        let global_instance_arc = AIService::global_instance();
        let mut lock_result = global_instance_arc.lock().await;
        let ai_service_mut = lock_result.as_mut().expect("Couldn't get lock on AIService");
        //let ai_service_mut = ai_service_lock.as_mut().expect("AIService not initialized");
        f(ai_service_mut)
    }

    pub fn add_task(task: AITask) -> Result<AITask> {
        let task_id = Ad4mDb::with_global_instance(|db| db.add_task(task.model_id, task.system_prompt, task.prompt_examples))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        let retrieved_task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;

        Ok(retrieved_task)
    }

    pub fn delete_task(task_id: String) -> Result<()> {
        Ad4mDb::with_global_instance(|db| db.remove_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(())
    }

    pub fn get_tasks() -> Result<Vec<AITask>> {
        let tasks = Ad4mDb::with_global_instance(|db| db.get_tasks())
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;
        Ok(tasks)
    }

    pub fn update_task(task: AITask) -> Result<AITask> {
        let task_id = task.id.clone();
        Ad4mDb::with_global_instance(|db| db.update_task(task.id, task.model_id, task.system_prompt, task.prompt_examples))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?;

        let updated_task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;

        Ok(updated_task)
    }
    pub fn prompt(task_id: String, prompt: String) -> Result<()> {
        let task = Ad4mDb::with_global_instance(|db| db.get_task(task_id))
            .map_err(|e| AIServiceError::DatabaseError(e.to_string()))?
            .ok_or(AIServiceError::TaskNotFound)?;


        // Run AI model with prompt

        Ok(())
    }

    pub async fn embed(text: String) -> Result<Vec<f32>> {
        let global_instance_arc = AIService::global_instance();
        let lock_result = global_instance_arc.lock().await;
        let ai_service_ref = lock_result.as_ref().expect("Couldn't get lock on AIService");
        
        let embedding = ai_service_ref.bert.embed(text).await?;
        Ok(embedding.to_vec())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_embedding() {
        AIService::init_global_instance().await.expect("initialization to work");
        let vector = AIService::embed("Test string".into()).await.expect("embed to return a result");
        assert!(vector.len() > 300)
    }
}