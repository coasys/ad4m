use std::sync::{Arc, Mutex};
use std::fmt;
use crate::db::Ad4mDb;
use crate::types::AITask;

#[derive(Debug)]
pub enum AIServiceError {
    DatabaseError(String),
    TaskNotFound,
    ServiceNotInitialized,
    LockError,
}

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

pub type Result<T> = std::result::Result<T, AIServiceError>;


lazy_static! {
    static ref AI_SERVICE: Arc<Mutex<Option<AIService>>> = Arc::new(Mutex::new(None));
}

pub struct AIService {}

impl AIService {
    pub fn init_global_instance() {
        let mut ai_service = AI_SERVICE.lock().unwrap();
        *ai_service = Some(AIService::new());
    }

    pub fn new() -> Self {
        Self {}
    }

    pub fn global_instance() -> Arc<Mutex<Option<AIService>>> {
        AI_SERVICE.clone()
    }

    pub fn with_global_instance<F, R>(f: F) -> R
    where
        F: FnOnce(&AIService) -> R,
    {
        let global_instance_arc = AIService::global_instance();
        let lock_result = global_instance_arc.lock();
        let ai_service_lock = lock_result.expect("Couldn't get lock on AIService");
        let ai_service_ref = ai_service_lock.as_ref().expect("AIService not initialized");
        f(ai_service_ref)
    }

    pub fn with_mutable_global_instance<F>(f: F)
    where
        F: FnOnce(&mut AIService),
    {
        let global_instance_arc = AIService::global_instance();
        let lock_result = global_instance_arc.lock();
        let mut ai_service_lock = lock_result.expect("Couldn't get lock on AIService");
        let ai_service_mut = ai_service_lock.as_mut().expect("AIService not initialized");
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
}
