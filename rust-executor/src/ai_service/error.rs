use std::error::Error;
use std::fmt;

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