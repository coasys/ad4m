use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum AIServiceError {
    DatabaseError(String),
    TaskNotFound,
    StreamNotFound,
    ServiceNotInitialized,
    #[allow(dead_code)]
    LockError,
    CrazyError(String),
    /// Eight utterances already wait on this stream. This audio was not queued.
    UtteranceQueueFull,
    /// The worker is gone, so the stream accepted nothing more.
    TranscriptionStreamClosed,
    /// The audio is past the shared 10 MiB float32 ceiling.
    UtteranceTooLarge {
        samples: usize,
        limit: usize,
    },
}

impl AIServiceError {
    /// Whether `err` is the typed queue-full refusal.
    pub(crate) fn is_utterance_queue_full(err: &anyhow::Error) -> bool {
        matches!(
            err.downcast_ref::<AIServiceError>(),
            Some(AIServiceError::UtteranceQueueFull)
        )
    }
}

impl Error for AIServiceError {}

impl fmt::Display for AIServiceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AIServiceError::DatabaseError(msg) => write!(f, "Database error: {}", msg),
            AIServiceError::TaskNotFound => write!(f, "Task not found"),
            AIServiceError::ServiceNotInitialized => write!(f, "Service not initialized"),
            AIServiceError::LockError => write!(f, "Lock error"),
            AIServiceError::StreamNotFound => write!(f, "Transcription stream not found"),
            AIServiceError::CrazyError(msg) => write!(f, "Something crazy happened: {}", msg),
            AIServiceError::UtteranceQueueFull => write!(
                f,
                "The utterance queue of this stream is full. This audio was not accepted."
            ),
            AIServiceError::TranscriptionStreamClosed => write!(
                f,
                "The transcription stream closed before it accepted this audio."
            ),
            AIServiceError::UtteranceTooLarge { samples, limit } => write!(
                f,
                "This audio has {samples} samples. The limit is {limit} samples. It was not accepted."
            ),
        }
    }
}
