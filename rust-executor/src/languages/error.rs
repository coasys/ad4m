use std::fmt;

/// Errors that can occur during language operations
#[derive(Debug, Clone)]
pub enum LanguageError {
    /// Failed to load a language bundle
    LoadError { address: String, message: String },

    /// Runtime execution error in a language
    RuntimeError { address: String, message: String },

    /// Callback registration or invocation error
    CallbackError { address: String, message: String },

    /// Language not found
    NotFound { address: String },

    /// Invalid language bundle
    InvalidBundle { message: String },

    /// IO error (file system operations)
    IoError { message: String },

    /// Serialization/deserialization error
    SerializationError { message: String },

    /// Bootstrap module execution failed for a language runtime
    BootstrapFailed { address: String, message: String },

    /// V8 event loop panic/error in a language runtime
    EventLoopPanic { address: String, message: String },
}

impl fmt::Display for LanguageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LanguageError::LoadError { address, message } => {
                write!(f, "Failed to load language {}: {}", address, message)
            }
            LanguageError::RuntimeError { address, message } => {
                write!(f, "Runtime error in language {}: {}", address, message)
            }
            LanguageError::CallbackError { address, message } => {
                write!(f, "Callback error in language {}: {}", address, message)
            }
            LanguageError::NotFound { address } => {
                write!(f, "Language not found: {}", address)
            }
            LanguageError::InvalidBundle { message } => {
                write!(f, "Invalid language bundle: {}", message)
            }
            LanguageError::IoError { message } => {
                write!(f, "IO error: {}", message)
            }
            LanguageError::SerializationError { message } => {
                write!(f, "Serialization error: {}", message)
            }
            LanguageError::BootstrapFailed { address, message } => {
                write!(f, "Bootstrap failed for language {}: {}", address, message)
            }
            LanguageError::EventLoopPanic { address, message } => {
                write!(f, "Event loop panic in language {}: {}", address, message)
            }
        }
    }
}

impl std::error::Error for LanguageError {}

impl From<std::io::Error> for LanguageError {
    fn from(err: std::io::Error) -> Self {
        LanguageError::IoError {
            message: err.to_string(),
        }
    }
}

impl From<serde_json::Error> for LanguageError {
    fn from(err: serde_json::Error) -> Self {
        LanguageError::SerializationError {
            message: err.to_string(),
        }
    }
}
