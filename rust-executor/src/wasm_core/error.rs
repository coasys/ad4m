//! Error types for the WASM language runtime.

use std::fmt;

/// Errors that can occur during WASM language loading and execution.
#[derive(Debug)]
pub enum WasmLanguageError {
    /// The WASM module could not be compiled.
    CompilationError(String),
    /// The WASM module is missing required exports.
    MissingExport(String),
    /// The WASM module's ABI version is incompatible.
    AbiVersionMismatch {
        expected_min: u32,
        expected_max: u32,
        actual: u32,
    },
    /// Memory allocation failed in the guest.
    AllocationFailed {
        requested_size: u32,
    },
    /// A guest function returned an invalid fat pointer.
    InvalidFatPointer {
        fat_ptr: u64,
    },
    /// The data read from guest memory is not valid UTF-8.
    InvalidUtf8(std::string::FromUtf8Error),
    /// JSON deserialisation of data from the guest failed.
    JsonError(serde_json::Error),
    /// A WASM runtime error occurred during function execution.
    RuntimeError(String),
    /// The WASM module's memory could not be accessed.
    MemoryAccessError(String),
    /// A host function received invalid arguments.
    HostFunctionError(String),
    /// The requested function is not available (optional export not present).
    FunctionNotAvailable(String),
    /// I/O error loading the WASM file.
    IoError(std::io::Error),
    /// The guest function returned a null/error result.
    GuestError(String),
}

impl fmt::Display for WasmLanguageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmLanguageError::CompilationError(msg) => {
                write!(f, "WASM compilation error: {}", msg)
            }
            WasmLanguageError::MissingExport(name) => {
                write!(f, "WASM module missing required export: {}", name)
            }
            WasmLanguageError::AbiVersionMismatch {
                expected_min,
                expected_max,
                actual,
            } => {
                write!(
                    f,
                    "ABI version mismatch: module has version {}, host supports {}-{}",
                    actual, expected_min, expected_max
                )
            }
            WasmLanguageError::AllocationFailed { requested_size } => {
                write!(
                    f,
                    "Guest memory allocation failed for {} bytes",
                    requested_size
                )
            }
            WasmLanguageError::InvalidFatPointer { fat_ptr } => {
                write!(f, "Invalid fat pointer returned by guest: 0x{:016x}", fat_ptr)
            }
            WasmLanguageError::InvalidUtf8(err) => {
                write!(f, "Invalid UTF-8 from guest: {}", err)
            }
            WasmLanguageError::JsonError(err) => {
                write!(f, "JSON serialisation error: {}", err)
            }
            WasmLanguageError::RuntimeError(msg) => {
                write!(f, "WASM runtime error: {}", msg)
            }
            WasmLanguageError::MemoryAccessError(msg) => {
                write!(f, "WASM memory access error: {}", msg)
            }
            WasmLanguageError::HostFunctionError(msg) => {
                write!(f, "Host function error: {}", msg)
            }
            WasmLanguageError::FunctionNotAvailable(name) => {
                write!(f, "Function not available: {}", name)
            }
            WasmLanguageError::IoError(err) => {
                write!(f, "I/O error: {}", err)
            }
            WasmLanguageError::GuestError(msg) => {
                write!(f, "Guest returned error: {}", msg)
            }
        }
    }
}

impl std::error::Error for WasmLanguageError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            WasmLanguageError::InvalidUtf8(err) => Some(err),
            WasmLanguageError::JsonError(err) => Some(err),
            WasmLanguageError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for WasmLanguageError {
    fn from(err: std::io::Error) -> Self {
        WasmLanguageError::IoError(err)
    }
}

impl From<serde_json::Error> for WasmLanguageError {
    fn from(err: serde_json::Error) -> Self {
        WasmLanguageError::JsonError(err)
    }
}

impl From<std::string::FromUtf8Error> for WasmLanguageError {
    fn from(err: std::string::FromUtf8Error) -> Self {
        WasmLanguageError::InvalidUtf8(err)
    }
}

// From<WasmLanguageError> for AnyError covered by blanket impl
