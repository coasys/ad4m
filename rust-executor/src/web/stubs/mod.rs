//! What the stubs of the web executor share. The stubs themselves are crate-root modules
//! (`crate::languages`, `crate::ai_service`, ...), declared by path in `lib_wasm.rs`.

use std::fmt;

/// The error a stub returns: `what` does not exist in the web executor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NotAvailable(pub &'static str);

impl fmt::Display for NotAvailable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is not available in the web executor", self.0)
    }
}

impl std::error::Error for NotAvailable {}

/// [`NotAvailable`] as an `anyhow::Error`, so callers can still `downcast_ref` it.
pub fn not_available(what: &'static str) -> anyhow::Error {
    anyhow::Error::new(NotAvailable(what))
}
