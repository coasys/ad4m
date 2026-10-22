//! Tool bundles: JavaScript the executor runs for its agents' tools, in a sandbox that can
//! compute and nothing else.
//!
//! An app's rules for its own data — what a valid record is, how an edit applies — change with
//! every release of the app, and a copy in Rust would drift. So the app ships them as one ES module
//! and the executor runs it: [`BundleSandbox`] gives the module an isolate of its own. Nothing here
//! knows what a bundle exports; a caller names an export path and passes JSON.
//!
//! [`builtin`] is the one bundle the executor carries: WE's template tools, compiled in.

pub mod builtin;
mod sandbox;

pub use sandbox::{BundleSandbox, SandboxError, SandboxLimits};

#[cfg(test)]
mod tests;
