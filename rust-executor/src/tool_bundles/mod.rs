//! Tool bundles: JavaScript an app ships so agents on a node can use its tools, run in a sandbox
//! that can compute and nothing else.
//!
//! An app's rules for its own data — what a valid record is, how an edit applies — change with
//! every release of the app, and a copy in Rust would drift. So the app ships them as one ES module
//! and the executor runs it: [`BundleSandbox`] gives the module an isolate of its own, and
//! [`ToolBundleStore`] keeps bundles by content address. Nothing here knows what a bundle exports;
//! a caller names an export path and passes JSON.

mod registry;
mod sandbox;
mod store;

pub use registry::{
    BundleDocEntry, BundleTool, FoundTool, InstalledBundle, RegistryError, ToolBundleRegistry,
    Toolset,
};
pub use sandbox::{BundleSandbox, SandboxError, SandboxLimits};
pub use store::ToolBundleStore;

#[cfg(test)]
mod tests;
