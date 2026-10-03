//! Service Languages: typed, content-addressed
//! service interfaces, built-in implementations, and the host that every
//! caller reaches them through.

pub mod builtin;
pub mod capability;
pub mod codegen;
pub mod host;
pub mod interface;
pub mod registry;
pub mod schema_export;
pub mod semver;
pub mod ws;

#[cfg(test)]
mod tests;

pub use builtin::{CallContext, Caller, ServiceError, ServiceHealth, ServiceImplementation};
pub use host::{host, is_service_method, ServiceHost};
pub use interface::InterfaceDocument;
