//! API test utilities and shared test infrastructure.

#[cfg(test)]
pub mod agent_did_document_tests;
#[cfg(test)]
pub mod ai_task_access_tests;
#[cfg(test)]
pub mod anonymous_access_tests;
#[cfg(test)]
pub mod connection_tests;
#[cfg(test)]
pub mod error_tests;
#[cfg(test)]
pub mod executor_operation_tests;
#[cfg(test)]
pub mod flow_ws_tests;
#[cfg(test)]
pub mod handler_table_tests;
#[cfg(test)]
pub mod method_matrix_tests;
#[cfg(test)]
pub(crate) mod multi_user;
#[cfg(test)]
pub mod neighbourhood_access_tests;
#[cfg(test)]
pub mod notification_ws_tests;
#[cfg(test)]
pub mod runtime_access_tests;
#[cfg(test)]
pub mod runtime_ws_tests;
#[cfg(test)]
pub mod secret_leak_tests;
#[cfg(test)]
pub mod shacl_ws_tests;
#[cfg(test)]
pub mod start_server_tests;
#[cfg(test)]
pub mod support;
#[cfg(test)]
pub mod transcription_feed_tests;
#[cfg(test)]
pub mod types_tests;
