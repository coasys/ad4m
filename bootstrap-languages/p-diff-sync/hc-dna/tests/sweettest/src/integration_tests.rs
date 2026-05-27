//! Integration tests for perspective_diff_sync
//!
//! This file imports all test modules and runs them as integration tests.

// Import test modules
mod test_commit_pull;
mod test_render;
mod test_revisions;
mod test_telepresence;
mod test_validation_storm;
mod utils;

// Re-export for external access if needed
pub use utils::*;
