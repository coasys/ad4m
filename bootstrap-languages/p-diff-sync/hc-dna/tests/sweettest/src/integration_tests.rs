//! Integration tests for perspective_diff_sync
//!
//! This file imports all test modules and runs them as integration tests.

// Import test modules
mod utils;
mod test_commit_pull;
mod test_render;
mod test_revisions;
mod test_telepresence;

// Re-export for external access if needed
pub use utils::*;
