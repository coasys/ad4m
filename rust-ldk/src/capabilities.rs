//! Capability traits for AD4M languages
//!
//! These traits define what capabilities a language can have.
/// Languages implement these traits to indicate support for specific functionality.

use crate::types::{LinkExpression, PerspectiveDiff};

/// Capability for languages that can sync links
///
/// Languages implementing this trait can add and remove links
/// from a shared perspective.
///
/// # Example
///
/// ```rust
/// use ad4m_ldk::{LinkSyncCapability, LinkExpression, PerspectiveDiff};
///
/// struct MyLinkLanguage;
///
/// impl LinkSyncCapability for MyLinkLanguage {
///     fn add_links(&mut self, links: Vec<LinkExpression>) -> Result<(), String> {
///         // Add links to the language's state
///         Ok(())
///     }
///
///     fn remove_links(&mut self, links: Vec<String>) -> Result<(), String> {
///         // Remove links from the language's state
///         Ok(())
///     }
/// }
/// ```
pub trait LinkSyncCapability {
    /// Adds links to the language's state
    ///
    /// # Arguments
    ///
    /// * `links` - The links to add
    fn add_links(&mut self, links: Vec<LinkExpression>) -> Result<(), String>;

    /// Removes links from the language's state
    ///
    /// # Arguments
    ///
    /// * `links` - The link hashes to remove
    fn remove_links(&mut self, links: Vec<String>) -> Result<(), String>;

    /// Commits a perspective diff
    ///
    /// Default implementation calls add_links and remove_links
    fn commit_diff(&mut self, diff: PerspectiveDiff) -> Result<(), String> {
        if !diff.additions.is_empty() {
            self.add_links(diff.additions)?;
        }
        if !diff.removals.is_empty() {
            let hashes: Vec<String> = diff
                .removals
                .iter()
                .map(|l| format!("{}-{}", l.author, l.timestamp))
                .collect();
            self.remove_links(hashes)?;
        }
        Ok(())
    }
}

/// Capability for languages that can create and retrieve expressions
///
/// Languages implementing this trait can create new expressions
/// and retrieve existing ones by hash.
///
/// # Example
///
/// ```rust
/// use ad4m_ldk::ExpressionCapability;
///
/// struct MyExpressionLanguage;
///
/// impl ExpressionCapability for MyExpressionLanguage {
///     fn create(&mut self, expression: Vec<u8>) -> Result<String, String> {
///         // Create and store the expression
///         Ok("expression_hash".to_string())
///     }
///
///     fn get(&mut self, hash: String) -> Result<Option<Vec<u8>>, String> {
///         // Retrieve the expression
///         Ok(None)
///     }
/// }
/// ```
pub trait ExpressionCapability {
    /// Creates a new expression
    ///
    /// # Arguments
    ///
    /// * `expression` - The raw expression data
    ///
    /// # Returns
    ///
    /// The hash of the created expression
    fn create(&mut self, expression: Vec<u8>) -> Result<String, String>;

    /// Retrieves an expression by its hash
    ///
    /// # Arguments
    ///
    /// * `hash` - The hash of the expression to retrieve
    ///
    /// # Returns
    ///
    /// The expression data, or None if not found
    fn get(&mut self, hash: String) -> Result<Option<Vec<u8>>, String>;
}

/// Capability for languages that support telepresence
///
/// Languages implementing this trait can send and receive signals
/// for real-time communication between agents.
pub trait TelepresenceCapability {
    /// Sets the online status of the agent
    fn set_online_status(&mut self, status: serde_json::Value) -> Result<(), String>;

    /// Gets the list of currently online agents
    fn get_online_agents(&mut self) -> Result<Vec<serde_json::Value>, String>;

    /// Sends a signal to a specific agent
    fn send_signal(&mut self, remote_agent_did: String, payload: serde_json::Value) -> Result<(), String>;

    /// Sends a broadcast signal to all agents
    fn send_broadcast(&mut self, payload: serde_json::Value) -> Result<(), String>;
}

/// Capability for languages that support direct messaging
///
/// Languages implementing this trait can send and receive
/// direct messages between agents.
pub trait DirectMessageCapability {
    /// Sends a direct message to another agent
    fn send_message(&mut self, to: String, message: Vec<u8>) -> Result<(), String>;

    /// Called when a direct message is received
    fn on_message(&mut self, from: String, message: Vec<u8>) -> Result<(), String>;
}
