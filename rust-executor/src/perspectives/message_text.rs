//! Which predicates hold a message's text in a perspective.
//!
//! The executor reads message text in two places: mention matching (the
//! waker's subscription scans it for the agent's names) and transcripts (the
//! text printed for each entry). Neither should guess at an app's class or
//! property names, so a perspective's installed schema says which of its
//! predicates hold message text, as a role (`declared_roles.rs`):
//!
//! ```text
//! <app://body> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://message_text>
//! ```
//!
//! An app can declare several — say, the text of a post and the text of a
//! comment. A perspective that declares none keeps the older behaviour: the
//! `body` property of a `Message` class for mentions, `body` or the identity
//! property for transcripts.

use super::perspective_instance::PerspectiveInstance;
use std::sync::Arc;

/// The role a predicate plays when it holds a message's text.
pub const MESSAGE_TEXT: &str = "ad4m://message_text";

impl PerspectiveInstance {
    /// The predicates this perspective's authors declared to hold message
    /// text. Empty when none are declared.
    pub async fn message_text_predicates(&self) -> Arc<Vec<String>> {
        self.declared_for(MESSAGE_TEXT).await
    }
}
