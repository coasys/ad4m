//! Which predicates link a message to the agents it mentions.
//!
//! Matching mentions in text is a guess: a name can be anybody's, and a
//! search over every literal in a perspective is slow. An app that records a
//! mention as a link — message → agent DID — can say which predicate that is,
//! as a role (`declared_roles.rs`):
//!
//! ```text
//! <app://mention> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://mentions>
//! ```
//!
//! A mention subscription in such a perspective follows those links to the
//! agent's DID and nothing else: no name collisions, an indexed lookup, and the
//! woken item is the message itself. A perspective that declares none is
//! matched in text, on `@name` or the DID.

use super::perspective_instance::PerspectiveInstance;
use std::sync::Arc;

/// The role a predicate plays when it links a message to an agent it mentions.
pub const MENTIONS: &str = "ad4m://mentions";

impl PerspectiveInstance {
    /// The predicates this perspective's authors declared to link a message
    /// to the agents it mentions. Empty when none are declared.
    pub async fn mention_relations(&self) -> Arc<Vec<String>> {
        self.declared_for(MENTIONS).await
    }
}
