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
//! `ad4m://mentions` itself is always one of them, so an app can also write it
//! directly without declaring anything.
//!
//! A mention subscription follows those links to the agent's DID — an exact,
//! indexed match whose woken item is the message itself — and also matches
//! `@name` or the DID in message text. A mention written without the app's
//! picker (pasted, imported, or by another bot through MCP) has no link, and
//! still has to wake its agent.

use super::perspective_instance::PerspectiveInstance;
use std::sync::Arc;

/// The role a predicate plays when it links a message to an agent it mentions.
pub const MENTIONS: &str = "ad4m://mentions";

impl PerspectiveInstance {
    /// The predicates linking a message to the agents it mentions:
    /// `ad4m://mentions`, then each one this perspective's authors declared.
    pub async fn mention_relations(&self) -> Arc<Vec<String>> {
        let declared = self.declared_for(MENTIONS).await;
        let mut all = vec![MENTIONS.to_string()];
        all.extend(declared.iter().cloned());
        Arc::new(all)
    }
}
