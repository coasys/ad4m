//! `crate::identity` on wasm32: no `did:scid` key logs in the web executor; the agent is its
//! device `did:key`.
//!
//! Real: `types` (the native file by path; `agent.get` and the event list name them).
//! Stubbed: `service()` is always the native "not started" answer, so an agent speaks for
//! no `did:scid`; `rekey::finish_moves_at_start` has nothing to move.

#[path = "../../identity/types.rs"]
pub mod types;

use crate::api::ws_handler::WsRpcError;
use std::sync::Arc;

/// The identity module never starts on wasm32: the answer native gives before it starts,
/// naming the web executor.
pub fn service() -> Result<Arc<service::IdentityService>, WsRpcError> {
    Err(WsRpcError {
        data: None,
        code: 503,
        message: crate::web::stubs::NotAvailable("the identity module").to_string(),
    })
}

pub mod service {
    use crate::agent::AgentContext;
    use crate::api::ws_handler::WsRpcError;
    use serde_json::Value;

    /// Never built on wasm32 ([`super::service`] is always an error).
    pub struct IdentityService {
        _none: (),
    }

    impl IdentityService {
        pub async fn publish_person_profile(
            &self,
            _agent: &AgentContext,
            _profile: &Value,
        ) -> Result<Option<String>, WsRpcError> {
            Err(WsRpcError::not_implemented(
                crate::web::stubs::NotAvailable("the identity module").to_string(),
            ))
        }
    }

    /// No agent speaks for a `did:scid` in the web executor.
    pub async fn identity_of_agent(_agent_did: &str) -> Option<String> {
        None
    }
}

pub mod rekey {
    /// Nothing to move: the web executor never rekeys.
    pub fn finish_moves_at_start() {}
}
