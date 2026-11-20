//! `Language` on wasm32: an address and an empty capability set. No language runtime runs in
//! the web executor, so `has()` is false for every capability and each call answers as native
//! `Language` does for a language without that capability (nothing to commit, sync, render,
//! no peers, no telepresence). A perspective with a link language then works as a private one.

use super::capability::Capability;
use crate::types::{OnlineAgent, Perspective, PerspectiveDiff, PerspectiveExpression};
use anyhow::Error as AnyError;

#[derive(Clone)]
pub struct Language {
    address: String,
}

impl Language {
    pub fn new(address: String) -> Self {
        Self { address }
    }

    pub fn address(&self) -> &str {
        &self.address
    }

    /// Always false: the capability set is empty.
    pub fn has(&self, _cap: Capability) -> bool {
        false
    }

    pub async fn commit(&mut self, _diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        Ok(None)
    }

    pub async fn sync(&mut self) -> Result<(), AnyError> {
        Ok(())
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        Ok(None)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        Ok(None)
    }

    pub async fn set_local_agents(&mut self, _agents: Vec<String>) -> Result<(), AnyError> {
        Ok(())
    }

    pub async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        Ok(Vec::new())
    }

    pub async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
        Ok(false)
    }

    pub async fn set_online_status(
        &mut self,
        _status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        Ok(())
    }

    pub async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
        Ok(Vec::new())
    }

    pub async fn send_signal(
        &mut self,
        _remote_agent_did: String,
        _payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        Ok(())
    }

    pub async fn send_broadcast(
        &mut self,
        _payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        Ok(())
    }
}
