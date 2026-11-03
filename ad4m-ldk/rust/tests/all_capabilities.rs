//! Expands `ad4m_language!` with every capability arm, so `cargo test`
//! type-checks every shim the macro can emit. The shims marshal through
//! `JsValue` and only run inside WASM; the slot logic they share has unit
//! tests in `src/state.rs`.

use ad4m_ldk::prelude::*;
use ad4m_ldk::traits::LanguageSourceCapability;

pub struct Full;

impl Language for Full {
    fn name() -> &'static str {
        "@test/full"
    }
    fn version() -> &'static str {
        "0.0.0"
    }
    async fn init() -> LanguageResult<Self> {
        Ok(Full)
    }
}

impl ExpressionCapability for Full {
    async fn expression_create(&mut self, _content: serde_json::Value) -> LanguageResult<Address> {
        Ok("addr".into())
    }
    async fn expression_get(&mut self, _address: Address) -> LanguageResult<Option<Expression>> {
        Ok(None)
    }
}

impl PerspectiveCommitCapability for Full {
    fn perspective_commit(&mut self, _diff: PerspectiveDiff) -> LanguageResult<()> {
        Ok(())
    }
}

impl PerspectiveSyncCapability for Full {
    fn perspective_sync_sync(&mut self) -> LanguageResult<PerspectiveDiff> {
        Err(LanguageError::internal("unused"))
    }
    fn perspective_sync_render(&mut self) -> LanguageResult<Perspective> {
        Err(LanguageError::internal("unused"))
    }
    fn perspective_sync_current_revision(&mut self) -> LanguageResult<Option<String>> {
        Ok(None)
    }
}

impl PerspectiveQueryCapability for Full {
    fn perspective_query_supported_kinds(&self) -> Vec<String> {
        Vec::new()
    }
    fn perspective_query_run(&mut self, _request: QueryRequest) -> LanguageResult<QueryResponse> {
        Err(LanguageError::internal("unused"))
    }
}

impl PeersCapability for Full {
    fn peers_set_local(&mut self, _agents: Vec<Did>) -> LanguageResult<()> {
        Ok(())
    }
    fn peers_remote(&mut self) -> LanguageResult<Vec<Did>> {
        Ok(Vec::new())
    }
}

impl TelepresenceCapability for Full {
    fn telepresence_set_online_status(&mut self, _status: serde_json::Value) -> LanguageResult<()> {
        Ok(())
    }
    fn telepresence_get_online_agents(&mut self) -> LanguageResult<serde_json::Value> {
        Ok(serde_json::Value::Null)
    }
    fn telepresence_send_signal(
        &mut self,
        _remote_did: Did,
        _payload: serde_json::Value,
    ) -> LanguageResult<serde_json::Value> {
        Ok(serde_json::Value::Null)
    }
    fn telepresence_send_broadcast(
        &mut self,
        _payload: serde_json::Value,
    ) -> LanguageResult<serde_json::Value> {
        Ok(serde_json::Value::Null)
    }
}

impl LanguageSourceCapability for Full {
    async fn language_get_source(&mut self, _address: Address) -> LanguageResult<String> {
        Ok(String::new())
    }
}

impl HolochainSignalHandler for Full {
    fn handle_holochain_signal(&mut self, _signal: serde_json::Value) -> LanguageResult<()> {
        Ok(())
    }
}

ad4m_language! {
    language: Full,
    capabilities: [
        expression, perspective_commit, perspective_sync, perspective_query,
        peers, language_source, telepresence,
    ],
    holochain_signal: true,
}

#[test]
fn every_capability_arm_expands() {
    assert_eq!(__ad4m_name(), "@test/full");
}
