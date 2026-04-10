//! Capability traits implemented by Language structs. Spec §6.
//!
//! Languages implement only the traits corresponding to the capabilities
//! they support. The `ad4m_language!` macro then generates `#[no_mangle]`
//! shims *only* for the listed capabilities — the WASM export table is
//! therefore minimal and export-presence-as-capability-detection works.

use crate::errors::LanguageResult;
use crate::types::*;

/// Required lifecycle for every Language.
pub trait Language: 'static {
    fn name() -> &'static str;
    fn version() -> &'static str;
    fn is_public() -> bool { false }
    fn init() -> LanguageResult<Self>
    where
        Self: Sized;
    fn teardown(&mut self) -> LanguageResult<()> { Ok(()) }
    fn interactions(&self) -> Vec<Interaction> { Vec::new() }
}

/// `expression` capability — authoring, retrieving, signalling.
pub trait ExpressionCapability: Language {
    fn expression_create(&mut self, content: serde_json::Value) -> LanguageResult<Address>;
    fn expression_get(&mut self, address: Address) -> LanguageResult<Option<Expression>>;
    fn expression_interact(
        &mut self,
        address: Address,
        interaction: String,
        params: serde_json::Value,
    ) -> LanguageResult<serde_json::Value> {
        let _ = (address, interaction, params);
        Ok(serde_json::Value::Null)
    }
}

/// `perspective-commit` capability — writing diffs.
pub trait PerspectiveCommitCapability: Language {
    fn perspective_commit(&mut self, diff: PerspectiveDiff) -> LanguageResult<()>;
}

/// `perspective-sync` capability — gossip/pull/render.
pub trait PerspectiveSyncCapability: Language {
    fn perspective_sync_sync(&mut self) -> LanguageResult<PerspectiveDiff>;
    fn perspective_sync_render(&mut self) -> LanguageResult<Perspective>;
    fn perspective_sync_current_revision(&mut self) -> LanguageResult<Option<String>>;
}

/// `perspective-query` capability — structured reads.
pub trait PerspectiveQueryCapability: Language {
    fn perspective_query_supported_kinds(&self) -> Vec<String>;
    fn perspective_query_run(&mut self, request: QueryRequest) -> LanguageResult<QueryResponse>;
}

/// `peers` capability.
pub trait PeersCapability: Language {
    fn peers_set_local(&mut self, agents: Vec<Did>) -> LanguageResult<()>;
    fn peers_remote(&mut self) -> LanguageResult<Vec<Did>>;
}

/// `telepresence` capability.
pub trait TelepresenceCapability: Language {
    fn telepresence_set_online_status(&mut self, status: serde_json::Value) -> LanguageResult<()>;
    fn telepresence_get_online_agents(&mut self) -> LanguageResult<serde_json::Value>;
    fn telepresence_send_signal(
        &mut self,
        remote_did: Did,
        payload: serde_json::Value,
    ) -> LanguageResult<serde_json::Value>;
    fn telepresence_send_broadcast(
        &mut self,
        payload: serde_json::Value,
    ) -> LanguageResult<serde_json::Value>;
}

/// Optional Holochain signal handler. Auto-wired when the language
/// declares `holochain_signal` in the `ad4m_language!` macro.
pub trait HolochainSignalHandler: Language {
    fn handle_holochain_signal(&mut self, signal: serde_json::Value) -> LanguageResult<()>;
}
