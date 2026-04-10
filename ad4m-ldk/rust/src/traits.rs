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
    /// Async so that languages can register Holochain DNAs, await zome
    /// calls, or otherwise use any of the Promise-returning imports
    /// during initialization. Languages whose init logic is sync can
    /// simply write `async fn init() -> LanguageResult<Self> { Ok(...) }`
    /// — there is no runtime cost for a future that never suspends.
    fn init() -> impl std::future::Future<Output = LanguageResult<Self>>
    where
        Self: Sized;
    fn teardown(&mut self) -> LanguageResult<()> { Ok(()) }
    /// Returns the interactions available for the given expression
    /// address. Spec §5.7 — the runtime calls this per expression to
    /// discover what actions can be performed against it. Languages
    /// that don't expose per-expression interactions can ignore the
    /// address argument and return a static list (or `Vec::new()`).
    fn interactions(&self, _address: Address) -> Vec<Interaction> { Vec::new() }

    /// Execute a named interaction against an expression. Spec §5.7 —
    /// Rust ALDK languages cannot return JS callables in `interactions()`
    /// (the list crosses the wasm-bindgen boundary as plain JSON), so
    /// the runtime calls this top-level method to actually invoke the
    /// chosen interaction. JS-authored languages can keep the legacy
    /// closure form on the interaction object; this method is the
    /// Rust-friendly counterpart.
    ///
    /// Default implementation returns `Ok(None)` so languages without
    /// interactions don't need to override it.
    fn expression_interact(
        &mut self,
        _address: Address,
        _name: String,
        _parameters: serde_json::Value,
    ) -> LanguageResult<Option<serde_json::Value>> {
        Ok(None)
    }
}

/// `expression` capability — authoring + retrieving expressions.
///
/// Per-expression actions ("interactions") are described via the
/// `Language::interactions(address)` lifecycle method and invoked
/// through the `Interaction` object the runtime constructs from that
/// description — there is no top-level `expressionInteract` export.
pub trait ExpressionCapability: Language {
    fn expression_create(&mut self, content: serde_json::Value) -> LanguageResult<Address>;
    fn expression_get(&mut self, address: Address) -> LanguageResult<Option<Expression>>;
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
