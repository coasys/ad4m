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
///
/// Both methods return a `Future` so languages can call async host
/// imports (`holochain_call`, HTTP via `fetch`, …). Languages whose
/// implementation is synchronous can simply write `async fn` — a
/// future that never suspends has no runtime cost.
pub trait ExpressionCapability: Language {
    fn expression_create(
        &mut self,
        content: serde_json::Value,
    ) -> impl std::future::Future<Output = LanguageResult<Address>>;
    fn expression_get(
        &mut self,
        address: Address,
    ) -> impl std::future::Future<Output = LanguageResult<Option<Expression>>>;
}

/// `identity` capability — the agent language's identity interface
/// (section 5.8 of `docs-src/language-interface-spec.md`), with the offer
/// and claim calls of device linking. Each call maps to
/// one store route. The language signs nothing and holds no key: signed
/// events and requests pass through unchanged. A refusal is an
/// [`IdentityErrorCode`] converted into the `LanguageError`
/// (`Err(IdentityErrorCode::NotFound.into())`).
///
/// The executor calls these only on the language at
/// `system_addresses.agent_language`.
pub trait IdentityCapability: Language {
    /// `GET /v1/logs/{scid}?from=`
    fn identity_log(
        &mut self,
        did: String,
        from: u64,
    ) -> impl std::future::Future<Output = LanguageResult<Option<LogPage>>>;
    /// `GET /v1/heads/{scid}`
    fn identity_head(
        &mut self,
        did: String,
    ) -> impl std::future::Future<Output = LanguageResult<Option<SignedHead>>>;
    /// `POST /v1/logs/{scid}/events`
    fn identity_append(
        &mut self,
        did: String,
        req: AppendRequest,
    ) -> impl std::future::Future<Output = LanguageResult<SignedHead>>;
    /// `GET /v1/keys/{multikey}?wait=` (0–8 seconds)
    fn identity_key_owner(
        &mut self,
        multikey: String,
        wait_s: u8,
    ) -> impl std::future::Future<Output = LanguageResult<Option<KeyIndexEntry>>>;
    /// `GET /v1/pow`
    fn identity_pow_params(
        &mut self,
    ) -> impl std::future::Future<Output = LanguageResult<PowParams>>;
    /// `POST /v1/notes/{scid}`
    fn identity_attach_note(
        &mut self,
        did: String,
        token: String,
        req: SignedRequest,
    ) -> impl std::future::Future<Output = LanguageResult<VouchNote>>;
    /// `PUT /v1/agents/{did}`; no signed request for a did:key record.
    fn identity_put_agent(
        &mut self,
        record: AgentRecord,
        req: Option<SignedRequest>,
    ) -> impl std::future::Future<Output = LanguageResult<()>>;
    /// `POST /v1/offers/{scid}`
    fn identity_open_offer(
        &mut self,
        offer: SignedOffer,
    ) -> impl std::future::Future<Output = LanguageResult<OfferOpened>>;
    /// `GET /v1/offers/{scid}?wait=`, signed by the current master key.
    fn identity_list_offers(
        &mut self,
        did: String,
        req: SignedRequest,
        wait_s: u8,
    ) -> impl std::future::Future<Output = LanguageResult<Vec<OfferListing>>>;
    /// `DELETE /v1/offers/{scid}/{offerId}`, signed by the current master key.
    fn identity_cancel_offer(
        &mut self,
        did: String,
        offer_id: String,
        req: SignedRequest,
    ) -> impl std::future::Future<Output = LanguageResult<()>>;
    /// `POST /v1/offers/{scid}/{offerId}/claim`
    fn identity_claim_offer(
        &mut self,
        did: String,
        offer_id: String,
        claim: Claim,
    ) -> impl std::future::Future<Output = LanguageResult<ClaimReceipt>>;
    /// `GET /v1/offers/{scid}/{offerId}/state?key=&wait=`
    fn identity_offer_state(
        &mut self,
        did: String,
        offer_id: String,
        key: String,
        wait_s: u8,
    ) -> impl std::future::Future<Output = LanguageResult<OfferProgress>>;
}

/// `token` capability — the agent language's token interface
/// (section 5.9 of `docs-src/language-interface-spec.md`, interfaces only).
/// Accounts are DIDs and amounts decimal strings; the language signs
/// through its context. A refusal is a
/// [`TokenErrorCode`] converted into the `LanguageError`. Transfers reach
/// the executor as the `token-transfer` language signal
/// (`runtime::emit_token_transfer`).
///
/// The executor calls these only on the language at
/// `system_addresses.agent_language`.
pub trait TokenCapability: Language {
    fn token_assets(&mut self) -> impl std::future::Future<Output = LanguageResult<Vec<String>>>;
    fn token_balance(
        &mut self,
        account: String,
        asset: String,
    ) -> impl std::future::Future<Output = LanguageResult<TokenBalance>>;
    /// Sends from the agent that the language runs for.
    fn token_send(
        &mut self,
        to: String,
        asset: String,
        amount: Amount,
        memo: Option<String>,
    ) -> impl std::future::Future<Output = LanguageResult<TokenTransfer>>;
    /// Transfers to the agent, newest last, after `cursor`.
    fn token_received(
        &mut self,
        cursor: Option<String>,
        limit: u32,
    ) -> impl std::future::Future<Output = LanguageResult<TransferPage>>;
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

/// `language-source` capability — fetching language source bundles.
///
/// Only the "language language" (the bootstrap language that stores and
/// distributes language bundles) implements this. The executor calls
/// `language_get_source(address)` when it needs to install a language
/// from the network.
///
/// Async because the implementation typically fetches from a remote store.
pub trait LanguageSourceCapability: Language {
    fn language_get_source(
        &mut self,
        address: Address,
    ) -> impl std::future::Future<Output = LanguageResult<String>>;
}

/// Optional Holochain signal handler. Auto-wired when the language
/// declares `holochain_signal` in the `ad4m_language!` macro.
pub trait HolochainSignalHandler: Language {
    fn handle_holochain_signal(&mut self, signal: serde_json::Value) -> LanguageResult<()>;
}
