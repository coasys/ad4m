//! # Centralized Agent Expression Store, Rust ALDK port
//!
//! The closed beta's agent language: agent profiles and the identity interface on
//! the shared identity store, on the Rust ALDK; `pnpm build` ships it.
//!
//! - [`store`]: the identity calls, one store route each; the language signs nothing.
//! - [`agents`]: `expressionCreate`/`expressionGet` on the store's `/v1/agents/{did}`, in
//!   place of mock.ad4m.dev.
//!
//! The store's base URL is the `storeUrl` language setting (the executor fills it from
//! `identity.storeUrl`), read on every call; without one, the production store. The language
//! does not implement the token interface: the executor answers `disabled`.

pub mod agents;
pub mod store;

use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, KeyIndexEntry, LogPage, OfferListing,
    OfferOpened, OfferProgress, PowParams, SignedHead, SignedOffer, SignedRequest, VouchNote,
};
use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

use agents::AgentKey;
use store::{store_url_from, Http, HttpReply, Store};

/// The host's `httpFetch`.
pub struct HostHttp;

impl Http for HostHttp {
    async fn fetch(
        &self,
        url: &str,
        method: &str,
        headers_json: &str,
        body: &str,
    ) -> Result<HttpReply, String> {
        rt::http_fetch_typed(url, method, headers_json, body)
            .await
            .map(|r| HttpReply {
                status: r.status,
                body: r.body,
            })
            .map_err(|e| format!("{e:?}"))
    }
}

/// The agent the host runs the language for.
pub struct HostAgent;

impl AgentKey for HostAgent {
    fn did(&self) -> LanguageResult<String> {
        Ok(rt::agent_did()?)
    }

    fn sign(&self, bytes: &[u8]) -> LanguageResult<Vec<u8>> {
        Ok(rt::agent_sign(bytes)?)
    }

    fn now_ms(&self) -> u64 {
        js_sys::Date::now() as u64
    }
}

pub struct CentralizedAgentLanguage {
    store: Store<HostHttp>,
}

impl Language for CentralizedAgentLanguage {
    fn name() -> &'static str {
        "centralized-agent-expression-store"
    }
    fn version() -> &'static str {
        "0.2.0"
    }
    fn is_public() -> bool {
        false
    }

    async fn init() -> LanguageResult<Self> {
        Ok(CentralizedAgentLanguage {
            store: Store::new(HostHttp, || store_url_from(&rt::language_settings())),
        })
    }
}

impl ExpressionCapability for CentralizedAgentLanguage {
    async fn expression_create(&mut self, content: serde_json::Value) -> LanguageResult<Address> {
        agents::create(&self.store, &HostAgent, content).await
    }

    async fn expression_get(&mut self, address: Address) -> LanguageResult<Option<Expression>> {
        agents::get(&self.store, &address, HostAgent.now_ms() / 1000).await
    }
}

impl IdentityCapability for CentralizedAgentLanguage {
    async fn identity_log(&mut self, did: String, from: u64) -> LanguageResult<Option<LogPage>> {
        self.store.log(&did, from).await
    }

    async fn identity_head(&mut self, did: String) -> LanguageResult<Option<SignedHead>> {
        self.store.head(&did).await
    }

    async fn identity_append(
        &mut self,
        did: String,
        req: AppendRequest,
    ) -> LanguageResult<SignedHead> {
        self.store.append(&did, &req).await
    }

    async fn identity_key_owner(
        &mut self,
        multikey: String,
        wait_s: u8,
    ) -> LanguageResult<Option<KeyIndexEntry>> {
        self.store.key_owner(&multikey, wait_s).await
    }

    async fn identity_pow_params(&mut self) -> LanguageResult<PowParams> {
        self.store.pow_params().await
    }

    async fn identity_attach_note(
        &mut self,
        did: String,
        token: String,
        req: SignedRequest,
    ) -> LanguageResult<VouchNote> {
        self.store.attach_note(&did, &token, &req).await
    }

    async fn identity_put_agent(
        &mut self,
        record: AgentRecord,
        req: Option<SignedRequest>,
    ) -> LanguageResult<()> {
        self.store.put_agent(&record, req.as_ref()).await
    }

    async fn identity_open_offer(&mut self, offer: SignedOffer) -> LanguageResult<OfferOpened> {
        self.store.open_offer(&offer).await
    }

    async fn identity_list_offers(
        &mut self,
        did: String,
        req: SignedRequest,
        wait_s: u8,
    ) -> LanguageResult<Vec<OfferListing>> {
        self.store.list_offers(&did, &req, wait_s).await
    }

    async fn identity_cancel_offer(
        &mut self,
        did: String,
        offer_id: String,
        req: SignedRequest,
    ) -> LanguageResult<()> {
        self.store.cancel_offer(&did, &offer_id, &req).await
    }

    async fn identity_claim_offer(
        &mut self,
        did: String,
        offer_id: String,
        claim: Claim,
    ) -> LanguageResult<ClaimReceipt> {
        self.store.claim_offer(&did, &offer_id, &claim).await
    }

    async fn identity_offer_state(
        &mut self,
        did: String,
        offer_id: String,
        key: String,
        wait_s: u8,
    ) -> LanguageResult<OfferProgress> {
        self.store.offer_state(&did, &offer_id, &key, wait_s).await
    }
}

ad4m_language! {
    language: CentralizedAgentLanguage,
    capabilities: [expression, identity],
    holochain_signal: false,
}
