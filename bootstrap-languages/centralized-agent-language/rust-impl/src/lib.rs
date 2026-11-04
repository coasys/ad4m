//! # Centralized Agent Expression Store, Rust ALDK port
//!
//! The closed beta's agent language: agent profiles and the identity interface on
//! the shared identity store, on the Rust ALDK; `pnpm build` ships it.
//!
//! - [`store`]: the identity calls, one store route each (logs, offers, the device inbox, the
//!   private record, the request box); the language signs nothing and opens nothing.
//! - [`agents`]: `expressionCreate`/`expressionGet` on the store's `/v1/agents/{did}`, in
//!   place of mock.ad4m.dev.
//!
//! The store's base URL is the `storeUrl` language setting (the executor fills it from
//! `identity.storeUrl`), read on every call; without one, `DEFAULT_STORE_URL` (the staging network's store). It must be
//! `https`, or plain `http` to a loopback address. The `storeKeys` setting (from
//! `identity.storeKeys`) lists the store keys whose signed heads the language trusts when it
//! reads a log to check a `did:scid` record; without it no such record verifies. The language
//! does not implement the token interface: the executor answers `disabled`.

pub mod agents;
pub mod store;

use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, DeviceRequest, InboxAddress, InboxAuth,
    InboxMessage, InboxStored, InboxWrite, KeyIndexEntry, LogPage, OfferListing, OfferOpened,
    OfferProgress, PostedRequest, PowParams, RecordStored, RequestPosted, SealedRecord, SignedHead,
    SignedOffer, SignedRequest, VouchNote,
};
use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

use agents::AgentKey;
use store::{store_settings_from, Http, HttpFailure, HttpReply, Store};

/// The host's `httpFetch`.
pub struct HostHttp;

impl Http for HostHttp {
    async fn fetch(
        &self,
        url: &str,
        method: &str,
        headers_json: &str,
        body: &str,
        max_bytes: usize,
    ) -> Result<HttpReply, HttpFailure> {
        rt::http_fetch_typed_capped(url, method, headers_json, body, max_bytes)
            .await
            .map(|r| HttpReply {
                status: r.status,
                body: r.body,
            })
            .map_err(|e| {
                if rt::http_fetch_too_large(&e) {
                    HttpFailure::TooLarge
                } else {
                    HttpFailure::Failed(format!("{e:?}"))
                }
            })
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

    fn signed_expression(&self, data: &serde_json::Value) -> LanguageResult<Expression> {
        let signed = rt::agent_create_signed_expression_typed(data)?;
        serde_wasm_bindgen::from_value(signed)
            .map_err(|e| LanguageError::internal(format!("the host's signed expression: {e}")))
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
            store: Store::new(HostHttp, || store_settings_from(&rt::language_settings())),
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

    async fn identity_inbox_put(
        &mut self,
        address: InboxAddress,
        write: InboxWrite,
        req: SignedRequest,
    ) -> LanguageResult<InboxStored> {
        self.store.inbox_put(&address, &write, &req).await
    }

    async fn identity_inbox_list(
        &mut self,
        address: InboxAddress,
        auth: InboxAuth,
        wait_s: u8,
    ) -> LanguageResult<Vec<InboxMessage>> {
        self.store.inbox_list(&address, &auth, wait_s).await
    }

    async fn identity_inbox_ack(
        &mut self,
        address: InboxAddress,
        id: String,
        auth: InboxAuth,
    ) -> LanguageResult<()> {
        self.store.inbox_ack(&address, &id, &auth).await
    }

    async fn identity_get_record(
        &mut self,
        did: String,
        req: SignedRequest,
    ) -> LanguageResult<Option<SealedRecord>> {
        self.store.get_record(&did, &req).await
    }

    async fn identity_put_record(
        &mut self,
        did: String,
        record: SealedRecord,
        req: SignedRequest,
    ) -> LanguageResult<RecordStored> {
        self.store.put_record(&did, &record, &req).await
    }

    async fn identity_post_request(
        &mut self,
        did: String,
        post: PostedRequest,
        req: SignedRequest,
    ) -> LanguageResult<RequestPosted> {
        self.store.post_request(&did, &post, &req).await
    }

    async fn identity_list_requests(
        &mut self,
        did: String,
        since: u64,
        req: SignedRequest,
    ) -> LanguageResult<Vec<DeviceRequest>> {
        self.store.list_requests(&did, since, &req).await
    }

    async fn identity_delete_request(
        &mut self,
        did: String,
        id: String,
        req: SignedRequest,
    ) -> LanguageResult<()> {
        self.store.delete_request(&did, &id, &req).await
    }
}

ad4m_language! {
    language: CentralizedAgentLanguage,
    capabilities: [expression, identity],
    holochain_signal: false,
}
