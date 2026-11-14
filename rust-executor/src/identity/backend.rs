//! [`IdentityBackend`]: the executor's view of the agent language's identity interface
//! (section 5.8 of `docs-src/language-interface-spec.md`). The one implementation,
//! [`AgentLanguageBackend`], calls the
//! language at `system_addresses.agent_language` through
//! `LanguageController::agent_language_for`, which refuses any other language. Nothing in the
//! executor calls the store's HTTP routes directly, so a deployment can swap
//! the store behind the language. (A foreign `src` host is not the store: the resolver reads
//! it itself, `src_fetch.rs`.)

use std::time::Duration;

use ad4m_ldk::identity::{
    AgentRecord, AppendRequest, Claim, ClaimReceipt, DeviceRequest, ForbiddenReason,
    IdentityErrorCode, InboxAddress, InboxAuth, InboxMessage, InboxStored, InboxWrite,
    KeyIndexEntry, LogPage, OfferListing, OfferOpened, OfferProgress, PostedRequest,
    RateLimitScope, RecordStored, RequestPosted, SealedRecord, SignedHead, SignedOffer,
    SignedRequest, StoredRecord,
};
use async_trait::async_trait;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde_json::Value;

use crate::languages::capability::Capability;
use crate::languages::LanguageController;

/// How long one call may take before it counts as `Unavailable` (a read should answer within
/// 5 s; the language's own host timeout is 10 s, so a slow store answers `unavailable`
/// from the language first).
pub const CALL_TIMEOUT: Duration = Duration::from_secs(15);

/// How the identity interface refused or failed.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum IdentityError {
    /// `409` with the store's head: the append does not continue it.
    #[error("the log moved on: the store's head is seq {}", .0.seq)]
    Conflict(SignedHead),
    #[error("the key {0} sits in a log already")]
    KeyTaken(String),
    #[error("refused: {0:?}")]
    Forbidden(ForbiddenReason),
    #[error("invalid: {0}")]
    Invalid(String),
    #[error("rate limited ({scope:?}): retry after {retry_after_s} s")]
    RateLimited {
        scope: RateLimitScope,
        retry_after_s: u64,
    },
    #[error("the identity store is unavailable: {0}")]
    Unavailable(String),
    /// The loaded agent language does not serve the call (a language without the identity
    /// interface, such as a local test language): no store answers through it, now or after a
    /// retry. Not [`IdentityError::Unavailable`], which a later call may get past.
    #[error("the agent language does not serve {0}")]
    NotServed(String),
    /// A head that no key of `identity.storeKeys` signed.
    #[error("the store's head is not signed by a key of identity.storeKeys")]
    BadHeadSignature,
    /// `410`: an offer that ended, with its state.
    #[error("gone: {0}")]
    Gone(String),
    /// `404`: an unknown offer.
    #[error("not found")]
    NotFound,
}

impl From<IdentityErrorCode> for IdentityError {
    fn from(code: IdentityErrorCode) -> Self {
        match code {
            IdentityErrorCode::Conflict { head } => IdentityError::Conflict(head),
            IdentityErrorCode::KeyTaken { key } => IdentityError::KeyTaken(key),
            IdentityErrorCode::Forbidden { reason } => IdentityError::Forbidden(reason),
            IdentityErrorCode::Invalid { reason } => IdentityError::Invalid(reason),
            IdentityErrorCode::RateLimited {
                scope,
                retry_after_sec,
            } => IdentityError::RateLimited {
                scope,
                retry_after_s: retry_after_sec,
            },
            IdentityErrorCode::Gone { state } => IdentityError::Gone(state),
            IdentityErrorCode::NotFound => IdentityError::NotFound,
            IdentityErrorCode::Unavailable => {
                IdentityError::Unavailable("the agent language answered unavailable".into())
            }
        }
    }
}

/// The agent language's identity calls the executor makes today.
#[async_trait]
pub trait IdentityBackend: Send + Sync {
    /// One page of `did`'s log from seq `from`, from the configured store. A foreign `src` is
    /// never read here: the resolver reads it itself (`resolver::SrcFetch`).
    async fn log(&self, did: &str, from: u64) -> Result<Option<LogPage>, IdentityError>;
    /// The store's key index: the identity a signing multikey sits in. A hint only: the
    /// resolver checks the named log lists the key.
    async fn key_owner(
        &self,
        multikey: &str,
        wait_s: u8,
    ) -> Result<Option<KeyIndexEntry>, IdentityError>;
    async fn append(&self, did: &str, req: &AppendRequest) -> Result<SignedHead, IdentityError>;
    /// Writes an agent record (`PUT /v1/agents/{did}`); a `did:scid` record comes with a
    /// signed request of that identity.
    async fn put_agent(
        &self,
        record: &AgentRecord,
        req: Option<&SignedRequest>,
    ) -> Result<(), IdentityError>;
    /// The `version` of the agent record the store holds for `did`, as the agent language
    /// answers the record (`expressionGet`, `proof.record.version`); `None` without one. A put
    /// must carry a greater version, so a device whose clock lags another's reads it first.
    async fn agent_version(&self, did: &str) -> Result<Option<u64>, IdentityError>;
    async fn open_offer(&self, offer: &SignedOffer) -> Result<OfferOpened, IdentityError>;
    async fn list_offers(
        &self,
        did: &str,
        req: &SignedRequest,
        wait_s: u8,
    ) -> Result<Vec<OfferListing>, IdentityError>;
    async fn claim_offer(
        &self,
        did: &str,
        offer_id: &str,
        claim: &Claim,
    ) -> Result<ClaimReceipt, IdentityError>;
    async fn offer_state(
        &self,
        did: &str,
        offer_id: &str,
        key: &str,
        wait_s: u8,
    ) -> Result<OfferProgress, IdentityError>;
    /// Writes a sealed message to an inbox, with the master-signed request of the write.
    async fn inbox_put(
        &self,
        address: &InboxAddress,
        write: &InboxWrite,
        req: &SignedRequest,
    ) -> Result<InboxStored, IdentityError>;
    /// The messages of an inbox, oldest first, waiting up to `wait_s` (0–8) for one.
    async fn inbox_list(
        &self,
        address: &InboxAddress,
        auth: &InboxAuth,
        wait_s: u8,
    ) -> Result<Vec<InboxMessage>, IdentityError>;
    /// Deletes message `id` of an inbox.
    async fn inbox_ack(
        &self,
        address: &InboxAddress,
        id: &str,
        auth: &InboxAuth,
    ) -> Result<(), IdentityError>;
    /// The identity's sealed private record with the store's receipt for it; `None` before
    /// the first write.
    async fn get_record(
        &self,
        did: &str,
        req: &SignedRequest,
    ) -> Result<Option<StoredRecord>, IdentityError>;
    async fn put_record(
        &self,
        did: &str,
        record: &SealedRecord,
        req: &SignedRequest,
    ) -> Result<RecordStored, IdentityError>;
    async fn post_request(
        &self,
        did: &str,
        post: &PostedRequest,
        req: &SignedRequest,
    ) -> Result<RequestPosted, IdentityError>;
    async fn list_requests(
        &self,
        did: &str,
        since: u64,
        req: &SignedRequest,
    ) -> Result<Vec<DeviceRequest>, IdentityError>;
    async fn delete_request(
        &self,
        did: &str,
        id: &str,
        req: &SignedRequest,
    ) -> Result<(), IdentityError>;
}

/// The script that calls `export` with `args` (JSON literals) on the language and answers
/// `{"ok": result}`, `{"undefined": true}` when the export answered `undefined`, or
/// `{"err": {"message": …}}`. A refusal's message is the refusal as JSON (`IdentityError` in
/// `ad4m-ldk/js`, `IdentityErrorCode` in `ad4m-ldk/rust`).
pub(crate) fn call_script(export: &str, args: &[Value]) -> String {
    let args = args
        .iter()
        .map(Value::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        r#"JSON.stringify(await (async () => {{
    try {{
        const result = await language.{export}({args});
        return result === undefined ? {{ undefined: true }} : {{ ok: result }};
    }} catch (e) {{
        const message = (e && typeof e === "object" && "message" in e) ? String(e.message) : String(e);
        return {{ err: {{ message }} }};
    }}
}})())"#
    )
}

#[derive(Deserialize)]
struct Envelope {
    #[serde(default)]
    ok: Option<Value>,
    #[serde(default)]
    undefined: bool,
    #[serde(default)]
    err: Option<EnvelopeError>,
}

#[derive(Deserialize)]
struct EnvelopeError {
    message: String,
}

/// What an export may answer as `undefined`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Undefined {
    /// Nothing: an identity export answers `null` for no value (`identityLog` with no log), so
    /// `undefined` is a language that does not follow the interface, never a missing log.
    Refused,
    /// `null`: a void export (`Promise<void>`), or an expression adapter's `get`.
    Null,
}

/// The answer of a [`call_script`] run: the result as `T`, or the refusal it carried.
pub(crate) fn read_answer<T: DeserializeOwned>(
    text: &str,
    undefined: Undefined,
) -> Result<T, IdentityError> {
    let envelope: Envelope = serde_json::from_str(text)
        .map_err(|e| IdentityError::Unavailable(format!("the agent language answered {e}")))?;
    if let Some(err) = envelope.err {
        return Err(
            match serde_json::from_str::<IdentityErrorCode>(&err.message) {
                Ok(code) => code.into(),
                Err(_) => IdentityError::Unavailable(err.message),
            },
        );
    }
    if envelope.undefined && undefined == Undefined::Refused {
        return Err(IdentityError::Invalid(
            "the agent language answered undefined".into(),
        ));
    }
    serde_json::from_value(envelope.ok.unwrap_or(Value::Null))
        .map_err(|e| IdentityError::Invalid(format!("the agent language answered {e}")))
}

/// The `version` of the agent record a profile came in (`proof.record.version`, which the agent
/// language adds to a `did:scid` profile); `None` without one.
pub(crate) fn record_version(profile: &Value) -> Option<u64> {
    profile
        .get("proof")?
        .get("record")?
        .get("version")?
        .as_u64()
}

/// [`IdentityBackend`] on the agent language.
pub struct AgentLanguageBackend;

impl AgentLanguageBackend {
    async fn call<T: DeserializeOwned>(
        &self,
        cap: Capability,
        args: &[Value],
    ) -> Result<T, IdentityError> {
        let undefined = match cap {
            Capability::IdentityPutAgent => Undefined::Null,
            _ => Undefined::Refused,
        };
        self.call_export(cap, cap.export_name(), args, undefined)
            .await
    }

    /// `export` on the agent language that serves `cap`.
    async fn call_export<T: DeserializeOwned>(
        &self,
        cap: Capability,
        export: &str,
        args: &[Value],
        undefined: Undefined,
    ) -> Result<T, IdentityError> {
        let controller = LanguageController::global_instance();
        let settled = controller.system_addresses.lock().await.settled;
        let language = controller
            .agent_language_for(cap)
            .await
            .map_err(|e| no_agent_language(cap, e, settled))?;
        if !language.has(cap) {
            // A language whose capabilities are not registered yet may still load them.
            return Err(
                if crate::languages::capability::capabilities_registered(language.address()) {
                    IdentityError::NotServed(cap.wire_name().to_string())
                } else {
                    IdentityError::Unavailable(format!(
                        "the agent language has not loaded {}",
                        cap.wire_name()
                    ))
                },
            );
        }
        let script = call_script(export, args);
        let run = controller.execute_on_language(language.address(), &script);
        let text = tokio::time::timeout(CALL_TIMEOUT, run)
            .await
            .map_err(|_| IdentityError::Unavailable(format!("{export} timed out")))?
            .map_err(|e| IdentityError::Unavailable(e.to_string()))?;
        read_answer(&text, undefined)
    }
}

fn json<T: serde::Serialize>(value: &T) -> Result<Value, IdentityError> {
    serde_json::to_value(value).map_err(|e| IdentityError::Invalid(e.to_string()))
}

#[async_trait]
impl IdentityBackend for AgentLanguageBackend {
    async fn log(&self, did: &str, from: u64) -> Result<Option<LogPage>, IdentityError> {
        self.call(Capability::IdentityLog, &[did.into(), from.into()])
            .await
    }

    async fn key_owner(
        &self,
        multikey: &str,
        wait_s: u8,
    ) -> Result<Option<KeyIndexEntry>, IdentityError> {
        self.call(
            Capability::IdentityKeyOwner,
            &[multikey.into(), wait_s.into()],
        )
        .await
    }

    async fn append(&self, did: &str, req: &AppendRequest) -> Result<SignedHead, IdentityError> {
        self.call(Capability::IdentityAppend, &[did.into(), json(req)?])
            .await
    }

    async fn put_agent(
        &self,
        record: &AgentRecord,
        req: Option<&SignedRequest>,
    ) -> Result<(), IdentityError> {
        let req = req.map(json).transpose()?.unwrap_or(Value::Null);
        self.call(Capability::IdentityPutAgent, &[json(record)?, req])
            .await
    }

    async fn agent_version(&self, did: &str) -> Result<Option<u64>, IdentityError> {
        // The language that takes the record answers it: the profile with `proof.record`.
        let profile: Option<Value> = self
            .call_export(
                Capability::IdentityPutAgent,
                "expressionGet",
                &[did.into()],
                Undefined::Null,
            )
            .await?;
        Ok(profile.as_ref().and_then(record_version))
    }

    async fn open_offer(&self, offer: &SignedOffer) -> Result<OfferOpened, IdentityError> {
        self.call(Capability::IdentityOpenOffer, &[json(offer)?])
            .await
    }

    async fn list_offers(
        &self,
        did: &str,
        req: &SignedRequest,
        wait_s: u8,
    ) -> Result<Vec<OfferListing>, IdentityError> {
        self.call(
            Capability::IdentityListOffers,
            &[did.into(), json(req)?, wait_s.into()],
        )
        .await
    }

    async fn claim_offer(
        &self,
        did: &str,
        offer_id: &str,
        claim: &Claim,
    ) -> Result<ClaimReceipt, IdentityError> {
        self.call(
            Capability::IdentityClaimOffer,
            &[did.into(), offer_id.into(), json(claim)?],
        )
        .await
    }

    async fn offer_state(
        &self,
        did: &str,
        offer_id: &str,
        key: &str,
        wait_s: u8,
    ) -> Result<OfferProgress, IdentityError> {
        self.call(
            Capability::IdentityOfferState,
            &[did.into(), offer_id.into(), key.into(), wait_s.into()],
        )
        .await
    }

    async fn inbox_put(
        &self,
        address: &InboxAddress,
        write: &InboxWrite,
        req: &SignedRequest,
    ) -> Result<InboxStored, IdentityError> {
        self.call(
            Capability::IdentityInboxPut,
            &[json(address)?, json(write)?, json(req)?],
        )
        .await
    }

    async fn inbox_list(
        &self,
        address: &InboxAddress,
        auth: &InboxAuth,
        wait_s: u8,
    ) -> Result<Vec<InboxMessage>, IdentityError> {
        self.call(
            Capability::IdentityInboxList,
            &[json(address)?, json(auth)?, wait_s.into()],
        )
        .await
    }

    async fn inbox_ack(
        &self,
        address: &InboxAddress,
        id: &str,
        auth: &InboxAuth,
    ) -> Result<(), IdentityError> {
        self.call(
            Capability::IdentityInboxAck,
            &[json(address)?, id.into(), json(auth)?],
        )
        .await
    }

    async fn get_record(
        &self,
        did: &str,
        req: &SignedRequest,
    ) -> Result<Option<StoredRecord>, IdentityError> {
        self.call(Capability::IdentityGetRecord, &[did.into(), json(req)?])
            .await
    }

    async fn put_record(
        &self,
        did: &str,
        record: &SealedRecord,
        req: &SignedRequest,
    ) -> Result<RecordStored, IdentityError> {
        self.call(
            Capability::IdentityPutRecord,
            &[did.into(), json(record)?, json(req)?],
        )
        .await
    }

    async fn post_request(
        &self,
        did: &str,
        post: &PostedRequest,
        req: &SignedRequest,
    ) -> Result<RequestPosted, IdentityError> {
        self.call(
            Capability::IdentityPostRequest,
            &[did.into(), json(post)?, json(req)?],
        )
        .await
    }

    async fn list_requests(
        &self,
        did: &str,
        since: u64,
        req: &SignedRequest,
    ) -> Result<Vec<DeviceRequest>, IdentityError> {
        self.call(
            Capability::IdentityListRequests,
            &[did.into(), since.into(), json(req)?],
        )
        .await
    }

    async fn delete_request(
        &self,
        did: &str,
        id: &str,
        req: &SignedRequest,
    ) -> Result<(), IdentityError> {
        self.call(
            Capability::IdentityDeleteRequest,
            &[did.into(), id.into(), json(req)?],
        )
        .await
    }
}

/// The error of a call that found no agent language. Once the system languages have `settled`
/// (loaded, or failed to, or the executor runs language-language only) a missing one stays
/// missing, so no store answers: [`IdentityError::NotServed`]. Before that the agent language
/// may still come: [`IdentityError::Unavailable`].
fn no_agent_language(
    cap: Capability,
    error: crate::languages::error::LanguageError,
    settled: bool,
) -> IdentityError {
    match error {
        crate::languages::error::LanguageError::NotFound { .. } if settled => {
            IdentityError::NotServed(cap.wire_name().to_string())
        }
        e => IdentityError::Unavailable(e.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// An executor whose system languages settled without an agent language (language-language
    /// only, or a failed load) has no key index for good: it answers "not served" (no log), not
    /// "try later". While the system languages are still to come, a missing agent language is a
    /// transient failure.
    #[test]
    fn a_settled_executor_without_an_agent_language_has_no_key_index() {
        let missing = || crate::languages::error::LanguageError::NotFound {
            address: "agent_language".into(),
        };
        assert!(matches!(
            no_agent_language(Capability::IdentityKeyOwner, missing(), true),
            IdentityError::NotServed(_)
        ));
        assert!(matches!(
            no_agent_language(Capability::IdentityKeyOwner, missing(), false),
            IdentityError::Unavailable(_)
        ));
    }

    #[test]
    fn the_script_calls_the_export_with_json_arguments() {
        let script = call_script("identityLog", &[json!("did:scid:coasys:1:Qm\"x"), json!(3)]);
        assert!(script.contains(r#"language.identityLog("did:scid:coasys:1:Qm\"x", 3)"#));
        assert!(script.is_ascii());
    }

    /// `identityLog` answering `undefined` is a language off the interface, not a missing log
    /// (`null`), which would read as a store behind a log it signed; a void export's
    /// `undefined` is its `null`.
    #[test]
    fn an_undefined_answer_is_no_null() {
        let script = call_script("identityLog", &[json!("did:scid:coasys:1:Qm"), json!(0)]);
        assert!(script.contains("result === undefined ? { undefined: true } : { ok: result }"));
        let answer = r#"{"undefined": true}"#;
        assert!(matches!(
            read_answer::<Option<LogPage>>(answer, Undefined::Refused),
            Err(IdentityError::Invalid(m)) if m.contains("undefined")
        ));
        read_answer::<()>(answer, Undefined::Null).unwrap();
        assert_eq!(
            read_answer::<Option<Value>>(answer, Undefined::Null).unwrap(),
            None
        );
        assert_eq!(
            read_answer::<Option<LogPage>>(r#"{"ok": null}"#, Undefined::Refused).unwrap(),
            None
        );
    }

    #[test]
    fn a_result_and_each_refusal_read_back() {
        let page: Option<u64> = read_answer(r#"{"ok": null}"#, Undefined::Refused).unwrap();
        assert_eq!(page, None);
        let n: u64 = read_answer(r#"{"ok": 7}"#, Undefined::Refused).unwrap();
        assert_eq!(n, 7);

        let refusal = |code: Value| {
            let text = json!({"err": {"message": code.to_string()}}).to_string();
            read_answer::<Value>(&text, Undefined::Refused).unwrap_err()
        };
        assert_eq!(
            refusal(json!({"code": "key-taken", "key": "z6Mk"})),
            IdentityError::KeyTaken("z6Mk".into())
        );
        assert_eq!(
            refusal(json!({"code": "forbidden", "reason": "secret"})),
            IdentityError::Forbidden(ForbiddenReason::Secret)
        );
        assert_eq!(
            refusal(json!({"code": "gone", "state": "spent"})),
            IdentityError::Gone("spent".into())
        );
        assert_eq!(
            refusal(json!({"code": "not-found"})),
            IdentityError::NotFound
        );
        assert!(matches!(
            refusal(json!({"code": "unavailable"})),
            IdentityError::Unavailable(_)
        ));
        // A thrown error that carries no refusal is the language failing: unavailable.
        assert!(matches!(
            read_answer::<Value>(
                r#"{"err": {"message": "TypeError: boom"}}"#,
                Undefined::Refused
            ),
            Err(IdentityError::Unavailable(m)) if m == "TypeError: boom"
        ));
        assert!(matches!(
            read_answer::<Value>("not json", Undefined::Refused),
            Err(IdentityError::Unavailable(_))
        ));
    }
}
