//! [`IdentityBackend`]: the executor's view of the agent language's identity interface
//! (AR-01 4.8.1, 4.9; AR-02 4.16). The one implementation, [`AgentLanguageBackend`], calls the
//! language at `system_addresses.agent_language` through
//! `LanguageController::agent_language_for`, which refuses any other language. Nothing in the
//! executor calls the store's HTTP routes directly (decision 14.1).

use std::time::Duration;

use ad4m_ldk::identity::{
    AppendRequest, Claim, ClaimReceipt, ForbiddenReason, IdentityErrorCode, LogPage, OfferListing,
    OfferOpened, OfferProgress, RateLimitScope, SignedHead, SignedOffer, SignedRequest,
};
use async_trait::async_trait;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde_json::Value;

use crate::languages::capability::Capability;
use crate::languages::LanguageController;

/// How long one call may take before it counts as `Unavailable` (AR-01 4.8.2 rule 7 asks 5 s
/// for reads; the language's own host timeout is 10 s, so a slow store answers `unavailable`
/// from the language first).
pub const CALL_TIMEOUT: Duration = Duration::from_secs(15);

/// How the identity interface refused or failed (AR-01 4.8.1).
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

/// The agent language's identity calls the executor makes in slice 1.
#[async_trait]
pub trait IdentityBackend: Send + Sync {
    async fn log(&self, did: &str, from: u64) -> Result<Option<LogPage>, IdentityError>;
    async fn append(&self, did: &str, req: &AppendRequest) -> Result<SignedHead, IdentityError>;
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
}

/// The script that calls `export` with `args` (JSON literals) on the language and answers
/// `{"ok": result}` or `{"err": {"message": …}}`. A refusal's message is the refusal as JSON
/// (`IdentityError` in `ad4m-ldk/js`, `IdentityErrorCode` in `ad4m-ldk/rust`).
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
        return {{ ok: result === undefined ? null : result }};
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
    err: Option<EnvelopeError>,
}

#[derive(Deserialize)]
struct EnvelopeError {
    message: String,
}

/// The answer of a [`call_script`] run: the result as `T`, or the refusal it carried.
pub(crate) fn read_answer<T: DeserializeOwned>(text: &str) -> Result<T, IdentityError> {
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
    serde_json::from_value(envelope.ok.unwrap_or(Value::Null))
        .map_err(|e| IdentityError::Invalid(format!("the agent language answered {e}")))
}

/// [`IdentityBackend`] on the agent language.
pub struct AgentLanguageBackend;

impl AgentLanguageBackend {
    async fn call<T: DeserializeOwned>(
        &self,
        cap: Capability,
        args: &[Value],
    ) -> Result<T, IdentityError> {
        let controller = LanguageController::global_instance();
        let language = controller
            .agent_language_for(cap)
            .await
            .map_err(|e| IdentityError::Unavailable(e.to_string()))?;
        if !language.has(cap) {
            return Err(IdentityError::Unavailable(format!(
                "the agent language does not serve {}",
                cap.wire_name()
            )));
        }
        let script = call_script(cap.export_name(), args);
        let run = controller.execute_on_language(language.address(), &script);
        let text = tokio::time::timeout(CALL_TIMEOUT, run)
            .await
            .map_err(|_| IdentityError::Unavailable(format!("{} timed out", cap.wire_name())))?
            .map_err(|e| IdentityError::Unavailable(e.to_string()))?;
        read_answer(&text)
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

    async fn append(&self, did: &str, req: &AppendRequest) -> Result<SignedHead, IdentityError> {
        self.call(Capability::IdentityAppend, &[did.into(), json(req)?])
            .await
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn the_script_calls_the_export_with_json_arguments() {
        let script = call_script("identityLog", &[json!("did:scid:coasys:1:Qm\"x"), json!(3)]);
        assert!(script.contains(r#"language.identityLog("did:scid:coasys:1:Qm\"x", 3)"#));
        assert!(script.is_ascii());
    }

    #[test]
    fn a_result_and_each_refusal_read_back() {
        let page: Option<u64> = read_answer(r#"{"ok": null}"#).unwrap();
        assert_eq!(page, None);
        let n: u64 = read_answer(r#"{"ok": 7}"#).unwrap();
        assert_eq!(n, 7);

        let refusal = |code: Value| {
            let text = json!({"err": {"message": code.to_string()}}).to_string();
            read_answer::<Value>(&text).unwrap_err()
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
            read_answer::<Value>(r#"{"err": {"message": "TypeError: boom"}}"#),
            Err(IdentityError::Unavailable(m)) if m == "TypeError: boom"
        ));
        assert!(matches!(
            read_answer::<Value>("not json"),
            Err(IdentityError::Unavailable(_))
        ));
    }
}
