//! Agent profiles on the shared store's agent records, in place of
//! the Node server at mock.ad4m.dev.
//!
//! - `create` publishes the running agent's profile under its `did:key`. The host signs the
//!   Agent as an AD4M expression (`createSignedExpression`: hex Ed25519 signature of
//!   `sha256(json(data) || timestamp)` by the agent's did:key), and that whole signed
//!   expression is the record's `profile`. The record's `sig` is the agent key's signature
//!   (through the host's `agentSign`) of the `coasys:link` bytes of
//!   `{profile, version, anchor: null}`, so it covers the signed expression too. Sent as
//!   `PUT /v1/agents/{did}` with no signed request.
//! - `get` reads `GET /v1/agents/{did}` and keeps the record only when its signature holds:
//!   for a `did:key`, by that key; for a `did:scid`, by a key that the log (read from the store
//!   and replayed here with the identity core, every event's signatures checked) lists, valid
//!   and without the bot mark at the record's anchor: the seq of that log the signer knew when
//!   it signed, at or below the head. A key revoked at seq N signs nothing valid anchored at or
//!   after N; a record the store took before the revocation stays valid. The store also
//!   requires `signedBy` valid at the log's head when a record arrives, so a revoked key
//!   cannot backdate a new record past it; the read relies on that admission for arrival
//!   order. Any other record is ignored (`None`).
//!   A record's profile is the agent's signed profile expression, answered only when its
//!   author is the record's did and its data is an Agent naming that did (the executor checks
//!   the expression's signature on every read). A `did:key` record answers it as stored. A
//!   `did:scid` record answers it exactly as stored plus `proof.record` (`{version, anchor,
//!   sig}` of the record), so the executor can check the record itself at its anchor rather
//!   than the expression's key at the log's head: removing `proof.record` gives back the
//!   record's profile, which `sig` covers, and `proof.key` is the record's `signedBy`. A
//!   `did:scid` profile that is not exactly an expression of that form is ignored.

use ad4m_hash_chain::{decode_ed25519_multikey, decode_proof_value, encode_proof_value};
use ad4m_identity_core::{fold_json, verify_signed, KelChain, LINK_DOMAIN};
use ad4m_ldk::identity::{AgentRecord, AgentRecordProof, IdentityErrorCode, LogPage};
use ad4m_ldk::prelude::{Expression, LanguageError, LanguageResult};
use serde_json::Value;

use crate::store::{segment, Http, Store};

/// The running agent, as the host gives it.
pub trait AgentKey {
    fn did(&self) -> LanguageResult<String>;
    /// Ed25519 signature of `bytes` by the agent's key.
    fn sign(&self, bytes: &[u8]) -> LanguageResult<Vec<u8>>;
    /// Now, in unix milliseconds.
    fn now_ms(&self) -> u64;
    /// `data` as an AD4M expression signed by the agent's key, as the host's
    /// `createSignedExpression` makes it: the expression the executor's read path verifies.
    fn signed_expression(&self, data: &Value) -> LanguageResult<Expression>;
}

fn signature_holds(record: &AgentRecord, key: &str) -> bool {
    decode_proof_value(&record.sig)
        .is_some_and(|sig| verify_signed(LINK_DOMAIN, &record.signed_value(), key, &sig))
}

fn invalid(reason: impl Into<String>) -> LanguageError {
    IdentityErrorCode::Invalid {
        reason: reason.into(),
    }
    .into()
}

/// The agent profile from an Agent object, as `expressionCreate` receives it.
pub fn profile_of(mut content: Value, did: &str) -> LanguageResult<Value> {
    let has_links = content
        .get("perspective")
        .and_then(|p| p.get("links"))
        .is_some_and(Value::is_array);
    let named = content
        .get("did")
        .and_then(Value::as_str)
        .map(str::to_string);
    let Some(named) = named.filter(|_| has_links) else {
        return Err(LanguageError::invalid_input(
            "Content must be an Agent object",
        ));
    };
    if named != did {
        return Err(LanguageError::permission_denied(
            "Can't set Agent Expression for foreign DID - only for self",
        ));
    }
    let object = content
        .as_object_mut()
        .ok_or_else(|| LanguageError::invalid_input("Content must be an Agent object"))?;
    if object.get("directMessageLanguage").is_none() {
        object.insert("directMessageLanguage".into(), Value::Null);
    }
    if let Some(links) = object
        .get_mut("perspective")
        .and_then(|p| p.get_mut("links"))
        .and_then(Value::as_array_mut)
    {
        for link in links {
            if let Some(proof) = link.get_mut("proof").and_then(Value::as_object_mut) {
                proof.remove("valid");
                proof.remove("invalid");
            }
        }
    }
    Ok(content)
}

/// A `did:key` record of `agent` at `version`; its profile is `expression`, the agent's signed
/// profile expression.
pub fn did_key_record(
    agent: &impl AgentKey,
    expression: &Expression,
    version: u64,
) -> LanguageResult<AgentRecord> {
    let did = agent.did()?;
    let profile = serde_json::to_value(expression).map_err(|_| invalid("profile"))?;
    let mut record = AgentRecord {
        did: did.clone(),
        profile,
        version,
        signed_by: did,
        anchor: None,
        sig: String::new(),
    };
    let bytes = ad4m_identity_core::signing_bytes(LINK_DOMAIN, &record.signed_value())
        .map_err(|_| invalid("profile"))?;
    let sig: [u8; 64] = agent
        .sign(&bytes)?
        .try_into()
        .map_err(|_| LanguageError::internal("the agent's signature is not 64 bytes"))?;
    record.sig = encode_proof_value(&sig);
    Ok(record)
}

/// `expressionCreate`: publishes the agent's profile; answers its did.
pub async fn create<H: Http>(
    store: &Store<H>,
    agent: &impl AgentKey,
    content: Value,
) -> LanguageResult<String> {
    let did = agent.did()?;
    if !did.starts_with("did:key:") {
        return Err(invalid(
            "an agent with a log publishes through identityPutAgent",
        ));
    }
    let profile = profile_of(content, &did)?;
    let mut expression = agent.signed_expression(&profile)?;
    if expression.author != did {
        return Err(LanguageError::internal(
            "the host signed the profile as another agent",
        ));
    }
    expression.proof.valid = None;
    expression.proof.invalid = None;
    let mut version = agent.now_ms();
    for attempt in 0..2 {
        let record = did_key_record(agent, &expression, version)?;
        let body = serde_json::to_value(&record).map_err(|_| invalid("malformed"))?;
        let reply = store
            .send(
                "PUT",
                &format!("/v1/agents/{}", segment(&did)),
                crate::store::Send {
                    body: Some(body),
                    ..Default::default()
                },
            )
            .await?;
        if (200..300).contains(&reply.status) {
            return Ok(did);
        }
        // The store holds a newer version (another clock): publish just above it, once.
        let stored = (reply.status == 409)
            .then(|| serde_json::from_str::<Value>(&reply.body).ok())
            .flatten()
            .and_then(|b| b.get("version").and_then(Value::as_u64));
        match stored {
            Some(stored) if attempt == 0 => version = stored + 1,
            _ => return Err(crate::store::refusal_of(reply.status, &reply.body).into()),
        }
    }
    Err(IdentityErrorCode::Unavailable.into())
}

/// `expressionGet`: the agent's profile as an expression, or `None`.
pub async fn get<H: Http>(
    store: &Store<H>,
    did: &str,
    now_s: u64,
) -> LanguageResult<Option<Expression>> {
    let record: Option<AgentRecord> = store
        .read(&format!("/v1/agents/{}", segment(did)), Vec::new())
        .await?;
    let Some(record) = record.filter(|r| r.did == did) else {
        return Ok(None);
    };
    if !verified(store, &record, now_s).await? {
        return Ok(None);
    }
    if record.did.starts_with("did:key:") {
        return Ok(signed_profile_of(&record));
    }
    Ok(scid_profile_of(&record))
}

/// The signed profile expression a `did:key` record carries, when it is this agent's (see
/// [`own_profile`]). The language does not judge the expression's own signature: the executor
/// verifies it on every read.
fn signed_profile_of(record: &AgentRecord) -> Option<Expression> {
    let mut expression = own_profile(record)?;
    expression.proof.valid = None;
    expression.proof.invalid = None;
    expression.proof.record = None;
    Some(expression)
}

/// The record's profile as an expression, when it is this agent's: authored by the record's
/// did, its data an Agent naming that did.
fn own_profile(record: &AgentRecord) -> Option<Expression> {
    let expression: Expression = serde_json::from_value(record.profile.clone()).ok()?;
    let agent_did = expression.data.get("did").and_then(Value::as_str);
    let has_links = expression
        .data
        .get("perspective")
        .and_then(|p| p.get("links"))
        .is_some_and(Value::is_array);
    if expression.author != record.did || agent_did != Some(record.did.as_str()) || !has_links {
        return None;
    }
    Some(expression)
}

/// The profile expression a `did:scid` record carries, with the record's binding added as
/// `proof.record`. Kept only when the stored profile is exactly such an expression (no field
/// the expression type would drop, no `proof.valid`, `proof.invalid` or `proof.record`) and its
/// `proof.key` is the record's `signedBy`: then the answer minus `proof.record` is the record's
/// profile, and the record signature can be checked over it.
fn scid_profile_of(record: &AgentRecord) -> Option<Expression> {
    let anchor = record.anchor?;
    let mut expression = own_profile(record)?;
    let proof = &expression.proof;
    if proof.valid.is_some()
        || proof.invalid.is_some()
        || proof.record.is_some()
        || proof.key != record.signed_by
        || serde_json::to_value(&expression).ok()? != record.profile
    {
        return None;
    }
    expression.proof.record = Some(AgentRecordProof {
        version: record.version,
        anchor,
        sig: record.sig.clone(),
    });
    Some(expression)
}

async fn verified<H: Http>(
    store: &Store<H>,
    record: &AgentRecord,
    now_s: u64,
) -> LanguageResult<bool> {
    if let Some(key) = record.did.strip_prefix("did:key:") {
        return Ok(decode_ed25519_multikey(key).is_some()
            && record.signed_by == record.did
            && record.anchor.is_none()
            && signature_holds(record, key));
    }
    if !record.did.starts_with(ad4m_identity_core::DID_PREFIX) {
        return Ok(false);
    }
    let (Some(anchor), Some(key)) = (record.anchor, record.signed_by.strip_prefix("did:key:"))
    else {
        return Ok(false);
    };
    if !signature_holds(record, key) {
        return Ok(false);
    }
    let Some(chain) = read_log(store, &record.did).await? else {
        return Ok(false);
    };
    if anchor > chain.head().seq {
        return Ok(false);
    }
    let state = chain.state();
    Ok(state.key_id_of(&key.to_string()).is_some_and(|id| {
        state.bot_scope_at(id, anchor).is_none() && state.key_valid_at_time(id, anchor, now_s)
    }))
}

/// The whole log of `did` from the store, replayed (every event's signatures checked).
/// `None` when the store holds none or the events do not replay.
pub async fn read_log<H: Http>(store: &Store<H>, did: &str) -> LanguageResult<Option<KelChain>> {
    let mut events: Vec<Value> = Vec::new();
    loop {
        let Some(page): Option<LogPage> = store.log(did, events.len() as u64).await? else {
            return Ok(None);
        };
        let done = page.events.is_empty()
            || events.len() as u64 + page.events.len() as u64 > page.head.seq;
        events.extend(page.events);
        if done {
            break;
        }
    }
    Ok(fold_json(&events).ok())
}

/// `version` (unix milliseconds) as an ISO 8601 timestamp, `YYYY-MM-DDTHH:MM:SS.mmmZ`.
pub fn iso_time_ms(ms: u64) -> String {
    let secs = ms / 1000;
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    // Civil date from days since 1970-01-01 (Howard Hinnant's algorithm).
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = yoe + era * 400 + i64::from(month <= 2);
    format!(
        "{year:04}-{month:02}-{day:02}T{:02}:{:02}:{:02}.{:03}Z",
        rem / 3600,
        rem % 3600 / 60,
        rem % 60,
        ms % 1000
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn iso_time_matches_known_dates() {
        assert_eq!(iso_time_ms(0), "1970-01-01T00:00:00.000Z");
        assert_eq!(iso_time_ms(1_800_000_000_123), "2027-01-15T08:00:00.123Z");
        assert_eq!(iso_time_ms(951_782_400_000), "2000-02-29T00:00:00.000Z");
    }

    #[test]
    fn a_profile_drops_link_verdicts_and_refuses_a_foreign_did() {
        let content = json!({
            "did": "did:key:z6MkA",
            "perspective": { "links": [{ "proof": { "signature": "s", "valid": true, "invalid": false } }] }
        });
        let profile = profile_of(content.clone(), "did:key:z6MkA").unwrap();
        assert_eq!(
            profile["perspective"]["links"][0]["proof"],
            json!({ "signature": "s" })
        );
        assert_eq!(profile["directMessageLanguage"], Value::Null);
        assert!(profile_of(content, "did:key:z6MkB").is_err());
        assert!(profile_of(json!({ "did": "did:key:z6MkA" }), "did:key:z6MkA").is_err());
    }
}
