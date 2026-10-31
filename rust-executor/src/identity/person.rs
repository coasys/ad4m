//! The person's profile: the profile of an agent that speaks for an identity, published under
//! the identity's `did:scid` as well as under the device's `did:key`, so others see the person.
//!
//! It is one agent record (`PUT /v1/agents/{did:scid}`, the agent language's `identityPutAgent`):
//! - `profile`: the agent's Agent with `did` the `did:scid`, as an expression authored by the
//!   `did:scid` and signed by the device key the way `create_signed_expression` signs, with
//!   `proof.key` the bare device `did:key`;
//! - `anchor`: the log's trusted seq ([`KelCache::trusted`](super::cache::KelCache::trusted));
//! - `sig`: the device key's `coasys:link` signature over `{profile, version, anchor}`;
//! - a signed request by the same device key at the trusted head, for the store that signed it.
//!
//! The store admits it, the agent language answers it with `proof.record`, and
//! `resolver::verify_expression` checks it at the anchor.

use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes};
use ad4m_identity_core::build::Signer;
use ad4m_identity_core::{did_of, signing_bytes, LINK_DOMAIN, REQUEST_DOMAIN};
use ad4m_ldk::identity::{AgentRecord, RequestBody, SignedHead, SignedRequest};
use chrono::{DateTime, SecondsFormat, Utc};
use serde_json::{json, Value};

use super::device::{device_key, WalletSigner};
use super::service::{backend_error, cache_error, db, now, IdentityService};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;

/// How long the signed request of a put stays valid (the store takes at most 300 s).
const REQUEST_TTL_S: i64 = 60;

/// The person's agent record and the signed request that writes it.
pub struct PersonRecord {
    pub record: AgentRecord,
    pub request: SignedRequest,
}

/// The agent record of `profile` under `did` (a `did:scid`), signed by `signer` (the device key
/// `key_id` of the log) and anchored at `head`, the accepted head at the log's trusted seq.
/// `profile` is the Agent JSON of the did:key publish (`did`, `perspective`,
/// `directMessageLanguage`); its `did` becomes `did`, and a missing perspective an empty one.
/// `at` gives the expression's timestamp, the record's version (unix milliseconds) and the
/// request's expiry.
pub fn person_record(
    signer: &WalletSigner,
    did: &str,
    key_id: &str,
    head: &SignedHead,
    profile: &Value,
    at: DateTime<Utc>,
) -> Result<PersonRecord, String> {
    let device = format!("did:key:{}", signer.key());
    let mut data = profile.clone();
    let fields = data
        .as_object_mut()
        .ok_or("the agent's profile is not an object")?;
    fields.insert("did".into(), did.into());
    if !fields
        .get("perspective")
        .and_then(|p| p.get("links"))
        .is_some_and(Value::is_array)
    {
        fields.insert("perspective".into(), json!({ "links": [] }));
    }
    // Readers get the profile through the agent language, which drops every null member: sign
    // what they will see.
    let data = crate::js_core::utils::sort_json_value(&without_nulls(&data));
    let signature = signer.sign(&crate::agent::signatures::hash_data_and_timestamp(
        &data, &at,
    ));
    let expression = json!({
        "author": did,
        "timestamp": at.to_rfc3339_opts(SecondsFormat::Millis, true),
        "data": data,
        "proof": { "key": device, "signature": hex::encode(signature) },
    });
    let mut record = AgentRecord {
        did: did.to_string(),
        profile: expression,
        version: u64::try_from(at.timestamp_millis()).map_err(|e| e.to_string())?,
        signed_by: device,
        anchor: Some(head.seq),
        sig: String::new(),
    };
    let bytes = signing_bytes(LINK_DOMAIN, &record.signed_value()).map_err(|e| e.to_string())?;
    record.sig = encode_proof_value(&signer.sign(&bytes));

    // The agent language sends the record as its canonical JSON.
    let body = serde_json::to_value(&record).map_err(|e| e.to_string())?;
    let body = canonical_json(&body).map_err(|e| e.to_string())?;
    let req = RequestBody {
        did: did.to_string(),
        store: head.store.clone(),
        key_id: key_id.to_string(),
        kel_seq: head.seq,
        kel_hash: head.hash.clone(),
        method: "PUT".into(),
        path: format!("/v1/agents/{did}"),
        body_hash: hash_bytes(body.as_bytes()),
        nonce: bs58::encode(rand::random::<[u8; 16]>()).into_string(),
        expires: u64::try_from(at.timestamp() + REQUEST_TTL_S).map_err(|e| e.to_string())?,
    };
    let bytes = signing_bytes(REQUEST_DOMAIN, &req).map_err(|e| e.to_string())?;
    let sig = encode_proof_value(&signer.sign(&bytes));
    signer.check()?;
    Ok(PersonRecord {
        record,
        request: SignedRequest { req, sig },
    })
}

/// `value` without its null members, at any depth.
fn without_nulls(value: &Value) -> Value {
    match value {
        Value::Object(map) => Value::Object(
            map.iter()
                .filter(|(_, v)| !v.is_null())
                .map(|(k, v)| (k.clone(), without_nulls(v)))
                .collect(),
        ),
        Value::Array(items) => Value::Array(items.iter().map(without_nulls).collect()),
        other => other.clone(),
    }
}

impl IdentityService {
    /// Publishes `profile` (the Agent JSON of `agent`'s did:key publish) as the person's profile
    /// when `agent` speaks for an identity: bound to it in `identity_agents`, its device key
    /// listed and valid at the log's trusted seq, without the bot mark. Answers the `did:scid`
    /// it wrote under, or `None` when the agent speaks for no one there.
    pub async fn publish_person_profile(
        &self,
        agent: &AgentContext,
        profile: &Value,
    ) -> Result<Option<String>, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(None);
        };
        let Some((chain, trusted)) = self.cache.trusted(&scid).await.map_err(cache_error)? else {
            return Ok(None);
        };
        let state = chain.state();
        let Some(key_id) = super::resolver::device_key_id(state, &key).filter(|id| {
            state.bot_scope_at(id, trusted).is_none() && state.key_valid_at_time(id, trusted, now())
        }) else {
            return Ok(None);
        };
        let head = self
            .cache
            .accepted_head(&scid)
            .map_err(cache_error)?
            .filter(|head| head.seq == trusted)
            .ok_or_else(|| WsRpcError::internal("the accepted head moved while publishing"))?;
        let did = did_of(&scid);
        let signer = WalletSigner::new(agent).map_err(WsRpcError::internal)?;
        let put = person_record(&signer, &did, &key_id, &head, profile, Utc::now())
            .map_err(WsRpcError::internal)?;
        self.backend
            .put_agent(&put.record, Some(&put.request))
            .await
            .map_err(backend_error)?;
        Ok(Some(did))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;
    use crate::identity::backend::IdentityBackend;
    use crate::identity::cache::tests::{accept_signed, trusting, FakeStore, TestLog};
    use crate::identity::device::enc_key_of;
    use ad4m_hash_chain::decode_proof_value;
    use ad4m_identity_core::build::{add_unsigned, cosign, revoke};
    use ad4m_identity_core::verify_signed;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    /// A managed agent with a stored profile (one link), a person's log whose `add` at seq 1
    /// lists the agent's key as d2, and a service over a cache trusting the fake store.
    struct World {
        service: IdentityService,
        store: Arc<FakeStore>,
        agent: AgentContext,
        agent_did: String,
        key: String,
        log: TestLog,
    }

    impl World {
        fn new(seed: u8) -> World {
            crate::test_utils::setup_wallet();
            crate::test_utils::setup_agent();
            let agent_did = crate::test_utils::create_test_user(&format!(
                "person-{}@example.org",
                uuid::Uuid::new_v4().simple()
            ));
            let agent = AgentContext::managed(agent_did.clone());
            let (_, key) = device_key(&agent).unwrap();
            let profile: crate::types::Agent = serde_json::from_value(json!({
                "did": agent_did,
                "directMessageLanguage": "lang://dm",
                "perspective": { "links": [{
                    "author": agent_did,
                    "timestamp": "2026-10-30T00:00:00.000Z",
                    "data": { "source": agent_did, "predicate": "rdf://name", "target": "literal://string:Ada" },
                    "proof": { "key": format!("{agent_did}#key"), "signature": "00", "valid": true },
                }]},
            }))
            .unwrap();
            AgentService::store_managed_agent_profile(&agent_did, &profile).unwrap();

            let mut log = TestLog::new(seed);
            let chain = log.chain();
            let device = ad4m_identity_core::DeviceKey {
                id: chain.state().next_device_id(),
                enc: enc_key_of(&key).unwrap(),
                sign: key.clone(),
                life: ad4m_identity_core::KeyLife::Standing,
            };
            let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
            cosign(&mut add, "m0", &log.master).unwrap();
            log.events.push(add);

            let store = Arc::new(FakeStore::new(vec![]));
            let backend: Arc<dyn IdentityBackend> = store.clone();
            let cache = Arc::new(trusting());
            let service = IdentityService {
                resolver: Arc::new(crate::identity::resolver::Resolver::new(
                    cache.clone(),
                    backend.clone(),
                    None,
                    false,
                )),
                backend,
                cache,
                store_url: None,
                challenges: Default::default(),
            };
            World {
                service,
                store,
                agent,
                agent_did,
                key,
                log,
            }
        }

        fn did(&self) -> String {
            did_of(&self.log.scid())
        }

        /// The store signs the whole log, and the agent is bound to the person.
        async fn bind(&self) {
            accept_signed(&self.service.cache, &self.log.events).await;
            let id = self
                .service
                .record_agent_if_listed(&self.agent, &self.log.scid())
                .await
                .unwrap();
            assert_eq!(id.as_deref(), Some("d2"));
        }

        fn puts(&self) -> Vec<(AgentRecord, Option<SignedRequest>)> {
            self.store.puts.lock().unwrap().clone()
        }

        /// A profile publish whose did:key publish succeeds (or fails, when `fail`); answers
        /// the publish's result and the Agent JSON the did:key publish was given.
        async fn publish(&self, fail: bool) -> (Result<(), deno_core::anyhow::Error>, Value) {
            let sent = Mutex::new(Value::Null);
            let into = &sent;
            let result = AgentService::publish_profile(
                &self.agent,
                Some(&self.service),
                move |json| async move {
                    *into.lock().unwrap() = json;
                    if fail {
                        Err(deno_core::anyhow::anyhow!("the did:key publish failed"))
                    } else {
                        Ok(())
                    }
                },
            )
            .await;
            let sent = sent.lock().unwrap().clone();
            (result, sent)
        }
    }

    /// Binding an agent publishes the person's profile: the agent's profile under the did:scid,
    /// a record anchored at the trusted seq and signed by the device key, and a signed request
    /// by that key at the trusted head, which every reader accepts. Only the first binding does.
    #[tokio::test]
    async fn binding_an_agent_publishes_the_persons_profile_at_the_trusted_seq() {
        let world = World::new(81);
        world.bind().await;
        let puts = world.puts();
        assert_eq!(puts.len(), 1, "one person's profile on binding");
        let (record, request) = &puts[0];
        let did = world.did();

        assert_eq!(record.did, did);
        assert_eq!(record.anchor, Some(1), "anchored at the trusted seq");
        assert_eq!(record.signed_by, world.agent_did);
        let profile = &record.profile;
        assert_eq!(profile["author"], json!(did));
        assert_eq!(profile["proof"]["key"], json!(world.agent_did));
        assert_eq!(profile["data"]["did"], json!(did));
        assert_eq!(profile["data"]["directMessageLanguage"], json!("lang://dm"));
        let links = profile["data"]["perspective"]["links"].as_array().unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0]["data"]["target"], json!("literal://string:Ada"));
        assert!(
            links[0]["proof"].get("valid").is_none(),
            "published without link decorations"
        );
        let sig = decode_proof_value(&record.sig).unwrap();
        assert!(verify_signed(
            LINK_DOMAIN,
            &record.signed_value(),
            &world.key,
            &sig
        ));

        // The signed request: the device key d2 at the trusted head, for the store that signed
        // it, covering the record as the agent language sends it.
        let request = request
            .as_ref()
            .expect("a did:scid record has a signed request");
        let head = world
            .service
            .cache
            .accepted_head(&world.log.scid())
            .unwrap()
            .unwrap();
        let req = &request.req;
        assert_eq!(
            (req.did.as_str(), req.key_id.as_str(), req.kel_seq),
            (did.as_str(), "d2", 1)
        );
        assert_eq!((&req.kel_hash, &req.store), (&head.hash, &head.store));
        assert_eq!(
            (req.method.as_str(), req.path.clone()),
            ("PUT", format!("/v1/agents/{did}"))
        );
        let body = canonical_json(&serde_json::to_value(record).unwrap()).unwrap();
        assert_eq!(req.body_hash, hash_bytes(body.as_bytes()));
        assert!(req.expires > now() && req.expires <= now() + 300);
        let sig = decode_proof_value(&request.sig).unwrap();
        assert!(verify_signed(REQUEST_DOMAIN, req, &world.key, &sig));

        // As the agent language answers it (with `proof.record`; its expressionGet shim drops
        // every null member on the way to JS), the executor's reader admits the profile for
        // the person at its anchor, the data sorted as the reader sorts it. The record holds no
        // null member, so the reader re-serializes exactly the profile that was signed.
        assert_eq!(&without_nulls(profile), profile, "no null member to drop");
        let mut expression: crate::types::Expression<Value> =
            serde_json::from_value(profile.clone()).unwrap();
        expression.data = crate::js_core::utils::sort_json_value(&expression.data);
        assert_eq!(&serde_json::to_value(&expression).unwrap(), profile);
        let proof = json!({ "version": record.version, "anchor": 1, "sig": record.sig });
        assert!(crate::identity::resolver::verify_expression_at(
            &world.service.cache,
            None,
            &expression,
            &proof,
            now()
        )
        .await
        .unwrap());

        // A later binding check of the same agent publishes nothing more.
        world
            .service
            .record_agent_if_listed(&world.agent, &world.log.scid())
            .await
            .unwrap();
        assert_eq!(world.puts().len(), 1);
    }

    /// A profile publish writes the did:key profile always, and the person's profile only while
    /// the agent speaks for the person at the trusted seq.
    #[tokio::test]
    async fn a_profile_publish_writes_the_persons_profile_only_while_bound() {
        let mut world = World::new(82);
        let (result, sent) = world.publish(false).await;
        result.unwrap();
        assert_eq!(sent["did"], json!(world.agent_did), "the did:key profile");
        assert!(world.puts().is_empty(), "unbound: no person's profile");

        world.bind().await;
        let (result, sent) = world.publish(false).await;
        result.unwrap();
        assert_eq!(sent["did"], json!(world.agent_did));
        let puts = world.puts();
        assert_eq!(puts.len(), 2, "one on binding, one on the publish");
        assert_eq!(puts[1].0.did, world.did());
        assert_eq!(puts[1].0.anchor, Some(1));
        assert!(puts[1].0.version >= puts[0].0.version);

        // Revoked at the trusted seq: the key speaks for no one there, so only the did:key
        // profile goes out.
        let event = revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(event);
        accept_signed(&world.service.cache, &world.log.events).await;
        let (result, _) = world.publish(false).await;
        result.unwrap();
        assert_eq!(world.puts().len(), 2);
    }

    /// The person's profile failing leaves the did:key publish successful, and the next publish
    /// writes it; a failed did:key publish fails the publish and writes no person's profile.
    #[tokio::test]
    async fn a_failed_persons_profile_leaves_the_did_key_publish_successful() {
        let world = World::new(83);
        world.store.put_down.store(true, Ordering::SeqCst);
        world.bind().await;
        assert!(world.puts().is_empty());

        let calls = AtomicUsize::new(0);
        let counter = &calls;
        let result = AgentService::publish_profile(
            &world.agent,
            Some(&world.service),
            move |_| async move {
                counter.fetch_add(1, Ordering::SeqCst);
                Ok(())
            },
        )
        .await;
        result.expect("the did:key publish stands when the person's profile fails");
        assert_eq!(calls.load(Ordering::SeqCst), 1);
        assert!(world.puts().is_empty());

        world.store.put_down.store(false, Ordering::SeqCst);
        let (result, _) = world.publish(false).await;
        result.unwrap();
        assert_eq!(world.puts().len(), 1, "the next publish writes it");

        let (result, _) = world.publish(true).await;
        assert!(result.is_err());
        assert_eq!(world.puts().len(), 1);
    }
}
