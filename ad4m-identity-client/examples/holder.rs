//! A test holder over JSON lines, for the `tests/js` identity suite: it runs this crate's flows
//! against a real executor through `ad4m-client`'s WebSocket client, with master keys in a
//! `MemoryVault`. One JSON command per stdin line, one JSON answer per stdout line
//! (`{"error": "…"}` on failure).
//!
//! ```text
//! {"cmd":"connect","url":"http://127.0.0.1:12000","token":""}  -> {"ok":true}
//! {"cmd":"create"}                                              -> {"did","scid","seq"}
//! {"cmd":"open","did":"did:scid:…","store":"http://…"}           -> {"link","offerString","offerId"}
//! {"cmd":"waitClaim"}                                           -> {"matchCode","deviceKey","hint"}
//! {"cmd":"confirm"}                                             -> {"seq"}
//! {"cmd":"revoke","did":"did:scid:…","keyIds":["d2"]}            -> {"seq"}
//! {"cmd":"sealMaster","did":"did:scid:…","keyId":"d1"}          -> {"id","secret"}
//! {"cmd":"openBound","did":"…","store":"http://…","device":"coasys:device:1:…"} -> {"offerId","messageId"}
//! ```
//!
//! `sealMaster` seals the current master key to the device `keyId`'s `enc` key in the log and
//! writes it to that device's inbox through the executor (`identity.inboxPut`); it answers the
//! secret so a test can compare what the device takes. `openBound` opens an offer bound to the
//! device string's key and pushes its ticket into the device's pre-join inbox; `waitClaim` and
//! `confirm` then follow it as for `open`.
//!
//! Build: `cargo build -p ad4m-identity-client --example holder --features ws,test-vault`.

use std::collections::HashMap;
use std::io::{BufRead, Write};

use ad4m_client::ws_rpc::WsRpcClient;
use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes};
use ad4m_identity_client::flows::create::{CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::{Claimed, LinkFlow};
use ad4m_identity_client::flows::revoke::revoke;
use ad4m_identity_client::{Clock, ExecutorRpc, MemoryVault, OsRandom, SystemClock, ViaExecutor};
use ad4m_identity_core::build::Signer;
use ad4m_identity_core::inbox::{pre_address, seal, sign_payload, InboxAddress, InboxBody};
use ad4m_identity_core::link::{DeviceString, OfferKind, SignedBy};
use ad4m_identity_core::{signing_bytes, words, KelChain, REQUEST_DOMAIN};
use ad4m_ldk::identity::{InboxBind, InboxWrite, RequestBody, SignedRequest};
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};

static CLOCK: SystemClock = SystemClock;
static RANDOM: OsRandom = OsRandom;

fn field<'a>(command: &'a Value, name: &str) -> Result<&'a str, String> {
    command[name]
        .as_str()
        .ok_or_else(|| format!("the command needs a string `{name}`"))
}

/// The master keys of the identities this holder made, by DID: master #0 from the words, as
/// the vault holds it (the vault never gives its bytes out, and `sealMaster` sends them).
type Masters = HashMap<String, SigningKey>;

async fn create(
    executor: &ViaExecutor<WsRpcClient>,
    vault: &MemoryVault,
    masters: &mut Masters,
) -> Result<Value, String> {
    let (mut flow, screen) = CreateFlow::start(&RANDOM);
    let CreateScreen::ShowWords(words) = screen else {
        return Err("the flow did not show the words".into());
    };
    let parsed = words::parse(&words.join(" ")).map_err(|e| e.to_string())?;
    let master = words::master_key(&words::seed(&parsed), 0).map_err(|e| e.to_string())?;
    let CreateScreen::ConfirmWords { positions } =
        flow.words_written(&RANDOM).map_err(|e| e.to_string())?
    else {
        return Err("the flow did not ask for the words".into());
    };
    flow.confirm([&words[positions[0] - 1], &words[positions[1] - 1]])
        .map_err(|e| e.to_string())?;
    let created = flow
        .finish(executor, vault)
        .await
        .map_err(|e| e.to_string())?;
    masters.insert(created.did.clone(), master);
    Ok(json!({ "did": created.did, "scid": created.scid, "seq": created.head.seq }))
}

/// The log of `did` through the executor, and the store the executor accepted its head from.
async fn log_of(
    executor: &ViaExecutor<WsRpcClient>,
    did: &str,
) -> Result<(KelChain, String), String> {
    let events = executor.events(did).await.map_err(|e| e.to_string())?;
    let chain = ad4m_identity_core::fold(&events).map_err(|e| format!("{e:?}"))?;
    let store = executor
        .log(did, 0, false)
        .await
        .map_err(|e| e.to_string())?
        .head
        .ok_or("the executor accepted no head")?
        .store;
    Ok((chain, store))
}

/// The current master's position at `chain`'s head.
fn by_master(chain: &KelChain) -> Result<SignedBy, String> {
    let head = chain.head();
    Ok(SignedBy {
        key_id: chain
            .state()
            .current_master()
            .ok_or("the identity has ended")?
            .id
            .clone(),
        kel_seq: head.seq,
        kel_hash: head.hash.clone(),
    })
}

/// A store request for `method path` with the canonical JSON of `body`, signed by `master`.
fn master_request(
    master: &SigningKey,
    did: &str,
    store: &str,
    by: &SignedBy,
    method: &str,
    path: &str,
    body: &impl serde::Serialize,
) -> Result<SignedRequest, String> {
    let body = canonical_json(&json!(body)).map_err(|e| e.to_string())?;
    let mut nonce = [0u8; 16];
    rand_core::RngCore::fill_bytes(&mut rand_core::OsRng, &mut nonce);
    let req = RequestBody {
        did: did.to_string(),
        store: store.to_string(),
        key_id: by.key_id.clone(),
        kel_seq: by.kel_seq,
        kel_hash: by.kel_hash.clone(),
        method: method.into(),
        path: path.into(),
        body_hash: hash_bytes(body.as_bytes()),
        nonce: bs58::encode(nonce).into_string(),
        expires: CLOCK.now() + 60,
    };
    let bytes = signing_bytes(REQUEST_DOMAIN, &req).map_err(|e| e.to_string())?;
    let sig = encode_proof_value(&Signer::sign(master, &bytes));
    Ok(SignedRequest { req, sig })
}

/// Seals `body` from `master` (at `by`) to the device `to` (its `enc` key `to_enc`) for
/// `address`.
fn sealed_for(
    master: &SigningKey,
    by: &SignedBy,
    to: &str,
    to_enc: &str,
    address: &InboxAddress,
    body: InboxBody,
) -> Result<String, String> {
    let payload = sign_payload(to.into(), body, CLOCK.now(), by.clone(), master)
        .map_err(|e| e.to_string())?;
    seal(to_enc, address, &payload, &mut rand_core::OsRng).map_err(|e| e.to_string())
}

/// `sealMaster`: the current master key, sealed to device `key_id`'s `enc` key in the log, into
/// that device's inbox.
async fn seal_master(
    executor: &ViaExecutor<WsRpcClient>,
    masters: &Masters,
    did: &str,
    key_id: &str,
) -> Result<Value, String> {
    let master = masters
        .get(did)
        .ok_or("this holder did not make that identity")?;
    let (chain, store) = log_of(executor, did).await?;
    let state = chain.state();
    let device = state.keys.get(key_id).ok_or("the log lists no such key")?;
    let index = state
        .masters
        .iter()
        .position(|m| m.sign == Signer::key(master))
        .ok_or("the log does not list this master key")?;
    let secret = bs58::encode(master.to_bytes()).into_string();
    let by = by_master(&chain)?;
    let scid = chain.id().to_string();
    let address = InboxAddress::Joined {
        scid: scid.clone(),
        key_id: key_id.to_string(),
    };
    let body = InboxBody::Master {
        did: did.to_string(),
        index: index as u32,
        secret: secret.clone(),
    };
    let enc = device
        .enc
        .as_deref()
        .ok_or("the log lists no enc key for it")?;
    let sealed = sealed_for(master, &by, &device.sign, enc, &address, body)?;
    let write = InboxWrite {
        sealed: sealed.clone(),
        bind: None,
    };
    let path = format!("/v1/inbox/{scid}/{key_id}");
    let request = master_request(master, did, &store, &by, "PUT", &path, &write)?;
    let stored = ExecutorRpc::call(
        executor.rpc(),
        "identity.inboxPut",
            json!({ "address": { "scid": scid, "keyId": key_id }, "sealed": sealed, "request": request }),
        )
        .await
        .map_err(|e| e.to_string())?;
    Ok(json!({ "id": stored["id"], "secret": secret }))
}

/// Pushes `flow`'s ticket into the pre-join inbox of the device string `device`.
async fn push_ticket(
    executor: &ViaExecutor<WsRpcClient>,
    masters: &Masters,
    did: &str,
    device: &DeviceString,
    flow: &LinkFlow<'_, WsRpcClient>,
) -> Result<Value, String> {
    let master = masters
        .get(did)
        .ok_or("this holder did not make that identity")?;
    let (chain, store) = log_of(executor, did).await?;
    let by = by_master(&chain)?;
    let pre = pre_address(&device.sign, &device.r).map_err(|e| e.to_string())?;
    let ticket = InboxBody::OfferTicket(Box::new(flow.ticket()));
    let sealed = sealed_for(
        master,
        &by,
        &device.sign,
        &device.enc,
        &InboxAddress::Pre(pre.clone()),
        ticket,
    )?;
    let bind = InboxBind {
        scid: chain.id().to_string(),
        offer_id: flow.offer_id().to_string(),
        sign: device.sign.clone(),
        r: device.r.clone(),
    };
    let write = InboxWrite {
        sealed: sealed.clone(),
        bind: Some(bind.clone()),
    };
    let path = format!("/v1/inbox/pre/{pre}");
    let request = master_request(master, did, &store, &by, "PUT", &path, &write)?;
    let stored = ExecutorRpc::call(
        executor.rpc(),
        "identity.inboxPut",
        json!({ "address": { "pre": pre }, "sealed": sealed, "bind": bind, "request": request }),
    )
    .await
    .map_err(|e| e.to_string())?;
    Ok(json!({ "offerId": flow.offer_id(), "messageId": stored["id"] }))
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let vault = MemoryVault::new();
    let stdin = std::io::stdin();
    let mut out = std::io::stdout();
    let mut executor: Option<ViaExecutor<WsRpcClient>> = None;
    // The open link and its claim; the flow borrows the executor, so both live in this loop's
    // scope and the executor is never replaced while a link is open.
    let mut lines = stdin.lock().lines();

    // `connect` first: everything else needs the executor.
    let reply = |out: &mut std::io::Stdout, value: Value| {
        writeln!(out, "{value}").expect("stdout");
        out.flush().expect("stdout");
    };
    while executor.is_none() {
        let Some(Ok(line)) = lines.next() else { return };
        let command: Value = match serde_json::from_str(&line) {
            Ok(c) => c,
            Err(e) => {
                reply(&mut out, json!({ "error": e.to_string() }));
                continue;
            }
        };
        if command["cmd"] != "connect" {
            reply(&mut out, json!({ "error": "connect first" }));
            continue;
        }
        let url = command["url"].as_str().unwrap_or_default().to_string();
        let token = command["token"].as_str().unwrap_or_default().to_string();
        match WsRpcClient::connect(&url, &token).await {
            Ok(client) => {
                executor = Some(ViaExecutor::new(client));
                reply(&mut out, json!({ "ok": true }));
            }
            Err(e) => reply(&mut out, json!({ "error": e.to_string() })),
        }
    }
    let executor = executor.expect("connected above");
    let mut link: Option<LinkFlow<'_, WsRpcClient>> = None;
    let mut claimed: Option<Claimed> = None;
    let mut masters = Masters::new();

    for line in lines {
        let Ok(line) = line else { return };
        let command: Value = match serde_json::from_str(&line) {
            Ok(c) => c,
            Err(e) => {
                reply(&mut out, json!({ "error": e.to_string() }));
                continue;
            }
        };
        let answer: Result<Value, String> = match command["cmd"].as_str() {
            Some("create") => create(&executor, &vault, &mut masters).await,
            Some("sealMaster") => {
                async {
                    let did = field(&command, "did")?;
                    let key_id = field(&command, "keyId")?;
                    seal_master(&executor, &masters, did, key_id).await
                }
                .await
            }
            Some("openBound") => {
                async {
                    let did = field(&command, "did")?;
                    let store = field(&command, "store")?;
                    let device = DeviceString::parse(field(&command, "device")?)
                        .map_err(|e| e.to_string())?;
                    let flow = LinkFlow::open(
                        &executor,
                        &vault,
                        did,
                        OfferKind::Device,
                        Some(&device.sign),
                        store,
                        &CLOCK,
                        &RANDOM,
                    )
                    .await
                    .map_err(|e| e.to_string())?;
                    let answer = push_ticket(&executor, &masters, did, &device, &flow).await?;
                    link = Some(flow);
                    claimed = None;
                    Ok(answer)
                }
                .await
            }
            Some("open") => {
                async {
                    let did = field(&command, "did")?;
                    let store = field(&command, "store")?;
                    let flow = LinkFlow::open(
                        &executor,
                        &vault,
                        did,
                        OfferKind::Node,
                        None,
                        store,
                        &CLOCK,
                        &RANDOM,
                    )
                    .await
                    .map_err(|e| e.to_string())?;
                    let answer = json!({
                        "link": flow.link(),
                        "offerString": flow.offer_string(),
                        "offerId": flow.offer_id(),
                    });
                    link = Some(flow);
                    claimed = None;
                    Ok(answer)
                }
                .await
            }
            Some("waitClaim") => {
                async {
                    let flow = link.as_ref().ok_or("open a link first")?;
                    let claim = flow.wait_for_claim().await.map_err(|e| e.to_string())?;
                    let answer = json!({
                        "matchCode": claim.match_code,
                        "deviceKey": claim.device_key,
                        "hint": claim.hint,
                    });
                    claimed = Some(claim);
                    Ok(answer)
                }
                .await
            }
            Some("confirm") => {
                async {
                    let flow = link.take().ok_or("open a link first")?;
                    let claim = claimed.take().ok_or("wait for the claim first")?;
                    let head = flow.confirm(&claim).await.map_err(|e| e.to_string())?;
                    Ok(json!({ "seq": head.seq }))
                }
                .await
            }
            Some("revoke") => {
                async {
                    let did = field(&command, "did")?;
                    let ids: Vec<String> = serde_json::from_value(command["keyIds"].clone())
                        .map_err(|e| format!("keyIds: {e}"))?;
                    let head = revoke(&executor, &vault, did, &ids, &CLOCK)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({ "seq": head.seq }))
                }
                .await
            }
            other => Err(format!("unknown command {other:?}")),
        };
        match answer {
            Ok(value) => reply(&mut out, value),
            Err(e) => reply(&mut out, json!({ "error": e })),
        }
    }
}
