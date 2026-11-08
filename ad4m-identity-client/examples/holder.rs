//! A test holder over JSON lines, for the `tests/js` identity suite: it runs this crate's flows
//! against a real executor through `ad4m-client`'s WebSocket client, with master keys in a
//! `MemoryVault`. One JSON command per stdin line, one JSON answer per stdout line
//! (`{"error": "…"}` on failure).
//!
//! ```text
//! {"cmd":"connect","url":"http://127.0.0.1:12000","token":""}  -> {"ok":true}
//! {"cmd":"create","name":"Home","kind":"desktop-app"}           -> {"did","scid","seq","words","record"}
//! {"cmd":"open","did":"did:scid:…","store":"http://…"}           -> {"link","offerString","offerId"}
//! {"cmd":"openBot","did":"…","store":"http://…","can":["read","write"],"expires":1900000000}
//!                                                    -> {"link","offerString","offerId"}
//! {"cmd":"scan","did":"…","store":"http://…","device":"coasys:device:1:…"} -> {"offerId"}
//! {"cmd":"waitClaim"}                                -> {"matchCode","deviceKey","hint","canKeepMaster"}
//! {"cmd":"confirm","name":"Desk","kind":"desktop-app","keepMaster":true}
//!                                                    -> {"seq","keyId","master","record"}
//! {"cmd":"revoke","did":"did:scid:…","keyIds":["d2"]}            -> {"seq","record"}
//! {"cmd":"sendMaster","did":"did:scid:…","keyId":"d2"}           -> {"id","secret","record"}
//! {"cmd":"takeMaster"}                                          -> {"did","index","record"} | null
//! {"cmd":"record","did":"did:scid:…"}                            -> {"version","record"} | null
//! {"cmd":"recordDevice","did":"…","keyId":"d1","name":"Home","kind":"desktop-app"} -> {"record"}
//! {"cmd":"postRequest","body":{"kind":"send-master"}}           -> {"id"}
//! {"cmd":"requests"}                                            -> [DeviceRequest]
//! {"cmd":"answer","did":"did:scid:…","id":"…"}                   -> {"answered","record"}
//! {"cmd":"rotate","did":"did:scid:…","keyIds":[]}   -> {"index","seq","tries","revoked","pushes","record","kept"}
//! {"cmd":"refreshMaster","did":"did:scid:…"}         -> {"refreshed","index"?,"id"?,"record"?}
//! {"cmd":"removeMaster","did":"did:scid:…"}          -> {"record"}
//! {"cmd":"restore","words":"…","name":"Laptop","kind":"desktop-app","lockOut":["d1"]}
//!        -> {"did","devices","index","seq","tries","revoked","keyId","pushes","record"}
//!           (an enrolment that failed after the rotation: {…,"enrolled":{"error"}} instead of
//!           keyId, pushes and record)
//! ```
//!
//! `scan` is the holder scanning a device string: a bound offer whose ticket goes into the
//! device's pre-join inbox; `waitClaim` and `confirm` then follow it as for `open`. `confirm`
//! names the device's record entry (`kind` defaults to `server` for an `open` offer and to
//! `desktop-app` for a scan) and with `keepMaster` puts the master key into its inbox.
//! `sendMaster` answers the secret too, from the words this holder made it with, so a test
//! can compare what the device takes. `takeMaster` is the device's side, for a holder that
//! connects to the device's executor as its custody service. `record` and `master`/`record`
//! fields answer a record write's version, or `{"error"}`.
//!
//! `create` lists this device in the record as `name` (default `This computer`) of `kind`
//! (default `desktop-app`), holding master #0. It answers the 12 words too, so a test can restore from them on another executor;
//! `rotate` rotates with the words this holder made the identity with. `restore` is a new
//! device's holder: without `lockOut` it locks out the devices ticked by default.
//!
//! Build: `cargo build -p ad4m-identity-client --example holder --features ws,test-vault`.

use std::collections::HashMap;
use std::io::{BufRead, Write};

use ad4m_client::ws_rpc::WsRpcClient;
use ad4m_identity_client::flows::create::{CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::{Claimed, LinkFlow};
use ad4m_identity_client::flows::master::{
    refresh_master, remove_master, send_master, take_master, Refreshed,
};
use ad4m_identity_client::flows::requests::{answer, Answered};
use ad4m_identity_client::flows::restore::RestoreFlow;
use ad4m_identity_client::flows::revoke::revoke;
use ad4m_identity_client::flows::rotate::rotate;
use ad4m_identity_client::record::{self, Change};
use ad4m_identity_client::{ClientError, MemoryVault, OsRandom, SystemClock, ViaExecutor};
use ad4m_identity_core::link::{DeviceString, OfferKind};
use ad4m_identity_core::record::{DeviceEntry, DeviceKind, HoldsMaster};
use ad4m_identity_core::words;
use ad4m_identity_core::BotScope;
use ad4m_ldk::identity::DeviceRequestBody;
use serde_json::{json, Value};

static CLOCK: SystemClock = SystemClock;
static RANDOM: OsRandom = OsRandom;

fn field<'a>(command: &'a Value, name: &str) -> Result<&'a str, String> {
    command[name]
        .as_str()
        .ok_or_else(|| format!("the command needs a string `{name}`"))
}

/// The record entry a command names: `name` (else `name_default`, else "device") and `kind`
/// (else `kind_default`), an executor without the master key.
fn device_entry(
    command: &Value,
    name_default: Option<&str>,
    kind_default: DeviceKind,
) -> Result<DeviceEntry, String> {
    let kind = match command.get("kind") {
        Some(kind) => serde_json::from_value(kind.clone()).map_err(|e| format!("kind: {e}"))?,
        None => kind_default,
    };
    Ok(DeviceEntry {
        name: command["name"]
            .as_str()
            .or(name_default)
            .unwrap_or("device")
            .to_string(),
        kind,
        place: None,
        address: None,
        runs_executor: true,
        holds_master: HoldsMaster::No,
        master_index: None,
        operator: None,
    })
}

/// A record write's outcome as JSON: its version, or `{"error"}`.
fn written(record: &Result<u64, ClientError>) -> Value {
    match record {
        Ok(version) => json!(version),
        Err(e) => json!({ "error": e.to_string() }),
    }
}

/// A step's outcome as JSON: `true`, or `{"error"}`.
fn done(step: &Result<(), ClientError>) -> Value {
    match step {
        Ok(()) => json!(true),
        Err(e) => json!({ "error": e.to_string() }),
    }
}

/// The master #0 secret of each identity this holder made, base58btc, from the words: what
/// `sendMaster` answers so a test can compare what the device takes. The flows never see it.
type Masters = HashMap<String, String>;

/// The words of each identity this holder made, for `rotate`.
type Phrases = HashMap<String, String>;

/// The words this holder made `did` with.
fn words_of(phrases: &Phrases, did: &str) -> Result<words::Words, String> {
    let phrase = phrases
        .get(did)
        .ok_or_else(|| format!("this holder did not make {did}"))?;
    words::parse(phrase).map_err(|e| e.to_string())
}

/// The master-key pushes of a rotation as JSON: the message id, or `{"error"}`, by key id.
fn pushes(pushes: &[(String, Result<String, ClientError>)]) -> Value {
    pushes
        .iter()
        .map(|(key_id, pushed)| {
            let value = match pushed {
                Ok(id) => json!(id),
                Err(e) => json!({ "error": e.to_string() }),
            };
            (key_id.clone(), value)
        })
        .collect::<serde_json::Map<_, _>>()
        .into()
}

async fn create(
    executor: &ViaExecutor<WsRpcClient>,
    vault: &MemoryVault,
    masters: &mut Masters,
    phrases: &mut Phrases,
    this: DeviceEntry,
) -> Result<Value, String> {
    let (mut flow, screen) = CreateFlow::start(&RANDOM);
    let CreateScreen::ShowWords(words) = screen else {
        return Err("the flow did not show the words".into());
    };
    let parsed = words::parse(&words.expose().join(" ")).map_err(|e| e.to_string())?;
    let master = words::master_key(&words::seed(&parsed), 0).map_err(|e| e.to_string())?;
    let CreateScreen::ConfirmWords { positions } =
        flow.words_written(&RANDOM).map_err(|e| e.to_string())?
    else {
        return Err("the flow did not ask for the words".into());
    };
    let words = words.expose();
    flow.confirm([&words[positions[0] - 1], &words[positions[1] - 1]])
        .map_err(|e| e.to_string())?;
    let created = flow
        .finish(executor, vault, this, &SystemClock, &OsRandom)
        .await
        .map_err(|e| e.to_string())?;
    masters.insert(
        created.did.clone(),
        bs58::encode(master.to_bytes()).into_string(),
    );
    let phrase = words.join(" ");
    phrases.insert(created.did.clone(), phrase.clone());
    Ok(json!({
        "did": created.did,
        "scid": created.scid,
        "seq": created.head.seq,
        "words": phrase,
        "record": created.record.as_ref().map(written),
    }))
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
    let mut link: Option<(LinkFlow<'_, WsRpcClient>, DeviceKind)> = None;
    let mut claimed: Option<Claimed> = None;
    let mut masters = Masters::new();
    let mut phrases = Phrases::new();

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
            Some("create") => {
                match device_entry(&command, Some("This computer"), DeviceKind::DesktopApp) {
                    Ok(this) => create(&executor, &vault, &mut masters, &mut phrases, this).await,
                    Err(e) => Err(e),
                }
            }
            Some("rotate") => {
                async {
                    let did = field(&command, "did")?;
                    let ids: Vec<String> = match command.get("keyIds") {
                        Some(ids) => serde_json::from_value(ids.clone())
                            .map_err(|e| format!("keyIds: {e}"))?,
                        None => Vec::new(),
                    };
                    let words = words_of(&phrases, did)?;
                    let rotated = rotate(&executor, &vault, did, &words, &ids, &CLOCK, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({
                        "index": rotated.index,
                        "seq": rotated.head.seq,
                        "tries": rotated.tries,
                        "revoked": rotated.revoked,
                        "pushes": pushes(&rotated.pushes),
                        "record": written(&rotated.record),
                        "kept": written(&rotated.kept.map(u64::from)),
                    }))
                }
                .await
            }
            Some("refreshMaster") => {
                async {
                    let did = field(&command, "did")?;
                    match refresh_master(&executor, &vault, did, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?
                    {
                        Refreshed::Current => Ok(json!({ "refreshed": "current" })),
                        Refreshed::Promoted { index, record } => Ok(json!({
                            "refreshed": "promoted",
                            "index": index,
                            "record": written(&record),
                        })),
                        Refreshed::Unknown => Ok(json!({ "refreshed": "unknown" })),
                        Refreshed::Taken(taken) => Ok(json!({
                            "refreshed": "taken",
                            "index": taken.index,
                            "record": written(&taken.record),
                        })),
                        Refreshed::Asked(id) => Ok(json!({ "refreshed": "asked", "id": id })),
                    }
                }
                .await
            }
            Some("removeMaster") => {
                async {
                    let did = field(&command, "did")?;
                    let version = remove_master(&executor, &vault, did, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({ "record": version }))
                }
                .await
            }
            Some("restore") => {
                async {
                    let typed = field(&command, "words")?;
                    let this = device_entry(&command, Some("restored"), DeviceKind::DesktopApp)?;
                    let mut flow = RestoreFlow::start(&executor, &vault, typed, &CLOCK, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    let did = flow.did().to_string();
                    let devices: Vec<Value> = flow
                        .devices()
                        .iter()
                        .map(|d| {
                            json!({
                                "keyId": d.key_id,
                                "name": d.name,
                                "kind": d.kind,
                                "lockOut": d.lock_out,
                            })
                        })
                        .collect();
                    let lock_out: Vec<String> = match command.get("lockOut") {
                        Some(ids) => serde_json::from_value(ids.clone())
                            .map_err(|e| format!("lockOut: {e}"))?,
                        None => flow
                            .devices()
                            .iter()
                            .filter(|d| d.lock_out)
                            .map(|d| d.key_id.clone())
                            .collect(),
                    };
                    let restored = flow
                        .finish(&lock_out, this)
                        .await
                        .map_err(|e| e.to_string())?;
                    let mut answer = json!({
                        "did": did,
                        "devices": devices,
                        "index": restored.index,
                        "seq": restored.head.seq,
                        "tries": restored.tries,
                        "revoked": restored.revoked,
                    });
                    match &restored.enrolled {
                        Ok(enrolled) => {
                            answer["keyId"] = json!(enrolled.key_id);
                            answer["pushes"] = pushes(&enrolled.pushes);
                            answer["record"] = written(&enrolled.record);
                        }
                        Err(e) => answer["enrolled"] = json!({ "error": e.to_string() }),
                    }
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
                    link = Some((flow, DeviceKind::Server));
                    claimed = None;
                    Ok(answer)
                }
                .await
            }
            Some("openBot") => {
                async {
                    let did = field(&command, "did")?;
                    let store = field(&command, "store")?;
                    let can = command["can"]
                        .as_array()
                        .ok_or("missing can")?
                        .iter()
                        .map(|a| a.as_str().map(str::to_string).ok_or("can holds strings"))
                        .collect::<Result<Vec<_>, _>>()?;
                    let expires = command["expires"].as_u64().ok_or("missing expires")?;
                    let flow = LinkFlow::open_bot(
                        &executor,
                        &vault,
                        did,
                        BotScope { can, expires },
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
                    link = Some((flow, DeviceKind::Bot));
                    claimed = None;
                    Ok(answer)
                }
                .await
            }
            Some("scan") => {
                async {
                    let did = field(&command, "did")?;
                    let store = field(&command, "store")?;
                    let device = DeviceString::parse(field(&command, "device")?)
                        .map_err(|e| e.to_string())?;
                    let flow = LinkFlow::scan(
                        &executor,
                        &vault,
                        did,
                        OfferKind::Device,
                        &device,
                        store,
                        &CLOCK,
                        &RANDOM,
                    )
                    .await
                    .map_err(|e| e.to_string())?;
                    let answer = json!({ "offerId": flow.offer_id() });
                    link = Some((flow, DeviceKind::DesktopApp));
                    claimed = None;
                    Ok(answer)
                }
                .await
            }
            Some("waitClaim") => {
                async {
                    let (flow, _) = link.as_ref().ok_or("open a link first")?;
                    let claim = flow.wait_for_claim().await.map_err(|e| e.to_string())?;
                    let answer = json!({
                        "matchCode": claim.match_code,
                        "deviceKey": claim.device_key,
                        "hint": claim.hint,
                        "canKeepMaster": claim.can_keep_master,
                    });
                    claimed = Some(claim);
                    Ok(answer)
                }
                .await
            }
            Some("confirm") => {
                async {
                    let (flow, kind) = link.as_ref().ok_or("open a link first")?;
                    let claim = claimed.as_ref().ok_or("wait for the claim first")?;
                    let entry = device_entry(&command, claim.hint.as_deref(), *kind)?;
                    let keep = command["keepMaster"].as_bool().unwrap_or(false);
                    let linked = flow
                        .confirm(claim, entry, keep)
                        .await
                        .map_err(|e| e.to_string())?;
                    // Kept until the add is in: a failed publish can be confirmed again while
                    // the claim is still `claimed`.
                    link = None;
                    claimed = None;
                    Ok(json!({
                        "seq": linked.head.seq,
                        "keyId": linked.key_id,
                        "master": linked.master.map(|m| match m {
                            Ok(id) => json!(id),
                            Err(e) => json!({ "error": e.to_string() }),
                        }),
                        "record": written(&linked.record),
                    }))
                }
                .await
            }
            Some("revoke") => {
                async {
                    let did = field(&command, "did")?;
                    let ids: Vec<String> = serde_json::from_value(command["keyIds"].clone())
                        .map_err(|e| format!("keyIds: {e}"))?;
                    let revoked = revoke(&executor, &vault, did, &ids, &CLOCK, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({ "seq": revoked.head.seq, "record": written(&revoked.record) }))
                }
                .await
            }
            Some("sendMaster") => {
                async {
                    let did = field(&command, "did")?;
                    let key_id = field(&command, "keyId")?;
                    let sent = send_master(&executor, &vault, did, key_id, &CLOCK, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({
                        "id": sent.message_id,
                        "secret": masters.get(did),
                        "record": written(&sent.record),
                    }))
                }
                .await
            }
            Some("takeMaster") => match take_master(&executor, &vault, &RANDOM).await {
                Ok(Some(taken)) => Ok(json!({
                    "did": taken.did,
                    "index": taken.index,
                    "record": written(&taken.record),
                })),
                Ok(None) => Ok(Value::Null),
                Err(e) => Err(e.to_string()),
            },
            Some("record") => {
                async {
                    let did = field(&command, "did")?;
                    let opened = record::read(&executor, &vault, did)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(opened.map_or(
                        Value::Null,
                        |o| json!({ "version": o.version, "record": o.record }),
                    ))
                }
                .await
            }
            Some("recordDevice") => {
                async {
                    let did = field(&command, "did")?;
                    let key_id = field(&command, "keyId")?;
                    let entry = device_entry(&command, None, DeviceKind::Server)?;
                    let change = Change::Device {
                        key_id: key_id.to_string(),
                        entry,
                    };
                    let version = record::write(&executor, &vault, did, &[change], &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({ "record": version }))
                }
                .await
            }
            Some("postRequest") => {
                async {
                    let body: DeviceRequestBody = serde_json::from_value(command["body"].clone())
                        .map_err(|e| format!("body: {e}"))?;
                    let id = executor
                        .post_request(&body, None)
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(json!({ "id": id }))
                }
                .await
            }
            Some("requests") => executor
                .requests(0)
                .await
                .map(|r| json!(r))
                .map_err(|e| e.to_string()),
            Some("answer") => {
                async {
                    let did = field(&command, "did")?;
                    let id = field(&command, "id")?;
                    let listed = executor.requests(0).await.map_err(|e| e.to_string())?;
                    let request = listed
                        .iter()
                        .find(|r| r.id == id)
                        .ok_or("no such request")?;
                    match answer(&executor, &vault, did, request, &CLOCK, &RANDOM)
                        .await
                        .map_err(|e| e.to_string())?
                    {
                        Answered::MasterSent(sent) => Ok(json!({
                            "answered": "send-master",
                            "id": sent.message_id,
                            "record": written(&sent.record),
                        })),
                        Answered::Revoked(revoked) => Ok(json!({
                            "answered": "confirm-revoke",
                            "seq": revoked.head.seq,
                            "record": written(&revoked.record),
                        })),
                        Answered::Rotated { rotated, deleted } => Ok(json!({
                            "answered": "confirm-revoke",
                            "seq": rotated.head.seq,
                            "master": rotated.index,
                            "record": written(&rotated.record),
                            "deleted": done(&deleted),
                        })),
                    }
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
