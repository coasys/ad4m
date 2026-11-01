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
//! ```
//!
//! Build: `cargo build -p ad4m-identity-client --example holder --features ws,test-vault`.

use std::io::{BufRead, Write};

use ad4m_client::ws_rpc::WsRpcClient;
use ad4m_identity_client::flows::create::{CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::{Claimed, LinkFlow};
use ad4m_identity_client::flows::revoke::revoke;
use ad4m_identity_client::{MemoryVault, OsRandom, SystemClock, ViaExecutor};
use ad4m_identity_core::link::OfferKind;
use serde_json::{json, Value};

static CLOCK: SystemClock = SystemClock;
static RANDOM: OsRandom = OsRandom;

fn field<'a>(command: &'a Value, name: &str) -> Result<&'a str, String> {
    command[name]
        .as_str()
        .ok_or_else(|| format!("the command needs a string `{name}`"))
}

async fn create(executor: &ViaExecutor<WsRpcClient>, vault: &MemoryVault) -> Result<Value, String> {
    let (mut flow, screen) = CreateFlow::start(&RANDOM);
    let CreateScreen::ShowWords(words) = screen else {
        return Err("the flow did not show the words".into());
    };
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
    Ok(json!({ "did": created.did, "scid": created.scid, "seq": created.head.seq }))
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
            Some("create") => create(&executor, &vault).await,
            Some("open") => {
                async {
                    let did = field(&command, "did")?;
                    let store = field(&command, "store")?;
                    let flow = LinkFlow::open(
                        &executor,
                        &vault,
                        did,
                        OfferKind::Node,
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
