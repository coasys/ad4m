//! `ad4m-executor init --operator <did>`, `ad4m-executor claim <link>` and
//! `ad4m-executor link-code`: name the identity that operates this executor, and join it by
//! claiming an offer link that identity's app opened, or by showing a device string the app
//! scans.

use std::path::{Path, PathBuf};
use std::time::Duration;

use ad4m_client::ws_rpc::WsRpcClient;
use anyhow::{anyhow, bail, Context, Result};
use serde::Deserialize;
use serde_json::json;

/// The executor database in the data folder `init` and `run` share.
pub fn database_path(data_path: Option<&str>) -> PathBuf {
    let folder = match data_path {
        Some(path) => PathBuf::from(path),
        None => crate::startup::executor_data_path(),
    };
    folder.join("ad4m_db.sqlite")
}

/// Records `did` as the operator while the data folder's record is empty.
pub fn record_operator(db: &Path, did: &str) -> Result<()> {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let record = rust_executor::identity::operator::record_operator_at_init(db, did, now)
        .map_err(|e| anyhow!("{e}"))?;
    println!("This executor's operator is {}.", record.scid);
    Ok(())
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Claimed {
    did: String,
    offer_id: String,
    match_code: String,
    kind: String,
    expires: u64,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Join {
    state: String,
    did: Option<String>,
    key_id: Option<String>,
    seq: Option<u64>,
}

/// How long after the offer's expiry the holder may still confirm
/// (`ad4m_identity_core::link::CONFIRM_GRACE_S`).
const CONFIRM_GRACE_S: u64 = 300;

/// Claims `offer` on the executor at `executor_url` (the operator's credential `token`), prints
/// the match code, and unless `no_wait` waits until the holder confirms or the offer ends.
pub async fn claim(
    executor_url: &str,
    token: &str,
    offer: &str,
    hint: Option<String>,
    no_wait: bool,
) -> Result<()> {
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let mut params = json!({ "offer": offer });
    if let Some(hint) = hint {
        params["hint"] = json!(hint);
    }
    let claimed: Claimed = client.call("identity.claimOffer", params).await?;
    println!("Claimed a {} offer of {}.", claimed.kind, claimed.did);
    println!("Match code: {}", claimed.match_code);
    println!("Confirm on the device that opened the offer that it shows the same code.");
    if no_wait {
        println!(
            "Offer {}: run `ad4m-executor claim` again or call identity.joinStatus to follow it.",
            claimed.offer_id
        );
        return Ok(());
    }
    follow_join(
        &client,
        &claimed.did,
        &claimed.offer_id,
        claimed.expires + CONFIRM_GRACE_S,
    )
    .await
}

fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Follows this executor's claim of `offer_id` (of `did`) until the holder confirms, the offer
/// ends, or `deadline` passes.
async fn follow_join(client: &WsRpcClient, did: &str, offer_id: &str, deadline: u64) -> Result<()> {
    loop {
        let join: Join = client
            .call("identity.joinStatus", json!({ "offerId": offer_id }))
            .await?;
        match join.state.as_str() {
            "claimed" => {}
            "spent" => {
                println!(
                    "Joined {} as {} (event {}).",
                    join.did.unwrap_or_else(|| did.to_string()),
                    join.key_id.unwrap_or_else(|| "?".into()),
                    join.seq.map_or_else(|| "?".to_string(), |s| s.to_string())
                );
                return Ok(());
            }
            other => bail!("the offer ended unconfirmed: it is {other}"),
        }
        if now() > deadline {
            bail!("the holder did not confirm before the offer's confirm window closed");
        }
        tokio::time::sleep(Duration::from_secs(2)).await;
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct PreparedDevice {
    device: String,
    expires: u64,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct LinkTicket {
    did: String,
    offer_id: String,
    match_code: String,
}

/// How long a holder's offer lives after the store takes it
/// (`ad4m_identity_core::link::OFFER_TTL_S`).
const OFFER_TTL_S: u64 = 300;

/// `text` as a QR code of Unicode half blocks, with its quiet zone.
pub fn qr_code(text: &str) -> Result<String> {
    let code = qrcode::QrCode::new(text.as_bytes())
        .map_err(|e| anyhow!("the device string does not fit a QR code: {e}"))?;
    Ok(code
        .render::<qrcode::render::unicode::Dense1x2>()
        .quiet_zone(true)
        .build())
}

/// `ad4m-executor link-code`: asks the executor at `executor_url` (a loopback address) for a
/// fresh device string, prints it with its QR code, and once the executor claimed the offer
/// of the holder that scanned it (`link-ticket`), prints the identity and the match code and
/// follows the claim until the holder confirms.
pub async fn link_code(executor_url: &str, token: &str) -> Result<()> {
    if !is_loopback_url(executor_url) {
        bail!(
            "link-code runs only on the executor's own machine: {executor_url} is not a \
             loopback address (use ws://127.0.0.1:<port>/api/v1/ws)"
        );
    }
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let mut events = client.watch_events(json!({ "link-ticket": null })).await?;
    let prepared: PreparedDevice = client.call("identity.prepareDevice", json!({})).await?;
    println!("Scan this code with an app that holds your master key, or paste the string:");
    println!("{}", qr_code(&prepared.device)?);
    println!("{}", prepared.device);
    println!(
        "It is valid for {} minutes; run `ad4m-executor link-code` again for a new one.",
        prepared.expires.saturating_sub(now()).div_ceil(60)
    );
    let wait = Duration::from_secs(prepared.expires.saturating_sub(now()));
    let ticket = tokio::time::timeout(wait, async {
        loop {
            match events.recv().await {
                Ok(event) if event["type"] == "link-ticket" => {
                    return serde_json::from_value::<LinkTicket>(event).map_err(Into::into);
                }
                Ok(_) | Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => {}
                Err(e) => return Err(anyhow!("the executor closed the connection: {e}")),
            }
        }
    })
    .await
    .map_err(|_| {
        anyhow!(
            "no app's offer was claimed before the device string expired (the executor's log \
             says why it refused one)"
        )
    })??;
    println!("Claimed the offer of {}.", ticket.did);
    println!("Match code: {}", ticket.match_code);
    println!("Confirm on the app that it shows the same code.");
    follow_join(
        &client,
        &ticket.did,
        &ticket.offer_id,
        now() + OFFER_TTL_S + CONFIRM_GRACE_S,
    )
    .await
}

/// The admin credential a subcommand presents: the file `AD4M_ADMIN_CREDENTIAL_FILE` names
/// (mode 0600 or 0400), else `flag` (`--admin-credential` or `AD4M_ADMIN_CREDENTIAL`), else
/// none (an executor started with `--insecure-no-admin-credential`).
pub fn admin_credential(
    flag: Option<String>,
    env: impl Fn(&str) -> Option<String>,
) -> Result<String> {
    if let Some(path) = env("AD4M_ADMIN_CREDENTIAL_FILE").filter(|p| !p.trim().is_empty()) {
        return rust_executor::config_file::read_secret_file(Path::new(&path))
            .map_err(|e| anyhow!("{e}"));
    }
    Ok(flag
        .or_else(|| env("AD4M_ADMIN_CREDENTIAL"))
        .unwrap_or_default())
}

/// Whether `url` names a loopback host: `identity.rekey` answers only there.
fn is_loopback_url(url: &str) -> bool {
    let Ok(url) = url::Url::parse(url) else {
        return false;
    };
    match url.host() {
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        Some(url::Host::Domain(name)) => name == "localhost",
        None => false,
    }
}

#[derive(Deserialize)]
struct DeviceKeys {
    sign: String,
    enc: String,
}

/// `ad4m-executor rekey`: gives the executor at `executor_url` (a loopback address) a new main
/// wallet key, so a revoked server can link again, and prints its new device keys.
pub async fn rekey(executor_url: &str, token: &str) -> Result<()> {
    if !is_loopback_url(executor_url) {
        bail!(
            "rekey runs only on the executor's own machine: {executor_url} is not a loopback \
             address (use ws://127.0.0.1:<port>/api/v1/ws)"
        );
    }
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let keys: DeviceKeys = client.call("identity.rekey", json!({})).await?;
    println!("This executor's device key is now {}.", keys.sign);
    println!("Its encryption key is {}.", keys.enc);
    println!("The old key is kept for history. Link this executor again: show its device string or claim an offer.");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The device string's QR code is Unicode half blocks with a quiet zone, one text line
    /// per two module rows.
    #[test]
    fn a_device_string_prints_as_a_qr_code() {
        let text = qr_code("coasys:device:1:abc").unwrap();
        assert!(text.contains('█') || text.contains('▀') || text.contains('▄'));
        let lines: Vec<&str> = text.lines().collect();
        assert!(lines.len() > 10);
        assert!(lines
            .iter()
            .all(|l| l.chars().count() == lines[0].chars().count()));
    }

    #[test]
    fn rekey_reaches_only_a_loopback_executor() {
        assert!(is_loopback_url("ws://127.0.0.1:12000/api/v1/ws"));
        assert!(is_loopback_url("ws://[::1]:12000/api/v1/ws"));
        assert!(is_loopback_url("ws://localhost:12000/api/v1/ws"));
        assert!(!is_loopback_url("ws://10.0.0.5:12000/api/v1/ws"));
        assert!(!is_loopback_url("wss://executor.example/api/v1/ws"));
        assert!(!is_loopback_url("not a url"));
    }

    #[cfg(unix)]
    #[test]
    fn the_admin_credential_comes_from_its_file_first() {
        use std::os::unix::fs::PermissionsExt;
        let dir = std::env::temp_dir().join(format!("ad4m-cli-credential-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let file = dir.join("credential");
        std::fs::write(&file, "from-file\n").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o600)).unwrap();
        let path = file.to_string_lossy().into_owned();
        let env = |name: &str| match name {
            "AD4M_ADMIN_CREDENTIAL_FILE" => Some(path.clone()),
            "AD4M_ADMIN_CREDENTIAL" => Some("from-env".to_string()),
            _ => None,
        };
        assert_eq!(
            admin_credential(Some("from-flag".into()), env).unwrap(),
            "from-file"
        );
        let no_file =
            |name: &str| (name == "AD4M_ADMIN_CREDENTIAL").then(|| "from-env".to_string());
        assert_eq!(
            admin_credential(Some("from-flag".into()), no_file).unwrap(),
            "from-flag"
        );
        assert_eq!(admin_credential(None, no_file).unwrap(), "from-env");
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert!(
            admin_credential(None, env).is_err(),
            "a group-readable file is refused"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    const A: &str = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";
    const B: &str = "did:scid:coasys:1:QmT78zSuBmuS4z925WZfrqQ1qHaJ56DQaTfyMUF7F8ff5o";

    #[test]
    fn init_records_the_operator_once_and_refuses_another() {
        let dir = std::env::temp_dir().join(format!("ad4m-cli-operator-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let db = database_path(dir.to_str());
        record_operator(&db, A).unwrap();
        record_operator(&db, A).unwrap();
        let err = record_operator(&db, B).unwrap_err().to_string();
        assert!(
            err.contains(&format!("This executor's operator is {A}")),
            "{err}"
        );
        assert!(record_operator(&db, "did:key:z6Mk").is_err());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
