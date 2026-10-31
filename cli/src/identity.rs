//! `ad4m-executor init --operator <did>` and `ad4m-executor claim <link>`: name the identity
//! that operates this executor, and join it by claiming an offer link that identity's app
//! opened.

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
    let deadline = claimed.expires + CONFIRM_GRACE_S;
    loop {
        let join: Join = client
            .call(
                "identity.joinStatus",
                json!({ "offerId": claimed.offer_id }),
            )
            .await?;
        match join.state.as_str() {
            "claimed" => {}
            "spent" => {
                println!(
                    "Joined {} as {} (event {}).",
                    join.did.unwrap_or(claimed.did),
                    join.key_id.unwrap_or_else(|| "?".into()),
                    join.seq.map_or_else(|| "?".to_string(), |s| s.to_string())
                );
                return Ok(());
            }
            other => bail!("the offer ended unconfirmed: it is {other}"),
        }
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        if now > deadline {
            bail!("the holder did not confirm before the offer's confirm window closed");
        }
        tokio::time::sleep(Duration::from_secs(2)).await;
    }
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
