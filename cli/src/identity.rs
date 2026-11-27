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

/// `ad4m-executor init [--operator <did>]`: `init` runs the executor's init (`data_path`,
/// `seed`). On a data folder `init` made before (it holds the seed), `--operator` records (or refuses) the operator
/// first, and without `--network-bootstrap-seed` it stops there: the folder keeps its seed
/// and everything else, and a refused operator changes nothing. A folder with the database but
/// no seed has the operator checked (or recorded) before `init` writes the seed. On a new folder
/// `init` makes it, then the operator is recorded.
pub fn init_with_operator(
    data_path: Option<String>,
    seed: Option<String>,
    operator: Option<&str>,
    init: impl FnOnce(Option<String>, Option<String>) -> Result<()>,
) -> Result<()> {
    let db = database_path(data_path.as_deref());
    // Initialized: `init` wrote the seed.
    let existing = db
        .parent()
        .is_some_and(|dir| dir.join("mainnet_seed.seed").exists());
    // A database without the seed may hold an operator record already: check it before `init`
    // writes the seed, so a refused operator changes nothing there either.
    let check_first = existing || db.exists();
    if let (Some(operator), true) = (operator, check_first) {
        record_operator(&db, operator)?;
        if existing && seed.is_none() {
            return Ok(());
        }
    }
    init(data_path, seed)?;
    if let (Some(operator), false) = (operator, check_first) {
        record_operator(&db, operator)?;
    }
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

/// How far past its issue an offer expires at most (`ad4m_identity_core::link::OFFER_TTL_S`).
const OFFER_TTL_S: u64 = 300;

/// Claims `offer` on the executor at `executor_url` (the operator's credential `token`), prints
/// the match code, and unless `no_wait` waits until the holder confirms or the offer ends.
///
/// A plaintext executor that is not on loopback is refused unless
/// `allow_insecure_remote` is set. [`ad4m_client::connect::refuse_insecure_remote`]
/// runs first, before the credential is placed in the socket URL.
pub async fn claim(
    executor_url: &str,
    token: &str,
    offer: &str,
    hint: Option<String>,
    no_wait: bool,
    allow_insecure_remote: bool,
) -> Result<()> {
    let executor_url = ad4m_client::connect::normalize_base_url(executor_url);
    ad4m_client::connect::refuse_insecure_remote(&executor_url, allow_insecure_remote)?;
    // The offer was issued before now, so its confirm window closes at most this far ahead:
    // neither the handshake nor the claim may outlast it.
    let window = tokio::time::Instant::now() + Duration::from_secs(OFFER_TTL_S + CONFIRM_GRACE_S);
    let closed = || anyhow!("the executor did not answer before the offer's confirm window closed");
    let client = tokio::time::timeout_at(window, WsRpcClient::connect(&executor_url, token))
        .await
        .map_err(|_| closed())?
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let mut params = json!({ "offer": offer });
    if let Some(hint) = hint {
        params["hint"] = json!(hint);
    }
    let claimed: Claimed =
        tokio::time::timeout_at(window, client.call("identity.claimOffer", params))
            .await
            .map_err(|_| closed())??;
    println!("Claimed a {} offer of {}.", claimed.kind, claimed.did);
    println!("Match code: {}", claimed.match_code);
    println!("Confirm on the device that opened the offer that it shows the same code.");
    if no_wait {
        println!(
            "Offer {}: confirm on the app's device; identity.joinStatus reports when this executor has joined.",
            claimed.offer_id
        );
        return Ok(());
    }
    let offer_id = claimed.offer_id.clone();
    let joined = follow_join(
        &claimed,
        || {
            client.call::<Join>(
                "identity.joinStatus",
                json!({ "offerId": offer_id.clone() }),
            )
        },
        unix_now,
        Duration::from_secs(2),
    )
    .await?;
    println!("{joined}");
    Ok(())
}

fn unix_now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Follows the claim through `join_status` every `poll` until the holder confirms it (the
/// "Joined …" line), the offer ends, or its confirm window (`expires + CONFIRM_GRACE_S` by
/// `now`) closes. Each call and each wait between calls is cut off at the window's end, so an
/// executor that never answers cannot hold the command past it. A spent offer whose log does not list this executor's key
/// (`keyId: null`) is an error: the executor did not join.
async fn follow_join<F, Fut>(
    claimed: &Claimed,
    mut join_status: F,
    now: impl Fn() -> u64,
    poll: Duration,
) -> Result<String>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = Result<Join>>,
{
    let closed = || anyhow!("the holder did not confirm before the offer's confirm window closed");
    let deadline = claimed.expires.saturating_add(CONFIRM_GRACE_S);
    loop {
        let left = deadline.saturating_sub(now());
        if left == 0 {
            return Err(closed());
        }
        let join = tokio::time::timeout(Duration::from_secs(left), join_status())
            .await
            .map_err(|_| closed())??;
        match join.state.as_str() {
            "claimed" => {}
            "spent" => {
                let did = join.did.unwrap_or_else(|| claimed.did.clone());
                let Some(key_id) = join.key_id else {
                    bail!(
                        "the offer was spent, but the log of {did} does not list this executor's key"
                    );
                };
                return Ok(format!(
                    "Joined {did} as {key_id} (event {}).",
                    join.seq.map_or_else(|| "?".to_string(), |s| s.to_string())
                ));
            }
            other => bail!("the offer ended unconfirmed: it is {other}"),
        }
        // Never past the window: the next turn then ends the claim on time.
        let left = Duration::from_secs(deadline.saturating_sub(now()));
        tokio::time::sleep(poll.min(left)).await;
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
             address (use http://127.0.0.1:<port>)"
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
        assert!(is_loopback_url("http://127.0.0.1:12000"));
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

    fn claimed(expires: u64) -> Claimed {
        Claimed {
            did: A.into(),
            offer_id: "o1".into(),
            match_code: "R7K Q2M".into(),
            kind: "node".into(),
            expires,
        }
    }

    fn join(state: &str, key_id: Option<&str>) -> Join {
        Join {
            state: state.into(),
            did: Some(A.into()),
            key_id: key_id.map(str::to_string),
            seq: Some(1),
        }
    }

    /// A spent offer joins only with the key's id; `keyId: null` is an error.
    #[tokio::test]
    async fn a_spent_offer_without_this_executors_key_is_an_error() {
        let now = || 100;
        let answers =
            std::cell::RefCell::new(vec![join("spent", Some("d2")), join("claimed", None)]);
        let next = || {
            let answer = answers.borrow_mut().pop().unwrap();
            async move { Ok(answer) }
        };
        let joined = follow_join(&claimed(200), next, now, Duration::ZERO)
            .await
            .unwrap();
        assert_eq!(joined, format!("Joined {A} as d2 (event 1)."));

        let refused = follow_join(
            &claimed(200),
            || async { Ok(join("spent", None)) },
            now,
            Duration::ZERO,
        )
        .await;
        assert!(refused.is_err(), "{refused:?}");
        let ended = follow_join(
            &claimed(200),
            || async { Ok(join("expired", None)) },
            now,
            Duration::ZERO,
        )
        .await;
        assert!(ended.unwrap_err().to_string().contains("expired"));
    }

    /// An executor that never answers `identity.joinStatus` cannot hold `claim` past the
    /// confirm window, and an `expires` near `u64::MAX` does not wrap the window.
    #[tokio::test]
    async fn claim_follows_the_join_no_longer_than_the_confirm_window() {
        let start = std::time::Instant::now();
        let t0 = unix_now();
        let silent = follow_join(
            &claimed(t0 + 1 - CONFIRM_GRACE_S),
            std::future::pending::<Result<Join>>,
            unix_now,
            Duration::ZERO,
        )
        .await;
        assert!(silent.is_err());
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "{:?}",
            start.elapsed()
        );

        let far = follow_join(
            &claimed(u64::MAX),
            || async { Ok(join("spent", Some("d2"))) },
            // Past the confirm grace: a deadline that wrapped (a release build does not check
            // overflow) would lie behind this and end the claim.
            || CONFIRM_GRACE_S + 1,
            Duration::ZERO,
        )
        .await;
        assert!(far.is_ok(), "{far:?}");
    }

    /// A wait between polls ends with the window: a claim the holder never confirms ends when
    /// the window closes, not a poll interval later.
    #[tokio::test(start_paused = true)]
    async fn the_poll_wait_ends_with_the_confirm_window() {
        let start = tokio::time::Instant::now();
        let now = || 1_000 + start.elapsed().as_secs();
        let unconfirmed = follow_join(
            &claimed(1_000 + 1 - CONFIRM_GRACE_S),
            || async { Ok(join("claimed", None)) },
            now,
            Duration::from_secs(60),
        )
        .await;
        assert!(unconfirmed.is_err());
        assert_eq!(start.elapsed(), Duration::from_secs(1));
    }

    /// An executor that takes the connection and never completes the handshake cannot hold
    /// `claim` past the longest confirm window an offer can have.
    #[tokio::test(start_paused = true)]
    async fn a_silent_executor_cannot_hold_the_claim_past_the_window() {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        tokio::spawn(async move {
            let (_socket, _) = listener.accept().await.unwrap();
            std::future::pending::<()>().await;
        });
        let start = tokio::time::Instant::now();
        let refused = claim(
            &format!("http://127.0.0.1:{port}"),
            "token",
            "offer",
            None,
            true,
            false,
        )
        .await
        .expect_err("the executor never answers");
        assert!(refused.to_string().contains("confirm window"), "{refused}");
        assert_eq!(
            start.elapsed(),
            Duration::from_secs(OFFER_TTL_S + CONFIRM_GRACE_S)
        );
    }

    /// On an existing data folder `--operator` records or refuses before init runs, and init
    /// (which rewrites the seed) runs only with a new seed; on a new folder init runs first.
    #[test]
    fn init_with_an_operator_on_an_existing_folder_keeps_its_seed() {
        let dir = std::env::temp_dir().join(format!("ad4m-cli-init-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("mainnet_seed.seed"), "{}").unwrap();
        let path = Some(dir.to_str().unwrap().to_string());
        let ran = std::cell::Cell::new(0);
        let init = |_: Option<String>, _: Option<String>| {
            ran.set(ran.get() + 1);
            Ok(())
        };

        init_with_operator(path.clone(), None, Some(A), init).unwrap();
        assert_eq!(ran.get(), 0, "an existing folder keeps its seed");
        let refused = init_with_operator(path.clone(), None, Some(B), init);
        assert!(refused.is_err());
        let refused = init_with_operator(path.clone(), Some("seed".into()), Some(B), init);
        assert!(refused.is_err());
        assert_eq!(ran.get(), 0, "a refused operator runs no init");
        init_with_operator(path.clone(), Some("seed".into()), Some(A), init).unwrap();
        assert_eq!(ran.get(), 1, "a new seed is written");
        init_with_operator(path, None, None, init).unwrap();
        assert_eq!(ran.get(), 2, "init without --operator runs as before");

        // A new folder: init makes it, then the operator is recorded.
        let fresh = dir.join("new");
        let fresh_path = Some(fresh.to_str().unwrap().to_string());
        init_with_operator(fresh_path.clone(), None, Some(A), |_, _| {
            std::fs::create_dir_all(&fresh)?;
            std::fs::write(fresh.join("mainnet_seed.seed"), "{}")?;
            Ok(())
        })
        .unwrap();
        assert!(record_operator(&database_path(fresh_path.as_deref()), B).is_err());
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A folder whose database records an operator but whose seed is gone: another operator is
    /// refused before `init` writes a seed; the recorded one lets `init` make the seed.
    #[test]
    fn init_without_the_seed_checks_the_recorded_operator_first() {
        let dir = std::env::temp_dir().join(format!("ad4m-cli-noseed-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = Some(dir.to_str().unwrap().to_string());
        record_operator(&database_path(path.as_deref()), A).unwrap();
        let seed = dir.join("mainnet_seed.seed");
        let init = |_: Option<String>, _: Option<String>| {
            std::fs::write(&seed, "{}")?;
            Ok(())
        };

        let refused = init_with_operator(path.clone(), Some("seed".into()), Some(B), init);
        assert!(refused.is_err());
        assert!(!seed.exists(), "a refused operator writes no seed");
        init_with_operator(path, Some("seed".into()), Some(A), init).unwrap();
        assert!(seed.exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    const SECRET: &str = "admin-secret-should-not-travel";

    /// The error `claim` returns for `url`. The admin credential is never in it.
    /// A call that blocks on a socket fails the test after 3s.
    async fn claim_message(url: &str, allow_insecure_remote: bool) -> String {
        let result = tokio::time::timeout(
            Duration::from_secs(3),
            claim(url, SECRET, "offer", None, true, allow_insecure_remote),
        )
        .await
        .unwrap_or_else(|_| panic!("claim blocked on a socket for {url}"));
        let err = result.unwrap_err();
        let msg = format!("{err:#}");
        assert!(
            !msg.contains(SECRET),
            "the admin credential leaked into the error for {url}: {msg}"
        );
        msg
    }

    /// A remote `http://` (and `ws://`) executor is refused before `claim` puts
    /// the admin credential on a socket, and the credential is absent from the error.
    #[tokio::test]
    async fn claim_refuses_remote_http_and_omits_the_credential() {
        for url in ["http://203.0.113.55:9", "ws://203.0.113.55:9"] {
            let msg = claim_message(url, false).await;
            assert!(
                msg.contains("plaintext"),
                "expected a plaintext refusal for {url}, got {msg}"
            );
            assert!(
                msg.contains("allow_insecure_remote"),
                "the refusal for {url} does not name the flag: {msg}"
            );
        }
    }

    /// `--allow-insecure-remote` lets `claim` dial a plaintext executor off loopback.
    #[tokio::test]
    async fn claim_allows_remote_http_when_the_flag_is_set() {
        // `0.0.0.0` is not loopback. Nothing accepts the port, so the dial
        // fails at once and the error is a reachability error.
        let msg = claim_message("http://0.0.0.0:1", true).await;
        assert!(
            !msg.contains("plaintext"),
            "the flag did not permit plaintext: {msg}"
        );
        assert!(msg.contains("cannot reach"), "the flag did not dial: {msg}");
    }

    /// Loopback `http://` is allowed: 127.0.0.0/8, `::1`, and `localhost`.
    /// A listener on 127.0.0.1 sees the credential, which stays out of the error.
    #[tokio::test]
    async fn claim_allows_loopback_http() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use tokio::io::AsyncReadExt;

        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        let seen = std::sync::Arc::new(AtomicBool::new(false));
        let seen_task = std::sync::Arc::clone(&seen);
        tokio::spawn(async move {
            let Ok(Ok((mut sock, _))) =
                tokio::time::timeout(Duration::from_secs(2), listener.accept()).await
            else {
                return;
            };
            let mut got = Vec::new();
            let deadline = tokio::time::Instant::now() + Duration::from_secs(1);
            while tokio::time::Instant::now() < deadline {
                let mut buf = [0u8; 1024];
                match tokio::time::timeout(Duration::from_millis(200), sock.read(&mut buf)).await {
                    Ok(Ok(0)) | Ok(Err(_)) | Err(_) => break,
                    Ok(Ok(n)) => {
                        got.extend_from_slice(&buf[..n]);
                        if got.windows(SECRET.len()).any(|w| w == SECRET.as_bytes()) {
                            seen_task.store(true, Ordering::SeqCst);
                            break;
                        }
                    }
                }
            }
        });

        let msg = claim_message(&format!("http://127.0.0.1:{port}"), false).await;
        assert!(!msg.contains("plaintext"), "loopback was refused: {msg}");
        assert!(
            seen.load(Ordering::SeqCst),
            "loopback did not dial the admin credential: {msg}"
        );

        for url in [
            "http://127.0.0.1:1",
            "http://127.8.0.1:1",
            "http://localhost:1",
            "http://[::1]:1",
        ] {
            let msg = claim_message(url, false).await;
            assert!(
                !msg.contains("plaintext"),
                "loopback {url} was refused: {msg}"
            );
        }
    }

    /// `https://` and `wss://` to a non-loopback host are allowed.
    #[tokio::test]
    async fn claim_allows_https() {
        for url in ["https://0.0.0.0:1", "wss://0.0.0.0:1"] {
            let msg = claim_message(url, false).await;
            assert!(
                !msg.contains("plaintext"),
                "TLS {url} was refused as plaintext: {msg}"
            );
            assert!(
                msg.contains("cannot reach"),
                "TLS {url} did not dial: {msg}"
            );
        }
    }
}
