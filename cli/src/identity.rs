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
/// and everything else, and a refused operator changes nothing. On a new folder `init` makes
/// it, then the operator is recorded.
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
    if let (Some(operator), true) = (operator, existing) {
        record_operator(&db, operator)?;
        if seed.is_none() {
            return Ok(());
        }
    }
    init(data_path, seed)?;
    if let (Some(operator), false) = (operator, existing) {
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
/// `now`) closes. Each call is cut off at the window's end, so an executor that never answers
/// cannot hold the command past it. A spent offer whose log does not list this executor's key
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
        tokio::time::sleep(poll).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            || 100,
            Duration::ZERO,
        )
        .await;
        assert!(far.is_ok(), "{far:?}");
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
}
