//! `ad4m-executor storage`: make an operator secret, unlock a sealed executor, read its
//! storage status, move it to a new secret, or give up the data a lost secret locked (`reset`,
//! offline). A secret is typed at the terminal with echo off and checked here before it
//! is sent; it is never taken from an argument or the environment (other processes of the user
//! read both), and nothing but its fingerprint is printed.

use ad4m_client::ws_rpc::WsRpcClient;
use anyhow::{bail, Context, Result};
use clap::Subcommand;
use rust_executor::at_rest::secret_source::{
    fingerprint, new_secret_hex, parse_hex, SECRET_FILE_VAR, SECRET_VALUE_VAR,
};
use rust_executor::at_rest::{StorageErrorKind, StorageState, StorageStatus};
use serde::Deserialize;
use serde_json::json;
use zeroize::Zeroizing;

#[derive(Debug, Subcommand)]
pub enum StorageCommand {
    /// Print a new operator secret (64 hex characters) from the OS random source. Keep it off
    /// the data folder's disk: an executor started with it encrypts that folder under it.
    NewSecret,
    /// Unlock a sealed executor on this machine: type its operator secret when asked (it is
    /// not echoed). Takes no secret as an argument or from the environment; prints only the
    /// secret's fingerprint. Uses the admin credential as `rekey` does.
    Unlock {
        /// Refused: a secret on the command line is visible to every process of the machine.
        #[arg(hide = true, trailing_var_arg = true)]
        given: Vec<String>,
    },
    /// Print the executor's storage status: sealed, unlocking, migrating or open, and its
    /// keyring id.
    Status,
    /// For a lost storage secret: move the data the keyring guards (the keyring, the database,
    /// the agent, the managed users' profiles, Holochain's folder, the sealed executor secrets,
    /// the perspectives' stores, the languages' sealed storage files) into a `lost-<time>`
    /// folder in the data folder, so the next start begins from nothing. Nothing is deleted:
    /// the old secret still opens the moved data. Runs offline, with the executor stopped;
    /// never touches a path outside the data folder. Without `--confirm <keyring id>` it only
    /// prints what would move and the id to confirm with.
    Reset {
        /// The executor's data folder (`run`'s --app-data-path); default `~/.ad4m`.
        #[arg(long)]
        data_path: Option<std::path::PathBuf>,
        /// The keyring's id (from `storage status` or the plan this prints), or
        /// `unreadable-keyring` for a keyring file that does not read.
        #[arg(long)]
        confirm: Option<String>,
    },
    /// Move a running executor on this machine to a new operator secret: type the current
    /// secret, then the new one twice, when asked (none is echoed; make the new one with
    /// `storage new-secret`). From then on only the new secret opens this executor's data.
    /// Takes no secret as an argument or from the environment; prints only fingerprints.
    RotateSecret {
        /// Refused: a secret on the command line is visible to every process of the machine.
        #[arg(hide = true, trailing_var_arg = true)]
        given: Vec<String>,
    },
}

/// What `storage new-secret` prints.
pub fn new_secret_text() -> Zeroizing<String> {
    let secret = new_secret_hex();
    Zeroizing::new(format!(
        "{}\nKeep this secret safe, away from the data folder's disk. If you lose it, this \
         device's data cannot be opened again.",
        &*secret
    ))
}

/// Refuses a secret given to `storage <command>` on the command line or in
/// `AD4M_STORAGE_SECRET`, without repeating it.
pub fn refuse_given_secret(
    command: &str,
    given: &[String],
    env: impl Fn(&str) -> Option<String>,
) -> Result<()> {
    if !given.is_empty() {
        bail!(
            "storage {command} takes no argument: a secret on the command line is visible to \
             every process of this machine. Run `ad4m-executor storage {command}` and type the \
             secret when asked."
        );
    }
    if env(SECRET_VALUE_VAR).is_some() {
        bail!(
            "{SECRET_VALUE_VAR} is set: the environment is readable by your other processes. \
             Unset it, run `ad4m-executor storage {command}` and type the secret when asked, \
             or start the executor with {SECRET_FILE_VAR}."
        );
    }
    Ok(())
}

/// Reads a secret from the terminal with echo off, after `prompt`.
pub fn read_secret_from_terminal(prompt: &str) -> Result<Zeroizing<String>> {
    rpassword::prompt_password(format!("{prompt} (not shown): "))
        .map(Zeroizing::new)
        .context("cannot read the secret from the terminal")
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct UnlockAnswer {
    status: StorageStatus,
    fingerprint: String,
    created: bool,
}

fn state_name(state: StorageState) -> &'static str {
    match state {
        StorageState::Sealed => "sealed",
        StorageState::Unlocking => "unlocking",
        StorageState::Migrating => "migrating",
        StorageState::Open => "open",
    }
}

/// The line `storage unlock` prints: the state, the keyring and the secret's fingerprint.
fn describe_unlock(answer: &UnlockAnswer) -> String {
    let made = if answer.created {
        " A new keyring was made: this secret now opens this executor's data."
    } else {
        ""
    };
    format!(
        "Storage {} (keyring {}), secret fingerprint {}.{made}",
        state_name(answer.status.state),
        answer.status.keyring_id,
        answer.fingerprint
    )
}

/// The lines `storage status` prints.
pub fn describe_status(status: &StorageStatus) -> String {
    let mut text = format!(
        "Storage {} (keyring {})",
        state_name(status.state),
        status.keyring_id
    );
    if status.state == StorageState::Sealed {
        text.push_str(": waiting for `ad4m-executor storage unlock`");
    }
    if let Some(migration) = &status.migration {
        text.push_str(&format!(
            "\nMoving plaintext data under its keys: {} of {} stores done{}",
            migration.stores_done,
            migration.stores_total,
            migration
                .store
                .as_ref()
                .map(|s| format!(", now {s}"))
                .unwrap_or_default()
        ));
    }
    if let Some(error) = status.last_error {
        let line = match error {
            StorageErrorKind::WrongSecret => "The last unlock failed: wrong-secret",
            StorageErrorKind::CorruptKeyring => "The last unlock failed: corrupt-keyring",
            StorageErrorKind::SecretOnDataDisk => "The last unlock failed: secret-on-data-disk",
            StorageErrorKind::MigrationFailed => {
                "The plaintext data did not move under its keys (migration-failed): it stays \
                 as it was, see the executor log; the next unlock tries again"
            }
        };
        text.push_str(&format!("\n{line}"));
    }
    text
}

/// `storage unlock`: sends `secret` to the executor at `executor_url` (a loopback address) and
/// answers the line to print.
pub async fn unlock(executor_url: &str, token: &str, secret: &str) -> Result<String> {
    if !crate::identity::is_loopback_url(executor_url) {
        bail!(
            "storage unlock runs only on the executor's own machine: {executor_url} is not a \
             loopback address (use http://127.0.0.1:<port>)"
        );
    }
    // Checked here, so a typo is not sent and the fingerprint can be matched.
    let bytes = parse_hex(secret).map_err(|e| anyhow::anyhow!("{e}"))?;
    let expected = fingerprint(&bytes);
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let answer: UnlockAnswer = client
        .call("storage.unlock", json!({ "secret": secret.trim() }))
        .await?;
    if answer.fingerprint != expected {
        bail!(
            "the executor answered for another secret (fingerprint {}, typed {expected})",
            answer.fingerprint
        );
    }
    Ok(describe_unlock(&answer))
}

/// `storage status`: the executor's storage status, as lines to print.
pub async fn status(executor_url: &str, token: &str) -> Result<String> {
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let status: StorageStatus = client.call("runtime.storageStatus", json!({})).await?;
    Ok(describe_status(&status))
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RotateAnswer {
    generation: u32,
    old_fingerprint: String,
    new_fingerprint: String,
}

/// The lines `storage rotate-secret` prints: the fingerprints and the root generation.
fn describe_rotation(answer: &RotateAnswer) -> String {
    format!(
        "The storage secret changed: fingerprint {} -> {} (root generation {}).\nFrom now on \
         only the new secret opens this executor's data: replace the old one wherever the \
         executor reads it at start (the secret file), and keep the new one safe.",
        answer.old_fingerprint, answer.new_fingerprint, answer.generation
    )
}

/// `storage rotate-secret`: checks the three typed texts (the current secret, the new one and
/// its confirmation), sends the two secrets to the executor at `executor_url` (a loopback
/// address) and answers the lines to print.
pub async fn rotate_secret(
    executor_url: &str,
    token: &str,
    current: &str,
    new: &str,
    confirmed: &str,
) -> Result<String> {
    if !crate::identity::is_loopback_url(executor_url) {
        bail!(
            "storage rotate-secret runs only on the executor's own machine: {executor_url} is \
             not a loopback address (use http://127.0.0.1:<port>)"
        );
    }
    let current_bytes =
        parse_hex(current).map_err(|e| anyhow::anyhow!("the current secret: {e}"))?;
    let new_bytes = parse_hex(new).map_err(|e| anyhow::anyhow!("the new secret: {e}"))?;
    if new.trim() != confirmed.trim() {
        bail!("the new secret and its confirmation differ: nothing changed");
    }
    if *current_bytes == *new_bytes {
        bail!("the new secret is the current one: nothing changed");
    }
    let (old_expected, new_expected) = (fingerprint(&current_bytes), fingerprint(&new_bytes));
    let client = WsRpcClient::connect(executor_url, token)
        .await
        .with_context(|| format!("cannot reach the executor at {executor_url}"))?;
    let answer: RotateAnswer = client
        .call(
            "storage.rotateSecret",
            json!({ "currentSecret": current.trim(), "newSecret": new.trim() }),
        )
        .await?;
    if answer.old_fingerprint != old_expected || answer.new_fingerprint != new_expected {
        bail!(
            "the executor answered for other secrets (fingerprints {} -> {}, typed {old_expected} \
             -> {new_expected})",
            answer.old_fingerprint,
            answer.new_fingerprint
        );
    }
    Ok(describe_rotation(&answer))
}

/// `storage reset`: without `confirm`, the lines that list what would move and the id to
/// confirm with; with it, the reset done, or why it was refused.
pub fn reset(data_dir: &std::path::Path, confirm: Option<&str>) -> Result<String> {
    use rust_executor::at_rest::reset;
    let plan = reset::plan(data_dir)?;
    let Some(confirm) = confirm else {
        let listed = plan
            .moves
            .iter()
            .map(|p| format!("  {}", p.display()))
            .collect::<Vec<_>>()
            .join("\n");
        return Ok(format!(
            "A reset of {} moves into a lost- folder there:\n{listed}\nThe next start makes a new \
             keyring and a new agent. Nothing is deleted: the old secret still opens the moved \
             data. Stop the executor, then run `ad4m-executor storage reset --data-path {} \
             --confirm {}`.",
            data_dir.display(),
            data_dir.display(),
            plan.confirmation
        ));
    };
    let report = reset::reset(data_dir, confirm)?;
    Ok(format!(
        "Moved {} pieces into {}. The next start makes a new keyring.",
        report.moved.len(),
        report.lost.display()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SECRET: &str = "0101010101010101010101010101010101010101010101010101010101010101";

    #[test]
    fn a_new_secret_is_64_hex_characters_and_a_warning() {
        let text = new_secret_text();
        let first = text.lines().next().unwrap();
        assert_eq!(first.len(), 64);
        assert!(parse_hex(first).is_ok());
        assert!(text.contains("cannot be opened again"));
        assert_ne!(first, new_secret_text().lines().next().unwrap());
    }

    /// The unlock refuses a secret on the command line or in the environment and never repeats
    /// it.
    #[test]
    fn unlock_refuses_a_secret_in_an_argument_or_the_environment() {
        let none = |_: &str| None;
        for command in ["unlock", "rotate-secret"] {
            refuse_given_secret(command, &[], none).unwrap();
            let err = refuse_given_secret(command, &[SECRET.to_string()], none)
                .unwrap_err()
                .to_string();
            assert!(
                err.contains(&format!("storage {command} takes no argument")),
                "{err}"
            );
            assert!(!err.contains(SECRET));
            let env = |name: &str| (name == SECRET_VALUE_VAR).then(|| SECRET.to_string());
            let err = refuse_given_secret(command, &[], env)
                .unwrap_err()
                .to_string();
            assert!(err.contains(SECRET_VALUE_VAR), "{err}");
            assert!(!err.contains(SECRET));
        }
    }

    /// What the unlock prints names the state, the keyring and the fingerprint, and no byte of
    /// the secret.
    #[test]
    fn unlock_prints_only_the_fingerprint() {
        let answer = UnlockAnswer {
            status: StorageStatus {
                state: StorageState::Unlocking,
                encrypted_at_rest: false,
                stores: None,
                keyring_id: "ab".repeat(16),
                secret_source: None,
                secret_on_data_disk: None,
                root_generation: Some(1),
                last_error: None,
                migration: None,
            },
            fingerprint: fingerprint(&[1; 32]),
            created: true,
        };
        let line = describe_unlock(&answer);
        assert!(line.contains(&fingerprint(&[1; 32])), "{line}");
        assert!(line.contains("unlocking"), "{line}");
        assert!(line.contains("new keyring"), "{line}");
        assert!(!line.contains("0101"), "{line}");
    }

    /// A database that did not move under its key is told apart from a failed unlock.
    #[test]
    fn status_names_a_failed_database_migration() {
        let status = |last_error| StorageStatus {
            state: StorageState::Open,
            encrypted_at_rest: false,
            stores: None,
            keyring_id: "ab".repeat(16),
            secret_source: None,
            secret_on_data_disk: None,
            root_generation: Some(1),
            last_error,
            migration: None,
        };
        let text = describe_status(&status(Some(StorageErrorKind::MigrationFailed)));
        assert!(text.contains("migration-failed"), "{text}");
        assert!(text.contains("the next unlock tries again"), "{text}");
        assert!(!text.contains("unlock failed"), "{text}");
        let text = describe_status(&status(Some(StorageErrorKind::WrongSecret)));
        assert!(
            text.contains("The last unlock failed: wrong-secret"),
            "{text}"
        );
    }

    /// A migrating executor's status names the state and how far the move is.
    #[test]
    fn status_shows_the_migration_progress() {
        let status = StorageStatus {
            state: StorageState::Migrating,
            encrypted_at_rest: false,
            stores: None,
            keyring_id: "ab".repeat(16),
            secret_source: None,
            secret_on_data_disk: None,
            root_generation: Some(1),
            last_error: None,
            migration: Some(rust_executor::at_rest::StorageMigrationProgress {
                step: rust_executor::at_rest::MigrationStep::LanguageStorage,
                stores_done: 2,
                stores_total: 5,
                store: Some("language:QmA".into()),
            }),
        };
        let text = describe_status(&status);
        assert!(text.starts_with("Storage migrating"), "{text}");
        assert!(
            text.contains("2 of 5 stores done, now language:QmA"),
            "{text}"
        );
    }

    /// Without the confirmation the reset only lists what would move and the id to confirm
    /// with; with it, the keyring moves out; a data folder without a keyring is refused.
    #[test]
    fn reset_lists_first_and_moves_only_when_confirmed() {
        let data = tempfile::tempdir().unwrap();
        assert!(
            reset(data.path(), None).is_err(),
            "no keyring, nothing to reset"
        );
        let keys = rust_executor::at_rest::DataKeys::load(
            rust_executor::at_rest::keyring_file::path_in(data.path()),
        )
        .unwrap();
        keys.unlock(Zeroizing::new([5u8; 32])).unwrap();
        let id = keys.keyring_id().to_hex();
        let text = reset(data.path(), None).unwrap();
        assert!(text.contains(&format!("--confirm {id}")), "{text}");
        assert!(text.contains("keyring.json"), "{text}");
        assert!(data.path().join("ad4m/keyring.json").exists());
        let err = reset(data.path(), Some("no")).unwrap_err().to_string();
        assert!(err.contains("not confirmed"), "{err}");
        let text = reset(data.path(), Some(&id)).unwrap();
        assert!(text.contains("Moved 1 pieces"), "{text}");
        assert!(!data.path().join("ad4m/keyring.json").exists());
    }

    /// The unlock goes only to a loopback address, and a malformed secret is not sent.
    #[tokio::test]
    async fn unlock_checks_the_address_and_the_secret_before_sending() {
        let err = unlock("http://203.0.113.1:12000", "", SECRET)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("own machine"), "{err}");
        // Port 9 on loopback: nothing listens, but the secret is refused before the connect.
        let err = unlock("http://127.0.0.1:9", "", "xyz")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("64 hex characters"), "{err}");
    }

    /// The rotation goes only to a loopback address; a malformed secret, a confirmation that
    /// differs and a new secret equal to the current one are refused before anything is sent,
    /// and no message repeats a secret.
    #[tokio::test]
    async fn rotate_secret_checks_the_address_and_the_secrets_before_sending() {
        let new = "02".repeat(32);
        let err = rotate_secret("http://203.0.113.1:12000", "", SECRET, &new, &new)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("own machine"), "{err}");
        // Port 9 on loopback: nothing listens, so each refusal comes before the connect.
        let local = "http://127.0.0.1:9";
        for (current, next, confirmed, expected) in [
            ("xyz", new.as_str(), new.as_str(), "the current secret"),
            (SECRET, "xyz", "xyz", "the new secret"),
            (SECRET, new.as_str(), SECRET, "differ"),
            (SECRET, SECRET, SECRET, "is the current one"),
        ] {
            let err = rotate_secret(local, "", current, next, confirmed)
                .await
                .unwrap_err()
                .to_string();
            assert!(err.contains(expected), "{err}");
            assert!(!err.contains(SECRET) && !err.contains(&new), "{err}");
        }
        let err = rotate_secret(local, "", SECRET, &new, &new)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("cannot reach"), "{err}");
    }

    /// What the rotation prints names both fingerprints and the generation, no secret.
    #[test]
    fn rotate_secret_prints_only_the_fingerprints() {
        let answer = RotateAnswer {
            generation: 2,
            old_fingerprint: fingerprint(&[1; 32]),
            new_fingerprint: fingerprint(&[2; 32]),
        };
        let text = describe_rotation(&answer);
        assert!(text.contains(&format!(
            "{} -> {}",
            answer.old_fingerprint, answer.new_fingerprint
        )));
        assert!(text.contains("root generation 2"));
        assert!(!text.contains(SECRET) && !text.contains(&"02".repeat(32)));
    }
}
