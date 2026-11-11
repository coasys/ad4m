//! `ad4m-executor storage`: make an operator secret, unlock a sealed executor, read its
//! storage status. The secret is typed at the terminal with echo off and checked here before it
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
    /// Print the executor's storage status: sealed, unlocking or open, and its keyring id.
    Status,
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

/// Refuses a secret given on the command line or in `AD4M_STORAGE_SECRET`, without repeating
/// it.
pub fn refuse_given_secret(given: &[String], env: impl Fn(&str) -> Option<String>) -> Result<()> {
    if !given.is_empty() {
        bail!(
            "storage unlock takes no argument: a secret on the command line is visible to \
             every process of this machine. Run `ad4m-executor storage unlock` and type the \
             secret when asked, or start the executor with {SECRET_FILE_VAR}."
        );
    }
    if env(SECRET_VALUE_VAR).is_some() {
        bail!(
            "{SECRET_VALUE_VAR} is set: the environment is readable by your other processes. \
             Unset it, run `ad4m-executor storage unlock` and type the secret when asked, or \
             start the executor with {SECRET_FILE_VAR}."
        );
    }
    Ok(())
}

/// Reads the secret from the terminal with echo off.
pub fn read_secret_from_terminal() -> Result<Zeroizing<String>> {
    rpassword::prompt_password("Storage secret (not shown): ")
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
    if let Some(error) = status.last_error {
        let line = match error {
            StorageErrorKind::WrongSecret => "The last unlock failed: wrong-secret",
            StorageErrorKind::CorruptKeyring => "The last unlock failed: corrupt-keyring",
            StorageErrorKind::SecretOnDataDisk => "The last unlock failed: secret-on-data-disk",
            StorageErrorKind::MigrationFailed => {
                "The database did not move under its key (migration-failed): it stays in the \
                 clear, see the executor log"
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
        refuse_given_secret(&[], none).unwrap();
        let err = refuse_given_secret(&[SECRET.to_string()], none)
            .unwrap_err()
            .to_string();
        assert!(err.contains("takes no argument"), "{err}");
        assert!(!err.contains(SECRET));
        let env = |name: &str| (name == SECRET_VALUE_VAR).then(|| SECRET.to_string());
        let err = refuse_given_secret(&[], env).unwrap_err().to_string();
        assert!(err.contains(SECRET_VALUE_VAR), "{err}");
        assert!(!err.contains(SECRET));
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
        };
        let text = describe_status(&status(Some(StorageErrorKind::MigrationFailed)));
        assert!(text.contains("migration-failed"), "{text}");
        assert!(text.contains("stays in the clear"), "{text}");
        assert!(!text.contains("unlock failed"), "{text}");
        let text = describe_status(&status(Some(StorageErrorKind::WrongSecret)));
        assert!(
            text.contains("The last unlock failed: wrong-secret"),
            "{text}"
        );
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
}
