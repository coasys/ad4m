//! The executor's own secrets outside its stores, such as the private key of the certificate
//! its local-network listener serves: each one a sealed file in `{data}/ad4m`, never a
//! plaintext file, and no RPC returns one.
//!
//! **The file.** `{data}/ad4m/<name>.sealed`, mode 0600, written atomically:
//!
//! ```text
//! "AD4MSEAL" | format (u8, 1) | store id length (u16) | "executor" | key version (u32)
//!            | nonce (24) | XChaCha20-Poly1305 ciphertext and tag
//! ```
//!
//! integers big endian, under the executor store key's subkey for secrets
//! ([`SubkeyPurpose::Secret`]). The associated data names the file's path inside `ad4m/` and
//! the store, so a sealed secret renamed or moved does not open.

use std::io;
use std::path::{Component, Path, PathBuf};

use ad4m_keyring::KeyVersion;
use chacha20poly1305::aead::{Aead, KeyInit, Payload};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use rand::RngCore;
use zeroize::Zeroizing;

use super::data_keys::{DataKeys, KeyringError, SubkeyPurpose};
use super::files;
use super::keyring_file::StoreId;
use super::lang_storage::{FORMAT, MAGIC};

const NONCE_LEN: usize = 24;
const TAG_LEN: usize = 16;
const AD_LABEL: &[u8] = b"ad4m:at-rest:secret:v1";
/// The suffix of a sealed secret's file.
pub const SUFFIX: &str = ".sealed";

/// Why an executor secret was not sealed or opened. No variant carries the secret.
#[derive(Debug, thiserror::Error)]
pub enum ExecutorSecretError {
    #[error(transparent)]
    Keyring(#[from] KeyringError),
    #[error("not a name for an executor secret: {0:?}")]
    BadName(String),
    #[error("no executor secret {0}")]
    NotFound(String),
    #[error("the executor secret {0} does not open under the executor's key")]
    DoesNotOpen(String),
    #[error("{path}: {error}")]
    Io { path: PathBuf, error: io::Error },
}

fn io_error(path: &Path, error: io::Error) -> ExecutorSecretError {
    ExecutorSecretError::Io {
        path: path.to_path_buf(),
        error,
    }
}

/// The path inside `ad4m/` of the secret `name`: relative, `/`-separated, without `.` or
/// `..` and not empty.
fn file_of(name: &str) -> Result<String, ExecutorSecretError> {
    let bad = || ExecutorSecretError::BadName(name.to_string());
    let path = Path::new(name);
    if name.is_empty() || name.contains('\\') {
        return Err(bad());
    }
    for component in path.components() {
        match component {
            Component::Normal(_) => {}
            _ => return Err(bad()),
        }
    }
    Ok(format!("{name}{SUFFIX}"))
}

fn associated_data(file: &str, store: &str) -> Vec<u8> {
    let mut ad = Vec::new();
    for part in [AD_LABEL, &[FORMAT][..], file.as_bytes(), store.as_bytes()] {
        ad.extend_from_slice(&(part.len() as u32).to_be_bytes());
        ad.extend_from_slice(part);
    }
    ad
}

impl DataKeys {
    /// The folder the keyring file is in, `{data}/ad4m`: where the executor's secrets live.
    fn secrets_folder(&self) -> PathBuf {
        self.path()
            .parent()
            .map_or_else(PathBuf::new, Path::to_path_buf)
    }

    /// The sealed file of the secret `name`.
    pub fn executor_secret_path(&self, name: &str) -> Result<PathBuf, ExecutorSecretError> {
        Ok(self.secrets_folder().join(file_of(name)?))
    }

    /// Seals `bytes` as the executor secret `name` (for example `lan/key`), under the current
    /// version of the executor store key, and writes it to `{data}/ad4m/<name>.sealed`
    /// atomically, mode 0600, its folders 0700.
    pub fn seal_executor_secret(
        &self,
        name: &str,
        bytes: &[u8],
    ) -> Result<(), ExecutorSecretError> {
        let file = file_of(name)?;
        let path = self.secrets_folder().join(&file);
        let (version, key) = self.subkey(&StoreId::Executor, None, SubkeyPurpose::Secret)?;
        let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
        let mut nonce = [0u8; NONCE_LEN];
        rand::thread_rng().fill_bytes(&mut nonce);
        let store = StoreId::Executor.to_string();
        let sealed = cipher
            .encrypt(
                XNonce::from_slice(&nonce),
                Payload {
                    msg: bytes,
                    aad: &associated_data(&file, &store),
                },
            )
            .expect("XChaCha20-Poly1305 seals any length a secret has");
        let mut out = Vec::with_capacity(MAGIC.len() + 7 + store.len() + NONCE_LEN + sealed.len());
        out.extend_from_slice(MAGIC);
        out.push(FORMAT);
        out.extend_from_slice(&(store.len() as u16).to_be_bytes());
        out.extend_from_slice(store.as_bytes());
        out.extend_from_slice(&version.0.to_be_bytes());
        out.extend_from_slice(&nonce);
        out.extend_from_slice(&sealed);
        if let Some(folder) = path.parent() {
            files::create_private_dir_all(folder).map_err(|e| io_error(folder, e))?;
        }
        files::write_atomic(&path, &out).map_err(|e| io_error(&path, e))
    }

    /// Opens the executor secret `name`. `NotFound` when it was never sealed.
    pub fn open_executor_secret(
        &self,
        name: &str,
    ) -> Result<Zeroizing<Vec<u8>>, ExecutorSecretError> {
        let file = file_of(name)?;
        let path = self.secrets_folder().join(&file);
        let bytes = match std::fs::read(&path) {
            Ok(bytes) => bytes,
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                return Err(ExecutorSecretError::NotFound(name.to_string()))
            }
            Err(e) => return Err(io_error(&path, e)),
        };
        let does_not_open = || ExecutorSecretError::DoesNotOpen(name.to_string());
        let store = StoreId::Executor.to_string();
        let rest = bytes.strip_prefix(&MAGIC[..]).ok_or_else(does_not_open)?;
        let (&format, rest) = rest.split_first().ok_or_else(does_not_open)?;
        if format != FORMAT || rest.len() < 2 {
            return Err(does_not_open());
        }
        let store_len = u16::from_be_bytes([rest[0], rest[1]]) as usize;
        let rest = &rest[2..];
        if store_len != store.len()
            || rest.len() < store_len + 4 + NONCE_LEN + TAG_LEN
            || &rest[..store_len] != store.as_bytes()
        {
            return Err(does_not_open());
        }
        let rest = &rest[store_len..];
        let version = KeyVersion(u32::from_be_bytes([rest[0], rest[1], rest[2], rest[3]]));
        let nonce = &rest[4..4 + NONCE_LEN];
        let (_, key) = self.subkey(&StoreId::Executor, Some(version), SubkeyPurpose::Secret)?;
        let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
        cipher
            .decrypt(
                XNonce::from_slice(nonce),
                Payload {
                    msg: &rest[4 + NONCE_LEN..],
                    aad: &associated_data(&file, &store),
                },
            )
            .map(Zeroizing::new)
            .map_err(|_| does_not_open())
    }

    /// Moves a secret an older executor kept in the plaintext file `plaintext` under the
    /// executor's key: sealed as `name`, then the plaintext file overwritten with zeros and
    /// removed. Answers whether it moved one; without the plaintext file, nothing to do. A
    /// failure leaves the plaintext file as it was.
    pub fn seal_plaintext_executor_secret(
        &self,
        name: &str,
        plaintext: &Path,
    ) -> Result<bool, ExecutorSecretError> {
        let bytes = match std::fs::read(plaintext) {
            Ok(bytes) => Zeroizing::new(bytes),
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(false),
            Err(e) => return Err(io_error(plaintext, e)),
        };
        self.seal_executor_secret(name, &bytes)?;
        // The sealed copy opens before the plaintext goes.
        if *self.open_executor_secret(name)? != *bytes {
            return Err(ExecutorSecretError::DoesNotOpen(name.to_string()));
        }
        let zeros = vec![0u8; bytes.len()];
        let overwritten = std::fs::OpenOptions::new()
            .write(true)
            .open(plaintext)
            .and_then(|mut file| {
                io::Write::write_all(&mut file, &zeros)?;
                file.sync_all()
            });
        if let Err(e) = overwritten {
            log::warn!(
                "Could not overwrite {} before removing it: {e}",
                plaintext.display()
            );
        }
        std::fs::remove_file(plaintext).map_err(|e| io_error(plaintext, e))?;
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zeroize::Zeroizing;

    fn open_keys(folder: &Path) -> DataKeys {
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder)).unwrap();
        keys.unlock(Zeroizing::new([7u8; 32])).unwrap();
        keys
    }

    const KEY_DER: &[u8] = b"-----BEGIN PRIVATE KEY-----\nlan-certificate-private-key\n";

    #[test]
    fn a_sealed_secret_opens_and_its_file_holds_no_plaintext() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        keys.seal_executor_secret("lan/key", KEY_DER).unwrap();
        let path = folder.path().join("ad4m/lan/key.sealed");
        assert_eq!(keys.executor_secret_path("lan/key").unwrap(), path);
        let on_disk = std::fs::read(&path).unwrap();
        assert!(on_disk.starts_with(MAGIC));
        assert!(!on_disk
            .windows(b"lan-certificate".len())
            .any(|w| w == b"lan-certificate"));
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = |p: &Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode(&path), 0o600);
            assert_eq!(mode(&folder.path().join("ad4m/lan")), 0o700);
        }
        assert_eq!(&*keys.open_executor_secret("lan/key").unwrap(), KEY_DER);

        // Another start: the same secret opens the same file.
        let again = open_keys(folder.path());
        assert_eq!(&*again.open_executor_secret("lan/key").unwrap(), KEY_DER);
    }

    #[test]
    fn a_secret_moved_renamed_altered_or_missing_does_not_open() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        keys.seal_executor_secret("lan/key", KEY_DER).unwrap();
        let ad4m = folder.path().join("ad4m");
        std::fs::copy(ad4m.join("lan/key.sealed"), ad4m.join("lan/other.sealed")).unwrap();
        assert!(matches!(
            keys.open_executor_secret("lan/other"),
            Err(ExecutorSecretError::DoesNotOpen(_))
        ));
        let mut altered = std::fs::read(ad4m.join("lan/key.sealed")).unwrap();
        let last = altered.len() - 1;
        altered[last] ^= 1;
        std::fs::write(ad4m.join("lan/key.sealed"), altered).unwrap();
        assert!(matches!(
            keys.open_executor_secret("lan/key"),
            Err(ExecutorSecretError::DoesNotOpen(_))
        ));
        assert!(matches!(
            keys.open_executor_secret("lan/none"),
            Err(ExecutorSecretError::NotFound(_))
        ));
        for bad in ["", "../key", "/abs", "lan/../key", "./key"] {
            assert!(
                matches!(
                    keys.seal_executor_secret(bad, b"x"),
                    Err(ExecutorSecretError::BadName(_))
                ),
                "{bad}"
            );
        }
    }

    #[test]
    fn a_sealed_keyring_seals_and_opens_nothing() {
        let folder = tempfile::tempdir().unwrap();
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder.path())).unwrap();
        assert!(matches!(
            keys.seal_executor_secret("lan/key", KEY_DER),
            Err(ExecutorSecretError::Keyring(KeyringError::Sealed))
        ));
        assert!(!folder.path().join("ad4m/lan/key.sealed").exists());
    }

    #[test]
    fn a_plaintext_key_file_is_sealed_then_removed() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path());
        let plaintext = folder.path().join("ad4m/lan/key.pem");
        std::fs::create_dir_all(plaintext.parent().unwrap()).unwrap();
        std::fs::write(&plaintext, KEY_DER).unwrap();

        assert!(keys
            .seal_plaintext_executor_secret("lan/key", &plaintext)
            .unwrap());
        assert!(!plaintext.exists(), "the plaintext file is gone");
        assert_eq!(&*keys.open_executor_secret("lan/key").unwrap(), KEY_DER);
        // Nothing left to move on the next start.
        assert!(!keys
            .seal_plaintext_executor_secret("lan/key", &plaintext)
            .unwrap());
        assert_eq!(&*keys.open_executor_secret("lan/key").unwrap(), KEY_DER);
    }
}
