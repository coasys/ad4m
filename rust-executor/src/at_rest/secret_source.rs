//! Where the operator secret comes from, and the checks it passes before it is used.
//!
//! | Source | How |
//! |---|---|
//! | in-process | a shell that embeds the executor (the launcher) hands it to `run` in [`crate::Ad4mConfig::storage_secret`] |
//! | file | `AD4M_STORAGE_SECRET_FILE`: a file of 64 hex characters, mode 0600 or 0400 |
//! | unlock call | `storage.unlock` on a loopback listener, while the executor waits sealed |
//!
//! A secret file on the same filesystem as the data folder protects nothing when that disk is
//! taken: it is refused (`secret-on-data-disk`) unless `allow_secret_on_data_disk` is set, and
//! then the status reports the data as not encrypted at rest.

use std::fmt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use ts_rs::TS;
use zeroize::Zeroizing;

/// The variable naming the secret file.
pub const SECRET_FILE_VAR: &str = "AD4M_STORAGE_SECRET_FILE";
/// A variable that would carry the secret itself: refused, the environment is readable by
/// other processes of the same user.
pub const SECRET_VALUE_VAR: &str = "AD4M_STORAGE_SECRET";
/// The secret's length in bytes; 64 hex characters.
pub const SECRET_LEN: usize = 32;

/// How this start got its operator secret.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum SecretSourceKind {
    InProcess,
    File,
    UnlockCall,
}

/// The operator secret and where it came from. `Debug` shows no byte; the bytes are zeroed
/// when the last copy is dropped.
#[derive(Clone)]
pub struct StorageSecret {
    bytes: Zeroizing<[u8; SECRET_LEN]>,
    pub source: SecretSourceKind,
    /// The file it was read from (source `File`): checked against the data folder's disk.
    pub file: Option<PathBuf>,
}

impl fmt::Debug for StorageSecret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StorageSecret")
            .field("source", &self.source)
            .field("file", &self.file)
            .finish_non_exhaustive()
    }
}

impl StorageSecret {
    /// A secret an embedding shell holds (source in-process).
    pub fn in_process(bytes: [u8; SECRET_LEN]) -> Self {
        StorageSecret {
            bytes: Zeroizing::new(bytes),
            source: SecretSourceKind::InProcess,
            file: None,
        }
    }

    /// A secret from 64 hex characters, with its source.
    pub fn from_hex(
        text: &str,
        source: SecretSourceKind,
        file: Option<PathBuf>,
    ) -> Result<Self, SecretError> {
        Ok(StorageSecret {
            bytes: parse_hex(text)?,
            source,
            file,
        })
    }

    /// Reads the secret file at `path`: mode 0600 or 0400, 64 hex characters, one trailing
    /// newline allowed.
    pub fn read_file(path: &Path) -> Result<Self, crate::config_file::ConfigFileError> {
        let text = Zeroizing::new(crate::config_file::read_secret_file(path)?);
        Self::from_hex(&text, SecretSourceKind::File, Some(path.to_path_buf())).map_err(|e| {
            crate::config_file::ConfigFileError::InvalidStorageSecret {
                source: path.display().to_string(),
                reason: e.to_string(),
            }
        })
    }

    pub fn bytes(&self) -> &[u8; SECRET_LEN] {
        &self.bytes
    }

    /// The bytes, as an owned copy that is zeroed when dropped.
    pub fn to_zeroizing(&self) -> Zeroizing<[u8; SECRET_LEN]> {
        Zeroizing::new(*self.bytes)
    }
}

/// Why a secret was refused. Never carries a byte of it.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SecretError {
    #[error("the storage secret is 64 hex characters, this one has {0}")]
    Length(usize),
    #[error("the storage secret is hex (0-9, a-f)")]
    NotHex,
    #[error(
        "secret-on-data-disk: the storage secret file is on the same disk as the data folder, \
         so whoever takes that disk takes both; put it on another disk, or set \
         allow_secret_on_data_disk to accept data that is not encrypted at rest"
    )]
    OnDataDisk,
}

/// 64 hex characters (either case) to the secret. Surrounding whitespace is dropped.
pub fn parse_hex(text: &str) -> Result<Zeroizing<[u8; SECRET_LEN]>, SecretError> {
    let text = text.trim();
    if text.len() != SECRET_LEN * 2 {
        return Err(SecretError::Length(text.chars().count()));
    }
    let mut bytes = Zeroizing::new([0u8; SECRET_LEN]);
    hex::decode_to_slice(text, &mut *bytes).map_err(|_| SecretError::NotHex)?;
    Ok(bytes)
}

/// A fresh secret from the OS random source, as 64 hex characters.
pub fn new_secret_hex() -> Zeroizing<String> {
    let mut bytes = Zeroizing::new([0u8; SECRET_LEN]);
    rand::RngCore::fill_bytes(&mut rand::rngs::OsRng, &mut *bytes);
    Zeroizing::new(hex::encode(*bytes))
}

/// The secret's fingerprint: the first 8 hex characters of its SHA-256. Enough to tell two
/// secrets apart, nothing to guess one from.
pub fn fingerprint(secret: &[u8; SECRET_LEN]) -> String {
    hex::encode(&Sha256::digest(secret)[..4])
}

/// Whether `a` and `b` are on the same filesystem (the same `st_dev`). Elsewhere than unix,
/// `false`: the check cannot be made.
pub fn same_device(a: &Path, b: &Path) -> std::io::Result<bool> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        Ok(std::fs::metadata(a)?.dev() == std::fs::metadata(b)?.dev())
    }
    #[cfg(not(unix))]
    {
        let _ = (a, b);
        Ok(false)
    }
}

/// Where the secret sits relative to the data folder `data_dir`: `Ok(false)` off it,
/// `Ok(true)` on it with `allow` set, [`SecretError::OnDataDisk`] on it otherwise. Only a
/// secret file is checked; a secret from a shell or the unlock call sits on no disk.
pub fn check_placement(
    secret: &StorageSecret,
    data_dir: &Path,
    allow: bool,
) -> Result<bool, SecretError> {
    let Some(file) = &secret.file else {
        return Ok(false);
    };
    let on_disk = same_device(file, data_dir).unwrap_or_else(|e| {
        log::warn!(
            "Could not compare the disk of the storage secret file with the data folder's: {e}"
        );
        true
    });
    match (on_disk, allow) {
        (false, _) => Ok(false),
        (true, true) => Ok(true),
        (true, false) => Err(SecretError::OnDataDisk),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const HEX: &str = "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff";

    #[test]
    fn a_secret_is_64_hex_characters() {
        assert_eq!(parse_hex(HEX).unwrap()[1], 0x11);
        assert_eq!(
            parse_hex(&format!("{}\n", HEX.to_uppercase())).unwrap()[15],
            0xff
        );
        assert_eq!(parse_hex(&HEX[..62]), Err(SecretError::Length(62)));
        assert_eq!(
            parse_hex(&format!("{}zz", &HEX[..62])),
            Err(SecretError::NotHex)
        );
    }

    #[test]
    fn a_new_secret_is_fresh_hex_and_its_fingerprint_short() {
        let a = new_secret_hex();
        let b = new_secret_hex();
        assert_ne!(*a, *b);
        let bytes = parse_hex(&a).unwrap();
        let print = fingerprint(&bytes);
        assert_eq!(print.len(), 8);
        assert_eq!(a.len(), 64);
        assert_eq!(
            fingerprint(&[0u8; 32]),
            hex::encode(&Sha256::digest([0u8; 32])[..4])
        );
    }

    #[test]
    fn debug_shows_no_byte() {
        let secret = StorageSecret::from_hex(HEX, SecretSourceKind::File, None).unwrap();
        let shown = format!("{secret:?}");
        assert!(!shown.contains("0011"), "{shown}");
        assert!(shown.contains("File"), "{shown}");
    }

    #[cfg(unix)]
    #[test]
    fn a_secret_file_on_the_data_disk_is_refused_unless_allowed() {
        use std::os::unix::fs::PermissionsExt;
        let data = tempfile::tempdir().unwrap();
        let path = data.path().join("secret");
        std::fs::write(&path, format!("{HEX}\n")).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        let secret = StorageSecret::read_file(&path).unwrap();
        assert_eq!(secret.source, SecretSourceKind::File);
        assert_eq!(
            check_placement(&secret, data.path(), false),
            Err(SecretError::OnDataDisk)
        );
        assert_eq!(check_placement(&secret, data.path(), true), Ok(true));
        let in_process = StorageSecret::in_process([1; 32]);
        assert_eq!(check_placement(&in_process, data.path(), false), Ok(false));
    }

    #[cfg(unix)]
    #[test]
    fn a_secret_file_others_can_read_or_that_is_not_hex_is_refused() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("secret");
        std::fs::write(&path, HEX).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert!(StorageSecret::read_file(&path).is_err());
        std::fs::write(&path, "not a secret").unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        let refused = StorageSecret::read_file(&path).unwrap_err().to_string();
        assert!(refused.contains("64 hex"), "{refused}");
        assert!(!refused.contains("not a secret"), "{refused}");
    }
}
