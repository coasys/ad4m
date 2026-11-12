//! Sealed archives: the platform snapshot leaves the executor only in this form.
//!
//! The snapshot is a tar.gz of RocksDB checkpoints of the perspectives' SPARQL stores, and
//! those stores are plain on the disk, so the archive as a whole is sealed before it leaves:
//! no tar header, file name or link in it is readable without this executor's keyring.
//!
//! **The format.**
//!
//! ```text
//! "AD4MARCH" | format (u8, 1) | keyring id (16) | key version (u32) | nonce prefix (19)
//!            | segment 1 | ... | segment n
//! ```
//!
//! integers big endian. The segments are the archive cut into 64 KiB pieces, each sealed
//! with XChaCha20-Poly1305 in the STREAM construction (big-endian 32-bit counter, last-segment
//! flag), so each sealed segment is 16 bytes longer than its piece and an archive cut short
//! or reordered does not open. The key is the executor store key's subkey for archives
//! ([`SubkeyPurpose::Archive`]) of the version in the header. Every segment's associated data
//! is a label and the whole header, so a header changed in any byte does not open either.
//!
//! An archive opens only under the keyring that sealed it.

use std::path::PathBuf;

use ad4m_keyring::KeyVersion;
use chacha20poly1305::aead::generic_array::GenericArray;
use chacha20poly1305::aead::stream::{DecryptorBE32, EncryptorBE32};
use chacha20poly1305::aead::{KeyInit, Payload};
use chacha20poly1305::XChaCha20Poly1305;
use rand::RngCore;

use super::data_keys::{DataKeys, KeyringError, SubkeyPurpose};
use super::files;
use super::keyring_file::{KeyringId, StoreId};

/// The first bytes of a sealed archive.
pub const MAGIC: &[u8; 8] = b"AD4MARCH";
/// The format byte after [`MAGIC`].
pub const FORMAT: u8 = 1;
/// The plaintext length of every segment but the last.
pub const SEGMENT: usize = 64 * 1024;

const KEYRING_ID_LEN: usize = 16;
/// XChaCha20's 24-byte nonce less STREAM's 4-byte counter and 1-byte last flag.
const NONCE_PREFIX_LEN: usize = 19;
const TAG_LEN: usize = 16;
/// The length of the header.
pub const HEADER_LEN: usize = MAGIC.len() + 1 + KEYRING_ID_LEN + 4 + NONCE_PREFIX_LEN;
const AD_LABEL: &[u8] = b"ad4m:at-rest:archive:v1";

/// The file whose presence says this executor has uploaded a sealed snapshot, so a
/// plaintext one is no longer restored. Next to the keyring file, in `{data}/ad4m`.
const SEALED_ONLY_FILE: &str = "platform-snapshot-sealed";

/// Why an archive was not sealed or opened. No variant carries plaintext or key material.
#[derive(Debug, thiserror::Error)]
pub enum ArchiveError {
    #[error(transparent)]
    Keyring(#[from] KeyringError),
    #[error("not a sealed archive")]
    NotAnArchive,
    #[error("sealed archive format {0} is not one this executor reads")]
    UnknownFormat(u8),
    #[error("the archive was sealed under keyring {archive}, not this executor's keyring {ours}")]
    OtherKeyring { archive: String, ours: String },
    #[error("the archive does not open under this executor's key: damaged or cut short")]
    DoesNotOpen,
}

/// Whether `bytes` start as a sealed archive does.
pub fn is_sealed_archive(bytes: &[u8]) -> bool {
    bytes.starts_with(MAGIC)
}

fn associated_data(header: &[u8]) -> Vec<u8> {
    let mut ad = Vec::with_capacity(8 + AD_LABEL.len() + header.len());
    for part in [AD_LABEL, header] {
        ad.extend_from_slice(&(part.len() as u32).to_be_bytes());
        ad.extend_from_slice(part);
    }
    ad
}

/// Seals `plain` under the current version of the executor store key's archive subkey.
pub fn seal_archive(keys: &DataKeys, plain: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let (version, key) = keys.subkey(&StoreId::Executor, None, SubkeyPurpose::Archive)?;
    let mut nonce_prefix = [0u8; NONCE_PREFIX_LEN];
    rand::thread_rng().fill_bytes(&mut nonce_prefix);

    let segments = plain.len().div_ceil(SEGMENT).max(1);
    let mut out = Vec::with_capacity(HEADER_LEN + plain.len() + segments * TAG_LEN);
    out.extend_from_slice(MAGIC);
    out.push(FORMAT);
    out.extend_from_slice(&keys.keyring_id().0);
    out.extend_from_slice(&version.0.to_be_bytes());
    out.extend_from_slice(&nonce_prefix);
    let ad = associated_data(&out[..HEADER_LEN]);

    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let mut encryptor = EncryptorBE32::from_aead(cipher, GenericArray::from_slice(&nonce_prefix));
    let mut pieces = plain.chunks(SEGMENT).peekable();
    let failed = |_| ArchiveError::DoesNotOpen;
    loop {
        let piece = pieces.next().unwrap_or(&[]);
        let payload = Payload {
            msg: piece,
            aad: &ad,
        };
        if pieces.peek().is_none() {
            out.extend_from_slice(&encryptor.encrypt_last(payload).map_err(failed)?);
            break;
        }
        out.extend_from_slice(&encryptor.encrypt_next(payload).map_err(failed)?);
    }
    Ok(out)
}

/// Opens an archive [`seal_archive`] sealed under this keyring.
pub fn open_archive(keys: &DataKeys, sealed: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let rest = sealed
        .strip_prefix(&MAGIC[..])
        .ok_or(ArchiveError::NotAnArchive)?;
    let (&format, rest) = rest.split_first().ok_or(ArchiveError::NotAnArchive)?;
    if format != FORMAT {
        return Err(ArchiveError::UnknownFormat(format));
    }
    if sealed.len() < HEADER_LEN + TAG_LEN {
        return Err(ArchiveError::DoesNotOpen);
    }
    let keyring_id = KeyringId(rest[..KEYRING_ID_LEN].try_into().expect("16 bytes"));
    let ours = keys.keyring_id();
    if keyring_id != ours {
        return Err(ArchiveError::OtherKeyring {
            archive: keyring_id.to_hex(),
            ours: ours.to_hex(),
        });
    }
    let rest = &rest[KEYRING_ID_LEN..];
    let version = KeyVersion(u32::from_be_bytes(rest[..4].try_into().expect("4 bytes")));
    let nonce_prefix = &rest[4..4 + NONCE_PREFIX_LEN];
    let (_, key) = keys.subkey(&StoreId::Executor, Some(version), SubkeyPurpose::Archive)?;
    let ad = associated_data(&sealed[..HEADER_LEN]);

    let cipher = XChaCha20Poly1305::new_from_slice(key.expose_secret()).expect("32-byte key");
    let mut decryptor = DecryptorBE32::from_aead(cipher, GenericArray::from_slice(nonce_prefix));
    let mut body = &sealed[HEADER_LEN..];
    let mut plain = Vec::with_capacity(body.len());
    let failed = |_| ArchiveError::DoesNotOpen;
    while body.len() > SEGMENT + TAG_LEN {
        let (segment, next) = body.split_at(SEGMENT + TAG_LEN);
        let payload = Payload {
            msg: segment,
            aad: &ad,
        };
        plain.extend_from_slice(&decryptor.decrypt_next(payload).map_err(failed)?);
        body = next;
    }
    let payload = Payload {
        msg: body,
        aad: &ad,
    };
    plain.extend_from_slice(&decryptor.decrypt_last(payload).map_err(failed)?);
    Ok(plain)
}

fn sealed_only_path(keys: &DataKeys) -> PathBuf {
    keys.path()
        .parent()
        .map_or_else(PathBuf::new, |folder| folder.to_path_buf())
        .join(SEALED_ONLY_FILE)
}

/// Records that this executor has uploaded a sealed platform snapshot: from now on a
/// plaintext snapshot is refused rather than restored.
pub fn mark_sealed_only(keys: &DataKeys) -> std::io::Result<()> {
    let path = sealed_only_path(keys);
    if path.exists() {
        return Ok(());
    }
    files::write_atomic(&path, b"")
}

/// Whether this executor has uploaded a sealed platform snapshot ([`mark_sealed_only`]).
pub fn is_sealed_only(keys: &DataKeys) -> bool {
    sealed_only_path(keys).exists()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use zeroize::Zeroizing;

    fn open_keys(folder: &Path, secret: u8) -> DataKeys {
        let keys = DataKeys::load(super::super::keyring_file::path_in(folder)).unwrap();
        keys.unlock(Zeroizing::new([secret; 32])).unwrap();
        keys
    }

    fn pattern(len: usize) -> Vec<u8> {
        (0..len).map(|i| (i * 31 % 251) as u8).collect()
    }

    #[test]
    fn archives_of_every_length_open_to_what_was_sealed() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        for len in [
            0,
            1,
            SEGMENT - 1,
            SEGMENT,
            SEGMENT + 1,
            2 * SEGMENT,
            3 * SEGMENT + 17,
        ] {
            let plain = pattern(len);
            let sealed = seal_archive(&keys, &plain).unwrap();
            let segments = len.div_ceil(SEGMENT).max(1);
            assert_eq!(sealed.len(), HEADER_LEN + len + segments * TAG_LEN, "{len}");
            assert_eq!(open_archive(&keys, &sealed).unwrap(), plain, "{len}");
        }
    }

    #[test]
    fn the_header_names_the_format_the_keyring_and_the_key_version() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        let sealed = seal_archive(&keys, b"perspectives/").unwrap();
        assert!(is_sealed_archive(&sealed));
        assert_eq!(&sealed[..8], b"AD4MARCH");
        assert_eq!(sealed[8], FORMAT);
        assert_eq!(&sealed[9..25], &keys.keyring_id().0);
        let (version, _) = keys
            .subkey(&StoreId::Executor, None, SubkeyPurpose::Archive)
            .unwrap();
        assert_eq!(&sealed[25..29], &version.0.to_be_bytes());
        assert!(!sealed
            .windows(b"perspectives".len())
            .any(|w| w == b"perspectives"));
        // A new nonce prefix each time: the same archive never seals to the same bytes.
        assert_ne!(sealed, seal_archive(&keys, b"perspectives/").unwrap());
    }

    #[test]
    fn a_changed_byte_anywhere_does_not_open() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        let sealed = seal_archive(&keys, &pattern(SEGMENT + 100)).unwrap();
        // The nonce prefix, the first segment, the second, the last byte.
        for at in [29, 40, HEADER_LEN, HEADER_LEN + SEGMENT, sealed.len() - 1] {
            let mut changed = sealed.clone();
            changed[at] ^= 1;
            assert!(
                matches!(
                    open_archive(&keys, &changed),
                    Err(ArchiveError::DoesNotOpen)
                ),
                "byte {at}"
            );
        }
        // A key version the keyring does not hold.
        let mut version = sealed.clone();
        version[28] ^= 1;
        assert!(matches!(
            open_archive(&keys, &version),
            Err(ArchiveError::Keyring(KeyringError::UnknownStore))
        ));
        let mut format = sealed.clone();
        format[8] = 2;
        assert!(matches!(
            open_archive(&keys, &format),
            Err(ArchiveError::UnknownFormat(2))
        ));
    }

    #[test]
    fn an_archive_cut_short_does_not_open() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        let sealed = seal_archive(&keys, &pattern(2 * SEGMENT + 10)).unwrap();
        // Cut at a segment boundary: the segment now last was not sealed as the last one.
        let at_boundary = &sealed[..HEADER_LEN + 2 * (SEGMENT + TAG_LEN)];
        assert!(matches!(
            open_archive(&keys, at_boundary),
            Err(ArchiveError::DoesNotOpen)
        ));
        assert!(matches!(
            open_archive(&keys, &sealed[..sealed.len() - 1]),
            Err(ArchiveError::DoesNotOpen)
        ));
        assert!(matches!(
            open_archive(&keys, &sealed[..HEADER_LEN]),
            Err(ArchiveError::DoesNotOpen)
        ));
    }

    #[test]
    fn an_archive_opens_only_under_the_keyring_that_sealed_it() {
        let ours = tempfile::tempdir().unwrap();
        let theirs = tempfile::tempdir().unwrap();
        let keys = open_keys(ours.path(), 3);
        let other = open_keys(theirs.path(), 3);
        let sealed = seal_archive(&keys, b"links").unwrap();
        assert!(matches!(
            open_archive(&other, &sealed),
            Err(ArchiveError::OtherKeyring { .. })
        ));
        // The same keyring id with another key (a forged header) still does not open.
        let mut forged = seal_archive(&other, b"links").unwrap();
        forged[9..25].copy_from_slice(&keys.keyring_id().0);
        assert!(open_archive(&keys, &forged).is_err());
    }

    #[test]
    fn a_sealed_keyring_neither_seals_nor_opens() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        let sealed = seal_archive(&keys, b"links").unwrap();
        let locked = DataKeys::load(keys.path().to_path_buf()).unwrap();
        assert!(matches!(
            seal_archive(&locked, b"links"),
            Err(ArchiveError::Keyring(KeyringError::Sealed))
        ));
        assert!(matches!(
            open_archive(&locked, &sealed),
            Err(ArchiveError::Keyring(KeyringError::Sealed))
        ));
        assert!(matches!(
            open_archive(&keys, b"\x1f\x8b plain gzip"),
            Err(ArchiveError::NotAnArchive)
        ));
    }

    #[test]
    fn the_sealed_only_mark_lives_next_to_the_keyring() {
        let folder = tempfile::tempdir().unwrap();
        let keys = open_keys(folder.path(), 3);
        assert!(!is_sealed_only(&keys));
        mark_sealed_only(&keys).unwrap();
        mark_sealed_only(&keys).unwrap();
        assert!(is_sealed_only(&keys));
        assert!(keys
            .path()
            .parent()
            .unwrap()
            .join(SEALED_ONLY_FILE)
            .exists());
    }
}
