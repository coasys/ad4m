//! HPKE (RFC 9180) base mode, single shot, with DHKEM(X25519, HKDF-SHA256), HKDF-SHA256 and
//! ChaCha20-Poly1305: the one way the device inbox and the private record seal to an X25519
//! key. A sealed text is `base64url(enc ‖ ciphertext)` without padding: the 32-byte
//! encapsulated key, then the AEAD ciphertext with its 16-byte tag. The AAD is empty; the
//! `info` string binds the seal to its purpose and place.

use base64::Engine;
use hpke::aead::ChaCha20Poly1305;
use hpke::kdf::HkdfSha256;
use hpke::kem::X25519HkdfSha256;
use hpke::{Deserializable, Kem, OpModeR, OpModeS, Serializable};
use rand_core::{CryptoRng, RngCore};
use zeroize::Zeroizing;

use crate::keys::{decode_x25519_multikey, X25519Secret};

type KemX = X25519HkdfSha256;
type PublicKey = <KemX as Kem>::PublicKey;
type PrivateKey = <KemX as Kem>::PrivateKey;
type EncappedKey = <KemX as Kem>::EncappedKey;

/// Bytes of the encapsulated key at the start of a sealed text.
const ENC_LEN: usize = 32;
/// Bytes of the AEAD tag at the end.
const TAG_LEN: usize = 16;

/// Why a seal cannot be made (`Recipient`) or opened (`Open`). An open never says more than
/// "does not open".
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SealError {
    /// The recipient is not an X25519 multikey.
    Recipient,
    /// Not base64url, too short, or the AEAD refuses it (wrong key, wrong `info`, tampered).
    Open,
}

/// base64url without padding: the text of a seal, and of a record's ciphertext.
pub(crate) fn b64() -> base64::engine::GeneralPurpose {
    base64::engine::general_purpose::URL_SAFE_NO_PAD
}

/// True when `text` is non-empty base64url without padding: the shape check of a sealed text a
/// store keeps but cannot open.
pub(crate) fn is_base64url(text: &str) -> bool {
    !text.is_empty()
        && text
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_')
}

/// Seals `plaintext` to the X25519 multikey `to` under `info`.
pub(crate) fn seal<R: CryptoRng + RngCore>(
    to: &str,
    info: &[u8],
    plaintext: &[u8],
    rng: &mut R,
) -> Result<String, SealError> {
    let key = decode_x25519_multikey(to).ok_or(SealError::Recipient)?;
    let key = PublicKey::from_bytes(&key).map_err(|_| SealError::Recipient)?;
    let (enc, ciphertext) = hpke::single_shot_seal::<ChaCha20Poly1305, HkdfSha256, KemX, _>(
        &OpModeS::Base,
        &key,
        info,
        plaintext,
        &[],
        rng,
    )
    .map_err(|_| SealError::Recipient)?;
    let mut out = enc.to_bytes().to_vec();
    out.extend_from_slice(&ciphertext);
    Ok(b64().encode(out))
}

/// Opens a sealed text with `secret` under `info`. The plaintext is wiped when dropped: it can
/// be a record key or a master key.
pub(crate) fn open(
    secret: &X25519Secret,
    info: &[u8],
    sealed: &str,
) -> Result<Zeroizing<Vec<u8>>, SealError> {
    let bytes = b64().decode(sealed).map_err(|_| SealError::Open)?;
    if bytes.len() < ENC_LEN + TAG_LEN {
        return Err(SealError::Open);
    }
    let (enc, ciphertext) = bytes.split_at(ENC_LEN);
    let enc = EncappedKey::from_bytes(enc).map_err(|_| SealError::Open)?;
    let key = PrivateKey::from_bytes(Zeroizing::new(secret.to_bytes()).as_slice())
        .map_err(|_| SealError::Open)?;
    hpke::single_shot_open::<ChaCha20Poly1305, HkdfSha256, KemX>(
        &OpModeR::Base,
        &key,
        &enc,
        info,
        ciphertext,
        &[],
    )
    .map(Zeroizing::new)
    .map_err(|_| SealError::Open)
}

/// `label ‖ 0x00 ‖ place`: the `info` of a seal.
pub(crate) fn info(label: &str, place: &str) -> Vec<u8> {
    let mut out = label.as_bytes().to_vec();
    out.push(0);
    out.extend_from_slice(place.as_bytes());
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keys::x25519_multikey_of;

    /// Bytes from a counter: a fixed RNG for repeatable seals.
    struct Counter(u8);
    impl RngCore for Counter {
        fn next_u32(&mut self) -> u32 {
            rand_core::impls::next_u32_via_fill(self)
        }
        fn next_u64(&mut self) -> u64 {
            rand_core::impls::next_u64_via_fill(self)
        }
        fn fill_bytes(&mut self, dest: &mut [u8]) {
            for b in dest {
                self.0 = self.0.wrapping_add(1);
                *b = self.0;
            }
        }
        fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand_core::Error> {
            self.fill_bytes(dest);
            Ok(())
        }
    }
    impl CryptoRng for Counter {}

    #[test]
    fn a_seal_opens_only_with_its_key_and_info() {
        let secret = X25519Secret::from([3u8; 32]);
        let to = x25519_multikey_of(&secret);
        let sealed = seal(&to, b"a\0b", b"hello", &mut Counter(0)).unwrap();
        assert_eq!(
            open(&secret, b"a\0b", &sealed).unwrap().as_slice(),
            b"hello"
        );
        assert_eq!(open(&secret, b"a\0c", &sealed), Err(SealError::Open));
        let other = X25519Secret::from([4u8; 32]);
        assert_eq!(open(&other, b"a\0b", &sealed), Err(SealError::Open));
        let mut bytes = b64().decode(&sealed).unwrap();
        let last = bytes.len() - 1;
        bytes[last] ^= 1;
        assert_eq!(
            open(&secret, b"a\0b", &b64().encode(bytes)),
            Err(SealError::Open)
        );
        assert_eq!(open(&secret, b"a\0b", "short"), Err(SealError::Open));
        assert_eq!(
            seal("z6Mknot", b"", b"", &mut Counter(0)),
            Err(SealError::Recipient)
        );
    }

    #[test]
    fn the_same_rng_gives_the_same_seal() {
        let to = x25519_multikey_of(&X25519Secret::from([3u8; 32]));
        assert_eq!(
            seal(&to, b"i", b"p", &mut Counter(7)).unwrap(),
            seal(&to, b"i", b"p", &mut Counter(7)).unwrap()
        );
    }
}
