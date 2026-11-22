use chacha20poly1305::aead::{AeadInPlace, KeyInit};
use chacha20poly1305::{XChaCha20Poly1305, XNonce};
use rand_core::{OsRng, RngCore};
use serde::{Deserialize, Serialize};
use zeroize::Zeroizing;

use crate::{hex_serde, hkdf_subkey, tags, KeyringError, SecretKey, KEY_LEN};

/// The bytes of a wrap's nonce.
pub const WRAP_NONCE_LEN: usize = 24;
/// The bytes of an AEAD tag.
const TAG_LEN: usize = 16;

/// The AEAD of a [`SymWrap`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WrapAlg {
    /// XChaCha20-Poly1305: a 24-byte random nonce, so random nonces never collide in practice.
    #[serde(rename = "xchacha20poly1305")]
    XChaCha20Poly1305,
}

/// A key wrapped under another symmetric key. Holds no secret: the nonce and the
/// ciphertext (the 32-byte key and the 16-byte tag), hex on the wire.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymWrap {
    pub alg: WrapAlg,
    #[serde(with = "hex_serde::array")]
    pub nonce: [u8; WRAP_NONCE_LEN],
    #[serde(with = "hex_serde::vec")]
    pub ct: Vec<u8>,
}

/// One key in a keyring: what it is, which one, and its version. For example
/// `("person", "main", 1)`, `("store", "perspective:<uuid>", 2)`, or `("root", "", g)` for the
/// root key of generation `g`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KeyRef<'a> {
    pub kind: &'a str,
    pub id: &'a str,
    pub version: u32,
}

impl<'a> KeyRef<'a> {
    pub fn new(kind: &'a str, id: &'a str, version: u32) -> Self {
        KeyRef { kind, id, version }
    }

    fn encode_into(&self, out: &mut Vec<u8>) {
        push_field(out, self.kind.as_bytes());
        push_field(out, self.id.as_bytes());
        out.extend_from_slice(&self.version.to_be_bytes());
    }
}

/// Where a wrap belongs: the keyring, the key it holds and the key it is wrapped under. It is
/// the wrap's associated data, so a wrap copied to another entry, another version or another
/// keyring does not open.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WrapContext<'a> {
    pub keyring_id: &'a [u8; 16],
    pub wrapped: KeyRef<'a>,
    pub wrapper: KeyRef<'a>,
}

impl WrapContext<'_> {
    /// The associated data: the domain tag, the keyring id, then the wrapped key and its
    /// wrapper, every variable-length field prefixed with its length (u64, big-endian), so two
    /// different contexts never give the same bytes.
    pub fn associated_data(&self) -> Vec<u8> {
        let mut out = Vec::new();
        push_field(&mut out, tags::DEK_WRAP_V1.as_bytes());
        out.extend_from_slice(self.keyring_id);
        self.wrapped.encode_into(&mut out);
        self.wrapper.encode_into(&mut out);
        out
    }
}

fn push_field(out: &mut Vec<u8>, bytes: &[u8]) {
    out.extend_from_slice(&(bytes.len() as u64).to_be_bytes());
    out.extend_from_slice(bytes);
}

/// The AEAD key of every wrap under `under`: derived for wraps alone, so a key that wraps is
/// never used raw by this cipher for anything else.
fn wrap_cipher(under: &SecretKey) -> XChaCha20Poly1305 {
    let kek = hkdf_subkey(under, &[], tags::DEK_WRAP_V1);
    XChaCha20Poly1305::new(kek.expose_secret().into())
}

/// Wraps `key` under `under` for the place `ctx` names, with a fresh random nonce.
pub fn wrap(key: &SecretKey, under: &SecretKey, ctx: &WrapContext) -> SymWrap {
    let mut nonce = [0u8; WRAP_NONCE_LEN];
    OsRng.fill_bytes(&mut nonce);
    // Room for the tag up front: the buffer never reallocates, so no unwiped copy of the key
    // is freed.
    let mut buffer = Zeroizing::new(Vec::with_capacity(KEY_LEN + TAG_LEN));
    buffer.extend_from_slice(key.expose_secret());
    wrap_cipher(under)
        .encrypt_in_place(
            XNonce::from_slice(&nonce),
            &ctx.associated_data(),
            &mut *buffer,
        )
        .expect("XChaCha20-Poly1305 seals 32 bytes");
    SymWrap {
        alg: WrapAlg::XChaCha20Poly1305,
        nonce,
        ct: std::mem::take(&mut *buffer),
    }
}

/// Opens a wrap made by [`wrap`]. Fails with [`KeyringError::Unwrap`] for another wrapping
/// key, any other field in `ctx`, or an altered wrap.
pub fn unwrap(
    w: &SymWrap,
    under: &SecretKey,
    ctx: &WrapContext,
) -> Result<SecretKey, KeyringError> {
    let WrapAlg::XChaCha20Poly1305 = w.alg;
    if w.ct.len() != KEY_LEN + TAG_LEN {
        return Err(KeyringError::Unwrap);
    }
    let mut buffer = Zeroizing::new(w.ct.clone());
    wrap_cipher(under)
        .decrypt_in_place(
            XNonce::from_slice(&w.nonce),
            &ctx.associated_data(),
            &mut *buffer,
        )
        .map_err(|_| KeyringError::Unwrap)?;
    SecretKey::from_slice(&buffer)
}
