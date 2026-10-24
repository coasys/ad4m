//! The 12 words and master-key derivation (feature `words`; AR-01 4.4).
//!
//! - 12 BIP-39 English words, 128 bits of entropy; the BIP-39 seed with an empty passphrase.
//! - Master key *n*: SLIP-0010 Ed25519 at the hardened path `m/7133'/0'/n'`.
//! - `salt_n = HKDF-SHA256(ikm = seed, salt = "coasys/1", info = "next-salt/" ‖ n)[0..16]`,
//!   with *n* in decimal ASCII; the commitment carries it as base58btc.
//! - The private record's recovery key: `HKDF-SHA256(ikm = seed, salt = "coasys/1",
//!   info = "record-recovery")`, 32 bytes, as an X25519 secret (AR-02 4.9).
//!
//! The words make master *n* and `salt_n` for every *n*, so a restore with the words alone
//! reproduces every commitment and no device stores the next key or its salt. Device keys never
//! come from the words. `Words`, `Seed` and every intermediate secret are zeroed on drop.

use bip39::{Language, Mnemonic};
use ed25519_dalek::SigningKey;
use hkdf::Hkdf;
use hmac::{Hmac, Mac};
use rand_core::{CryptoRng, OsRng, RngCore};
use sha2::{Sha256, Sha512};
use std::fmt;
use x25519_dalek::StaticSecret;
use zeroize::Zeroizing;

use crate::{commitment, Hash};

/// The number of words.
pub const WORD_COUNT: usize = 12;
/// The first index of the master-key path `m/7133'/0'/n'`.
pub const PATH_PURPOSE: u32 = 7133;
/// The second index of the master-key path.
pub const PATH_ACCOUNT: u32 = 0;
/// The salt length in bytes.
pub const SALT_LEN: usize = 16;
/// The HKDF salt of every derivation from the seed.
pub const HKDF_SALT: &[u8] = b"coasys/1";
/// The HKDF `info` of the recovery key.
pub const RECOVERY_INFO: &[u8] = b"record-recovery";

const HARDENED: u32 = 0x8000_0000;

/// 12 BIP-39 English words, lower case, one space apart. Never printed by `Debug`.
pub struct Words(Zeroizing<String>);

impl Words {
    /// The words as one line.
    pub fn phrase(&self) -> &str {
        &self.0
    }
    /// The words in order.
    pub fn words(&self) -> Vec<&str> {
        self.0.split(' ').collect()
    }
}

impl fmt::Debug for Words {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Words({WORD_COUNT} words)")
    }
}

/// The 64-byte BIP-39 seed. Never printed by `Debug`.
pub struct Seed(Zeroizing<[u8; 64]>);

impl Seed {
    pub fn as_bytes(&self) -> &[u8; 64] {
        &self.0
    }
}

impl fmt::Debug for Seed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Seed(..)")
    }
}

/// Why words do not parse, or a derivation index is out of range.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum WordsError {
    #[error("{0} words, expected 12")]
    Count(usize),
    /// `position` counts from 1. `suggestion` is a list word with the same first four letters,
    /// or else the nearest list word at edit distance 1 or 2.
    #[error("word {position} ({word:?}) is not a BIP-39 English word")]
    UnknownWord {
        position: usize,
        word: String,
        suggestion: Option<String>,
    },
    #[error("the words do not end with their checksum: one of them is wrong")]
    Checksum,
    #[error("master key index {0} is not below 2^31")]
    Index(u32),
}

/// New words from the operating system's random source.
pub fn generate() -> Words {
    generate_with(&mut OsRng)
}

/// New words from the caller's random source.
pub fn generate_with<R: RngCore + CryptoRng>(rng: &mut R) -> Words {
    let mut entropy = Zeroizing::new([0u8; 16]);
    rng.fill_bytes(&mut entropy[..]);
    from_entropy(&entropy)
}

/// The words of 16 bytes of entropy (BIP-39).
pub fn from_entropy(entropy: &[u8; 16]) -> Words {
    let mut phrase = Zeroizing::new(String::with_capacity(12 * 9));
    // 16 bytes is a valid BIP-39 entropy length, so `from_entropy_in` cannot refuse it.
    let mnemonic = Mnemonic::from_entropy_in(Language::English, entropy)
        .expect("16 bytes is a BIP-39 entropy length");
    for (i, word) in mnemonic.words().enumerate() {
        if i > 0 {
            phrase.push(' ');
        }
        phrase.push_str(word);
    }
    Words(phrase)
}

/// Parses words as a person types them: any whitespace between words (one per line works),
/// any case. Names the first word the list lacks, with a suggestion.
pub fn parse(input: &str) -> Result<Words, WordsError> {
    let lower = Zeroizing::new(input.to_lowercase());
    let words: Vec<&str> = lower.split_whitespace().collect();
    if words.len() != WORD_COUNT {
        return Err(WordsError::Count(words.len()));
    }
    for (i, word) in words.iter().enumerate() {
        if Language::English.find_word(word).is_none() {
            return Err(WordsError::UnknownWord {
                position: i + 1,
                word: word.to_string(),
                suggestion: suggest(word).map(str::to_string),
            });
        }
    }
    let phrase = Zeroizing::new(words.join(" "));
    // Count and words are checked above, so the only refusal left is the checksum.
    match Mnemonic::parse_in_normalized(Language::English, &phrase) {
        Ok(_) => Ok(Words(phrase)),
        Err(_) => Err(WordsError::Checksum),
    }
}

/// A list word for an unknown `word`: the word with the same first four letters (English
/// BIP-39 words differ in their first four), else the nearest word at edit distance 1 or 2.
fn suggest(word: &str) -> Option<&'static str> {
    let list = Language::English.word_list();
    let prefix: String = word.chars().take(4).collect();
    if prefix.chars().count() == 4 {
        if let Some(found) = list.iter().find(|w| w.starts_with(&prefix)) {
            return Some(found);
        }
    }
    let mut best: Option<(usize, &'static str)> = None;
    for candidate in list.iter() {
        let d = edit_distance(word, candidate);
        if d <= 2 && best.is_none_or(|(bd, _)| d < bd) {
            best = Some((d, candidate));
        }
    }
    best.map(|(_, w)| w)
}

fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut row: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.iter().enumerate() {
        let mut prev = row[0];
        row[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let cur = row[j + 1];
            row[j + 1] = if ca == cb {
                prev
            } else {
                1 + prev.min(row[j]).min(cur)
            };
            prev = cur;
        }
    }
    row[b.len()]
}

/// The BIP-39 seed of the words, with an empty passphrase.
pub fn seed(words: &Words) -> Seed {
    // `Words` is only made from a checked phrase, so it always parses.
    let mnemonic = Mnemonic::parse_in_normalized(Language::English, words.phrase())
        .expect("Words holds a checked phrase");
    Seed(Zeroizing::new(mnemonic.to_seed_normalized("")))
}

type HmacSha512 = Hmac<Sha512>;

fn hmac_sha512(key: &[u8], parts: &[&[u8]]) -> Zeroizing<[u8; 64]> {
    let mut mac = HmacSha512::new_from_slice(key).expect("HMAC takes a key of any length");
    for part in parts {
        mac.update(part);
    }
    let mut out = Zeroizing::new([0u8; 64]);
    out.copy_from_slice(&mac.finalize().into_bytes());
    out
}

/// A SLIP-0010 node: the private key and the chain code.
pub type Slip10Node = (Zeroizing<[u8; 32]>, Zeroizing<[u8; 32]>);

/// SLIP-0010 Ed25519 derivation from `seed` along `path`, every index hardened (Ed25519 has
/// only hardened children). Gives the private key and the chain code. Each index must be below 2^31.
pub fn derive_ed25519(seed: &[u8], path: &[u32]) -> Result<Slip10Node, WordsError> {
    let mut i = hmac_sha512(b"ed25519 seed", &[seed]);
    for &index in path {
        if index >= HARDENED {
            return Err(WordsError::Index(index));
        }
        let hardened = (index | HARDENED).to_be_bytes();
        i = hmac_sha512(&i[32..], &[&[0u8], &i[..32], &hardened]);
    }
    let mut key = Zeroizing::new([0u8; 32]);
    let mut chain = Zeroizing::new([0u8; 32]);
    key.copy_from_slice(&i[..32]);
    chain.copy_from_slice(&i[32..]);
    Ok((key, chain))
}

/// Master key *n*: SLIP-0010 Ed25519 at `m/7133'/0'/n'`. `n` must be below 2^31.
pub fn master_key(seed: &Seed, n: u32) -> Result<SigningKey, WordsError> {
    let (key, _) = derive_ed25519(seed.as_bytes(), &[PATH_PURPOSE, PATH_ACCOUNT, n])?;
    Ok(SigningKey::from_bytes(&key))
}

fn hkdf(seed: &Seed, info: &[u8], out: &mut [u8]) {
    Hkdf::<Sha256>::new(Some(HKDF_SALT), seed.as_bytes())
        .expand(info, out)
        .expect("HKDF-SHA256 gives up to 8160 bytes");
}

/// `salt_n`: the first 16 bytes of `HKDF-SHA256(seed, "coasys/1", "next-salt/<n>")`.
pub fn salt(seed: &Seed, n: u32) -> [u8; SALT_LEN] {
    let mut out = [0u8; SALT_LEN];
    hkdf(seed, format!("next-salt/{n}").as_bytes(), &mut out);
    out
}

/// The commitment to master *n* and `salt_n` as base58btc: what #0 (for *n* = 1) or the
/// rotation to master *n* − 1 (for later *n*) puts in `next`, and what the rotation to
/// master *n* reveals.
pub fn next_commitment(seed: &Seed, n: u32) -> Result<(Hash, String), WordsError> {
    let key = master_key(seed, n)?;
    let multikey = ad4m_hash_chain::encode_ed25519_multikey(key.verifying_key().as_bytes());
    let salt = bs58::encode(salt(seed, n)).into_string();
    Ok((commitment(&multikey, &salt), salt))
}

/// The private record's recovery key (AR-02 4.9; decision 22.1).
pub fn recovery_secret(seed: &Seed) -> StaticSecret {
    let mut out = Zeroizing::new([0u8; 32]);
    hkdf(seed, RECOVERY_INFO, &mut out[..]);
    StaticSecret::from(*out)
}
