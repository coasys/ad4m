//! Vouching notes (decisions 23.1 and 23.2 of the identity architecture): the platform's signed
//! statement that it checked one email address for an identity. The store keeps at most one
//! note beside a log; everyone reads `vouched = note present && verify_note passes`.
//!
//! A note is signed under [`VOUCH_DOMAIN`] with the domain-field scheme
//! ([`crate::domain::signing_bytes`]) over `{did, method, time, voucher}`; `voucher` is the
//! `did:key` of the vouching key and `sig` the proof value (multibase base58btc) of the
//! signature. The shape is the wire type `ad4m_ldk::identity::VouchNote`, member for member.

use ad4m_hash_chain::{decode_proof_value, Multikey};
use serde::{Deserialize, Serialize};

use crate::domain::{signing_bytes, verify_signed, DomainError};

/// The signing domain of vouching notes.
pub const VOUCH_DOMAIN: &str = "coasys:vouch";
/// The method of an email check.
pub const METHOD_EMAIL: &str = "email";

/// A vouching note, as the store keeps it and `LogPage.note` carries it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VouchNote {
    /// The identity the note vouches for (`did:scid:coasys:1:…`).
    pub did: String,
    /// How the platform checked it: `"email"`.
    pub method: String,
    /// When, in unix seconds.
    pub time: u64,
    /// `did:key` of the vouching key.
    pub voucher: String,
    /// The vouching key's signature under [`VOUCH_DOMAIN`] (a proof value).
    pub sig: String,
}

/// What the vouching key signs.
#[derive(Serialize)]
struct NoteStatement<'a> {
    did: &'a str,
    method: &'a str,
    time: u64,
    voucher: &'a str,
}

impl VouchNote {
    fn statement(&self) -> NoteStatement<'_> {
        NoteStatement {
            did: &self.did,
            method: &self.method,
            time: self.time,
            voucher: &self.voucher,
        }
    }

    /// The bytes the vouching key signs: [`signing_bytes`] of `{did, method, time, voucher}`
    /// under [`VOUCH_DOMAIN`].
    pub fn signing_bytes(&self) -> Result<Vec<u8>, DomainError> {
        signing_bytes(VOUCH_DOMAIN, &self.statement())
    }
}

/// True when `note.sig` is the signature of the key `note.voucher` names over the note, and that
/// key is one of `trusted` (multikeys). A reader checks `note.did` against the identity it reads
/// on its own: the note answers who vouched, not for whom it is shown.
pub fn verify_note(note: &VouchNote, trusted: &[Multikey]) -> bool {
    let Some(key) = note.voucher.strip_prefix("did:key:") else {
        return false;
    };
    if !trusted.iter().any(|t| t == key) {
        return false;
    }
    let Some(sig) = decode_proof_value(&note.sig) else {
        return false;
    };
    verify_signed(VOUCH_DOMAIN, &note.statement(), key, &sig)
}

/// A note for `did` checked by `method` at `time`, signed by the vouching key `signer`. The
/// vouching service signs with it; the core's tests and the store's contract tests too.
#[cfg(feature = "build")]
pub fn sign_note(
    did: &str,
    method: &str,
    time: u64,
    signer: &dyn crate::build::Signer,
) -> Result<VouchNote, DomainError> {
    let mut note = VouchNote {
        did: did.to_string(),
        method: method.to_string(),
        time,
        voucher: format!("did:key:{}", signer.key()),
        sig: String::new(),
    };
    let bytes = note.signing_bytes()?;
    note.sig = ad4m_hash_chain::encode_proof_value(&signer.sign(&bytes));
    Ok(note)
}

#[cfg(test)]
mod tests {
    use ed25519_dalek::SigningKey;

    use super::*;

    const DID: &str = "did:scid:coasys:1:zQmExample";

    trait Key {
        fn key(&self) -> Multikey;
    }

    impl Key for SigningKey {
        fn key(&self) -> Multikey {
            ad4m_hash_chain::encode_ed25519_multikey(self.verifying_key().as_bytes())
        }
    }

    /// A note signed by `voucher`, made here so the tests run without the `build` feature.
    fn sign_note(
        did: &str,
        method: &str,
        time: u64,
        voucher: &SigningKey,
    ) -> Result<VouchNote, DomainError> {
        let mut note = VouchNote {
            did: did.into(),
            method: method.into(),
            time,
            voucher: format!("did:key:{}", voucher.key()),
            sig: String::new(),
        };
        let bytes = note.signing_bytes()?;
        let sig = ed25519_dalek::Signer::sign(voucher, &bytes).to_bytes();
        note.sig = ad4m_hash_chain::encode_proof_value(&sig);
        Ok(note)
    }

    #[cfg(feature = "build")]
    #[test]
    fn the_build_signer_makes_the_same_note() {
        let voucher = SigningKey::from_bytes(&[3; 32]);
        let ours = sign_note(DID, METHOD_EMAIL, 9, &voucher).unwrap();
        let built = super::sign_note(DID, METHOD_EMAIL, 9, &voucher).unwrap();
        assert_eq!(ours, built);
        assert!(verify_note(&built, &[voucher.key()]));
    }

    #[test]
    fn a_note_verifies_under_its_trusted_voucher_and_nothing_else() {
        let voucher = SigningKey::from_bytes(&[3; 32]);
        let other = SigningKey::from_bytes(&[4; 32]);
        let note = sign_note(DID, METHOD_EMAIL, 1_800_000_000, &voucher).unwrap();
        assert_eq!(note.voucher, format!("did:key:{}", voucher.key()));
        assert!(verify_note(&note, &[other.key(), voucher.key()]));
        // An untrusted voucher.
        assert!(!verify_note(&note, &[other.key()]));
        assert!(!verify_note(&note, &[]));
        // Every signed member is covered.
        for change in [
            |n: &mut VouchNote| n.did.push('x'),
            |n: &mut VouchNote| n.method = "phone".into(),
            |n: &mut VouchNote| n.time += 1,
        ] {
            let mut changed = note.clone();
            change(&mut changed);
            assert!(!verify_note(&changed, &[voucher.key()]));
        }
        // A voucher swapped for another trusted key, the signature kept.
        let mut swapped = note.clone();
        swapped.voucher = format!("did:key:{}", other.key());
        assert!(!verify_note(&swapped, &[voucher.key(), other.key()]));
        // A voucher that is no did:key, and a broken signature.
        let mut bare = note.clone();
        bare.voucher = voucher.key();
        assert!(!verify_note(&bare, &[voucher.key()]));
        let mut broken = note;
        broken.sig = "znot-a-signature".into();
        assert!(!verify_note(&broken, &[voucher.key()]));
    }

    #[test]
    fn a_note_signs_in_the_vouch_domain_only() {
        let voucher = SigningKey::from_bytes(&[3; 32]);
        let note = sign_note(DID, METHOD_EMAIL, 7, &voucher).unwrap();
        let sig = decode_proof_value(&note.sig).unwrap();
        // The same statement under another domain does not verify.
        assert!(!verify_signed(
            crate::HEAD_DOMAIN,
            &note.statement(),
            &voucher.key(),
            &sig
        ));
        let text = String::from_utf8(note.signing_bytes().unwrap()).unwrap();
        assert!(text.contains(r#""domain":"coasys:vouch""#), "{text}");
    }

    #[test]
    fn the_wire_shape_is_the_ldk_one() {
        let voucher = SigningKey::from_bytes(&[3; 32]);
        let note = sign_note(DID, METHOD_EMAIL, 7, &voucher).unwrap();
        let value = serde_json::to_value(&note).unwrap();
        let mut members: Vec<&str> = value
            .as_object()
            .unwrap()
            .keys()
            .map(String::as_str)
            .collect();
        members.sort_unstable();
        assert_eq!(members, ["did", "method", "sig", "time", "voucher"]);
        let back: VouchNote = serde_json::from_value(value).unwrap();
        assert_eq!(back, note);
    }
}
