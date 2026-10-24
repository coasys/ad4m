//! The store's signed heads (`coasys:head`, AR-01 4.5.2, cache rule 4): a head counts only when
//! one of the store keys in the executor's config (`identity.storeKeys`) signed it.

use ad4m_hash_chain::decode_proof_value;
use ad4m_identity_core::verify_signed;
use ad4m_ldk::identity::SignedHead;
use serde_json::json;

/// The signing domain of the store's heads.
pub const HEAD_DOMAIN: &str = "coasys:head";

/// True when `head` is signed by its `store` key and that key is one of `trusted` (multikeys).
pub fn verify_head(head: &SignedHead, trusted: &[String]) -> bool {
    let Some(key) = head.store.strip_prefix("did:key:") else {
        return false;
    };
    if !trusted.iter().any(|t| t == key) {
        return false;
    }
    let Some(sig) = decode_proof_value(&head.sig) else {
        return false;
    };
    let value = json!({
        "scid": head.scid, "seq": head.seq, "hash": head.hash, "time": head.time, "store": head.store,
    });
    verify_signed(HEAD_DOMAIN, &value, key, &sig)
}

#[cfg(test)]
pub(crate) mod test_store_key {
    //! A store head key for tests, signing as `identity-store` does.
    use super::*;
    use ad4m_hash_chain::{encode_ed25519_multikey, encode_proof_value};
    use ad4m_identity_core::signing_bytes;
    use ed25519_dalek::SigningKey;

    pub struct TestStoreKey(pub SigningKey);

    impl TestStoreKey {
        pub fn new(seed: u8) -> Self {
            TestStoreKey(SigningKey::from_bytes(&[seed; 32]))
        }

        pub fn multikey(&self) -> String {
            encode_ed25519_multikey(self.0.verifying_key().as_bytes())
        }

        pub fn sign_head(&self, scid: &str, seq: u64, hash: &str, time: u64) -> SignedHead {
            let store = format!("did:key:{}", self.multikey());
            let value =
                json!({ "scid": scid, "seq": seq, "hash": hash, "time": time, "store": store });
            let bytes = signing_bytes(HEAD_DOMAIN, &value).unwrap();
            let sig = ed25519_dalek::Signer::sign(&self.0, &bytes).to_bytes();
            SignedHead {
                scid: scid.to_string(),
                seq,
                hash: hash.to_string(),
                time,
                store,
                sig: encode_proof_value(&sig),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_store_key::TestStoreKey;
    use super::*;

    #[test]
    fn a_head_counts_only_under_a_configured_store_key() {
        let key = TestStoreKey::new(1);
        let other = TestStoreKey::new(2);
        let head = key.sign_head("QmScid", 3, "QmHash", 1_700_000_000);
        assert!(verify_head(&head, &[key.multikey()]));
        assert!(verify_head(&head, &[other.multikey(), key.multikey()]));
        assert!(!verify_head(&head, &[other.multikey()]));
        assert!(!verify_head(&head, &[]));
        let mut moved = head.clone();
        moved.seq = 4;
        assert!(!verify_head(&moved, &[key.multikey()]));
    }
}
