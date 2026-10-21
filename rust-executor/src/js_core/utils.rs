use cid::Cid;
use multibase::Base;
use multihash_codetable::{Code, MultihashDigest};
use std::collections::BTreeMap;

/// The address content is stored and found under: a CIDv1 over the SHA-256 of its bytes, base58,
/// with the `Qm` prefix AD4M has always written. What the `hash` op answers a Language with, and
/// what names a stored tool bundle.
pub fn content_address(data: &str) -> String {
    let multihash = Code::Sha2_256.digest(data.as_bytes());
    let cid = Cid::new_v1(0, multihash);
    format!("Qm{}", multibase::encode(Base::Base58Btc, cid.to_bytes()))
}

pub fn sort_json_value(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(k.clone(), sort_json_value(v));
            }
            serde_json::Value::Object(serde_json::Map::from_iter(map))
        }
        serde_json::Value::Array(arr) => {
            serde_json::Value::Array(arr.iter().map(sort_json_value).collect())
        }
        _ => value.clone(),
    }
}
