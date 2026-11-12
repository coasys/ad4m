//! Lowercase hex on the wire; any case and surrounding whitespace accepted, as the server
//! link language's `hexToBytes` accepts them.

use serde::{de::Error as _, Deserialize, Deserializer, Serializer};

fn decode<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
    let text = String::deserialize(deserializer)?;
    hex::decode(text.trim()).map_err(D::Error::custom)
}

pub mod array {
    use super::*;

    pub fn serialize<S: Serializer, const N: usize>(
        bytes: &[u8; N],
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&hex::encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>, const N: usize>(
        deserializer: D,
    ) -> Result<[u8; N], D::Error> {
        let bytes = decode(deserializer)?;
        let len = bytes.len();
        bytes
            .try_into()
            .map_err(|_| D::Error::custom(format!("expected {N} bytes of hex, found {len}")))
    }
}

pub mod vec {
    use super::*;

    pub fn serialize<S: Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&hex::encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
        decode(deserializer)
    }
}
