use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::Deserialize;
use std::fmt;

#[derive(Debug)]
pub struct ByteArray(Vec<u8>);

impl From<ByteArray> for Vec<u8> {
    fn from(bytes: ByteArray) -> Vec<u8> {
        bytes.0
    }
}

struct ByteArrayVisitor;

impl<'de> Visitor<'de> for ByteArrayVisitor {
    type Value = ByteArray;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a map representing a byte array")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut bytes = Vec::new();

        while let Some((key, value)) = access.next_entry::<String, u8>()? {
            let index: usize = key.parse().map_err(de::Error::custom)?;
            // Ensure the vector is large enough to hold the byte at index `key`.
            if index >= bytes.len() {
                bytes.resize(index + 1, 0);
            }
            bytes[index] = value;
        }

        Ok(ByteArray(bytes))
    }
}

impl<'de> Deserialize<'de> for ByteArray {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(ByteArrayVisitor)
    }
}
