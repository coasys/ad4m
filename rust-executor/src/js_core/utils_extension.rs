use cid::Cid;
use deno_core::op2;
use log::{debug, error, warn};
use multibase::Base;
use multihash_codetable::{Code, MultihashDigest};

use crate::js_core::error::AnyhowWrapperError;

/// The canonical AD4M content-address hash (spec section 7.7): SHA-256
/// multihash in a CIDv1, base58btc-encoded, prefixed with `"Qm"`.
///
/// This format is frozen: existing expression and language addresses are
/// these strings, so every runtime must produce them byte for byte. It is not
/// an IPFS CIDv0 (`Qm` + base58 multihash); it is `"Qm"` followed by a
/// base58btc multibase CIDv1 (which starts with `z`). The vectors in
/// `ad4m-ldk/js/test/hash-vectors.json` pin it.
fn content_hash(data: &str) -> String {
    // Compute the SHA-256 multihash
    let multihash = Code::Sha2_256.digest(data.as_bytes());

    // CIDv1 with codec 0x00. (DAG-Protobuf would be 0x70; do not change the
    // codec, it is part of the frozen format.)
    let cid = Cid::new_v1(0, multihash);

    // Encode the CID in base58btc (IPFS default)
    let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());

    format!("Qm{}", encoded_cid)
}

#[op2]
#[string]
fn hash(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    Ok(content_hash(&data))
}

#[op2]
#[string]
fn console_log(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    // JS `console.log` maps to Rust `debug` — mirroring browser semantics,
    // where console.log is developer chatter, not an operator-facing event.
    // JS callers that need info-level visibility should use `console.info`
    // (routed via a follow-up op) or emit through a structured channel.
    debug!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_debug(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    debug!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_error(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    error!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_warn(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    warn!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

deno_core::extension!(
    utils_service,
    ops = [hash, console_log, console_debug, console_error, console_warn],
    esm_entry_point = "ext:utils_service/utils_extension.js",
    esm = [dir "src/js_core", "utils_extension.js"]
);

#[cfg(test)]
mod tests {
    use super::content_hash;
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct Vector {
        name: String,
        input: String,
        output: String,
    }

    /// `hash()` is the content address every content-addressed Language
    /// computes (spec section 7.7): two runtimes must agree or Expression
    /// URLs stop being portable. These vectors pin the algorithm; the same
    /// JSON is shared with JS-side tests.
    #[test]
    fn hash_vectors_match_pinned_json() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../ad4m-ldk/js/test/hash-vectors.json"
        );
        let json =
            std::fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot read {path}: {e}"));
        let vectors: Vec<Vector> = serde_json::from_str(&json).unwrap();
        assert!(!vectors.is_empty(), "no vectors in {path}");
        for v in &vectors {
            assert_eq!(
                content_hash(&v.input),
                v.output,
                "hash vector {:?} failed",
                v.name
            );
        }
    }
}
