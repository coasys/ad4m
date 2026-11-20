use cid::Cid;
use deno_core::op2;
use log::{log, Level};
use multibase::Base;
use multihash_codetable::{Code, MultihashDigest};

use crate::js_core::error::AnyhowWrapperError;
#[op2]
#[string]
fn hash(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    // Compute the SHA-256 multihash
    let multihash = Code::Sha2_256.digest(data.as_bytes());

    // Create a CID with default settings (version 1, DAG-Protobuf)
    let cid = Cid::new_v1(0, multihash);

    // Encode the CID in base58btc (IPFS default)
    let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());

    Ok(format!("Qm{}", encoded_cid))
}

/// Test-only record of every console line with its level: a test cannot rely on
/// `log::set_logger` (a prior test's `try_init` may already own the global logger).
#[cfg(test)]
pub(crate) static CONSOLE_LINES: std::sync::Mutex<Vec<(Level, String)>> =
    std::sync::Mutex::new(Vec::new());

fn console_line(level: Level, data: &str) {
    #[cfg(test)]
    CONSOLE_LINES
        .lock()
        .unwrap()
        .push((level, data.to_string()));
    log!(level, "[JSCORE]: {}", data);
}

#[op2]
#[string]
fn console_log(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    // JS `console.log` maps to Rust `debug` — mirroring browser semantics,
    // where console.log is developer chatter, not an operator-facing event.
    // JS callers that need info-level visibility use `console.info`.
    console_line(Level::Debug, &data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_debug(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    console_line(Level::Debug, &data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_info(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    console_line(Level::Info, &data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_error(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    console_line(Level::Error, &data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_warn(#[string] data: String) -> Result<String, AnyhowWrapperError> {
    console_line(Level::Warn, &data);

    Ok(String::from("temp"))
}

deno_core::extension!(
    utils_service,
    ops = [hash, console_log, console_debug, console_info, console_error, console_warn],
    esm_entry_point = "ext:utils_service/utils_extension.js",
    esm = [dir "src/js_core", "utils_extension.js"]
);
