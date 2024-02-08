use std::borrow::Cow;

use cid::Cid;
use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use multibase::Base;
use multihash::{Code, MultihashDigest};
use log::{error, info, debug, warn};

use super::JS_CORE_HANDLE;

#[op2]
#[string]
fn hash(#[string] data: String) -> Result<String, AnyError> {
    // Compute the SHA-256 multihash
    let multihash = Code::Sha2_256.digest(data.as_bytes());

    // Create a CID with default settings (version 1, DAG-Protobuf)
    let cid = Cid::new_v1(0, multihash);

    // Encode the CID in base58btc (IPFS default)
    let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());

    Ok(format!("Qm{}", encoded_cid))
}

#[op2]
#[string]
fn console_log(#[string] data: String) -> Result<String, AnyError> {
    info!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_debug(#[string] data: String) -> Result<String, AnyError> {
    debug!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_error(#[string] data: String) -> Result<String, AnyError> {
    error!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2]
#[string]
fn console_warn(#[string] data: String) -> Result<String, AnyError> {
    warn!("[JSCORE]: {}", data);

    Ok(String::from("temp"))
}

#[op2(async)]
#[string]
async fn load_module(#[string] path: String) -> Result<String, AnyError> {
    info!("Trying to load module: {}", path);

    let mut js_core_handle = JS_CORE_HANDLE.lock().await;

    if let Some(ref mut value) = *js_core_handle {
        // Call mutable functions on JsCoreHandle
        let _res = value.load_module(path).await;
    }

    Ok(String::from("temp"))
}

pub fn build() -> Extension {
    Extension {
        name: "utils",
        js_files: Cow::Borrowed(&include_js_files!(holochain_service "src/js_core/utils_extension.js",)),
        ops: Cow::Borrowed(&[
            hash::DECL,
            load_module::DECL,
            console_log::DECL,
            console_debug::DECL,
            console_error::DECL,
            console_warn::DECL,
        ]),
        ..Default::default()
    }
}
