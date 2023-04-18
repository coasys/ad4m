use cid::Cid;
use deno_core::{error::AnyError, include_js_files, op, Extension};
use log::info;
use multibase::Base;
use multihash::{Code, MultihashDigest};

use crate::globals::SIGNING_DNA;

use super::JS_CORE_HANDLE;

#[op]
fn get_signing_dna() -> Result<Vec<u8>, AnyError> {
    Ok(SIGNING_DNA.to_vec())
}

#[op]
fn hash(data: String) -> Result<String, AnyError> {
    // Compute the SHA-256 multihash
    let multihash = Code::Sha2_256.digest(data.as_bytes());

    // Create a CID with default settings (version 1, DAG-Protobuf)
    let cid = Cid::new_v1(0, multihash);

    // Encode the CID in base58btc (IPFS default)
    let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());

    Ok(format!("Qm{}", encoded_cid))
}

#[op]
async fn load_module(path: String) -> Result<String, AnyError> {
    info!("Trying to load module: {}", path);
    let js_core_handle = JS_CORE_HANDLE
        .lock()
        .expect("Could not get lock on js_core_handle");

    let _res = js_core_handle.clone().unwrap().load_module(path).await;
    Ok(String::from("temp"))
}

pub fn build() -> Extension {
    Extension::builder("wallet")
        .js(include_js_files!(wallet "utils_extension.js",))
        .ops(vec![
            get_signing_dna::decl(),
            hash::decl(),
            load_module::decl(),
        ])
        .force_op_registration()
        .build()
}
