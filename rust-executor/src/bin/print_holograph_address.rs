//! Prints content-addressed AD4M Language hashes.
//!
//! Used by the JS integration test (`tests/js/tests/
//! holograph-link.test.ts`) to derive verified addresses without
//! re-implementing the AD4M hash algorithm in TS.
//!
//! Two modes:
//!   * No args: print the canonical
//!     `hash("@coasys/holograph-link@<v>")` package-id address used
//!     by the Step 6d `resolve_link_language` default switch.
//!   * One arg (file path): print the AD4M content-address of the
//!     file's bytes (SHA-256 -> CIDv1 -> base58btc -> "Qm" prefix).
//!     This is what `LanguageController::calculate_language_hash`
//!     computes, so a Language pre-installed at this address will
//!     pass `install_language`'s hash-verification.

use rust_executor::neighbourhoods::{holograph_link_default_address, HOLOGRAPH_LINK_PACKAGE_ID};

fn content_address(bytes: &[u8]) -> String {
    use cid::Cid;
    use multibase::Base;
    use multihash::{Code, MultihashDigest};
    let multihash = Code::Sha2_256.digest(bytes);
    let cid = Cid::new_v1(0, multihash);
    let encoded = multibase::encode(Base::Base58Btc, cid.to_bytes());
    format!("Qm{}", encoded)
}

fn main() {
    let mut args = std::env::args().skip(1);
    match args.next() {
        None => {
            println!("{}", holograph_link_default_address());
            eprintln!("(derived from package id: {})", HOLOGRAPH_LINK_PACKAGE_ID);
        }
        Some(path) => {
            let bytes = std::fs::read(&path).expect("read bundle");
            println!("{}", content_address(&bytes));
            eprintln!("(content-address of {})", path);
        }
    }
}
