//! Prints the canonical AD4M content-address for the holograph-link
//! Language. Used by the JS integration test (`tests/js/tests/
//! holograph-link.test.ts`) to pre-install the bundle at the address
//! the executor will resolve to under
//! `HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1`.
//!
//! The address is computed deterministically from
//! `HOLOGRAPH_LINK_PACKAGE_ID` so callers (test scripts, build steps,
//! humans) can always re-derive it without booting a runtime.

use rust_executor::neighbourhoods::{holograph_link_default_address, HOLOGRAPH_LINK_PACKAGE_ID};

fn main() {
    println!("{}", holograph_link_default_address());
    eprintln!("(derived from package id: {})", HOLOGRAPH_LINK_PACKAGE_ID);
}
