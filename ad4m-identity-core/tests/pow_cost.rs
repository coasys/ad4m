//! The time `pow::solve` takes at the store's default cost (22 bits), over 24 SCIDs. Not a
//! check: it is ignored by default and prints the mean, median and worst time and hash count.
//!
//! ```bash
//! cargo test --release -p ad4m-identity-core --test pow_cost -- --ignored --nocapture
//! CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=$PWD/target/wasm-tools/bin/wasm-bindgen-test-runner \
//!   cargo test --release -p ad4m-identity-core --target wasm32-unknown-unknown --features wasm \
//!   --test pow_cost -- --include-ignored --nocapture
//! ```

use ad4m_identity_core::pow;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const BITS: u8 = 22;
const SCIDS: u8 = 24;
/// A host challenge for the measurement (any text: the proof commits to it).
const CHALLENGE: &str = "5CoStMeAsUrE";

/// A SCID-shaped text: base58btc of a sha2-256 multihash of `i`.
fn scid(i: u8) -> String {
    let mut multihash = vec![0x12, 0x20];
    multihash.extend_from_slice(&ad4m_hash_chain::sha256(&[b'p', b'o', b'w', i]));
    bs58::encode(multihash).into_string()
}

/// Milliseconds from a fixed point.
#[cfg(not(target_arch = "wasm32"))]
fn now_ms() -> f64 {
    use std::sync::OnceLock;
    static START: OnceLock<std::time::Instant> = OnceLock::new();
    START
        .get_or_init(std::time::Instant::now)
        .elapsed()
        .as_secs_f64()
        * 1000.0
}

#[cfg(target_arch = "wasm32")]
fn now_ms() -> f64 {
    js_sys::Date::now()
}

fn print(line: &str) {
    #[cfg(target_arch = "wasm32")]
    wasm_bindgen_test::console_log!("{line}");
    #[cfg(not(target_arch = "wasm32"))]
    println!("{line}");
}

#[test]
#[ignore = "a measurement, not a check: run with --ignored"]
fn pow_solve_cost_at_the_default_bits() {
    let mut times = Vec::new();
    let mut hashes = Vec::new();
    for i in 0..SCIDS {
        let scid = scid(i);
        let started = now_ms();
        let proof = pow::solve(&scid, CHALLENGE, BITS);
        times.push(now_ms() - started);
        assert!(pow::check(&scid, &proof, BITS));
        let nonce: [u8; 16] = bs58::decode(&proof.nonce)
            .into_vec()
            .unwrap()
            .try_into()
            .unwrap();
        hashes.push(u128::from_be_bytes(nonce) + 1);
    }
    let mut sorted = times.clone();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = sorted.len();
    let mean = sorted.iter().sum::<f64>() / n as f64;
    let median = (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0;
    let worst = sorted[n - 1];
    let mean_hashes = hashes.iter().sum::<u128>() as f64 / n as f64;
    let rate = hashes.iter().sum::<u128>() as f64 / (times.iter().sum::<f64>() / 1000.0);
    print(&format!(
        "pow::solve at {BITS} bits over {n} SCIDs: mean {mean:.0} ms, median {median:.0} ms, \
         worst {worst:.0} ms; mean {mean_hashes:.0} hashes, {rate:.0} hashes/s"
    ));
    for (i, (t, h)) in times.iter().zip(&hashes).enumerate() {
        print(&format!("  scid {i:2}: {t:8.0} ms, {h:9} hashes"));
    }
}
