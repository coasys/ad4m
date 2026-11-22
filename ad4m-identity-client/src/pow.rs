//! The proof of work for a new log with no vouching note (the store's brake, section 14 of
//! `docs-src/identity/did-scid-coasys-1.md`). The party that builds event #0 solves it: the
//! store refuses a new log without one (`403 pow`), and the executor passes it to the store
//! unchanged (`identity.publish`).
//!
//! The solve runs on the caller's thread and does not yield: about `2^bits` hashes. A surface
//! with a UI thread runs it on a worker thread (natively) or in a web worker (WASM).

use ad4m_identity_core::pow as core_pow;
use ad4m_ldk::identity::{Pow, PowParams};

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::ClientError;

/// The highest cost the client solves. The store's default is 22 bits; each bit doubles the
/// work, so 28 bits is 64 times the default. A store that asks for more is refused
/// ([`ClientError::PowTooCostly`]), as `pow::solve` does not return until it finds a proof.
pub const MAX_POW_BITS: u8 = 28;

/// A proof for `scid` at `params.bits` over `params.challenge` (a `GET /v1/pow` answer): the
/// store spends the challenge once, and only until `challenge.expires`, so a publish that ran
/// past it, and a resumed create, read `identity.powParams` again for a fresh one.
pub fn solve(scid: &str, params: &PowParams) -> Result<Pow, ClientError> {
    if params.bits > MAX_POW_BITS {
        return Err(ClientError::PowTooCostly(params.bits));
    }
    let solved = core_pow::solve(scid, &params.challenge.nonce, params.bits);
    Ok(Pow {
        nonce: solved.nonce,
        bits: solved.bits,
        challenge: solved.challenge,
    })
}

/// Reads the store's cost and a fresh challenge through the executor (`identity.powParams`,
/// from the store's `GET /v1/pow`) and solves the proof for `scid` over them.
pub async fn solve_for<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    scid: &str,
) -> Result<Pow, ClientError> {
    let params = executor.pow_params().await?;
    solve(scid, &params)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad4m_ldk::identity::PowChallenge;

    fn params(bits: u8) -> PowParams {
        PowParams {
            bits,
            challenge: PowChallenge {
                nonce: "5TestChallenge".into(),
                expires: 0,
            },
        }
    }

    #[test]
    fn a_solved_proof_passes_the_check_at_the_asked_bits() {
        let scid = "QmTestScidForThePowOfTheClient";
        let pow = solve(scid, &params(10)).unwrap();
        assert_eq!(pow.bits, 10);
        assert_eq!(pow.challenge, "5TestChallenge");
        let core = core_pow::Pow {
            nonce: pow.nonce.clone(),
            bits: pow.bits,
            challenge: pow.challenge.clone(),
        };
        assert!(core_pow::check(scid, &core, 10));
        assert_eq!(solve(scid, &params(10)).unwrap(), pow, "deterministic");
    }

    #[test]
    fn a_cost_above_the_cap_is_refused_before_any_work() {
        match solve("QmAny", &params(MAX_POW_BITS + 1)) {
            Err(ClientError::PowTooCostly(bits)) => assert_eq!(bits, MAX_POW_BITS + 1),
            other => panic!("{other:?}"),
        }
    }
}
