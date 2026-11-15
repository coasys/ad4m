//! The seed of an agent's X25519 key for room keys (slice 3.B, K8).
//!
//! The server link language opens room keys with an X25519 key of its own, not the agent's
//! Ed25519 key in Montgomery form: a device key signs that X25519 key under `coasys:room-key`
//! (`ad4m-identity-core` `wire::room_key_signing_bytes`) and the link server hands it to every
//! member who seals a room key. Its secret is
//! `HKDF-SHA256(agent signing seed, info = ROOM_KEY_SEED_INFO)`: the language asks for it with
//! `agentRoomKeySeed`, so the agent's signing secret never reaches language code.
//!
//! No signing op signs a message equal to or starting with [`ROOM_KEY_SEED_INFO`]
//! ([`refuse_room_key_seed_domain`], called by `sign_for_context` and
//! `sign_string_hex_for_context`): language code that can sign cannot get a signature over the
//! seed's domain, so no derivation over that domain can be made from a signature either.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use hkdf::Hkdf;
use sha2::Sha256;
use zeroize::Zeroizing;

use crate::agent::AgentContext;
use crate::wallet::wallet_backend;

/// The HKDF info of the room key seed, and the prefix no signing op signs.
pub const ROOM_KEY_SEED_INFO: &[u8] = b"adam-server-link-language:x25519-seed:v1";

/// Refuses a message to sign that is, or starts with, [`ROOM_KEY_SEED_INFO`].
pub fn refuse_room_key_seed_domain(message: &[u8]) -> Result<(), AnyError> {
    if message.starts_with(ROOM_KEY_SEED_INFO) {
        return Err(anyhow!(
            "Refusing to sign a message in the room key seed's domain (adam-server-link-language:x25519-seed:v1)"
        ));
    }
    Ok(())
}

/// The room key seed of an Ed25519 secret (its first 32 bytes are the seed).
pub fn room_key_seed_of(ed25519_secret: &[u8]) -> Result<Zeroizing<[u8; 32]>, AnyError> {
    let seed = ed25519_secret
        .get(..32)
        .ok_or_else(|| anyhow!("An Ed25519 secret key has at least 32 bytes"))?;
    let mut out = Zeroizing::new([0u8; 32]);
    Hkdf::<Sha256>::new(None, seed)
        .expand(ROOM_KEY_SEED_INFO, out.as_mut())
        .map_err(|_| anyhow!("HKDF output length"))?;
    Ok(out)
}

/// The room key seed of the agent of `context`: the main agent or a managed user. Fails when
/// the wallet holds no secret for it (a locked wallet, or a shared-mode key held elsewhere).
pub fn room_key_seed_for_context(context: &AgentContext) -> Result<Zeroizing<[u8; 32]>, AnyError> {
    let key_name = context.wallet_key_name()?;
    let secret = Zeroizing::new(
        wallet_backend()
            .get_secret_key(&key_name)
            .ok_or_else(|| anyhow!("{} key not found", key_name))?,
    );
    room_key_seed_of(secret.as_slice())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{sign_for_context, sign_string_hex_for_context, AgentSignature};

    fn ensure_setup() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
    }

    /// The seed of the Ed25519 seed 0x01 * 32: HKDF-SHA256 with no salt and the domain as
    /// info (RFC 5869, computed apart from this code). The server link language's tests pin
    /// the same value (`tests/auth.test.ts`).
    #[test]
    fn the_seed_is_hkdf_of_the_signing_seed_under_the_domain() {
        let seed = room_key_seed_of(&[1u8; 32]).unwrap();
        assert_eq!(
            hex::encode(*seed),
            "d6364e920c0d6c40cc94a94a385e08a46a842e087549c972de5bc2041f36799f"
        );
        // A 64-byte secret (seed ‖ public key) derives from its seed.
        let mut long = [1u8; 64];
        long[32..].fill(9);
        assert_eq!(*room_key_seed_of(&long).unwrap(), *seed);
        assert!(room_key_seed_of(&[1u8; 31]).is_err());
    }

    #[test]
    fn the_seed_differs_per_agent_and_is_not_the_signing_seed() {
        let a = room_key_seed_of(&[1u8; 32]).unwrap();
        let b = room_key_seed_of(&[2u8; 32]).unwrap();
        assert_ne!(*a, *b);
        assert_ne!(*a, [1u8; 32]);
    }

    #[test]
    fn the_main_agent_has_a_seed() {
        ensure_setup();
        let main = AgentContext::main_agent();
        let one = room_key_seed_for_context(&main).unwrap();
        let two = room_key_seed_for_context(&main).unwrap();
        assert_eq!(*one, *two);
    }

    /// K8: every signing op refuses the seed's domain, alone or as a prefix, and signs
    /// anything else.
    #[test]
    fn every_signing_op_refuses_the_seed_domain() {
        ensure_setup();
        let main = AgentContext::main_agent();
        let domain = "adam-server-link-language:x25519-seed:v1";
        let prefixed = format!("{domain}:more");

        assert!(sign_for_context(domain.as_bytes(), &main).is_err());
        assert!(sign_for_context(prefixed.as_bytes(), &main).is_err());
        assert!(sign_string_hex_for_context(domain.to_string(), &main).is_err());
        assert!(sign_string_hex_for_context(prefixed.clone(), &main).is_err());
        assert!(AgentSignature::from_message_for_context(domain.to_string(), &main).is_err());

        // Not a prefix: signed.
        assert!(sign_for_context(format!("x{domain}").as_bytes(), &main).is_ok());
        assert!(sign_string_hex_for_context(format!("x{domain}"), &main).is_ok());
        assert!(sign_for_context(b"adam-server-link-language:x25519-seed:v", &main).is_ok());
    }
}
