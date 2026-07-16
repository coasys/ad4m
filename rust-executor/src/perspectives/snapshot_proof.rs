//! Snapshot proof-bundle construction and verification.
//!
//! The wire type (`SnapshotProof`) lives in `crate::types::domain` so ts-rs can
//! export it to the TS SDK; this module holds the *logic* that produces and
//! verifies bundles using the local agent's signing service. A proof is simply
//! "signed by a DID": AD4M treats individual agents and groups (graphs that
//! carry their own DID) identically, so there is no signer-role distinction —
//! verification resolves the `signer_did`'s document and checks the signature,
//! whatever kind of identity that DID denotes.

use crate::agent::signatures::verify_string_signed_by_did;
use crate::agent::AgentContext;
use crate::agent::{did_for_context, sign_string_hex_for_context, signing_key_id_for_context};
use crate::types::SnapshotProof;
use deno_core::anyhow::{anyhow, Error};

/// The exact byte string a proof signs: the content-hash IRI and the snapshot
/// timestamp, joined by a single newline. The newline is a hard field separator
/// so no `(iri, timestamp)` pair can collide with another by shifting the
/// boundary. Both signing and verification route through this one function, so
/// the separator choice only has to be internally consistent.
pub fn signing_payload(graph_content_hash_iri: &str, timestamp: &str) -> String {
    format!("{graph_content_hash_iri}\n{timestamp}")
}

/// Produce the proof bundle for a snapshot being exported. Signs
/// `signing_payload(iri, timestamp)` with the DID carried by `context` and
/// returns a single proof. The bundle is a `Vec` because a snapshot may in
/// principle carry several co-signatures; export attaches exactly one — the
/// snapshotter's.
pub fn build_proof_bundle(
    graph_content_hash_iri: &str,
    timestamp: &str,
    context: &AgentContext,
) -> Result<Vec<SnapshotProof>, Error> {
    let payload = signing_payload(graph_content_hash_iri, timestamp);
    let signature = sign_string_hex_for_context(payload, context)?;
    Ok(vec![SnapshotProof {
        signer_did: did_for_context(context)?,
        signer_key_id: Some(signing_key_id_for_context(context)?),
        signature,
        timestamp: timestamp.to_string(),
    }])
}

/// Verify every proof in a bundle against the snapshot's content-hash IRI. An
/// empty bundle is rejected — an unsigned blob is not a snapshot (error text
/// `"unsigned snapshot"`). Each proof folds its own `timestamp` back into the
/// payload it signed and is checked against its `signer_did`; a DID whose method
/// cannot be resolved simply fails to verify, so unsupported identities are
/// rejected without any special-casing.
pub fn verify_proof_bundle(
    graph_content_hash_iri: &str,
    proofs: &[SnapshotProof],
) -> Result<(), Error> {
    if proofs.is_empty() {
        return Err(anyhow!("unsigned snapshot"));
    }
    for proof in proofs {
        let payload = signing_payload(graph_content_hash_iri, &proof.timestamp);
        let valid = verify_string_signed_by_did(&proof.signer_did, &payload, &proof.signature)?;
        if !valid {
            return Err(anyhow!(
                "snapshot proof signature invalid for signer {}",
                proof.signer_did
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{AgentContext, AgentService};
    use crate::test_utils::setup_wallet;
    use once_cell::sync::OnceCell;

    static SETUP: OnceCell<()> = OnceCell::new();

    /// Initialise the global wallet + test agent once for this module so
    /// `AgentContext::main_agent()` has a signing key.
    fn ensure_agent() {
        SETUP.get_or_init(|| {
            setup_wallet();
            AgentService::init_global_test_instance();
        });
    }

    const IRI: &str = "graph://f00dcafe";
    const TS: &str = "2024-01-15T10:00:00.000Z";

    #[test]
    fn signing_payload_joins_iri_and_timestamp_with_a_single_newline() {
        // The `\n` is a hard field separator: no (iri, ts) pair can collide with
        // another by shifting the boundary.
        assert_eq!(
            signing_payload("graph://h", "2024-01-01T00:00:00Z"),
            "graph://h\n2024-01-01T00:00:00Z"
        );
    }

    #[test]
    fn build_bundle_emits_a_single_verifiable_proof() {
        // Export yields exactly one proof over `(iri || timestamp)`, signed by
        // the DID carried by the local signing context.
        ensure_agent();
        let ctx = AgentContext::main_agent();
        let proofs = build_proof_bundle(IRI, TS, &ctx).expect("build proof");
        assert_eq!(proofs.len(), 1, "export yields exactly one proof");
        assert_eq!(proofs[0].timestamp, TS);
        assert!(
            proofs[0].signer_did.starts_with("did:"),
            "the proof is signed by a DID, got {}",
            proofs[0].signer_did
        );
        verify_proof_bundle(IRI, &proofs).expect("a freshly-built bundle must verify");
    }

    #[test]
    fn empty_bundle_is_an_unsigned_snapshot() {
        // An unsigned blob is not a snapshot.
        let err = verify_proof_bundle(IRI, &[])
            .expect_err("empty bundle must be rejected")
            .to_string();
        assert!(err.contains("unsigned snapshot"), "got: {err}");
    }

    #[test]
    fn tampered_signature_fails_verification() {
        ensure_agent();
        let ctx = AgentContext::main_agent();
        let mut proofs = build_proof_bundle(IRI, TS, &ctx).unwrap();
        proofs[0].signature = "00".repeat(64);
        assert!(
            verify_proof_bundle(IRI, &proofs).is_err(),
            "a tampered signature must fail verification"
        );
    }

    #[test]
    fn proof_bound_to_a_different_iri_fails_verification() {
        // The proof is bound to the exact content-hash IRI it signed; verifying
        // it against any other IRI (a tampered hash) must fail.
        ensure_agent();
        let ctx = AgentContext::main_agent();
        let proofs = build_proof_bundle(IRI, TS, &ctx).unwrap();
        assert!(
            verify_proof_bundle("graph://tampered", &proofs).is_err(),
            "verifying against a different content-hash IRI must fail"
        );
    }

    #[test]
    fn proof_from_an_unresolvable_did_fails_verification() {
        // A DID whose method cannot be resolved to a key yields no valid
        // signature, so the proof is rejected — no identity is trusted just for
        // asserting a DID.
        let proof = SnapshotProof {
            signer_did: "did:graph:whatever".to_string(),
            signer_key_id: None,
            signature: "deadbeef".to_string(),
            timestamp: TS.to_string(),
        };
        assert!(
            verify_proof_bundle(IRI, &[proof]).is_err(),
            "a proof from an unresolvable DID must fail verification"
        );
    }
}
