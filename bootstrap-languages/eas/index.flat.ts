/**
 * # AES (Ethereum Attestation Service) Language — Flat Export Language
 *
 * Expression language that fetches attestations from EAS (easscan.org).
 * Read-only: expressionGet fetches attestations for an Ethereum address,
 * no expressionCreate (attestations are created on-chain).
 * Flat-export migration of the legacy create()-factory version.
 */

import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "aes-language";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

let agent: any = null;

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
}

export async function teardown(): Promise<void> {
    agent = null;
}

export function interactions(): any[] {
    return [];
}

export function isImmutableExpression(_address: string): boolean {
    return false;
}

// =============================================================================
// Expression capability (read-only — no expressionCreate)
// =============================================================================

export async function expressionGet(ethAddr: string): Promise<any> {
    let attestations = await axiod.post(
        "https://easscan.org/graphql",
        {
            query: `
        query Query($attestationsWhere: AttestationWhereInput) {
          attestations(where: $attestationsWhere) {
            id
            data
            decodedDataJson
            recipient
            attester
            time
            timeCreated
            expirationTime
            revocationTime
            refUID
            revocable
            revoked
            txid
            schemaId
            ipfsHash
            isOffchain
          }
        }
      `,
            variables: {
                attestationsWhere: {
                    recipient: {
                        equals: ethAddr,
                    },
                },
            },
        },
        {
            headers: {
                Accept: "application/json",
                "Content-Type": "application/json",
            },
        }
    );

    if (attestations.status !== 200) {
        console.error("Failed to fetch attestations", attestations);
        throw new Error("Failed to fetch attestations");
    }

    let attestationsCleaned = attestations.data.data.attestations;
    let attestationExpression = agent.createSignedExpression(attestationsCleaned);

    return attestationExpression;
}
