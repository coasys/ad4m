/**
 * Flat export version of aes-language (Ethereum Attestation Service adapter).
 * Tests the flat export pattern with a real API-backed language.
 */
import axiod from "https://deno.land/x/axiod/mod.ts";

// Direct exports (flat pattern)
export const name = "aes-language-flat";
export const version = "0.1.0";

// Module-level state
let agent: any = null;

export async function init(contextJson: string): Promise<void> {
    const context = JSON.parse(contextJson);
    // Agent available via globalThis (set by bootstrap before init)
    agent = globalThis.__agentProxy__;
    console.log("[aes-flat] init() called");
}

export function interactions(expressionAddress: string): any[] {
    return [];
}

// ExpressionUI
export function expressionIcon(): string {
    return "";
}

export function expressionConstructorIcon(): string {
    return "";
}

// ExpressionAdapter.get
export async function expressionGet(address: string): Promise<any | null> {
    try {
        const response = await axiod.post("https://easscan.org/graphql", {
            query: `
                query Query($attestationsWhere: AttestationWhereInput) {
                    attestations(where: $attestationsWhere) {
                        id, data, decodedDataJson, recipient, attester,
                        time, timeCreated, expirationTime, revocationTime,
                        refUID, revocable, revoked, txid, schemaId, ipfsHash, isOffchain
                    }
                }
            `,
            variables: {
                "attestationsWhere": { "recipient": { "equals": address } }
            }
        }, {
            headers: {
                "Accept": "application/json",
                "Content-Type": "application/json",
            }
        });

        if (response.status !== 200) {
            console.error("[aes-flat] Failed to fetch attestations:", response.status);
            return null;
        }

        const attestations = response.data.data.attestations;
        return agent.createSignedExpression(attestations);
    } catch (e) {
        console.error("[aes-flat] expressionGet error:", e);
        return null;
    }
}

// ExpressionAdapter.putAdapter.createPublic
export async function expressionCreate(content: object): Promise<string> {
    // EAS is read-only (attestations come from Ethereum)
    throw new Error("EAS language does not support creating attestations");
}

// isImmutableExpression
export function isImmutableExpression(address: string): boolean {
    return false;
}

// Teardown
export async function teardown(): Promise<void> {
    console.log("[aes-flat] teardown() called");
}
