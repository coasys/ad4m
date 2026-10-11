/**
 * # AES (Ethereum Attestation Service) Language
 *
 * Expression language that fetches attestations from EAS (easscan.org).
 * Read-only: expressionGet fetches attestations for an Ethereum address,
 * no expressionCreate (attestations are created on-chain).
 */

import axiod from "https://deno.land/x/axiod/mod.ts";
import { defineLanguage, agentCreateSignedExpression } from "@coasys/ad4m-ldk";

const language = defineLanguage({
    name: "aes-language",
    version: "0.1.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        isImmutable(_address: string): boolean {
            return false;
        },

        async get(ethAddr: string): Promise<any> {
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
            let attestationExpression =
                agentCreateSignedExpression(attestationsCleaned);

            return attestationExpression;
        },
    },
});

export const {
    name,
    version,
    init,
    teardown,
    interactions,
    expressionGet,
    isImmutableExpression,
} = language;
