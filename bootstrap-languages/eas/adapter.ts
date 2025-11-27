import type { Address, Agent, Expression, PublicSharing, LanguageContext, HolochainLanguageDelegate, ExpressionAdapter, AgentService } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import axiod from "https://deno.land/x/axiod/mod.ts";

export default class ExpressionAdapterImpl implements ExpressionAdapter {
  #agent: AgentService;
  putAdapter: PublicSharing

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
  }

  async get(ethAddr: Address): Promise<Expression> {
    let attestations = await axiod.post("https://easscan.org/graphql", {
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
        "attestationsWhere": {
          "recipient": {
            "equals": ethAddr
          }
        }
      }
    },
    {
      headers: {
        "Accept": "application/json",
        "Content-Type": "application/json",
      }
    });

    if (attestations.status !== 200) {
      console.error("Failed to fetch attestations", attestations);
      throw new Error("Failed to fetch attestations");
    };

    let attestationsCleaned = attestations.data.data.attestations;
    let attestationExpression = this.#agent.createSignedExpression(attestationsCleaned);

    return attestationExpression;
  };
}
