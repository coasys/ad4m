import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService, HolochainLanguageDelegate, NeighbourhoodExpression } from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types"
import axios from "axios";
import https from "https";
import { DNA_NICK } from "./dna";
import { NeighbourhoodStorage } from "./neighbourhoodStorage";

class NeighbourhoodPutAdapter implements PublicSharing {
  #agent: AgentService
  #DNA: HolochainLanguageDelegate;

  constructor(context: LanguageContext) {
      this.#agent = context.agent
      this.#DNA = context.Holochain as HolochainLanguageDelegate;
  }

  async createPublic(neighbourhood: object): Promise<Address> {
    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "file_storage", fn_name, payload));

    const expression: any = this.#agent.createSignedExpression(neighbourhood)

    //Store the FileMetadataExpression
    const address = await storage.storeNeighbourhoodExpression(expression)
    if (!Buffer.isBuffer(address)) {
        throw new Error("Could not create FileExpression data")
    };

    //@ts-ignore
    return address.toString("hex")
  }
}

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;
  #DNA: HolochainLanguageDelegate;

  constructor(context: LanguageContext) {
    this.putAdapter = new NeighbourhoodPutAdapter(context);
    this.#DNA = context.Holochain as HolochainLanguageDelegate;
  }

  async get(address: Address): Promise<Expression> {
    const cid = address.toString();

    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language-language", fn_name, payload));

    let addressBuffer = Buffer.from(address, 'hex');
    const expression = (await storage.getNeighbourhoodExpression(addressBuffer)) as NeighbourhoodExpression

    if (!expression) {
      return null;
    };
    if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
        expression.data.data_base64 = "";
        return expression;
    };

    return expression
  }
}
