import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService, HolochainLanguageDelegate, NeighbourhoodExpression } from "@perspect3vism/ad4m";
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
    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "neighbourhood_storage", fn_name, payload));

    const expression: any = this.#agent.createSignedExpression(neighbourhood)

    //Store the FileMetadataExpression
    const address = await storage.storeNeighbourhoodExpression(expression)
    if (!Buffer.isBuffer(address)) {
        throw new Error("Could not create NeighbourhoodExpression data")
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
    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "neighbourhood_storage", fn_name, payload));

    let addressBuffer = Buffer.from(address, 'hex');
    const expression = (await storage.getNeighbourhoodExpression(addressBuffer)) as NeighbourhoodExpression
    if (!expression) {
      return null;
    };

    return expression
  }
}
