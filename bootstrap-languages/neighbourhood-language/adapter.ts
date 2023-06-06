import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService, HolochainLanguageDelegate, NeighbourhoodExpression } from "@perspect3vism/ad4m";
import { DNA_NICK } from "./dna";
import { NeighbourhoodStorage } from "./neighbourhoodStorage";
import type { IPFS } from "ipfs-core-types"

class NeighbourhoodPutAdapter implements PublicSharing {
  #agent: AgentService
  #DNA: HolochainLanguageDelegate;
  #IPFS: IPFS;

  constructor(context: LanguageContext) {
      this.#agent = context.agent
      this.#DNA = context.Holochain as HolochainLanguageDelegate;
      // @ts-ignore
      this.#IPFS = context.IPFS;
  }

  async createPublic(neighbourhood: object): Promise<Address> {
    const ipfsAddress = await this.#IPFS.add(
      { content: JSON.stringify(neighbourhood)},
      { onlyHash: true},
    );
    // @ts-ignore
    const hash = ipfsAddress.cid.toString();

    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "neighbourhood_storage", fn_name, payload));

    const expression: any = this.#agent.createSignedExpression(neighbourhood)

    //Store the FileMetadataExpression
    await storage.storeNeighbourhoodExpression({
      neighbourhood: expression,
      address: hash
    })

    //@ts-ignore
    return hash
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
        //Check the first two characters of address are equal to Qm
    if (address.substring(0, 2) != "Qm") {
      console.error("LanguageLanguage.get(): The address is not a valid hash");
      return null;
    }

    const storage = new NeighbourhoodStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "neighbourhood_storage", fn_name, payload));

    const expression = (await storage.getNeighbourhoodExpression(address)) as NeighbourhoodExpression
    if (!expression) {
      return null;
    };

    return expression
  }
}
