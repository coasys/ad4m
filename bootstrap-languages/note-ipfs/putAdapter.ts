import type { Address, AgentService, PublicSharing, LanguageContext } from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types";

export class IpfsPutAdapter implements PublicSharing {
    #agent: AgentService
    #IPFS: IPFS

    constructor(context: LanguageContext) {
        this.#agent = context.agent
        this.#IPFS = context.IPFS
    }

    async createPublic(note: object): Promise<Address> {
        try {
            //@ts-ignore
            note = JSON.parse(note)
        }catch(e){

        }

        const agent = this.#agent
        const expression = agent.createSignedExpression(note)
        const content = JSON.stringify(expression)
        const result = await this.#IPFS.add({content})
        // @ts-ignore
        return result.cid.toString() as Address
    }
}