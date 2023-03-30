import type { Address, AgentService, PublicSharing, LanguageContext, Perspective } from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types";

export class IpfsPutAdapter implements PublicSharing {
    #agent: AgentService
    #IPFS: IPFS

    constructor(context: LanguageContext) {
        this.#agent = context.agent
        this.#IPFS = context.IPFS
    }

    async createPublic(perspective: object): Promise<Address> {
        try {
            //@ts-ignore
            perspective = JSON.parse(perspective)
        }catch(e){
            
        }
        if (typeof perspective === "object" && perspective.hasOwnProperty('links')) {
            const P = perspective as Perspective
            if (typeof P.links !== "object") {
                throw new Error('invalid link property type')
            }
        }
        else {
            throw new Error('invalid object type')
        }

        const agent = this.#agent
        const expression = agent.createSignedExpression(perspective)
        const content = JSON.stringify(expression)
        const result = await this.#IPFS.add({content})
        // @ts-ignore
        return result.cid.toString() as Address
    }
}