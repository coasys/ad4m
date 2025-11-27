import type { Address, AgentService, PublicSharing, LanguageContext, Perspective } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";

export class IpfsPutAdapter implements PublicSharing {
    #agent: AgentService

    constructor(context: LanguageContext) {
        this.#agent = context.agent
    }

    async createPublic(perspective: object): Promise<Address> {
        console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
        // try {
        //     //@ts-ignore
        //     perspective = JSON.parse(perspective)
        // }catch(e){
            
        // }
        // if (typeof perspective === "object" && perspective.hasOwnProperty('links')) {
        //     const P = perspective as Perspective
        //     if (typeof P.links !== "object") {
        //         throw new Error('invalid link property type')
        //     }
        // }
        // else {
        //     throw new Error('invalid object type')
        // }

        // const agent = this.#agent
        // const expression = agent.createSignedExpression(perspective)
        // const content = JSON.stringify(expression)
        // const result = await this.#IPFS.add({content})
        // @ts-ignore
        return "" as Address
    }
}