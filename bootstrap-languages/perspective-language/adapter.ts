import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { IpfsPutAdapter } from './putAdapter.ts'
// import { toString as uint8ArrayToString } from 'uint8arrays/to-string'
// import { concat as uint8ArrayConcat } from 'uint8arrays/concat';

export default class Adapter implements ExpressionAdapter {
    putAdapter: PublicSharing

    constructor(context: LanguageContext) {
        this.putAdapter = new IpfsPutAdapter(context)
    }

    async get(address: Address): Promise<Expression> {
        console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
        // const cid = address.toString()

        // const chunks = []
        // // @ts-ignore
        // for await (const chunk of this.#IPFS.cat(cid)) {
        //     chunks.push(chunk)
        // }

        // const fileString = uint8ArrayToString(uint8ArrayConcat(chunks));
        // const fileJson = JSON.parse(fileString)
        // //pin file to help persistence
        // await this.#IPFS.pin.add(cid);
        return null
    }
}