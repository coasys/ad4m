import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext } from "@perspect3vism/ad4m";
import { IpfsPutAdapter } from './putAdapter'
import { toString as uint8ArrayToString } from 'uint8arrays/to-string'
import { concat as uint8ArrayConcat } from 'uint8arrays/concat';
import type { IPFS } from "ipfs-core-types";

export default class Adapter implements ExpressionAdapter {
    #IPFS: IPFS

    putAdapter: PublicSharing

    constructor(context: LanguageContext) {
        this.#IPFS = context.IPFS
        this.putAdapter = new IpfsPutAdapter(context)
    }

    async get(address: Address): Promise<Expression> {
        const cid = address.toString()

        const chunks = []
        // @ts-ignore
        for await (const chunk of this.#IPFS.cat(cid)) {
            chunks.push(chunk)
        }

        const fileString = uint8ArrayToString(uint8ArrayConcat(chunks));
        const fileJson = JSON.parse(fileString)
        //pin file to help persistence
        await this.#IPFS.pin.add(cid);
        return fileJson
    }
}