import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, HolochainLanguageDelegate } from "@perspect3vism/ad4m";
import { FileStoragePutAdapter } from './putAdapter'
import pako from "pako";
import { FileStorage } from "./file-storage";
import { DNA_NICK } from "./dna";
import type { FileExpression } from "./types";

export default class Adapter implements ExpressionAdapter {
    putAdapter: PublicSharing
    #DNA: HolochainLanguageDelegate;

    constructor(context: LanguageContext) {
        this.putAdapter = new FileStoragePutAdapter(context)
        this.#DNA = context.Holochain as HolochainLanguageDelegate;
    }

    async get(address: Address): Promise<Expression> {
        const storage = new FileStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "file_storage", fn_name, payload));

        let addressBuffer = Buffer.from(address, "hex");
        const expression = (await storage.getFileExpression(addressBuffer)) as FileExpression
        if (!expression) {
            return null;
        };
        if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
            expression.data.data_base64 = "";
            return expression;
        };
        const data_compressed = await storage.download(expression.data.chunks_hashes);
        let data_stream = await data_compressed.arrayBuffer();

        const data_uncompressed = pako.inflate(data_stream);
        const buffer = Buffer.from(data_uncompressed)

        expression.data.data_base64 = buffer.toString("base64")

        return expression
    }
}