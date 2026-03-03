import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, HolochainLanguageDelegate } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { FileStoragePutAdapter } from './putAdapter.ts'
import pako from "https://esm.sh/v135/pako@2.1.0";
import { FileStorage } from "./file-storage.ts";
import { DNA_NICK } from "./build/dna.js";
import type { FileExpression } from "./types.ts";

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