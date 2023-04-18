import type { Address, AgentService, PublicSharing, LanguageContext, HolochainLanguageDelegate } from "https://esm.sh/@perspect3vism/ad4m@0.3.4";
import pako from "https://esm.sh/pako@2.1.0";
import { FileStorage } from "./file-storage.ts";
import { DNA_NICK } from "./build/dna.js";
//import { Blob } from "https://esm.sh/buffer@6.0.3";
import type { FileExpression, FileMetadata } from "./types.ts";

export interface FileData {
    name: string;
    file_type: string;
    data_base64: string;
}

export class FileStoragePutAdapter implements PublicSharing {
    #agent: AgentService
    #DNA: HolochainLanguageDelegate;

    constructor(context: LanguageContext) {
        this.#agent = context.agent
        this.#DNA = context.Holochain as HolochainLanguageDelegate;
    }

    async createPublic(fileData: FileData): Promise<Address> {
        console.log("createPublic fileData", fileData)
        try {
            // Just in case...
            if(typeof fileData === "string"){
                //@ts-ignore
                fileData = JSON.parse(fileData)
            }
        }catch(e){}

        const storage = new FileStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "file_storage", fn_name, payload));

        const data_uncompressed = Uint8Array.from(Buffer.from(fileData.data_base64, "base64"));
        const data_compressed = pako.deflate(data_uncompressed)
        const blob = new Blob([data_compressed])

        const hashes = await storage.upload(blob);
        
        const fileMetadata = {
            name: fileData.name,
            size: data_uncompressed.length,
            file_type: fileData.file_type,
            checksum: "1234",
            chunks_hashes: hashes,
            data_base64: fileData.data_base64
        } as FileMetadata

        //Create the signed expression object
        const expression: FileExpression = this.#agent.createSignedExpression(fileMetadata)
        //Remove the data_base64 from the expression, since this is already stored above
        delete expression.data.data_base64;

        //Store the FileMetadataExpression
        const address = await storage.storeFileExpression(expression)
        if (!Buffer.isBuffer(address)) {
            throw new Error("Could not create FileExpression data")
        };
        //@ts-ignore
        return address.toString("hex")
    }
}