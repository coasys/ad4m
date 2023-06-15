import type { Address, AgentService, PublicSharing, LanguageContext, HolochainLanguageDelegate, LanguageLanguageInput, LanguageMeta, LanguageMetaInternal, LanguageExpression } from "@perspect3vism/ad4m";
import pako from "pako";
import { LanguageStorage } from "./languageStorage";
import { DNA_NICK } from "./dna";
import { Blob } from "buffer";
import type { LanguageMetadata } from "./types";
import type { IPFS } from "ipfs-core-types"

export type EntryHash = Uint8Array;

export interface LanguageData {
    name: string;
    file_type: string;
    data_base64: string;
}


export class LanguageStoragePutAdapter implements PublicSharing {
    #agent: AgentService
    #DNA: HolochainLanguageDelegate;
    #IPFS: IPFS;

    constructor(context: LanguageContext) {
        this.#agent = context.agent;
        this.#DNA = context.Holochain as HolochainLanguageDelegate;
        // @ts-ignore
        this.#IPFS = context.IPFS;
    }

    async createPublic(language: LanguageLanguageInput): Promise<Address> {
        const ipfsAddress = await this.#IPFS.add(
            { content: language.bundle.toString()},
            { onlyHash: true},
        );
        // @ts-ignore
        const hash = ipfsAddress.cid.toString();

        if(hash != language.meta.address) {
            throw new Error(`Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${hash}`)
        }

        //console.log("createPublic fileData", language)
        try {
            // Just in case...
            if(typeof language === "string"){
                //@ts-ignore
                fileData = JSON.parse(fileData)
            }
        }catch(e){}

        const storage = new LanguageStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language_storage", fn_name, payload));

        const data_uncompressed = Uint8Array.from(Buffer.from(language.bundle.toString()));
        const data_compressed = pako.deflate(data_uncompressed)
        const blob = new Blob([data_compressed])

        const hashes = await storage.upload(blob);

        const fileMetadata = {
            name: language.meta.name,
            description: language.meta.description,
            address: language.meta.address,
            chunks_hashes: hashes,
            size: data_uncompressed.length,
        } as LanguageMetadata

        //Create the signed expression object
        const expression: LanguageExpression = this.#agent.createSignedExpression(fileMetadata)
        //Remove the data_base64 from the expression, since this is already stored above
        delete expression.data.data_base64;

        //Store the FileMetadataExpression
        await storage.storeLanguageExpression(expression)

        //@ts-ignore
        return hash
    }
}