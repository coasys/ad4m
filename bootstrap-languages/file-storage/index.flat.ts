/**
 * # File Storage — Flat Export Language
 *
 * Expression language that stores files via Holochain DNA with
 * chunked upload/download and pako compression.
 * Flat-export migration of the legacy create()-factory version.
 */

import pako from "https://esm.sh/v135/pako@2.1.0";
import { FileStorage } from "./file-storage.ts";
import { DNA, DNA_NICK } from "./build/dna.js";
import type { FileExpression, FileMetadata } from "./types.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "file-storage";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

let hc: any = null;
let agent: any = null;

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
    hc = (globalThis as any).__holochainDelegate__;

    await hc.registerDNAs([{ file: DNA, nick: DNA_NICK }]);
}

export async function teardown(): Promise<void> {
    hc = null;
    agent = null;
}

export function interactions(): any[] {
    return [];
}

export function isImmutableExpression(_address: string): boolean {
    return true;
}

// =============================================================================
// Expression capability
// =============================================================================

export interface FileData {
    name: string;
    file_type: string;
    data_base64: string;
}

export async function expressionCreate(fileData: any): Promise<string> {
    console.log("createPublic fileData", fileData);
    try {
        if (typeof fileData === "string") {
            fileData = JSON.parse(fileData);
        }
    } catch (_e) {}

    const storage = new FileStorage((fn_name, payload) =>
        hc.call(DNA_NICK, "file_storage", fn_name, payload)
    );

    const data_uncompressed = Uint8Array.from(Buffer.from(fileData.data_base64, "base64"));
    const data_compressed = pako.deflate(data_uncompressed);
    const blob = new Blob([data_compressed]);

    const hashes = await storage.upload(blob);

    const fileMetadata = {
        name: fileData.name,
        size: data_uncompressed.length,
        file_type: fileData.file_type,
        checksum: "1234",
        chunks_hashes: hashes,
        data_base64: fileData.data_base64,
    } as FileMetadata;

    const expression: FileExpression = agent.createSignedExpression(fileMetadata);
    delete expression.data.data_base64;
    expression.data.chunks_hashes = expression.data.chunks_hashes.map(
        (chunk: { [key: string]: number }) => {
            const byteArray = new Uint8Array(Object.keys(chunk).length);
            for (const key in chunk) {
                const index = parseInt(key, 10);
                byteArray[index] = chunk[key];
            }
            return byteArray;
        }
    );

    let address = await storage.storeFileExpression(expression);
    if (!Buffer.isBuffer(address)) {
        address = Buffer.from(address);
    }
    //@ts-ignore
    return address.toString("hex");
}

export async function expressionGet(address: string): Promise<any> {
    const storage = new FileStorage((fn_name, payload) =>
        hc.call(DNA_NICK, "file_storage", fn_name, payload)
    );

    let addressBuffer = Buffer.from(address, "hex");
    const expression = (await storage.getFileExpression(addressBuffer)) as FileExpression;
    if (!expression) {
        return null;
    }
    if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
        expression.data.data_base64 = "";
        return expression;
    }
    const data_compressed = await storage.download(expression.data.chunks_hashes);
    let data_stream = await data_compressed.arrayBuffer();

    const data_uncompressed = pako.inflate(data_stream);
    const buffer = Buffer.from(data_uncompressed);

    expression.data.data_base64 = buffer.toString("base64");

    return expression;
}
