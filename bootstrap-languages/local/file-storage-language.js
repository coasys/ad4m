// Local file-storage language — stores file expressions locally.
//
// Replaces the centralized Cloudflare Workers KV file store for
// local/standalone deployments. Uses ad4m:host storagePut/storageGet
// for persistence within the executor's per-language storage directory.
import {
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "ad4m:host";

export const name = "local-file-store";
export const version = "0.1.0";

export async function init() {}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(fileData) {
    if (typeof fileData === "string") {
        try { fileData = JSON.parse(fileData); } catch (_) {}
    }

    const fileMetadata = {
        name: fileData.name,
        size: fileData.data_base64 ? Math.ceil(fileData.data_base64.length * 3 / 4) : 0,
        file_type: fileData.file_type,
        data_base64: fileData.data_base64,
    };

    const address = hash(JSON.stringify(fileMetadata));
    const expression = agentCreateSignedExpression(fileMetadata);
    storagePut("file-" + address, JSON.stringify(expression));
    return address;
}

export async function expressionGet(address) {
    try {
        const raw = storageGet("file-" + address);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
