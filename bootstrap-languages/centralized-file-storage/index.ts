/**
 * # Centralized File Store — Flat Export Language
 *
 * Expression language that stores files via a centralized proxy
 * (Cloudflare Workers KV). Flat-export migration of the legacy
 * create()-factory version.
 */

import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "centralized-file-store";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev/";

let agent: any = null;

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
}

export async function teardown(): Promise<void> {
    agent = null;
}

export function interactions(): any[] {
    return [];
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
    try {
        if (typeof fileData === "string") {
            fileData = JSON.parse(fileData);
        }
    } catch (_e) {}

    const data_uncompressed = Uint8Array.from(Buffer.from(fileData.data_base64, "base64"));

    const fileMetadata = {
        name: fileData.name,
        size: data_uncompressed.length,
        file_type: fileData.file_type,
        data_base64: fileData.data_base64,
    };

    // @ts-ignore — UTILS is injected by the runtime
    const hash = UTILS.hash(JSON.stringify(fileMetadata));
    const expression = agent.createSignedExpression(fileMetadata);

    const postData = {
        key: hash,
        value: JSON.stringify(expression),
    };
    try {
        const postResult = await axiod.post(PROXY_URL, postData);
        if (postResult.status != 200) {
            console.error("Upload file data gets error: ", postResult);
        }
    } catch (e: any) {
        if (e?.response?.status === 400 && e?.response?.data === "Key already exists") {
            console.log("File already exists at key:", hash, "— reusing existing upload");
        } else {
            throw e;
        }
    }

    return hash;
}

export async function expressionGet(address: string): Promise<any> {
    const cid = address.toString();

    let presignedUrl;
    try {
        const getPresignedUrl = await axiod.get(PROXY_URL + `?key=${cid}`);
        presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
        console.error("Get File failed at getting presigned url", e);
        return null;
    }

    let object;
    try {
        const getObject = await axiod.get(presignedUrl);
        object = getObject.data;
    } catch (e) {
        console.error("Get meta information failed at getting meta information", e);
        return null;
    }

    return object;
}
