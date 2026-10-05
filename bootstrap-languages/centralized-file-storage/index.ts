/**
 * # Centralized File Store
 *
 * Expression language that stores files via a centralized proxy
 * (Cloudflare Workers KV).
 *
 * Addresses are content hashes, so every expression is immutable: the
 * runtime caches what it reads and, through `prepare` / `publish`, what it
 * writes, and queues publishes that fail so writes succeed offline.
 */

import { defineLanguage, agentCreateSignedExpression, hash } from "@coasys/ad4m-ldk";

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev/";

// Without a timeout a stalled connection holds the language's one request
// slot, and every other read and write queues behind it.
const REQUEST_TIMEOUT_MS = 20_000;

export interface FileData {
    name: string;
    file_type: string;
    data_base64: string;
}

async function request(url: string, init: RequestInit = {}): Promise<Response> {
    return await fetch(url, { ...init, signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS) });
}

function prepare(fileData: any): { address: string; expression: any } {
    try {
        if (typeof fileData === "string") {
            fileData = JSON.parse(fileData);
        }
    } catch (_e) {}

    const data_uncompressed = Uint8Array.from(
        Buffer.from(fileData.data_base64, "base64")
    );

    const fileMetadata = {
        name: fileData.name,
        size: data_uncompressed.length,
        file_type: fileData.file_type,
        data_base64: fileData.data_base64,
    };

    const address = hash(JSON.stringify(fileMetadata));
    const expression = agentCreateSignedExpression(fileMetadata);
    return { address, expression };
}

async function publish(address: string, expression: any): Promise<void> {
    const response = await request(PROXY_URL, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ key: address, value: JSON.stringify(expression) }),
    });
    if (response.ok) return;

    const body = await response.text();
    if (response.status === 400 && body === "Key already exists") return;
    throw new Error(`Upload of ${address} failed: ${response.status} ${body}`);
}

const language = defineLanguage({
    name: "centralized-file-store",
    version: "0.2.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        isImmutable(_address: string): boolean {
            return true;
        },

        async prepare(fileData: any) {
            return prepare(fileData);
        },

        async publish(address: string, expression: any): Promise<void> {
            await publish(address, expression);
        },

        async create(fileData: any): Promise<string> {
            const { address, expression } = prepare(fileData);
            await publish(address, expression);
            return address;
        },

        async get(address: string): Promise<any> {
            const cid = address.toString();

            let presignedUrl;
            try {
                const response = await request(PROXY_URL + `?key=${cid}`);
                if (!response.ok) return null;
                presignedUrl = (await response.json()).url;
            } catch (e) {
                console.error("Get File failed at getting presigned url", e);
                return null;
            }

            try {
                const response = await request(presignedUrl);
                if (!response.ok) return null;
                return await response.json();
            } catch (e) {
                console.error("Get meta information failed at getting meta information", e);
                return null;
            }
        },
    },
});

export const {
    name,
    version,
    init,
    teardown,
    interactions,
    expressionGet,
    expressionCreate,
    isImmutableExpression,
    expressionPrepare,
    expressionPublish,
} = language;
