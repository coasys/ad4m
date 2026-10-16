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

    const address = fileAddress(fileMetadata);
    const expression = agentCreateSignedExpression(fileMetadata);
    return { address, expression };
}

// The address of a file: the hash of its metadata, in this key order.
// Signing sorts the keys of `expression.data`, so a fetched expression is
// checked by rebuilding the object, never by hashing `data` as it is.
function fileAddress(data: any): string {
    return hash(JSON.stringify({
        name: data.name,
        size: data.size,
        file_type: data.file_type,
        data_base64: data.data_base64,
    }));
}

// Whether `expression` holds the file `address` names. Every address here
// is immutable, so the runtime caches what `get()` returns forever: an
// expression stored under someone else's address must read as missing.
function holdsFile(expression: any, address: string): boolean {
    const data = expression?.data;
    return !!data && typeof data === "object" && fileAddress(data) === address;
}

// What the gateway stores under `address`, unchecked; null if nothing.
async function fetchStored(address: string): Promise<any> {
    const cid = address.toString();

    // `inline=1` asks the gateway for the expression itself, one round
    // trip. A gateway that ignores it answers with a pre-signed URL
    // instead, and the object is a second request.
    let presignedUrl;
    try {
        const response = await request(PROXY_URL + `?key=${cid}&inline=1`);
        if (!response.ok) return null;
        const body = await response.json();
        if (typeof body?.url !== "string") return body;
        presignedUrl = body.url;
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
}

async function publish(address: string, expression: any): Promise<void> {
    const response = await request(PROXY_URL, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ key: address, value: JSON.stringify(expression) }),
    });
    if (response.ok) return;

    const body = await response.text();
    if (response.status === 400 && body === "Key already exists") {
        // The key is the file's hash, so an expression there that holds
        // this file is this upload, whoever signed it. Anything else means
        // someone else took the key first: the file is not published, and
        // the runtime keeps the publish queued.
        const stored = await fetchStored(address);
        if (holdsFile(stored, address)) return;
        throw new Error(stored === null
            ? `Upload of ${address} failed: the key exists but could not be read`
            : `Upload of ${address} failed: the key holds a different file`);
    }
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
            const expression = await fetchStored(address);
            if (expression === null) return null;
            if (!holdsFile(expression, address)) {
                console.error(`Expression stored under ${address} does not hold that file`);
                return null;
            }
            return expression;
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
