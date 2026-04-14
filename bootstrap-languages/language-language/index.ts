/**
 * # Language Language — Flat Export Language
 *
 * Expression language that stores language metadata and bundles via a
 * centralized proxy (Cloudflare Workers KV). Also implements the
 * languageAdapter (getLanguageSource) capability.
 * Flat-export migration of the legacy create()-factory version.
 */

import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "languages";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev";

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

export async function expressionCreate(language: any): Promise<string> {
    // @ts-ignore — UTILS is injected by the runtime
    const hash = UTILS.hash(language.bundle.toString());

    if (hash != language.meta.address)
        throw new Error(
            `Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${hash}`
        );

    const expression = agent.createSignedExpression(language.meta);

    const key = `meta-${hash}`;
    const metaPostData = {
        key: key,
        value: JSON.stringify(expression),
    };
    try {
        const metaPostResult = await axiod.post(PROXY_URL, metaPostData);
        if (metaPostResult.status != 200) {
            console.error("Upload language meta data gets error: ", metaPostResult);
        }

        const languageBundleBucketParams = {
            key: hash,
            value: language.bundle.toString(),
        };
        const bundlePostResult = await axiod.post(PROXY_URL, languageBundleBucketParams);
        if (bundlePostResult.status != 200) {
            console.error("Upload language bundle data gets error: ", bundlePostResult);
        }

        return hash;
    } catch (e: any) {
        if (e.response.status == 400 && e.response.data.includes("Key already exists")) {
            console.log("[Cloudflare-based Language Language]: Tried to replace existing language. Ignoring...");
            return hash;
        }
        console.error("[Cloudflare-based Language Language]: Error storing Language: ", e.response.data);
        throw e;
    }
}

export async function expressionGet(address: string): Promise<any> {
    if (address.substring(0, 2) != "Qm") {
        console.error("LanguageLanguage.get(): The address is not a valid hash");
        return null;
    }
    const metaDataKey = `meta-${address}`;

    let presignedUrl;
    try {
        const getPresignedUrl = await axiod.get(PROXY_URL + `?key=${metaDataKey}`);
        presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
        console.error("Get meta information failed at getting presigned url", address);
        return null;
    }

    let metaObject;
    try {
        const getMetaObject = await axiod.get(presignedUrl);
        metaObject = getMetaObject.data;
    } catch (e) {
        console.error("Get meta information failed at getting meta information", presignedUrl);
        return null;
    }

    return metaObject;
}

// =============================================================================
// Language adapter capability (getLanguageSource)
// =============================================================================

export async function languageGetSource(address: string): Promise<string> {
    if (address.substring(0, 2) != "Qm") {
        console.error("LanguageLanguage.getLanguageSource(): The address is not a valid hash");
        return "";
    }
    const cid = address.toString();

    let presignedUrl;
    try {
        const getPresignedUrl = await axiod.get(PROXY_URL + `?key=${cid}`);
        presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
        console.error("Get language source failed at getting presigned url", address);
        throw e;
    }

    let languageSource;
    try {
        const getLanguageSource = await axiod.get(presignedUrl);
        languageSource = getLanguageSource.data;
    } catch (e) {
        console.error("Get language source failed at getting language source", address);
        throw e;
    }

    return languageSource;
}
