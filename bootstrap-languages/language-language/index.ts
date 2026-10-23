/**
 * # Language Language
 *
 * Expression language that stores language metadata and bundles via a
 * centralized proxy (Cloudflare Workers KV). Also implements the
 * LanguageSource capability (languageGetSource).
 */

import { defineLanguage, agentCreateSignedExpression, hash } from '@coasys/ad4m-ldk';
import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Constants
// =============================================================================

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev";

// A language's address is the hash of its bundle and its meta is published once,
// so the meta under an address never changes: each is fetched at most once.
const metaCache = new Map<string, any>();

// Store `value` under `key`. A key that already exists holds the same content
// (the key is the content's hash), so that counts as stored. Any other failure
// throws.
async function store(key: string, value: string): Promise<void> {
    try {
        await axiod.post(PROXY_URL, { key, value });
    } catch (e: any) {
        const data = e?.response?.data;
        if (e?.response?.status == 400 && typeof data === "string" && data.includes("Key already exists")) return;
        console.error(`[Cloudflare-based Language Language]: storing ${key} failed:`, data ?? e);
        throw e;
    }
}

// =============================================================================
// Language definition
// =============================================================================

const lang = defineLanguage({
    name: "languages",
    version: "0.1.0",

    async init() {},

    teardown() {},

    interactions(_address: string) {
        return [];
    },

    expression: {
        async create(language: any): Promise<string> {
            const computed = hash(language.bundle.toString());

            if (computed != language.meta.address)
                throw new Error(
                    `Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${computed}`
                );

            const expression = agentCreateSignedExpression(language.meta);

            // The bundle first, so a stored meta never names a missing bundle.
            // A publish that stores nothing must fail here: the store is
            // consistent, so a reader that misses the language later will not
            // try again.
            await store(computed, language.bundle.toString());
            await store(`meta-${computed}`, JSON.stringify(expression));
            return computed;
        },

        async get(address: string): Promise<any> {
            if (address.substring(0, 2) != "Qm") {
                console.error("LanguageLanguage.get(): The address is not a valid hash");
                return null;
            }
            if (metaCache.has(address)) return metaCache.get(address);
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

            // Not published: not cached, so a later read after a publish finds it.
            if (metaObject) metaCache.set(address, metaObject);
            return metaObject;
        },
    },

    languageSource: {
        async getSource(address: string): Promise<string> {
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
        },
    },
});

export default lang;
export const {
    name, version, init, teardown, interactions,
    expressionCreate, expressionGet,
    languageGetSource,
} = lang;
