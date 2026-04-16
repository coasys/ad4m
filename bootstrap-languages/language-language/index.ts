/**
 * # Language Language
 *
 * Expression language that stores language metadata and bundles via a
 * centralized proxy (Cloudflare Workers KV). Also implements the
 * LanguageSource capability (languageGetSource).
 */

import { defineLanguage, agentCreateSignedExpression } from '@coasys/ad4m-ldk';
import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Constants
// =============================================================================

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev";

// =============================================================================
// Language definition
// =============================================================================

// @ts-ignore -- UTILS is injected by the runtime
const UTILS = () => (globalThis as any).UTILS;

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
            const hash = UTILS().hash(language.bundle.toString());

            if (hash != language.meta.address)
                throw new Error(
                    `Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${hash}`
                );

            const expression = agentCreateSignedExpression(language.meta);

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
        },

        async get(address: string): Promise<any> {
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
