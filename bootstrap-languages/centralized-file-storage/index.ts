/**
 * # Centralized File Store
 *
 * Expression language that stores files via a centralized proxy
 * (Cloudflare Workers KV).
 */

import axiod from "https://deno.land/x/axiod/mod.ts";
import { defineLanguage, agentCreateSignedExpression, hash } from "@coasys/ad4m-ldk";

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev/";

export interface FileData {
    name: string;
    file_type: string;
    data_base64: string;
}

const language = defineLanguage({
    name: "centralized-file-store",
    version: "0.1.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        async create(fileData: any): Promise<string> {
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

            const postData = {
                key: address,
                value: JSON.stringify(expression),
            };
            try {
                const postResult = await axiod.post(PROXY_URL, postData);
                if (postResult.status != 200) {
                    console.error("Upload file data gets error: ", postResult);
                }
            } catch (e: any) {
                if (e?.response?.status === 400 && e?.response?.data === "Key already exists") {
                    console.log("File already exists at key:", address, "— reusing existing upload");
                } else {
                    throw e;
                }
            }

            return address;
        },

        async get(address: string): Promise<any> {
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
} = language;
