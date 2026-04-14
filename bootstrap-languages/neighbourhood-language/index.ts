/**
 * # Neighbourhood Store
 *
 * Expression language that stores neighbourhood metadata via a centralized
 * proxy (Cloudflare Workers KV).
 */

import axiod from "https://deno.land/x/axiod/mod.ts";
import { defineLanguage, agentCreateSignedExpression } from "@coasys/ad4m-ldk";

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev/";

const language = defineLanguage({
    name: "neighbourhood-store",
    version: "0.1.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        async create(neighbourhood: object): Promise<string> {
            // @ts-ignore — UTILS is injected by the runtime
            const hash = UTILS.hash(JSON.stringify(neighbourhood));
            const expression = agentCreateSignedExpression(neighbourhood);

            const neighbourhoodPostData = {
                key: hash,
                value: JSON.stringify(expression),
            };
            const neighbourhoodPostResult = await axiod.post(PROXY_URL, neighbourhoodPostData);
            if (neighbourhoodPostResult.status != 200) {
                console.error("Upload neighbourhood data gets error: ", neighbourhoodPostResult);
            }

            return hash;
        },

        async get(address: string): Promise<any> {
            const cid = address.toString();

            let presignedUrl;
            try {
                const getPresignedUrl = await axiod.get(PROXY_URL + `?key=${cid}`);
                presignedUrl = getPresignedUrl.data.url;
            } catch (e) {
                console.error("Get neighbourhood failed at getting presigned url", e);
                return null;
            }

            let neighbourhoodObject;
            try {
                const getNeighbourhoodObject = await axiod.get(presignedUrl);
                neighbourhoodObject = getNeighbourhoodObject.data;
            } catch (e) {
                console.error("Get meta information failed at getting meta information", e);
                return null;
            }

            return neighbourhoodObject;
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
