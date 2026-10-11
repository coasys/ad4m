/**
 * # Centralized Agent Expression Store
 *
 * Expression language that stores agent expressions via a centralized
 * server (mock.ad4m.dev).
 */

import axiod from "https://deno.land/x/axiod/mod.ts";
import {
    defineLanguage,
    agentDid,
    agentCreateSignedExpression,
} from "@coasys/ad4m-ldk";

// Central agent-expression server. Keep in sync with the Rust port in
// rust-impl/src/lib.rs.
const SERVER_URL = "https://mock.ad4m.dev";

const language = defineLanguage({
    name: "centralized-agent-expression-store",
    version: "0.1.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        async create(content: any): Promise<string> {
            if (!content["did"] || !content["perspective"] || !content["perspective"].links)
                throw "Content must be an Agent object";

            const agentObj = content;
            if (agentObj.did != agentDid())
                throw "Can't set Agent Expression for foreign DID - only for self";

            if (!agentObj.directMessageLanguage) agentObj.directMessageLanguage = undefined;

            agentObj.perspective!.links.forEach((link: any) => {
                delete link.proof.valid;
                delete link.proof.invalid;
            });

            const expression = agentCreateSignedExpression(agentObj);

            await axiod.post(`${SERVER_URL}/agent`, {
                data: {
                    did: agentObj.did,
                    expression,
                },
            });

            return agentObj.did;
        },

        async get(did: string): Promise<any> {
            console.log("Getting expression with did", did);
            const data = await axiod.get(`${SERVER_URL}/agent`, {
                params: { did },
            });
            return data.data.expression;
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
