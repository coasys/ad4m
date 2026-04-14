/**
 * # Agent Expression Store
 *
 * Expression language that stores agent expressions via Holochain DNA.
 */

import {
    defineLanguage,
    agentDid,
    agentCreateSignedExpression,
    holochainRegisterDnas,
    holochainCall,
} from "@coasys/ad4m-ldk";
import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

const language = defineLanguage({
    name: "agent-expression-store",
    version: "0.1.0",

    async init() {
        const dnaBundle = Buffer.from(BUNDLE, "base64");
        await holochainRegisterDnas([
            {
                file: dnaBundle,
                nick: DNA_ROLE,
                zomeCalls: [
                    [ZOME_NAME, "create_agent_expression"],
                    [ZOME_NAME, "get_agent_expression"],
                ],
            } as any,
        ]);
    },

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
                delete link.status;
            });

            const expression = agentCreateSignedExpression(agentObj);
            await holochainCall(DNA_ROLE, ZOME_NAME, "create_agent_expression", expression);
            return agentObj.did;
        },

        async get(did: string): Promise<any> {
            console.log("Getting expression with did", did);
            return await holochainCall(DNA_ROLE, ZOME_NAME, "get_agent_expression", did);
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
