// Local agent-language — stores agent expressions locally.
//
// Uses ad4m:host storageGet/storagePut for persistence. Each agent
// expression holds a DID, public perspective links, and optional
// direct-message language reference.
import {
    agentDid,
    agentCreateSignedExpression,
    storageGet,
    storagePut,
} from "ad4m:host";

export const name = "local-agent-store";
export const version = "0.1.0";

export async function init() {}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(content) {
    if (!content || !content.did || !content.perspective) {
        throw new Error("Agent expression requires did and perspective fields");
    }

    const myDid = agentDid();
    if (content.did !== myDid) {
        throw new Error("Can only create agent expressions for own DID");
    }

    // Strip proof.valid/invalid from perspective links (match production behaviour)
    if (content.perspective && content.perspective.links) {
        content.perspective.links = content.perspective.links.map(function(link) {
            if (link.proof) {
                delete link.proof.valid;
                delete link.proof.invalid;
            }
            return link;
        });
    }

    const expression = agentCreateSignedExpression(content);
    storagePut("agent-" + content.did, JSON.stringify(expression));
    return content.did;
}

export async function expressionGet(did) {
    try {
        const raw = storageGet("agent-" + did);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
