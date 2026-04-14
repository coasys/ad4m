/**
 * # Direct Message Language
 *
 * DM language backed by Holochain DNA. Implements the direct-message
 * capability (directMessage* flat exports) which is a runtime-retained
 * feature not in the v1.0 capability spec — so those exports sit
 * alongside the `defineLanguage()` ones.
 */

import {
    defineLanguage,
    agentDid,
    agentCreateSignedExpression,
    holochainRegisterDnas,
    holochainCall,
} from "@coasys/ad4m-ldk";
import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

//!@ad4m-template-variable
const recipient_did = "<not templated yet>";

let messageCallbacks: ((msg: any) => Promise<void>)[] = [];

async function handleSignal(signal: any): Promise<void> {
    console.debug("DM Language got HC signal:", signal);
    let payload = signal.payload;
    try {
        const str = signal.payload.toString();
        const cropped = str.substring(str.indexOf("{"));
        payload = JSON.parse(cropped);
    } catch (e) {
        console.error(e);
    }
    for (const cb of messageCallbacks) {
        await cb(payload);
    }
}

function onlyRecipient(): void {
    const did = agentDid();
    console.log(recipient_did, did);
    if (recipient_did !== did) throw new Error("Only recipient can call this function!");
}

const language = defineLanguage({
    name: "direct-message-language",
    version: "0.1.0",

    async init() {
        const dnaBundle = Buffer.from(BUNDLE, "base64");
        await holochainRegisterDnas([
            {
                file: dnaBundle,
                nick: DNA_ROLE,
                zomeCalls: [
                    [ZOME_NAME, "send_p2p"],
                    [ZOME_NAME, "send_inbox"],
                    [ZOME_NAME, "set_status"],
                    [ZOME_NAME, "get_status"],
                    [ZOME_NAME, "fetch_inbox"],
                    [ZOME_NAME, "inbox"],
                ],
            } as any,
        ]);
    },

    async teardown() {
        messageCallbacks = [];
    },

    interactions() { return []; },

    handleHolochainSignal: handleSignal,
});

export const {
    name,
    version,
    init,
    teardown,
    interactions,
    handleHolochainSignal,
} = language;

// =============================================================================
// Direct Message capability — runtime-retained flat exports, not in the v1 spec
// =============================================================================

export function directMessageRecipient(): string {
    return recipient_did;
}

export async function directMessageStatus(): Promise<any> {
    let status = null;
    try {
        status = await holochainCall(DNA_ROLE, ZOME_NAME, "get_status", null);
    } catch (e) {
        console.debug("DirectMessage Language couldn't get status:", e);
    }
    return status;
}

export async function directMessageSendP2P(message: any): Promise<any> {
    try {
        const messageExpression = agentCreateSignedExpression(message);
        await holochainCall(DNA_ROLE, ZOME_NAME, "send_p2p", messageExpression);
        return messageExpression;
    } catch (e) {
        console.error("Direct Message Language: Error sending p2p to", recipient_did);
    }
}

export async function directMessageSendInbox(message: any): Promise<any> {
    try {
        const messageExpression = agentCreateSignedExpression(message);
        await holochainCall(DNA_ROLE, ZOME_NAME, "send_inbox", messageExpression);
        return messageExpression;
    } catch (e) {
        console.error("Direct Message Language: Error sending to inbox of", recipient_did);
    }
}

export async function directMessageSetStatus(status: any): Promise<void> {
    onlyRecipient();
    const statusExpression = agentCreateSignedExpression(status);
    await holochainCall(DNA_ROLE, ZOME_NAME, "set_status", statusExpression);
}

export async function directMessageInbox(filter?: string): Promise<any[]> {
    onlyRecipient();
    await holochainCall(DNA_ROLE, ZOME_NAME, "fetch_inbox", null);
    return (await holochainCall(DNA_ROLE, ZOME_NAME, "inbox", filter)) as any[];
}

export function directMessageAddMessageCallback(callback: (msg: any) => Promise<void>): void {
    console.log("adding callback on dm language");
    onlyRecipient();
    messageCallbacks.push(callback);
}
