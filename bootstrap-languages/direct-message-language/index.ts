/**
 * # Direct Message Language — Flat Export Language
 *
 * DM language backed by Holochain DNA. Implements the flat DM capability
 * (directMessage*) which is a runtime-retained feature not in the v1.0 spec.
 * Flat-export migration of the legacy create()-factory version.
 */

import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "direct-message-language";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

//!@ad4m-template-variable
const recipient_did = "<not templated yet>";

let hc: any = null;
let agent: any = null;
let messageCallbacks: ((msg: any) => Promise<void>)[] = [];

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
    hc = (globalThis as any).__holochainDelegate__;

    const dnaBundle = Buffer.from(BUNDLE, "base64");
    await hc.registerDNAs(
        [
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
            },
        ],
        async (signal: any) => {
            console.debug("DM Language got HC signal:", signal);
            let payload = signal.payload;
            try {
                let string = signal.payload.toString();
                let cropped = string.substring(string.indexOf("{"));
                let parsed = JSON.parse(cropped);
                payload = parsed;
            } catch (e) {
                console.error(e);
            }
            for (const cb of messageCallbacks) {
                await cb(payload);
            }
        }
    );
}

export async function teardown(): Promise<void> {
    hc = null;
    agent = null;
    messageCallbacks = [];
}

export function interactions(): any[] {
    return [];
}

// =============================================================================
// Direct Message capability (flat DM exports)
// =============================================================================

export function directMessageRecipient(): string {
    return recipient_did;
}

export async function directMessageStatus(): Promise<any> {
    let status = null;
    try {
        status = await hc.call(DNA_ROLE, ZOME_NAME, "get_status", null);
    } catch (e) {
        console.debug("DirectMessage Language couldn't get status:", e);
    }
    return status;
}

export async function directMessageSendP2P(message: any): Promise<any> {
    try {
        const messageExpression = agent.createSignedExpression(message);
        await hc.call(DNA_ROLE, ZOME_NAME, "send_p2p", messageExpression);
        return messageExpression;
    } catch (e) {
        console.error("Direct Message Language: Error sending p2p to", recipient_did);
    }
}

export async function directMessageSendInbox(message: any): Promise<any> {
    try {
        const messageExpression = agent.createSignedExpression(message);
        await hc.call(DNA_ROLE, ZOME_NAME, "send_inbox", messageExpression);
        return messageExpression;
    } catch (e) {
        console.error("Direct Message Language: Error sending to inbox of", recipient_did);
    }
}

export async function directMessageSetStatus(status: any): Promise<void> {
    onlyRecipient();
    const statusExpression = agent.createSignedExpression(status);
    await hc.call(DNA_ROLE, ZOME_NAME, "set_status", statusExpression);
}

export async function directMessageInbox(filter?: string): Promise<any[]> {
    onlyRecipient();
    await hc.call(DNA_ROLE, ZOME_NAME, "fetch_inbox", null);
    return await hc.call(DNA_ROLE, ZOME_NAME, "inbox", filter);
}

export function directMessageAddMessageCallback(callback: (msg: any) => Promise<void>): void {
    console.log("adding callback on dm language");
    onlyRecipient();
    messageCallbacks.push(callback);
}

// =============================================================================
// Private helpers
// =============================================================================

function onlyRecipient(): void {
    console.log(recipient_did, agent.did);
    if (recipient_did !== agent.did) throw new Error("Only recipient can call this function!");
}
