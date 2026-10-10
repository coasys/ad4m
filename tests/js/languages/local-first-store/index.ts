/**
 * Test language for local-first expression writes and cached reads.
 *
 * An in-memory map stands in for remote storage, and the `setOnline`
 * interaction cuts it off, so a test can create while "offline" and watch
 * the runtime publish from its queue later. `stats` reports how often the
 * runtime called `expressionGet`, which a cached read must not do, and the
 * most `expressionGet` calls in flight at once since the last `stats`: a
 * runtime serves one request at a time, so more than one means one batch.
 * `setImmutable(false)` makes every address mutable.
 * Interactions take any address of this language: `<lang>://control`.
 */
import { agentCreateSignedExpression, hash } from "ad4m:host";

export const name = "local-first-store";
export const version = "0.1.0";

const remote = new Map<string, any>();
let online = true;
let immutable = true;
let getCalls = 0;
let getsInFlight = 0;
let peakGetsInFlight = 0;

export async function init(): Promise<void> {}
export async function teardown(): Promise<void> {}

export function isImmutableExpression(_address: string): boolean {
    return immutable;
}

export async function expressionPrepare(content: object): Promise<{ address: string; expression: any }> {
    return { address: hash(JSON.stringify(content)), expression: agentCreateSignedExpression(content) };
}

export async function expressionPublish(address: string, expression: any): Promise<void> {
    if (!online) throw new Error("offline");
    remote.set(address, expression);
}

export async function expressionCreate(content: object): Promise<string> {
    const { address, expression } = await expressionPrepare(content);
    await expressionPublish(address, expression);
    return address;
}

export async function expressionGet(address: string): Promise<any | null> {
    getCalls++;
    if (!online) throw new Error("offline");
    getsInFlight++;
    peakGetsInFlight = Math.max(peakGetsInFlight, getsInFlight);
    try {
        // Stay in flight long enough for the rest of a batch to start.
        await new Promise((resolve) => setTimeout(resolve, 50));
        return remote.get(address) ?? null;
    } finally {
        getsInFlight--;
    }
}

export function interactions(_address: string): any[] {
    return [
        {
            label: "Set online",
            name: "setOnline",
            parameters: [{ name: "online", type: "boolean" }],
            execute: async (params: { online: boolean }) => {
                online = params.online;
                return "ok";
            },
        },
        {
            label: "Set immutable",
            name: "setImmutable",
            parameters: [{ name: "immutable", type: "boolean" }],
            execute: async (params: { immutable: boolean }) => {
                immutable = params.immutable;
                return "ok";
            },
        },
        {
            label: "Store remotely only",
            name: "seedRemote",
            parameters: [{ name: "contents", type: "array" }],
            execute: async (params: { contents: object[] }) => {
                const addresses = params.contents.map((content) => {
                    const address = hash(JSON.stringify(content));
                    remote.set(address, agentCreateSignedExpression(content));
                    return address;
                });
                return JSON.stringify(addresses);
            },
        },
        {
            label: "Stats",
            name: "stats",
            parameters: [],
            execute: async () => {
                const peakGets = peakGetsInFlight;
                peakGetsInFlight = 0;
                return JSON.stringify({ getCalls, peakGets, remote: [...remote.keys()] });
            },
        },
    ];
}
