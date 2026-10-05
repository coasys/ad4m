/**
 * Test language for local-first expression writes and cached reads.
 *
 * An in-memory map stands in for remote storage, and the `setOnline`
 * interaction cuts it off, so a test can create while "offline" and watch
 * the runtime publish from its queue later. `stats` reports how often the
 * runtime called `expressionGet`, which a cached read must not do.
 * Interactions take any address of this language: `<lang>://control`.
 */
import { agentCreateSignedExpression, hash } from "ad4m:host";

export const name = "local-first-store";
export const version = "0.1.0";

const remote = new Map<string, any>();
let online = true;
let getCalls = 0;

export async function init(): Promise<void> {}
export async function teardown(): Promise<void> {}

export function isImmutableExpression(_address: string): boolean {
    return true;
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
    return remote.get(address) ?? null;
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
            execute: async () => JSON.stringify({ getCalls, remote: [...remote.keys()] }),
        },
    ];
}
