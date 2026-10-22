import type { AgentAdapter } from "./adapters.js";
import { buildKeyRing, opensOwnBoxes, type KeyRing } from "./encryption.js";
import type { KeyRingStatus } from "./key-ring-policy.js";
import type { KeysResponse } from "./types.js";

/** What `loadKeyRing` reads and writes; index.ts wires it to the module state. */
export interface KeyRingSetupDeps {
    getToken(): Promise<string>;
    fetchRoomKeys(token: string): Promise<KeysResponse | null>;
    deleteRoomKey(token: string, version: number): Promise<void>;
    getAgent(): Pick<AgentAdapter, "openSealed">;
    myDid(): string;
    /** True once teardown started a new lifecycle: the result then belongs to no one. */
    superseded(): boolean;
    /** Sets the key ring state, at the point the outcome is known. */
    apply(status: KeyRingStatus, ring: KeyRing | null): void;
}

/**
 * Reads this agent's room key boxes and sets the key ring state: "none" for a
 * room without E2E, "pending" while no box opens, "ready" with the versions
 * that open, "error" when the keys could not be read or opened. Drops a
 * spoiled box only while this executor opens a box sealed to its own DID.
 * Leaves the state untouched once `superseded()`.
 */
export async function loadKeyRing(deps: KeyRingSetupDeps): Promise<void> {
    try {
        const token = await deps.getToken();
        const keysRes = await deps.fetchRoomKeys(token);
        if (deps.superseded()) return;
        if (!keysRes) {
            deps.apply("none", null);
            return;
        }
        if (keysRes.keys.length === 0 && keysRes.e2e_enabled) {
            deps.apply("pending", null);
            console.log(
                "[server-link-language] room has E2E enabled but this agent has no keys yet — " +
                "commits blocked until a member holding the room key grants it",
            );
            return;
        }
        if (keysRes.keys.length === 0) {
            deps.apply("none", null);
            return;
        }
        const { ring, spoiled } = buildKeyRing(keysRes.keys, deps.getAgent());
        if (spoiled.length > 0 && !opensOwnBoxes(deps.getAgent(), deps.myDid())) {
            // The boxes failed because this executor cannot open any box now (a locked
            // agent), not because they are spoiled: drop none, and try again later.
            throw new Error(`this executor does not open a box sealed to ${deps.myDid()} right now`);
        }
        // A spoiled box would block its version for good: drop it, and a member holding the
        // version grants it again.
        for (const version of spoiled) {
            await deps.deleteRoomKey(token, version).catch((err) =>
                console.error(`[server-link-language] could not drop the spoiled box for version ${version}:`, err),
            );
        }
        if (deps.superseded()) return;
        if (ring.size === 0) {
            // Boxes sealed to this agent, none of which open (sealed in an older
            // format, or a spoiled grant): the same as holding none.
            deps.apply("pending", null);
            console.log("[server-link-language] no room key version opens for this agent — waiting for a new one");
            return;
        }
        deps.apply("ready", ring);
        const versions = [...ring.keys()].sort((a, b) => a - b);
        console.log(`[server-link-language] E2E key ring acquired (${versions.length} version(s): ${versions.join(", ")})`);
    } catch (err) {
        if (deps.superseded()) return;
        deps.apply("error", null);
        console.error(
            "[server-link-language] failed to acquire/decrypt E2E key ring — " +
            "refusing to commit plaintext until this resolves:",
            err,
        );
    }
}
