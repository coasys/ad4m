/**
 * Room key exchange (protocol 2): any member rotates, any member grants.
 *
 * Every key that authenticates publishes an X25519 key with its signed statement
 * (src/room.ts), and `/members` lists the keys the room admits. A box goes only to a key whose
 * statement verifies against its own DID and that the executor's membership of the space
 * admits (`membershipAdmits`, K1): the server chooses whom to list, so a member does not take
 * its word for either. The link server takes a rotation only when its boxes are for exactly
 * the keys it admits (409 otherwise), lets a member grant only versions it holds, and keeps
 * the first box stored per key and version.
 */

import type { RoomConfig } from "./adapters.js";
import * as api from "./api.js";
import { generateRoomKey, roomKeyCommitment, sealRoomKeyTo, type KeyRing } from "./encryption.js";
import { verifyRoomKeySignature } from "./room.js";
import type { MemberKey, MemberMissingKeys } from "./types.js";

function message(err: unknown): string {
    return err instanceof Error ? err.message : String(err);
}

/**
 * There is no plaintext mode: only "ready" commits.
 * - "pending": this agent holds no room key it opens (none minted yet, or no member holding
 *   one has granted it).
 * - "ready": the key ring is loaded.
 * - "error": the key ring could not be read or opened.
 */
export type KeyRingStatus = "pending" | "ready" | "error";

/** Whether the space admits a key now, as the executor's membership says (`membershipAdmits`). */
export type Admits = (did: string) => Promise<boolean>;

/**
 * K2: any admitted member holding no room key mints one, once a run, when the room has no
 * version at all: `/keys/missing` does not name it (that list names the admitted keys lacking
 * a version, and is empty while the room has none). A member the list names waits for a grant.
 */
export function mintsRoomKey(
    state: { status: KeyRingStatus; rotatedThisRun: boolean; admitted: boolean },
    missing: MemberMissingKeys[],
    myDid: string,
): boolean {
    if (!state.admitted || state.rotatedThisRun || state.status !== "pending") return false;
    return !missing.some((m) => m.did === myDid);
}

/**
 * The key ring after a `/keys` read or open failed (network, server, a locked
 * agent). The keys already held are still the room's keys: a loaded ring
 * stays loaded, so commits still encrypt and inbound links still open. Catch-up
 * re-reads `/keys` while any link is held, so dropping the ring here would
 * turn one failed read into refused commits and held links. Only a ring that
 * never loaded becomes "error".
 */
export function afterFailedKeyRead(
    status: KeyRingStatus,
    ring: KeyRing | null,
): { status: KeyRingStatus; ring: KeyRing | null } {
    if (status === "ready" && ring && ring.size > 0) return { status, ring };
    return { status: "error", ring: null };
}

/**
 * The keys of `keys` a box may go to (K1): the statement verifies, and the space admits the
 * key. The others are returned as `skipped`, each logged.
 */
async function sealable(keys: MemberKey[], admits: Admits): Promise<{ sealTo: MemberKey[]; skipped: string[] }> {
    const sealTo: MemberKey[] = [];
    const skipped: string[] = [];
    for (const key of keys) {
        if (!verifyRoomKeySignature(key.did, key.x25519PublicKey, key.x25519Signature)) {
            console.error(`[server-link-language] the room lists an X25519 key ${key.did} did not sign; not sealing to it`);
            skipped.push(key.did);
            continue;
        }
        let admitted = false;
        try {
            admitted = await admits(key.did);
        } catch (err) {
            console.error(`[server-link-language] cannot ask whether the space admits ${key.did}: ${message(err)}`);
        }
        if (!admitted) {
            console.error(`[server-link-language] the space does not admit ${key.did} (the room lists it); not sealing to it`);
            skipped.push(key.did);
            continue;
        }
        sealTo.push(key);
    }
    return { sealTo, skipped };
}

/** Attempts at a rotation the server refuses with 409 before giving up. */
export const ROTATE_ATTEMPTS = 3;

export interface RotateOutcome {
    result: api.RotateResponse;
    /** Keys the room listed that this rotation did not seal to (K1). */
    skipped: string[];
}

/**
 * Any admitted member: a new room key version, sealed to every key of `/members` that K1
 * allows. Returns null, and rotates nothing, when there is no key to seal to. When the server
 * refuses the boxes (409: they are not for exactly its admitted keys), `beforeRetry` runs
 * (the caller catches up on membership, so the executor knows a member the server just
 * admitted) and the rotation starts over with a fresh read, at most `ROTATE_ATTEMPTS` times.
 */
export async function rotateRoomKey(
    config: RoomConfig,
    token: string,
    admits: Admits,
    opts: { beforeRetry?: () => Promise<void> } = {},
): Promise<RotateOutcome | null> {
    for (let attempt = 1; ; attempt++) {
        const members = await api.fetchMembers(config, token);
        const { sealTo, skipped } = await sealable(members.keys, admits);
        if (sealTo.length === 0) return null;
        const roomKey = generateRoomKey();
        const keys: api.RotateKeyEntry[] = sealTo.map((key) => ({
            did: key.did,
            encryptedKey: sealRoomKeyTo(key.x25519PublicKey, roomKey),
        }));
        try {
            const result = await api.rotateKeys(config, token, keys, roomKeyCommitment(roomKey));
            return { result, skipped };
        } catch (err) {
            if (!(err instanceof api.RotateMismatch) || attempt >= ROTATE_ATTEMPTS) throw err;
            console.log(
                `[server-link-language] the room's keys changed during rotation (missing: ${err.missing.join(", ") || "-"}; ` +
                `extra: ${err.extra.join(", ") || "-"}) — sealing again`,
            );
            await opts.beforeRetry?.();
        }
    }
}

/**
 * Any member: seals each version in `ring` to every other admitted key lacking it, at the
 * X25519 key `/members` lists for it, if K1 allows. A key it cannot grant to (not listed, a
 * statement that fails, not admitted by the space) is logged and skipped. Returns the versions
 * the server stored, per key.
 */
export async function grantMissingRoomKeys(
    config: RoomConfig,
    token: string,
    ring: KeyRing,
    myDid: string,
    admits: Admits,
): Promise<Array<{ did: string; versions: number[] }>> {
    const { membersNeedingHistoricalKeys } = await api.fetchMissingKeys(config, token);
    const needing = membersNeedingHistoricalKeys.filter(
        (m) => m.did !== myDid && m.missingVersions.some((v) => ring.has(v)),
    );
    if (needing.length === 0) return [];
    const members = await api.fetchMembers(config, token);
    const listed = members.keys.filter((k) => needing.some((m) => m.did === k.did));
    const { sealTo } = await sealable(listed, admits);
    const grants: Array<{ did: string; versions: number[] }> = [];
    for (const member of needing) {
        const key = sealTo.find((k) => k.did === member.did);
        if (!key) {
            if (!listed.some((k) => k.did === member.did)) {
                console.error(`[server-link-language] ${member.did} lacks a key version but the room lists no X25519 key for it`);
            }
            continue;
        }
        try {
            const keys: api.GrantKeyEntry[] = [];
            for (const version of member.missingVersions) {
                const roomKey = ring.get(version);
                if (roomKey) keys.push({ version, encryptedKey: sealRoomKeyTo(key.x25519PublicKey, roomKey) });
            }
            const versions = await api.grantKeys(config, token, member.did, keys);
            if (versions.length > 0) grants.push({ did: member.did, versions });
        } catch (err) {
            console.error(`[server-link-language] key grant to ${member.did} failed: ${message(err)}`);
        }
    }
    return grants;
}
