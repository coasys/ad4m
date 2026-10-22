/**
 * Room key exchange: the room admin rotates, any member grants.
 *
 * Every member's key follows from its DID (src/encryption.ts), so nothing
 * waits for a member to connect: a rotation seals the new version to every
 * ACL member, and any member holding a version seals it to an ACL member
 * lacking it. The link server lets a member grant only versions it holds
 * and keeps the first box stored per member and version.
 */

import type { RoomConfig } from "./adapters.js";
import * as api from "./api.js";
import { generateRoomKey, sealToDid, type KeyRing } from "./encryption.js";

function message(err: unknown): string {
    return err instanceof Error ? err.message : String(err);
}

/**
 * Room admin only (the server refuses anyone else): a new room key version,
 * sealed to the DID of every ACL member. Returns null, and rotates nothing,
 * when no member's DID can be sealed to.
 */
export async function rotateRoomKey(
    config: RoomConfig,
    token: string,
): Promise<api.RotateResponse | null> {
    const acl = await api.fetchAclInfo(config, token);
    const roomKey = generateRoomKey();
    const keys: api.RotateKeyEntry[] = [];
    for (const member of acl.members) {
        try {
            keys.push({ did: member.did, encryptedKey: sealToDid(member.did, roomKey) });
        } catch (err) {
            console.error(`[server-link-language] cannot seal the room key to ${member.did}, skipping: ${message(err)}`);
        }
    }
    if (keys.length === 0) return null;
    return api.rotateKeys(config, token, keys);
}

/**
 * Any member: seals each version in `ring` to every other ACL member that
 * lacks it. A member it cannot grant to (removed meanwhile, a DID that is
 * not an Ed25519 did:key) is logged and skipped. Returns the versions the
 * server stored, per member.
 */
export async function grantMissingRoomKeys(
    config: RoomConfig,
    token: string,
    ring: KeyRing,
    myDid: string,
): Promise<Array<{ did: string; versions: number[] }>> {
    const { membersNeedingHistoricalKeys } = await api.fetchMissingKeys(config, token);
    const grants: Array<{ did: string; versions: number[] }> = [];
    for (const member of membersNeedingHistoricalKeys) {
        if (member.did === myDid) continue;
        try {
            const keys: api.GrantKeyEntry[] = [];
            for (const version of member.missingVersions) {
                const roomKey = ring.get(version);
                if (roomKey) keys.push({ version, encryptedKey: sealToDid(member.did, roomKey) });
            }
            if (keys.length === 0) continue;
            const versions = await api.grantKeys(config, token, member.did, keys);
            if (versions.length > 0) grants.push({ did: member.did, versions });
        } catch (err) {
            console.error(`[server-link-language] key grant to ${member.did} failed: ${message(err)}`);
        }
    }
    return grants;
}
