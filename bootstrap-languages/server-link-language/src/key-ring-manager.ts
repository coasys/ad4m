/**
 * The room's E2E key ring for one language instance: loading it, the
 * admin's duties (initial key, grants to members who lack keys), and how a
 * member waiting for its first grant notices the grant quickly.
 *
 * Pure module: talks to the server through api.ts (the Transport
 * singleton) and gets its token, X25519 private key and "new keys arrived"
 * reaction from the caller, so tests can run two instances (two agents)
 * side by side against one server.
 *
 * ## Why a waiting member polls
 *
 * The link-server sends no message when the admin grants keys (its WS
 * messages are diff, telepresence, online-agents, peer-joined/left,
 * status-changed, auth-error). A member whose /keys answer was "E2E room,
 * no keys for you" ("pending") would otherwise only re-check on a
 * catch-up, and only once per KEY_RING_RETRY_COOLDOWN_MS. So while
 * pending it re-reads /keys on a short schedule (PENDING_POLL_SCHEDULE_MS),
 * and restarts that schedule when a peer joins (the admin may just have
 * come online and granted).
 */

import * as api from "./api.js";
import type { RoomConfig } from "./adapters.js";
import {
    buildKeyRing,
    generateRoomKey,
    hexToBytes,
    sealRoomKeyForRecipient,
    verifyX25519Ownership,
    type KeyRing,
} from "./encryption.js";
import type { KeyRingStatus } from "./key-ring-policy.js";

/** Background key-ring refreshes (catch-up, WS diffs with unknown versions)
 *  run at most once per this many ms, except while the pending poll runs. */
export const KEY_RING_RETRY_COOLDOWN_MS = 10_000;

/** Delays between /keys reads while this agent waits for its first grant:
 *  every 500 ms for 10 s, then every 2 s up to one minute. About 45
 *  requests, well inside the server's room limit (300 per minute per JWT). */
export const PENDING_POLL_SCHEDULE_MS: number[] = [
    ...Array.from({ length: 20 }, () => 500),
    ...Array.from({ length: 25 }, () => 2_000),
];

export interface KeyRingManagerDeps {
    config: RoomConfig;
    getToken: () => Promise<string>;
    /** This agent's X25519 private key, which opens the sealed room keys. */
    x25519PrivateKey: () => Uint8Array;
    /** Called when new key versions arrived through the pending poll: the
     *  caller re-reads the room so links it could not decrypt appear. */
    onNewKeys: () => Promise<void>;
    /** Overrides PENDING_POLL_SCHEDULE_MS (tests). */
    pendingPollScheduleMs?: number[];
    now?: () => number;
}

export class KeyRingManager {
    /** Version → AES-256-GCM key. Null for a plaintext room OR while E2E
     *  setup is still in flight. */
    keyRing: KeyRing | null = null;
    /**
     * - "none": room has no E2E — plaintext commits are fine.
     * - "ready": key ring acquired and decrypted — encrypted commits ready.
     * - "pending": room HAS E2E but this agent has no keys yet (freshly
     *   added member awaiting grant). commit() refuses to send plaintext.
     * - "error": E2E keys almost certainly exist but we could not get/open
     *   them. commit() refuses to send plaintext.
     */
    status: KeyRingStatus = "none";
    /** True when this agent is the room admin. */
    isAdmin = false;

    private readonly deps: KeyRingManagerDeps;
    private lastRetry = 0;
    private loadInflight: Promise<void> | null = null;
    private grantInflight: Promise<void> | null = null;
    private grantAgain = false;
    private pollTimer: ReturnType<typeof setTimeout> | null = null;
    private polling = false;
    /** Bumped by teardown and by every poll restart: a callback from an
     *  older generation does nothing. */
    private gen = 0;
    private pollGen = 0;

    constructor(deps: KeyRingManagerDeps) {
        this.deps = deps;
    }

    private now(): number {
        return (this.deps.now ?? Date.now)();
    }

    /** Asks the server whether `did` administers the room. */
    async detectAdmin(did: string): Promise<boolean> {
        try {
            const aclInfo = await api.fetchAclInfo(this.deps.config, await this.deps.getToken());
            this.isAdmin = aclInfo.admin === did;
        } catch (err) {
            console.error("[server-link-language] could not read the room's ACL; acting as a member:", err);
            this.isAdmin = false;
        }
        return this.isAdmin;
    }

    /** Reads /keys once and updates keyRing/status. Starts the pending poll
     *  when the answer is "E2E room, no keys for you yet". */
    async setup(): Promise<void> {
        await this.loadCoalesced();
        if (this.status === "pending") this.startPendingPoll();
        else this.stopPendingPoll();
    }

    /** setup() shared by concurrent callers. */
    loadCoalesced(): Promise<void> {
        if (!this.loadInflight) {
            const p = this.load().finally(() => {
                if (this.loadInflight === p) this.loadInflight = null;
            });
            this.loadInflight = p;
        }
        return this.loadInflight;
    }

    private async load(): Promise<void> {
        const gen = this.gen;
        try {
            const token = await this.deps.getToken();
            const keysRes = await api.fetchRoomKeys(this.deps.config, token);
            if (gen !== this.gen) return;
            if (!keysRes) {
                this.status = "none";
                this.keyRing = null;
                return;
            }
            if (keysRes.keys.length === 0 && keysRes.e2e_enabled) {
                if (this.status !== "pending") {
                    console.log(
                        "[server-link-language] room has E2E enabled but this agent has no keys yet — " +
                        "commits blocked until the room admin grants keys",
                    );
                }
                this.status = "pending";
                this.keyRing = null;
                return;
            }
            if (keysRes.keys.length === 0) {
                this.status = "none";
                this.keyRing = null;
                return;
            }
            this.keyRing = buildKeyRing(keysRes.keys, this.deps.x25519PrivateKey());
            this.status = "ready";
            const versions = [...this.keyRing.keys()].sort((a, b) => a - b);
            console.log(
                `[server-link-language] E2E key ring acquired (${versions.length} version(s): ${versions.join(", ")})`,
            );
        } catch (err) {
            if (gen !== this.gen) return;
            this.status = "error";
            this.keyRing = null;
            console.error(
                "[server-link-language] failed to acquire/decrypt E2E key ring — " +
                "refusing to commit plaintext until this resolves:",
                err,
            );
        }
    }

    /**
     * Background refresh (sync's refreshKeyRing dep). Returns true when new
     * key versions arrived, false when the ring did not grow, null when it
     * skipped: within the cooldown, or while the pending poll is already
     * re-reading /keys on its own faster schedule.
     */
    async refresh(): Promise<boolean | null> {
        if (this.polling) return null;
        const now = this.now();
        if (now - this.lastRetry < KEY_RING_RETRY_COOLDOWN_MS) return null;
        this.lastRetry = now;
        const prevSize = this.keyRing?.size ?? 0;
        await this.loadCoalesced();
        return (this.keyRing?.size ?? 0) > prevSize;
    }

    /**
     * A peer connected. The admin grants keys to members who lack them; a
     * member still waiting for its first grant restarts its fast poll (the
     * peer may be the admin coming online and granting).
     */
    onPeerJoined(): void {
        if (this.isAdmin) {
            void this.grantMissing();
        } else if (this.status === "pending") {
            this.startPendingPoll();
        }
    }

    /** Starts (or restarts) the pending poll from the top of its schedule. */
    startPendingPoll(): void {
        this.stopPendingPoll();
        const gen = this.gen;
        const pollGen = ++this.pollGen;
        const schedule = this.deps.pendingPollScheduleMs ?? PENDING_POLL_SCHEDULE_MS;
        const current = () => gen === this.gen && pollGen === this.pollGen;
        const end = () => { if (current()) this.polling = false; };
        const tick = async (i: number): Promise<void> => {
            this.pollTimer = null;
            if (!current()) return;
            // Another path (commit, catch-up) already got the keys and
            // re-read the room itself.
            if (this.status !== "pending") return end();
            await this.loadCoalesced();
            if (!current()) return;
            // Cast: load() changed status across the await; TS keeps the
            // narrowing from the check above.
            const status = this.status as KeyRingStatus;
            if (status === "ready") {
                end();
                console.log("[server-link-language] room keys granted — re-reading the room");
                try {
                    await this.deps.onNewKeys();
                } catch (err) {
                    console.error("[server-link-language] re-reading the room after a key grant failed:", err);
                }
                return;
            }
            if (status === "pending" && i + 1 < schedule.length) {
                this.pollTimer = setTimeout(() => void tick(i + 1), schedule[i + 1]);
            } else {
                end();
            }
        };
        if (schedule.length > 0) {
            this.polling = true;
            this.pollTimer = setTimeout(() => void tick(0), schedule[0]);
        }
    }

    stopPendingPoll(): void {
        if (this.pollTimer) {
            clearTimeout(this.pollTimer);
            this.pollTimer = null;
        }
        this.pollGen++;
        this.polling = false;
    }

    /** True from startPendingPoll until the poll got keys, ran out of
     *  schedule, or was stopped. */
    isPolling(): boolean {
        return this.polling;
    }

    /**
     * Admin only: seal every key version a member lacks to that member's
     * registered X25519 key. /keys/missing lists every ACL member with an
     * X25519 key and a missing version, including a member with no keys at
     * all, so this covers new joiners. Concurrent calls share one run; a
     * call that arrives during a run causes one more run afterwards, so a
     * member who registered meanwhile is not missed.
     */
    grantMissing(): Promise<void> {
        if (this.grantInflight) {
            this.grantAgain = true;
            return this.grantInflight;
        }
        const run = (async () => {
            do {
                this.grantAgain = false;
                await this.grantOnce();
            } while (this.grantAgain);
        })().finally(() => {
            this.grantInflight = null;
        });
        this.grantInflight = run;
        return run;
    }

    private async grantOnce(): Promise<void> {
        const keyRing = this.keyRing;
        if (!this.isAdmin || !keyRing || keyRing.size === 0) return;
        const config = this.deps.config;
        try {
            const token = await this.deps.getToken();
            const missingRes = await api.fetchMissingKeys(config, token);
            for (const member of missingRes.membersNeedingHistoricalKeys) {
                // Verify X25519 key ownership before trusting the server's ACL
                if (member.x25519Signature) {
                    if (!verifyX25519Ownership(member.did, member.x25519PublicKey, member.x25519Signature)) {
                        console.error(
                            `[server-link-language] X25519 signature verification failed for ${member.did} — skipping key grant`,
                        );
                        continue;
                    }
                }
                const recipientPub = hexToBytes(member.x25519PublicKey);
                const sealedKeys: api.GrantKeyEntry[] = [];
                for (const version of member.missingVersions) {
                    const roomKey = keyRing.get(version);
                    if (!roomKey) continue; // admin also lacks this version — skip
                    sealedKeys.push({ version, encryptedKey: sealRoomKeyForRecipient(roomKey, recipientPub) });
                }
                if (sealedKeys.length === 0) continue;
                const granted = await api.grantKeys(config, token, member.did, sealedKeys);
                if (granted.length > 0) {
                    console.log(
                        `[server-link-language] admin auto-granted key versions [${granted.join(", ")}] to ${member.did}`,
                    );
                }
            }
        } catch (err) {
            console.error("[server-link-language] admin auto-grant failed (non-fatal):", err);
        }
    }

    /**
     * Admin only: generate a room key client-side, seal it to every ACL
     * member's X25519 public key, and send only the sealed envelopes — the
     * server never sees plaintext key material. Throws on failure.
     */
    async rotate(): Promise<void> {
        if (!this.isAdmin) return;
        const config = this.deps.config;
        try {
            const token = await this.deps.getToken();
            const aclRes = await api.fetchAclInfo(config, token);
            const roomKey = generateRoomKey();
            const sealedKeys: api.RotateKeyEntry[] = [];
            for (const member of aclRes.members) {
                if (!member.x25519PublicKey) continue;
                // Verify X25519 key ownership before trusting the server's ACL
                if (member.x25519Signature) {
                    if (!verifyX25519Ownership(member.did, member.x25519PublicKey, member.x25519Signature)) {
                        console.error(
                            `[server-link-language] X25519 signature verification failed for ${member.did} — skipping`,
                        );
                        continue;
                    }
                }
                sealedKeys.push({
                    did: member.did,
                    encryptedKey: sealRoomKeyForRecipient(roomKey, hexToBytes(member.x25519PublicKey)),
                });
            }
            if (sealedKeys.length === 0) {
                console.log("[server-link-language] no members with X25519 keys — skipping rotation");
                return;
            }
            const result = await api.rotateKeys(config, token, sealedKeys);
            console.log(
                `[server-link-language] client-side key rotation complete: version ${result.version}, ` +
                `${result.recipients.length} recipient(s)`,
            );
            // Not loadCoalesced: a read already in flight started before
            // the rotation and would miss the new version.
            await this.load();
            // Members who registered between the ACL read and the rotation
            // got no envelope; grant them now rather than at the next sync.
            void this.grantMissing();
        } catch (err) {
            console.error("[server-link-language] key rotation failed:", err);
            throw err;
        }
    }

    /** Stops the poll and makes every in-flight callback a no-op. */
    teardown(): void {
        this.gen++;
        this.stopPendingPoll();
        this.keyRing = null;
        this.status = "none";
        this.isAdmin = false;
        this.lastRetry = 0;
        this.loadInflight = null;
        this.grantInflight = null;
        this.grantAgain = false;
    }
}
