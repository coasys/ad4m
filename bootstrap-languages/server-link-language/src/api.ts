/**
 * Server HTTP client — thin, typed wrappers around every
 * link-server REST endpoint. Pure module: takes the Transport
 * singleton from adapters.ts (swappable for a mock in tests) and never
 * imports ad4m:host directly.
 */

import { getTransport } from "./adapters.js";
import type { RoomConfig } from "./adapters.js";
import type {
    AclResponse,
    AuthChallengeResponse,
    AuthTokenResponse,
    MissingKeysResponse,
    KeysResponse,
    PeersResponse,
    RenderResponse,
    SealedBox,
    SyncResponse,
    WirePerspectiveDiff,
} from "./types.js";

export class ApiError extends Error {
    status: number;
    constructor(status: number, message: string) {
        super(message);
        this.name = "ApiError";
        this.status = status;
    }
}

function roomUrl(config: RoomConfig, path: string): string {
    const base = config.serverUrl.replace(/\/+$/, "");
    return `${base}/rooms/${encodeURIComponent(config.roomId)}${path}`;
}

function jsonHeaders(token?: string): Record<string, string> {
    const headers: Record<string, string> = { "Content-Type": "application/json" };
    if (token) headers["Authorization"] = `Bearer ${token}`;
    return headers;
}

async function request<T>(
    url: string,
    method: string,
    headers: Record<string, string>,
    body?: string,
): Promise<T> {
    const res = await getTransport().fetch(url, method, headers, body ?? "");
    if (res.status < 200 || res.status >= 300) {
        throw new ApiError(res.status, res.body || `HTTP ${res.status} from ${method} ${url}`);
    }
    if (!res.body || res.body.length === 0) {
        return undefined as unknown as T;
    }
    try {
        return JSON.parse(res.body) as T;
    } catch (err) {
        throw new ApiError(
            res.status,
            `Invalid JSON response from ${method} ${url}: ${(err as Error).message}`,
        );
    }
}

// ---------------------------------------------------------------------------
// Auth
// ---------------------------------------------------------------------------

/** Step 1: POST {did} -> {challenge} */
export async function requestChallenge(config: RoomConfig, did: string): Promise<string> {
    const res = await request<AuthChallengeResponse>(
        roomUrl(config, "/auth"),
        "POST",
        jsonHeaders(),
        JSON.stringify({ did }),
    );
    return res.challenge;
}

/**
 * Step 2: POST {did, challenge, signature} -> {token}
 */
export async function verifyChallenge(
    config: RoomConfig,
    did: string,
    challenge: string,
    signature: string,
): Promise<string> {
    const res = await request<AuthTokenResponse>(
        roomUrl(config, "/auth"),
        "POST",
        jsonHeaders(),
        JSON.stringify({ did, challenge, signature }),
    );
    return res.token;
}

// ---------------------------------------------------------------------------
// Perspective sync / commit
// ---------------------------------------------------------------------------

/** Where the server put an accepted commit in the room's diff log. Fields
 *  are absent when the server's answer carries none (older servers). */
export interface CommitResult {
    sequence?: number;
    revision?: string;
}

export async function commitDiff(config: RoomConfig, token: string, diff: WirePerspectiveDiff): Promise<CommitResult> {
    const res = await request<unknown>(roomUrl(config, "/commit"), "POST", jsonHeaders(token), JSON.stringify(diff));
    if (!res || typeof res !== "object") return {};
    const { sequence, revision } = res as { sequence?: unknown; revision?: unknown };
    return {
        sequence: typeof sequence === "number" ? sequence : undefined,
        revision: typeof revision === "string" ? revision : undefined,
    };
}

export async function fetchSync(config: RoomConfig, token: string, since: number): Promise<SyncResponse> {
    const url = roomUrl(config, `/sync?since=${encodeURIComponent(String(since))}`);
    const res = await request<Partial<SyncResponse>>(url, "GET", jsonHeaders(token));
    return {
        diffs: res.diffs ?? [],
        revision: res.revision ?? "",
        sequence: typeof res.sequence === "number" ? res.sequence : since,
    };
}

export async function fetchRender(config: RoomConfig, token: string): Promise<RenderResponse> {
    const res = await request<Partial<RenderResponse>>(roomUrl(config, "/render"), "GET", jsonHeaders(token));
    return {
        links: res.links ?? [],
        revision: res.revision ?? "",
        sequence: typeof res.sequence === "number" ? res.sequence : 0,
    };
}

// ---------------------------------------------------------------------------
// Peers / revision / ACL / keys
// ---------------------------------------------------------------------------

export async function fetchPeers(config: RoomConfig, token: string): Promise<string[]> {
    const res = await request<Partial<PeersResponse>>(roomUrl(config, "/peers"), "GET", jsonHeaders(token));
    return res.peers ?? [];
}

// ---------------------------------------------------------------------------
// E2E key rotation (client-side, server never sees plaintext)
// ---------------------------------------------------------------------------

export interface RotateKeyEntry {
    did: string;
    encryptedKey: SealedBox;
}

export interface RotateResponse {
    version: number;
    recipients: string[];
}

/**
 * Rotates the room's E2E key (room admin only). The caller generates the
 * room key locally, seals it to each ACL member's DID, and sends only the
 * sealed boxes. The server never touches the plaintext key.
 */
export async function rotateKeys(
    config: RoomConfig,
    token: string,
    keys: RotateKeyEntry[],
): Promise<RotateResponse> {
    return request<RotateResponse>(
        roomUrl(config, "/keys/rotate"),
        "POST",
        jsonHeaders(token),
        JSON.stringify({ keys }),
    );
}

/**
 * Fetches the room's E2E keys for this agent.
 *
 * Returns null when the room has no E2E at all (server responds 404).
 * Returns `{ keys: [], e2e_enabled: true }` when the room HAS E2E but
 * this agent has no keys yet (freshly added member awaiting grant).
 */
export async function fetchRoomKeys(config: RoomConfig, token: string): Promise<KeysResponse | null> {
    try {
        const res = await request<Partial<KeysResponse>>(roomUrl(config, "/keys"), "GET", jsonHeaders(token));
        if (!res) return null;
        return {
            keys: res.keys ?? [],
            e2e_enabled: res.e2e_enabled ?? (!!res.keys && res.keys.length > 0),
        };
    } catch (err) {
        if (err instanceof ApiError && (err.status === 404 || err.status === 204)) {
            return null; // Room has no E2E — 404
        }
        throw err;
    }
}

// ---------------------------------------------------------------------------
// E2E key grant (any member re-seals versions it holds for a member lacking them)
// ---------------------------------------------------------------------------

export interface GrantKeyEntry {
    version: number;
    encryptedKey: SealedBox;
}

export async function grantKeys(
    config: RoomConfig,
    token: string,
    targetDid: string,
    keys: GrantKeyEntry[],
): Promise<number[]> {
    const res = await request<{ granted: number[] }>(
        roomUrl(config, "/keys/grant"),
        "POST",
        jsonHeaders(token),
        JSON.stringify({ targetDid, keys }),
    );
    return res.granted ?? [];
}

// ---------------------------------------------------------------------------
// E2E missing keys + ACL query
// ---------------------------------------------------------------------------

/** Any member. Returns the ACL members lacking key versions, by DID. */
export async function fetchMissingKeys(config: RoomConfig, token: string): Promise<MissingKeysResponse> {
    const res = await request<Partial<MissingKeysResponse>>(
        roomUrl(config, "/keys/missing"), "GET", jsonHeaders(token),
    );
    return { membersNeedingHistoricalKeys: res.membersNeedingHistoricalKeys ?? [] };
}

/** Returns the room's admin DID and its ACL members' DIDs. */
export async function fetchAclInfo(config: RoomConfig, token: string): Promise<AclResponse> {
    const res = await request<Partial<AclResponse>>(roomUrl(config, "/acl"), "GET", jsonHeaders(token));
    return { admin: res.admin ?? "", members: res.members ?? [] };
}

// ---------------------------------------------------------------------------
// WebSocket URL
// ---------------------------------------------------------------------------

/**
 * Returns the WebSocket endpoint URL for a room. The token is NOT
 * included in the URL — auth happens via a first-message frame
 * (`{type:"auth",token:"..."}`) sent immediately after the upgrade
 * completes. This keeps JWTs out of access logs, CDN caches, and
 * browser history.
 */
export function wsUrl(config: RoomConfig): string {
    const httpUrl = roomUrl(config, "/ws");
    return httpUrl.replace(/^http/, "ws"); // http(s):// -> ws(s)://
}
