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
    KeysResponse,
    PeersResponse,
    RenderResponse,
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
 *
 * `x25519PublicKeyHex` is an additive field (not in the base spec) this
 * language sends so the server can capture the agent's E2E public key at
 * the one point it already verifies DID ownership. A server that doesn't
 * care about E2E simply ignores it. See src/encryption.ts module doc.
 */
export async function verifyChallenge(
    config: RoomConfig,
    did: string,
    challenge: string,
    signature: string,
    x25519PublicKeyHex?: string,
): Promise<string> {
    const payload: Record<string, unknown> = { did, challenge, signature };
    if (x25519PublicKeyHex) payload.x25519PublicKey = x25519PublicKeyHex;

    const res = await request<AuthTokenResponse>(
        roomUrl(config, "/auth"),
        "POST",
        jsonHeaders(),
        JSON.stringify(payload),
    );
    return res.token;
}

// ---------------------------------------------------------------------------
// Perspective sync / commit
// ---------------------------------------------------------------------------

export async function commitDiff(config: RoomConfig, token: string, diff: WirePerspectiveDiff): Promise<void> {
    await request<unknown>(roomUrl(config, "/commit"), "POST", jsonHeaders(token), JSON.stringify(diff));
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

export async function fetchAcl(config: RoomConfig, token: string): Promise<AclResponse> {
    const res = await request<Partial<AclResponse>>(roomUrl(config, "/acl"), "GET", jsonHeaders(token));
    return { admin: res.admin ?? "", members: res.members ?? [] };
}

/** Returns null when the room has no E2E keys configured (server responds
 * 404/204, or returns an empty keys array). */
export async function fetchRoomKeys(config: RoomConfig, token: string): Promise<KeysResponse | null> {
    try {
        const res = await request<Partial<KeysResponse>>(roomUrl(config, "/keys"), "GET", jsonHeaders(token));
        if (!res || !res.keys || res.keys.length === 0) return null;
        return { keys: res.keys };
    } catch (err) {
        if (err instanceof ApiError && (err.status === 404 || err.status === 204)) {
            return null;
        }
        throw err;
    }
}

// ---------------------------------------------------------------------------
// E2E key grant (admin re-seals historical versions for late members)
// ---------------------------------------------------------------------------

export interface GrantKeyEntry {
    version: number;
    encryptedKey: { ephemeralPublicKey: string; nonce: string; ciphertext: string };
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
