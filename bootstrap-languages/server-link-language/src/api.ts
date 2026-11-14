/**
 * Server HTTP client — thin, typed wrappers around every
 * link-server REST endpoint. Pure module: takes the Transport
 * singleton from adapters.ts (swappable for a mock in tests) and never
 * imports ad4m:host directly.
 */

import { getTransport } from "./adapters.js";
import type { RoomConfig } from "./adapters.js";
import type {
    AuthRequest,
    AuthResponse,
    MembersResponse,
    MembershipLinkResult,
    MembershipResponse,
    MissingKeysResponse,
    KeysResponse,
    PeersResponse,
    RenderResponse,
    SealedBox,
    SyncResponse,
    WireMembershipLink,
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

/**
 * Protocol 2: one signed request, `{did, signature, timestamp, x25519PublicKey, x25519Signature}`
 * -> `{token, expiresAt, admitted}`. Auth never creates a room: a room that does not exist yet
 * answers 404 (its creator's genesis on `/membership` makes it), a protocol 1 room 410, a
 * suspended room 423.
 */
export async function authenticate(config: RoomConfig, body: AuthRequest): Promise<AuthResponse> {
    const res = await request<Partial<AuthResponse>>(roomUrl(config, "/auth"), "POST", jsonHeaders(), JSON.stringify(body));
    if (typeof res?.token !== "string" || typeof res.admitted !== "boolean") {
        throw new Error("link server answered /auth without {token, admitted}");
    }
    return { token: res.token, expiresAt: res.expiresAt, admitted: res.admitted };
}

// ---------------------------------------------------------------------------
// Membership (protocol 2)
// ---------------------------------------------------------------------------

/** What the server said about a membership POST: each link's result, and the refusal when none counted. */
export interface MembershipPostResult {
    /** True when at least one link counted (200); false when the server counted none (403). */
    accepted: boolean;
    reason?: string;
    results: MembershipLinkResult[];
}

/**
 * POST membership links. With `token` null the request carries the room's genesis
 * (`{creator, salt}`) and no token: the server creates the room when the room does not exist
 * and the links make `creator` its first member. A room that exists ignores the genesis.
 * A 403 (no link counted) is an answer, not an error; every other non-2xx status throws.
 */
export async function postMembership(
    config: RoomConfig,
    token: string | null,
    links: WireMembershipLink[],
    genesis?: { creator: string; salt: string },
): Promise<MembershipPostResult> {
    const body = JSON.stringify(genesis ? { genesis, links } : { links });
    const res = await getTransport().fetch(roomUrl(config, "/membership"), "POST", jsonHeaders(token ?? undefined), body);
    if (res.status !== 403 && (res.status < 200 || res.status >= 300)) {
        throw new ApiError(res.status, res.body || `HTTP ${res.status} from POST /membership`);
    }
    let parsed: { reason?: unknown; results?: unknown; error?: unknown };
    try {
        parsed = JSON.parse(res.body) as typeof parsed;
    } catch {
        throw new ApiError(res.status, `Invalid JSON response from POST /membership`);
    }
    if (res.status === 403 && !Array.isArray(parsed.results)) {
        // A 403 without results is an auth refusal (a token the room refuses), not a judgement.
        throw new ApiError(403, res.body);
    }
    return {
        accepted: res.status !== 403,
        reason: typeof parsed.reason === "string" ? parsed.reason : undefined,
        results: Array.isArray(parsed.results) ? (parsed.results as MembershipLinkResult[]) : [],
    };
}

/** One page of the room's membership log after `since`. */
export async function fetchMembership(
    config: RoomConfig,
    token: string,
    since: number,
    limit: number,
): Promise<MembershipResponse> {
    const path = `/membership?since=${encodeURIComponent(String(since))}&limit=${limit}`;
    const res = await request<MembershipResponse>(roomUrl(config, path), "GET", jsonHeaders(token));
    expectPage(Array.isArray(res?.links) && typeof res.seq === "number", path);
    return res;
}

/**
 * The room's members, the keys a room key is sealed to (each with its signed X25519 key), and
 * the room's latest key version (0 before the first rotation).
 */
export async function fetchMembers(config: RoomConfig, token: string): Promise<MembersResponse> {
    const res = await request<Partial<MembersResponse>>(roomUrl(config, "/members"), "GET", jsonHeaders(token));
    if (!Array.isArray(res?.keys)) throw new Error("link server answered /members without a keys list");
    const latest = res.latestKeyVersion;
    if (typeof latest !== "number" || !Number.isSafeInteger(latest) || latest < 0) {
        throw new Error("link server answered /members without the room's latest key version");
    }
    return { members: Array.isArray(res.members) ? res.members : [], keys: res.keys, latestKeyVersion: latest };
}

// ---------------------------------------------------------------------------
// Perspective sync / commit
// ---------------------------------------------------------------------------

/** Where the server put an accepted commit in the room's diff log. A field
 *  the answer does not carry is absent. */
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

/** Throws unless `ok`: an answer without the paged fields cannot be read as a page. */
function expectPage(ok: boolean, path: string): void {
    if (!ok) throw new Error(`link server answered GET ${path} without a paged body`);
}

/** One page of the diff log after `since`: at most `limit` diffs, each with its sequence. */
export async function fetchSync(config: RoomConfig, token: string, since: number, limit: number): Promise<SyncResponse> {
    const path = `/sync?since=${encodeURIComponent(String(since))}&limit=${limit}`;
    const res = await request<SyncResponse>(roomUrl(config, path), "GET", jsonHeaders(token));
    expectPage(
        Array.isArray(res?.diffs) &&
            res.diffs.every((d) => typeof d?.sequence === "number") &&
            typeof res.revision === "string" &&
            typeof res.sequence === "number" &&
            (res.next === null || (typeof res.next === "number" && res.next > since)),
        path,
    );
    return res;
}

/** One page of the room's snapshot: the first when `next` is null. */
export async function fetchRender(
    config: RoomConfig,
    token: string,
    page: { limit: number; next: string | null },
): Promise<RenderResponse> {
    const path = `/render?limit=${page.limit}` + (page.next !== null ? `&next=${encodeURIComponent(page.next)}` : "");
    const res = await request<RenderResponse>(roomUrl(config, path), "GET", jsonHeaders(token));
    expectPage(
        Array.isArray(res?.links) &&
            typeof res.revision === "string" &&
            typeof res.sequence === "number" &&
            typeof res.upto === "number" &&
            (res.next === null || (typeof res.next === "string" && res.next.length > 0)),
        path,
    );
    return res;
}

// ---------------------------------------------------------------------------
// Peers
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
 * A rotation the server refused (409): its boxes were not sealed to exactly the room's
 * admitted keys (`missing` are admitted keys without a box, `extra` boxes for keys that are not
 * admitted), or the room's latest version is no longer the one before the rotation's. The
 * rotator refetches `/members` and seals again.
 */
export class RotateMismatch extends ApiError {
    missing: string[];
    extra: string[];
    constructor(body: string) {
        super(409, body);
        let parsed: { missing?: unknown; extra?: unknown } = {};
        try {
            parsed = JSON.parse(body) as typeof parsed;
        } catch {
            // A 409 without a readable body names nothing; the caller refetches all the same.
        }
        const strings = (v: unknown): string[] => (Array.isArray(v) ? v.filter((d): d is string => typeof d === "string") : []);
        this.missing = strings(parsed.missing);
        this.extra = strings(parsed.extra);
    }
}

/**
 * Rotates the room's E2E key (any member, K2). The caller generates the room key locally,
 * seals it to each admitted key's X25519 key, and sends only the sealed boxes and the key's
 * commitment (`roomKeyCommitment`), as `version`: the version the boxes are sealed for (K9),
 * the room's latest plus one. The server never touches the plaintext key. Throws
 * `RotateMismatch` when the boxes are not for exactly the admitted keys or `version` is not
 * the next one.
 */
export async function rotateKeys(
    config: RoomConfig,
    token: string,
    keys: RotateKeyEntry[],
    commitment: string,
    version: number,
): Promise<RotateResponse> {
    try {
        return await request<RotateResponse>(
            roomUrl(config, "/keys/rotate"),
            "POST",
            jsonHeaders(token),
            JSON.stringify({ keys, commitment, version }),
        );
    } catch (e) {
        if (e instanceof ApiError && e.status === 409) throw new RotateMismatch(e.message);
        throw e;
    }
}

/** Drops this agent's box for `version`, so a member holding the version grants it again. */
export async function deleteRoomKey(config: RoomConfig, token: string, version: number): Promise<void> {
    // No body, so no JSON content type: the server refuses an empty JSON body.
    await request<unknown>(roomUrl(config, `/keys/${version}`), "DELETE", { Authorization: `Bearer ${token}` });
}

/**
 * Fetches the room key versions sealed to this agent. The server answers 200
 * with an empty list when it holds none: the agent waits for one. Any error
 * status, 404 included, throws.
 */
export async function fetchRoomKeys(config: RoomConfig, token: string): Promise<KeysResponse> {
    const res = await request<Partial<KeysResponse> | undefined>(roomUrl(config, "/keys"), "GET", jsonHeaders(token));
    // A body without a list is a broken answer, not "no keys": throwing keeps a loaded ring (`afterFailedKeyRead`).
    if (!Array.isArray(res?.keys)) throw new Error("server-link-language: /keys answered without a keys list");
    return { keys: res.keys };
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
// E2E missing keys
// ---------------------------------------------------------------------------

/** Any member. Returns the admitted keys lacking key versions, by DID. */
export async function fetchMissingKeys(config: RoomConfig, token: string): Promise<MissingKeysResponse> {
    const res = await request<Partial<MissingKeysResponse>>(
        roomUrl(config, "/keys/missing"), "GET", jsonHeaders(token),
    );
    return { membersNeedingHistoricalKeys: res.membersNeedingHistoricalKeys ?? [] };
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
