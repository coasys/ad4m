/**
 * Protocol 2 authentication against link-server, plus token-expiry tracking so callers can
 * request a guaranteed-fresh JWT without re-authenticating on every single call.
 *
 * One request: the agent's key signs `coasys-link-server:auth:2:<roomId>:<timestamp>`
 * (`agentSignStringHex`) and its room key statement: its X25519 key, the public half of
 * `agentRoomKeySeed`, signed raw over the identity core's `coasys:room-key` bytes (`agentSign`,
 * K7). The server answers a token and whether the room admits the key now.
 */

import { x25519Public } from "../keyring/keyring.js";
import { getAgent, getConfig } from "./adapters.js";
import * as api from "./api.js";
import { bytesToHex } from "./encryption.js";
import { authMessage, roomKeySigningBytes } from "./room.js";

export interface AuthSession {
    token: string;
    /** ms since epoch the token expires at, or null if it has no readable `exp` claim. */
    expiresAt: number | null;
    /** Whether the room admitted this key when it authenticated. */
    admitted: boolean;
}

/** This agent's X25519 key and its signed statement, as sent at auth. */
export interface RoomKeyIdentity {
    x25519PublicKey: string;
    x25519Signature: string;
}

let _session: AuthSession | null = null;
/** In-flight authenticate() promise: two concurrent callers share one auth request. */
let _authInFlight: Promise<AuthSession> | null = null;
/** The room key statement, signed once per run. */
let _roomKeyIdentity: RoomKeyIdentity | null = null;

/** This agent's X25519 key (from `agentRoomKeySeed`) and its signed statement, made once. */
export async function roomKeyIdentity(): Promise<RoomKeyIdentity> {
    if (_roomKeyIdentity) return _roomKeyIdentity;
    const agent = getAgent();
    const x25519PublicKey = bytesToHex(x25519Public(await agent.roomKeySeed()));
    const x25519Signature = bytesToHex(agent.sign(roomKeySigningBytes(agent.did(), x25519PublicKey)));
    _roomKeyIdentity = { x25519PublicKey, x25519Signature };
    return _roomKeyIdentity;
}

/**
 * Decodes a JWT's payload WITHOUT verifying its signature — we only read
 * the `exp` claim to decide when to proactively refresh. The server
 * remains the sole source of truth for actual token validity; a token
 * this function can't parse is simply treated as non-expiring (refreshed
 * lazily on the next 401).
 */
export function parseJwtExpiryMs(token: string): number | null {
    const parts = token.split(".");
    if (parts.length !== 3) return null;
    try {
        const b64 = parts[1].replace(/-/g, "+").replace(/_/g, "/");
        const padded = b64 + "=".repeat((4 - (b64.length % 4)) % 4);
        const binary = atob(padded);
        const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
        const payload = JSON.parse(new TextDecoder().decode(bytes)) as { exp?: number };
        return typeof payload.exp === "number" ? payload.exp * 1000 : null;
    } catch {
        return null;
    }
}

/** Authenticates with one signed request and caches the session. */
export async function authenticate(): Promise<AuthSession> {
    const config = getConfig();
    const agent = getAgent();
    const did = agent.did();
    const { x25519PublicKey, x25519Signature } = await roomKeyIdentity();
    const timestamp = Date.now();
    const signature = agent.signStringHex(authMessage(config.roomId, timestamp));
    const res = await api.authenticate(config, { did, signature, timestamp, x25519PublicKey, x25519Signature });

    const session: AuthSession = { token: res.token, expiresAt: parseJwtExpiryMs(res.token), admitted: res.admitted };
    _session = session;
    return session;
}

/** The cached session, or a new one; like `getValidToken`, but with whether the room admits the key. */
export async function getValidSession(skewMs = 30_000): Promise<AuthSession> {
    if (_session && (_session.expiresAt === null || _session.expiresAt - Date.now() > skewMs)) {
        return _session;
    }
    if (!_authInFlight) {
        _authInFlight = authenticate().finally(() => { _authInFlight = null; });
    }
    return _authInFlight;
}

/**
 * Returns a token guaranteed valid for at least `skewMs` more milliseconds
 * (default 30s), transparently re-authenticating if there is no cached
 * session, the cached token is expired, or it's about to expire.
 *
 * Uses an in-flight promise cache so two concurrent callers (e.g. WS
 * reconnect + batched flush) share the same authenticate() round-trip.
 */
export async function getValidToken(skewMs = 30_000): Promise<string> {
    return (await getValidSession(skewMs)).token;
}

/** Forces the next getValidToken() call to re-authenticate — call this
 * after a request comes back 401 even though the cached token looked fresh. */
export function invalidateSession(): void {
    _session = null;
}

export function resetAuth(): void {
    _session = null;
    _authInFlight = null;
    _roomKeyIdentity = null;
}
