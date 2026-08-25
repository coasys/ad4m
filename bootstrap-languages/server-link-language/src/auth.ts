/**
 * DID challenge-response authentication against link-server, plus
 * token-expiry tracking so callers can request a guaranteed-fresh JWT
 * without re-authenticating on every single call.
 */

import { getAgent, getConfig } from "./adapters.js";
import * as api from "./api.js";
import { bytesToHex, deriveX25519KeyPair } from "./encryption.js";

export interface AuthSession {
    token: string;
    /** ms since epoch the token expires at, or null if it has no readable `exp` claim. */
    expiresAt: number | null;
}

let _session: AuthSession | null = null;
let _x25519PublicKeyHex: string | null = null;

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
        const payload = JSON.parse(atob(padded)) as { exp?: number };
        return typeof payload.exp === "number" ? payload.exp * 1000 : null;
    } catch {
        return null;
    }
}

function myX25519PublicKeyHex(): string {
    if (_x25519PublicKeyHex) return _x25519PublicKeyHex;
    const agent = getAgent();
    const { publicKey } = deriveX25519KeyPair((payload) => agent.signStringHex(payload));
    _x25519PublicKeyHex = bytesToHex(publicKey);
    return _x25519PublicKeyHex;
}

/** Runs the full two-step DID challenge-response flow and caches the result. */
export async function authenticate(): Promise<AuthSession> {
    const config = getConfig();
    const agent = getAgent();
    const did = agent.did();

    const challenge = await api.requestChallenge(config, did);
    const signature = agent.signStringHex(challenge);
    const token = await api.verifyChallenge(config, did, challenge, signature, myX25519PublicKeyHex());

    const session: AuthSession = { token, expiresAt: parseJwtExpiryMs(token) };
    _session = session;
    return session;
}

/**
 * Returns a token guaranteed valid for at least `skewMs` more milliseconds
 * (default 30s), transparently re-authenticating if there is no cached
 * session, the cached token is expired, or it's about to expire.
 */
export async function getValidToken(skewMs = 30_000): Promise<string> {
    if (_session && (_session.expiresAt === null || _session.expiresAt - Date.now() > skewMs)) {
        return _session.token;
    }
    const session = await authenticate();
    return session.token;
}

/** Forces the next getValidToken() call to re-authenticate — call this
 * after a request comes back 401 even though the cached token looked fresh. */
export function invalidateSession(): void {
    _session = null;
}

export function currentSession(): AuthSession | null {
    return _session;
}

/** Returns this agent's derived X25519 public key (hex). Exposed so
 * index.ts / encryption bootstrapping can reuse the same derivation
 * without re-deriving. */
export function getX25519PublicKeyHex(): string {
    return myX25519PublicKeyHex();
}

export function resetAuth(): void {
    _session = null;
    _x25519PublicKeyHex = null;
}
