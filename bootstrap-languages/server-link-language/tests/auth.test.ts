/**
 * Tests for src/auth.ts — the DID challenge-response flow against a mock
 * link-server, and token-expiry-driven refresh behaviour.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";

import type { AgentAdapter, Transport, TransportResponse } from "../src/adapters.js";
import { initAgent, initConfig, initTransport, resetAdapters } from "../src/adapters.js";
import * as auth from "../src/auth.js";

// ---------------------------------------------------------------------------
// Mock adapters
// ---------------------------------------------------------------------------

class MockTransport implements Transport {
    calls: Array<{ url: string; method: string; headers: Record<string, string>; body: string }> = [];
    private routes: Array<{
        test: (url: string, method: string) => boolean;
        handle: (body: string) => TransportResponse;
    }> = [];

    route(test: (url: string, method: string) => boolean, handle: (body: string) => TransportResponse): void {
        this.routes.push({ test, handle });
    }

    async fetch(url: string, method: string, headers: Record<string, string>, body: string): Promise<TransportResponse> {
        this.calls.push({ url, method, headers, body });
        const route = this.routes.find((r) => r.test(url, method));
        if (!route) throw new Error(`MockTransport: no route for ${method} ${url}`);
        return route.handle(body);
    }
}

class MockAgent implements AgentAdapter {
    constructor(private readonly didValue: string) {}
    did(): string {
        return this.didValue;
    }
    signStringHex(payload: string): string {
        // Deterministic stand-in for EdDSA signing — auth.ts only relies on
        // determinism (same input -> same output), not on real Ed25519 math.
        return createHash("sha256").update(`${this.didValue}:${payload}`).digest("hex");
    }
}

/** Builds an unsigned (alg: none) JWT — auth.ts never verifies the
 * signature, it only decodes the payload to read `exp`. */
function makeJwt(payload: Record<string, unknown>): string {
    const b64url = (input: string) =>
        Buffer.from(input, "utf8").toString("base64").replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
    return `${b64url(JSON.stringify({ alg: "none", typ: "JWT" }))}.${b64url(JSON.stringify(payload))}.`;
}

function setup(transport: Transport, did = "did:key:zTestAgent"): void {
    resetAdapters();
    initTransport(transport);
    initAgent(new MockAgent(did));
    initConfig({ serverUrl: "https://server.example", roomId: "room-123" });
    auth.resetAuth();
}

/** Routes both auth steps (same endpoint) to a challenge-then-token flow. */
function authRoute(transport: MockTransport, tokenPayload: Record<string, unknown> = { exp: Math.floor(Date.now() / 1000) + 3600 }) {
    let authCalls = 0;
    transport.route(
        (url, method) => method === "POST" && url.endsWith("/rooms/room-123/auth"),
        (body) => {
            const parsed = JSON.parse(body) as Record<string, unknown>;
            if (!("signature" in parsed)) {
                return { status: 200, headers: {}, body: JSON.stringify({ challenge: "chal-abc" }) };
            }
            authCalls++;
            return { status: 200, headers: {}, body: JSON.stringify({ token: makeJwt(tokenPayload) }) };
        },
    );
    return () => authCalls;
}

// ---------------------------------------------------------------------------
// parseJwtExpiryMs
// ---------------------------------------------------------------------------

describe("auth: parseJwtExpiryMs", () => {
    it("reads the exp claim (seconds) as milliseconds", () => {
        const token = makeJwt({ exp: 1700000000 });
        assert.equal(auth.parseJwtExpiryMs(token), 1700000000 * 1000);
    });

    it("returns null for a malformed token", () => {
        assert.equal(auth.parseJwtExpiryMs("not-a-jwt"), null);
    });

    it("returns null when there is no exp claim", () => {
        assert.equal(auth.parseJwtExpiryMs(makeJwt({ sub: "foo" })), null);
    });
});

// ---------------------------------------------------------------------------
// authenticate()
// ---------------------------------------------------------------------------

describe("auth: authenticate", () => {
    it("performs the two-step DID challenge-response flow and includes the derived X25519 public key", async () => {
        const transport = new MockTransport();
        setup(transport);

        transport.route(
            (url, method) => method === "POST" && url.endsWith("/rooms/room-123/auth"),
            (body) => {
                const parsed = JSON.parse(body) as Record<string, unknown>;
                if (!("signature" in parsed)) {
                    assert.equal(parsed.did, "did:key:zTestAgent");
                    return { status: 200, headers: {}, body: JSON.stringify({ challenge: "chal-abc" }) };
                }
                assert.equal(parsed.challenge, "chal-abc");
                assert.ok(typeof parsed.signature === "string" && parsed.signature.length > 0);
                assert.ok(
                    typeof parsed.x25519PublicKey === "string" && parsed.x25519PublicKey.length === 64,
                    "expected a 32-byte hex-encoded X25519 public key on step 2",
                );
                return {
                    status: 200,
                    headers: {},
                    body: JSON.stringify({ token: makeJwt({ exp: Math.floor(Date.now() / 1000) + 3600 }) }),
                };
            },
        );

        const session = await auth.authenticate();
        assert.ok(session.token);
        assert.ok(session.expiresAt !== null && session.expiresAt > Date.now());
        assert.equal(transport.calls.length, 2);
        assert.equal(auth.currentSession()?.token, session.token);
    });

    it("propagates a server error from the challenge step", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(() => true, () => ({ status: 403, headers: {}, body: "forbidden" }));

        await assert.rejects(() => auth.authenticate());
    });
});

// ---------------------------------------------------------------------------
// getValidToken caching + refresh
// ---------------------------------------------------------------------------

describe("auth: getValidToken", () => {
    it("caches a token that is not close to expiring", async () => {
        const transport = new MockTransport();
        setup(transport);
        const authCalls = authRoute(transport, { exp: Math.floor(Date.now() / 1000) + 3600 });

        const t1 = await auth.getValidToken();
        const t2 = await auth.getValidToken();

        assert.equal(t1, t2);
        assert.equal(authCalls(), 1);
    });

    it("re-authenticates when the cached token is within the skew window", async () => {
        const transport = new MockTransport();
        setup(transport);
        // Expires in 5s — inside the default 30s skew.
        const authCalls = authRoute(transport, { exp: Math.floor(Date.now() / 1000) + 5 });

        await auth.getValidToken();
        await auth.getValidToken();

        assert.equal(authCalls(), 2);
    });

    it("re-authenticates after invalidateSession()", async () => {
        const transport = new MockTransport();
        setup(transport);
        const authCalls = authRoute(transport, { exp: Math.floor(Date.now() / 1000) + 3600 });

        await auth.getValidToken();
        auth.invalidateSession();
        await auth.getValidToken();

        assert.equal(authCalls(), 2);
    });

    it("treats a token with no exp claim as non-expiring", async () => {
        const transport = new MockTransport();
        setup(transport);
        let authCalls = 0;
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/rooms/room-123/auth"),
            (body) => {
                const parsed = JSON.parse(body) as Record<string, unknown>;
                if (!("signature" in parsed)) return { status: 200, headers: {}, body: JSON.stringify({ challenge: "c" }) };
                authCalls++;
                return { status: 200, headers: {}, body: JSON.stringify({ token: "opaque-server-token" }) };
            },
        );

        await auth.getValidToken();
        await auth.getValidToken();

        assert.equal(authCalls, 1);
    });
});

// ---------------------------------------------------------------------------
// X25519 public key derivation caching
// ---------------------------------------------------------------------------

describe("auth: getX25519PublicKeyHex", () => {
    it("is deterministic and memoized across calls", () => {
        resetAdapters();
        initAgent(new MockAgent("did:key:zXAgent"));
        auth.resetAuth();

        const k1 = auth.getX25519PublicKeyHex();
        const k2 = auth.getX25519PublicKeyHex();
        assert.equal(k1, k2);
        assert.equal(k1.length, 64);
    });

    it("differs between agents", () => {
        resetAdapters();
        initAgent(new MockAgent("did:key:zAgentOne"));
        auth.resetAuth();
        const k1 = auth.getX25519PublicKeyHex();

        resetAdapters();
        initAgent(new MockAgent("did:key:zAgentTwo"));
        auth.resetAuth();
        const k2 = auth.getX25519PublicKeyHex();

        assert.notEqual(k1, k2);
    });
});
