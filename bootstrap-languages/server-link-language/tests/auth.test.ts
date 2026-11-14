/**
 * Tests for src/auth.ts — the DID challenge-response flow against a mock
 * link-server, and token-expiry-driven refresh behaviour.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { ed25519 } from "@noble/curves/ed25519";
import { sha256 } from "@noble/hashes/sha256";

import type { Transport, TransportResponse } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import * as auth from "../src/auth.js";
import { bytesToHex } from "../src/encryption.js";
import { roomKeySigningBytes, verifyRoomKeySignature } from "../src/room.js";
import { x25519Public } from "../keyring/keyring.js";
import { TestAgent } from "./agent.js";

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



/** Builds an unsigned (alg: none) JWT — auth.ts never verifies the
 * signature, it only decodes the payload to read `exp`. */
function makeJwt(payload: Record<string, unknown>): string {
    const b64url = (input: string) =>
        Buffer.from(input, "utf8").toString("base64").replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
    return `${b64url(JSON.stringify({ alg: "none", typ: "JWT" }))}.${b64url(JSON.stringify(payload))}.`;
}

function setup(transport: Transport): TestAgent {
    resetAdapters();
    const agent = new TestAgent(5);
    initAdapters({
        transport,
        agent,
        config: { serverUrl: "https://server.example", roomId: "room-123" },
    });
    auth.resetAuth();
    return agent;
}

/** Routes both auth steps (same endpoint) to a challenge-then-token flow. */
function authRoute(
    transport: MockTransport,
    tokenPayload: Record<string, unknown> = { exp: Math.floor(Date.now() / 1000) + 3600 },
    admitted = true,
) {
    let authCalls = 0;
    transport.route(
        (url, method) => method === "POST" && url.endsWith("/rooms/room-123/auth"),
        () => {
            authCalls++;
            return { status: 200, headers: {}, body: JSON.stringify({ token: makeJwt(tokenPayload), admitted }) };
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

describe("auth: authenticate (protocol 2)", () => {
    it("signs the room's auth message and the X25519 key once, and caches the session", async () => {
        const transport = new MockTransport();
        const agent = setup(transport);
        const before = Date.now();
        let body: Record<string, unknown> = {};
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/rooms/room-123/auth"),
            (raw) => {
                body = JSON.parse(raw) as Record<string, unknown>;
                return {
                    status: 200,
                    headers: {},
                    body: JSON.stringify({ token: makeJwt({ exp: Math.floor(Date.now() / 1000) + 3600 }), admitted: true }),
                };
            },
        );

        const session = await auth.authenticate();
        assert.deepEqual(
            Object.keys(body).sort(),
            ["did", "signature", "timestamp", "x25519PublicKey", "x25519Signature"],
        );
        assert.equal(body.did, agent.did());
        const timestamp = body.timestamp as number;
        assert.ok(Number.isInteger(timestamp) && timestamp >= before && timestamp <= Date.now());
        assert.deepEqual(agent.signed, [`coasys-link-server:auth:2:room-123:${timestamp}`], "the auth message, once");
        const message = sha256(new TextEncoder().encode(agent.signed[0]));
        assert.ok(ed25519.verify(body.signature as string, message, ed25519.getPublicKey(agent.seed)));

        const x25519 = bytesToHex(x25519Public(await agent.roomKeySeed()));
        assert.equal(body.x25519PublicKey, x25519, "the X25519 key of agentRoomKeySeed");
        assert.ok(verifyRoomKeySignature(agent.did(), x25519, body.x25519Signature as string));
        assert.equal(session.admitted, true);

        assert.equal(await auth.getValidToken(), session.token);
        assert.equal(transport.calls.length, 1, "no extra auth call for a cached token");
    });

    it("reports a key the room does not admit", async () => {
        const transport = new MockTransport();
        setup(transport);
        authRoute(transport, { exp: Math.floor(Date.now() / 1000) + 3600 }, false);
        const session = await auth.authenticate();
        assert.equal(session.admitted, false);
    });

    it("propagates a server error (a room that does not exist yet is a 404)", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(() => true, () => ({ status: 404, headers: {}, body: '{"error":"no such room"}' }));

        await assert.rejects(() => auth.authenticate(), (err: unknown) => (err as { status?: number }).status === 404);
    });
});

describe("auth: the room key signature (K7)", () => {
    // From ad4m-identity-core's `room_key_signing_bytes` and `verify_room_key` (the link
    // server's node binding) for the Ed25519 seed 0x07 x 32 and the X25519 key 0x11 x 32.
    const VECTOR = {
        did: "did:key:z6MkvDqGT54cXesYGvABpF1UapVNwjCqRcafi4Px6Thv5T3Z",
        x25519: "11".repeat(32),
        bytes:
            '{"did":"did:key:z6MkvDqGT54cXesYGvABpF1UapVNwjCqRcafi4Px6Thv5T3Z","domain":"coasys:room-key",' +
            '"x25519PublicKey":"1111111111111111111111111111111111111111111111111111111111111111"}',
        signature:
            "2721373cb6dd5136eb153c328541a7098b3ee8d963486c7b6c3274286058fbac" +
            "12b05878ed4d9ff715afd6205c3b8864cdb0f9fe6210d023bc5410cde8149902",
    };

    it("signs the identity core's coasys:room-key bytes over {x25519PublicKey, did}", async () => {
        assert.equal(new TextDecoder().decode(roomKeySigningBytes(VECTOR.did, VECTOR.x25519)), VECTOR.bytes);
        const agent = new TestAgent(7);
        assert.equal(agent.did(), VECTOR.did);
        assert.equal(bytesToHex(agent.sign(roomKeySigningBytes(VECTOR.did, VECTOR.x25519))), VECTOR.signature);
        assert.ok(verifyRoomKeySignature(VECTOR.did, VECTOR.x25519, VECTOR.signature));
    });

    it("refuses a malformed statement and a signature of another key or X25519 key", () => {
        assert.throws(() => roomKeySigningBytes("did:scid:coasys:1:x", VECTOR.x25519));
        assert.throws(() => roomKeySigningBytes(VECTOR.did, "11".repeat(31)));
        assert.throws(() => roomKeySigningBytes(VECTOR.did, "AA".repeat(32)));
        assert.equal(verifyRoomKeySignature(VECTOR.did, "22".repeat(32), VECTOR.signature), false);
        assert.equal(verifyRoomKeySignature(new TestAgent(8).did(), VECTOR.x25519, VECTOR.signature), false);
        assert.equal(verifyRoomKeySignature(VECTOR.did, VECTOR.x25519, "zz"), false);
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
                authCalls++;
                return { status: 200, headers: {}, body: JSON.stringify({ token: "opaque-server-token", admitted: true }) };
            },
        );

        await auth.getValidToken();
        await auth.getValidToken();

        assert.equal(authCalls, 1);
    });
});

