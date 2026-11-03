/**
 * Tests for src/api.ts answers the language reads its state from.
 */

import { describe, it, afterEach } from "node:test";
import assert from "node:assert/strict";

import type { Transport, TransportResponse } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import * as api from "../src/api.js";

const config = { serverUrl: "https://server.example", roomId: "room-1" };

function answering(response: TransportResponse): void {
    const transport: Transport = { fetch: async () => response };
    initAdapters({ transport, config });
}

afterEach(() => resetAdapters());

describe("api: fetchRoomKeys", () => {
    it("reads a 404 as holding no key: the agent waits for one, it never means plaintext", async () => {
        // An older link server, or a room an older build created, answers 404
        // to a member without a key.
        answering({ status: 404, headers: {}, body: "{\"error\":\"no room key available for this agent yet\"}" });
        assert.deepEqual(await api.fetchRoomKeys(config, "token"), { keys: [] });
    });

    it("returns the sealed versions the server holds for this agent", async () => {
        const entry = { encryptedKey: { ephemeralPublicKey: "aa", nonce: "bb", ciphertext: "cc" }, version: 1 };
        answering({ status: 200, headers: {}, body: JSON.stringify({ keys: [entry], e2e_enabled: true }) });
        assert.deepEqual(await api.fetchRoomKeys(config, "token"), { keys: [entry] });
    });

    it("propagates any other failure", async () => {
        answering({ status: 500, headers: {}, body: "boom" });
        await assert.rejects(() => api.fetchRoomKeys(config, "token"), (err) => err instanceof api.ApiError && err.status === 500);
    });
});
