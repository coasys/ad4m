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
    it("reads an empty list as holding no key: the agent waits for one", async () => {
        answering({ status: 200, headers: {}, body: JSON.stringify({ keys: [] }) });
        assert.deepEqual(await api.fetchRoomKeys(config, "token"), { keys: [] });
    });

    it("returns the sealed versions the server holds for this agent", async () => {
        const entry = { encryptedKey: { ephemeralPublicKey: "aa", nonce: "bb", ciphertext: "cc" }, version: 1 };
        answering({ status: 200, headers: {}, body: JSON.stringify({ keys: [entry] }) });
        assert.deepEqual(await api.fetchRoomKeys(config, "token"), { keys: [entry] });
    });

    it("throws on a 2xx answer without a keys list, so a loaded key ring is kept", async () => {
        for (const body of ["", "{}", JSON.stringify({ keys: "x" })]) {
            answering({ status: 200, headers: {}, body });
            await assert.rejects(() => api.fetchRoomKeys(config, "token"), /without a keys list/);
            resetAdapters();
        }
    });

    it("throws on any error status, a 404 included: it is never read as holding no key", async () => {
        for (const status of [404, 500]) {
            answering({ status, headers: {}, body: "{\"error\":\"x\"}" });
            await assert.rejects(
                () => api.fetchRoomKeys(config, "token"),
                (err) => err instanceof api.ApiError && err.status === status,
            );
            resetAdapters();
        }
    });
});
