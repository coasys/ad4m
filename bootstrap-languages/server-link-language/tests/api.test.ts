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

describe("api: fetchMembers / rotateKeys (K9)", () => {
    it("reads the room's latest key version from /members", async () => {
        answering({ status: 200, headers: {}, body: JSON.stringify({ members: [], keys: [], latestKeyVersion: 3 }) });
        assert.deepEqual(await api.fetchMembers(config, "token"), { members: [], keys: [], latestKeyVersion: 3 });
    });

    it("refuses /members without a latest key version: a rotation would not know what to seal for", async () => {
        for (const latestKeyVersion of [undefined, -1, 1.5, "2"]) {
            answering({ status: 200, headers: {}, body: JSON.stringify({ members: [], keys: [], latestKeyVersion }) });
            await assert.rejects(() => api.fetchMembers(config, "token"), /latest key version/);
            resetAdapters();
        }
    });

    it("sends the version the boxes are sealed for with the rotation", async () => {
        let sent: unknown;
        const transport: Transport = {
            fetch: async (_url, _method, _headers, body) => {
                sent = JSON.parse(body);
                return { status: 200, headers: {}, body: JSON.stringify({ version: 4, recipients: [] }) };
            },
        };
        initAdapters({ transport, config });
        await api.rotateKeys(config, "token", [], "c0", 4);
        assert.deepEqual(sent, { keys: [], commitment: "c0", version: 4 });
    });
});
