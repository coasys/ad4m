import { describe, it, afterEach } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { initAdapters, getConfig, resetAdapters } from "../src/adapters.js";
import { adminRotates, afterFailedKeyRead } from "../src/room-keys.js";
import { generateRoomKey } from "../src/encryption.js";

const __dirname = dirname(fileURLToPath(import.meta.url));
const bundlePath = resolve(__dirname, "..", "build", "bundle.js");

function readBundle(): string {
    return readFileSync(bundlePath, "utf-8");
}

function extractPossibleTemplateParams(bundle: string): string[] {
    const match = bundle.match(/possibleTemplateParams\s*[=:]\s*\[([^\]]*)\]/);
    if (!match) return [];
    return match[1]
        .split(",")
        .map((s) => s.trim().replace(/^["']|["']$/g, ""))
        .filter(Boolean);
}

afterEach(() => resetAdapters());

describe("template uniqueness", () => {
    it("possibleTemplateParams contains only SERVER_URL and UID", () => {
        const params = extractPossibleTemplateParams(readBundle());
        assert.deepStrictEqual(params.sort(), ["SERVER_URL", "UID"]);
    });

    it("ROOM_ID does not appear as a template variable in the bundle", () => {
        const lines = readBundle().split("\n");
        for (let i = 0; i < lines.length; i++) {
            if (lines[i].includes("//!@ad4m-template-variable")) {
                const nextLine = lines[i + 1] || "";
                assert.ok(
                    !nextLine.includes("ROOM_ID"),
                    `ROOM_ID found as template variable at bundle line ${i + 2}: ${nextLine}`,
                );
            }
        }
    });

    it("bundle has exactly two template variable markers", () => {
        const markers = readBundle()
            .split("\n")
            .filter((line) => line.includes("//!@ad4m-template-variable"));
        assert.strictEqual(
            markers.length,
            2,
            `Expected 2 template markers (SERVER_URL, UID), found ${markers.length}`,
        );
    });

    it("config.roomId maps to the UID value passed at init", () => {
        const uid = "test-uid-abc-123";
        initAdapters({ config: { serverUrl: "https://example.com", roomId: uid } });
        assert.strictEqual(getConfig().roomId, uid);
    });

    it("same UID produces identical config.roomId across inits", () => {
        const uid = "shared-uid-value";

        initAdapters({ config: { serverUrl: "https://server-a.com", roomId: uid } });
        const roomA = getConfig().roomId;
        resetAdapters();

        initAdapters({ config: { serverUrl: "https://server-a.com", roomId: uid } });
        const roomB = getConfig().roomId;

        assert.strictEqual(roomA, roomB);
        assert.strictEqual(roomA, uid);
    });

    it("different UIDs produce different config.roomId values", () => {
        initAdapters({ config: { serverUrl: "https://example.com", roomId: "uid-alpha" } });
        const roomA = getConfig().roomId;
        resetAdapters();

        initAdapters({ config: { serverUrl: "https://example.com", roomId: "uid-beta" } });
        const roomB = getConfig().roomId;

        assert.notStrictEqual(roomA, roomB);
    });
});

describe("adminRotates", () => {
    it("rotates when the admin holds no room key it can open", () => {
        assert.strictEqual(adminRotates("none", false), true);
        assert.strictEqual(adminRotates("pending", false), true);
    });

    it("rotates at most once a run: if its own new box did not open, another version would not either", () => {
        assert.strictEqual(adminRotates("pending", true), false);
        assert.strictEqual(adminRotates("none", true), false);
    });

    it("does not rotate while it holds a key, or when the key ring errored", () => {
        assert.strictEqual(adminRotates("ready", false), false);
        assert.strictEqual(adminRotates("error", false), false);
    });
});

describe("afterFailedKeyRead", () => {
    it("keeps a loaded key ring when a refresh fails: commits still encrypt, inbound links still open", () => {
        const ring = new Map([[1, generateRoomKey()]]);
        const after = afterFailedKeyRead("ready", ring);
        assert.strictEqual(after.status, "ready");
        assert.strictEqual(after.ring, ring);
    });

    it("is an error when no ring was loaded: commits refuse plaintext", () => {
        for (const status of ["none", "pending", "error"] as const) {
            assert.deepStrictEqual(afterFailedKeyRead(status, null), { status: "error", ring: null });
        }
        assert.deepStrictEqual(afterFailedKeyRead("ready", new Map()), { status: "error", ring: null });
    });
});
