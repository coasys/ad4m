import { describe, it, afterEach } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { resetAdapters } from "../src/adapters.js";
import { afterFailedKeyRead } from "../src/room-keys.js";
import { generateRoomKey } from "../src/encryption.js";
import { roomIdOf } from "../src/room.js";

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
    it("possibleTemplateParams are SERVER_URL, UID and CREATOR", () => {
        const params = extractPossibleTemplateParams(readBundle());
        assert.deepStrictEqual(params.sort(), ["CREATOR", "SERVER_URL", "UID"]);
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

    it("bundle has exactly three template variable markers", () => {
        const markers = readBundle()
            .split("\n")
            .filter((line) => line.includes("//!@ad4m-template-variable"));
        assert.strictEqual(
            markers.length,
            3,
            `Expected 3 template markers (SERVER_URL, UID, CREATOR), found ${markers.length}`,
        );
    });
});

describe("room id", () => {
    it("is base58(sha256(CREATOR, 0x00, UID)), as the link server checks it at genesis", () => {
        // From `roomIdOf` in link-server/src/membership.ts.
        assert.strictEqual(roomIdOf("did:scid:coasys:1:QmCreator", "3f2a-uid"), "G6zF1MMHd8RamYLF7ZGBVpCbDhfigXNvt8mP9Dv5BBhY");
        assert.strictEqual(roomIdOf("x", "\u0000a"), "7gZU8QszgaZypRYWkBoKuzeR7XWf4mjG8J2dUaV5C4SX");
    });

    it("is the same for the same CREATOR and UID, and differs when either differs", () => {
        const creator = "did:scid:coasys:1:QmCreator";
        assert.strictEqual(roomIdOf(creator, "uid-alpha"), roomIdOf(creator, "uid-alpha"));
        assert.notStrictEqual(roomIdOf(creator, "uid-alpha"), roomIdOf(creator, "uid-beta"));
        assert.notStrictEqual(roomIdOf(creator, "uid-alpha"), roomIdOf("did:scid:coasys:1:QmOther", "uid-alpha"));
        // The separator keeps ("ab", "c") and ("a", "bc") apart.
        assert.notStrictEqual(roomIdOf("ab", "c"), roomIdOf("a", "bc"));
    });
});

describe("afterFailedKeyRead", () => {
    it("keeps a loaded key ring when a refresh fails: commits still encrypt, inbound links still open", () => {
        const ring = new Map([[1, generateRoomKey()]]);
        const after = afterFailedKeyRead("ready", ring);
        assert.strictEqual(after.status, "ready");
        assert.strictEqual(after.ring, ring);
    });

    it("is an error when no ring was loaded: commits wait", () => {
        for (const status of ["pending", "error"] as const) {
            assert.deepStrictEqual(afterFailedKeyRead(status, null), { status: "error", ring: null });
        }
        assert.deepStrictEqual(afterFailedKeyRead("ready", new Map()), { status: "error", ring: null });
    });
});
