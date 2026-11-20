// Loads the built package the way an app does (`init()` with the wasm bytes) and checks that the
// exports answer: run after `pnpm build`.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const pkg = join(dirname(fileURLToPath(import.meta.url)), "..", "pkg");
const identity = await import(join(pkg, "identity.js"));
await identity.default({ module_or_path: readFileSync(join(pkg, "identity_bg.wasm")) });

const enc = identity.EncKey.generate();
assert.equal(identity.EncKey.fromSecret(enc.secret()).multikey(), enc.multikey());
const sign = identity.ed25519Multikey(new Uint8Array(32).fill(9));
const text = identity.deviceString(sign, enc.multikey(), identity.newR(), 1_700_000_300, undefined, "https://store.example");
assert.ok(text.startsWith("coasys:device:1:"));
assert.equal(identity.matchCode("scid", "offer", sign), identity.matchCode("scid", "offer", sign));
assert.throws(() => identity.deviceString("nope", enc.multikey(), identity.newR(), 1, undefined, "x"));
console.log("identity-wasm: ok");
