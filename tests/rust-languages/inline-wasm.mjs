#!/usr/bin/env node
/**
 * Post-processor: turn a wasm-bindgen `--target deno` output pair
 * (test_wasm_language.js + test_wasm_language_bg.wasm) into a single
 * self-contained ES module bundle that can be published as an AD4M
 * Language without companion files.
 *
 * The transformation replaces the `fetch(new URL('..._bg.wasm'))` line
 * with an in-memory base64 → ArrayBuffer → WebAssembly.compile() chain.
 *
 * Usage:
 *   node inline-wasm.mjs <lang-dir>
 * where <lang-dir> contains build-deno/test_wasm_language.js + .wasm.
 */
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join, basename } from "node:path";

const langDir = process.argv[2];
if (!langDir) {
    console.error("usage: inline-wasm.mjs <lang-dir>");
    process.exit(1);
}

const langName = basename(langDir).replace(/-/g, "_");
const denoDir = join(langDir, "build-deno");
const jsPath = join(denoDir, `${langName}.js`);
const wasmPath = join(denoDir, `${langName}_bg.wasm`);
const outDir = join(langDir, "build");
const outPath = join(outDir, "bundle.js");

const js = readFileSync(jsPath, "utf8");
const wasm = readFileSync(wasmPath);
const wasmB64 = wasm.toString("base64");

// Replace the deno-target loader with an in-memory equivalent.
const fetchPattern =
    /const wasmUrl = new URL\([^)]*\);\nconst wasmInstantiated = await WebAssembly\.instantiateStreaming\(fetch\(wasmUrl\), __wbg_get_imports\(\)\);/;

if (!fetchPattern.test(js)) {
    console.error("could not find wasm-bindgen deno loader pattern in", jsPath);
    process.exit(2);
}

const replacement = `const __wasmB64 = "${wasmB64}";
function __b64ToBytes(b64) {
    const bin = (typeof atob === "function")
        ? atob(b64)
        : Buffer.from(b64, "base64").toString("binary");
    const out = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
}
const __wasmBytes = __b64ToBytes(__wasmB64);
const __wasmModule = await WebAssembly.compile(__wasmBytes);
const wasmInstantiated = { instance: await WebAssembly.instantiate(__wasmModule, __wbg_get_imports()), module: __wasmModule };`;

const inlined = js.replace(fetchPattern, replacement);

mkdirSync(outDir, { recursive: true });
writeFileSync(outPath, inlined);
console.log(`wrote ${outPath} (${(inlined.length / 1024).toFixed(1)} KiB, wasm ${(wasm.length / 1024).toFixed(1)} KiB)`);
