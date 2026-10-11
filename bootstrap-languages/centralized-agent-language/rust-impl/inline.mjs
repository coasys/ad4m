#!/usr/bin/env node
// Inline the wasm-bindgen deno target output into a single-file ES module.
// Produces build/bundle.js with the wasm bytes embedded as base64.
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const denoDir = join(here, "build-deno");
const jsPath = join(denoDir, "centralized_agent_language.js");
const wasmPath = join(denoDir, "centralized_agent_language_bg.wasm");
const outDir = join(here, "build");
const outPath = join(outDir, "bundle.js");

const js = readFileSync(jsPath, "utf8");
const wasm = readFileSync(wasmPath);
const wasmB64 = wasm.toString("base64");

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
