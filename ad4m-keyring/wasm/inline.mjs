#!/usr/bin/env node
// Inlines a wasm-bindgen `--target deno` output into one ES module with the wasm bytes as
// base64, so a language bundle (esbuild) carries it and no file is fetched at load. The same
// module loads under Node, which the language's tests use.
//
//   node inline.mjs <deno out dir> <crate name> <output .js>
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";

const [denoDir, name, outPath] = process.argv.slice(2);
if (!denoDir || !name || !outPath) {
    console.error("usage: node inline.mjs <deno out dir> <crate name> <output .js>");
    process.exit(2);
}

const jsPath = join(denoDir, `${name}.js`);
const js = readFileSync(jsPath, "utf8");
const wasm = readFileSync(join(denoDir, `${name}_bg.wasm`));

const fetchPattern =
    /const wasmUrl = new URL\([^)]*\);\nconst wasmInstantiated = await WebAssembly\.instantiateStreaming\(fetch\(wasmUrl\), __wbg_get_imports\(\)\);/;
if (!fetchPattern.test(js)) {
    console.error("could not find the wasm-bindgen deno loader in", jsPath);
    process.exit(2);
}

const replacement = `const __wasmB64 = "${wasm.toString("base64")}";
function __b64ToBytes(b64) {
    const bin = (typeof atob === "function")
        ? atob(b64)
        : Buffer.from(b64, "base64").toString("binary");
    const out = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
}
const __wasmModule = await WebAssembly.compile(__b64ToBytes(__wasmB64));
const wasmInstantiated = { instance: await WebAssembly.instantiate(__wasmModule, __wbg_get_imports()), module: __wasmModule };`;

mkdirSync(dirname(outPath), { recursive: true });
const inlined = js.replace(fetchPattern, replacement);
writeFileSync(outPath, inlined);
console.log(`wrote ${outPath} (${(inlined.length / 1024).toFixed(1)} KiB, wasm ${(wasm.length / 1024).toFixed(1)} KiB)`);
