
import "../deno_std-0.177.0/node/util.ts";
import "../deno_std-0.177.0/node/global.ts";
const process = globalThis.process;
const Buffer = globalThis.Buffer;
const setImmediate = globalThis.setImmediate;
const clearImmediate = globalThis.clearImmediate;

console.log(process)
console.log(Buffer)
console.log(setImmediate)
console.log(clearImmediate)

import "./main.js"
