
import "../deno_std-0.177.0/node/util.ts";
import "../deno_std-0.177.0/node/global.ts";
const process = globalThis.process;
const Buffer = globalThis.Buffer;
const setImmediate = globalThis.setImmediate;
const clearImmediate = globalThis.clearImmediate;

const __dirname = new URL('.', import.meta.url).pathname;
const __filename = new URL('', import.meta.url).pathname;

globalThis.__dirname = __dirname;
globalThis.__filename = __filename;

import { init as internalInit } from "./main.js"
import * as internalPath from "../deno_std-0.177.0/node/path.ts";
import * as internalOs from "../deno_std-0.177.0/node/os.ts"
console.log("Hello from deno.js")
export const init = internalInit
export const path = internalPath
export const os = internalOs

import "../xhr/mod.ts";

import {HTMLElement } from "../linkedom"

globalThis.HTMLElement = HTMLElement;
