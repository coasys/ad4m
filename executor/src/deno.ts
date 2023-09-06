import "https://deno.land/std@0.177.0/node/util.ts";
import "https://deno.land/std@0.177.0/node/global.ts";

const process = globalThis.process;
const Buffer = globalThis.Buffer;
const setImmediate = globalThis.setImmediate;
const clearImmediate = globalThis.clearImmediate;
const __dirname = new URL('.', import.meta.url).pathname;
const __filename = new URL('', import.meta.url).pathname;
globalThis.__dirname = __dirname;
globalThis.__filename = __filename;

import { init as internalInit } from "./main.ts"
import * as internalPath from "https://deno.land/std@0.177.0/node/path.ts";
import * as internalOs from "https://deno.land/std@0.177.0/node/os.ts"

export const init = internalInit
export const path = internalPath
export const os = internalOs

console.log = (...args) => {
  UTILS.consoleLog(`${args.reduce((acc, cur) => acc += `${cur} `, "")}`)
};

console.debug = (...args) => {
  UTILS.consoleDebug(`${args.reduce((acc, cur) => acc += `${cur} `, "")}`)
};

console.error = (...args) => {
  UTILS.consoleError(`${args.reduce((acc, cur) => acc += `${cur} `, "")}`)
};

console.warn = (...args) => {
  UTILS.consoleWarn(`${args.reduce((acc, cur) => acc += `${cur} `, "")}`)
};

import "https://deno.land/x/xhr@0.3.0/mod.ts";

import { HTMLElement } from "linkedom"

// @ts-ignore
globalThis.HTMLElement = HTMLElement;